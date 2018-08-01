open Ulmus.Std

type head =
  | Def of string | Con of string | Var of string
  | Lambda of string * term

and term =
  { head : head
  ; args : term list
  }

let rec is_ground = function
  | { head = Con _; args } -> List.for_all is_ground args
  | _ -> false

type term_context =
  { ctxt_head : head; ctxt_before : term list; ctxt_after : term list }

type point =
  { focus : term
  ; ctxt  : term_context list
  }

let rec plug term = function
  | [] -> term
  | { ctxt_head; ctxt_before; ctxt_after } :: context ->
     let term =
       { head = ctxt_head
       ; args = List.rev_append ctxt_before (term :: ctxt_after)
       }
     in
     plug term context

let fold_term f term =
  let rec fold context term =
    let rec loop accum before after = match after with
      | [] -> List.rev accum
      | a::after ->
         let r =
           fold ({ ctxt_head   = term.head
                 ; ctxt_before = before
                 ; ctxt_after  = after
                 } :: context)
             a
         in
         loop (r::accum) (a::before) after
    in
    let args = loop [] [] term.args in
    f { focus = term; ctxt = context } term.head args
  in
  fold [] term

(**********************************************************************)

type pattern =
  | PVar of string
  | PCon of string * pattern list

type guard = string * [`Le|`Gt] * string

(* Assumptions: the pattern is linear, and all the variables in the
   rhs are named in the head *)
type rule =
  { head  : string
  ; pats  : pattern list
  ; guard : guard option
  ; rhs   : term
  }

let arithmetic_rule = function
  | { head = Def "(+)"
    ; args = [ { head = Con x1; args = [] }
             ; { head = Con x2; args = [] } ]
    } ->
     (match int_of_string x1, int_of_string x2 with
       | exception Failure _ ->
          None
       | x1, x2 ->
          Some { head = Con (string_of_int (x1+x2)); args = [] })
  | _ ->
     None

module VarMap = Map.Make (String)

(* FIXME: this should also recurse into lambdas, after doing renaming
   on their bound vars. *)

let rec inst_rhs subst ({head;args} as tm) =
  let args = List.map (inst_rhs subst) args in
  match head with
    | Var vnm ->
       (match VarMap.find vnm subst with
         | exception Not_found ->
            (* Shouldn't happen if all the rules are well formed *)
            tm
         | {head; args = pre_args} ->
            {head; args = pre_args @ args})
    | Lambda (var, tm) ->
       { head = Lambda (var, inst_rhs subst tm)
       ; args }
    | _ ->
       {tm with args}

let beta_rule = function
  | { head = Lambda (var_x, body); args = arg1 :: rest_args } ->
     (* FIXME: substitute arg1 into body, with capture avoiding
        substitution *)
     let subst = VarMap.singleton var_x arg1 in
     let { head; args } = inst_rhs subst body in
     Some { head; args = args @ rest_args }
  | _ ->
     None

let get_int (subst : term VarMap.t) x =
  match VarMap.find x subst with
    | exception Not_found -> None
    | { head = Con x } ->
       (match int_of_string x with
         | exception Failure _ -> None
         | i -> Some i)
    | _ ->
       None

let check_guard subst = function
  | None -> true
  | Some (x, cmp, y) ->
     match get_int subst x, cmp, get_int subst y with
       | None, _, _ | _, _, None -> false
       | Some x, `Gt, Some y -> x > y
       | Some x, `Le, Some y -> x <= y

let rec match_term_pat subst tm pat = match tm, pat with
  | tm,
    PVar pv ->
     Some (VarMap.add pv tm subst)
  | {head = Con ahead; args},
    PCon (phead, pats)
    when ahead = phead -> 
     match_terms_pats subst args pats
  | _ ->
     None

and match_terms_pats subst args pats =
  match args, pats with
    | [], [] ->
       Some subst
    | [], _::_ | _::_, [] ->
       None
    | a::args, p::pats ->
       (match match_term_pat subst a p with
         | None -> None
         | Some subst -> match_terms_pats subst args pats)

let rec match_arg_patterns subst args pats =
  match args, pats with
    | args,    []      -> Some (subst, args)
    | [],      _::_    -> None
    | a::args, p::pats ->
       (match match_term_pat subst a p with
         | Some subst -> match_arg_patterns subst args pats
         | None       -> None)

type gen_rule =
  | Arithmetic
  | Beta
  | Rewrite of rule

let apply_rule (rule : gen_rule) (term : term) =
  match rule with
    | Rewrite rule ->
       (if Def rule.head <> term.head then None
       else match match_arg_patterns VarMap.empty term.args rule.pats with
         | None ->
            None
         | Some (subst, remaining_args) ->
            if check_guard subst rule.guard then
              let {head; args} = inst_rhs subst rule.rhs in
              Some {head ; args = args @ remaining_args}
            else
              None)
    | Arithmetic ->
       arithmetic_rule term
    | Beta ->
       beta_rule term

let apply_at_point rule point =
  match apply_rule rule point.focus with
    | Some term -> Some (plug term point.ctxt)
    | None      -> None

(* Now:
   1. Render terms to HTML
   2. Mode of operation:
      - Select a rule (persistent)
      - Select a place to apply it
*)

let var nm = { head = Var nm; args = [] }
let var_ nm args = { head = Var nm; args }
let def nm args = { head = Def nm; args }
let con nm args = { head = Con nm; args }

let lam x tm = { head = Lambda (x, tm); args = [] }

let rules =
  [| `R Arithmetic
   ; `R Beta
   ; `D
   ; `R (Rewrite { head = "iterNat"; pats = [ PVar "zero"; PVar "succ"; PCon ("Zero", []) ]
                 ; guard = None
                 ; rhs = var "zero" })
   ; `R (Rewrite { head = "iterNat"; pats = [ PVar "zero"; PVar "succ"; PCon ("Succ", [PVar "n"]) ]
                 ; guard = None
                 ; rhs = var_ "succ" [ def "iterNat" [ var "zero"; var "succ"; var "n" ] ]
                 })
   ; `D
   ; `R (Rewrite { head = "caseNat"; pats = [ PVar "zero"; PVar "succ"; PCon ("Zero", []) ]
                 ; guard = None
                 ; rhs = var "zero" })
   ; `R (Rewrite { head = "caseNat"; pats = [ PVar "zero"; PVar "succ"; PCon ("Succ", [PVar "n"]) ]
                 ; guard = None
                 ; rhs = var_ "succ" [ var "n" ]
                 })
   ; `D
   ; `R (Rewrite { head = "plus"; pats = [ PVar "m" ]
                 ; guard = None
                 ; rhs = def "iterNat" [ var "m"; con "Succ" [] ]
                 })

   ; `R (Rewrite { head = "plus2"; pats = [ PVar "m" ]
                 ; guard = None
                 ; rhs = def "iterNat" [ var "m"; { head = Lambda ("x", con "Succ" [var "x"]); args = []} ]
                 })
   ; `D
   ; `R (Rewrite { head = "eqNat"; pats = []
                 ; guard = None
                 ; rhs = def "iterNat"
                       [ def "caseNat" [ con "True" []; lam "k" (con "False" []) ]
                       ; lam "eqNat_m" (def "caseNat" [ con "False" []; lam "k" (var_ "eqNat_m" [ var "k" ]) ])
                       ] })

   (* ; Rewrite { head = "total"; pats = [ PCon ("Nil", []) ] *)
   (*           ; guard = None *)
   (*           ; rhs  = con "0" [] } *)
   (* ; Rewrite { head = "total"; pats = [ PCon ("Cons", [PVar "x"; PVar "xs"]) ] *)
   (*           ; guard = None *)
   (*           ; rhs  = def "(+)" [ var "x"; def "total" [var "xs"]] } *)
(*   ; Rewrite { head = "append"; pats = [ PCon ("Nil", []); PVar "ys" ]
             ; guard = None
             ; rhs  = { head = Var "ys"; args = [] } }
   ; Rewrite { head = "append"; pats = [ PCon ("Cons", [PVar "x"; PVar "xs"]); PVar "ys" ]
             ; guard = None
             ; rhs  = con "Cons" [ var "x"; def "append" [ var "xs"; var "ys" ] ] }

   ; Rewrite { head = "insert"; pats = [ PVar "x"; PCon ("Leaf", []) ]
             ; guard = None
             ; rhs = con "Node" [con "Leaf" []; var "x"; con "Leaf" []] }
   ; Rewrite { head  = "insert"; pats = [ PVar "x"; PCon ("Node", [PVar "l"; PVar "y"; PVar "r"]) ]
             ; guard = Some ("x", `Le, "y")
             ; rhs   = con "Node" [ def "insert" [var "x"; var "l"]; var "y"; var "r"] }
   ; Rewrite { head  = "insert"; pats = [ PVar "x"; PCon ("Node", [PVar "l"; PVar "y"; PVar "r"]) ]
             ; guard = Some ("x", `Gt, "y")
             ; rhs   = con "Node" [ var "l"; var "y"; def "insert" [var "x"; var "r"]] }

   ; Rewrite { head  = "foldList"; pats = [PVar "n"; PVar "c"; PCon ("Nil", [])]
             ; guard = None
             ; rhs   = var "n"
             }
   ; Rewrite { head  = "foldList"; pats = [PVar "n"; PVar "c"; PCon ("Cons", [PVar "x"; PVar "xs"])]
             ; guard = None
             ; rhs   = var_ "c" [ var "x"; def "foldList" [var "n"; var "c"; var "xs"] ]
             }

   ; Rewrite { head = "makeTree"; pats = []; guard = None
             ; rhs = def "foldList" [ con "Leaf" []; def "insert" [] ] }

   ; Rewrite { head = "glueIn"; pats = [PVar "xs"; PVar "y"; PVar "zs" ]
             ; guard = None
             ; rhs = def "append" [ var "xs"; con "Cons" [ var "y"; var "zs" ] ] }

   ; Rewrite { head = "flatten"; pats = [ PCon ("Leaf", []) ]
             ; guard = None
             ; rhs = con "Nil" [] }

   ; Rewrite { head = "flatten"; pats = [ PCon ("Node", [PVar "lt"; PVar "x"; PVar "rt"]) ]
             ; guard = None
             ; rhs = def "glueIn" [ def "flatten" [var "lt"]; var "x"; def "flatten" [ var "rt" ] ] }

   ; Rewrite { head = "compose"; pats = [ PVar "f"; PVar "g"; PVar "a" ]
             ; guard = None
             ; rhs  = var_ "f" [ var_ "g" [ var "a" ] ] }

   ; Rewrite { head = "sort"; pats = []; guard = None
             ; rhs = def "compose" [ def "flatten" []; def "makeTree" [] ]
             }*)
  |]

(**********************************************************************)
type term_action =
  | ApplyCurrentRule of point
  | Undo
  | Redo

type action =
  | SelectRule of int
  | TermAction of int * term_action
  | UpdateUsername of string

type term_state =
  { term  : term With_history.t
  ; error : string option
  }

type state =
  { current_rule : int
  ; username     : string
  ; terms        : term_state array
  }

(* TODO:
   (a) multiple terms
   (b) per-term undo and redo
   (c) something that notices when you're done all the terms
*)

let of_int i =
  con (string_of_int i) []

let of_list l =
  List.fold_right (fun x xs -> con "Cons" [of_int x; xs]) l (con "Nil" [])

let of_list_ l =
  List.fold_right (fun x xs -> con "Cons" [x; xs]) l (con "Nil" [])

let mk_term term =
  { term = With_history.genesis term; error = None }

let lecture_terms =
  [ of_list [1;2;3;4]
  ; def "total" [of_list []]
  ; def "total" [of_list [1;2;3;4]]
  ; def "append" [of_list []; of_list [1;2;3]]
  ; def "append" [of_list [1;2;3]; of_list []]
  ; def "append" [of_list [1;2]; of_list [3;4]]
  ; def "total" [def "append" [of_list [1;2]; of_list [3;4]]]
  ]

let leaf = con "Leaf" []

let node x y z = con "Node" [x;y;z]

let rec to_nat = function
  | 0 -> con "Zero" []
  | n -> con "Succ" [ to_nat (n-1) ]

let lab_terms =
  [ def "iterNat" [ of_int 0; { head = Lambda ("x", def "(+)" [ var "x"; of_int 1]); args = [] }; to_nat 4 ]
  ; def "iterNat" [ to_nat 2; con "Succ" []; to_nat 2 ]
  ; def "plus" [ to_nat 2; to_nat 2 ]
  ; def "plus2" [ to_nat 2; to_nat 2 ]
  ; def "eqNat" [ to_nat 1; to_nat 2 ]
  ; def "eqNat" [ to_nat 2; to_nat 2 ]
    (*def "append" [ of_list [1;2]; of_list [3;4] ]
  ; def "append" [ def "append" [ of_list [1;2]; of_list [3;4] ]; of_list [5; 6] ]

  ; def "insert" [ of_int 3; leaf ]
  ; def "insert" [ of_int 2; def "insert" [ of_int 3; leaf ] ]
  ; def "insert" [ of_int 4; def "insert" [ of_int 3; leaf ] ]
  ; def "insert" [ of_int 6; (node
                                (node leaf (of_int 1) (node leaf (of_int 2) leaf)) (of_int 3)
                                (node (node leaf (of_int 4) leaf) (of_int 8) leaf)) ]

  ; def "foldList" [ of_int 0; def "(+)" []; of_list [ 1; 2; 3 ] ]
  ; def "foldList" [ of_list [5; 6]; con "Cons" []; of_list [1; 2; 3; 4] ]
  ; def "foldList" [ of_list []; def "append" []; of_list_ [of_list [1;2]; of_list [3;4]; of_list [5;6]] ]

  ; def "makeTree" [ of_list [] ]
  ; def "makeTree" [ of_list [4] ]
  ; def "makeTree" [ of_list [7; 1; 6; 2; 5; 3; 4] ]
  ; def "glueIn"   [ of_list [1;2;3]; of_int 4; of_list [5;6;7] ]
  ; def "glueIn"   [ def "glueIn" [ of_list [1]; of_int 2; of_list [3] ]
                   ; of_int 4
                   ; def "glueIn" [ of_list [5]; of_int 6; of_list [7] ]
                   ]
  ; def "flatten" [ leaf ]
  ; def "flatten" [ node leaf (of_int 3) leaf ]
  ; def "flatten" [ node
                      (node (node leaf (of_int 1) leaf) (of_int 2) (node leaf (of_int 3) leaf))
                      (of_int 4)
                      (node (node leaf (of_int 5) leaf) (of_int 6) (node leaf (of_int 7) leaf))
                  ]
  ; def "compose" [ con "Cons" [of_int 1]; con "Cons" [of_int 2]; of_list [] ]
  ; def "compose" [ def "compose" [ con "Cons" [of_int 1]; con "Cons" [of_int 2] ]
                  ; con "Cons" [of_int 3]
                  ; of_list [] ]
  ; def "compose" [ def "compose" []
                  ; def "compose" []
                  ; con "Cons" [of_int 1]
                  ; def "append" []
                  ; of_list [2]
                  ; of_list [3]
                  ]
      ;*)
    (* def "foldList" [ con "Nil" []
   *                  ; def "compose" [ con "Cons" []; def "(+)" [ of_int 1 ] ]
   *                  ; of_list [1;2;3]
   *                  ]
   * ; def "foldList" [ con "Nil" []
   *                  ; { head = Lambda ("x", { head = Lambda ("y", con "Cons" [ def "(+)" [ of_int 1; var "x" ]; var "y" ]); args = [] }); args = [] }
   *                  ; of_list [1;2;3]
   *                  ] *)
(*
  ; def "sort" [ of_list [] ]
  ; def "sort" [ of_list [4] ]
  ; def "sort" [ of_list [7;1;6;2;5;3;4] ]
  ; def "sort" [ of_list [1;2;3;4] ]
*)
  ]

let initial =
  { current_rule = 0
  ; username = ""
  ; terms =
      Array.of_list @@
      List.map mk_term @@
      lab_terms
(*
      [| { term = With_history.genesis @@
             def "insert"
               [ of_int 2
               ; def "insert"
                   [ of_int 3
                   ; con "Leaf" []
                   ]
               ]
         ; error = None }
       ; { term = With_history.genesis @@
             def "total"
               [ def "append"
                   [ of_list (List.map of_int [1;2;3;4;5;6])
                   ; of_list (List.map of_int [7;8;9])
                   ]
               ]
         ; error = None
         }
       ; { term =
             With_history.genesis @@
             def "compose"
               [ con "Cons" [con "1" []]
               ; con "Cons" [con "2" []]
               ; con "Nil" []
               ]
         ; error = None
         }
       ; { term =
             With_history.genesis @@
             def "append"
               [ con "Cons" [con "A" []; con "Cons" [con "B" []; con "Nil" []]]
               ; con "Nil" [] ]
         ; error = None
         }
      |]*)
  }


let update_term current_rule action state =
  match action with
    | ApplyCurrentRule point ->
       (match rules.(current_rule) with
         | `D -> state
         | `R rule ->
            match apply_at_point rule point with
              | None ->
                 { state with error = Some "Selected rule does not apply here!" }
              | Some term ->
                 { term  = With_history.update_to term state.term
                 ; error = None })
    | Undo ->
       (match With_history.undo state.term with
         | None      -> state
         | Some term -> { state with term })
    | Redo ->
       (match With_history.redo state.term with
         | None      -> state
         | Some term -> { state with term })

let update action state =
  match action with
    | UpdateUsername str ->
       { state with username = str }
    | SelectRule i ->
       { state with current_rule = i }
    | TermAction (idx, action) ->
       let terms =
         Array.mapi
           (fun j term_state ->
              if idx = j then update_term state.current_rule action term_state else { term_state with error = None })
           state.terms
       in
       { state with terms }

let rec render_head ?active =
  function
    | Var vnm -> Html.(span ~attrs:[A.class_ "variable"] (text vnm))
    | Con cnm -> Html.(span ~attrs:[A.class_ "constructor"] (text cnm))
    | Def dnm -> Html.(span ~attrs:[A.class_ "definition"] (text dnm))
    | Lambda (vnm, tm) ->
       Html.(span (text "(\\" ^^ text vnm ^^ text " → "
             ^^ render_term ?active tm ^^ text ")"))

and render_term ?(active=false) term =
  fold_term
    (fun point head args topmost ->
       let parens d =
         if topmost then d else Html.(text "(" ^^ d ^^ text ")")
       in
       let attrs =
         if active then
           Html.[ A.class_ "term active"
                ; E.onclick (ApplyCurrentRule point) ]
         else
           Html.[ A.class_ "term" ]
       in
       Html.span ~attrs
         begin
           match head, args with
             | Def "(+)", [ arg1; arg2 ] ->
                Html.(
                  parens begin
                    arg1 false
                    ^^ span ~attrs:[A.class_ "definition"] (text "+") ^^ arg2 false
                  end)
             | head, [] ->
                render_head ~active head
             | head_tm, args ->
                Html.(
                  parens begin
                    render_head ~active head_tm
                    ^^
                    concat_list (List.map (fun d -> (*text " " ^^*) d false) args)
                  end)
         end)
    term
    true

let rec render_pat pat =
  let open Html in
  match pat with
    | PVar vnm ->
       span ~attrs:[A.class_ "variable"] (text vnm)
    | PCon (cnm, []) ->
       span ~attrs:[A.class_ "constructor"] (text cnm)
    | PCon (cnm, args) ->
       text "("
       ^^ span ~attrs:[A.class_ "constructor"] (text cnm)
       ^^ text " "
       ^^ render_pats args
       ^^ text ")"
and render_pats pats =
  let open Html in
  concat_map (fun p -> text " " ^^ render_pat p) pats

let nbsp = "\xc2\xa0" (* NBSP in UTF-8 *)

let render_rule selection idx rule =
     let open Html in
     let classes =
       if idx = selection then "rule selected" else "rule"
     in
     match rule with
       | `R rule ->
          div ~attrs:[A.class_ classes; E.onclick (SelectRule idx)] begin
            (match rule with
              | Rewrite { head=rhead; pats; guard; rhs } ->
                 span ~attrs:[A.class_ "definition"] (text rhead)
                 ^^
                 render_pats pats
                 ^^
                 (match guard with
                   | None             -> empty
                   | Some (x, cmp, y) ->
                      text (Printf.sprintf " | %s %s %s" x (match cmp with `Gt -> ">" | `Le -> "<=") y))
                 ^^
                 text " = "
                 ^^
                 map (fun a -> TermAction (-1, a)) (render_term rhs)
              | Arithmetic ->
                 text "<< arithmetic >>"
              | Beta ->
                 text "<< apply >>")
          end
       | `D ->
          text nbsp

let render { current_rule; terms; username } =
  let open Ulmus.Dynamic_HTML in
  let rendered_rules =
    Array.to_list (Array.mapi (render_rule current_rule) rules)
  in
  (if Array.for_all (fun { term } -> is_ground (With_history.now term)) terms then
     begin
       h1 (text "Finished!")
     end
   else empty)
  ^^
  div ~attrs:[A.class_ "weerighter"]
    begin
      div ~attrs:[A.class_ "rules"]
        (h3 (text "Rules")
         ^^
         concat_list rendered_rules)
      ^^
      div ~attrs:[A.class_ "terms"] @@
      begin
        h3 (text "Programs")
        ^^
        concat_list
          (List.mapi
             (fun idx { term; error } ->
                map (fun a -> TermAction (idx, a)) @@
                let cur_term = With_history.now term in
                if is_ground cur_term then
                  begin
                    div ~attrs:[A.class_ "termblock"] @@
                    div ~attrs:[A.class_ "termdisplay"]
                      begin
                        div (render_term ~active:false cur_term)
                        ^^
                        div ~attrs:[A.class_ "termcomment"]
                          (text "Done!")
                      end
                  end
                else
                  begin
                    div ~attrs:[A.class_ "termblock"] @@
                    div ~attrs:[A.class_ "termdisplay"]
                      begin
                        div
                          (render_term ~active:true cur_term)
                        ^^
                        div ~attrs:[A.class_ "termcomment"]
                          begin
                            button ~attrs:[ A.disabled
                                              (not (With_history.has_past term))
                                          ; E.onclick Undo ]
                              (text "Undo")
                            ^^
                            button ~attrs:[ A.disabled
                                              (not (With_history.has_future term))
                                          ; E.onclick Redo ]
                              (text "Redo")
                          end
                      end
                    ^^
                    (match error with
                      | None -> empty
                      | Some error -> div ~attrs:[A.class_ "error"] (text error))
                  end)
             (Array.to_list terms))
      end
    end
