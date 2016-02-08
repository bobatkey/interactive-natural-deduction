type formula =
  | Atom    of string
  | Implies of formula * formula
  | And     of formula * formula
  | Or      of formula * formula
  | Not     of formula

let parens l1 s l2 =
  if l1 > l2 then "("^s l1^")" else s l1

let drop f l =
  f (l-1)

let rec string_of_formula f =
  let text x l = x in
  let (^^) x y l = x l ^ y l in
  let rec to_string = function
    | Atom a ->
       text a
    | Implies (f1, f2) ->
       parens 10 (drop (to_string f1) ^^ text " → " ^^ to_string f2)
    | And (f1, f2) ->
       parens 08 (drop (to_string f1) ^^ text " ∧ " ^^ to_string f2)
    | Or (f1, f2) ->
       parens 07 (drop (to_string f1) ^^ text " ∨ " ^^ to_string f2)
    | Not f ->
       text "¬" ^^ parens 0 (to_string f)
  in
  to_string f 10

let (@->) f1 f2 = Implies (f1, f2)

(**********************************************************************)
type prooftree =
  { formula : formula
  ; status  : [ `Open
              | `Rule of proofbox list
              ]
  }

and proofbox =
  { subtree    : prooftree
  ; assumption : formula option
  }

let initial formula =
  { formula; status = `Open }

(**********************************************************************)
let a = Atom "A"
let b = Atom "B"
let c = Atom "C"
let d = Atom "D"

let test =
  { formula = And (a, b) @-> (a @-> c) @-> (b @-> d) @-> And (c,d)
  ; status = `Rule
        [ { assumption = Some (And (a, b))
          ; subtree = 
              { formula = (a @-> c) @-> (b @-> d) @-> And (c,d)
              ; status = `Rule
                    [ {assumption = Some (a @-> c)
                      ; subtree =
                          { formula = (b @-> d) @-> And (c,d)
                          ; status = `Rule
                                [ { assumption = Some (b @-> d)
                                  ; subtree = 
                                      { formula =  And (c,d)
                                      ; status = `Rule
                                            [ { assumption = None
                                              ; subtree =
                                                  { formula = c
                                                  ; status = `Rule
                                                        [ { assumption = None; subtree = { formula = a @-> c; status = `Open }}
                                                        ; { assumption = None
                                                          ; subtree =
                                                              { formula = a
                                                              ; status = `Rule
                                                                    [ { assumption = None
                                                                      ; subtree = { formula = And (a,b); status = `Rule [] } } ]
                                                              }
                                                          }
                                                        ]
                                                  }
                                              }
                                            ; { assumption = None
                                              ; subtree =
                                                  { formula = d
                                                  ; status = `Rule
                                                        [ { assumption = None; subtree = { formula = b @-> d; status = `Open }}
                                                        ; { assumption = None
                                                          ; subtree =
                                                              { formula = b
                                                              ; status = `Rule
                                                                    [ { assumption = None
                                                                      ; subtree = { formula = And (a,b); status = `Rule [] } } ]
                                                              }
                                                          }
                                                        ]
                                                  }
                                              }
                                            ]
                                      }
                                  }
                                ]
                          }
                      }
                    ]
              }
          }
        ]
  }

(*
   Plan:

   - maintain a proof state as a tree
   - also: selection information: at most one formula can be selected
     - when selected: show the rules that can be applied
     - the appropriate introduction rule
     - all of the elimination rules (will require the formula being eliminated to be entered: or use meta-variables?)
     - the assumption rule, if appropriate

   If no prompt, then can select any open goal
   If a goal is selected, then offer selection of what to do
   If an eliminator is selected, then prompt for the eliminated formula
     (not showing the controls)
*)

(*******************************************************************************)
let update_nth f i l =
  let rec update i l = match i, l with
    | 0, x::xs -> f x::xs
    | n, x::xs -> x::update (n-1) xs
    | _, _     -> invalid_arg "update_nth"
  in
  update i l

type goal = int list

type rule = goal -> prooftree -> prooftree

let update_tree update_node path tree =
  let rec update_tree assumps path node =
    match path, node.status with
      | [], _ ->
         (match update_node assumps node.formula with
           | `Open ->
              { node with status = `Open }
           | `Rule premises ->
              let premises =
                List.map
                  (fun (assumption,formula) ->
                     {assumption;subtree={formula;status=`Open}})
                  premises
              in
              { node with status = `Rule premises })
      | pos::path, `Rule premises ->
         let premises = update_nth (update_box assumps path) pos premises in
         { node with status = `Rule premises }
      | _, _ ->
         invalid_arg "mismatched path" (* This shouldn't happen *)
  and update_box assumps path {assumption;subtree} =
    match assumption with
      | None   -> {assumption; subtree = update_tree assumps path subtree}
      | Some f -> {assumption; subtree = update_tree (f::assumps) path subtree}
  in
  update_tree [] (List.rev path) tree

let implies_intro =
  update_tree (fun _assumps formula -> match formula with
      | Implies (f1, f2) -> `Rule [ (Some f1, f2) ]
      | _ -> invalid_arg "incorrectly applied implies_intro")

let implies_elim f1 =
  update_tree (fun _assumps f2 -> `Rule [ (None, f1 @-> f2); (None, f1) ])

let conj_intro =
  update_tree (fun _assumps -> function
      | And (f1, f2) -> `Rule [ (None, f1); (None, f2) ]
      | _ -> invalid_arg "incorrectly applied conj_intro")

let conj_elim1 f2 =
  update_tree (fun _assumps f1 -> `Rule [ (None, And (f1, f2)) ])

let conj_elim2 f1 =
  update_tree (fun _assumps f2 -> `Rule [ (None, And (f1, f2)) ])

let disj_intro1 =
  update_tree (fun _assumps -> function
      | Or (f1, f2) -> `Rule [ (None, f1) ]
      | _ -> invalid_arg "incorrectly applied disj_intro1")

let disj_intro2 =
  update_tree (fun _assumps -> function
      | Or (f1, f2) -> `Rule [ (None, f2) ]
      | _ -> invalid_arg "incorrectly applied disj_intro2")

let disj_elim f1 f2 =
  update_tree (fun _assumps f ->
      `Rule [ (None, Or (f1, f2))
            ; (Some f1, f)
            ; (Some f2, f)
            ])

let by_assumption =
  update_tree (fun assumps f ->
      if List.mem f assumps then `Rule []
      else invalid_arg "assumption not applicable")

let makeopen =
  update_tree (fun _assumps _f -> `Open)

(**********************************************************************)
module Renderer
    (H : Html.S)
    (E : sig
       type action
       val makeselection : int list -> action H.attribute
     end)
  : sig
    val render : int list option -> prooftree -> E.action H.html
  end =
struct
  open H
  
  let proofbox elements =
    div ~attrs:[A.class_ "proofbox"] elements

  let premisebox elements =
    div ~attrs:[A.class_ "premisebox"] elements

  let formulabox path formula =
    button ~attrs:[ A.class_ "formulabox"
                  ; E.makeselection path ]
      (text (string_of_formula formula))

  let openformula path formula =
    button ~attrs:[ A.class_ "formulabox open"
                  ; E.makeselection path ]
      (text (string_of_formula formula))

  let selectedformulabox path formula =
    button ~attrs:[ A.class_ "formulabox selected"
                  ; E.makeselection path ]
      (text (string_of_formula formula))

  let openselectedformulabox path formula =
    button ~attrs:[ A.class_ "formulabox open selected"
                  ; E.makeselection path ]
      (text (string_of_formula formula))

  let render selection proof =
    let rec render path {formula;status} =
      proofbox begin%concat
        match status with
          | `Open when selection = Some path ->
             openselectedformulabox path formula
          | `Open ->
             openformula path formula
          | `Rule premises ->
             premisebox begin
               premises
               |> List.mapi (fun i premise -> render_box (i::path) premise)
               |> concat_list
             end;
             if selection = Some path then
               selectedformulabox path formula
             else
               formulabox path formula
      end

    and render_box path {subtree;assumption} = match assumption with
      | Some assumption ->
         div ~attrs:[A.class_ "assumptionbox"] begin%concat
           div ~attrs:[A.class_ "assumption"] begin%concat
             text "assuming ";
             text (string_of_formula assumption);
             text " ..."
           end;
           render path subtree
         end
      | None ->
         render path subtree
    in
    render [] proof
end
