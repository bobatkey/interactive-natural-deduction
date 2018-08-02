let index_of x xs =
  let rec find idx = function
    | []      -> None
    | y :: xs -> if x = y then Some idx else find (idx+1) xs
  in
  find 0 xs

type mvar = string

let mvar_equal = String.equal

module MVarMap = Map.Make (String)

type constr = string

let constr_equal = String.equal

(**********************************************************************)
type tm =
  | Var  of int
  | Con of constr * binding_tm list
  | Unk of mvar * int list

and binding_tm =
  { binders : int
  ; term    : tm
  }

let nobind term =
  { binders = 0; term }

module Term = struct
  type t = tm

  let rec to_string = function
    | Var i          -> "b" ^ string_of_int i
    | Con (c, [])    -> c
    | Con (c, tms)   -> c ^ "(" ^ String.concat "," (List.map bindings_tm_to_string tms) ^ ")"
    | Unk (mv, [])   -> "?" ^ mv
    | Unk (mv, vars) -> "?" ^ mv ^ "[" ^ String.concat "," (List.map (Printf.sprintf "b%d") vars) ^ "]"
  and bindings_tm_to_string = function
    | {binders = 0; term} -> to_string term
    | {binders = n; term} -> Printf.sprintf "%d.%s" n (to_string term)
end

(**********************************************************************)
(*
let lam m   = Con ("lam", [{binders = 1; term = m}])
let var i   = Var i
let app m n = Con ("app", [nobind m; nobind n])
let const c = Con (c, [])

(* The ground term '(\x. f x) (x y)' *)
let redex =
  app (lam (app (const "f") (var 0))) (app (const "x") (const "y"))

(* The pattern *)
let redex_pat =
  app (lam (Unk ("X", [0]))) (Unk ("Y", []))

let pat =
  app (Unk ("X", [])) (Unk ("X", []))

let tm =
  app (lam (var 0)) (lam (var 0))
*)

(**********************************************************************)
(* Preparation of a term to be a definition: remap all the variables
   in the term to be bound by the definition of the metavariable. *)

(** [prepare args tm] *)
let prepare vars tm =
  let prepare_var offset i =
    if i < offset then i
    else
      let i = i - offset in
      match index_of i vars with
        | None -> raise Not_found
        | Some v -> v + offset
  in
  let rec prepare offset = function
    | Var i ->
       Var (prepare_var offset i)
    | Con (c, ms) ->
       Con (c, List.map (prepare_binding_tm offset) ms)
    | Unk (mv, vars) ->
       (* FIXME: do an occurs check here *)
       (* FIXME: could do pruning here: if any of the variables
          required are not accessble, then create new metavariables to
          replace the old ones, but with fewer variables. *)
       Unk (mv, List.map (prepare_var offset) vars)

  and prepare_binding_tm offset { binders; term } =
    { binders; term = prepare (offset+binders) term }
  in
  try Ok (prepare 0 tm) with Not_found -> Error "scope failure"

(**********************************************************************)
(* Instantiation of metavariable definitions *)
let instantiate vars tm =
  let instantiate_var offset i =
    if i < offset then i
    else List.nth vars (i - offset) + offset
  in
  let rec instantiate offset = function
    | Var i ->
       Var (instantiate_var offset i)
    | Con (c, ms) ->
       Con (c, List.map (instantiate_binding_tm offset) ms)
    | Unk (mv, vars) ->
       Unk (mv, List.map (instantiate_var offset) vars)

  and instantiate_binding_tm offset { binders; term } =
    { binders; term = instantiate (offset+binders) term }
  in
  instantiate 0 tm

(**********************************************************************)
(* Substituting for a metavariable *)
let rec subst_mv (mv, m as defn) = function
  | Var i ->
     Var i
  | Con (c, ms) ->
     Con (c, List.map (subst_mv_binding defn) ms)
  | Unk (mv', vs) ->
     if mvar_equal mv mv' then instantiate vs m else Unk (mv', vs)

and subst_mv_binding defn { binders; term } =
  { binders; term = subst_mv defn term }

let apply_subst subst f =
  List.fold_right subst_mv subst f

(* ?X[x,y,z] := c(x,y)

   ?X[a,b,c] ==> c(a,b)

   each metavariable has an arity, which ought to be respected. When
   turning a term into a definition for a metavariable, we renumber
   the variables to be easily substitutable: go through the definition
   and look up that variable in the instantiation list. *)

(**********************************************************************)
(* Unification:

   ?X[xs]    =?= M : n           ==>   ?X := M   if xs <= fv(M), ?X \notin fmv(M)
      .. and symmetrically
   Lam M     =?= Lam N : n       ==>   M =?= N : n+1
   App(h,Ms) =?= App (h,Ns) : n  ==>   zipWith (=?= : n) Ms Ns
   _         =?= _               ==>   _|_
*)

let update_subst binding =
  List.map (fun (mv, m) -> (mv, subst_mv binding m))

let update_problems binding =
  List.map (fun (m, m') -> (subst_mv binding m, subst_mv binding m'))

let rec unify subst = function
  | [] ->
     Ok subst

  (* FIXME: what to do if we have ?X[args1] =?= ?X[args2] ??

     if args1 = args2, then we proceed with no more
     information. Otherwise, we find a way of instantiating ?X to make
     the argument lists line up (special case of pruning?). *)
  | (Unk (mv, args1), Unk (mv', args2)) :: problems when mvar_equal mv mv' ->
     if args1 = args2 then
       unify subst problems
     else
       failwith "FIXME: self-unification of metavars"

  | (Unk (mv, args), m | m, Unk (mv, args)) :: problems ->
     (* FIXME: occurs check *)
     (match prepare args m with
       | Error e ->
          Error e
       | Ok m ->
          let defn     = (mv, m) in
          let subst    = defn :: update_subst defn subst in
          let problems = update_problems defn problems in
          unify subst problems)

  | (Var i, Var j) :: problems ->
     if i = j then
       unify subst problems
     else
       Error "object variable mismatch"

  | (Con (c1, ms1), Con (c2, ms2)) :: problems ->
     if constr_equal c1 c2 then
       zip_problems subst ms1 ms2 problems
     else
       Error "constructor mismatch"

  | (Var _, Con _ | Con _, Var _) :: _ ->
     Error "object variable =/= constructed term"

and zip_problems subst ms ms' problems = match ms, ms' with
  | [], [] ->
     unify subst problems

  | {binders = b1; term = m1}::ms1, {binders = b2; term = m2}::ms2 ->
     if b1 = b2 then
       zip_problems subst ms1 ms2 ((m1,m2)::problems)
     else
       Error "binder mismatch (terms must be ill-sorted)"

  | _, _ ->
     Error "length mismatch (terms must be ill-sorted)"



(**********************************************************************)
type rule =
  { parameters : (string * int) list
  ; name       : string
  ; conclusion : tm
  ; premises   : (tm list * tm) list
  }

let q mv = Unk (mv, [])
let (&&&) a b = Con ("AND", [nobind a; nobind b])
let (-->) a b = Con ("IMP", [nobind a; nobind b])
let v x       = Con (x, [])

let goal = (v"A" &&& v"B") --> (v"B" &&& v"A")

let (|-) a f = (a,f)

let impl_intro =
  (* (?P ==> ?Q) ==> ?P -> ?Q *)
  { parameters = [ "P", 0; "Q", 0 ]
  ; premises   = [ [q"P"] |- q"Q" ]
  ; conclusion = q"P" --> q"Q"
  ; name       = "IMP-I"
  }

let impl_elim =
  { parameters = [ "P", 0; "Q", 0 ]
  ; premises   = [ [] |- q"P" --> q"Q"; [] |- q"P" ]
  ; conclusion = q"Q"
  ; name       = "IMP-E"
  }

let conj_elim_alt =
  { parameters = [ "P", 0; "Q", 0; "R", 0 ]
  ; premises =
      [ []           |- (q"P" &&& q"Q")
      ; [q"P"; q"Q"] |- q"R"
      ]
  ; conclusion =
      q"R"
  ; name = "AND-E"
  }

let conj_intro =
  { parameters = [ "P", 0; "Q", 0 ]
  ; premises   = [ [] |- q"P" ; [] |- q"Q" ]
  ; conclusion = q"P" &&& q"Q"
  ; name       = "AND-I"
  }


(*
let disj_elim =
  (* ?P \/ ?Q ==> (?P ==> ?R) ==> (?Q ==> ?R) ==> ?R *)
  { parameters = ["P", 0; "Q", 0; "R", 0]
  ; conclusion = Unk ("R", [])
  ; premises =
      [ ([], Con ("OR", [nobind (Unk ("P",[])); nobind (Unk ("Q",[]))]))
      ; ([Unk ("P",[])], Unk ("R", []))
      ; ([Unk ("Q",[])], Unk ("R", []))
      ]
  }
*)


(* The Isabelle style presentation is nice, but is there any need for
   any deeper arrow nesting? *)

(* Plan, when applying a rule to the current goal,

   1. Freshen all the meta variables
   2. Unify the goal with the conclusion of the rule
   3. Create the premises

   When attempting an assumption use, unify the current goal
   with the chosen assumption.

   After each unification, we push the computed substitution
   through all the formulas in the proof tree (and the holes?).
*)

(* FIXME: thread the fresh name generation through the freshener. *)
let freshname =
  let next = ref 0 in
  fun () ->
    let name = "X" ^ string_of_int !next in incr next; name

let list f map xs =
  List.fold_right
    (fun x (ys, map) -> let y, map = f map x in (y::ys, map))
    xs
    ([], map)
    

let rec freshen_tm map = function
  | Var i ->
     Var i, map
  | Con (c, tms) ->
     let tms, map = list freshen_binding_tm map tms in
     Con (c, tms), map
  | Unk (mv, vars) ->
     let nm, map =
       match MVarMap.find mv map with
         | exception Not_found ->
            let nm = freshname () in nm, MVarMap.add mv nm map
         | nm ->
            nm, map
     in
     Unk (nm, vars), map

and freshen_binding_tm map {binders; term} =
  let term, map = freshen_tm map term in
  { binders; term }, map

let freshen_premise map (assumps, goal) =
  let goal,    map = freshen_tm map goal in
  let assumps, map = list freshen_tm map assumps in
  (assumps, goal), map

let freshen_premises map =
  list freshen_premise map

module System = struct

  type formula = tm

  type assumption = tm

  (* a substitution *)
  type update = (mvar * tm) list

  let empty_update = []

  let update_formula =
    apply_subst

  let update_assumption =
    apply_subst

  type nonrec rule = rule

  type error = string

  let apply rule goal =
    let                  map = MVarMap.empty in
    let rule_conclusion, map = freshen_tm map rule.conclusion in
    let rule_premises,   _   = freshen_premises map rule.premises in
    match unify [] [rule_conclusion, goal] with
      | Ok subst ->
         let subgoals =
           List.map
             (fun (assumps, subgoal) ->
                List.map (apply_subst subst) assumps, apply_subst subst subgoal)
             rule_premises
         in
         Ok (subgoals, subst)
      | Error err ->
         Error err

  let unify_with_assumption goal assump =
    unify [] [goal, assump]

  let name_of_rule r =
    r.name

end

module UI = struct
  module Calculus = System

  type partial = { impossible : 'a. 'a }

  let name_of_partial x = x.impossible

  type rule_selector =
    | Immediate of Calculus.rule
    | Disabled  of string
    | Partial   of partial

  type selector_group =
    { group_name : string
    ; rules      : rule_selector list
    }

  let rule_selection _assumps _goal =
    [ { group_name = "Rules"
      ; rules =
          [ Immediate impl_intro
          ; Immediate impl_elim
          ; Immediate conj_intro
          ; Immediate conj_elim_alt
          ]
      }
    ]

  (* Partial proof presentation *)
  type partial_formula_part =
    | T of string
    | I of string * (string -> partial)
    | F of Calculus.formula

  type partial_premise =
    { premise_formula    : partial_formula_part list
    ; premise_assumption : string option
    }

  type partial_presentation =
    { premises : partial_premise list
    ; apply    : Calculus.rule option
    }

  let present_partial _ x = x.impossible
end
