open Unify

type rule =
  { parameters : (string * int) list
  ; name       : string
  ; conclusion : tm
  ; premises   : (tm list * tm) list
  }

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

(**********************************************************************)
(* (->,/\) propositional logic *)

let q mv      = Unk (mv, [])
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

let conj_elim1 =
  { parameters = [ "P", 0; "Q", 0 ]
  ; premises   = [ [] |- (q"P" &&& q"Q") ]
  ; conclusion = q"P"
  ; name       = "AND-E1"
  }

let conj_elim2 =
  { parameters = [ "P", 0; "Q", 0 ]
  ; premises   = [ [] |- (q"P" &&& q"Q") ]
  ; conclusion = q"Q"
  ; name       = "AND-E2"
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


(**********************************************************************)
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

  type error =
    [ `ConstructorMismatch
    | `IllSortedTerms
    | `OccursCheck
    | `VarConstructorMismatch
    | `VarMismatch
    | `VariableOutOfScope ]

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

  (* FIXME: make this generic by checking which rules unify with the
     goal. *)
  let rule_selection _assumps _goal =
    [ { group_name = "Implication"
      ; rules =
          [ Immediate impl_intro
          ; Immediate impl_elim
          ]
      }
    ; { group_name = "Conjunction"
      ; rules =
          [ Immediate conj_intro
          ; Immediate conj_elim_alt
          ; Immediate conj_elim1
          ; Immediate conj_elim2
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
