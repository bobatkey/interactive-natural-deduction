type 'hole prooftree = private
  { formula : Formula.t
  ; status  : 'hole status
  }

and 'hole status = private
  | Open
  | Partial of 'hole
  | Rule    of string * 'hole proofbox list

and 'hole proofbox = private
  { subtree    : 'hole prooftree
  ; assumption : Formula.t option
  }

(**********************************************************************)
val initial : Formula.t -> 'hole prooftree

(**********************************************************************)
type goal = int list

type 'hole rule = goal -> 'hole prooftree -> 'hole prooftree

val by_assumption : 'hole rule
val makeopen : 'hole rule

val implies_intro : 'hole rule
val implies_elim : Formula.t -> 'hole rule

val conj_intro: 'hole rule
val conj_elim1 : Formula.t -> 'hole rule
val conj_elim2 : Formula.t -> 'hole rule

val disj_intro1 : 'hole rule
val disj_intro2 : 'hole rule
val disj_elim : Formula.t -> Formula.t -> 'hole rule

val false_elim : 'hole rule

val not_intro : 'hole rule
val not_elim : Formula.t -> 'hole rule

val raa : 'hole rule

val set_partial : 'hole -> 'hole rule
