type partial =
  | Partial_Implies_elim of string
  | Partial_Conj_elim1   of string
  | Partial_Conj_elim2   of string
  | Partial_Disj_elim    of string * string
  | Partial_Not_elim     of string

type prooftree = private
  { formula : Formula.t
  ; status  : status
  }

and status = private
  | Open
  | Rule    of string * proofbox list
  | Partial of partial

and proofbox = private
  { subtree    : prooftree
  ; assumption : Formula.t option
  }

(**********************************************************************)
val initial : Formula.t -> prooftree

(**********************************************************************)
type goal = int list

type rule = goal -> prooftree -> prooftree

val by_assumption : rule
val makeopen : rule

val implies_intro : rule
val implies_elim : Formula.t -> rule

val conj_intro: rule
val conj_elim1 : Formula.t -> rule
val conj_elim2 : Formula.t -> rule

val disj_intro1 : rule
val disj_intro2 : rule
val disj_elim : Formula.t -> Formula.t -> rule

val false_elim : rule

val not_intro : rule
val not_elim : Formula.t -> rule

val raa : rule

val set_partial : partial -> rule
