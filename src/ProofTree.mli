val parse_formula : string -> Formula.t option

type prooftree

type goal

val initial : Formula.t -> prooftree

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

(**********************************************************************)

module UI : sig
  type state = prooftree
  type action
  val render : state -> action Dynamic_HTML.html
  val update : action -> state -> state
end
