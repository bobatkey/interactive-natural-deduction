type formula =
  | Atom    of string
  | Implies of formula * formula
  | And     of formula * formula
  | Or      of formula * formula
  | Not     of formula

val a : formula
val b : formula
val c : formula
val d : formula

val (@->) : formula -> formula -> formula

(**********************************************************************)
type prooftree

type goal

val initial : formula -> prooftree

type rule = goal -> prooftree -> prooftree

val by_assumption : rule
val makeopen : rule

val implies_intro : rule
val implies_elim : formula -> rule

val conj_intro: rule
val conj_elim1 : formula -> rule
val conj_elim2 : formula -> rule

val disj_intro1 : rule
val disj_intro2 : rule
val disj_elim : formula -> formula -> rule

(**********************************************************************)

module Renderer (H : Html.S)
    (E : sig
       type action
       val makeselection : goal -> action H.attribute
     end)
  : sig
    val render : goal option -> prooftree -> E.action H.html
  end
