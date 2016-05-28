type state

type action

val render : state -> action Dynamic_HTML.html

val update : action -> state -> state

val initial : Formula.t -> state
