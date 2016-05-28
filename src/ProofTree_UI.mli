type state = ProofTree.prooftree

type action

val render : state -> action Dynamic_HTML.html

val update : action -> state -> state
