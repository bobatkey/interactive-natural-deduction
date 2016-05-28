type t =
  | Atom    of string
  | Implies of t * t
  | And     of t * t
  | Or      of t * t
  | Not     of t
  | True
  | False

val (@->) : t -> t -> t

val to_string : ?unicode:bool -> t -> string

val of_string : string -> t option

val is_implication : t -> bool
val is_conjunction : t -> bool
val is_disjunction : t -> bool
val is_negation    : t -> bool
val is_truth       : t -> bool
