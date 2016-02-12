type t =
  | Atom    of string
  | Implies of t * t
  | And     of t * t
  | Or      of t * t
  | Not     of t
  | False

val (@->) : t -> t -> t

val to_string : ?unicode:bool -> t -> string
