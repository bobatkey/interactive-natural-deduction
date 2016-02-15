type t =
  | Atom    of string
  | Implies of t * t
  | And     of t * t
  | Or      of t * t
  | Not     of t
  | False
