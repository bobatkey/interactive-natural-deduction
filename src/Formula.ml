type t =
  | Atom    of string
  | Implies of t * t
  | And     of t * t
  | Or      of t * t
  | Not     of t

let parens l1 s l2 =
  if l1 > l2 then "("^s l1^")" else s l1

let drop f l =
  f (l-1)

let rec to_string f =
  let text x l = x in
  let (^^) x y l = x l ^ y l in
  let rec to_string = function
    | Atom a ->
       text a
    | Implies (f1, f2) ->
       parens 10 (drop (to_string f1) ^^ text " → " ^^ to_string f2)
    | And (f1, f2) ->
       parens 08 (drop (to_string f1) ^^ text " ∧ " ^^ to_string f2)
    | Or (f1, f2) ->
       parens 07 (drop (to_string f1) ^^ text " ∨ " ^^ to_string f2)
    | Not f ->
       text "¬" ^^ parens 0 (to_string f)
  in
  to_string f 10

let (@->) f1 f2 = Implies (f1, f2)

