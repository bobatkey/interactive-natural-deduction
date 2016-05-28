type t = Formula_ast.t =
  | Atom    of string
  | Implies of t * t
  | And     of t * t
  | Or      of t * t
  | Not     of t
  | True
  | False

let (@->) f1 f2 = Implies (f1, f2)

(**********************************************************************)
let parens l1 s l2 =
  if l1 > l2 then "("^s l1^")" else s l1

let drop f l =
  f (l-1)

let rec to_string ?(unicode=true) f =
  let arrow = if unicode then " → " else " -> " in
  let conj  = if unicode then " ∧ " else " /\\ " in
  let disj  = if unicode then " ∨ " else " \\/ " in
  let true_  = if unicode then "⊤" else "True" in
  let false_ = if unicode then "⊥" else "False" in
  let text x l = x in
  let (^^) x y l = x l ^ y l in
  let rec to_string = function
    | Atom a ->
       text a
    | Implies (f1, f2) ->
       parens 10 begin%concatenate
         drop (to_string f1);
         text arrow;
         to_string f2
       end
    | And (f1, f2) ->
       parens 08 begin%concatenate
         drop (to_string f1);
         text conj;
         to_string f2
       end
    | Or (f1, f2) ->
       parens 07 begin%concatenate
         drop (to_string f1);
         text disj;
         to_string f2
       end
    | Not f ->
       begin%concatenate
         text "¬";
         parens 0 (to_string f)
       end
    | True ->
       text true_
    | False ->
       text false_
  in
  to_string f 10

(**********************************************************************)
let of_string string =
  let lb = Lexing.from_string string in
  match Formula_parser.whole_formula Formula_lexer.token lb with
    | exception Formula_parser.Error -> None
    | f -> Some f

(**********************************************************************)
let is_implication = function Implies _ -> true | _ -> false
let is_conjunction = function And _ -> true | _ -> false
let is_disjunction = function Or _ -> true | _ -> false
let is_negation    = function Not _ -> true | _ -> false
let is_truth       = function True -> true | _ -> false
