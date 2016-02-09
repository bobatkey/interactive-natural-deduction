{
open Formula_parser
}

let white   = [' ' '\t' ]+
let ident   = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
| white   { token lexbuf }
| "->"    { ARROW }
| "/\\"   { CONJ }
| "\\/"   { DISJ }
| "("     { LPAREN }
| ")"     { RPAREN }
| "False" { FALSE }
| ident   { IDENT (Lexing.lexeme lexbuf) }
| eof     { EOF }
| _       { raise Error }
