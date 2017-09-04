{
type token =
  | SET
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LSQBRACK
  | RSQBRACK
  | EQUALS
  | COLON
  | SEMICOLON
  | DOT
  | ARROW
  | COMMA
  | ASTERISK
  | UNDERSCORE
  | BACKSLASH
  | BOOL
  | TRUE
  | FALSE
  | NAT
  | ZERO
  | SUCC
  | HASH_RECURSION
  | HASH_ELIMQ
  | SAME_CLASS
  | SLASH
  | BY_CASES
  | FOR
  | REFL
  | COERCE
  | COH
  | SUBST
  | FUNEXT
  | DEFINE
  | AS
  | HASH_FST
  | HASH_SND
  | IDENT of string
  | EOF

  | COMMENT
  | INVALID
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id_cont = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']
let id      = ['a'-'z' 'A'-'Z'] id_cont* | ['_'] id_cont+

rule token = parse
| white        { token lexbuf }
| "Set"        { `Token, SET }
| '('          { `Token, LPAREN }
| ')'          { `Token, RPAREN }
| '{'          { `Token, LBRACE }
| '}'          { `Token, RBRACE }
| '['          { `Token, LSQBRACK }
| ']'          { `Token, RSQBRACK }
| '='          { `Token, EQUALS }
| ':'          { `Token, COLON }
| ';'          { `Token, SEMICOLON }
| '.'          { `Token, DOT }
| "->"         { `Token, ARROW }
| ","          { `Token, COMMA }
| "*"          { `Token, ASTERISK }
| '_'          { `Token, UNDERSCORE }
| "\\"         { `Token, BACKSLASH }
| "Bool"       { `Token, BOOL }
| "True"       { `Token, TRUE }
| "False"      { `Token, FALSE }
| "Nat"        { `Token, NAT }
| "Zero"       { `Token, ZERO }
| "Succ"       { `Token, SUCC }
| "#recursion" { `Token, HASH_RECURSION }
| "#elimq"     { `Token, HASH_ELIMQ }
| "same-class" { `Token, SAME_CLASS }
| "/"          { `Token, SLASH }
| "by_cases"   { `Token, BY_CASES }
| "for"        { `Token, FOR }
| "refl"       { `Token, REFL }
| "coerce"     { `Token, COERCE }
| "coherence"  { `Token, COH }
| "subst"      { `Token, SUBST }
| "funext"     { `Token, FUNEXT }
| "define"     { `Token, DEFINE }
| "as"         { `Token, AS }
| "#fst"       { `Token, HASH_FST }
| "#snd"       { `Token, HASH_SND }
| id           { `Token, IDENT (Lexing.lexeme lexbuf) }
| "(*"         { `Comment 1, COMMENT }
| eof          { `Token,     EOF }
| _            { `Token,     INVALID }

and comment n = parse
| [^'*']* "*)"    { (if n = 1 then `Token else `Comment (n-1)), COMMENT }
| [^'*']* "(*"    { `Comment (n+1), COMMENT }
| [^'*']* "*"     { `Comment n, COMMENT }
| [^'*']+         { `Comment n, COMMENT }
| eof             { `Comment n, EOF }

{
type lexer = [ `Token | `Comment of int ]

let equal_lexer = (=)

let initial = `Token

let token = function
  | `Token     -> token
  | `Comment n -> comment n

let token_is_eoi = function EOF -> true | _ -> false

let style_of_token = function
  | SET | BOOL | TRUE | FALSE | NAT | ZERO | SUCC
  | REFL | COH | SUBST | FUNEXT | SAME_CLASS
  | SLASH ->
     "hl-constructor"
  | BY_CASES | HASH_ELIMQ | HASH_RECURSION | HASH_FST | HASH_SND | COERCE | FOR ->
     "hl-eliminator"
  | DEFINE | AS ->
     "hl-definitions"
  | LPAREN | RPAREN | LBRACE | RBRACE | LSQBRACK | RSQBRACK
  | EQUALS | COLON | SEMICOLON | DOT | COMMA
  | ARROW | ASTERISK | BACKSLASH | UNDERSCORE ->
     "hl-punctuation"
  | IDENT _ ->
     "hl-identifier"
  | COMMENT ->
     "hl-comment"
  | INVALID ->
     "hl-invalid"
  | EOF ->
     assert false

}