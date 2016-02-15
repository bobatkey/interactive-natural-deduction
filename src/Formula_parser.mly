%{

open Formula_ast

%}

%token <string> IDENT
%token ARROW
%token CONJ
%token DISJ
%token FALSE
%token NOT
%token LPAREN
%token RPAREN
%token EOF

%right ARROW
%left CONJ
%left DISJ
%left NOT

%start <Formula_ast.t> whole_formula

%%

whole_formula:
  f=formula; EOF { f }

formula:
  | i=IDENT { Atom i }
  | f1=formula; ARROW; f2=formula { Implies (f1, f2) }
  | f1=formula; CONJ; f2=formula  { And (f1, f2) }
  | f1=formula; DISJ; f2=formula  { Or (f1, f2) }
  | NOT; f=formula                { Not f }
  | LPAREN; f=formula; RPAREN     { f }
  | FALSE                         { False }
