%{

open Formula

%}

%token <string> IDENT
%token ARROW
%token CONJ
%token DISJ
%token FALSE
%token EOF

%right ARROW
%left CONJ
%left DISJ

%start <Formula.t> whole_formula

%%

whole_formula:
  f=formula; EOF { f }

formula:
  | i=IDENT { Atom i }
  | f1=formula; ARROW; f2=formula { Implies (f1, f2) }
  | f1=formula; CONJ; f2=formula  { And (f1, f2) }
  | f1=formula; DISJ; f2=formula  { Or (f1, f2) }
  | FALSE                         { False }