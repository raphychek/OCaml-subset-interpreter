%{
 
open Types

%}

%token <int> INT
%token <string> VAR
%token <string> UND /* token spécial pour gérer l'underscore */
%token LET REC IN CC FUN ARROW EVAL
%token PP INTO STAR
%token PRINT RAISE VB
%token REF DEREF ASSIGN UNIT
%token LP RP C TRY WITH E
%token PLUS TIMES DIV MINUS OU ET NOT FIRST SECOND
%token IF THEN ELSE SC
%token EQ LT LE GT GE NE
%token EOF

%nonassoc LET REC IN CC
%nonassoc FUN TRY WITH E
%nonassoc PP INTO
%right ARROW 
%left STAR
%left SC 
%nonassoc ASSIGN
%nonassoc IF THEN ELSE
%nonassoc EQ LT LE GT GE NE
%nonassoc C
%left PLUS MINUS OU ET
%left TIMES DIV FIRST SECOND
%nonassoc PRINT RAISE
%nonassoc UMINUS NOT
%nonassoc REF DEREF

%start main
%type <Types.expr> main
%type <Types.guy> typ

%%
/* point d'entrée du parser */
main:
  | EOF           { Unit } /* Quand on oublie d'écrire quelque chose dans le fichier (trico) */
  | toplevel EOF  { $1 }
;

/* let ou expression en surface */
toplevel:
  | expr          { $1 }
  | expr CC       { $1 }
  | letopcc       { $1 }
;

/* corps du parser, définition des expressions */
expr:
  | expr PLUS  expr               { Add(false, $1, $3) }
  | expr TIMES expr               { Mul(false, $1, $3) }
  | expr MINUS expr               { Min(false, $1, $3) }
  | expr DIV   expr               { Div($1, $3) }
  | MINUS expr %prec UMINUS       { Min(false, Cst(false,0), $2) }

  | appl             { $1 }
  | PRINT  sexpr     { Print(false,$2) }
  | FIRST  sexpr     { Fst($2) }
  | SECOND sexpr     { Snd($2) }

  | REF  sexpr       {Ref $2}
  | expr ASSIGN expr {Assign($1,$3)}
  | expr SC     expr { Let(false, Var(false,"_",WE), $1, $3) }
  | expr  C     expr { Couple($1, $3) }


  | LET aff  EQ   expr IN   expr  { Let(false,$2, $4, $6) } 
  | IF  bool THEN expr ELSE expr  { Ite($2, $4, $6) }

  | FUN         func              { $2 }
  | LET     VAR fonction IN expr  { Let(false, Var(false,$2,WE), $3, $5) }
  | LET REC VAR fonction IN expr  { LeR(Var(false,$3,WE), $4, $6) }
  | LET REC vat EQ  expr IN expr  { LeR($3, $5, $7) }

  | TRY expr WITH    excep ARROW expr { Try($2, $4, $6) }
  | TRY expr WITH VB excep ARROW expr { Try($2, $5, $7) }
  | RAISE  sexpr  { Raise $2 }
  | E      sexpr  { Exc $2 }
;

/* enchainement d'évaluation */
appl:
  | appl sexpr    { Appl($1, $2) }
  | sexpr         { $1 }
;

/* ce qu'on donne à manger à un let pour gérér les affectations multiples */
aff:
  | LP aff RP     { $2 }
  | UND           { Var(false, $1, WE) }
  | vat           { $1 }
  | aff C aff     { Couple($1, $3) }
;

/* argument de fonctions */
arg:
  | vat           { $1 }
  | LP  arg RP    { $2 }
  | arg C   arg   { Couple($1, $3) }
;

/* Nom de variable ou Nom de variable typé */
vat:
  | VAR           { Var(false, $1, WE) }
  | VAR PP typ    { Var(false, $1, $3) }
;

/* typage d'argument */
typ:
  | LP typ RP     { $2 }
  | INTO          { INT }
  | typ ARROW typ { FUN($1, $3) }
  | typ TIMES typ { TUP($1, $3) }
;

/* expressions qui peuvent passer en argument de fonctions */
sexpr:
  | UNIT          { Unit }
  | INT           { Cst(false, $1) }
  | vat           { $1 }
  | DEREF sexpr   { Deref $2 }
  | LP expr RP    { $2 }
;

/* expressions booléennes qui ne peuvent apparaitre que dans un if then else */
bool:
  | LP   bool RP  { $2 }
  | expr EQ expr  { Eq($1, $3) }
  | expr LE expr  { Le($1, $3) }
  | expr LT expr  { Lt($1, $3) }
  | expr GE expr  { Ge($1, $3) }
  | expr GT expr  { Gt($1, $3) }
  | expr NE expr  { Ne($1, $3) }

  | bool OU bool  { Ou($1, $3) }
  | bool ET bool  { Et($1, $3) }
  | NOT  bool     { Not($2) }
;

/* définition des let en surface */
letopcc:
  | LET aff     EQ  expr suite    { Let(false, $2, $4, $5) }
  | LET VAR     fonction suite    { Let(false, Var(false, $2, WE), $3, $4) }
  | LET REC VAR fonction suite    { LeR(Var(false,$3,WE), $4, $5) }
  | LET REC vat EQ  expr suite    { LeR($3, $5, $6) }
;

/* gère les fins de lignes du toplevel (absence ou non de ";;") */
suite:
  |               { Cst(false,0) }
  | CC            { Cst(false,0) }
  |    letopcc    { $1 }
  | CC toplevel   { $2 }
;

/* ce qui peut être reconnu dans le match de with */
excep:
  |   UND         { Var(false, $1, WE) }
  | E UND         { Var(false, $2, WE) }
  | E VAR         { Var(false, $2, WE) }
  | E INT         { Cst(false, $2) }
  | E MINUS INT   { Cst(false, -$3) }
;

/* couple variable/expression qui définit une fonction */
fonction:
  | arg fonction  { Fonc($1, $2) }
  | arg EQ expr   { Fonc($1, $3) }
;

/* fonction à plusieurs arguments avec FUN */
func:
  | arg func       { Fonc($1, $2) }
  | arg ARROW expr { Fonc($1, $3) }
;