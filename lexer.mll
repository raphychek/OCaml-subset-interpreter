{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n'] { token lexbuf }
  | eof             { EOF }

  | "()"            { UNIT }
  | '('             { LP }
  | ')'             { RP }
  | '='             { EQ }
  | '>'             { GT }
  | ">="            { GE }
  | '<'             { LT }
  | "<="            { LE }
  | "<>"            { NE }
  | "->"            { ARROW }

  | "ref"           { REF }
  | "!"             { DEREF }
  | ":="            { ASSIGN }

  | ':'             { PP }

  | ","             { C }
  | "fst"           { FIRST }
  | "snd"           { SECOND }

  | "try"           { TRY }
  | "with"          { WITH }
  | "E"             { E }
  | "raise"         { RAISE }

  | "not"           { NOT }
  | "&&"            { ET }
  | "||"            { OU }

  | '+'             { PLUS }
  | '*'             { TIMES }
  | '/'             { DIV }
  | '-'             { MINUS }
  | ['0'-'9']+ as s { INT (int_of_string s) }

  | "let"           { LET }
  | "rec"           { REC }
  | "fun"           { FUN }
  | "in"            { IN }
  | ";;"            { CC }
  | ';'             { SC }
  | '|'             { VB }

  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }

  | "prInt"         { PRINT }
  | "int"           { INTO }

  | ['a'-'z']['a'-'z' 'A'-'Z' '_' ''' '1'-'9']* as s { VAR s }
  | ['_']['a'-'z' 'A'-'Z' '_' ''' '1'-'9']+     as s { VAR s }
  | ['_']                                       as s { UND (String.make 1 s) }