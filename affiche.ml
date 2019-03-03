open Types

(* Fonctions d'affichage pour les différentes représentations *)

(* fonction d'affichage programme Fouine *)
let rec aff_expr e = match e with
  | Cst(boule,k)   -> print_int k
  | Var(boule,s,t) -> print_string s
  | Unit  -> print_string "()"
  
  | Add(boule,e1,e2) -> aff_aux " + " e1 e2
  | Mul(boule,e1,e2) -> aff_aux " * " e1 e2
  | Div(e1,e2) -> aff_aux " / " e1 e2
  | Min(boule,e1,e2) -> aff_aux " - " e1 e2

  | Couple(a,b) -> aff_aux " , " a b
  | Fst(c) -> print_string "fst(" ; aff_expr c; print_string ")"
  | Snd(c) -> print_string "snd(" ; aff_expr c; print_string ")"

  | Try(e1,c,e2) -> aff_try e1 c e2
  | Raise(e)     -> print_string "raise (" ; aff_expr e ; print_string ")"
  | Exc(e)       -> print_string "(E " ; aff_expr e ; print_string ")"

  | Ite(e1,e2,e3)     -> aff_ite e1 e2 e3
  | Let(boule,Var(boul,"_",t),e2,e3) -> aff_expr e2 ; print_string ";\n" ; aff_expr e3
  | Let(boule,e1,e2,e3)     -> aff_let e1 e2 e3
  | LeR(e1,e2,e3)      -> aff_ler e1 e2 e3

  | Print(boule,e) -> print_string "prInt (" ; aff_expr e ; print_string ")" 

  | Fonc(x,e)   -> print_string "(fun " ; aff_expr x ; print_string " -> " ; aff_expr e ; print_string ")"
  | Appl(e1,e2) -> aff_aux " " e1 e2

  | Eq(e1,e2) -> aff_aux " = " e1 e2
  | Lt(e1,e2) -> aff_aux " < "  e1 e2
  | Le(e1,e2) -> aff_aux " <= " e1 e2
  | Gt(e1,e2) -> aff_aux " > "  e1 e2
  | Ge(e1,e2) -> aff_aux " >= " e1 e2
  | Ne(e1,e2) -> aff_aux " <> " e1 e2

  | Ou(e1,e2) -> aff_aux " || " e1 e2
  | Et(e1,e2) -> aff_aux " && " e1 e2
  | Not(e)    -> print_string "not(" ; aff_expr e ; print_string ")"

  | Ref(e)        -> print_string "ref (" ; aff_expr e ; print_string ")"
  | Deref(e)      -> print_string "!(" ; aff_expr e ; print_string ")"
  | Assign(e1,e2) -> aff_aux " := " e1 e2

(* Auxiliaire pour plein de cas *)
(* Avec plein de parenthèses pour bien montrer le parsing *)
and aff_aux s a b = 
  print_string "(";
  aff_expr a;
  print_string s;
  aff_expr b;
  print_string ")"

(* Affiche ifthenelse *)
and aff_ite b t e =
  print_string "(if " ; aff_expr b ; print_string " then (";
  aff_expr t;
  print_string ") else (";
  aff_expr e;
  print_string "))"

(* Affiche letin *)
(* Retour à la ligne pour à peine plus de lisibilité *)
and aff_let e1 e2 e3 =
  print_string "let " ; aff_expr e1 ; print_string " = " ; aff_expr e2 ; print_string " in\n";
  aff_expr e3

(* Affiche let rec *)
and aff_ler e1 e2 e3 =
  print_string "let rec " ; aff_expr e1 ; print_string " = " ; aff_expr e2 ; print_string " in\n";
  aff_expr e3

(* Affiche fonction récursive *)
and aff_rfun f x e1 e2 =
  print_string ("let rec "^f^" "^x^" =\n");
  aff_expr e1;
  print_string "\nin\n";
  aff_expr e2

(* Affiche try with *)
and aff_try e1 c e2 =
  print_string "try\n";
  aff_expr e1;
  print_string "\nwith E ";
  aff_expr c;
  print_string " -> ";
  aff_expr e2

(* Affichage d'une instruction *)
let aff_instr i = match i with
    | C(k)   -> print_string "C " ; print_int k ; print_string " ; "
    | A      -> print_string "A ; "
    | S      -> print_string "S ; "  
    | M      -> print_string "M ; "
    | LET(x) -> print_string ("LET "^x^" ; ")
    | ACC(x) -> print_string ("ACC "^x^" ; ")
    | ENDLET -> print_string "ENDLET ; "
    | PRINT  -> print_string "PRINT ; "

(* Affichage d'une liste d'instructon, ajout du skip à la fin pour simplifier l'affichage *)
let aff_comp c =
  List.iter aff_instr c ; print_string "SKIP\n"