open Types

(* Génération et interpétation de code pour la machine à pile *)

let rec rechercher x env = match env with
	| [] -> raise Fenetre
	| (y,Ent(a))::env' -> (if (y=x) then a else rechercher x env') 
  	| _ -> raise Impossible


(* s : pile, c : liste de instr, env : environnement *)
let rec eval_pile env s c = match c with
	| [] -> Ent(List.hd s) (* on a fini le programme, on renvoie la tête de pile *)
	| (h::q) -> (match h with 
		| C(a) -> eval_pile env (a::s) q

		| A -> (match s with
				| (a::b::s') -> eval_pile env ((a+b)::s') q
				| _ -> raise Fenetre)
		| S -> (match s with
				| (a::b::s') -> eval_pile env ((a-b)::s') q
				| _ -> raise Fenetre)
		| M -> (match s with
				| (a::b::s') -> eval_pile env ((a*b)::s') q
				| _ -> raise Fenetre)

		| LET(x) -> eval_pile ((x,Ent(List.hd s))::env) (List.tl s) q
		| ACC(x) -> let a = rechercher x env in eval_pile env (a::s) q
		| ENDLET -> eval_pile (List.tl env) s q
		| PRINT  -> print_int (List.hd s) ; print_newline () ; eval_pile env s q)

(* prend un arbe de programme fouine en entrée et renvoie une liste d'instruction pour la machine à pile *)
let rec compile e = match e with 
	| Cst(b,k)   -> [C(k)]
  	| Var(b,x,t) -> [ACC(x)]
	| Let(b,Var(d,x,t),e1,e2) -> (compile e1)@[LET(x)]@(compile e2)@[ENDLET]
  	| Add(b,e1,e2) -> (compile e2)@(compile e1)@[A]
  	| Mul(b,e1,e2) -> (compile e2)@(compile e1)@[M]
  	| Min(b,e1,e2) -> (compile e2)@(compile e1)@[S]
  	| Print(b,e)    -> (compile e)@[PRINT]
  	| _ -> raise CompErr