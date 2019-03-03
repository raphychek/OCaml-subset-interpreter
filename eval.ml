open Types
open Machine

(* Evaluation d'un arbre de programme Fouine *)

(* Tableau de valeur qui représente la mémoire *)
(* m.(0) contient la position de la première case libre *)
let m = Array.make 100 (Ent(1))

(* type les arguments d'une fonction annotée *)
let rec set_type t w = match t,w with
  | WE      , x            -> x
  | INT     , Var(b,s,WE)  -> Var(b,s,INT)
  | INT     , Var(b,s,INT) -> Var(b,s,INT)
  | FUN(u,v), Var(b,s,WE)  -> Var(b,s,FUN(u,v))
  | TUP(u,v), Var(b,s,WE)  -> Var(b,s,TUP(u,v))
  | TUP(u,v), Couple(x,y)  -> Couple(set_type u x, set_type v y)
  | FUN(u,v), Var(b,s,FUN(i,j)) when u=i && v=j -> Var(b,s,FUN(i,j))
  | TUP(u,v), Var(b,s,TUP(i,j)) when u=i && v=j -> Var(b,s,TUP(i,j))
  | _       , _            -> raise MalType

(* vérifie que la variable w est bien du type t *)
let rec check_type t w = match t,w with
  | WE      , x                        -> x
  | INT     , Ent(a)                   -> Ent(a)
  | FUN(u,v), Clo(x,y,WE,en)           -> Clo(set_type u x,y,v,en)
  | FUN(u,v), Clo(x,y,t,en)   when v=t -> Clo(set_type u x,y,t,en)
  | FUN(u,v), Rcl(f,x,y,WE,en)         -> Rcl(f,set_type u x,y,v,en)
  | FUN(u,v), Rcl(f,x,y,t,en) when v=t -> Rcl(f,set_type u x,y,t,en)
  | TUP(u,v), Tup(x,y)                 -> Tup(check_type u x, check_type v y)
  | _       , _                        -> raise MalType

(* recherche dans l'environement la valeur associée à s *)
let rec recherche s t = function
  | []                 -> raise (VarLibre s)
  | (r, x)::q when s=r -> check_type t x
  | h::q               -> recherche s t q

(* affectation multiple avec let/couple, renvoie l'environemment en entrée mis à jour *)
let rec destruct_couple a c env = match a,c with
  | Var(boule,"_",t), r        -> env
  | Var(boule, s ,t), r        -> (s,check_type t r)::env
  | Couple(x ,y)    , Tup(u,v) -> destruct_couple x u (destruct_couple y v env)
  | _               , _        -> raise (Typage "Couple")

(* eval prend une continuation, un environement et une expression et renvoie une valeur *)
(* si on rencontre un noeud pur (avec un booléen à true) on exécute la branche avec la machine *)
let rec eval env e = match e with
  | Cst(boule,k)   -> if boule then (eval_pile env [] (compile e)) else Ent(k)
  | Var(boule,x,t) -> recherche x t env
  | Unit  -> Unit

  (* Bloc Arithmétique *)
  | Add(boule,e1,e2) -> if boule then (eval_pile env [] (compile e)) else 
  (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> Ent(a + b)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Add"))
  | Mul(boule,e1,e2) -> if boule then (eval_pile env [] (compile e)) else
  (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> Ent(a * b)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Mul"))
  | Min(boule,e1,e2) -> if boule then (eval_pile env [] (compile e)) else
  (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> Ent(a - b)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Min"))
  | Div(e1,e2)       -> (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> if b=0 then raise DivZero else Ent(a / b)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Div"))

  (* Bloc Couple *)
  | Couple(a,b) -> Tup(eval env a, eval env b)
  | Fst(e) -> (match (eval env e) with 
    | Tup(a,b) -> a
    | Rex(a) -> Rex(a)
    | _        -> raise (Typage "Fst"))
  | Snd(e) -> (match (eval env e) with
    | Tup(a,b) -> b
    | Rex(a) -> Rex(a)
    | _        -> raise (Typage "Snd"))

  (* Bloc IfThenElse *)
  | Ite(e1,e2,e3) -> (match (eval env e1) with
    | Ent(a) -> if a>0 then (eval env e2) else (eval env e3)
    | Rex(a) -> Rex(a)
    | _      -> raise (Typage "Ite"))

  (* Bloc LetIn *)
  | Let(boule,e1,e2,e3) -> if boule then (eval_pile env [] (compile e)) else
  (match (eval env e2) with
    | Rex(a) -> Rex(a)
    | x      -> eval (destruct_couple e1 x env) e3)

  (* Bloc Récursif *)
  | LeR(Var(boule,s,t),e2,e3) -> (match (eval env e2) with
    | Clo(x,e,t2,enw) -> eval ((s,Rcl(s,x,e,t2,enw))::env) e3
    (* On traite ca comme on peut, même si on risque d'en obtenir des choses bizarres *)
    | Unit   -> raise (Typage "LeR")
    | Rex(a) -> Rex(a)
    | x      -> eval ((s,x)::env) e3)
  | LeR(x,e2,e3) -> raise Impossible

  (* Bloc Affichage *)
  | Print(boule,e) -> if boule then (eval_pile env [] (compile e)) else 
  (match (eval env e) with
    | Ent(v) -> print_int v ; print_newline () ; Ent(v)
    | Rex(a) -> Rex(a)
    | _      -> raise (Typage "Print"))

  (* Bloc Fonctions *)
  | Fonc(x,e)       -> Clo(x,e,WE,env)
  | Appl(e1,e2)     -> (match (eval env e1) with
    | Clo(x,e,t,enw)   -> check_type t (eval (destruct_couple x (eval env e2) enw) e)
    (* On ajoute à l'environnement dans le cas d'une fonction récursive sa clôture *)
    | Rcl(f,x,e,t,enw) -> check_type t (eval (destruct_couple x (eval env e2) ((f,Rcl(f,x,e,t,enw))::enw)) e)
    | Rex(a)           -> Rex(a)
    | _                -> raise(Typage "Appl"))

  (* Bloc Comparaisons Booléennes *)
  | Eq(e1,e2) -> (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> if a =  b then Ent(1) else Ent(0)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Eq"))
  | Lt(e1,e2) -> (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> if a <  b then Ent(1) else Ent(0)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Lt"))
  | Le(e1,e2) -> (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> if a <= b then Ent(1) else Ent(0)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Le"))
  | Gt(e1,e2) -> (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> if a >  b then Ent(1) else Ent(0)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Gt"))
  | Ge(e1,e2) -> (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> if a >= b then Ent(1) else Ent(0)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Ge"))
  | Ne(e1,e2) -> (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> if a <> b then Ent(1) else Ent(0)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Ne"))

  (* Bloc Opérateurs Booléens *)
  | Ou(e1,e2) -> (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> if a+b > 0 then Ent(1) else Ent(0)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Or"))
  | Et(e1,e2) -> (match ((eval env e1),(eval env e2)) with
    | Ent(a), Ent(b) -> if a*b > 0 then Ent(1) else Ent(0)
    | _     , Rex(a) -> Rex(a)
    | Rex(a), _      -> Rex(a)
    | _     , _      -> raise (Typage "Et"))
  | Not(e1) -> (match (eval env e1) with
    | Ent(a) -> if a > 0 then Ent(0) else Ent(1)
    | Rex(a) -> Rex(a)
    | _      -> raise (Typage "Not"))

  (* Bloc Références *)
  | Ref(e) -> (match (m.(0)) with
    | Ent(a) -> let _ = m.(a)<-(eval env e) ; m.(0)<-Ent(a+1) in Vref(a)
    | Rex(a) -> Rex(a)
    | _      -> raise Impossible)
  | Deref(e) -> (match (eval env e) with
    | Vref(a) -> m.(a)
    | Rex(a) -> Rex(a)
    | _       -> raise (Typage "Bang"))
  | Assign(e1,e2) -> (match (eval env e1) with
    | Vref(a) -> m.(a)<-(eval env e2) ; Unit
    | Rex(a) -> Rex(a)
    | _       -> raise (Typage "Assign"))

  (* Bloc Exceptions *)
  | Try(e1,c,e2) -> (match ((eval env e1), c) with
    | Rex(a), Var(b,"_",t) -> eval env e2
    | Rex(a), Var(b, x ,t) -> eval ((x,Ent(a))::env) e2
    | Rex(a), Cst(b,x)   -> if a=x then eval env e2 else Rex(a)
    | x     , _          -> x)
  | Raise(e) -> (match (eval env e) with
    | Exc(a) -> Rex(a)
    | Rex(a) -> Rex(a)
    | _      -> raise (Typage "Raise"))
  (* on n'autorise que des E of int *)
  | Exc(e) -> (match (eval env e) with
    | Ent(a) -> Exc(a)
    | Rex(a) -> Rex(a)
    | _      -> raise (Typage "Exc"))