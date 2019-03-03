open Types

(* Traitement de l'arbre pour la recharche de branches pures *)

(* parcours de l'arbre pour chercher toutes les branches pures *)
(* e : arbre fouine *)
(* p : booléen qui indique si on est possiblement dans une branche pure (pour la validation des Var et Cst) *)
let rec repand e p = match e with
  | Cst(boule,k)   -> p, Cst(p,k)
  | Var(boule,s,t) -> p, Var(p,s,t)
  | Unit  -> false, Unit
  
  | Add(boule,e1,e2) -> let b1,e1' = repand e1 true and b2,e2' = repand e2 true in b1&&b2, Add(b1&&b2,e1',e2') 
  | Mul(boule,e1,e2) -> let b1,e1' = repand e1 true and b2,e2' = repand e2 true in b1&&b2, Mul(b1&&b2,e1',e2')
  | Min(boule,e1,e2) -> let b1,e1' = repand e1 true and b2,e2' = repand e2 true in b1&&b2, Min(b1&&b2,e1',e2')
  | Div(e1,e2)       -> let b1,e1' = repand e1 true and b2,e2' = repand e2 true in false , Div(e1',e2')

  | Couple(e1,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Couple(e1',e2')
  | Fst(c)        -> let b ,c'  = repand c  false in false, Fst(c')
  | Snd(c)        -> let b ,c'  = repand c  false in false, Snd(c')

  | Try(e1,c,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Try(e1',falsifie c,e2')
  | Raise(e)     -> let b ,e'  = repand e  false in false, Raise(e')
  | Exc(e)       -> let b ,e'  = repand e  false in false, Exc(e')

  | Ite(e1,e2,e3)                    -> let b2,e2' = repand e2 false and b3,e3' = repand e3 false in false , Ite(e1,e2',e3')
  | Let(boule,Var(boul,"_",t),e2,e3) -> let b2,e2' = repand e2 false and b3,e3' = repand e3 false in false , Let(false ,Var(false,"_",t),e2',e3')
  | Let(boule,Var(boul,x,t)  ,e2,e3) -> let b2,e2' = repand e2 true  and b3,e3' = repand e3 true  in b2&&b3, Let(b2&&b3,Var(b2&&b3,x,t)  ,e2',e3')
  | Let(boule,e1,e2,e3)              -> let b2,e2' = repand e2 true  and b3,e3' = repand e3 true  in false , Let(false ,falsifie e1,e2',e3')
  | LeR(e1,e2,e3)                    -> let b3,e3' = repand e3 false in false, LeR(falsifie e1,e2,e3')

  | Print(boule,e) -> let b,e' = repand e false in b, Print(b,e')

  | Fonc(x,e)   -> let b ,e'  = repand e  false in false, Fonc(x,e')
  | Appl(e1,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Appl(e1',e2')

  | Eq(e1,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Eq(e1',e2')
  | Lt(e1,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Lt(e1',e2')
  | Le(e1,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Le(e1',e2')
  | Gt(e1,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Gt(e1',e2')
  | Ge(e1,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Ge(e1',e2')
  | Ne(e1,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Ne(e1',e2')

  | Ou(e1,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Ou(e1',e2')
  | Et(e1,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Et(e1',e2')
  | Not(e)    -> let b ,e'  = repand e  false in false, Not(e')

  | Ref(e)        -> let b ,e'  = repand e  false in false, Ref(e')
  | Deref(e)      -> let b ,e'  = repand e  false in false, Deref(e')
  | Assign(e1,e2) -> let b1,e1' = repand e1 false and b2,e2' = repand e2 false in false, Assign(e1',e2')

(* parcours de l'arbre pour rendre toutes les branches impures *)
and falsifie e = match e with
  | Cst(boule,k)   -> Cst(false,k)
  | Var(boule,s,t) -> Var(false,s,t)
  | Unit  -> Unit
  
  | Add(boule,e1,e2) -> Add(false, falsifie e1, falsifie e2)
  | Mul(boule,e1,e2) -> Mul(false, falsifie e1, falsifie e2)
  | Min(boule,e1,e2) -> Min(false, falsifie e1, falsifie e2)
  | Div(e1,e2)       -> Div(falsifie e1, falsifie e2)

  | Couple(e1,e2) -> Couple(falsifie e1, falsifie e2)
  | Fst(c) -> Fst(falsifie c)
  | Snd(c) -> Snd(falsifie c)

  | Try(e1,c,e2) -> Try(falsifie e1, falsifie c, falsifie e2)
  | Raise(e)     -> Raise(falsifie e)
  | Exc(e)       -> Exc(falsifie e)

  | Ite(e1,e2,e3)       -> Ite(falsifie e1, falsifie e2, falsifie e3)
  | Let(boule,e1,e2,e3) -> Let(false, falsifie e1, falsifie e2, falsifie e3)
  | LeR(e1,e2,e3)        -> LeR(falsifie e1, falsifie e2, falsifie e3)

  | Print(boule,e) -> Print(false, falsifie e)

  | Fonc(x,e)   -> Fonc(x, falsifie e)
  | Appl(e1,e2) -> Appl(falsifie e1, falsifie e2)

  | Eq(e1,e2) -> Eq(falsifie e1, falsifie e2)
  | Lt(e1,e2) -> Lt(falsifie e1, falsifie e2)
  | Le(e1,e2) -> Le(falsifie e1, falsifie e2)
  | Gt(e1,e2) -> Gt(falsifie e1, falsifie e2)
  | Ge(e1,e2) -> Ge(falsifie e1, falsifie e2)
  | Ne(e1,e2) -> Ne(falsifie e1, falsifie e2)

  | Ou(e1,e2) -> Ou(falsifie e1, falsifie e2)
  | Et(e1,e2) -> Et(falsifie e1, falsifie e2)
  | Not(e)    -> Not(falsifie e)

  | Ref(e)        -> Ref(falsifie e)
  | Deref(e)      -> Deref(falsifie e)
  | Assign(e1,e2) -> Assign(falsifie e1, falsifie e2)