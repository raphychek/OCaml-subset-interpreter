(* ensemble des types et exceptions définis pour le projet *)

exception Fenetre             (* Problème lors de l'exécution de la machine *)
exception VarLibre of string  (* Nom mal associé dans l'environement *)
exception Typage of string    (* Mauvaise valeur en entrée (cloture au lieu d'entier ou l'inverse) *)
exception DivZero             (* Division par zéro *)
exception Impossible          (* Cas qui ne sont pas sensés pouvoir arriver *)
exception RaiseTry of int     (* Exception fouine non rattrapée *)
exception ErrPars             (* Erreur au moment du parsing *)
exception CompErr             (* Erreur au moment de la compilation en langage machine *)
exception MalType             (* Erreur de typage fouine *)

(* type pour typer les variables, on le nomme à l'américaine... *)
type guy = (* skateboard *)
  | WE             (* WhatEver, on ne se préoccupe pas du type, par défaut *)
  | INT            (* entier *)
  | FUN of guy*guy (* fonction *)
  | TUP of guy*guy (* couple *)

(* Noeuds de l'arbre d'expression *)
(* bool pour indiquer la pureté là où c'est possible *)
(* guy  pour indiquer le type *)
type expr =
  | Unit
  | Var of bool*string*guy  (* On ne renomme pas les variables, on conserve l'entrée du programme *)
  | Cst of bool*int
  
  | Add of bool*expr*expr
  | Mul of bool*expr*expr
  | Div of expr*expr
  | Min of bool*expr*expr
  
  | Couple of expr*expr
  | Fst    of expr
  | Snd    of expr

  | Ref    of expr
  | Deref  of expr
  | Assign of expr*expr

  | Try   of expr*expr*expr
  | Raise of expr
  | Exc   of expr

  | Ite of expr*expr*expr
  | Let of bool*expr*expr*expr
  | LeR of expr*expr*expr

  | Eq of expr*expr
  | Lt of expr*expr
  | Le of expr*expr
  | Gt of expr*expr
  | Ge of expr*expr
  | Ne of expr*expr

  | Ou  of expr*expr
  | Et  of expr*expr
  | Not of expr

  | Print of bool*expr
  | Fonc  of expr*expr
  | Appl  of expr*expr

(* valeur : entier, clôture, couple de valeurs, pointeur de référence, exception, unit *)
(* clôture : nom de la variable, expression de la fonction, environement au moment où elle est définie *)
(* clôture de fonction récursive : on stocke aussi le nom de la fonction pour la remettre dans l'environnement *)
type valeur =
  | Ent  of int
  | Clo  of expr*expr*guy*((string*valeur) list)
  | Rcl  of string*expr*expr*guy*((string*valeur) list)
  | Tup  of valeur*valeur
  | Vref of int (* pointeur vers une case mémoire *)
  | Exc  of int (* exception *)
  | Rex  of int (* exception qui a été levé *)
  | Unit
(* environement : (string*valeur) list *)

(* instructions acceptées par la machine à pile *)
type instr =
  | C of int       (* constante *)
  | A              (* addition *)
  | S              (* soustraction *)
  | M              (* multiplication *)
  | LET of string  (* affectation du let *)
  | ACC of string  (* lecture dans l'environnement *)
  | ENDLET         (* fin de portée let *)
  | PRINT          (* affichage *)