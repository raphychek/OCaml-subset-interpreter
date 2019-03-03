open Types
open Eval
open Affiche 
open Purity
open Machine

let debug     = ref false (* Affichage du programme entrée après traitement *)
let machine   = ref false (* Traitement des noeuds dès que possible par la machine à pile *)
let stackcode = ref false (* Programme pur, compilé en liste d'instructiions et traités par la machine *)
let input = ref stdin
let initi = ref true

let set_input name =
  if !initi then
  begin
    input := (open_in name); (* on n'a pas cherché comment n'autoriser que les *.ml *)
    initi := false  (* pour etre sur de ne prendre que la premiere entrée, on n'a pas testé toutes les subtilités *)
  end
;;

let options =
begin
  let speclist = [("-debug", Arg.Set debug, "Enables debug mode");("-machine", Arg.Set machine, "Enables machine mode");("-stackcode", Arg.Set stackcode, "Enables stackcode mode")] in
  let usage_msg = "Options available :" in
  Arg.parse speclist set_input usage_msg;
end

let _ = options

let lexbuf = Lexing.from_channel (!input)

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () =  try Parser.main Lexer.token lexbuf with _ -> raise ErrPars

let traitement result =
  if !debug then
  begin (* Affiche l'arbe obtenu après le parsing *)
    print_string "--- Code ---\n";
    aff_expr result;
    print_string ";;\n\n";
  (* On démarre avec un environement vide *)
  end;
  if (!stackcode || !machine) then (*si on utilise la machine *)
  begin
  if !stackcode then (* si le programme doit tourner entièrement sur la machine*)
  begin
    print_string "--- Stack ---\n";
    let boule, resu = repand result false in
    if boule then (* si le programme est pur *)
    begin
      let comp = compile resu in (* on compile le programme *)
      aff_comp comp ; print_string "\n--- Résultat ---\n" ; let _ = eval_pile [] [] comp in ()
    end
    else
      print_string "Error : programme impur.\n"
  end
  else
  begin
    if !machine then  (* si on veut utiliser la machine dès que possible*)
      begin
        let boule, resu = repand result false in (* on identifie les branches pures *)
        if !debug then print_string "--- Résultat ---\n";
        (match (eval [] resu) with Rex(a) -> raise (RaiseTry a) | _ -> ())
      end
  end
  end
  else
  begin
    if !debug then print_string "--- Résultat ---\n";
    (match (eval [] (falsifie result)) with Rex(a) -> raise (RaiseTry a) | _ -> ()) (* sinon on rend toutes les branches impures *)
  end;;

(* Lancer l'interprétation du programme donné *)
let calc () =
  try
      let result = parse () in
        traitement result
  with VarLibre(s) -> (print_string ("Error : "^s^" est une variable libre.\n"))
    | Typage(s)    -> (print_string ("Error : "^s^" a un paramètre incorrect.\n"))
    | RaiseTry(a)  -> (print_string  "Error : " ; print_int a ; print_string " est une exception non rattrapée.\n")
    | Impossible   -> (print_string  "Error : Comment t'as fait pour arriver là?\n")
    | DivZero      -> (print_string  "Error : division par zéro\n")
    | ErrPars      -> (print_string  "Error : parsing\n")
    | Fenetre      -> (print_string  "Error : on le jete par la fenetre\n")
    | CompErr      -> (print_string  "Error : compilation en langage pile\n")
    | MalType      -> (print_string  "Error : programme mal typé\n")
    | _            -> (print_string  "Error : non prévue\n")
;;

let _ = calc()