open Turtle
(** Words, rewrite systems, and rewriting *)

type 's word =
  | Symb of 's
  | Seq of 's word list
  | Branch of 's word

type 's rewrite_rules = 's -> 's word

type 's system = {
    axiom : 's word;
    rules : 's rewrite_rules;
    interp : 's -> Turtle.command list }



type 's etat_system = {

  mutable systeme : 's system ;
  mutable chaine : 's word ;
}

type 's alphabet = Lettre of 's | O | F (* O pour [ et F pour ] *)

type 's tree = 
  | Nil 
  | Node of ('s alphabet * int) * 's tree list


(* initialise l'etat du systeme avec le system donne en parametre *)
let init ( system :'s system ) : 's etat_system  = 
  { systeme = system ; chaine = system.axiom }


(* 
      Cette partie a été implementé initialement dans le projet 
      dans laquelle on applique pas le principe d'interpretation à la volée 
      et qui est evidemment plus couteuse en temps et en espace memoire
      
      Actuellement on l'utilise plus dans le programme , mais on la garde pour 
      laisser trace de l'evolution de notre travail et comparer les deux methodes
*)


(* interpereter la chaine de symboles de etat_system et produire 
la liste de commandes corepondante dans le champs 'commandes' *)

let rec interpreter_chaine (interpretation : 's -> Turtle.command list )
(chaine : 's word ) :  Turtle.command list =
   let list = [] in
     match chaine with
    | Symb x -> list @ interpretation x 
    | Seq l -> list @ List.flatten (List.map (interpreter_chaine interpretation) l)
    | Branch x -> list @ [ Store ] @ ( interpreter_chaine interpretation x ) @ [ Restore ]


(* appliquer la substitution 'rules' sur les symboles de la chaine et mettre le resultat dans 'chaine' *)
let rec appliquer_substitution  ( substitution : 's rewrite_rules )
(chaine : 's word ) : 's word =
  match chaine with
  | Symb x -> substitution x
  | Seq l -> Seq (List.map (appliquer_substitution substitution) l)
  | Branch word -> Branch (appliquer_substitution substitution word)
  

(*

      Les fonctions implementées dans cette partie sont celles permettant
      l'interpretation des L-systemes à la volée 

*)

(* crée la foret associée à 'chaine' à une profondeur donnée *)
let insert_mot_foret ( profondeur : int ) ( chaine : 's word ) : 's tree list =

    let rec aux ( w : 's word ) : 's tree list =
      match w with
      Symb x -> [Node ((Lettre x, profondeur),[])]
      | Seq l -> List.flatten (List.map aux l)
      | Branch m -> [Node((O, profondeur),[])]@(aux m)@[Node((F, profondeur),[])]

  in aux chaine;;

(* construit la foret associée à tree ; jusqu'à la profondeur nb_iterations - 2
 et interprete  directement ceux de  nb_iterations -1 *)
let rec substitue_et_interprete ( tree : 's tree ) ( nb_iterations : int ) 
( substitution : 's rewrite_rules ) 
( interpretation : 's -> Turtle.command list ) 
( turtle : Turtle.etat ) : unit =
    match tree with 
    | Nil -> ()
    | Node((a,n),t) -> 
      match a with
      Lettre x -> if n == nb_iterations then 
          let commande = interpretation x in Turtle.execute_commands commande turtle
        else let m = substitution x in
        let l = insert_mot_foret (n+1) m in
        List.iter (fun x -> substitue_et_interprete x nb_iterations substitution interpretation turtle) l;
        List.iter  (fun x -> substitue_et_interprete x nb_iterations substitution interpretation turtle) t
      | O -> let e = Turtle.store turtle in
      List.iter  (fun x -> substitue_et_interprete x nb_iterations substitution interpretation e) t
      
      | F -> let e = Turtle.restore turtle in
      List.iter  (fun x -> substitue_et_interprete x nb_iterations substitution interpretation e) t


 
	









 
