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


(** Put here any type and function interfaces concerning systems *)

type 's etat_system = {

  mutable systeme : 's system ;
  mutable chaine : 's word ;
}

(* définir le type d'alphabet : O pour [ et F pour ] *)
type 's alphabet = Lettre of 's | O | F;; 

type 's tree = 
  | Nil 
  | Node of ('s alphabet * int) * 's tree list


(* initialise l'etat du systeme avec le system donne en parametre *)
val init : 's system -> 's etat_system 

(* interpereter la chaine de sumboles de etat_system et produire 
la liste de commandes corepondante dans le champs 'commandes' *)
val interpreter_chaine : ('s -> Turtle.command list) -> 's word -> Turtle.command list

(* appliquer la substitution 'rules' sur les symboles de la chaine et ;ettre le resultat dans 'chaine' *)
val appliquer_substitution : 's rewrite_rules -> 's word -> 's word

val insert_mot_foret  : int -> 's word -> 's tree list 

val substitue_et_interprete : 's tree ->  int ->  's rewrite_rules -> ( 's -> Turtle.command list ) -> Turtle.etat -> unit 

