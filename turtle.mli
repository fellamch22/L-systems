
(** Turtle graphical commands *)
type command =
| Line of int      (** advance turtle while drawing *)
| Move of int      (** advance without drawing *)
| Turn of int      (** turn turtle by n degrees *)
| Store            (** save the current position of the turtle *)
| Restore          (** restore the last saved position not yet restored *)

(** Position and angle of the turtle *)
type position = {
  x: float;        (** position x *)
  y: float;        (** position y *)
  a: int;          (** angle of the direction *)
}

(** Put here any type and function signatures concerning turtle *)

type etat = {
  
          current_position : position ; (* position actuelle de la tortue *) 
          pile : position list 
}

(* calcule la prochaine position en fonction de la position 
actuelle et la longeur du deplacement *)
val next_position :  position -> int -> position

(* changer la position courante et se deplacer par x unites *)
val move : etat -> int -> etat

(* tourner l'angle actuel de la tortue par teta donne en parametre*)
val turn : etat -> int -> etat

(* sauvegarder l'etat actuel de la tortue  *)
val store : etat -> etat

(* modifier l'etat actuel de la tortue en remettant le dernier etat sauvegarde  *)
val restore : etat -> etat


(* executer une liste de commandes par la tortue *)
val execute_commands : command list ->  etat -> unit

