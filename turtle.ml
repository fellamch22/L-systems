open Graphics

type command =
| Line of int
| Move of int
| Turn of int
| Store
| Restore

type position = {
  x: float;      (** position x *)
  y: float;      (** position y *)
  a: int;        (** angle of the direction *)
}

(** Put here any type and function implementations concerning turtle *)

type etat = {
  
          current_position : position ; (* position actuelle de la tortue *) 
          pile : position list 
 
}


(* calcule la prochaine position en fonction de la position actuelle 
et la longeur du deplacement *)
let next_position ( current_position : position )( deplacement : int ) : position =
  let pi = 3.141592653589793238 in
  let teta = (* modulo 360*)
    if current_position.a >= 0 
    then current_position.a mod 360
    else
       current_position.a+360 
  in 
  if ( teta <= 90 ) then
  let teta_rad =( float_of_int (teta) *. pi ) /. 180.0  in
  { x = ((cos teta_rad) *. float_of_int deplacement) +. current_position.x
  ;y = ((sin teta_rad) *. float_of_int deplacement) +. current_position.y  
  ; a = current_position.a }
  else 
  if ( teta <= 180 ) then
  let teta_rad =( (180.0 -. float_of_int (teta)) *. pi ) /. 180.0  in
  { x = ((-.(cos teta_rad)) *. float_of_int deplacement) +. current_position.x
  ;y = ((sin teta_rad) *. float_of_int deplacement) +.current_position.y  
  ; a = current_position.a }
  else
  if (teta <= 270 ) then
  let teta_rad =( ( float_of_int (teta) -. 180.0 ) *. pi ) /. 180.0  in
  { x = ((-.(cos teta_rad)) *. float_of_int deplacement) +. current_position.x
  ;y = ((-.(sin teta_rad)) *. float_of_int deplacement) +. current_position.y 
  ; a = current_position.a }
  else (* 270 < teta <= 360*)
  let teta_rad =( (360.0 -. float_of_int (teta)) *. pi ) /. 180.0  in
  { x = ((cos teta_rad) *. float_of_int deplacement) +. current_position.x
  ;y = ((-.(sin teta_rad)) *. float_of_int deplacement) +. current_position.y  
  ; a = current_position.a }


(* calcule la prochaine position en fonction de la position 
actuelle et la longeur du deplacement *)
let draw_line ( etat : etat) ( deplacement : int ) : etat = 
  let e = next_position etat.current_position deplacement in
  let x = int_of_float etat.current_position.x in
  let y = int_of_float etat.current_position.y in 
  let a = etat.current_position.a in
  set_color (rgb x y a);
  lineto x  y;
  { current_position = e ; pile = etat.pile }


(* changer la position courante et se deplacer par x unites *)
let move ( etat : etat) ( deplacement : int ) : etat = 
    let e = next_position etat.current_position deplacement in
   let x = int_of_float etat.current_position.x in
   let y = int_of_float etat.current_position.y in
  moveto x y;
  { current_position = e ; pile = etat.pile }


(* tourner l'angle actuel de la tortue par teta donne en parametre*)
let turn ( etat : etat ) ( angle : int ) : etat = 
  { current_position = { x = etat.current_position.x ; 
  y = etat.current_position.y;
  a = etat.current_position.a + angle }
  ; pile = etat.pile }

(* sauvegarder l'etat actuel de la tortue  *)
let store ( etat : etat )  : etat = 
  { current_position = etat.current_position
  ; pile = etat.current_position::etat.pile }

(* modifier l'etat actuel de la tortue en remettant le dernier etat sauvegarde  *)
let restore ( etat : etat )  : etat = 
  let pos = List.hd etat.pile in
  moveto (int_of_float pos.x)  (int_of_float pos.y);
  { current_position = pos ; pile = List.tl etat.pile }


(* executer la liste des  commandes par la tortue *)
let rec execute_commands ( cmds : command list) (etat : etat) : unit = 
match cmds with
| []-> ()
| cmd :: lcmd ->
      let e = 
        (match cmd with
        | Line  d -> draw_line etat d
        | Move  d -> move etat d
        | Turn  teta -> turn etat teta
        | Store -> store etat
        | Restore -> restore etat
        ) in
        execute_commands lcmd e
