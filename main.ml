open Lsystems
open Graphics
open Systems
open Turtle
open Examples
open Analyser
(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)

let file_name = ref ""
let iter = ref 5
let scale = ref 1

(* Graphic functions *) (* generer la fenetre*)
let create_window w h =
  open_graph (" " ^ string_of_int w ^ "x" ^ string_of_int h);
  auto_synchronize false

let close_after_event () =
  ignore (wait_next_event [Button_down ; Key_pressed])


let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals , Binome : Mechouar Fella et Li-Fang Su"

let action_what () = Printf.printf "%s\n" usage; exit 0

let read_file (f : string) : unit = (file_name := f) 

let read_iter (i : int) : unit = (iter := i)

let read_scale (s: int) : unit = (scale := s)

let cmdline_options = [
("--what" , Arg.Unit action_what, "description");
("--open" , Arg.String read_file, "Ouvre un fichier pour l'analyser et pour creer un L-Systeme");
("--iter", Arg.Int read_iter, "Pour le nombre d'iteration");
("-s", Arg.Int read_scale, "Pour donner l'echelle du dessin ");
("-i", Arg.Int read_iter, "Pour le nombre d'iteration");
]

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s) 

let tortue = { current_position = { x =(800.0) ; y=(100.0) ; a=90} ; (* position actuelle de la tortue *)
pile = [] } (* derniere position memorise de la tortue *)


let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  
  let sys_test = Analyser.analyse !file_name !scale in
  let etat_sys = init sys_test in 

  create_window 2000 1200;
  clear_graph ();
  moveto (800) (100) ;
  set_color black;
  fill_rect 0 0 2000 1200;
  
  let tr = insert_mot_foret 0 etat_sys.chaine 
  in 
  List.iter (fun x -> substitue_et_interprete x !iter etat_sys.systeme.rules etat_sys.systeme.interp tortue) tr;

  synchronize ();

  close_after_event ()
  

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)

let () = if not !Sys.interactive then main ()
