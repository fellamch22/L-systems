(*lire un fichier .sys et cree le L-System correspondent *)
open Turtle
open Systems
open Printf
open Scanf


(*renvoi la taille d'un word*)
let rec get_word_length (w : 's word) = match w with
    | Symb(_) -> 1
    | Seq(l) -> List.length l 
    | Branch(word) -> 2 + get_word_length word

let word_of_branch (s : string) (index : int): char word = 
    let rec aux i acc = 
        if i = index then if String.get s i = '[' then aux (i+1) acc else failwith "Branch is not valid: not begining with ["
        else if i = String.length s then failwith "Branch is not valid not ending with ]"  
        else if (String.get s i = ']') then Branch(Seq(List.rev acc))
        else aux (i+1) (Symb(String.get s i) :: acc) in 
    aux index []

let word_of_string (s : string) : char word = 
    let rec aux i acc = 
        if i = String.length s then Seq(List.rev acc)
        else if String.get s i = '[' then
        let w = word_of_branch s i in aux (i+get_word_length w) (w :: acc)
        else if String.get s i = ']' then failwith "word is not valid: ] without ["
        else aux (i+1) (Symb(String.get s i) :: acc) in 
    aux 0 []


let is_empty (s : string) : bool = String.trim s = ""

let is_comment (s : string) : bool = if is_empty s then false else String.get s 0 = '#'

let assert_empty_line (line : string) : unit = 
    if is_empty line then () else failwith "Syntax error: line is not empty"


let analyse (file_name : string) (scale : int ) : (char system) =

    let file = if is_empty (file_name) then "./examples/test.sys" else file_name in

    (* Read file and display the first line *)
    let ic = open_in file
    and line = ref "#" 
    and axiom = ref (Symb('v'))
    and interp = ref (fun x -> [])
    and rules =  ref (fun x -> Symb('_')) in

    let read_ignore_comment () = 
        line := "#";
        while is_comment !line do
            line := input_line ic;
        done; in

    begin

        read_ignore_comment (); (* comme read_line mais en prenent compte les commentaires *)
        axiom:= word_of_string !line ;

        (* pour sauter une ligne *)
        read_ignore_comment ();
        assert_empty_line !line;

        (*pour lire la regle*)
        read_ignore_comment ();

        let entries = ref [] in
        while not(is_empty !line) do 
            let param = String.get !line 0 and 
            value = word_of_string (String.sub !line 2 ((String.length !line)-2)) in
            entries:= (param,value) :: !entries;
            read_ignore_comment ();
        done;
        assert_empty_line !line; (*a la fin on a lu une ligne vide *)

        rules := (function s -> let eq = (fun (param,_) -> param = s) in 
            (* si il existe un parametre = s on regarde snd=value de ce dernier
                sinon pour tout parametre non defnie -> Symb(s) *)
            if (List.exists eq !entries) then snd (List.find eq !entries) else Symb(s)) ;


        (*pour lire l'interpretation*)
        read_ignore_comment ();
        (
            let entries = ref [] in
            try
                while not(is_empty !line) do 
                    let len = String.length !line in
                    let param = String.get !line 0 and 
                    n = if len >= 4 then
                        int_of_string (String.sub !line 3 (len-3)) else 0 in
                    let value =  match String.get !line 2 with
                            | 'M' -> Move(max 1 (n / scale) )
                            | 'L' -> Line(max 1 (n / scale) )
                            | 'T' -> Turn(n)
                            | 'S' -> Store
                            | 'R' -> Restore
                            | _ -> failwith "Syntax error: command doesn't exists" in
                    entries:= (param,value) :: !entries;
                    read_ignore_comment ();
                done;
            with | End_of_file -> (close_in ic;
                    interp := (function s -> let eq = (fun (param,_) -> param = s) in 
                        if (List.exists eq !entries) then [snd (List.find eq !entries)] else failwith "Invalid input for interp");
                    )
        );


        (* renvoi le L-Systeme qui correspond au fichier*)
        {axiom = !axiom; rules = !rules; interp = !interp};
    end   

  