open Turtle
open Systems



val get_word_length : 's word -> int

val word_of_branch  : string ->  int -> char word

val word_of_string  : string -> char word 

val is_empty : string -> bool

val is_comment : string -> bool

val assert_empty_line : string -> unit 

val analyse  : string -> int -> char system

