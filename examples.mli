(* Examples tirés du livre "The Algorithmic Beauty of Plants".
   Un exemple consiste en un axiome, un système de réécriture,
   et une interprétation. Pour plus d'exemples, voir les fichiers
   dans le répertoire examples/
*)

(* Pour l'exemple ci-dessous, ces trois symboles suffisent.
   A vous de voir ce que vous voudrez faire de ce type symbol ensuite.
*)

type symbol = A|P|M
type symbol1 = A|P|M|N
(* snow flake  - Figure 3 du sujet *)

val snow : symbol Systems.system
val tree : symbol Systems.system
val kochv2 : symbol Systems.system
val branch1 : symbol Systems.system
val dragon : symbol1 Systems.system
val kochv1 : symbol1 Systems.system

