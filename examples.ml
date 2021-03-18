
open Systems
open Turtle

(* Examples tirés du livre "The Algorithmic Beauty of Plants".
   Un exemple consiste en un axiome, un système de réécriture,
   et une interprétation. Pour plus d'exemples, voir les fichiers
   dans le répertoire examples/
*)

(* Pour l'exemple ci-dessous, ces trois symboles suffisent.
   A vous de voir ce que vous voudrez faire de ce type symbol ensuite.
*)

type symbol = A|P|M

(* snow flake  - Figure 3 du sujet *)

let snow : symbol system =
  let a = Symb A in
  let p = Symb P in
  let m = Symb M in
  {
    axiom = Seq [a;p;p;a;p;p;a];
    rules =
      (function
       | A -> Seq [a;m;a;p;p;a;m;a]
       | s -> Symb s);
    interp =
      (function
       | A -> [Line 30]
       | P -> [Turn 60]
       | M -> [Turn (-60)])
  }



let tree : symbol system =
   let a = Symb A in
   let p = Symb P in
   let m = Symb M in
   {
     axiom = Seq [a];
     rules =
       (function
        | A -> Seq [a;Branch(Seq[p;a]);a;Branch(Seq[m;a]);a]
        | s -> Symb s);
     interp =
       (function
        | A -> [Line 1]
        | P -> [Turn 25]
        | M -> [Turn (-25)])
   }

   let kochv2 : symbol system =
    let a = Symb A in
    let p = Symb P in
    let m = Symb M in
    {
      axiom = Seq[a;p;a;p;a;p;a];
      rules =
        (function
         | A -> Seq [a;a;p;a;p;a;p;a;p;a;p;a;m;a]
         | s -> Symb s);
      interp =
        (function
         | A -> [Line 5]
         | P -> [Turn 90]
         | M -> [Turn (-90)])
    }


let branch1 : symbol system =
  let a = Symb A in
  let p = Symb P in
  let m = Symb M in
  {
    axiom = a;
    rules =
      (function
       | A -> Seq [a;Branch(Seq[p;a]);a;Branch(Seq[m;a]);a]
       | s -> Symb s);
    interp =
      (function
       | A -> [Line 10]
       | P -> [Turn 25]
       | M -> [Turn (-25)])
  }

  type symbol1 = A|P|M|N

  let dragon : symbol1 system =
    let a = Symb A in
    let p = Symb P in
    let m = Symb M in
    let n = Symb N in
    {
      axiom = a;
      rules =
        (function
         | A -> Seq [a;m;p;m]
         | P -> Seq [n;a;n;p]
         | s -> Symb s);
      interp =
        (function
         | A -> [Line 10]
         | P -> [Line 10]
         | M -> [Turn 90]
         | N -> [Turn (-90)])
    }


let kochv1 : symbol1 system =
  let a = Symb A in
  let p = Symb P in
  let m = Symb M in
  let n = Symb N in
  {
    axiom = a;
    rules =
      (function   
       | A -> Seq [a;m;p;n;a;a;m;a;m;a;a;m;a;p;m;a;a;n;p;m;a;a;n;a;n;a;a;n;a;p;n;a;a;a]
       | P -> Seq [p;p;p;p;p;p]
       | s -> Symb s);
    interp =
      (function
       | A -> [Line 10]
       | P -> [Move 10]
       | M -> [Turn 90]
       | N -> [Turn (-90)])
  }
