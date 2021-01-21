(*
   Uebungen zur Funktionale Programmierung
   von Peretto Lucas 5IB 
*)


(* 1. GGT *)

let rec ggt a b =
  if b = 0 
  then a 
  else ggt b (a mod b);;

ggt 55 200 ;; 



(* 2. Fibonacci Folge *)

let rec fibonacci n =
  if n < 1 
  then 0
  else if n = 1 then 1
  else (fibonacci (n - 1)) + (fibonacci (n - 2));;

fibonacci 6 ;;


(* 3. rekursive Funktion die alle Elemente einer Liste summiert *) 

let rec sum l = 
  match l with 
    [] -> 0. 
  | h :: t -> h +. (sum t);;

sum [10.;9.;8.;7.;6.;5.;4.;3.;2.;1.] ;;


(* 4.  *)



(* 5. Get the Highest Value of a given number list  *)

let list = [4; 3;7;6;1;7;4;8;3;4] ;;


let maxNum = function
    [] -> invalid_arg "emptyList!"
  | x::xs -> List.fold_left max x xs ;;


maxNum list ;;


(* 6. *)

(* 7. *)
   
(* 8. *)
   
(* 9. reverses a list  *)

let rev =
  let rec rev_append acc l =
    match l with
      [] -> acc
    | h::t -> rev_append (h::acc) t in
  fun l -> rev_append [] l;;


rev [0;1;2;3;4;5;6;7;8;9]


