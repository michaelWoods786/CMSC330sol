open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target =
fold (fun x y -> if (y = target) then 1 + x else x) 0 lst
;;

let uniq lst = 
(*takes in an accumulator and a y*)
let f acc y = if (count_occ acc y == 0) then y :: acc else
acc in
(*calls fold with f function, accumulator, and lst*)

fold f [] lst
;;
let assoc_list lst =

(*how does it know to use lst?*)
let g acc y = (y , count_occ lst y) :: acc in
let lst2 = fold g [] lst in
uniq lst2
;;
