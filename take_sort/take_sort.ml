
(*
 * Similar problem in Haskell
 * http://stackoverflow.com/questions/5949871/memory-efficient-algorithm-for-take-n-sort-xs-sorted-prefix-problem
 *)

open Core.Std

let generate () =
  let rec aux acc = function
    | 0 -> acc
    | n ->
      let num = Random.int 100000000 in
      aux (num :: acc) (n - 1)
  in
  aux [] 10000000

let sort lst = List.sort ~cmp:(Int.compare) lst
let sort2 lst =
  let arr = Array.of_list lst in
  Array.sort ~cmp:(Int.compare) arr;
  Array.to_list arr

let () =
  let rxs = generate () in
  printf "Sorting...%!";
  let sorted = sort2 rxs in
  let result = List.take sorted 20 in
  List.iter result ~f:(fun n -> printf "%d " n);
  printf "\nAllocated: %d MBytes\n" (Int.of_float (Gc.allocated_bytes () /. 1024. /. 1024.))

