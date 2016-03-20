
(*
  Decomposes given interger into a sequience of unique smaller integers.
*)
let decompose n =
  let rec aux i n acc sum =
    if n > 0 then
      aux (i + 1) (n - i) (i :: acc) (sum + i)
    else
      List.rev acc, sum
  in
  let lst, sum = aux 1 n [] 0 in
  let diff = sum - n in
  List.filter (fun i -> i <> diff) lst

let print_list lst =
  List.iter (fun n -> Printf.printf "%d " n) lst;
  print_newline ()

let () =
  let n = int_of_string @@ Sys.argv.(1) in
  let lst = decompose n in
  print_list lst;
  Printf.printf "sum: %d\n" (List.fold_left (+) 0 lst)

