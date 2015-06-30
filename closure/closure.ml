
let generate () =
  let counter = ref 0 in
  let rec aux = function
    | 0 -> []
    | n ->
      let increment () =
        let prev = !counter in
        counter := prev + 1;
        prev
      in
      increment::(aux (n - 1))
  in
  aux 10

let () =
  let seq = generate () in
  let seq2 = seq in
  List.iter (fun f -> Printf.printf "%d\n" (f ())) seq;
  List.iter (fun f -> Printf.printf "%d\n" (f ())) seq2

