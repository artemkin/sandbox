
(* fish operator for writer monad *)
let (>=>) a b = fun x ->
  let y, s1 = a x in
  let z, s2 = b y in
  z, s1 ^ s2

let return x = x, ""

(* Example *)

let concat lst = (String.concat " " lst), "concat "
let uppercase str = (String.uppercase str), "uppercase "

let process = concat >=> uppercase

let () =
  let input = ["one";"two";"three";"four"] in
  let result, log = process input in
  Printf.printf "Result: %s\nLog: %s\n" result log

