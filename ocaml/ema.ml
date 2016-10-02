

let exponential_moving_average lst interval =
  let rec aux avg t' = function
    | [] -> avg
    | (v, t) :: tl ->
        let a = (t -. t') /. interval in
        let k = exp (-.a) in
        aux (k *. avg +. (1.0 -. k) *. v) t tl
  in
  aux 0.0 0.0 lst

let () =
  let avg = exponential_moving_average [1.0, 1.0; 1.0, 2.0; 1.0, 3.0; 1.0, 4.0] 1.0 in
  Printf.printf "%f\n" avg

