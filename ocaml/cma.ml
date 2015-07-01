
let cumulative_moving_average lst =
  let rec aux avg n = function
    | [] -> avg
    | x :: tl ->
      let n = n +. 1.0 in
      aux (avg +. (x -. avg) /. n) n tl
  in
  aux 0.0 0.0 lst

let () =
  let avg = cumulative_moving_average [1.0;5.3;6.134;7.12;5.93] in
  Printf.printf "%f\n" avg

