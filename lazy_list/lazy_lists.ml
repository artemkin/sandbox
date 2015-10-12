

module LazyList = struct

  type 'a node =
    | Empty
    | Node of 'a * 'a t
  and 'a t = 'a node Lazy.t

  let empty = Lazy.from_val Empty

  let hd t = Lazy.from_fun (fun () ->
      match Lazy.force t with
      | Empty -> failwith "LazyList.hd"
      | Node (h, _) -> h)

  let tl t = Lazy.from_fun (fun () ->
      match Lazy.force t with
      | Empty -> failwith "LazyList.tl"
      | Node (_, t) -> t)

  let rec of_list lst = Lazy.from_fun (fun () ->
      match lst with
      | [] -> Empty
      | h :: t -> Node (h, of_list t))

  let rec to_list t =
    match Lazy.force t with
    | Empty -> []
    | Node (h, t) -> h :: (to_list t)

  let rec map t ~f = Lazy.from_fun (fun () ->
      match Lazy.force t with
      | Empty -> Empty
      | Node (h, t) -> Node (f h, map t ~f))

  let fold t ~init ~f =
    let rec loop t init f =
      match Lazy.force t with
      | Empty -> init
      | Node (h, t) -> loop t (f init h) f
    in
    Lazy.from_fun (fun () -> loop t init f)

end


let () =
  [5;4;3;2;1]
  |> LazyList.of_list
  |> LazyList.map ~f:(fun n -> n * 2)
  |> LazyList.fold ~init:0 ~f:(fun acc n -> acc - n)
  |> fun num ->
  Printf.printf "%d\n" (Lazy.force num)

