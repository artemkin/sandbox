
open Core.Std

type node = Dir of string * node list | File of string

let rec find_dir node = function
  | [] -> None
  | h :: t ->
    match t, node with
    | [], Dir (name, _) when name = h -> Some node
    | _, Dir (name, nodes) when name = h ->
      List.find_map nodes ~f:(fun node -> find_dir node t)
    | _, _ -> None

type t = {
  root: node;
  cd: node;
}

type error =
  | Err_wrong_path

let create () =
  let root = Dir ("C:", []) in
  { root; cd = root }


(* Monadic variant: one line shorter, but less readable

let change_dir t path =
  (let open Option.Monad_infix in
   Path.of_string path >>= fun { kind; path; name } ->
   let tree = (match kind with Absolute -> t.root | Relative -> t.cd) in
   find_dir tree (path @ [name]) >>= fun dir ->
   Some { root = t.root; cd = dir })
  |> Result.of_option ~error:Err_wrong_path

*)

let change_dir t path =
  match Path.of_string path with
  | None -> Error Err_wrong_path
  | Some {kind; path; name} ->
    let tree = (match kind with Absolute -> t.root | Relative -> t.cd) in
    match find_dir tree (path @ [name]) with
    | None -> Error Err_wrong_path
    | Some dir -> Ok { root = t.root; cd = dir }



let print_name prefix name = printf "%s|_%s\n" prefix name

let rec print_node prefix ~last = function
  | File name ->
    print_name prefix name
  | Dir (name, nodes) ->
    print_name prefix name;
    let prefix = prefix ^ (if last then "    " else "|   ") in
    print_nodes prefix nodes;
    if last then print_endline prefix
and print_nodes prefix = function
  | [] -> ()
  | [x] -> print_node prefix ~last:true x
  | h :: t ->
    print_node prefix ~last:false h;
    print_nodes prefix t

let print = function
  | File name -> print_endline name
  | Dir (name, nodes) ->
    print_endline name;
    print_nodes "" nodes

let () =
  let test =
    Dir ("C:", [
        Dir ("DIR1", [
            Dir ("DIR2", [
                Dir ("DIR3", [
                    File "readme.txt"
                  ])
              ]);
            Dir ("EDIR4", [
                File "temp.dat"
              ])
          ]);
        Dir ("QWER", [
          ])
      ])
  in
  print test;
  (match Path.of_string "C:\\DIR1\\readme.txt" with
   | None -> printf "wrong path\n"
   | Some path -> printf "%s %s %s\n"
                    (if path.Path.kind = Absolute then "absolute" else "relative")
                    (List.to_string path.path ~f:Fn.id)
                    path.Path.name);
  let fs = { root = test; cd = test; } in
  match change_dir fs "C:\\DIR1" with
  | Ok { cd; _ } -> print cd
  | Error _ -> print_endline "Wrong path"

