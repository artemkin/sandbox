
open Core.Std

type node = File | Dir of node String.Map.t

type t = {
  root: node String.Map.t;
  cd: string list;
}

let rec find_dir nodes = function
  | [] -> assert false
  | h :: t ->
    match Map.find nodes h with
    | None -> None
    | Some node ->
      match t, node with
      | [], Dir _ -> Some node
      | _, Dir nodes -> find_dir nodes t
      | _ -> None

let modify_node nodes path ~f =
  with_return (fun r ->
      let rec loop nodes = function
        | [] -> assert false
        | [h] ->
          Map.change nodes h (fun node ->
              match f node with
              | `Create_or_modify node -> Some node
              | `Remove -> None
              | `Report_error err -> r.return (Error err))
        | h :: t ->
          Map.change nodes h (function
              | None -> r.return (Error `Wrong_path)
              | Some node ->
                match node with
                | File -> Some node
                | Dir nodes ->
                  let nodes = loop nodes t in
                  Some (Dir nodes))
      in
      Ok (loop nodes path))

let rec print_nodes prefix nodes =
  let print_name prefix name = printf "%s|_%s\n" prefix name in
  ignore (Map.fold nodes ~init:(Map.length nodes - 1) ~f:(fun ~key ~data i ->
      let last = i = 0 in
      let completed =
        (match data with
         | File ->
           print_name prefix (String.lowercase key);
           true
         | Dir nodes ->
           print_name prefix key;
           let prefix = prefix ^ (if last then "    " else "|   ") in
           print_nodes prefix nodes;
           Map.length nodes = 0)
      in
      let prefix = if last then String.rstrip prefix else (prefix ^ "|") in
      if completed && String.length prefix > 0 then print_endline prefix;
      i - 1))

let create drive =
  let empty = Dir String.Map.empty in
  let root = Map.add (String.Map.empty) ~key:drive ~data:empty in
  { root; cd = [drive] }


let with_parsed_path t path ~f =
  match Path.of_string path with
  | None -> Error `Wrong_path
  | Some { kind; path; name } ->
    let path = (match kind with Absolute -> path | Relative -> t.cd @ path) in
    f path name

let change_dir t path =
  with_parsed_path t path ~f:(fun path name ->
      let path = path @ [name] in
      match find_dir t.root path with
      | None -> Error `Wrong_path
      | Some _ -> Ok { t with cd = path })

let make_dir t path =
  with_parsed_path t path ~f:(fun path name ->
      let path = path @ [name] in
      modify_node t.root path ~f:(function
          | Some _ -> `Report_error `Dir_already_exists
          | None -> `Create_or_modify (Dir String.Map.empty))
      |> Result.map ~f:(fun root -> { t with root }))

let make_file t path =
  with_parsed_path t path ~f:(fun path name ->
      let path = path @ [name] in
      modify_node t.root path ~f:(function
          | Some _ -> `Report_error `File_already_exists
          | None -> `Create_or_modify File)
      |> Result.map ~f:(fun root -> { t with root }))

let remove_dir t path =
  with_parsed_path t path ~f:(fun path name ->
      let path = path @ [name] in
      if path = t.cd then Error `Can't_remove_current_directory
      else
        modify_node t.root path ~f:(function
            | None -> `Report_error `Dir_not_found
            | Some node ->
              match node with
              | File -> `Report_error `Not_dir
              | Dir nodes ->
                if Map.length nodes <> 0 then `Report_error `Dir_not_empty
                else `Remove)
        |> Result.map ~f:(fun root -> { t with root }))

let remove_file t path =
  with_parsed_path t path ~f:(fun path name ->
      let path = path @ [name] in
      modify_node t.root path ~f:(function
          | None -> `Report_error `File_not_found
          | Some node ->
            match node with
            | Dir _ -> `Report_error `Not_file
            | File -> `Remove)
      |> Result.map ~f:(fun root -> { t with root }))

let print t drive =
  match Map.find_exn t.root drive with
  | File -> assert false
  | Dir nodes ->
    print_endline drive;
    print_nodes "" nodes

