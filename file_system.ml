
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
      | _, _ -> None

let create_dir node path =
  with_return (fun r ->
      let rec loop node = function
        | [] -> assert false
        | h :: t ->
          match t, node with
          | [], Dir nodes ->
            let nodes = Map.change nodes h (function
                | Some _ -> r.return None
                | None -> Some (Dir String.Map.empty)) in
            Some (Dir nodes)
          | _, Dir nodes ->
            let nodes = Map.change nodes h (function
                | None -> r.return None
                | Some node -> loop node t) in
            Some (Dir nodes)
          | _, _ -> Some node
      in
      loop node path)

let create_file node path =
  with_return (fun r ->
      let rec loop node = function
        | [] -> assert false
        | h :: t ->
          match t, node with
          | [], Dir nodes ->
            let nodes = Map.change nodes h (function
                | Some _ -> r.return None
                | None -> Some File) in
            Some (Dir nodes)
          | _, Dir nodes ->
            let nodes = Map.change nodes h (function
                | None -> r.return None
                | Some node -> loop node t) in
            Some (Dir nodes)
          | _, _ -> Some node
      in
      loop node path
    )

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
      let prefix = String.rstrip prefix in
      if last && completed && String.length prefix > 0 then print_endline prefix;
      i - 1))

let create drive =
  let empty = Dir String.Map.empty in
  let root = Map.add (String.Map.empty) ~key:drive ~data:empty in
  { root; cd = [drive] }


let change_dir t path =
  match Path.of_string path with
  | None -> Error `Wrong_path
  | Some { kind; path; name } ->
    let path = (match kind with Absolute -> path | Relative -> t.cd @ path) in
    let path = path @ [name] in
    match find_dir t.root path with
    | None -> Error `Wrong_path
    | Some _ -> Ok { root = t.root; cd = path }

let make_dir t path =
  match Path.of_string path with
  | None -> Error `Wrong_path
  | Some { kind; path; name } ->
    let path = (match kind with Absolute -> path | Relative -> t.cd @ path) in
    let path = path @ [name] in
    match create_dir (Dir t.root) path with
    | None -> Error `Wrong_path
    | Some root ->
      match root with
      | File -> assert false
      | Dir nodes -> Ok { root = nodes; cd = t.cd }

let make_file t path =
  match Path.of_string path with
  | None -> Error `Wrong_path
  | Some { kind; path; name } ->
    let path = (match kind with Absolute -> path | Relative -> t.cd @ path) in
    let path = path @ [name] in
    match create_file (Dir t.root) path with
    | None -> Error `Wrong_path
    | Some root ->
      match root with
      | File -> assert false
      | Dir nodes -> Ok { root = nodes; cd = t.cd }

let print t drive =
  match Map.find_exn t.root drive with
  | File -> assert false
  | Dir nodes ->
    print_endline drive;
    print_nodes "" nodes

