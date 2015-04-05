
open Core.Std

type link_kind = Hard_link | Dynamic_link

module Link_counters = struct
  type t = { hlinks: int; dlinks: int; }

  let empty = { hlinks = 0; dlinks = 0 }

  let succ t = function
    | Hard_link -> { t with hlinks = t.hlinks + 1 }
    | Dynamic_link -> { t with dlinks = t.dlinks + 1 }

  let sub t1 t2 =
    { hlinks = t1.hlinks - t2.hlinks;
      dlinks = t1.dlinks - t2.dlinks
    }

  let status t =
    match t.hlinks > 0, t.dlinks > 0 with
    | false, false -> `No_links
    | false, true -> `Dynamic_links_only
    | true, _ -> `Hard_links
end

type node =
  | File of Link_counters.t
  | Dir of node String.Map.t * Link_counters.t
  | Link of link_kind * Path.name_kind

module Link_info = struct
  type t =
    { dlinked: unit String.Table.t;
      hlinked: int String.Table.t;
      links: Link_counters.t String.Table.t;
    }

  let update t path_name (counters: Link_counters.t) =
    if counters.dlinks <> 0 then Hashtbl.add_exn t.dlinked ~key:path_name ~data:();
    if counters.hlinks <> 0 then Hashtbl.add_exn t.hlinked ~key:path_name ~data:counters.hlinks

  let create () =
    let create = String.Table.create in
    { dlinked = create (); hlinked = create (); links = create () }

  let of_node ~path ~name node =
    let t = create () in
    let rec loop ~path ~name = function
      | File counters -> update t (Path.concat_path_name ~path ~name) counters
      | Dir (nodes, counters) ->
        let path = (Path.concat_path_name ~path ~name) in
        update t path counters;
        Map.iter nodes ~f:(fun ~key ~data -> loop ~path ~name:key data)
      | Link (kind, _) ->
        Hashtbl.change t.links name (function
            | None -> Some (Link_counters.succ Link_counters.empty kind)
            | Some counters -> Some (Link_counters.succ counters kind))
    in
    loop ~path ~name node;
    t

  let get_hlinked_externally t =
    Hashtbl.filter_mapi t.hlinked ~f:(fun ~key ~data ->
        match Hashtbl.find t.links key with
        | None -> Some ()
        | Some { hlinks; dlinks = _ } -> if data = hlinks then None else Some ())
end


(* Auxiliary functions *)

let rec find_node nodes = function
  | [] -> assert false
  | hd :: tl ->
    match Map.find nodes hd with
    | None -> None
    | Some node ->
      match tl, node with
      | [], _ -> Some node
      | _, Dir (nodes, _) -> find_node nodes tl
      | _, File _ | _, Link _ -> None

let modify_node nodes path ~f =
  with_return (fun r ->
      let rec loop nodes = function
        | [] -> assert false
        | [hd] ->
          let cont = ref Fn.id in
          Map.change nodes hd (fun node ->
              match f node with
              | `Create_or_modify node -> Some node
              | `Remove -> None
              | `Remove_and_continue f -> cont := f; None
              | `Report_error err -> r.return (Error err))
          |> !cont
        | hd :: tl ->
          Map.change nodes hd (function
              | None -> r.return (Error `Wrong_path)
              | Some node ->
                match node with
                | File _ | Link _ -> Some node
                | Dir (nodes, counters) ->
                  let nodes = loop nodes tl in
                  Some (Dir (nodes, counters)))
      in
      Ok (loop nodes path))

let remove_links nodes ~name =
  let rec f ~key ~data =
    match data with
    | File _ -> Some data
    | Dir (nodes, counters) ->
      let nodes = Map.filter_mapi nodes ~f in
      Some (Dir (nodes, counters))
    | Link ((Dynamic_link | Hard_link), _) ->
      if key = name then None else Some data
  in
  Map.filter_mapi nodes ~f

let rec print_nodes nodes ~prefix =
  let print_name prefix name = printf "%s|_%s\n" prefix name in
  ignore (Map.fold nodes ~init:(Map.length nodes - 1) ~f:(fun ~key ~data i ->
      let last = i = 0 in
      let leaf, dir =
        (match data with
         | File _ ->
           print_name prefix (String.lowercase key);
           true, false
         | Link (kind, name_kind) ->
           let path_name = if name_kind <> File_name then key else
               let path, name = Path.split_path_name key in
               Path.concat_path_name ~path ~name:(String.lowercase name)
           in
           let kind = match kind with Hard_link -> "hlink" | Dynamic_link -> "dlink" in
           let link = sprintf "%s[%s]" kind path_name in
           print_name prefix link;
           true, false
         | Dir (nodes, _) ->
           print_name prefix key;
           let prefix = prefix ^ (if last then "    " else "|   ") in
           print_nodes nodes ~prefix;
           Map.length nodes = 0, true)
      in
      let prefix = if last then String.rstrip prefix else (prefix ^ "|") in
      if leaf && String.length prefix > 0 && (last || (not last && dir))
      then print_endline prefix;
      i - 1))

let rec filter_node node ~path ~name can't_be_removed =
  let path = (Path.concat_path_name ~path ~name) in
  match node with
  | Link (_, _) -> None
  | File _ ->
    if (Hashtbl.mem can't_be_removed path) then Some node else None
  | Dir (nodes, counters) ->
    let nodes = Map.filter_mapi nodes ~f:(fun ~key ~data ->
        filter_node data ~path ~name:key can't_be_removed) in
    if Map.length nodes > 0 || Hashtbl.mem can't_be_removed path
    then Some (Dir (nodes, counters)) else None

let remove_dlinks_and_update_link_counters nodes { Link_info.dlinked; links; _ } =
  let update counters path =
    match Hashtbl.find links path with
    | None -> counters
    | Some removed -> Link_counters.sub counters removed
  in
  let rec f ~path ~key:name ~data =
    match data with
    | File counters ->
      let path = Path.concat_path_name ~path ~name in
      Some (File (update counters path))
    | Dir (nodes, counters) ->
      let path = Path.concat_path_name ~path ~name in
      let nodes = Map.filter_mapi nodes ~f:(f ~path) in
      let counters = update counters path in
      Some (Dir (nodes, counters))
    | Link (Hard_link, _) -> Some data
    | Link (Dynamic_link, _) ->
      if Hashtbl.mem dlinked name then None else Some data
  in
  Map.filter_mapi nodes ~f:(f ~path:"")

let rec reset_link_counters = function
  | File _ -> File Link_counters.empty
  | Link (_, _) as link -> link
  | Dir (nodes, _) ->
    let nodes = Map.map nodes ~f:reset_link_counters in
    Dir (nodes, Link_counters.empty)

let increment_link nodes ~link_kind path =
  let name_kind = ref Path.File_or_dir_name in
  let root = modify_node nodes path ~f:(function
      | None -> `Report_error `Source_not_found
      | Some (Link _) -> `Report_error `Wrong_path
      | Some (File counters) ->
        name_kind := File_name;
        `Create_or_modify (File (Link_counters.succ counters link_kind))
      | Some (Dir (nodes, counters)) ->
        name_kind := Dir_name;
        `Create_or_modify (Dir (nodes, Link_counters.succ counters link_kind)))
  in
  Result.map root ~f:(fun root -> root, !name_kind)


(* Commands *)

type t =
  { root: node String.Map.t;
    cd: Path.t;
  }

let empty_dir = Dir (String.Map.empty, Link_counters.empty)
let empty_file = File Link_counters.empty
let drive = "C:"

let empty =
  let root = Map.add (String.Map.empty) ~key:drive ~data:empty_dir in
  let cd = Option.value_exn (Path.of_string drive) in
  { root; cd }

let with_parsed_path t ~path ~(name_kind: Path.name_kind) ~f =
  match Path.of_string path ~name_kind with
  | None -> Error `Wrong_path
  | Some path ->
    (match path.path_kind with
     | Absolute_path -> Some path
     | Relative_path -> Path.concat t.cd path)
    |> function
    | None -> Error `Wrong_path
    | Some path -> f path

let change_dir t path =
  with_parsed_path t ~path ~name_kind:Dir_name ~f:(fun p ->
      match find_node t.root p.path_name with
      | Some Dir _ -> Ok { t with cd = p }
      | None | Some File _ | Some Link _ -> Error `Wrong_path)

let make_dir t path =
  with_parsed_path t ~path ~name_kind:Dir_name ~f:(fun { path_name; _ } ->
      modify_node t.root path_name ~f:(function
          | Some _ -> `Report_error `Already_exists
          | None -> `Create_or_modify empty_dir)
      |> Result.map ~f:(fun root -> { t with root }))

let make_file t path =
  with_parsed_path t ~path ~name_kind:File_name ~f:(fun { path_name; _ } ->
      modify_node t.root path_name ~f:(function
          | Some _ -> `Report_error `Already_exists
          | None -> `Create_or_modify empty_file)
      |> Result.map ~f:(fun root -> { t with root }))

let try_to_remove_with_links path counters =
  match Link_counters.status counters with
  | `No_links -> `Remove
  | `Hard_links -> `Report_error `Hard_linked
  | `Dynamic_links_only ->
    let name = Path.path_name_to_string path in
    `Remove_and_continue (remove_links ~name)

let remove_dir t path =
  with_parsed_path t ~path ~name_kind:Path.Dir_name ~f:(fun p ->
      if p = t.cd then Error `Can't_remove_current_directory
      else
        modify_node t.root p.path_name ~f:(function
            | None -> `Report_error `Dir_not_found
            | Some (File _ | Link _) -> `Report_error `Not_dir
            | Some (Dir (nodes, counters)) ->
              if Map.length nodes <> 0 then `Report_error `Dir_not_empty
              else try_to_remove_with_links p counters)
        |> Result.map ~f:(fun root -> { t with root }))

let remove_file t path =
  with_parsed_path t ~path ~name_kind:File_name ~f:(fun p ->
      modify_node t.root p.path_name ~f:(function
          | None -> `Report_error `File_not_found
          | Some (Dir _ | Link _) -> `Report_error `Not_file
          | Some (File counters) ->
            try_to_remove_with_links p counters)
      |> Result.map ~f:(fun root -> { t with root }))

let remove_dir_recursive t path =
  with_parsed_path t ~path ~name_kind:Dir_name ~f:(fun p ->
      let link_info = ref (Link_info.create ()) in
      modify_node t.root p.path_name ~f:(function
          | None -> `Report_error `Dir_not_found
          | Some (File _ | Link _) -> `Report_error `Not_dir
          | Some (Dir (_, _) as node) ->
            let path = Path.path_to_string p in
            let name = p.name in
            link_info := Link_info.of_node ~path ~name node;
            let can't_be_removed = Link_info.get_hlinked_externally !link_info in
            let cd = (Path.path_name_to_string t.cd) in
            ignore (Hashtbl.add can't_be_removed ~key:cd ~data:());
            match filter_node node ~path ~name can't_be_removed with
            | None -> `Remove
            | Some node -> `Create_or_modify node)
      |> Result.map ~f:(fun root ->
          let root = remove_dlinks_and_update_link_counters root !link_info in
          { t with root }))

let copy t ~src ~dest =
  with_parsed_path t ~path:src ~name_kind:File_or_dir_name ~f:(fun src ->
      with_parsed_path t ~path:dest ~name_kind:Dir_name ~f:(fun dest ->
          match find_node t.root src.path_name with
          | None -> Error `Source_not_found
          | Some (Link _) -> Error `Wrong_path
          | Some src_node ->
            modify_node t.root dest.path_name ~f:(function
                | None -> `Report_error `Destination_not_found
                | Some (File _ | Link _) -> `Report_error `Not_dir
                | Some (Dir (nodes, counters)) ->   (* !!!!!!!!!!!!!  TODO *)
                  with_return (fun r ->
                      let nodes = Map.change nodes src.name (function
                          | Some _ -> r.return (`Report_error `Already_exists)
                          | None -> Some src_node)
                      in
                      `Create_or_modify (Dir (nodes, counters))))
            |> Result.map ~f:(fun root -> { t with root })))

let make_link t ~link_kind ~src ~dest =
  with_return (fun r ->
      with_parsed_path t ~path:src ~name_kind:File_or_dir_name ~f:(fun src ->
          with_parsed_path t ~path:dest ~name_kind:Dir_name ~f:(fun dest ->
              let open Result.Monad_infix in
              increment_link t.root ~link_kind src.path_name
              >>= fun (root, name_kind) ->
              modify_node root dest.path_name ~f:(function
                  | None -> `Report_error `Destination_not_found
                  | Some (File _ | Link _) -> `Report_error `Not_dir
                  | Some (Dir (nodes, counters)) ->
                    let name = Path.path_name_to_string src in
                    let nodes = Map.change nodes name (function
                        | Some (File _ | Dir _) -> r.return (Error `Wrong_path)
                        | Some (Link _) -> r.return (Ok t) (* existing link is not treated as error *)
                        | None -> Some (Link (link_kind, name_kind)))
                    in
                    `Create_or_modify (Dir (nodes, counters)))
              >>| fun root -> { t with root })))

let print t =
  match Map.find_exn t.root drive with
  | File _ | Link _ -> assert false
  | Dir (nodes, _) ->
    print_endline drive;
    print_nodes nodes ~prefix:""

