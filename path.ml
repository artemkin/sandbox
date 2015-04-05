
(*
 * TODO: Already implemented. Add test cases
 * It seems we need to support trailing backslashes:
 * 1. "C:\\DIR1\\" - valid dir name
 * 2. "C:\\DIR1\\" - invalid file name
 * 3. "C:\\DIR1.TXT" - valid file name
 * 4. "C:\\DIR1.TXT\\" - invalid file name
*)

open Core.Std

type path_kind = Absolute_path | Relative_path

type name_kind = File_name | Dir_name | File_or_dir_name

type t =
  { path_kind : path_kind;
    name_kind : name_kind;
    path : string list;
    name : string;
    path_name : string list;
  }

let is_valid_drive_name str =
  String.length str = 2 && Char.is_alpha str.[0] && str.[1] = ':'

let is_valid_directory_name str =
  let len = String.length str in
  len >= 1 && len <= 8 && String.for_all str ~f:Char.is_alphanum

let is_valid_file_name str =
  match String.split str ~on:'.' with
  | [name] -> is_valid_directory_name name
  | [name; ext] ->
    is_valid_directory_name name &&
    String.length ext <= 3 &&
    String.for_all ext ~f:Char.is_alphanum
  | _ -> false

let get_name_kind ~trailing_backslash ~file_only =
  match trailing_backslash, file_only with
  | false, false -> Some File_or_dir_name
  | false, true -> Some File_name
  | true, false -> Some Dir_name
  | true, true -> None

let merge_name_kinds k1 k2 =
  match k1, k2 with
  | File_or_dir_name, File_or_dir_name -> Some File_or_dir_name
  | File_name, Dir_name | Dir_name, File_name -> None
  | File_name, _ | _, File_name -> Some File_name
  | Dir_name, _ | _, Dir_name -> Some Dir_name

let parse_and_validate_path ~name_kind path =
  let rec loop acc ~first ~absolute ~file_only = function
    | [] | [""] as t ->
      if first then None
      else
        let trailing_backslash = t = [""] in
        Option.bind (get_name_kind ~trailing_backslash ~file_only) (merge_name_kinds name_kind)
        |> Option.map ~f:(fun name_kind ->
            { path_kind = (if absolute then Absolute_path else Relative_path);
              name_kind;
              path = List.rev (List.tl_exn acc);
              name = List.hd_exn acc;
              path_name = List.rev acc;
            })
    | h :: t ->
      let last = t = [] || t = [""] in
      let drive = first && is_valid_drive_name h in
      let directory = is_valid_directory_name h in
      let file_only = last && not directory && is_valid_file_name h in
      if drive || directory || file_only then
        loop (h :: acc) ~first:false ~absolute:(absolute || drive) ~file_only t
      else
        None
  in
  loop [] ~first:true ~absolute:false ~file_only:false path

let of_string ?(name_kind = File_or_dir_name) str =
  String.strip str
  |> String.uppercase
  |> String.split ~on:'\\'
  |> parse_and_validate_path ~name_kind

let path_to_string t = String.concat ~sep:"\\" t.path

let path_name_to_string t = String.concat ~sep:"\\" t.path_name

let concat t1 t2 =
  let open Option.Monad_infix in
  (match t1.path_kind, t2.path_kind with
   | (Absolute_path | Relative_path), Absolute_path -> None
   | (Absolute_path | Relative_path) as kind, Relative_path -> Some kind)
  >>= fun path_kind ->
  (match t1.name_kind, t2.name_kind with
   | File_name, _ -> None
   | (Dir_name | File_or_dir_name),
     ((Dir_name | File_name | File_or_dir_name) as kind) -> Some kind)
  >>| fun name_kind ->
  let path = t1.path_name @ t2.path in
  let name = t2.name in
  { path_kind; name_kind; path; name; path_name = path @ [name]; }

let concat_path_name ~path ~name =
  if path = "" then name else path ^ "\\" ^ name

let split_path_name path_name = String.rsplit2_exn ~on:'\\' path_name

