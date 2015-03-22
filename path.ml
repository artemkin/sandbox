
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

type t = {
  path_kind : path_kind;
  name_kind : name_kind;
  path : string list;
  name : string;
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

let parse_and_validate_path path =
  let rec loop acc ~first ~absolute ~file_only = function
    | [] | [""] as t ->
      if first then None
      else
        let trailing_backslash = t = [""] in
        get_name_kind ~trailing_backslash ~file_only
        |> Option.map ~f:(fun name_kind ->
            {
              path_kind = (if absolute then Absolute_path else Relative_path);
              name_kind;
              path = List.rev (List.tl_exn acc);
              name = List.hd_exn acc;
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

let of_string str =
  String.strip str
  |> String.split ~on:'\\'
  |> parse_and_validate_path

