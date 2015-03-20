
(*
 * TODO
 * It seems we need to support trailing backslashes:
 * 1. "C:\\DIR1\\" - valid dir name
 * 2. "C:\\DIR1\\" - invalid file name
 * 3. "C:\\DIR1.TXT" - valid file name
 * 4. "C:\\DIR1.TXT\\" - invalid file name
*)

open Core.Std

type kind = Absolute | Relative

type t = {
  kind: kind;
  path: string list;
  name: string
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

let parse_and_validate_path path =
  let rec loop acc ~first ~absolute = function
    | [] ->
      if first then None
      else Some {
          kind = (if absolute then Absolute else Relative);
          path = List.rev (List.tl_exn acc);
          name = List.hd_exn acc;
        }
    | h :: t ->
      let last = t = [] in
      let drive = first && is_valid_drive_name h in
      let directory = is_valid_directory_name h in
      let file = last && is_valid_file_name h in
      if drive || directory || file then
        loop (h :: acc) ~first:false ~absolute:(absolute || drive) t
      else
        None
  in
  loop [] ~first:true ~absolute:false path

let of_string str =
  String.split str ~on:'\\'
  |> parse_and_validate_path

