
type path_kind = Absolute_path | Relative_path

type name_kind = File_name | Dir_name | File_or_dir_name

type t = {
  path_kind : path_kind;
  name_kind : name_kind;
  path : string list;
  name : string;
}

val of_string : string -> t option

