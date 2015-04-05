
type path_kind = Absolute_path | Relative_path

type name_kind = File_name | Dir_name | File_or_dir_name

type t =
  { path_kind : path_kind;
    name_kind : name_kind;
    path : string list;
    name : string;
    path_name : string list;
  }

val of_string : ?name_kind:name_kind -> string -> t option

val to_string : t -> string

val concat : t -> t -> t option

val concat_path_name : path:string -> name:string -> string

