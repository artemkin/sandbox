
type kind = Absolute | Relative

type t = {
  kind : kind;
  path : string list;
  name : string;
}

val of_string : string -> t option

