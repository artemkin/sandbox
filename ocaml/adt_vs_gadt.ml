
type expr =
  | SDefaultInt
  | SDefaultBool
  | SDefaultString
  | SInt of int
  | SBool of bool
  | SString of string

let str = function
  | SDefaultInt -> "42"
  | SDefaultBool -> "true"
  | SDefaultString -> "foo"
  | SInt n -> string_of_int n
  | SBool b -> string_of_bool b
  | SString s -> s

type _ expr =
  | DefaultInt : int expr
  | DefaultBool : bool expr
  | DefaultString : string expr
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | String : string -> string expr

let value : type a. a expr -> a = function
  | DefaultInt -> 42
  | DefaultBool -> true
  | DefaultString -> "foo"
  | Int n -> n
  | Bool b -> b
  | String s -> s

