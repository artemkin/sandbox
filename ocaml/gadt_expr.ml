
open Genlex

type _ expr =
  | Num : int -> int expr
  | Add : int expr * int expr -> int expr
  | Sub : int expr * int expr -> int expr
  | Mul : int expr * int expr -> int expr
  | Div : int expr * int expr -> int expr
  | Lt : int expr * int expr -> bool expr
  | Gt : int expr * int expr -> bool expr
  | And : bool expr * bool expr -> bool expr
  | Or : bool expr * bool expr -> bool expr

let rec eval_expr = function
  | Num n -> n
  | Add (a, b) -> (eval_expr a) + (eval_expr b)
  | Sub (a, b) -> (eval_expr a) - (eval_expr b)
  | Mul (a, b) -> (eval_expr a) * (eval_expr b)
  | Div (a, b) -> (eval_expr a) / (eval_expr b)

let rec eval_bexpr = function
  | Lt (a, b) -> (eval_expr a) < (eval_expr b)
  | Gt (a, b) -> (eval_expr a) > (eval_expr b)
  | And (a, b) -> (eval_bexpr a) && (eval_bexpr b)
  | Or (a, b) -> (eval_bexpr a) || (eval_bexpr b)

let rec eval : type a. a expr -> a = function
  | Num n -> n
  | Add (a, b) -> (eval a) + (eval b)
  | Sub (a, b) -> (eval a) - (eval b)
  | Mul (a, b) -> (eval a) * (eval b)
  | Div (a, b) -> (eval a) / (eval b)
  | Lt  (a, b) -> (eval a) < (eval b)
  | Gt  (a, b) -> (eval a) > (eval b)
  | And (a, b) -> (eval a) && (eval b)
  | Or  (a, b) -> (eval a) || (eval b)

let rec expr_to_string : type a. a expr -> string = function
  | Num n -> string_of_int n
  | Add (a, b) -> "(" ^ (expr_to_string a) ^ "+" ^ (expr_to_string b) ^ ")"
  | Sub (a, b) -> "(" ^ (expr_to_string a) ^ "-" ^ (expr_to_string b) ^ ")"
  | Mul (a, b) -> "(" ^ (expr_to_string a) ^ "*" ^ (expr_to_string b) ^ ")"
  | Div (a, b) -> "(" ^ (expr_to_string a) ^ "/" ^ (expr_to_string b) ^ ")"
  | Lt  (a, b) -> "(" ^ (expr_to_string a) ^ "<" ^ (expr_to_string b) ^ ")"
  | Gt  (a, b) -> "(" ^ (expr_to_string a) ^ ">" ^ (expr_to_string b) ^ ")"
  | And (a, b) -> "(" ^ (expr_to_string a) ^ "&" ^ (expr_to_string b) ^ ")"
  | Or  (a, b) -> "(" ^ (expr_to_string a) ^ "|" ^ (expr_to_string b) ^ ")"

let precedence = function
  | `Lt | `Gt -> 1
  | `Add | `Sub -> 2
  | `Mul | `Div -> 3

let process_op stack op =
  let prev_op =
    if Stack.is_empty stack then None
    else
      match Stack.top stack with
      | `OPEN_PAREN -> None
      | `OPERATOR prev_op when precedence prev_op < precedence op -> None
      | `OPERATOR prev_op ->
        ignore (Stack.pop stack);
        Some prev_op
  in
  Stack.push (`OPERATOR op) stack;
  prev_op


type _ ty =
  | TyInt : int ty
  | TyBool : bool ty

type any_expr = Any : 'a ty * 'a expr -> any_expr

let create_expr op a b =
  match op, a, b with
  | `Add, Any (TyInt,  a), Any (TyInt,  b) -> Any (TyInt,  Add (a, b))
  | `Sub, Any (TyInt,  a), Any (TyInt,  b) -> Any (TyInt,  Sub (a, b))
  | `Mul, Any (TyInt,  a), Any (TyInt,  b) -> Any (TyInt,  Mul (a, b))
  | `Div, Any (TyInt,  a), Any (TyInt,  b) -> Any (TyInt,  Div (a, b))
  | `Lt,  Any (TyInt,  a), Any (TyInt,  b) -> Any (TyBool, Lt  (a, b))
  | `Gt,  Any (TyInt,  a), Any (TyInt,  b) -> Any (TyBool, Gt  (a, b))
  | `And, Any (TyBool, a), Any (TyBool, b) -> Any (TyBool, And (a, b))
  | `Or,  Any (TyBool, a), Any (TyBool, b) -> Any (TyBool, Or  (a, b))
  | _, _, _ -> assert false

let eval_any : any_expr -> [> `Int of int | `Bool of bool] = function
  | Any (TyInt, expr) -> `Int (eval expr)
  | Any (TyBool, expr) -> `Bool (eval expr)

(*
type result = Result : 'a ty * 'a -> result

let eval_any = function
  | Any (TyInt, expr) -> Result (TyInt, (eval expr))
  | Any (TyBool, expr) -> Result (TyBool, (eval expr))


let result_to_string = function
  | Result (TyInt, n) -> string_of_int n
  | Result (TyBool, n) -> string_of_bool n
  *)

(*
let extract_expr (type v) (a:v expr) (b:v expr) =
  match a, b with
  | Add _ as a, Add _ as b -> a, b
  *)

(* type expr' = Int_expr of int expr | Bool_expr of bool expr *)

(*
let concrete : any_expr -> expr' = function
  | Any (Num _ as expr) -> Int_expr expr
  | Any (Add _ as expr) -> Int_expr expr
  | Any (Sub _ as expr) -> Int_expr expr
  | Any (Mul _ as expr) -> Int_expr expr
  | Any (Div _ as expr) -> Int_expr expr
  | Any (Lt  _ as expr) -> Bool_expr expr
  | Any (Gt  _ as expr) -> Bool_expr expr
  | Any (And _ as expr) -> Bool_expr expr
  | Any (Or  _ as expr) -> Bool_expr expr

let create_expr op a b =
  match op, concrete a, concrete b with
  | `Add, Int_expr  a, Int_expr  b -> Any (Add (a, b))
  | `Sub, Int_expr  a, Int_expr  b -> Any (Sub (a, b))
  | `Mul, Int_expr  a, Int_expr  b -> Any (Mul (a, b))
  | `Div, Int_expr  a, Int_expr  b -> Any (Div (a, b))
  | `Lt,  Int_expr  a, Int_expr  b -> Any (Lt  (a, b))
  | `Gt,  Int_expr  a, Int_expr  b -> Any (Gt  (a, b))
  | `And, Bool_expr a, Bool_expr b -> Any (And (a, b))
  | `Or,  Bool_expr a, Bool_expr b -> Any (Or  (a, b))
  | _, _, _ -> assert false
*)

let apply_op (exprs:any_expr Stack.t) op =
  let b = Stack.pop exprs in
  let a = Stack.pop exprs in
  let expr =
    match op, concrete a, concrete b with
    | `Add, Int_expr a, Int_expr b -> Add (a, b)
    | `Sub, Int_expr a, Int_expr b -> Sub (a, b)
    | `Mul, Int_expr a, Int_expr b -> Mul (a, b)
    | `Div, Int_expr a, Int_expr b -> Div (a, b)
    | `Lt, Int_expr a, Int_expr b -> Lt (a, b)
  in
  Stack.push expr exprs

let parse_expr s =
  let expressions = Stack.create () in
  let operators = Stack.create () in
  let rec loop () =
    match Stream.peek s with
    | Some (`Num n) ->
      Stream.junk s;
      Stack.push (Num n) expressions;
      loop ()
    | Some `OPEN_PAREN ->
      Stream.junk s;
      Stack.push `OPEN_PAREN operators;
      loop ()
    | Some `CLOSE_PAREN ->
      Stream.junk s;
      let rec apply_prev_ops () =
        match Stack.pop operators with
        | `OPEN_PAREN -> ()
        | `OPERATOR op ->
          apply_op expressions op;
          apply_prev_ops ()
      in
      apply_prev_ops ();
      loop ()
    | Some (`OPERATOR op) ->
      Stream.junk s;
      (match process_op operators op with
       | None -> ()
       | Some op -> apply_op expressions op);
      loop ()
    | None -> ()
  in
  loop ();
  while not (Stack.is_empty operators) do
    match Stack.pop operators with
    | `OPERATOR op -> apply_op expressions op
    | `OPEN_PAREN -> failwith "Unclosed '('"
  done;
  let ex = Stack.pop expressions in
  ex

let lex_expr s =
  let lexer = make_lexer ["+";"-";"*";"/";"(";")";"<";">"] in
  let tokens = lexer s in
  let prev_token = ref None in
  let next_token = ref None in
  let rec next _i =
    try
      let token =
        match !next_token with
        | Some _ as token ->
          next_token := None;
          token
        | None ->
          match !prev_token, Stream.next tokens with
          | _, Kwd "+" -> Some (`OPERATOR `Add)
          | _, Kwd "-" -> Some (`OPERATOR `Sub)
          | _, Kwd "*" -> Some (`OPERATOR `Mul)
          | _, Kwd "/" -> Some (`OPERATOR `Div)
          | _, Kwd "<" -> Some (`OPERATOR `Lt)
          | _, Kwd ">" -> Some (`OPERATOR `Gt)
          | _, Kwd "(" -> Some `OPEN_PAREN
          | _, Kwd ")" -> Some `CLOSE_PAREN
          | None, Int n | Some _, Int n when n >= 0 -> Some (`Num n)
          | Some prev_token, Int n -> (* replace -2 with - 2 if needed *)
            (match prev_token with
             | `OPERATOR _ | `OPEN_PAREN -> Some (`Num n)
             | `CLOSE_PAREN | `Num _ ->
               next_token := Some (`Num (abs n));
               Some (`OPERATOR `Sub))
          | _ -> failwith "Wrong token"
      in
      prev_token := token;
      token
    with
      Stream.Failure -> None
  in
  Stream.from next

let tests () =
  let test expr result =
    let s = Stream.of_string expr in
    lex_expr s |> parse_expr |> fun expr ->
    (*    Printf.printf "%s\n" (expr_to_string expr); *)
    eval_expr expr |> fun result' ->
    if result <> result' then assert false
  in
  test "2 + 2" 4;
  test "2 + 4 + 3 * 2 - 4 / 2" 10;
  test "2 + 2 + 2" 6;
  test "(2 + 2)" 4;
  test "2 + 4 + 3 * ( 2 - 4 ) / 2" 3;
  test "2 - (3 * (4 - 1) ) / 2" (-2);
  test "3 * ( ( 2 + 2 ) ) + 4" 16;
  test "2+4+3*2-4/2" 10;
  test "2- -2" 4;
  test "(-2+3)" 1;
  test "(3-1)-2" 0;
  test "3-2" 1

let () =
  tests ()

