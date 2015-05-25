
open Genlex

type expr =
  | Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let rec eval_expr = function
  | Num n -> n
  | Add (a, b) -> (eval_expr a) + (eval_expr b)
  | Sub (a, b) -> (eval_expr a) - (eval_expr b)
  | Mul (a, b) -> (eval_expr a) * (eval_expr b)
  | Div (a, b) -> (eval_expr a) / (eval_expr b)

let rec expr_to_string = function
  | Num n -> string_of_int n
  | Add (a, b) -> "(" ^ (expr_to_string a) ^ "+" ^ (expr_to_string b) ^ ")"
  | Sub (a, b) -> "(" ^ (expr_to_string a) ^ "-" ^ (expr_to_string b) ^ ")"
  | Mul (a, b) -> "(" ^ (expr_to_string a) ^ "*" ^ (expr_to_string b) ^ ")"
  | Div (a, b) -> "(" ^ (expr_to_string a) ^ "/" ^ (expr_to_string b) ^ ")"

let precedence = function
  | `Add -> 2
  | `Sub -> 2
  | `Mul -> 3
  | `Div -> 3

let get_prev_op stack op =
  if Stack.is_empty stack then None
  else
    match Stack.top stack with
    | `OPEN_PAREN -> None
    | `OPERATOR prev_op when precedence prev_op < precedence op -> None
    | `OPERATOR prev_op ->
      ignore (Stack.pop stack);
      Some prev_op

let apply_op exprs op =
  let b = Stack.pop exprs in
  let a = Stack.pop exprs in
  let expr = match op with
    | `Add -> Add (a, b)
    | `Sub -> Sub (a, b)
    | `Mul -> Mul (a, b)
    | `Div -> Div (a, b)
  in
  Stack.push expr exprs

(* Shunting-yard algorithm *)
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
      let rec apply () =
        match Stack.pop operators with
        | `OPEN_PAREN -> ()
        | `OPERATOR op ->
          apply_op expressions op;
          apply ()
      in
      apply ();
      loop ()
    | Some (`OPERATOR op) ->
      Stream.junk s;
      let rec apply () =
        match get_prev_op operators op with
        | None ->
          Stack.push (`OPERATOR op) operators;
        | Some op ->
          apply_op expressions op;
          apply ()
      in
      apply ();
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
  let lexer = make_lexer ["+";"-";"*";"/";"(";")"] in
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
  test "((2 + 2))" 4;
  test "(((2 + 2)))" 4;
  test "(((2)+(2)))" 4;
  test "2 + 4 + 3 * ( 2 - 4 ) / 2" 3;
  test "2 - (3 * (4 - 1) ) / 2" (-2);
  test "3 * ( ( 2 + 2 ) ) + 4" 16;
  test "2+4+3*2-4/2" 10;
  test "2- -2" 4;
  test "(-2+3)" 1;
  test "(3-1)-2" 0;
  test "2*3*4-5*2" 14;
  test "2*3-4/2+7*3" 25;
  test "3-2" 1

let () =
  tests ()

