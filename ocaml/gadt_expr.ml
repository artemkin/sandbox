
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

type _ expr_type =
  | Int_expr : int expr_type
  | Bool_expr : bool expr_type

type any_expr = Expr : 'a expr_type * 'a expr -> any_expr

let rec eval_aexpr = function
  | Num n -> n
  | Add (a, b) -> (eval_aexpr a) + (eval_aexpr b)
  | Sub (a, b) -> (eval_aexpr a) - (eval_aexpr b)
  | Mul (a, b) -> (eval_aexpr a) * (eval_aexpr b)
  | Div (a, b) -> (eval_aexpr a) / (eval_aexpr b)

let rec eval_bexpr = function
  | Lt  (a, b) -> (eval_aexpr a) <  (eval_aexpr b)
  | Gt  (a, b) -> (eval_aexpr a) >  (eval_aexpr b)
  | And (a, b) -> (eval_bexpr a) && (eval_bexpr b)
  | Or  (a, b) -> (eval_bexpr a) || (eval_bexpr b)

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

let eval_expr : any_expr -> [> `Int of int | `Bool of bool] = function
  | Expr (Int_expr, expr) -> `Int (eval expr)
  | Expr (Bool_expr, expr) -> `Bool (eval expr)

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
  | `Or         -> 1
  | `And        -> 2
  | `Lt  | `Gt  -> 3
  | `Add | `Sub -> 4
  | `Mul | `Div -> 5

let op_to_string = function
  | `Add -> "+"
  | `Sub -> "-"
  | `Mul -> "*"
  | `Div -> "/"
  | `Lt  -> "<"
  | `Gt  -> ">"
  | `And -> "and"
  | `Or  -> "or"


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


let apply_op exprs op =
  let create_expr (Expr (a_type, a)) (Expr (b_type, b)) =
    match op, a_type, a, b_type, b with
    | `Add, Int_expr,  a, Int_expr,  b -> Expr (Int_expr,  Add (a, b))
    | `Sub, Int_expr,  a, Int_expr,  b -> Expr (Int_expr,  Sub (a, b))
    | `Mul, Int_expr,  a, Int_expr,  b -> Expr (Int_expr,  Mul (a, b))
    | `Div, Int_expr,  a, Int_expr,  b -> Expr (Int_expr,  Div (a, b))
    | `Lt,  Int_expr,  a, Int_expr,  b -> Expr (Bool_expr, Lt  (a, b))
    | `Gt,  Int_expr,  a, Int_expr,  b -> Expr (Bool_expr, Gt  (a, b))
    | `And, Bool_expr, a, Bool_expr, b -> Expr (Bool_expr, And (a, b))
    | `Or,  Bool_expr, a, Bool_expr, b -> Expr (Bool_expr, Or  (a, b))
    | (`Add | `Sub | `Mul | `Div | `Lt | `Gt | `And | `Or), _, a, _, b ->
      let op = op_to_string op in
      let a = expr_to_string a in
      let b = expr_to_string b in
      let err = Printf.sprintf "Operator '%s' applied to wrong operands: a(%s) b(%s)" op a b in
      failwith err
  in
  let b = Stack.pop exprs in
  let a = Stack.pop exprs in
  let expr = create_expr a b in
  Stack.push expr exprs

let parse_expr s =
  let expressions = Stack.create () in
  let operators = Stack.create () in
  let rec loop () =
    match Stream.peek s with
    | Some (`Num n) ->
      Stream.junk s;
      Stack.push (Expr (Int_expr, (Num n))) expressions;
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
  let lexer = make_lexer ["+";"-";"*";"/";"<";">";"and";"or";"(";")"] in
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
          | _, Kwd "+"   -> Some (`OPERATOR `Add)
          | _, Kwd "-"   -> Some (`OPERATOR `Sub)
          | _, Kwd "*"   -> Some (`OPERATOR `Mul)
          | _, Kwd "/"   -> Some (`OPERATOR `Div)
          | _, Kwd "<"   -> Some (`OPERATOR `Lt)
          | _, Kwd ">"   -> Some (`OPERATOR `Gt)
          | _, Kwd "and" -> Some (`OPERATOR `And)
          | _, Kwd "or"  -> Some (`OPERATOR `Or)
          | _, Kwd "("   -> Some `OPEN_PAREN
          | _, Kwd ")"   -> Some `CLOSE_PAREN
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
  test "2 + 2" (`Int 4);
  test "2 + 4 + 3 * 2 - 4 / 2" (`Int 10);
  test "2 + 2 + 2" (`Int 6);
  test "(2 + 2)" (`Int 4);
  test "((2 + 2))" (`Int 4);
  test "(((2 + 2)))" (`Int 4);
  test "(((2)+(2)))" (`Int 4);
  test "2 + 4 + 3 * ( 2 - 4 ) / 2" (`Int 3);
  test "2 - (3 * (4 - 1) ) / 2" (`Int (-2));
  test "3 * ( ( 2 + 2 ) ) + 4" (`Int 16);
  test "2+4+3*2-4/2" (`Int 10);
  test "2- -2" (`Int 4);
  test "(-2+3)" (`Int 1);
  test "(3-1)-2" (`Int 0);
  test "3-2" (`Int 1);
  test "1<2" (`Bool true);
  test "1>2" (`Bool false);
  test "2+3<10-3" (`Bool true);
  test "6/2-1 < 3 and 1 > (2+7)" (`Bool false);
  test "6/2-1 < 3 or 1 > (2+7)" (`Bool true);
  test "(2+3)<10-3" (`Bool true);
  test "0<0" (`Bool false)

let () =
  tests ()

