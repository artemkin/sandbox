
open Genlex

type _ expr =
  | Var : string -> int expr
  | Int_lit : int -> int expr
  | Bool_lit : bool -> bool expr
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

let rec eval : type a. (string, int) Hashtbl.t -> a expr -> a =
  fun vars expr -> match expr with
    | Var name -> Hashtbl.find vars name
    | Int_lit n -> n
    | Bool_lit n -> n
    | Add (a, b) -> (eval vars a) + (eval vars b)
    | Sub (a, b) -> (eval vars a) - (eval vars b)
    | Mul (a, b) -> (eval vars a) * (eval vars b)
    | Div (a, b) -> (eval vars a) / (eval vars b)
    | Lt  (a, b) -> (eval vars a) < (eval vars b)
    | Gt  (a, b) -> (eval vars a) > (eval vars b)
    | And (a, b) -> (eval vars a) && (eval vars b)
    | Or  (a, b) -> (eval vars a) || (eval vars b)

let eval_expr : (string, int) Hashtbl.t -> any_expr -> [> `Int of int | `Bool of bool] =
  fun vars expr -> match expr with
    | Expr (Int_expr, expr) -> `Int (eval vars expr)
    | Expr (Bool_expr, expr) -> `Bool (eval vars expr)

let rec expr_to_string : type a. a expr -> string = function
  | Var name -> name
  | Int_lit n -> string_of_int n
  | Bool_lit n -> string_of_bool n
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

type token = [
  | `ASSIGN
  | `CLOSE_PAREN
  | `LITERAL of [ `Bool of bool | `Int of int ]
  | `OPEN_PAREN
  | `OPERATOR of [ `Add | `And | `Div | `Gt | `Lt | `Mul | `Or | `Sub ]
  | `SEMICOLON
  | `IF
  | `THEN
  | `ELSE
  | `OPEN_BRACE
  | `CLOSE_BRACE
  | `WHILE
  | `DO
  | `VAR of string ]

(* Shunting-yard algorithm *)
let parse_expr (s:token Stream.t) =
  let expressions = Stack.create () in
  let operators = Stack.create () in
  let rec loop () =
    match Stream.peek s with
    | Some (`LITERAL lit) ->
      Stream.junk s;
      let expr =
        (match lit with
         | `Int n -> Expr (Int_expr, Int_lit n)
         | `Bool b -> Expr (Bool_expr, Bool_lit b))
      in
      Stack.push expr expressions;
      loop ()
    | Some (`VAR name) ->
      Stream.junk s;
      let expr = Expr (Int_expr, Var name) in
      Stack.push expr expressions;
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
    | Some (`ASSIGN | `SEMICOLON | `IF | `THEN | `ELSE
           | `OPEN_BRACE | `CLOSE_BRACE | `WHILE | `DO) | None -> ()
  in
  loop ();
  while not (Stack.is_empty operators) do
    match Stack.pop operators with
    | `OPERATOR op -> apply_op expressions op
    | `OPEN_PAREN -> failwith "Unclosed '('"
  done;
  let ex = Stack.pop expressions in
  ex

type statement =
  | Assign of string * int expr
  | If of bool expr * statement * statement
  | While of bool expr * statement
  | Seq of statement list

let rec eval_stmt vars statement =
  match statement with
  | Assign (name, expr) ->
    Hashtbl.replace vars name (eval vars expr)
  | If (cond, s1, s2) ->
    if (eval vars cond) then (eval_stmt vars s1) else (eval_stmt vars s2)
  | While (cond, stmt) ->
    while (eval vars cond) do
      eval_stmt vars stmt
    done
  | Seq stmts -> List.iter (eval_stmt vars) stmts

let parse_int_expr s : int expr =
  match parse_expr s with
  | Expr (Int_expr, expr) -> expr
  | Expr (Bool_expr, _) -> assert false

let parse_bool_expr s : bool expr =
  match parse_expr s with
  | Expr (Int_expr, _) -> assert false
  | Expr (Bool_expr, expr) -> expr

let skip_token s token =
  (match Stream.next s with
   | t when t = token -> ()
   | _ -> assert false)

let parse (s:token Stream.t) =
  let rec loop acc =
    match Stream.peek s with
    | Some (`VAR name) ->
      Stream.junk s;
      skip_token s `ASSIGN;
      let expr = parse_int_expr s in
      loop ((Assign (name, expr)) :: acc)
    | Some `SEMICOLON ->
      Stream.junk s;
      loop acc
    | Some `IF ->
      Stream.junk s;
      let expr = parse_bool_expr s in
      skip_token s `THEN;
      skip_token s `OPEN_BRACE;
      let statement1 = loop [] in
      skip_token s `CLOSE_BRACE;
      skip_token s `ELSE;
      skip_token s `OPEN_BRACE;
      let statement2 = loop [] in
      skip_token s `CLOSE_BRACE;
      loop ((If (expr, statement1, statement2)) :: acc)
    | Some `WHILE ->
      Stream.junk s;
      let expr = parse_bool_expr s in
      skip_token s `DO;
      skip_token s `OPEN_BRACE;
      let statement = loop [] in
      skip_token s `CLOSE_BRACE;
      loop ((While (expr, statement)) :: acc)
    | Some (`ASSIGN | `OPERATOR _ | `LITERAL _ | `OPEN_PAREN | `CLOSE_PAREN | `THEN | `ELSE
           | `OPEN_BRACE | `CLOSE_BRACE | `DO) | None ->
      Seq (List.rev acc)
  in
  loop []

let lex s =
  let expr_tokens = ["+";"-";"*";"/";"<";">";"and";"or";"(";")";"true";"false"] in
  let statement_tokens = [":=";";";"if";"then";"else";"{";"}";"while";"do"] in
  let lexer = make_lexer (expr_tokens @ statement_tokens) in
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
          | _, Kwd "+"     -> Some (`OPERATOR `Add)
          | _, Kwd "-"     -> Some (`OPERATOR `Sub)
          | _, Kwd "*"     -> Some (`OPERATOR `Mul)
          | _, Kwd "/"     -> Some (`OPERATOR `Div)
          | _, Kwd "<"     -> Some (`OPERATOR `Lt)
          | _, Kwd ">"     -> Some (`OPERATOR `Gt)
          | _, Kwd "and"   -> Some (`OPERATOR `And)
          | _, Kwd "or"    -> Some (`OPERATOR `Or)
          | _, Kwd "("     -> Some `OPEN_PAREN
          | _, Kwd ")"     -> Some `CLOSE_PAREN
          | _, Kwd "true"  -> Some (`LITERAL (`Bool true))
          | _, Kwd "false" -> Some (`LITERAL (`Bool false))
          | _, Kwd ":="    -> Some `ASSIGN
          | _, Kwd ";"     -> Some `SEMICOLON
          | _, Kwd "if"    -> Some `IF
          | _, Kwd "then"  -> Some `THEN
          | _, Kwd "else"  -> Some `ELSE
          | _, Kwd "{"     -> Some `OPEN_BRACE
          | _, Kwd "}"     -> Some `CLOSE_BRACE
          | _, Kwd "while" -> Some `WHILE
          | _, Kwd "do"    -> Some `DO
          | _, Ident name  -> Some (`VAR name)
          | None, Int n | Some _, Int n when n >= 0 -> Some (`LITERAL (`Int n))
          | Some prev_token, Int n -> (* replace -2 with - 2 if needed *)
            (match prev_token with
             | `OPERATOR _ | `OPEN_PAREN | `ASSIGN | `SEMICOLON | `IF | `THEN | `ELSE
             | `OPEN_BRACE | `CLOSE_BRACE | `WHILE | `DO -> Some (`LITERAL (`Int n))
             | `CLOSE_PAREN | `LITERAL _ | `VAR _ ->
               next_token := Some (`LITERAL (`Int (abs n)));
               Some (`OPERATOR `Sub))
          | _ -> failwith "Wrong token"
      in
      prev_token := token;
      token
    with
      Stream.Failure -> None
  in
  Stream.from next

let () =
  let s = Stream.of_channel stdin in
  lex s
  |> parse
  |> fun statement ->
  let vars = Hashtbl.create 10 in
  eval_stmt vars statement;
  let vars = Hashtbl.fold (fun k v acc -> (k, v) :: acc) vars [] in
  let vars =  List.sort (fun (k1, _) (k2, _) -> compare k1 k2) vars in
  List.iter (fun (k, v) -> Printf.printf "%s %d\n" k v) vars

