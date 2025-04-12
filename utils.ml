open SmallCTypes
open TokenTypes

(**********************************
 *     Lexer and Parse Utils      *
 **********************************)

let string_of_token (t : token) : string = match t with
  | Tok_For -> "Tok_For"
  | Tok_From -> "Tok_From"
  | Tok_To -> "Tok_To"
  | Tok_While -> "Tok_While"
  | Tok_Int_Type -> "Tok_Int_Type"
  | Tok_Bool_Type -> "Tok_Bool_Type"
  | Tok_Sub -> "Tok_Sub"
  | Tok_Semi -> "Tok_Semi"
  | Tok_RParen -> "Tok_RParen"
  | Tok_RBrace -> "Tok_RBrace"
  | Tok_Print -> "Tok_Print"
  | Tok_Pow -> "Tok_Pow"
  | Tok_Add -> "Tok_Add"
  | Tok_Or -> "Tok_Or"
  | Tok_NotEqual -> "Tok_NotEqual"
  | Tok_Not -> "Tok_Not"
  | Tok_Mult -> "Tok_Mult"
  | Tok_Main -> "Tok_Main"
  | Tok_LessEqual -> "Tok_LessEqual"
  | Tok_Less -> "Tok_Less"
  | Tok_LParen -> "Tok_LParen"
  | Tok_LBrace -> "Tok_LBrace"
  | Tok_Int(i) -> "Tok_Int(" ^ (string_of_int i) ^ ")"
  | Tok_If -> "Tok_If"
  | Tok_ID(id) -> "Tok_ID(\"" ^ id ^ "\")"
  | Tok_GreaterEqual -> "Tok_GreaterEqual"
  | Tok_Greater -> "Tok_Greater"
  | Tok_Equal -> "Tok_Equal"
  | Tok_Else -> "Tok_Else"
  | Tok_Div -> "Tok_Div"
  | Tok_Bool(b) -> "Tok_Bool(" ^ (string_of_bool b) ^ ")"
  | Tok_Assign -> "Tok_Assign"
  | Tok_And -> "Tok_And"
  | EOF -> "EOF"

let rec string_of_data_type (t : data_type) : string = match t with
  | Int_Type -> "Int_Type"
  | Bool_Type -> "Bool_Type"

let rec string_of_expr (e : expr) : string =
  let unparse_two (s : string) (e1 : expr) (e2 : expr) =
    s ^ "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  in

  match e with
  | ID(s) -> "ID \"" ^ s ^ "\""
  | Int(n) -> "Int " ^ string_of_int n
  | Bool(b) -> "Bool " ^ string_of_bool b

  | Add(e1, e2) -> unparse_two "Add" e1 e2
  | Sub(e1, e2) -> unparse_two "Sub" e1 e2
  | Mult(e1, e2) -> unparse_two "Mult" e1 e2
  | Div(e1, e2) -> unparse_two "Div" e1 e2
  | Pow(e1, e2) -> unparse_two "Pow" e1 e2

  | Equal(e1, e2) -> unparse_two "Equal" e1 e2
  | NotEqual(e1, e2) -> unparse_two "NotEqual" e1 e2

  | Greater(e1, e2) -> unparse_two "Greater" e1 e2
  | Less(e1, e2) -> unparse_two "Less" e1 e2
  | GreaterEqual(e1, e2) -> unparse_two "GreaterEqual" e1 e2
  | LessEqual(e1, e2) -> unparse_two "LessEqual" e1 e2

  | Or(e1, e2) -> unparse_two "Or" e1 e2
  | And(e1, e2) -> unparse_two "And" e1 e2
  | Not(e) -> "Not(" ^ string_of_expr e ^ ")"

let rec string_of_stmt (s : stmt) : string = match s with
  | NoOp -> "NoOp"
  | Seq(s1, s2) -> "Seq(" ^ string_of_stmt s1 ^ ", " ^ string_of_stmt s2 ^ ")"
  | Declare(t, id) -> "Declare(" ^ string_of_data_type t ^ ", " ^ id ^ ")"
  | Assign(id, e) -> "Assign(" ^ id ^ ", " ^ string_of_expr e ^ ")"
  | If(guard, if_body, else_body) ->
    "If(" ^ string_of_expr guard ^ ", " ^ string_of_stmt if_body ^ ", " ^ string_of_stmt else_body ^ ")"
  | While(guard, body) -> "While(" ^ string_of_expr guard ^ ", " ^ string_of_stmt body ^ ")"
  | For(id, start, last, body) ->
     "For(" ^ id ^ "," ^ (string_of_expr start) ^ "," ^ (string_of_expr last) ^ "," ^ string_of_stmt body ^ ")"
  | Print(e) -> "Print(" ^ string_of_expr e ^ ")"

let string_of_list ?newline:(newline=false) (f : 'a -> string) (l : 'a list) : string =
  "[" ^ (String.concat ", " @@ List.map f l) ^ "]" ^ (if newline then "\n" else "");;

(**********************************
 * BEGIN ACTUAL PARSE HELPER CODE *
 **********************************)

let string_of_in_channel (ic : in_channel) : string =
  let lines : string list =
    let try_read () =
      try Some ((input_line ic) ^ "\n") with End_of_file -> None in
    let rec loop acc = match try_read () with
      | Some s -> loop (s :: acc)
      | None -> List.rev acc in
    loop []
  in

  List.fold_left (fun a e -> a ^ e) "" @@ lines

let tokenize_from_channel (c : in_channel) : token list =
  Lexer.tokenize @@ string_of_in_channel c

let tokenize_from_file (filename : string) : token list =
  let c = open_in filename in
  let s = tokenize_from_channel c in
  close_in c;
  s


(**********************************
 *        Evaluator Utils         *
 **********************************)

(* Buffers for testing prints *)
let (print_buffer : Buffer.t) = Buffer.create 100

let flush_print_buffer () : unit = Buffer.clear print_buffer;;
let assert_buffer_equal (s : string) : unit =
  let c = Buffer.contents print_buffer in
  if not (s = c) then failwith ("Printed: '" ^ c ^ "' Expected:'" ^ s ^ "'");;

(* This is the print you must use for the project *)
let print_output_string (s : string) : unit =
  Buffer.add_string print_buffer s;
  Stdlib.print_string s

let print_output_int (i : int) : unit =
  print_output_string (string_of_int i)

let print_output_bool (b : bool) : unit =
  print_output_string (string_of_bool b)

let print_output_newline () : unit =
  print_output_string "\n"

let read_from_file (input_filename : string) : string =
  let read_lines name : string list =
    let ic = open_in name in
    let try_read () =
      try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
      | Some s -> loop (s :: acc)
      | None -> close_in ic; List.rev acc in
    loop []
  in
  (* Read the file into lines *)
  let prog_lines = read_lines input_filename in
  (* Compress to a single string *)
  List.fold_left (fun a e -> a ^ e) "" prog_lines

(* Unparser *)

let rec unparse_data_type (t : data_type) : string = match t with
  | Int_Type -> "Int_Type"
  | Bool_Type -> "Bool_Type"

let rec unparse_expr (e : expr) : string =
  let unparse_two (s : string) (e1 : expr) (e2 : expr) =
    s ^ "(" ^ unparse_expr e1 ^ ", " ^ unparse_expr e2 ^ ")"
  in

  match e with
  | ID(s) -> "ID \"" ^ s ^ "\""
  | Int(n) -> "Int " ^ string_of_int n
  | Bool(b) -> "Bool " ^ string_of_bool b

  | Add(e1, e2) -> unparse_two "Plus" e1 e2
  | Sub(e1, e2) -> unparse_two "Sub" e1 e2
  | Mult(e1, e2) -> unparse_two "Mult" e1 e2
  | Div(e1, e2) -> unparse_two "Div" e1 e2
  | Pow(e1, e2) -> unparse_two "Pow" e1 e2

  | Equal(e1, e2) -> unparse_two "Equal" e1 e2
  | NotEqual(e1, e2) -> unparse_two "NotEqual" e1 e2

  | Greater(e1, e2) -> unparse_two "Greater" e1 e2
  | Less(e1, e2) -> unparse_two "Less" e1 e2
  | GreaterEqual(e1, e2) -> unparse_two "GreaterEqual" e1 e2
  | LessEqual(e1, e2) -> unparse_two "LessEqual" e1 e2

  | Or(e1, e2) -> unparse_two "Or" e1 e2
  | And(e1, e2) -> unparse_two "And" e1 e2
  | Not(e) -> "Not(" ^ unparse_expr e ^ ")"

let rec unparse_stmt (s : stmt) : string = match s with
  | NoOp -> "NoOp"
  | Seq(s1, s2) -> "Seq(" ^ unparse_stmt s1 ^ ", " ^ unparse_stmt s2 ^ ")"
  | Declare(t, id) -> "Declare(" ^ unparse_data_type t ^ ", " ^ id ^ ")"
  | Assign(id, e) -> "Assign(" ^ id ^ ", " ^ unparse_expr e ^ ")"
  | If(guard, if_body, else_body) ->
    "If(" ^ unparse_expr guard ^ ", " ^ unparse_stmt if_body ^ ", " ^ unparse_stmt else_body ^ ")"
  | While(guard, body) -> "While(" ^ unparse_expr guard ^ ", " ^ unparse_stmt body ^ ")"
  | For(id, start_expr, end_expr, body) -> "For(" ^ id ^ " from " ^ unparse_expr start_expr ^ " to " ^ unparse_expr end_expr ^ "){" ^ unparse_stmt body ^ "}"
  | Print(e) -> "Print(" ^ unparse_expr e ^ ")"

(* Removes shadowed bindings from an execution environment *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Eval report print function *)
let print_eval_env_report (env : environment): unit =

  print_string "*** BEGIN POST-EXECUTION ENVIRONMENT REPORT ***\n";

  List.iter (fun (var, value) ->
      let vs = begin match value with
        | Int_Val(i) -> string_of_int i
        | Bool_Val(b) -> string_of_bool b
      end in

      Printf.printf "- %s => %s\n" var vs) (prune_env env);

  print_string "***  END POST-EXECUTION ENVIRONMENT REPORT  ***\n"