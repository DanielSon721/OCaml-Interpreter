open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  parse_Or toks

and parse_Or toks =
  let t1, e1 = parse_And toks in
  (match lookahead t1 with
  | Tok_Or ->
      let t2 = match_token t1 Tok_Or in
      let t3, e2 = parse_Or t2 in
      (t3, Or (e1, e2))
  | _ -> (t1, e1))

and parse_And toks =
  let t1, e1 = parse_Equality toks in
  (match lookahead t1 with
  | Tok_And ->
      let t2 = match_token t1 Tok_And in
      let t3, e2 = parse_And t2 in
      (t3, And (e1, e2))
  | _ -> (t1, e1))

and parse_Equality toks =
  let t1, e1 = parse_Relational toks in
  (match lookahead t1 with
  | Tok_Equal ->
      let t2 = match_token t1 Tok_Equal in
      let t3, e2 = parse_Equality t2 in
      (t3, Equal (e1, e2))
  | Tok_NotEqual ->
      let t2 = match_token t1 Tok_NotEqual in
      let t3, e2 = parse_Equality t2 in
      (t3, NotEqual (e1, e2))
  | _ -> (t1, e1))

and parse_Relational toks =
  let t1, e1 = parse_Additive toks in
  (match lookahead t1 with
  | Tok_Greater ->
      let t2 = match_token t1 Tok_Greater in
      let t3, e2 = parse_Relational t2 in
      (t3, Greater (e1, e2))
  | Tok_Less ->
      let t2 = match_token t1 Tok_Less in
      let t3, e2 = parse_Relational t2 in
      (t3, Less (e1, e2))
  | Tok_GreaterEqual ->
      let t2 = match_token t1 Tok_GreaterEqual in
      let t3, e2 = parse_Relational t2 in
      (t3, GreaterEqual (e1, e2))
  | Tok_LessEqual ->
      let t2 = match_token t1 Tok_LessEqual in
      let t3, e2 = parse_Relational t2 in
      (t3, LessEqual (e1, e2))
  | _ -> (t1, e1))

and parse_Additive toks =
  let t1, e1 = parse_Multiplicative toks in
  (match lookahead t1 with
  | Tok_Add ->
      let t2 = match_token t1 Tok_Add in
      let t3, e2 = parse_Additive t2 in
      (t3, Add (e1, e2))
  | Tok_Sub ->
      let t2 = match_token t1 Tok_Sub in
      let t3, e2 = parse_Additive t2 in
      (t3, Sub (e1, e2))
  | _ -> (t1, e1))

and parse_Multiplicative toks =
  let t1, e1 = parse_Power toks in
  (match lookahead t1 with
  | Tok_Mult ->
      let t2 = match_token t1 Tok_Mult in
      let t3, e2 = parse_Multiplicative t2 in
      (t3, Mult (e1, e2))
  | Tok_Div ->
      let t2 = match_token t1 Tok_Div in
      let t3, e2 = parse_Multiplicative t2 in
      (t3, Div (e1, e2))
  | _ -> (t1, e1))

and parse_Power toks =
  let t1, e1 = parse_Unary toks in
  (match lookahead t1 with
  | Tok_Pow ->
      let t2 = match_token t1 Tok_Pow in
      let t3, e2 = parse_Power t2 in
      (t3, Pow (e1, e2))
  | _ -> (t1, e1))

and parse_Unary toks =
  (match lookahead toks with
  | Tok_Not ->
      let t1 = match_token toks Tok_Not in
      let t2, e2 = parse_Unary t1 in
      (t2, Not e2)
  | _ -> parse_Primary toks)

and parse_Primary toks =
  (match lookahead toks with
  | Tok_Int i -> let t = match_token toks (Tok_Int i) in
                    (t, Int i)
  | Tok_Bool b -> let t = match_token toks (Tok_Bool b) in
                  (t, Bool b)
  | Tok_ID id -> let t = match_token toks (Tok_ID id) in
                (t, ID id)
  | Tok_LParen -> let t_left = match_token toks Tok_LParen in
                  let t, e = parse_expr t_left in
                  let t_right = match_token t Tok_RParen in
                  (t_right, e)
  | _ -> raise (InvalidInputException "Unexpected token"))

let rec parse_stmt toks : stmt_result =
  (match lookahead toks with
  | Tok_Int_Type -> let t1 = match_token toks Tok_Int_Type in
                 (match lookahead t1 with
                  | Tok_ID id -> let t2 = match_token t1 (Tok_ID id) in
                                let t3 = match_token t2 Tok_Semi in
                                let t4, next = parse_stmt t3 in
                                (t4, Seq (Declare (Int_Type, id), next))
                  | _ -> failwith "Error")
  | Tok_Bool_Type -> let t1 = match_token toks Tok_Bool_Type in
                 (match lookahead t1 with
                  | Tok_ID id -> let t2 = match_token t1 (Tok_ID id) in
                                let t3 = match_token t2 Tok_Semi in
                                let t4, next = parse_stmt t3 in
                                (t4, Seq (Declare (Bool_Type, id), next))
                  | _ -> failwith "Error")
  | Tok_ID id -> let t1 = match_token toks (Tok_ID id) in
                 let t2 = match_token t1 Tok_Assign in
                 let t3, expr = parse_expr t2 in
                 let t4 = match_token t3 Tok_Semi in
                 let t5, next = parse_stmt t4 in
                 (t5, Seq (Assign (id, expr), next))
  | Tok_Print -> let t1 = match_token toks Tok_Print in
                 (match lookahead t1 with 
                 | Tok_ID "f" -> let t2 = match_token t1 (Tok_ID "f") in
                                 let t3 = match_token t2 Tok_LParen in
                                 let t4, expr = parse_expr t3 in
                                 let t5 = match_token t4 Tok_RParen in
                                 let t6 = match_token t5 Tok_Semi in
                                 let t7, next = parse_stmt t6 in
                                 (t7, Seq (Print (expr), next))
                 | _ -> failwith "Error")
  | Tok_If -> let t1 = match_token toks Tok_If in
              let t2 = match_token t1 Tok_LParen in
              let t3, expr = parse_expr t2 in
              let t4 = match_token t3 Tok_RParen in
              (* ^if expression^ *)
              let t5 = match_token t4 Tok_LBrace in
              let t6, stmt1 = parse_stmt t5 in
              let t7 = match_token t6 Tok_RBrace in
              (* ^if statement^ *)
              (match lookahead t7 with
              | Tok_Else -> let t8 = match_token t7 Tok_Else in
                            let t9 = match_token t8 Tok_LBrace in
                            let t10, stmt2 = parse_stmt t9 in
                            let t11 = match_token t10 Tok_RBrace in
                            (* ^else statement^ *)
                            let t12, next = parse_stmt t11 in
                            (t12, Seq (If (expr, stmt1, stmt2), next))
              | _ -> let t8, next = parse_stmt t7 in
                     (t8, Seq (If (expr, stmt1, NoOp), next)))
  | Tok_For -> let t1 = match_token toks Tok_For in
               let t2 = match_token t1 Tok_LParen in
               (match lookahead t2 with
               | Tok_ID id -> let t3 = match_token t2 (Tok_ID id) in
                                let t4 = match_token t3 Tok_From in
                                let t5, expr1 = parse_expr t4 in
                                let t6 = match_token t5 Tok_To in
                                let t7, expr2 = parse_expr t6 in
                                let t8 = match_token t7 Tok_RParen in
                                (* ^for expression^ *)
                                let t9 = match_token t8 Tok_LBrace in
                                let t10, stmt = parse_stmt t9 in
                                let t11 = match_token t10 Tok_RBrace in
                                (* ^for statement^ *)
                                let t12, next = parse_stmt t11 in
                                (t12, Seq (For (id, expr1, expr2, stmt), next))
                | _ -> raise (InvalidInputException "Error"))
  | Tok_While -> let t1 = match_token toks Tok_While in
                 let t2 = match_token t1 Tok_LParen in
                 let t3, expr = parse_expr t2 in
                 let t4 = match_token t3 Tok_RParen in
                 (* ^while expression^ *)
                 let t5 = match_token t4 Tok_LBrace in
                 let t6, stmt = parse_stmt t5 in
                 let t7 = match_token t6 Tok_RBrace in
                 (* ^while statement^ *)
                 let t8, next = parse_stmt t7 in
                 (t8, Seq (While (expr, stmt), next))
  | _ -> (toks, NoOp))


let parse_main toks : stmt =
  (match lookahead toks with
  | Tok_Int_Type -> let t1 = match_token toks Tok_Int_Type in
                    let t2 = match_token t1 Tok_Main in
                    let t3 = match_token t2 Tok_LParen in
                    let t4 = match_token t3 Tok_RParen in
                    let t5 = match_token t4 Tok_LBrace in
                    let t6, stmt = parse_stmt t5 in
                    let t7 = match_token t6 Tok_RBrace in
                    (match lookahead t7 with
                    | EOF -> stmt
                    | _ -> raise (InvalidInputException "Error"))
  | _ -> raise (InvalidInputException "Invalid syntax, parse_main"))
