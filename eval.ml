open SmallCTypes
open Utils
open TokenTypes

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env t =
  match t with
  | Int i -> Int_Val i
  | Bool b -> Bool_Val b
  | ID id -> if List.assoc_opt id env = None then raise (DeclareError "Eval_Id error") else List.assoc id env
  | Add (x,y) -> let x' = eval_expr env x in
                 let y' = eval_expr env y in
                 (match x', y' with
                  | Int_Val x', Int_Val y' -> Int_Val (x' + y')
                  | _ -> raise (TypeError "Error"))
  | Sub (x,y) -> let x' = eval_expr env x in
                 let y' = eval_expr env y in
                 (match x', y' with
                  | Int_Val x', Int_Val y' -> Int_Val (x' - y')
                  | _ -> raise (TypeError "Error"))
  | Mult (x,y) -> let x' = eval_expr env x in
                  let y' = eval_expr env y in
                  (match x', y' with
                  | Int_Val x', Int_Val y' -> Int_Val (x' * y')
                  | _ -> raise (TypeError "Error"))
  | Div (x,y) -> let x' = eval_expr env x in
                 let y' = eval_expr env y in
                  (match x', y' with
                  | Int_Val x', Int_Val 0 -> raise DivByZeroError
                  | Int_Val x', Int_Val y' -> Int_Val (x' / y')
                  | _ -> raise (TypeError "Error"))
  | Pow (x,y) -> let x' = eval_expr env x in
                 let y' = eval_expr env y in
                 (match x', y' with
                  | Int_Val x', Int_Val y' -> Int_Val (int_of_float ((float_of_int x') ** (float_of_int y')))
                  | _ -> raise (TypeError "Error in eval_expr Pow"))
  | Or (expr1, expr2) -> let expr1' = eval_expr env expr1 in
                         let expr2' = eval_expr env expr2 in
                         (match expr1', expr2' with
                          | Bool_Val expr1', Bool_Val expr2' -> Bool_Val (expr1' || expr2')
                          | _ -> raise (TypeError "Error in eval_expr Or"))
  | And (expr1, expr2) -> let expr1' = eval_expr env expr1 in
                          let expr2' = eval_expr env expr2 in
                          (match expr1', expr2' with
                           | Bool_Val expr1', Bool_Val expr2' -> Bool_Val (expr1' && expr2')
                           | _ -> raise (TypeError "Error in eval_expr And"))
  | Not (expr) -> let expr' = eval_expr env expr in
                  (match expr' with
                   | Bool_Val expr' -> Bool_Val (not expr')
                   | _ -> raise (TypeError "Error in eval_expr Not"))
  | Greater (expr1, expr2) -> let expr1' = eval_expr env expr1 in
                              let expr2' = eval_expr env expr2 in
                              (match expr1', expr2' with
                               | Int_Val expr1', Int_Val expr2' -> Bool_Val (expr1' > expr2')
                               | _ -> raise (TypeError "Error in eval_expr Greater"))
  | Less (expr1, expr2) -> let expr1' = eval_expr env expr1 in
                           let expr2' = eval_expr env expr2 in
                           (match expr1', expr2' with
                            | Int_Val expr1', Int_Val expr2' -> Bool_Val (expr1' < expr2')
                            | _ -> raise (TypeError "Error in eval_expr Less"))
  | GreaterEqual (expr1, expr2) -> let expr1' = eval_expr env expr1 in
                                   let expr2' = eval_expr env expr2 in
                                   (match expr1', expr2' with
                                    | Int_Val expr1', Int_Val expr2' -> Bool_Val (expr1' >= expr2')
                                    | _ -> raise (TypeError "Error in eval_expr GreaterEqual"))
  | LessEqual (expr1, expr2) -> let expr1' = eval_expr env expr1 in
                                let expr2' = eval_expr env expr2 in
                                (match expr1', expr2' with
                                 | Int_Val expr1', Int_Val expr2' -> Bool_Val (expr1' <= expr2')
                                 | _ -> raise (TypeError "Error in eval_expr LessEqual"))
  | Equal (expr1, expr2) -> let expr1' = eval_expr env expr1 in
                            let expr2' = eval_expr env expr2 in
                            (match expr1', expr2' with
                             | Int_Val expr1', Int_Val expr2' -> Bool_Val (expr1' = expr2')
                             | Bool_Val expr1', Bool_Val expr2' -> Bool_Val (expr1' = expr2')
                             | _ -> raise (TypeError "Error in eval_expr Equal"))
  | NotEqual (expr1, expr2) -> let expr1' = eval_expr env expr1 in
                               let expr2' = eval_expr env expr2 in
                               (match expr1', expr2' with
                                | Int_Val expr1', Int_Val expr2' -> Bool_Val (expr1' <> expr2')
                                | Bool_Val expr1', Bool_Val expr2' -> Bool_Val (expr1' <> expr2')
                                | _ -> raise (TypeError "Error in eval_expr NotEqual"))

let rec eval_stmt env s =
  (match s with
  | NoOp -> env
  | Seq (stmt1, stmt2) -> eval_stmt (eval_stmt env stmt1) stmt2
  | Declare (data_type, string) ->  if List.assoc_opt string env = None then (match data_type with 
                                                                             | Bool_Type -> let value = Bool_Val (false)
                                                                                            in (string, value) :: env
                                                                             | Int_Type -> let value = Int_Val (0)
                                                                                           in (string, value) :: env)
                                    else raise (DeclareError "Error in eval_stmt Declare")
  | Assign (string, expr) ->  if List.assoc_opt string env = None then raise (DeclareError "Error") 
                              else let value = eval_expr env expr in
                                   let data_type1 = (match value with
                                                     | Int_Val (x) -> Int_Type
                                                     | Bool_Val (x) -> Bool_Type)
                                   in
                                   let existing_value = List.assoc string env in
                                   let data_type2 = (match existing_value with
                                                     | Int_Val (x) -> Int_Type
                                                     | Bool_Val (x) -> Bool_Type)
                                   in
                                   if data_type1 <> data_type2 then raise (TypeError "Error in eval_stmt Assign")
                                   else let newList = List.remove_assoc string env in (string, value) :: newList
  | If (expr, stmt1, stmt2) -> (match eval_expr env expr with
                                | Bool_Val true -> eval_stmt env stmt1
                                | Bool_Val false -> eval_stmt env stmt2
                                | _ -> raise (TypeError "Error in eval_stmt If"))
  | While (expr, stmt) -> (match eval_expr env expr with
                           | Bool_Val true -> eval_stmt (eval_stmt env stmt) (While (expr, stmt))
                           | Bool_Val false -> env
                           | _ -> raise (TypeError "Error in eval_stmt While"))
  | For (string, expr1, expr2, stmt) -> let value1 = eval_expr env expr1 in
                                        let value2 = eval_expr env expr2 in
                                        (match value1, value2 with
                                         | Int_Val (num1), Int_Val (num2) -> let rec loop i env = if i > num2 then env
                                                                                                  else let env' = eval_stmt ((string, Int_Val i) :: List.remove_assoc string env) stmt in
                                                                                                       loop (i + 1) env'
                                                                             in loop num1 env
                                         | _ -> raise (TypeError "Error in eval_stmt For"))
  | Print (expr) -> (match eval_expr env expr with
                     | Int_Val i -> let _ = print_output_int i in
                                    let _ = print_output_newline () in
                                    env
                     | Bool_Val b -> let _ = print_output_bool b in
                                     let _ = print_output_newline () in
                                     env)
)