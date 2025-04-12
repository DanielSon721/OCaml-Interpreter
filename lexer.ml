open TokenTypes

let tokenize input =

  let mkcompile s = Re.compile (Re.Perl.re s) in
  let add_re = mkcompile "^\\+" in
  let sub_re = mkcompile "^-" in
  let mult_re = mkcompile "^\\*" in
  let div_re = mkcompile "^\\/" in
  let pow_re = mkcompile "^\\^" in
  let lp_re = mkcompile "^\\(" in
  let rp_re = mkcompile "^\\)" in
  let lb_re = mkcompile "^\\{" in
  let rb_re = mkcompile "^\\}" in
  let semi_re = mkcompile "^;" in
  let le_re = mkcompile "^<=" in
  let lt_re = mkcompile "^<" in
  let ge_re = mkcompile "^>=" in
  let gt_re = mkcompile "^>" in
  let eq_re = mkcompile "^==" in
  let ne_re = mkcompile "^!=" in
  let assign_re = mkcompile "^=" in
  let and_re = mkcompile "^&&" in
  let or_re = mkcompile "^\\|\\|" in
  let not_re = mkcompile "^!" in
  let true_re = mkcompile "^true" in
  let false_re = mkcompile "^false" in
  let int_type_re = mkcompile "^int" in
  let bool_type_re = mkcompile "^bool" in
  let print_re = mkcompile "^print" in
  let main_re = mkcompile "^main" in
  let if_re = mkcompile "^if" in
  let else_re = mkcompile "^else" in
  let while_re = mkcompile "^while" in
  let for_re = mkcompile "^for" in
  let from_re = mkcompile "^from" in
  let to_re = mkcompile "^to" in
  let num_re = mkcompile "^-?[0-9]+" in
  let id_re = mkcompile "^[a-zA-Z_][a-zA-Z0-9_]*" in
  let ws_re = mkcompile "^ " in
  let space_re= mkcompile "^(\\t|\\n|\\s)+" in

  let rec lexer program =
    if program = "" then [EOF]
    else if Re.execp add_re program then
      Tok_Add::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp sub_re program then
      Tok_Sub::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp mult_re program then
      Tok_Mult::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp div_re program then
      Tok_Div::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp pow_re program then
      Tok_Pow::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp lp_re program then
      Tok_LParen::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp rp_re program then
      Tok_RParen::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp lb_re program then
      Tok_LBrace::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp rb_re program then
      Tok_RBrace::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp semi_re program then
      Tok_Semi::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp ws_re program then
      (lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp le_re program then
      Tok_LessEqual::(lexer (String.sub program 2 ((String.length program) - 2)))
    else if Re.execp lt_re program then
      Tok_Less::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp ge_re program then
      Tok_GreaterEqual::(lexer (String.sub program 2 ((String.length program) - 2)))
    else if Re.execp gt_re program then
      Tok_Greater::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp eq_re program then
      Tok_Equal::(lexer (String.sub program 2 ((String.length program) - 2)))
    else if Re.execp ne_re program then
      Tok_NotEqual::(lexer (String.sub program 2 ((String.length program) - 2)))
    else if Re.execp assign_re program then
      Tok_Assign::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp and_re program then
      Tok_And::(lexer (String.sub program 2 ((String.length program) - 2)))
    else if Re.execp or_re program then
      Tok_Or::(lexer (String.sub program 2 ((String.length program) - 2)))
    else if Re.execp not_re program then
      Tok_Not::(lexer (String.sub program 1 ((String.length program) - 1)))
    else if Re.execp true_re program then
      Tok_Bool true::(lexer (String.sub program 4 ((String.length program) - 4)))
    else if Re.execp false_re program then
      Tok_Bool false::(lexer (String.sub program 5 ((String.length program) - 5)))
    else if Re.execp int_type_re program then
      Tok_Int_Type::(lexer (String.sub program 3 ((String.length program) - 3)))
    else if Re.execp bool_type_re program then
      Tok_Bool_Type::(lexer (String.sub program 4 ((String.length program) - 4)))
    else if Re.execp print_re program then
      Tok_Print::(lexer (String.sub program 5 ((String.length program) - 5)))
    else if Re.execp main_re program then
      Tok_Main::(lexer (String.sub program 4 ((String.length program) - 4)))
    else if Re.execp if_re program then
      Tok_If::(lexer (String.sub program 2 ((String.length program) - 2)))
    else if Re.execp else_re program then
      Tok_Else::(lexer (String.sub program 4 ((String.length program) - 4)))
    else if Re.execp while_re program then
      Tok_While::(lexer (String.sub program 5 ((String.length program) - 5)))
    else if Re.execp for_re program then
      Tok_For::(lexer (String.sub program 3 ((String.length program) - 3)))
    else if Re.execp from_re program then
      Tok_From::(lexer (String.sub program 4 ((String.length program) - 4)))
    else if Re.execp to_re program then
      Tok_To::(lexer (String.sub program 2 ((String.length program) - 2)))
    else if Re.execp num_re program then
      let num = Re.Group.get (Re.exec num_re program) 0 in
      let num_len = String.length num in
      Tok_Int(int_of_string num)::(lexer (String.sub program num_len ((String.length program) - num_len)))
    else if Re.execp id_re program then
      let id = Re.Group.get (Re.exec id_re program) 0 in
      let id_len = String.length id in
      Tok_ID id::(lexer (String.sub program id_len ((String.length program) - id_len)))
    else if Re.execp space_re program then
      (lexer (String.sub program 1 ((String.length program) - 1)))
    else
      raise (InvalidInputException ("Unexpected input: " ^ program))
  in lexer input