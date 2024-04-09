//
// Parser for SimpleC programs.  This component checks 
// the input program to see if it meets the syntax rules
// of SimpleC.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid SimpleC program.
//
// <<YOUR NAME>>
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

   //
  // beginswith
  //
  let beginswith (pattern: string) (literal: string) =
      literal.StartsWith (pattern)
  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //

    let next_token = List.head tokens

    if expected_token = next_token || expected_token = "identifier:"  || expected_token = "str_literal"  || expected_token = "int_literal"  then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)


  //
  // stmt
  //



  //
  // stmts
  //
  let rec private stmts tokens = 
    let T1 = stmt tokens
    let T2 = morestmts T1
    T2
  and private stmt tokens = 
    match tokens with
    | head :: _ when head = ";" -> matchToken ";" tokens
    | head :: _ when head = "int" -> vardecl tokens
    | head :: _ when head = "cin" -> input tokens
    | head :: _ when head = "cout" -> output tokens
    | head :: _ when head = "=" -> assignment tokens
    | head :: _ when head = "if" -> ifstmt

  and private vardecl tokens = 
    let T1 = matchToken "int" tokens
    let T2 = matchToken "identifier" T1
    let T3 = matchToken ";" T2
    T3

  and private input tokens = 
    let T1 = matchToken "cin" tokens
    let T2 = matchToken ">>" T1
    let T3 = matchToken "identifier" T2
    let T4 = matchToken ";" T3
    T4

  and private output tokens = 
    let T1 = matchToken "cout" tokens
    let T2 = matchToken "<<" T1
    let T3 = output_value T2
    let T4 = matchToken ";" T3
    T4

  and private output_value tokens = 
    match tokens with
    | head :: _ when head = "endl" -> matchToken "endl" tokens
    | _ -> expr_value tokens

  and private expr_value tokens = 
    match tokens with
    | head :: _ when beginswith "identifier:" head -> matchToken "identifier" tokens
    | head :: _ when beginswith "int_literal:" head -> matchToken "int_literal" tokens
    | head :: _ when beginswith "str_literal:" head -> matchToken "str_literal" tokens
    | head :: _ when head = "true" -> matchToken "true" tokens
    | head :: _ when head = "false" -> matchToken "false" tokens
    | _ -> failwith ("expecting identifier or literal, but found " + List.head tokens)

  and private assignment tokens = 
    let T1 = matchToken "identifier" tokens
    let T2 = matchToken "=" T1
    let T3 = expr T2
    let T4 = matchToken ";" T3
    T4

  and private expr tokens =
    match tokens with
    | head :: _ -> 
                  let T1 = expr_value tokens in
                  let T2 = expr_op T1 in
                  let T3 = expr_value T2 
                  T3
    | _ -> expr_value tokens

  and private expr_op tokens = 
    match tokens with
    | head :: _ when head = "+" -> matchToken "+" tokens
    | head :: _ when head = "-" -> matchToken "-" tokens
    | head :: _ when head = "*" -> matchToken "*" tokens
    | head :: _ when head = "/" -> matchToken "/" tokens
    | head :: _ when head = "^" -> matchToken "^" tokens
    | head :: _ when head = "<" -> matchToken "<" tokens
    | head :: _ when head = "<=" -> matchToken "<=" tokens
    | head :: _ when head = ">" -> matchToken ">" tokens
    | head :: _ when head = ">=" -> matchToken ">=" tokens
    | head :: _ when head = "==" -> matchToken "==" tokens
    | head :: _ when head = "!=" -> matchToken "!=" tokens




  //
  // simpleC
  //
  let private simpleC tokens = 
    // 
    // TODO: Start here by filling it in and
    //       creating your other functions!
    //
    
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8
    T9


  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid SimpleC program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "Success!"
    with 
      | ex -> "syntax_error: " + ex.Message
