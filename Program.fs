type operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Exponent

type token =
  | Number of float
  | Operator of operator


module Lexer =

  let lex_number (s: string) =
    let is_digit = function
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' -> true
    | _ -> false in

    let literal = Seq.toList s |> List.unfold (
      function
      | [] -> None
      | hd :: tl ->
        if is_digit hd then Some (hd, tl) else None) |> System.String.Concat
    in

    match literal with
    | "" -> None, s
    | l -> Some (Number (float l)), s[l.Length..]


  let lex_operator (s: string) =
    let get_operator = function
    | '+' -> Some (Operator Add)
    | '-' -> Some (Operator Subtract)
    | '*' -> Some (Operator Multiply)
    | '/' -> Some (Operator Divide)
    | '%' -> Some (Operator Modulo)
    | '^' -> Some (Operator Exponent)
    | _ -> None in

    match Seq.toList s with
    | hd :: _ -> (
      match get_operator hd with
      | Some op -> Some op, s[1..]
      | _ -> None, s)
    | _ -> None, s

  let lex_token (s: string) =
    let lexers = [lex_number; lex_operator] in
    let found_lexer = lexers |> List.find (fun lexer ->
      match lexer s with
      | Some _, _ -> true
      | _ -> false) in
    found_lexer s

  let generate_ast (s: string) =
    let head = lex_token s in
    let first_token, _ = head in

    let optional_ast = head |> List.unfold (
      function
      | _, "" -> None
      | _, u ->
        let next_p, next_u = lex_token u in
        Some (next_p, (next_p, next_u))) in

    let tail = optional_ast |> List.filter (
      fun opt -> opt.IsSome) |> List.map (
        fun opt -> opt.Value) in

    [first_token.Value] @ tail


let token_to_string = function
  | Number n -> sprintf "Number(%f)" n
  | Operator Add -> "Add"
  | Operator Subtract -> "Subtract"
  | Operator Multiply -> "Multiply"
  | Operator Divide -> "Divide"
  | Operator Modulo -> "Modulo"
  | Operator Exponent -> "Exponent"

  // (fun acc (p, u) -> acc @ [lex_token u]) (lex_token s)

let print_tokens ast =
  List.iter (fun t -> printfn "%s" (token_to_string t)) ast

let pemdas = [Exponent; Multiply; Divide; Add; Subtract]

module Evaluater =
  let apply_operation a b = function
    | Add -> a + b
    | Subtract -> a - b
    | Multiply -> a * b
    | Divide -> a / b
    | Modulo -> a % b
    | Exponent -> a ** b


  let rec evaluate_operator current_op = function
    | Number a :: Operator op :: Number b :: tl ->
      if op = current_op then
        evaluate_operator current_op ([Number (apply_operation a b op)] @ tl)
      else
        [ Number a; Operator op; ] @ evaluate_operator current_op ([Number b] @ tl)
    | r -> r


  let evaluate_ast tokens =
    let rec aux lst ops =
      let use_ops =
        match ops with
        | [] -> pemdas
        | x -> x in
      // (List.map token_to_string lst) |> String.concat ", " |> printfn "%s"


      match evaluate_operator (List.head use_ops) lst with
      | [Number n] -> n
      | other -> aux other (List.tail use_ops)
    in aux tokens pemdas

let evaluate_expression expression = expression |> Lexer.generate_ast |> Evaluater.evaluate_ast

printfn "%f" (evaluate_expression "23+13.5*3/4")
printfn "%f" (evaluate_expression "8+4-2^4/10")