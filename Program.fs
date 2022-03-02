type operator =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Exponent

type token =
  | Number of float
  | Operator of operator


let example = "23+13.5*3/4"

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
  | '+' -> Some (Operator Plus)
  | '-' -> Some (Operator Minus)
  | '*' -> Some (Operator Times)
  | '/' -> Some (Operator Divide)
  | '%' -> Some (Operator Modulo)
  | '^' -> Some (Operator Exponent)
  | _ -> None in

  match Seq.toList s with
  | hd :: _ -> if (get_operator hd).IsSome then (get_operator hd, s[1..]) else None, s
  | _ -> None, s

let token_to_string = function
  | Number n -> sprintf "Number(%f)" n
  | Operator Plus -> "Plus"
  | Operator Minus -> "Minus"
  | Operator Times -> "Times"
  | Operator Divide -> "Divide"
  | Operator Modulo -> "Module"
  | Operator Exponent -> "Exponent"

let lex_token (s: string) =
  let lexers = [lex_number; lex_operator] in
  let found_lexer = lexers |> List.find (fun lexer ->
    match lexer s with
    | Some _, _ -> true
    | _ -> false) in
  found_lexer s

let lex (s: string) =
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


  // (fun acc (p, u) -> acc @ [lex_token u]) (lex_token s)

let tokens = (lex example)
let print_tokens ast =
  List.iter (fun t -> printfn "%s" (token_to_string t)) ast

let pemdas = [Exponent; Times; Divide; Plus; Minus]

// type operator =
//   | Plus
//   | Minus
//   | Times
//   | Divide
//   | Modulo
//   | Exponent


let apply_operation a b = function
  | Plus -> a + b
  | Minus -> a - b
  | Times -> a * b
  | Divide -> a / b
  | Modulo -> a + b
  | Exponent -> a + b


let rec evaluate = function
  | Number a :: Operator op :: Number b :: tl ->
    evaluate ([Number (apply_operation a b op)] @ tl)
  | r -> r

print_tokens (evaluate tokens)