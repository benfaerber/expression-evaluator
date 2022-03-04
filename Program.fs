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
  | RawGroup of string
  | Group of token list

let rec string_of_token = function
  | Number n -> sprintf "Number (%f)" n
  | RawGroup rg -> sprintf "RawGroup (%s)" rg
  | Group g -> sprintf "Group (%s)" (List.map string_of_token g |> String.concat ", ")
  | Operator Add -> "Add"
  | Operator Subtract -> "Subtract"
  | Operator Multiply -> "Multiply"
  | Operator Divide -> "Divide"
  | Operator Modulo -> "Modulo"
  | Operator Exponent -> "Exponent"

let should_debug_print = true
let debug_print value =
  if should_debug_print then
    printfn "%s" value

let print_token token = token |> string_of_token |> debug_print

let print_ast ast =
  List.iter print_token ast
  debug_print ""

let does_sleep = true
let sleep () = if does_sleep then Async.Sleep(500) |> Async.RunSynchronously

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

  let lex_group (s: string) =
    if (s |> Seq.toList |> List.head) = '(' then
      let folder (l_open, l_close, acc) chr =
        let tally goal counter = counter + (if goal = chr then 1 else 0) in
        let c_open = tally '(' l_open in
        let c_close = tally ')' l_close in
        let next_acc = if c_open <> 0 && c_open = c_close then acc else acc ^ chr.ToString() in

        c_open, c_close, next_acc
      in

      let _, _, literal = List.fold folder (0, 0, "") (Seq.toList s) in
      debug_print (sprintf "Group Literal: %s" literal[1..])
      Some (RawGroup literal[1..]), s[literal.Length+1..]
    else
      None, s

  let lex_token (s: string) =
    let lexers = [lex_number; lex_group; lex_operator] in
    let found_lexer = lexers |> List.find (fun lexer ->
      match lexer s with
      | Some _, _ -> true
      | _ -> false) in
    found_lexer s

  let clean_ast ast =
    List.filter (function
    | RawGroup _ -> false
    | _ -> true) ast

  let rec generate_ast (s: string) =
    let head = lex_token s in
    let first_token, _ = head in

    let optional_ast = head |> List.unfold (
      function
      | _, "" -> None
      | Some (RawGroup g), u ->
        let sub_group = Some (Group (generate_ast g)) in
        print_ast (generate_ast g)
        Some (sub_group, (sub_group, u))
      | _, u ->
        let next_p, next_u = lex_token u in
        match next_p with
        | Some (RawGroup rg) ->
          let sub_ast = Some (Group (generate_ast rg)) in
          Some (sub_ast, (sub_ast, next_u))
        | _ -> Some (next_p, (next_p, next_u))) in

    let tail = optional_ast |> List.filter (
      fun opt -> opt.IsSome) |> List.map (
        fun opt -> opt.Value) in

    clean_ast ([first_token.Value] @ tail)


module Evaluater =
  // Parens are evauluated seperatetly from this list
  let pemdas = [ Exponent; Multiply; Divide; Add; Subtract; ]

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
        debug_print (sprintf "Evauluating %s Operations" (Operator op |> string_of_token))
        evaluate_operator current_op ([Number (apply_operation a b op)] @ tl)
      else
        debug_print (sprintf "Looking for %s Operations" (Operator current_op |> string_of_token))
        [ Number a; Operator op; ] @ evaluate_operator current_op ([Number b] @ tl)
    | r -> r


  let rec evaluate_ast tokens =
    let rec aux ast ops =
      let use_ops =
        match ops with
        | [] -> pemdas
        | x -> x in
      // (List.map token_to_string lst) |> String.concat ", " |> printfn "%s"

      sleep ()
      match ast with
      | Group g :: tl ->
        aux (aux g pemdas @ tl) use_ops
      | Number n :: Operator op :: Group g :: tl ->
        let res = aux ([Number n; Operator op;] @ aux g pemdas) pemdas in
        aux (res @ tl) pemdas
      | ts -> (
        match evaluate_operator (List.head use_ops) ts with
        | [Number n] -> [Number n]
        | other ->
          print_ast other
          aux other (List.tail use_ops))
    in

    match aux tokens pemdas with
    | [Number n] -> n
    | _ -> 0

let evaluate_expression expression =
  let ast = expression |> Lexer.generate_ast in
  debug_print "Complete AST: "
  print_ast ast
  debug_print ""

  Evaluater.evaluate_ast ast

let complex_expression_1 = "(23+(135-32)+2)*3/4"
let complex_expression_2 = "73/3+(4^2+(8-3))"


print_ast (Lexer.generate_ast complex_expression_2)
// printfn "%f" (evaluate_expression "23+13.5*3/4")
// printfn "%f" (evaluate_expression "8+4-2^4/10")
// printfn "%f" (evaluate_expression complex_expression_1)
printfn "%f" (evaluate_expression complex_expression_2)