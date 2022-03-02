type operator =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo

type token =
  | Number of float32
  | Operator of operator

let example = "23+13*23"


let lex_number (s: string) =
  let not_digit = function
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> false
  | _ -> true in

  let found_end = List.tryFindIndex not_digit (s |> Seq.toList) in
  printfn "Lex Number: %s %b" s found_end.IsSome

  match found_end with
  | None ->
    printfn "None"
    None, s
  | Some(end_point) ->
    printfn "%d" end_point
    if end_point = 0 then
      None, s
    else
      let num_literal = s[0..end_point-1] in
      let num = Number (System.Single.Parse(num_literal)) in
      Some num, s[end_point..]


let lex_operator (s: string) =
  let first = (Seq.toList s)[0] in
  let other = s[1..] in

  printfn "Lex Operator: %s" s
  match first with
  | '+' -> Some (Operator Plus), other
  | '-' -> Some (Operator Minus), other
  | '*' -> Some (Operator Times), other
  | '/' -> Some (Operator Divide), other
  | '%' -> Some (Operator Modulo), other
  | _ -> None, s


let lex_token (s: string) =
  let found_num, rest_num = lex_number s in
  printfn "%b" found_num.IsSome

  if found_num.IsSome then
    (found_num, rest_num)
  else
    let found_op, rest_op = lex_operator s in
    (found_op, rest_op)

let lexer (s: string) =
  let rec aux acc ((p, u): (option<token> * string)) =
    match u with
    | "" -> acc
    | _ -> aux (if p.IsSome then acc @ [p] else acc) (lex_token u)
  in aux [] (lex_token s)


let ast = lexer example
