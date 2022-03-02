type operator =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo

type token =
  | Number of float
  | Operator of operator

let example = "23+13*23"

let lex_number (s: string) =
  let is_digit = function
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
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

let p, u = (lex_number example)
printfn "%s" (token_to_string p.Value)