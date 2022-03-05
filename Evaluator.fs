module Evaluator
  open Syntax
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
        let res = evaluate_operator current_op ([Number (apply_operation a b op)] @ tl) in
        debug_print (sprintf "Evaluating %s Operations:" (Operator op |> string_of_token))
        print_ast res
        res
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
