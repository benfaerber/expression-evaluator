module Syntax

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

let is_debug_mode = false
let debug_print value =
  if is_debug_mode then
    printfn "%s" value

let print_token token = token |> string_of_token |> debug_print

let print_ast ast =
  List.iter print_token ast
  debug_print ""

let sleep () = if is_debug_mode then Async.Sleep(500) |> Async.RunSynchronously
