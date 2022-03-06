module Expression

open Syntax

let debug_expression_printer exp ast =
  debug_print "Complete AST:"
  debug_print "------------"
  debug_print (sprintf "Raw Expression: %s" exp)
  print_ast ast
  debug_print "-------------"

let evaluate exp =
  let ast = exp |> Lexer.generate_ast in
  debug_expression_printer exp ast
  ast |> Evaluator.evaluate_ast

let print_result exp = printfn "%f" (exp |> evaluate)

let repl () =
  printfn "Expression REPL"

  let rec aux () =
    printf "> "
    let inp = System.Console.ReadLine() in

    match inp with
    | "" -> aux ()
    | "q"
    | "quit" -> printfn "Bye!"
    | _ ->
      let solution = evaluate inp in
      printfn ": %f" solution
      aux ()

   in

  aux ()
