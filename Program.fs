open Syntax

module Expression =
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

  let print_result exp =
    printfn "%f" (exp |> evaluate)

  let repl () =
    printfn "Expression REPL"
    let rec aux () =
      printf "> "
      let inp = System.Console.ReadLine() in

      match inp with
      | "" -> aux ()
      | "q" | "quit" ->
        printfn "Bye!"
      | _ ->
        let solution = evaluate inp in
        printfn "- %f" solution
        aux ()

    in aux ()

let runner () =
  let is_repl_mode = true
  if is_repl_mode then
    Expression.repl ()
  else
    let complex_expression_1 = "(23+(135-32)+2)*3/4"
    let complex_expression_2 = "73/3+(4^2+(8-3))"
    let complex_expression_3 = "~40+20"


    // print_ast (Lexer.generate_ast complex_expression_2)
    // printfn "%f" (evaluate_expression "23+13.5*3/4")
    // printfn "%f" (evaluate_expression "8+4-2^4/10")
    printfn "%f" (Expression.evaluate complex_expression_1)
    printfn "%f" (Expression.evaluate complex_expression_2)
    printfn "%f" (Expression.evaluate "(1+1)")

runner ()