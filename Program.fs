
let runner () =
  let is_repl_mode = true
  if is_repl_mode then
    Expression.repl ()
  else
    let complex_expression_1 = "(23+(135-32)+2)*3/4"
    let complex_expression_2 = "73/3+(4^2+(8-3))"
    let complex_expression_3 = "~40+20"

    printfn "%f" (Expression.evaluate complex_expression_1)
    printfn "%f" (Expression.evaluate complex_expression_2)
    printfn "%f" (Expression.evaluate complex_expression_3)

runner ()