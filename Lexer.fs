module Lexer

open Syntax

let clean_expression (s: string) =
  let allowed = "01234567890+-*/%^.~()" |> Seq.toList in
  let cleaner = List.filter (fun l -> List.contains l allowed) in

  match s |> Seq.toList |> cleaner |> System.String.Concat with
  | "" -> "0"
  | r -> r


let lex_number (s: string) =
  let first = s |> Seq.toList |> List.head in
  let is_negative = first = '~' in

  let is_digit c =
    List.contains c ("0123456789." |> Seq.toList) in

  let trimmed, scalar, offset =
    if is_negative then
      s[1..], -1., 1
    else
      s, 1., 0 in

  let literal =
    Seq.toList trimmed
    |> List.unfold (function
      | [] -> None
      | hd :: tl ->
        if is_digit hd then
          Some(hd, tl)
        else
          None)
    |> System.String.Concat in

  match literal with
  | "" -> None, s
  | l -> Some(Number((float l) * scalar)), s[l.Length + offset ..]


let lex_operator (s: string) =
  let get_operator =
    function
    | '+' -> Some(Operator Add)
    | '-' -> Some(Operator Subtract)
    | '*' -> Some(Operator Multiply)
    | '/' -> Some(Operator Divide)
    | '%' -> Some(Operator Modulo)
    | '^' -> Some(Operator Exponent)
    | _ -> None in

  match Seq.toList s with
  | hd :: _ ->
    (match get_operator hd with
     | Some op -> Some op, s[1..]
     | _ -> None, s)
  | _ -> None, s


let lex_group (s: string) =
  if (s |> Seq.toList |> List.head) = '(' then
    let folder (l_counter, going, acc) chr =
      if not going then
        0, false, acc
      else
        let offset =
          match chr with
          | '(' -> 1
          | ')' -> -1
          | _ -> 0 in

        let counter = l_counter + offset in

        counter, counter <> 0, acc + chr.ToString() in

    let _, _, literal = List.fold folder (0, true, "") (Seq.toList s) in
    let tliteral = literal[1 .. literal.Length - 2] in
    debug_print (sprintf "Group Literal: %s" tliteral)
    Some(RawGroup tliteral), s[literal.Length ..]
  else
    None, s

let lex_token (s: string) =
  let lexers = [ lex_number; lex_group; lex_operator ] in

  let found_lexer =
    lexers
    |> List.find (fun lexer ->
      match lexer s with
      | Some _, _ -> true
      | _ -> false) in

  found_lexer s

let clean_ast =
  List.filter (function
    | RawGroup _ -> false
    | _ -> true)

let rec generate_ast (s: string) =
  let head = lex_token (s |> clean_expression) in
  let first_token, _ = head in

  let optional_ast =
    head
    |> List.unfold (function
      | Some (RawGroup g), u ->
        let sub_group = Some(Group(generate_ast g)) in
        g |> generate_ast |> print_ast
        Some(sub_group, (sub_group, u))
      | _, "" -> None
      | _, u ->
        let next_p, next_u = lex_token u in

        match next_p with
        | Some (RawGroup rg) -> let sub_ast = Some(Group(generate_ast rg)) in Some(sub_ast, (sub_ast, next_u))
        | _ -> Some(next_p, (next_p, next_u))) in

  let tail =
    optional_ast
    |> List.filter (fun opt -> opt.IsSome)
    |> List.map (fun opt -> opt.Value) in

  clean_ast ([ first_token.Value ] @ tail)
