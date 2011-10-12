open Ast

let rec eval state = function 
    Lit(x) -> x
  | Var(x) -> state.(x) (*do the lookup*)
  | Seq(e1,e2) -> eval state e1
  | Asn(j,e1) -> 
      state.(j) <- eval state e1  (*state.(j) <- v*)
  | Binop(e1, op, e2) ->
      let v1 = eval state e1 and v2 = eval state e2 in
      match op with
	Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let state = Array.make 10 0 in
  let result = eval state expr in
  print_endline (string_of_int result)
