(*KATHERINE A SCOTT*)
(*KAS2221@COLUMBIA.EDU*)
(*10/12/2011*)
(*PLT HW1*)
(*PROBLEM 1*)
(*********************************************************************************)
let rec uniq l = 
  match l with
    [] -> [] 
  | x::y::rest -> 
      if (x == y) then ([x]@(uniq rest))
      else ([x]@(uniq ([y]@rest)))
  | x::[] -> [x];;

