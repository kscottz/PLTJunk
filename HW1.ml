let rec uniq l = 
  match l with
    [] -> [] 
  | x::y::rest -> 
      if (x == y) then ([x]@(uniq rest))
      else ([x]@(uniq ([y]@rest)))
  | x::[] -> [x];;

