(*KATHERINE A SCOTT*)
(*KAS2221@COLUMBIA.EDU*)
(*10/12/2011*)
(*PLT HW1*)
(*PROBLEM 3 - HW 1 - scanner.mll*)
(*********************************************************************************)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { ASSIGN }
| ',' { ENDSTMNT }
| '$'['0'-'9'] as lit { VARIABLE(int_of_char lit.[1] - 48) }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| eof { EOF }
