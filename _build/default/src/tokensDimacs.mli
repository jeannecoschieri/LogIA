
(* The type of tokens. *)

type token = 
  | ZERO
  | P
  | NEWLINE
  | INT of (int)
  | EOF
  | CNF
