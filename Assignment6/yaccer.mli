type token =
  | V of (string)
  | C of (string)
  | LPAR
  | RPAR
  | COMMA
  | SC
  | NL
  | ASSIGN
  | DOT
  | EOF

val line :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A6.term * A6.term list
