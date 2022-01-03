type token =
  | FLOAT of (float)
  | LPARENTHESIS
  | RPARENTHESIS
  | COMMA
  | COLON
  | LBRACKET
  | RBRACKET
  | FTERM
  | NL
  | ASSIGN
  | INT of (int)
  | UOP of (string)
  | BOP of (string)

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
