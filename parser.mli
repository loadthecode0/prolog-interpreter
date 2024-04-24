type token =
  | INT of (int)
  | VARIABLE of (string)
  | IDENTIFIER of (string)
  | IS
  | NOT
  | GT
  | NEQ
  | ADD
  | EQUALS
  | PERIOD
  | SEPARATOR
  | IF
  | MUL
  | LPAREN
  | RPAREN
  | LSQ
  | RSQ
  | BAR
  | EOL
  | EOF
  | CUT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program_tree
val makegoal :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.goal_node
