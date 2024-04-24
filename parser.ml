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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Printf
  open Ast
# 30 "parser.ml"
let yytransl_const = [|
  260 (* IS *);
  261 (* NOT *);
  262 (* GT *);
  263 (* NEQ *);
  264 (* ADD *);
  265 (* EQUALS *);
  266 (* PERIOD *);
  267 (* SEPARATOR *);
  268 (* IF *);
  269 (* MUL *);
  270 (* LPAREN *);
  271 (* RPAREN *);
  272 (* LSQ *);
  273 (* RSQ *);
  274 (* BAR *);
  275 (* EOL *);
    0 (* EOF *);
  276 (* CUT *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VARIABLE *);
  259 (* IDENTIFIER *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\004\000\004\000\002\000\006\000\006\000\
\006\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\008\000\008\000\009\000\009\000\009\000\009\000\009\000\
\009\000\007\000\010\000\010\000\012\000\012\000\012\000\011\000\
\000\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\002\000\004\000\002\000\003\000\001\000\
\003\000\004\000\003\000\003\000\003\000\003\000\003\000\005\000\
\001\000\003\000\001\000\001\000\001\000\001\000\004\000\001\000\
\001\000\001\000\002\000\003\000\001\000\003\000\003\000\003\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\022\000\020\000\000\000\000\000\000\000\
\000\000\017\000\033\000\000\000\000\000\000\000\000\000\000\000\
\024\000\025\000\000\000\034\000\000\000\000\000\026\000\000\000\
\000\000\000\000\000\000\027\000\000\000\000\000\001\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\000\000\000\000\032\000\000\000\
\000\000\000\000\028\000\002\000\000\000\000\000\015\000\011\000\
\012\000\014\000\013\000\009\000\007\000\000\000\000\000\018\000\
\030\000\031\000\005\000\000\000\016\000\023\000"

let yydgoto = "\003\000\
\011\000\020\000\012\000\013\000\021\000\022\000\025\000\026\000\
\016\000\017\000\018\000\030\000"

let yysindex = "\060\000\
\044\255\049\255\000\000\000\000\000\000\000\000\036\255\071\255\
\054\255\000\000\000\000\009\000\024\255\006\255\034\255\072\255\
\000\000\000\000\049\255\000\000\042\255\056\255\000\000\045\255\
\053\255\060\255\066\255\000\000\251\254\065\255\000\000\044\255\
\000\000\049\255\071\255\071\255\071\255\071\255\071\255\071\255\
\068\255\015\255\049\255\000\000\071\255\071\255\000\000\071\255\
\071\255\071\255\000\000\000\000\074\255\080\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\083\255\087\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\082\255\000\000\000\000\
\000\000\000\000\000\000\000\000\110\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\253\254\000\000\000\000\000\000\
\000\000\000\000\096\255\000\000\098\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\096\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\097\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\081\000\000\000\004\000\247\255\001\000\225\255\
\248\255\000\000\000\000\065\000"

let yytablesize = 115
let yytable = "\027\000\
\029\000\015\000\015\000\054\000\014\000\049\000\008\000\024\000\
\031\000\041\000\042\000\008\000\050\000\062\000\063\000\033\000\
\064\000\034\000\036\000\015\000\037\000\038\000\039\000\040\000\
\053\000\048\000\027\000\055\000\056\000\057\000\058\000\059\000\
\015\000\061\000\015\000\014\000\027\000\027\000\023\000\027\000\
\029\000\066\000\032\000\015\000\004\000\005\000\006\000\035\000\
\007\000\004\000\005\000\006\000\043\000\007\000\004\000\005\000\
\006\000\008\000\045\000\009\000\001\000\002\000\019\000\010\000\
\009\000\044\000\046\000\008\000\010\000\009\000\028\000\004\000\
\005\000\006\000\047\000\036\000\048\000\037\000\038\000\039\000\
\040\000\051\000\060\000\067\000\008\000\021\000\009\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\068\000\026\000\
\021\000\069\000\021\000\021\000\023\000\070\000\023\000\023\000\
\023\000\023\000\010\000\010\000\010\000\003\000\019\000\010\000\
\052\000\065\000\029\000"

let yycheck = "\008\000\
\009\000\001\000\002\000\035\000\001\000\011\001\010\001\007\000\
\000\000\019\000\019\000\015\001\018\001\045\000\046\000\010\001\
\048\000\012\001\004\001\019\000\006\001\007\001\008\001\009\001\
\034\000\011\001\035\000\036\000\037\000\038\000\039\000\040\000\
\032\000\043\000\034\000\032\000\045\000\046\000\003\001\048\000\
\049\000\050\000\019\001\043\000\001\001\002\001\003\001\014\001\
\005\001\001\001\002\001\003\001\011\001\005\001\001\001\002\001\
\003\001\014\001\014\001\016\001\001\000\002\000\014\001\020\001\
\016\001\010\001\014\001\014\001\020\001\016\001\017\001\001\001\
\002\001\003\001\015\001\004\001\011\001\006\001\007\001\008\001\
\009\001\017\001\015\001\010\001\014\001\004\001\016\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\015\001\014\001\
\015\001\015\001\017\001\018\001\004\001\015\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\000\000\015\001\015\001\
\032\000\049\000\017\001"

let yynames_const = "\
  IS\000\
  NOT\000\
  GT\000\
  NEQ\000\
  ADD\000\
  EQUALS\000\
  PERIOD\000\
  SEPARATOR\000\
  IF\000\
  MUL\000\
  LPAREN\000\
  RPAREN\000\
  LSQ\000\
  RSQ\000\
  BAR\000\
  EOL\000\
  EOF\000\
  CUT\000\
  "

let yynames_block = "\
  INT\000\
  VARIABLE\000\
  IDENTIFIER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause_list) in
    Obj.repr(
# 32 "parser.mly"
                  (Prog(_1))
# 182 "parser.ml"
               : Ast.program_tree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'clause_list) in
    Obj.repr(
# 35 "parser.mly"
                         ( (_1)::_3)
# 190 "parser.ml"
               : 'clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 36 "parser.mly"
         ( [_1] )
# 197 "parser.ml"
               : 'clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomic_formula) in
    Obj.repr(
# 39 "parser.mly"
                        (Fact(Head(_1)))
# 204 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'atomic_formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'goal) in
    Obj.repr(
# 40 "parser.mly"
                                (Rule(Head(_1), Body(_3)))
# 212 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'goal) in
    Obj.repr(
# 44 "parser.mly"
              (Goal(_1))
# 219 "parser.ml"
               : Ast.goal_node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'goal) in
    Obj.repr(
# 47 "parser.mly"
                                 ((_1)::_3)
# 227 "parser.ml"
               : 'goal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_formula) in
    Obj.repr(
# 48 "parser.mly"
                 ([_1])
# 234 "parser.ml"
               : 'goal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'goal) in
    Obj.repr(
# 49 "parser.mly"
                     (_2)
# 241 "parser.ml"
               : 'goal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'predicate_symbol) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 52 "parser.mly"
                                           (Atom(_1, _3))
# 249 "parser.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 53 "parser.mly"
               (Atom("_gt_", [_1;_3]))
# 257 "parser.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 54 "parser.mly"
                (Atom("_neq_", [_1;_3]))
# 265 "parser.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 55 "parser.mly"
                   (Atom("_equals_", [_1;_3]))
# 273 "parser.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 56 "parser.mly"
                (Atom("_add_", [_1;_3]))
# 281 "parser.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 57 "parser.mly"
               (Atom("_is_", [_1; _3]))
# 289 "parser.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'predicate_symbol) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 58 "parser.mly"
                                               (Atom("_not_of_atom_("^ _2 ^")", _4))
# 297 "parser.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
      (Atom("_cut_", []))
# 303 "parser.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 63 "parser.mly"
                           ((_1)::_3)
# 311 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 64 "parser.mly"
       ([_1])
# 318 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
           (Var(_1))
# 325 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
             (Const_str(_1))
# 332 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 69 "parser.mly"
      (Num(_1))
# 339 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'predicate_symbol) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 70 "parser.mly"
                                           (Pred(_1, _3))
# 347 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 71 "parser.mly"
       (List(_1))
# 354 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple) in
    Obj.repr(
# 72 "parser.mly"
        (Tup(_1))
# 361 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
             (_1)
# 368 "parser.ml"
               : 'predicate_symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
          ([])
# 374 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_term_list) in
    Obj.repr(
# 81 "parser.mly"
                         (_2)
# 381 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 84 "parser.mly"
       ([_1])
# 388 "parser.ml"
               : 'list_term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_term_list) in
    Obj.repr(
# 85 "parser.mly"
                                ((_1)::_3)
# 396 "parser.ml"
               : 'list_term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 86 "parser.mly"
                ([_1; _3])
# 404 "parser.ml"
               : 'list_term_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 89 "parser.mly"
                          (_2)
# 411 "parser.ml"
               : 'tuple))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry makegoal *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program_tree)
let makegoal (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ast.goal_node)
