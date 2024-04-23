%{
  open Printf
  open Ast
%}

%token <int> INT
%token <string> VARIABLE
%token <string> IDENTIFIER
%token IS
%token NOT
%token GT
%token NEQ
%token ADD
%token EQUALS
%token PERIOD
%token SEPARATOR
%token IF
%token ADD MUL
%token LPAREN RPAREN
%token LSQ RSQ
%token BAR
%token EOL
%token EOF
%token CUT

%start program
%type <Ast.program_tree> program

%%

program: //returns program_tree
| clause_list EOF {Prog($1)}

clause_list: //returns clause_node list type
| clause EOL clause_list { ($1)::$3}
| clause { [$1] }

clause: 
| atomic_formula PERIOD {Fact(Head($1))} 
| atomic_formula IF goal PERIOD {Rule(Head($1), Body($3))}
| goal PERIOD {Goal(Body($1))}

goal: //returns atom_node list
| atomic_formula SEPARATOR goal  {($1)::$3}
| atomic_formula {[$1]}
| LPAREN goal RPAREN {$2}

atomic_formula: //atom_node ie string*(term_node list)
| predicate_symbol LPAREN term_list RPAREN {Atom($1, $3)}
| term GT term {Atom("_gt_", [$1;$3])}
| term NEQ term {Atom("_neq_", [$1;$3])}
| term EQUALS term {Atom("_equals_", [$1;$3])}
| term ADD term {Atom("_add_", [$1;$3])}
| term IS term {Atom("_is_", [$1; $3])}
| NOT predicate_symbol LPAREN term_list RPAREN {Atom("_not_of_atom_("^ $2 ^")", $4)}
| CUT {Atom("_cut_", [])} //0-ary symbol, no children
// | LPAREN predicate_symbol LPAREN term_list RPAREN RPAREN{Atom($2, $4)}

term_list: //returns term_node list
| term SEPARATOR term_list {($1)::$3}
| term {[$1]}

term: //returns term_node list
| VARIABLE {Var($1)} 
| IDENTIFIER {Const_str($1)} 
| INT {Num($1)}
| predicate_symbol LPAREN term_list RPAREN {Pred($1, $3)}
| list {List($1)}
| tuple {Tup($1)}
// | term IS term {Pred("is", [$1; $3])}
// | NOT term {Pred("not", [$2])}

predicate_symbol: //returns string (sym_node)
| IDENTIFIER {$1}

list:
  LSQ RSQ {[]}
| LSQ list_term_list RSQ {$2}

list_term_list:
  term {[$1]}
| term SEPARATOR list_term_list {($1)::$3}
| term BAR term {[$1; $3]}

tuple:
  LPAREN term_list RPAREN {$2}
