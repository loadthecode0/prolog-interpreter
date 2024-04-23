(* supporting functions for AST construction *)
open Printf;;

type var_node = string
type const_str_node = string
type sym_node = string (*can have >=0 arities*)
type term_node = 
    Var of var_node 
  | Num of int 
  | Const_str of const_str_node 
  | Pred of sym_node * (term_node list) (*last one is for positive arity symbols*)
  | List of term_node list 
  | Tup of term_node list 
type atom_node = Atom of sym_node * (term_node list)
type head_node = Head of atom_node
type body_node = Body of atom_node list
type clause_node = Fact of head_node | Rule of head_node * body_node | Goal of body_node 
type program_tree = Prog of clause_node list
type goal_node = Goal of atom_node list 

let rec print_term_list (tl:term_node list) = match tl with
    [] -> Printf.printf ""
  | [t] -> print_term t
  | t::tls -> (
      print_term t;
      Printf.printf ", ";
      print_term_list tls;
    )

and print_term (t:term_node) = match t with
    Var(v) -> Printf.printf "%s(Var)" v
  | Num(n) -> Printf.printf "%d(Num)" n
  | Const_str(s) -> Printf.printf "%s(Const Str)" s
  | Pred(s, []) -> Printf.printf "%s " s
  | Pred(s, l) -> (
      Printf.printf "[Sym]%s(" s;
      print_term_list l;
      Printf.printf ")";
    )
  | List(l) -> (
    Printf.printf "[List](" ;
    print_term_list l;
    Printf.printf ")";
  )
  | Tup(l) -> (
    Printf.printf "[Tuple](" ;
    print_term_list l;
    Printf.printf ")";
  )
;;


let rec print_atom_list (al:atom_node list) = match al with
    [] -> Printf.printf ""
  | [a] -> print_atom a
  | a::als -> (
      print_atom a;
      Printf.printf "\n\t\t\t";
      print_atom_list als;
    )

and print_atom (a:atom_node) = match a with
    Atom(s, []) -> Printf.printf "Symbol: %s " s
  | Atom(s, l) -> (
      Printf.printf "Symbol: %s, Args: " s;
      print_term_list l;
      Printf.printf " ";
    )
;;

let print_head (h:head_node) = match h with
  Head(a) -> (
    Printf.printf "Head (\n \t\t\t";
    print_atom a;
    Printf.printf "\n\t\t)\n";
  )
;;

let print_body (b:body_node) = match b with
  Body(al) -> (
    Printf.printf "Body (\n \t\t\t";
    print_atom_list al;
    Printf.printf "\n\t\t)\n";
  )
;;

let rec print_clause_list (cl:clause_node list) = match cl with
    [] -> Printf.printf ""
  | [c] -> print_clause c
  | c::cls -> (
      print_clause c;
      Printf.printf "\t";
      print_clause_list cls;
    )
    
and print_clause (c:clause_node) = match c with
    Fact(h) -> (
      Printf.printf "Fact_clause (\n \t\t";
      print_head h;
      Printf.printf "\n\t);\n";
    )
  | Rule(h, b) -> (
      Printf.printf "Rule_clause (\n \t\t";
      print_head h;
      Printf.printf "\t\n \t\t";
      print_body b;
      Printf.printf "\n\t);\n";
    )
  | Goal(b) -> (
      Printf.printf "Goal (\n \t\t";
      print_body b;
      Printf.printf "\n\t);\n";
    )
;;

let print_program (p:program_tree) = match p with
  Prog(cl) -> (
    Printf.printf "Program (\n \t";
    print_clause_list cl;
    Printf.printf ")\n";
  )
;;
