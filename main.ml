open Lexer;;
open Parser;;
open Ast;;
open Interpreter;;
open Mgu;;

(* Complete Program Tree *)
let myprog = Prog([
  Fact(Head(Atom("parent", [Const_str("bob"); Const_str("alice")])));
  Fact(Head(Atom("parent", [Const_str("alice"); Const_str("lisa")])));
  Rule(
    Head(Atom("sibling", [Var("X"); Var("Y")])),
    Body([
      Atom("parent", [Var("Z"); Var("X")]);
      Atom("parent", [Var("Z"); Var("Y")]);
      Atom("\\=", [Var("X"); Var("Y")])
    ])
  )
]) 
;;


(* % Facts defining parent-child relationships
parent(bob, alice).
parent(alice, lisa).
parent(alice, john).
parent(bob, tom).
parent(tom, mary).

% Grandparent rule - A grandparent is the parent of a parent
grandparent(GP, GC) :- parent(GP, P), parent(P, GC).

% Sibling rule - two persons are siblings if they share the same parent
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% Cousin rule - two persons are cousins if their parents are siblings
cousin(X, Y) :- parent(PX, X), parent(PY, Y), sibling(PX, PY).

% Query: Find all cousins of 'mary'
?- cousin(mary, Cousin). *)

let myquery1 = 
  Subgoal(
  Atom("sibling", [Const_str("lisa"); Var("alice")])
  )

;;

let myprog2 = Prog([
  Fact(Head(Atom("parent", [Const_str("bob"); Const_str("alice")]))); 
  Fact(Head(Atom("parent", [Const_str("alice"); Const_str("lisa")]))); 
  Fact(Head(Atom("parent", [Const_str("alice"); Const_str("john")]))); 
  Fact(Head(Atom("parent", [Const_str("bob"); Const_str("tom")]))); 
  Fact(Head(Atom("parent", [Const_str("tom"); Const_str("mary")]))); 
  Rule(
    Head(Atom("grandparent", [Var("GP"); Var("GC")])),
    Body([
      Atom("parent", [Var("GP"); Var("P")]);
      Atom("parent", [Var("P"); Var("GC")])
    ])
  );
  Rule(
    Head(Atom("sibling", [Var("X"); Var("Y")])),
    Body([
      Atom("parent", [Var("Z"); Var("X")]);
      Atom("parent", [Var("Z"); Var("Y")]);
      Atom("\\=", [Var("X"); Var("Y")])
    ])
  );
  Rule(
    Head(Atom("cousin", [Var("X"); Var("Y")])),
    Body([
      Atom("parent", [Var("PX"); Var("X")]);
      Atom("parent", [Var("PY"); Var("Y")]);
      Atom("sibling", [Var("PX"); Var("PY")])
    ])
  )
])

let mygoal = Goal([
  (* Atom("sibling", [Const_str("lisa"); Var("alice")]); *)
  Atom("sibling", [Const_str("lisa"); Var("bob")])
  ]);;

let mygoal2 = Goal([
    (* Atom("cousin", [Const_str("mary"); Var("Cousin")]); *)
    Atom("grandparent", [Var("X"); Const_str("lisa")])
  ])
;;

let myquery2 = Subgoal(
    (* Atom("cousin", [Const_str("mary"); Var("Cousin")]); *)
    Atom("grandparent", [Var("X"); Const_str("lisa")])
  )
;;
(* get_choices myquery myprog;; *)

(* let c = get_choices myquery1 myprog2;;

print_clause_list c;; *)

(* pp_tree (make_tree_prog myprog2);;
Printf.printf "Query:" ;;
pp_tree (make_tree_subgoal myquery2);;

let (b, unif) = resolve_query (myprog2) (mygoal2) in

print_sub unif; *)
(* Printf.printf "%b.\n" b;; *)

let read_file (file_name : string) : string =
  let channel = open_in file_name in
  let file_size = in_channel_length channel in
  let content = really_input_string channel file_size in
  close_in channel;
  content
;;

let remove_carriage_returns file_name =
  let input_channel = open_in file_name in
  let rec read_and_filter acc =
    try
      let next_char = input_char input_channel in
      if next_char = '\r' then
        read_and_filter acc
      else
        read_and_filter (acc ^ (String.make 1 next_char))
    with End_of_file ->
      close_in input_channel;
      acc
  in
  let result = read_and_filter "" in
  result
;;

let main () =

  let prog_str = remove_carriage_returns "program.txt"
  and query_str = remove_carriage_returns "query.txt"
    in
    try
      
      let _ = print_endline prog_str 
      and lexbuf = Lexing.from_string prog_str in
        let prog = Parser.program Lexer.token lexbuf in
          let lexbuf2 = Lexing.from_string query_str in
            let g = Parser.makegoal Lexer.token lexbuf2 in
              let _ = resolve_query (prog) (g) in ()
            
      
    with End_of_file -> exit 0
  ;;
  let _ = Printexc.print main ()