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
(* get_choices myquery myprog;; *)

(* let c = get_choices myquery1 myprog2;;

print_clause_list c;; *)

pp_tree (make_tree_prog myprog2);;
(* pp_tree (make_tree_subgoal myquery1);; *)

let (b, unif) = resolve_query (myprog2) (mygoal2) in

print_sub unif;
Printf.printf "%b.\n" b;;