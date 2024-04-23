


type myBool = T | F;;


type exp = 
  Num of int | Bl of myBool
| V of string (* variables *)
| Plus of exp * exp | Minus of exp * exp | Times of exp * exp | Div of exp * exp
| And of exp * exp | Or of exp * exp | Not of exp
| Eq of exp * exp | Gt of exp * exp
| IfTE of exp * exp * exp
| Switch of exp*((exp*exp)list)*exp
| Pair of exp * exp
| Fst of exp | Snd of exp 
| Tuple of exp list
| Proj of exp*exp (*number of tup*)
| Abs of string*exp
| App of exp*exp
;;

let myBool2bool b = match b with
    T -> true
  | F -> false
;; 

type opcode =
  LDN of int | LDB of bool
| LOOKUP of string
| PLUS | MINUS | TIMES | DIV
| AND | OR | NOT | EQ | GT
| COND of codelist*codelist
| CASE of ((int*codelist)list)*codelist
| MKPAIR | GETFST | GETSND
| MKTUP of int
| GETPROJ of int
| APP | MKCLOS of string*(codelist) | RET

and codelist = opcode list
;;

type table = (string*values) list
  and closure = string*codelist*table
    and values = N of int | B of bool | P of values*values | T of values list | Vclos of closure;; 
;;



type stack = values list;;

type secTriple = SEC of stack*table*codelist
type dump = secTriple list;; 

exception CaseNotFound_InvalidCase;;
exception InvalidIndex;;
exception Unbound_var of string ;; 
exception Stuck of table * stack * codelist;; 
exception InvalidTupleQuery_NotEnoughArguments;;
exception ProjectionNotFound_IndexOutOfBounds;;
exception DivideByZero_NotAllowed;;



let rec compile e = match e with
    Num n -> [ LDN n ]
  | Bl b -> [LDB (myBool2bool b) ] (* Constants *)
  | V x -> [LOOKUP x] (* Variables *)
  | Plus (e1, e2) -> (compile e1) @ (compile e2) @ [PLUS]
  | Times (e1, e2) -> (compile e1) @ (compile e2) @ [TIMES]
  | Minus (e1, e2) -> (compile e1) @ (compile e2) @ [MINUS]
  | Div (e1, e2) -> (compile e1) @ (compile e2) @ [DIV]
  | And (e1, e2) -> (compile e1) @ (compile e2) @ [AND]
  | Or (e1, e2) -> (compile e1) @ (compile e2) @ [OR]
  | Not e1 -> (compile e1) @ [NOT]
  | Eq (e1, e2) -> (compile e1) @ (compile e2) @ [EQ]
  | Gt(e1, e2) -> (compile e1) @ (compile e2) @ [GT]
  | IfTE(e0, e1, e2) -> (compile e0) @ [COND(compile e1, compile e2)]
  | Switch(e0, lst, e1) -> (compile e0) @ [CASE((List.map caseComp lst), (compile e1))]
  | Pair (e1, e2) ->  (compile e1) @ (compile e2) @ [MKPAIR]
  | Fst p -> (compile p) @ [GETFST]
  | Snd p -> (compile p) @ [GETSND]
  | Tuple (lst) -> 
      let n = (List.length lst) in 
        let compList = (List.map (compile) (lst)) in 
          (List.fold_left (@) [] compList) @ [MKTUP n]
  | Proj (Num i, t) -> (compile t) @ [GETPROJ i]
  | Proj (_, _) -> raise(InvalidIndex)
  | Abs (v, e) -> [MKCLOS(v, (compile e)@[RET])]
  | App (e1, e2) -> (compile e1) @ (compile e2) @ [APP]
  (* | _ -> raise(Invalid_arg) *)

and caseComp (e0, e) = match e0 with 
  | Num i -> (i, compile e)
  | _ -> raise (CaseNotFound_InvalidCase)
;; 



let rec lookup g x = match g with
    | [] -> raise (Unbound_var(x))
    | (y, ans)::tl->if x = y then ans else lookup tl x
;;

let rec augment g x a= match g with
    | [] -> [(x, a)]
    | (y, ans)::tl->if x = y then (x, a)::tl else (y, ans)::(augment tl x a)
;;




let rec print_value v = match v with
  | N(n) -> Printf.printf "%d" n;
  | B(b) -> Printf.printf "%b" b;
  | P(x,y)-> Printf.printf "("; print_value x; Printf.printf ", "; print_value y; Printf.printf ")";
  | T(t) -> Printf.printf "("; print_tuple_elts t; Printf.printf ")"
  | Vclos(cl) -> print_closure cl;

and print_tuple_elts =
  function
  | [] -> Printf.printf ""
  | [v] -> print_value v
  | v::t -> print_value v; Printf.printf ", "; print_tuple_elts t
  
and print_stack =
  Printf.printf "\n-------------------------\n";
  function
  | [] -> Printf.printf "\n-------------------------\n"
  | (P(x,y))::s-> Printf.printf"\n-------------------------\n|\t"; print_value (P(x,y));Printf.printf"\t|"; print_stack s
  | (v)::s -> Printf.printf"\n-------------------------\n|\t"; print_value v;Printf.printf"\t\t|"; print_stack s

and print_closure =
  function
  | (x, c, t) -> Printf.printf "Closure(%s -> [" x; print_codelist c; Printf.printf "])"

and print_table =
  function
  | [] -> Printf.printf "\n"
  | (x, v)::rem -> Printf.printf "%s -> " x; print_value v; Printf.printf "\n"; print_table rem

and print_dump =
  function
  | [] -> Printf.printf "\n"
  | (SEC(s, e, c))::rem -> Printf.printf "Stack(%d), Table(%d), Codelist(%d)\n" (List.length s) (List.length e) (List.length c); print_dump rem

and print_code = 

  function
  | LDN(n) -> Printf.printf "LDN %d" n; 
  | LDB(b) -> Printf.printf "LDB %b" b; 
  | LOOKUP(v) -> Printf.printf "LOOKUP %s" v; 
  | COND(c1, c2) -> Printf.printf "COND(\t"; print_codelist c1; Printf.printf ","; print_codelist c2; Printf.printf ")"; 
  | CASE (_, _) -> Printf.printf "CASE" ; 
  | MKCLOS(x, c) -> Printf.printf "MKCLOS (%s[" x; print_codelist c; Printf.printf "])"; 
  | PLUS -> Printf.printf "PLUS"; 
  | MINUS -> Printf.printf "MINUS"; 
  | TIMES -> Printf.printf "TIMES"; 
  | DIV -> Printf.printf "DIV"; 
  | AND -> Printf.printf "AND"; 
  | OR -> Printf.printf "OR"; 
  | NOT -> Printf.printf "NOT"; 
  | EQ -> Printf.printf "EQ"; 
  | GT -> Printf.printf "GT"; 
  | MKPAIR -> Printf.printf "MKPAIR"; 
  | GETFST -> Printf.printf "GETFST"; 
  | GETSND -> Printf.printf "GETSND"; 
  | MKTUP(n) -> Printf.printf "MKTUP %d" n; 
  | GETPROJ(n) -> Printf.printf "GETPROJ %d" n; 
  | APP -> Printf.printf "APP"; 
  | RET -> Printf.printf "RET"; 
  

and print_codelist = 
  
  function
  | [] -> Printf.printf ""
  | [c1] -> print_code c1
  | c1::c -> print_code c1; Printf.printf "; "; print_codelist c
;;

let rec search lst i e = match lst, e with
  | [], e -> e
  | (j, ej)::tl, e->if j = i then ej else search tl i e
;;



let rec genTupList acc s n = match s,n with
  | s, 0 -> (acc, s)
  | [], _ -> raise(InvalidTupleQuery_NotEnoughArguments)
  | s1::s', n -> genTupList (s1::acc) s' (n-1)
;;






let rec findProj lst n = match lst,n with
  | x::_, 0 -> x
  | [], n -> raise (ProjectionNotFound_IndexOutOfBounds)
  | x::tl, n -> findProj tl (n-1)

exception InvalidCond;;
let rec secd s g c d = 
  Printf.printf "Stack: \n";
  print_stack s;
  Printf.printf "\nTable: \n";
  print_table g;
  Printf.printf "\nCodelist: \n";
  print_codelist c;
  Printf.printf "\nDump: \n";
  print_dump d;
  Printf.printf "\n===================================================== \n\n";
  match s, c, d with
    v::_, [ ], _ -> v (* v-> vclos, no more opcodes, return top *) 
  | s, (LDN n)::c', d -> secd ((N n)::s) g c' d
  | s, (LDB b)::c', d -> secd ((B b)::s) g c' d
  | s, (LOOKUP x)::c', d -> secd ((lookup g x)::s) g c' d
  | (N n2)::(N n1)::s', PLUS::c', d -> secd (N(n1+n2)::s') g c' d
  | (N n2)::(N n1)::s', TIMES::c', d -> secd (N(n1*n2)::s') g c' d
  | (N n2)::(N n1)::s', MINUS::c', d -> secd (N(n1-n2)::s') g c' d
  | (N n2)::(N n1)::s', DIV::c', d -> if n2=0 then raise(DivideByZero_NotAllowed) else secd (N(n1/n2)::s') g c' d
  | (B b2)::(B b1)::s', AND::c', d -> secd (B(b1 && b2)::s') g c' d
  | (B b2)::(B b1)::s', OR::c', d -> secd (B(b1 || b2)::s') g c' d
  | (B b1)::s', NOT::c', d -> secd (B(not b1)::s') g c' d
  | (N n2)::(N n1)::s', EQ::c', d -> secd (B(n1 = n2)::s') g c' d
  | (N n2)::(N n1)::s', GT::c', d -> secd (B(n1 > n2)::s') g c' d
  | (B true)::s', COND(c1,c2)::c', d -> secd s' g (c1 @ c') d
  | (B false)::s', COND(c1,c2)::c', d -> secd s' g (c2 @ c') d
  | _::s', COND(c1,c2)::c', d -> raise(InvalidCond)
  | (N i)::s', CASE(lst, e)::c', d -> secd s' g (((search lst i e))@c') d
  | _::s', CASE(lst, e)::c', d -> raise(CaseNotFound_InvalidCase)
  | x2::x1::s', MKPAIR::c', d -> secd ((P(x1, x2))::s') g c' d
  | (P(x1, x2))::s', GETFST::c', d -> secd (x1::s') g c' d
  | (P(x1, x2))::s', GETSND::c', d -> secd (x2::s') g c' d
  | s, MKTUP(n)::c', d -> 
    let (tuplist, remStk) = (genTupList [] s n) in secd ((T(tuplist))::remStk) g c' d
  | (T(lst))::s', GETPROJ(n)::c', d -> secd ((findProj lst n)::s') g c' d
  | s, MKCLOS(v, c')::c'', d -> secd (Vclos(v, c', g)::s) g c'' d 
  | a::Vclos(v, c', g')::s, APP::c'', d -> secd [] (augment g' v a) c' (SEC(s, g, c'')::d)
  | a::s', RET::c', SEC(s, g, c'')::d -> secd (a::s) g c'' d
  | _, _, _ -> raise (Stuck (g, s, c))
;; 



let c1 = [LOOKUP("k"); GETFST; LDN(9)];;

let execute e = secd [] [] (compile e) [];;

(* print_codelist c1;; *)
let eval exp = print_value (execute exp); Printf.printf "\n";;

(* Test a number literal *)
let test_num () =
  let exp = Num 10 in
  (eval exp)

(* Test a boolean literal *)
let test_bool () =
  let exp = Bl F in
  (eval exp)

(* Test addition *)
let test_plus () =
  let exp = Plus (Num (-7), Num 5) in
  (eval exp)

(* Test multiplication *)
let test_times () =
  let exp = Times (Num 10, Num 39) in
  (eval exp)

(* Test boolean and *)
let test_and () =
  let exp = And (Bl T, Bl F) in
  (eval exp)

(* Test boolean or *)
let test_or () =
  let exp = Or (Bl T, Bl F) in
  (eval exp )

(* Test boolean not *)
let test_not () =
  let exp = Not (Bl T) in
  (eval exp )

(* Test equality *)
let test_eq1 () =
  let exp = Eq (Num 8, Num 2) in
  (eval exp)

let test_eq2 () =
  let exp = Eq (Num 8, Num 8) in
  (eval exp)

(* Test greater than *)
let test_gt () =
  let exp = Gt (Num 38, Num 12) in
  (eval exp)

(* Test conditional *)
let test_ifte () =
  let exp = IfTE (Eq (Num 1, Num 1), Num 42, Num 0) in
  (eval exp)

(* Test conditional *)
let test_switch () =
  let exp = Switch (Times((Plus (Num 1, Num 1)), Times(Num 2, Num 0)), [(Num 0, Num 42); (Num( -1), Num 4); (Num 2, Num 0)], Num 8) in
  (eval exp)

(* Test pair construction *)
let test_pair () =
  let exp = Pair (Num 1, Num 2) in
  (eval exp)

(* Test first element of a pair *)
let test_fst () =
  let exp = Fst (Pair (Num 1, Num 2)) in
  (eval exp)

(* Test second element of a pair *)
let test_snd () =
  let exp = Snd (Pair (Num 1, Num 2)) in
  (eval exp)

(* Test function application *)
let test_app1 () =
  let exp = App (Abs ("y", Plus (V "y", Num 20)), Num 15) in
  (eval exp)

let test_app2 () =
  let exp =
    Plus(
      Proj(Num 1, Tuple([Num 3; App (Abs ("u", Times (V "u", Num 10)), Num 5); Bl T])),
      Proj(Num 3, Tuple([Num 3; App (Abs ("u", Times (V "u", Num 10)), Num 5); Bl T; App (Abs ("v", Plus (V "v", Num 15)), Num 8)]))) 
  in
  (eval exp)

(* Test tuple *)
let test_tup () =
  let exp = Proj(Num 2, Tuple ([Abs ("x", Plus (V "x", Num 1)); Plus(Num 7, Num (10)); Gt(Times(Num 5, Num 7), Plus(Num 0, Num 43)); Or(Bl T, Bl F)]) )in
  (eval exp)

let test_1 () =

  let exp = Or(Bl T, Proj(Num 2, Tuple ([Abs ("x", Plus (V "x", Num 1)); Plus(Num 7, Num (10)); Gt(Times(Num 5, Num 7), Plus(Num 0, Num 43)); Or(Bl T, Bl F)]) ))in
  (eval exp);;

let test_2 () =

  let exp = Or(Num 8, Proj(Num 2, Tuple ([Abs ("x", Plus (V "x", Num 1)); Plus(Num 7, Num (10)); Gt(Times(Num 5, Num 7), Plus(Num 0, Num 43)); Or(Bl T, Bl F)]) ))in
  (eval exp);;

(* Test conditional *)
let test_3 () =
  let exp = Switch (Gt((Plus (Num 1, Num 1)), Times(Num 2, Num 0)), [(Num 0, Num 42); (Num( -1), Num 4); (Num 2, Num 0)], Num 8) in
  (eval exp)

let test_4 () =
  let exp = Proj(Bl T, Tuple ([Abs ("x", Plus (V "x", Num 1)); Plus(Num 7, Num (10)); Gt(Times(Num 5, Num 7), Plus(Num 0, Num 43)); Or(Bl T, Bl F)]) )in
  (eval exp)

let test_5 () =
  let exp = Snd (Pair (Num 1, V "v")) in
  (eval exp)

let test_6 () =
  let exp = Proj(Num 8, Tuple ([Plus(Num 7, Num (10)); Gt(Times(Num 5, Num 7), Plus(Num 0, Num 43)); Or(Bl T, Bl F)]) )in
  (eval exp)

  let test_7 () =
    let exp = Proj(Num 8, Tuple ([Plus(Num 7, Num (10)); Gt(Times(Num 5, Num 7), Div(Num 6, Num 0)); Or(Bl T, Bl F)]) )in
    (eval exp)


let rec main () =
  try 
    Printf.printf "Enter test#: ";
    let fn = match read_line() with
    |"0" -> test_num();
      main()
    | "1" -> test_bool();
      main()
    | "2" ->test_plus();
      main()
    | "3" ->test_times();
      main()
    | "4" ->test_and();
      main()
    | "5" ->test_or();
      main()
    | "6" ->test_eq1(); test_eq2();
      main()
    | "7" ->test_gt();
      main()
    | "8" ->test_ifte();
      main()
    | "9" ->test_switch();
      main()
    | "10" ->test_pair();
      main()
    | "11" ->test_fst();
      main()
    | "12" ->test_snd();
      main()
    | "13" ->test_app1(); test_app2();
      main()
    | "14" -> test_tup();
      main()
    | "15" -> test_1();
      main()
    | "16" -> test_2();
      main()
    | "17" -> test_3();
      main()
    | "18" -> test_4();
      main()
    | "19" -> test_5();
      main()
    | "20" -> test_6();
      main()
    | "21" -> test_7();
      main()
    | "help" -> 
      Printf.printf  
      "
[0] Test a number literal
[1] Test a boolean literal
[2] Plus
[3] Times
[4] And
[5] Or
[6] Eq
[7] Gt
[8] IfTE
[9] Switch
[10] Pair
[11] Fst
[12] Snd
[13] Abs and Abb
[14] Tuple and Projection
[15] General Test 
[16] Error Test 0
[17] Error Test 1
[18] Error Test 2
[19] Error Test 3
[20] Error Test 4
[21] Error Test 5
[help] show test# list
[halt] exit the program
      ";
      main()
    | "halt" -> ()
    | _ -> Printf.printf "Invalid query! Restarting . . . \n" ; main()
    in
      fn; 
  with 
  | Stuck(_,_,_) -> print_endline "ERROR! Message: Stuck"; main()
  | CaseNotFound_InvalidCase -> print_endline "ERROR! Message: CaseNotFound_InvalidCase"; main()
  | InvalidIndex -> print_endline "ERROR! Message: InvalidIndex"; main()
  | Unbound_var(x) -> Printf.printf "ERROR! Message: Unbound_var %s\n" x; main()
  | InvalidTupleQuery_NotEnoughArguments -> print_endline "ERROR! Message: InvalidTupleQuery_NotEnoughArguments"; main()
  | ProjectionNotFound_IndexOutOfBounds -> print_endline "ERROR! Message: ProjectionNotFound_IndexOutOfBounds"; main()
  | DivideByZero_NotAllowed -> print_endline "ERROR! Message: DivideByZero_NotAllowed"; main()
  | End_of_file -> ()
;;

Printf.printf 
"
Edit trees/subs/signature in tests.ml.
On prompt, enter test# to test functions as follows:
[0] Test a number literal
[1] Test a boolean literal
[2] Plus
[3] Times
[4] And
[5] Or
[6] Eq
[7] Gt
[8] IfTE
[9] Switch
[10] Pair
[11] Fst
[12] Snd
[13] Abs and Abb
[14] Tuple and Projection
[15] General Test 
[16] Error Test 0
[17] Error Test 1
[18] Error Test 2
[19] Error Test 3
[20] Error Test 4
[21] Error Test 5
[help] show test# list
[halt] exit the program
";;

(* main()   *)

let execute2 e g = secd [] [] (compile e) [];;

(* print_codelist c1;; *)
let eval2 exp g = print_value (execute2 exp g); Printf.printf "\n";;

(* secd [] [("x", VNum(1))] (compile (V "y")) [] ;;
secd [] [("x", VNum(1))] (compile (V "x")) [] ;;
secd [] [("x", VClos("x", [LOOKUP "y"; RET], [("y", VNum(2))])); ("y", VNum(1))] (compile (App(V "x", V "y"))) [] ;;
secd [] [("x", VNum(1))] (compile (App(Abs("x", V "x"), V "x"))) [] ;;
secd [] [("x", VNum(1)); ("y", VNum(2))] (compile (App(Abs("y", V "y"), V "x"))) [] ;;
secd [] [("x", VNum(1)); ("y", VNum(2))] (compile (App(Abs("y", Abs("x", V "y")), V "x"))) [] ;;
secd [] [("x", VNum(1)); ("y", VNum(2))] (compile (App(Abs("x", App(Abs("y", V "y"), V "x")), V "x"))) [] ;; *)

secd [] [("x", N(1)); ("y", N(2))] (compile (App(Abs("y", Abs("x", V "y")), V "x"))) [] ;;

