open Ast;;
open Mgu;;

exception SomethingInvalid;;

type state = Ast.program_tree * Ast.goal_node * substitution * (string list);;

type decision_stack = state list;;

type choice_list = Ast.clause_node list;;

let rec indexTermNode (i:int) (t:term_node): term_node = match t with
    Var(v) -> Var((string_of_int i) ^ "__" ^ v)
  | Pred(s, l) -> Pred(s, List.map (indexTermNode i) l)
  | _ -> t
;;

let rec indexAtom (i:int) (a:atom_node): atom_node = match a with
  Atom(s, l) -> Atom(s, List.map (indexTermNode i) l)
;;

let rec indexClause (i:int) (cl:clause_node) : clause_node = match cl with
    Fact(Head(a)) -> Fact(Head(indexAtom i a))
  | Rule(Head(a), Body(al)) -> Rule(Head(indexAtom i a), Body(List.map (indexAtom i) al))
;;

let rec indexParallelClauses (clause_list:(clause_node list)) (i:int): (clause_node list) = match clause_list with
    [] -> []
  | cl::cls -> (indexClause i cl)::indexParallelClauses cls (i+1)
;;

let rec increaseDepth (clause_list:(clause_node list)) (Atom(s, _): atom_node): (clause_node list) = match (clause_list) with
    [] -> []
  | cl::cls -> match cl with Fact(Head(Atom(s', _))) | Rule(Head(Atom(s', _)), _) ->
                if s = s' then (indexClause 0 cl)::(increaseDepth cls (Atom(s, [])))
                else cl::(increaseDepth cls (Atom(s, [])))
;;

let rec get_choices (sg:subgoal_node) (prog:program_tree) : choice_list = 
  let Subgoal(Atom(func, nodes)) = sg in
  match prog with
  | Prog([]) ->  []
  | Prog(cl::cls) -> match cl with
              | Fact(Head(Atom(func', _))) -> if func' = func then cl::(get_choices sg (Prog(cls)))
                                              else get_choices sg (Prog(cls))
              | Rule(Head(Atom(func', _)), _) -> if func' = func then cl::(get_choices sg (Prog(cls)))
                                                else get_choices sg (Prog(cls))
              | _ -> get_choices sg (Prog(cls))
;;

let rec filterSubst (varList:(string list)) (sub:substitution) : (substitution)=
  match varList with
  | [] -> []
  | v::vs -> let t = (find_sub v sub) in
      if (t <> (V(v))) then (v,t)::(filterSubst vs sub)
      else (filterSubst vs sub)
;;

let rec make_tree_prog (p:program_tree) : tree =
  match p with 
  | Prog([]) -> C{node = ("__empty_prog__", 0); children = []}
  | Prog(cls) -> C{node = ("__program__", List.length(cls)); children = (List.map make_tree_clause cls)}
  | _ -> raise SomethingInvalid

and make_tree_goal (g:goal_node) : tree =
  match g with 
  | Goal([]) -> C{node = ("__empty_goal__", 0); children = []}
  | Goal(atoms) -> C{node = ("__goal__", List.length(atoms)); children = (List.map make_tree_atom atoms)}
  | _ -> raise SomethingInvalid

and make_tree_subgoal (sg:subgoal_node) : tree =
  match sg with 
  | Subgoal(a) -> make_tree_atom a

and make_tree_clause (cl:clause_node) : tree =
  match cl with 
  | Fact(Head(a)) -> make_tree_atom a
  | Rule(Head(a), _) -> make_tree_atom a
  | _ -> raise SomethingInvalid

and make_tree_atom (a:atom_node) : tree =
  match a with
  | Atom(x, tl) -> C{node = (x, List.length(tl)); children = (List.map make_tree_term tl)}
  | _ -> raise SomethingInvalid

and make_tree_term (t:term_node) : tree =
  match t with
  | Var(v) -> V(v)
  | Num(n) -> C{node = (string_of_int(n), 0); children = []}
  | Const_str(s) -> C{node = (s, 0); children = []}
  | Pred (s, tl) -> make_tree_atom (Atom(s, tl))
  | _ -> raise SomethingInvalid

;;

let rec solve_goals (depth:int) (g:goal_node) (origVars : (string list)) (prog:program_tree) (table:substitution)=
  match g with
  | Goal([]) -> 
      let _ = ( Printf.printf "Finally:\n" ; print_sub (filterSubst origVars table) )in (true, [])
  | Goal(atom::remGoals) ->
      let sg = (Subgoal(atom)) in
          let Prog(clause_list) = prog in
            let subProg = increaseDepth clause_list atom in
            (
              let choices = get_choices sg (Prog(subProg)) in 
              solve_subgoal_with_choices (depth+1) sg origVars choices remGoals (Prog(subProg)) table
            );

and solve_subgoal_with_choices d sg origVars chs rg subProg table = match chs with
  | [] -> (false, [])
  | ch::remChoices ->
    try
      (
      let (b1, s1) = (unify sg ch table) in
      let (b2, s2) = 
      match ch with 
        | (Fact(Head(a))) ->(solve_goals (d+1) (Goal(rg)) origVars subProg s1)
        | (Rule(Head(a), Body(subgoals))) ->(solve_goals (d+1) (Goal(subgoals @ rg)) origVars subProg s1)
        in 
          if (b2 = true) then (b2, s2)
          else solve_subgoal_with_choices d sg origVars remChoices rg subProg table
      )
    with
      NOT_UNIFIABLE -> solve_subgoal_with_choices d sg origVars remChoices rg subProg table

and unify sg ch  table = 
  match ch with
  | (Fact(Head(a))) | (Rule(Head(a), _)) -> 
      let t1 = subst table (make_tree_atom a) 
      and t2 = subst table (make_tree_subgoal sg) in
        Printf.printf "Unifying these trees:\n";
        pp_tree t1; pp_tree t2;
        Printf.printf "mgu:\n";
        let unifier = compose_subst table (mgu (t1, t2))  in
          let _ = print_sub unifier in
            (true, unifier)
   
let resolve_query (prog:program_tree) (g:goal_node) = 
  match prog with Prog(clause_list) ->
    let _ = solve_goals 0 g (vars (make_tree_goal g)) (Prog(indexParallelClauses clause_list 1)) [] 
  in true;;

