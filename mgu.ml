open List;;
open Printf;;

type symbol = string*int
type signature = symbol list
type tree = V of string | C of {node: symbol; children: tree list}
type substitution = (string*tree) list (*variable to tree map*)

let rec print_signature =
  function
  | [] -> ()
  | (a, b) :: rest ->
    Printf.printf "%s, %d \n" a b;
    print_signature rest
;;

let rec print_list =
  function
  | [] -> Printf.printf "\n"
  | [s] -> Printf.printf "%s  \n" s
  | s :: rest ->
    Printf.printf "%s,  " s;
    print_list rest
;;

let rec iter f = function
  | [] -> ()
  | [x] ->
      f true x
  | x :: tl ->
      f false x;
      iter f tl

let to_buffer ?(line_prefix = "") ~get_name ~get_children buf x =
  let rec print_root indent x =
    bprintf buf "%s\n" (get_name x);
    let children = get_children x in
    iter (print_child indent) children
  and print_child indent is_last x =
    let line =
      if is_last then
        "└── "
      else
        "├── "
    in
    bprintf buf "%s%s" indent line;
    let extra_indent =
      if is_last then
        "    "
      else
        "│   "
    in
    print_root (indent ^ extra_indent) x
  in
  Buffer.add_string buf line_prefix;
  print_root line_prefix x

let to_string ?line_prefix ~get_name ~get_children x =
  let buf = Buffer.create 1000 in
  to_buffer ?line_prefix ~get_name ~get_children buf x;
  Buffer.contents buf

let pp_tree t =
  let get_name = function
    | V x -> x
    | C r -> let (s,n) = r.node in s
  in
  let get_children = function
    | V _ -> []
    | C r -> r.children
  in
  let result = to_string ~line_prefix:"\t " ~get_name ~get_children t 
  in
  print_string result;
  flush stdout;;

let rec print_sub =
  function
  | [] -> Printf.printf "\n"
  | (v, t) :: rest ->
    Printf.printf "%s ->  " v;
    pp_tree t;
    Printf.printf "\n";
    print_sub rest
;;

(* check if element exists in a list *)
let rec exist_list elem lst = match lst with
  | [] -> false
  | hd::tl -> (hd = elem) || exist_list elem tl
;;

(* check if element exists in a list *)
let rec exist sym sigma = match sigma with
    [] -> false
  | (s, _)::tl -> (s = sym) || exist sym tl
;;

(* check for duplicates in a list *)
let rec dupExist sigma = match sigma with 
  | [] -> false
  | (sym, _)::tl -> (exist sym tl) || dupExist tl
;;

(* check for negative arities *)
let rec negExist sigma = match sigma with 
  | [] -> false
  | (_, n) :: tl -> (n < 0) || (negExist tl)
;;

let check_sig sigma = (not(dupExist sigma)) && (not(negExist sigma));;

(* check if tree is well-formed *)
let rec wftree t = match t with
| V _ -> true
| C r -> 
  let (s, n) = r.node
  in 
    Printf.printf "%s, %d, %d\n" s n ( List.length (r.children));
    (n=(List.length r.children)) && (List.fold_left (&&) true (map wftree r.children))
;;   

let rec ht t = match t with
  | V _ -> 0
  | C r ->
    let (s,n) = r.node
    in 
      (if n = 0
      then 0
      else 1+(fold_left max 0 (map ht r.children)
      ))
;; 

let rec size t = match t with
  | V _ -> 1
  | C r -> 1+(fold_left (+) 0 (map size r.children) )
;; 


let remove_elt e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []
;;

let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs) (x::acc)
  in go l []
;;

let vars tr = 

  let rec vars_rec lst t = match t with 
    | V x -> (
      if (exist_list x lst) then lst 
      else [x] @ lst
      )
    
    | C r -> fold_left (@) [] (map (vars_rec lst) r.children)
  in
    remove_duplicates( vars_rec [] tr)
;;

let reverse_list lst =
  let rec rev2 s1 s2 = match s1 with
    | [] -> s2
    | x::xs -> rev2 xs (x::s2)
  in
    rev2 lst []
;;

let rec mirror t= match t with
  | V x -> V x 
  | C r ->
      C {
        node = r.node;
        children = reverse_list (map mirror r.children)
      }
;;

let rec find_sub x lst = match lst with
    | [] -> V x
    | (v, t)::tl -> if (v=x) then t else find_sub x tl
;;

let rec subst sub_fn t = match t with
  | V x -> find_sub x sub_fn
  | C r ->
      C {
        node = r.node;
        children = map (subst sub_fn) r.children
      };
;;

let sub_fn_id v = V v;;

exception NOT_UNIFIABLE

let rec expand acc lst2 = match lst2 with
  | [] -> acc 
  | (v, t)::rem -> 
      if (exist v acc) then expand acc rem 
      else expand ((v,t)::acc) rem
let rec compose_subst_lists_1 sub_fn_1 sub_fn_2 = match sub_fn_1 with
  | [] -> []
  | (v, t) :: rem -> 
      (v, (subst sub_fn_2 t) ) :: (compose_subst_lists_1 rem sub_fn_2)
;;

let compose_subst sub_fn_1 sub_fn_2 = expand (compose_subst_lists_1 sub_fn_1 sub_fn_2) sub_fn_2;;

let rec mgu treepair = match treepair with
  | (V x, V y) -> [(x, V y)]
  | (C r1, C r2) ->
      if (r1.node = r2.node) then mgu_children (r1.children,r2.children) (*in well-formed tree, if node is same, then children list have same size*)
      else raise NOT_UNIFIABLE
  | (V v, C r) ->
      let lst = vars (C r) in
        if (exist_list v lst) then raise NOT_UNIFIABLE
        else [(v, C r)]
  | (C r, V v) -> mgu (V v, C r)

and mgu_children lstpair  = match lstpair with
    | ([], []) -> []
    | ([t1], [t2]) -> mgu (t1, t2)
    | (t1::rem1, t2::rem2) ->
        let m = mgu_children(rem1, rem2) in
          let n = mgu((subst m t1), (subst m t2)) in
            compose_subst m n;
    | (_,_) -> raise NOT_UNIFIABLE
;;

