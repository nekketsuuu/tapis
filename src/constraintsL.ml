module ConstraintsL =
  Set.Make
    (struct
	type t = Type.level * Type.level (* l1 <= l2 *)
	let compare = compare
      end)
include ConstraintsL

open Error
open Type

(* sbst : Type.level sbst -> t -> t *)
let sbst (sigma : Type.level Sbst.sbst) c =
  let sbst'' sigma l =
    match l with
    | LVar(x) ->
       begin
	 try
	   List.assoc x sigma
	 with
	 | Not_found -> l
       end
    | _ -> l
  in
  let sbst' (l1, l2) =
    (sbst'' sigma l1, sbst'' sigma l2)
  in
  map sbst' c

let rec to_list c =
  if is_empty c then []
  else
    let ll = choose c in
    let c = remove ll c in
    ll :: to_list c

(*
 * 制約はレベルをノードとする有向グラフになる
 * 1. 連結成分分解 <- 必要か？ <- ちゃんと1からレベルをつけていくために必要では。
 * 2. サイクルを潰していってDAGにする
 * 3. DAGの根の方からレベルをつけていく -> トポロジカルソート！
 *)

exception Cycle of Type.level

(* solve : t -> Type.level sbst *)
let rec solve c =
  let cs = decompose c in
  let result = List.map (delete_cycle Sbst.empty) cs in
  let cs = List.map snd result in
  let sbst = List.fold_left
	       (fun sbst (sbst', _) -> Sbst.compose sbst' sbst)
	       Sbst.empty
	       result in
  let sbsts = List.map get_level cs in
  let sbst = List.fold_left
	       (fun s t -> Sbst.precise_compose t s) sbst sbsts in
  let sbst' = make_rest_sbst c cs in
  Sbst.precise_compose sbst' sbst
(* decompose : ConstraintsL.t -> ConstraintsL.t *)
and decompose c =
  if is_empty c then []
  else
    let queue = Queue.create () in
    let visited = ref [] in
    (* bfs : ConstrainsL.t -> Type.level -> ConstraintsL.t * ConstraintsL.t *)
    let rec bfs c l = (* undirected graph *)
      visited := l :: !visited;
      let (c1, c2) =
	ConstraintsL.partition
	  (fun (l1, l2) -> if l1 = l && not (List.mem l2 !visited) then
			     (Queue.add l2 queue; true)
			   else if l2 = l && not (List.mem l1 !visited) then
			     (Queue.add l1 queue; true)
			   else
			     false)
	  c
      in
      try
	let (c1', c2') = bfs c2 (Queue.take queue) in
	(union c1' c1, c2')
      with
      | Queue.Empty -> (ConstraintsL.empty, c)
    in
    let c = delete_eq c in
    if is_empty c then [c]
    else
      let (l1, _) = choose c in
      let (c1, c2) = bfs c l1 in
      c1 :: decompose c2
and delete_eq c =
  ConstraintsL.filter (fun (l1, l2) -> l1 <> l2) c
(* delete_cycle : Type.level sbst -> ConstraintsL.t ->
                    Type.level sbst * ConstraintsL.t *)
and delete_cycle eqs c =
  if is_empty c then (eqs, c)
  else
    let queue = Queue.create () in
    let visited = ref [] in
    (* get_cycle : Type.level -> string list -> var list *)
    let rec get_cycle l path =
      match path with
      | [] -> raise @@ ConvErr("delete_cycle: Can't make a cycle")
      | x :: xs ->
	 if x = l then [x]
	 else x :: get_cycle l xs
    in
    (* bfs : ConstraintsL.t -> Type.level -> var list -> var list *)
    let rec bfs c l path = (* directed graph *)
      try
	visited := l :: !visited;
	ConstraintsL.iter
	  (fun (l1, l2) -> if l1 = l then
			     (if List.mem l2 !visited then
				raise @@ Cycle(l2)
			      else
				Queue.add l2 queue))
	  c;
	bfs c (Queue.take queue) (l :: path)
      with
      | Cycle(l') -> get_cycle l' (l :: path)
      | Queue.Empty -> []
    in
    (* to_lvar : Type.level -> var *)
    let to_lvar l =
      match l with
      | LVar(x) ->
	  x
      | _ ->
	 raise @@ ConvErr("to_lvar: Not LVar")
    in
    (* contraction : ConstraintsL.t -> var list -> Type.level sbst * ConstraintsL.t *)
    let contraction c cycle =
      let lvars = List.map to_lvar cycle in
      let x = List.hd lvars in
      let eqs : Type.level Sbst.sbst =
	List.fold_left
	  (fun sbst y -> (y, LVar(x)) :: sbst)
	  Sbst.empty
	  (List.tl lvars) in
      (eqs, delete_eq @@ sbst eqs c)
    in
    let c = delete_eq c in
    if is_empty c then (eqs, c)
    else
      let (l1, _) = choose c in
      let cycle = bfs c l1 [] in
      if cycle = [] then
	(eqs, c)
      else
	let (eqs', c) = contraction c cycle in
	(* Here, we should use precise composition *)
	let eqs = Sbst.precise_compose eqs' eqs in
	delete_cycle eqs c
(* get_nodes : ConstraintsL.t -> Type.level list *)
and get_nodes c =
  if is_empty c then []
  else
    (* TODO(nekketsuuu): 効率化 *)
    let rec del_dup lst =
      match lst with
      | [] -> []
      | l :: lst ->
	 l :: (del_dup @@ List.filter (fun l' -> l <> l') lst)
    in
    let c_list = to_list c in
    let c_list = List.fold_left
		   (fun lst (l1, l2) -> l1 :: l2 :: lst)
		   []
		   c_list
    in
    del_dup c_list
(* get_level : ConstraintsL.t -> Type.level sbst *)
and get_level c =
  let sorted = ref [] in
  let is_start l c =
    not @@ ConstraintsL.exists (fun (_, l2) -> l2 = l) c in
  let rec topological_sort start c =
    match start with
    | [] ->
       if not @@ is_empty c then
	 raise @@ ConvErr("topological_sort: Not a DAG")
    | l :: start ->
       begin
	 sorted := l :: !sorted;
	 let (c1, c2) =
	   ConstraintsL.partition (fun (l1, _) -> l1 = l) c
	 in
	 let start =
	   ConstraintsL.fold
	     (fun (_, l2) start -> if is_start l2 c2 then l2 :: start
				   else start)
	     c1
	     start
	 in
	 topological_sort start c2
       end
  in
  let nodes = get_nodes c in
  let start =
    List.fold_left
      (fun start l -> if is_start l c then l :: start
		      else start)
      [] nodes
  in
  topological_sort start c;
  let (_, sbst) =
    (* fold_right *)
    List.fold_right
      (fun l (i, sbst) -> match l with
			  | LVar(x) ->
			     (i+1, (x, LInt(i)) :: sbst)
			  | _ ->
			     raise @@ ConvErr("get_level: Not a LVar"))
      !sorted
      (Type.min_level, Sbst.empty)
  in sbst
and make_rest_sbst c cs =
    let all_nodes = get_nodes c in
    let each_nodes = List.fold_left
		       List.append [] (List.map get_nodes cs) in
    let rest_nodes = diff all_nodes each_nodes in
    List.fold_left
      (fun sbst l -> match l with
		     | LVar(x) ->
			(x, LInt(Type.min_level)) :: sbst
		     | _ ->
			raise @@ ConvErr("make_rest_sbst: Not a LVar"))
      Sbst.empty
      rest_nodes
and diff lst1 lst2 =
  List.filter (fun l -> not @@ List.mem l lst2) lst1
and debug_sort lst =
  match lst with
  | [] -> ()
  | LVar(x) :: lst ->
     print_string @@ x ^ ", ";
     debug_sort lst
  | LInt(i) :: lst ->
     print_int i;
     print_string ", ";
     debug_sort lst

(*
 * debug
 *)

let c1 = ConstraintsL.of_list
	   [(LVar("x"), LVar("y"));
	    (LVar("y"), LVar("z"))]

let c2 = ConstraintsL.of_list
	   [(LVar("x"), LVar("y"));
	    (LVar("z"), LVar("w"))]

let c3 = ConstraintsL.of_list
	   [(LVar("x"), LVar("y"));
	    (LVar("z"), LVar("w"));
	    (LVar("w"), LVar("w"));
	    (LVar("w"), LVar("z"))]

let c4 = ConstraintsL.of_list
	   [(LVar("x"), LVar("y"));
	    (LVar("y"), LVar("z"));
	    (LVar("y"), LVar("w"));
	    (LVar("w"), LVar("x"))]

let c5 = ConstraintsL.of_list
	   [(LVar("x1"), LVar("x2"));
	    (LVar("x2"), LVar("x3"));
	    (LVar("x3"), LVar("x4"));
	    (LVar("x4"), LVar("x1"));
	    (LVar("x2"), LVar("y1"));
	    (LVar("y1"), LVar("y2"));
	    (LVar("y2"), LVar("y3"));
	    (LVar("y3"), LVar("x4"))]
