exception Empty
exception Not_found
exception Undefined

module type ORDERED =
sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end

module Integer : (ORDERED with type t = int) = 
struct
  type t = int
  let eq = (==)
  let lt = (<)
  let leq = (<=)
end

module type SET =
sig
  type elem
  type set
  val empty : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool
  val members : set -> elem list
end

module UnbalancedSet = functor (Elem : ORDERED)-> 
struct
  type elem = Elem.t
  type tree = E | T of tree * elem * tree
  type set = tree
  
  let empty = E

  (* Complexity for unbalanced binary trees depends on the depth (d) of the
  binary tree.  At best, the  depth is log(n), but in the worst case it 
  could be n.*)

  (* member: O(d) *)
  let rec member x = function
    | E -> false
    | T (a,y,b) ->
      if (Elem.lt x y) then member x a 
      else if (Elem.eq x y) then true
      else member x b
    
  (* insert: O(d) *)
  let rec insert x = function
    | E -> T (E,x,E)
    | T (a,y,b) as r->
      if (Elem.lt x y) then T (insert x a,y,b)
      else if (Elem.eq x y) then r
      else T (a,y,insert x b)

  (* members: n *)
  let rec members = function
    | E -> []
    | T (a,x,b) -> (members a) @ [x] @ (members b)
end

module type HEAP = functor (Elem : ORDERED) ->
sig
  type heap
  val empty : heap
  val is_empty : heap -> bool
  val insert : Elem.t -> heap -> heap
  val merge : heap -> heap -> heap
  val find_min : heap -> Elem.t
  val delete_min : heap -> heap
end

module LeftistHeap : HEAP = functor (Elem : ORDERED) ->
struct
  type heap = E | T of int * Elem.t * heap * heap
  
  let empty = E
  
  let is_empty = function
    | E -> true
    | _ -> false
    
  let rank = function
    | E -> 0
    | T (r,_,_,_) -> r
    
  let make x a b = 
    if rank a >= rank b then T (rank b + 1,x,a,b)
    else T (rank a + 1,x,b,a)

  (* merge: O(log n) *)
  let rec merge h1 h2 = match h1,h2 with
    | h,E -> h
    | E,h -> h
    | T (_,x,a1,b1),T (_,y,a2,b2) -> 
      if (Elem.leq x y) then make x a1 (merge b1 h2)
      else make y a2 (merge h1 b2)

  (* insert: O(log n) *)
  let insert x h = merge (T (1,x,E,E)) h
  
  (* find_min: O(1) *)
  let find_min = function
    | E -> raise Empty
    | T (_,x,_,_) -> x
  
  (* delete_min: O(log n) *)
  let delete_min = function
    | E -> raise Empty
    | T (_,x,lh,rh) -> merge lh rh  
end

module BinomialHeap : HEAP = functor (Elem : ORDERED) ->
struct
  type tree = Node of int * Elem.t * tree list
  let rank (Node (r,_,_)) = r
  let root (Node (_,x,_)) = x
  
  type heap = tree list
  
  let empty = []
  
  let is_empty = function
    | [] -> true
    | _ -> false
    
  let link (Node (r,x,c1) as t1) (Node (_,y,c2) as t2) =
    if Elem.leq x y then Node (r+1,x,t2::c1)
    else Node (r+1,y,t1::c2)
    
  let rec insert_tree t = function
    | [] -> [t]
    | t'::ts' as ts ->
      if rank t < rank t' then t :: ts 
      else insert_tree (link t t') ts'
    
  let insert x ts = insert_tree (Node (0,x,[])) ts
    
  let rec merge ts1 ts2 = match ts1,ts2 with
    | ts,[] -> ts
    | [],ts -> ts
    | t1::ts1',t2::ts2' ->
      if rank t1 < rank t2 then t1 :: merge ts1' ts2
      else if rank t2 < rank t1 then t2 :: merge ts1 ts2'
      else insert_tree (link t1 t2) (merge ts1' ts2')
      
  let rec remove_min_tree = function
    | [] -> raise Empty
    | [t] -> (t,[])
    | t::ts -> 
      let (t',ts') = remove_min_tree ts in
      if Elem.leq (root t) (root t') then (t,ts) else (t',t::ts')
  
  let find_min ts = 
    let (t,_) = remove_min_tree ts in 
    root t
    
  let delete_min ts = 
    let (Node (_,x,ts1),ts2) = remove_min_tree ts in
    merge (List.rev ts1) ts2
end
