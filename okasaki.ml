exception Empty
exception Not_found

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
  
  let rec member x = function
    | E -> false
    | T (a,y,b) ->
      if (Elem.lt x y) then member x a 
      else if (Elem.eq x y) then true
      else member x b
    
  let rec insert x = function
    | E -> T (E,x,E)
    | T (a,y,b) as r->
      if (Elem.lt x y) then T (insert x a,y,b)
      else if (Elem.eq x y) then r
      else T (a,y,insert x b)
      
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

  let rec merge h1 h2 = match h1,h2 with
    | h,E -> h
    | E,h -> h
    | T (_,x,a1,b1),T (_,y,a2,b2) -> 
      if (Elem.leq x y) then make x a1 (merge b1 h2)
      else make y a2 (merge h1 b2)
      
  let insert x h = merge (T (1,x,E,E)) h
  
  let find_min = function
    | E -> raise Empty
    | T (_,x,_,_) -> x
  
  let delete_min = function
    | E -> raise Empty
    | T (_,x,lh,rh) -> merge lh rh
  
end

module IntUnbalancedSet = UnbalancedSet(Integer)

open IntUnbalancedSet

let main () =
  let root = insert 5 (insert 10 (insert 0 (insert 1 empty))) in
  let elts = members root in
  let str = String.concat "," (List.map string_of_int elts) in
  Printf.printf "%s\n" str
  
;;
main ()
