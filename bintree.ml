module type ORDERED =
sig
  type t
  
  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
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

module UnbalancedSet = functor (Element : ORDERED)-> 
struct
  type elem = Element.t
  type tree = E | T of tree * elem * tree
  type set = tree
  
  let empty = E
  
  let rec member x = function
    | E -> false
    | T (a,y,b) ->
      if (Element.lt x y) then member x a 
      else if (Element.eq x y) then true
      else member x b
    
  let rec insert x = function
    | E -> T (E,x,E)
    | T (a,y,b) as r->
      if (Element.lt x y) then T (insert x a,y,b)
      else if (Element.eq x y) then r
      else T (a,y,insert x b)
      
  let rec members = function
    | E -> []
    | T (a,x,b) -> (members a) @ [x] @ (members b)
end

module Integer : (ORDERED with type t = int) = 
struct
  type t = int
  let eq = (==)
  let lt = (<)
  let leq = (<=)
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
