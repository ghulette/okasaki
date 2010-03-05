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
end

module UnbalancedSet = functor (Element : ORDERED) -> 
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
    
  let insert x _ = E
end
