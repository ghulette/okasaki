open Okasaki

module IntSet = UnbalancedSet(Integer)
open IntSet
let test_set () =
  let root = insert 5 (insert 10 (insert 0 (insert 1 empty))) in
  let elts = members root in
  let str = String.concat "," (List.map string_of_int elts) in
  Printf.printf "Set: %s\n" str

module IntLeftHeap = LeftistHeap(Integer)
open IntLeftHeap
let test_leftheap () =
  let root = insert 5 (insert 10 (insert 0 (insert 7 empty))) in
  let min1 = find_min root in
  let min2 = find_min (delete_min root) in
  Printf.printf "LeftHeap: %d,%d\n" min1 min2

module IntBinHeap = BinomialHeap(Integer)
open IntBinHeap
let test_binheap () =
  let root = insert 5 (insert 10 (insert 0 (insert 7 empty))) in
  let min1 = find_min root in
  let min2 = find_min (delete_min root) in
  Printf.printf "BinHeap: %d,%d\n" min1 min2

;;
test_set();
test_leftheap();
test_binheap();
