type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf;;

(*common tree *)

let rec height t = match t with
  | Node (v, l, r) -> 1 + max (height l) (height r) 
  | Leaf -> -1;;

(* BST *)
let rec bst_find_min t = match t with
  | Node (v, l, r) -> (match l with
                      | Node (vl, ll, lr) -> (match ll with
                            | Node (vll, lll, llr) -> bst_find_min l
                            | Leaf -> (l, ll))
                      | Leaf -> (l, Leaf))
  | Leaf -> (Leaf, Leaf)

let rec bst_push t value = match t with
  | Node (v, l, r) -> if v > value 
                      then Node (v, bst_push l value, r) 
                      else Node (v, l, bst_push r value)
  | Leaf -> Node (value, Leaf, Leaf);;

let rec bst_search t value = match t with
  | Node (v, l, r) -> if v == value then
                          Node (v, l, r)
                      else
                        if v > value then
                          bst_search l value
                        else
                          bst_search r value
  | Leaf -> Leaf

let rec bst_delete t value = let node = bst_search t value in
  match node with
  | Node (v, l, r) -> Node (v, l, r)
  | Leaf -> Leaf
