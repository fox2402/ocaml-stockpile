type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf;;

(*common tree *)

let rec height t = match t with
  | Node (v, l, r) -> 1 + max (height l) (height r) 
  | Leaf -> -1;;

(* BST *)
let rec bst_find_min t = match t with
  | Node (v, l, r) -> (match l with
                      | Node -> bst_find_min l
                      | Leaf -> v
  | Leaf -> -1

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


let rec bst_delete t value = match t with
  | Node (v, l, r) -> if v < value then
                        let (res, va) = bst_delete l value in
                        (Node (v, res, r), va)
                      if v > value then
                        let (res, va) = bst_delete r value in
                        (Node (v, l, res), va)
                      else
                        (match l, r with
                        | (Node, Leaf) -> (l, v)
                        | (Leaf, Node) -> (r, v)
                        | _, _ ->
            
  | Leaf -> (Leaf, -1)
