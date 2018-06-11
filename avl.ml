type 'a avl = AVL_Node of int * 'a * 'a avl * 'a avl | AVL_Leaf;;


let avl_rotate_left avl = match avl with 
  |AVL_Node(b, v, l, r) -> (match r with
    |AVL_Node(rb, rv, rl, rr) -> AVL_Node (rb, rv, AVL_Node(b, v, l, rl), rr)
    |AVL_Leaf -> AVL_Leaf)
  |AVL_Leaf -> AVL_Leaf;;


let avl_rotate_right avl = match avl with
  |AVL_Node(b, v, l, r) -> (match l with
    |AVL_Node(lb, lv, ll, lr) -> AVL_Node (lb, lv, ll, AVL_Node(b, v, lr, r))
    |AVL_Leaf -> AVL_Leaf)
  |AVL_Leaf -> AVL_Leaf;;
