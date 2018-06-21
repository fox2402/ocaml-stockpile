type 'a avl = AVL_Node of int * 'a * 'a avl * 'a avl | AVL_Leaf;;

let avl_ex_node avl = match avl with
  |AVL_Node (b, v, l, r) -> (b, v, l, r)
  |AVL_Leaf -> raise (Failure "extracting a Leaf");;

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

let reapply_balance avl =
  let (b, v, l, r) = avl_ex_node avl in 
  let (lb, lv, ll, lr) = avl_ex_node l in
  let (rb, rv, rl, rr) = avl_ex_node r in
  match b with
  |1 -> (AVL_Node(0, v, AVL_Node(0, lv, ll, lr), AVL_Node(-1, rv, rl, rr))
    ,0)
  |0 -> (AVL_Node(0, v, AVL_Node(0, lv, ll, lr), AVL_Node(0, rv, rl, rr))
    ,0)
  |(-1) -> (AVL_Node(0, v, AVL_Node(1, lv, ll, lr), AVL_Node(0, rv, rl, rr))
    ,0)
  |_ -> raise (Failure "out of bound balance value");;

let avl_balance_right avl unb = 
  let avl = 
    if unb != 0 then 
      let (b, v, l, r) = avl_ex_node avl in
      AVL_Node(b + 1, v, l, r);
    else
      avl
  in
  let (b, v, l, r) = avl_ex_node avl in
  if b == 2 then
    let (lb, lv, ll, lr) = avl_ex_node l in
    if lb >= 0 then
      let (b, v, l, r) = avl_ex_node (avl_rotate_right avl) in
      let (rb, rv, rl, rr) = avl_ex_node r in
      (AVL_Node(0, v, l, AVL_Node(0, rv, rl, rr)), 0)
    else
      let avl = avl_rotate_right (AVL_Node(b, v, avl_rotate_left l, r)) in
      reapply_balance avl
  else
    let unb = 
      if not(unb != 0 && b == 1) then
        0
      else
        unb
    in (avl, unb);;

let avl_balance_left avl unb = 
  let avl = 
    if unb != 0 then 
      let (b, v, l, r) = avl_ex_node avl in
      AVL_Node(b - 1, v, l, r);
    else
      avl
  in
  let (b, v, l, r) = avl_ex_node avl in
  if b == -2 then
    let (rb, rv, rl, rr) = avl_ex_node r in
    if rb <= 0 then
      let (b, v, l, r) = avl_ex_node (avl_rotate_left avl) in
      let (lb, lv, ll, lr) = avl_ex_node l in
      (AVL_Node(0, v, AVL_Node(0, lv, ll, lr), r), 0)
    else
      let avl = avl_rotate_left (AVL_Node(b, v, l, avl_rotate_right r)) in
      reapply_balance avl
  else
    let unb = 
      if not(unb != 0 && b == -1) then
        0
      else
        unb
    in (avl, unb);;

let rec _avl_insert avl e eq = 
  match avl with
  | AVL_Leaf -> (AVL_Node(0, e, AVL_Leaf, AVL_Leaf), 1)
  | AVL_Node(b, v, l, r) -> 
    (if (e < v) then let (res, unb) = _avl_insert l e eq in
      avl_balance_right (AVL_Node(b, v, res, r)) unb
    else let (res, unb) = _avl_insert r e eq in
      avl_balance_left (AVL_Node(b, v, l, res)) unb);;

let avl_insert avl e = 
  let (res, unb) = _avl_insert avl e 0 in
  res;;

let rec _avl_del_max avl = match avl with
  |AVL_Leaf -> (avl, 0, 0)
  |AVL_Node (b, v, l, r) -> (match r with 
                            |AVL_Leaf -> (r, v, 1)
                            |_ -> let (res, resv, unb) = _avl_del_max r in
                            let (fres, funb) = 
                            avl_balance_right (AVL_Node (b, v, l, res)) unb in
                            (fres, resv, funb));;


let rec _avl_del_min avl = match avl with
  |AVL_Leaf -> (avl, 0, 0)
  |AVL_Node (b, v, l, r) -> (match l with 
                            |AVL_Leaf -> (r, v, 1)
                            |_ -> let (res, resv, unb) = _avl_del_min l in
                            let (fres, funb) = 
                            avl_balance_left (AVL_Node (b, v, res, r)) unb in
                            (fres, resv, funb));;

let rec _avl_delete avl e unb = match avl with
  |AVL_Leaf -> (AVL_Leaf, unb)
  |AVL_Node (b, v, l, r) when e < v -> (let (res, unb) = _avl_delete l e unb in
                                        avl_balance_left 
                                        (AVL_Node(b, v, res, r)) unb)
  |AVL_Node (b, v, l, r) when e > v -> (let (res, unb) = _avl_delete r e unb in
                                        avl_balance_right 
                                        (AVL_Node(b, v, l, res)) unb)
  |AVL_Node (b, v, l, r) -> (match (l,r) with
                            |(AVL_Leaf, AVL_Leaf) -> (AVL_Leaf, 1)
                            |(AVL_Leaf, _) -> (let (res, resv, resb) =
                            _avl_del_min r in avl_balance_right 
                            (AVL_Node (b, resv, res, r)) resb) 
                            |(_, _) ->(let (res, resv, resb) = 
                            _avl_del_max l in avl_balance_left 
                            (AVL_Node (b, resv, res, r)) resb));;


let avl_delete avl e = let (res, unb) = _avl_delete avl e 0 in res;;

let a = AVL_Leaf;;
let a = avl_insert a 2;;
let a = avl_insert a 3;;
let a = avl_insert a 4;;
let a = avl_insert a 6;;
let a = avl_insert a 5;;
