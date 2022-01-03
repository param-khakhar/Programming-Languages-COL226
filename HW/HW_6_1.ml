exception LeafReached
exception InvalidP

type 'a tree = Leaf | Node of 'a * ('a tree) * ('a tree)

let rec subtree t2 p = match p with
    |[] -> t2
    |x::xr -> match t2 with
            | Leaf -> raise (LeafReached)
            | Node(a,b,c) -> match x with
                            | 1 -> subtree b xr
                            | 2 -> subtree c xr
                            | _ -> raise (InvalidP)

let rec find_path t1 t2 p = if t1 = t2 then [p] else match t1 with
  Leaf -> []
  | Node(x,c1,c2) -> (find_path c1 t2 (p@[0]))@(find_path c2 t2 (p@[1]))

let rec positions t2 t1 c = if t1 = t2 then [c] else match t2 with
  | Node(a0,a1,a2) -> (positions a1 t1 (c@[0])) @ (positions a2 t1 (c@[1]))
  | Leaf -> []

let a = Node(3,Node(2,Node(1,Leaf,Leaf),Node(-1,Leaf,Leaf)), Node(-2,Node(1,Leaf,Leaf),Node(-1,Leaf,Leaf)));;
let b = Node(-1,Leaf,Leaf);;

