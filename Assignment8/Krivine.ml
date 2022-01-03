(* Type Declarations for Krivine Machine *)
type variable = string
type exp = Int of int | Bool of bool | Var of variable | Lambda of variable * exp | Ap of exp * exp | Add of exp * exp | Eq of exp * exp | And of exp * exp
type closure = table * exp
and table = (variable * ans) list
and ans = Int of int | Bool of bool | App of (ans list) | CL of table * exp

exception BadStack
exception NotDefined
exception InvalidOpcode
exception BadTable
exception BadExpression

(* Finds the binding of a variable(expression) contained in the table. *)
let rec find tab v = match tab with
                  | [] -> raise (NotDefined)
                  | hd :: tail -> match hd,v with
                                  | (a,c),b -> if a = b then c else find tail v
                                  | _ -> raise (BadTable)
                                  
(* Function for updating the table. The variable is updated if present or else it would be added.*)
let rec update tab c x res = match tab with
                  | [] -> res @ [(x,c)]
                  | hd :: tail -> match hd with
                                  | (a,b) -> if a = x then res @ [(a,c)] @ tail else update tail c x res @ [hd]  

(* The function represents the evaluation, of the Krivine Machine for the stack *)
let rec k_evaluate st (stack: ans list) = match st with
                | hd :: tail -> (match hd with
                                CL (gamma, expr) -> (match expr with
                                        | Ap(b,c) -> k_evaluate [CL (gamma, b)] (CL (gamma ,c) :: stack)
                                        | Var(v) -> k_evaluate [(find gamma v)] stack
                                        | Lambda(x,e) -> (match stack with
                                                            hd1 :: tail1 -> (match hd1 with
                                                                        | CL (g,c) -> k_evaluate [CL((update gamma hd1 x []) ,e)] stack
                                                                        | _ -> raise (BadExpression))
                                                            | [] -> raise (BadStack))
                                        | Add(e1,e2) -> let Int(c1) = k_evaluate [CL (gamma, e1)] [] in let Int(c2) = k_evaluate [CL (gamma,e2)] [] in k_evaluate ([CL (gamma, Int(c1+c2))]@tail) stack
                                        | And(e1,e2) -> let Bool(c1) = k_evaluate [CL (gamma, e1)] [] in let Bool(c2) = k_evaluate [CL (gamma,e2)] [] in k_evaluate ([CL (gamma, Bool(c1&&c2))]@tail) stack
                                        | Eq (e1,e2) -> let Int(c1) = k_evaluate [CL (gamma, e1)] [] in let Int(c2) = k_evaluate [CL (gamma,e2)] [] in k_evaluate ([CL (gamma, Bool(c1=c2))]@tail) stack
                                        | Int(n) -> k_evaluate tail (Int(n)::stack)
                                        | Bool(b) -> k_evaluate tail (Bool(b)::stack)
                                        | _ -> raise (BadExpression))
                                | _ -> raise (BadStack))
                | [] -> (match stack with
                        hd2 :: tail2 -> hd2
                        | [] -> raise (BadStack))

let g = []
let tble = [("x",CL(g,Int(2)));("y",CL(g,Int(3)));("z",CL(g,Int(4)));("a",CL(g,Bool(true)));("b",CL(g,Bool(false)))]

(*Lookup TestCase*)
(*let expl = [CL(tble,Var("x"))]
let expl1 = [CL(tble,Var("a"))]*)

(* Arithmetic Operation Test Case*)
(*let expl = [CL(tble, Add (Int(2),Int(3)))] 
let expl1 = [CL(tble, Add (Int(2), Var("x")))]
let expl2 = [CL(tble, Add (Var("x"), Var("z")))]
let expl3 = [CL(tble, Eq (Var("x") , Int(2)))]
let expl4 = [CL(tble,Eq (Int(2) , Int(3)))]
let expl5 = [CL(tble,Eq (Var("y"), Var("z")))] 
let expl6 = [CL (tble,Add(Add (Int(2), Var("x")), Add (Var("x"), Var("z"))))]
let expl7 = [CL (tble,Add(Add (Int(2), Var("x")), Ap(Lambda ("x",Add(Var("x"),Int(1))),Int(7))))]
let expl12 = [CL (tble,Add(Ap(Lambda ("y",Add(Var("y"),Int(4))),Int(8)), Ap(Lambda ("x",Add(Var("x"),Int(1))),Int(7))))]
let expl8 = [CL (tble,Eq(Ap(Lambda ("y",Add(Var("y"),Int(4))),Int(8)), Ap(Lambda ("x",Add(Var("x"),Int(5))),Int(7))))]
let expl9 = [CL (tble, And (Var("a"),Var("b")))]
let expl10 = [CL (tble, And (Var("a"),Ap(Lambda ("y",And(Var("y"),Var("a"))),Bool(true))))]
let expl11 = [CL (tble, And (Ap(Lambda ("y",And(Var("y"),Var("a"))),Bool(true)),Ap(Lambda ("y",And(Var("y"),Var("a"))),Bool(true))))] *)

(* Abstraction and Application Test Case*)
(*let expl1 = [CL(tble,Ap(Lambda ("x",Add(Var("x"),Int(1))),Int(7)))]
let expl2 = [CL(tble,Ap(Lambda ("y",Add(Var("y"),Var("x"))),Int(8)))]
let expl3 = [CL(tble,Ap(Lambda ("y",Eq(Var("y"),Var("5"))),Int(8)))]
let expl4 = [CL(tble,Ap(Lambda ("y",Eq(Var("y"),Var("x"))),Int(2)))]
let expl5 = [CL(tble,Ap(Lambda ("y",And(Var("y"),Var("a"))),Bool(true)))]*)

