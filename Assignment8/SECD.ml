(* Type Declarations for the SECD machine *)
type variable = string

(* Opcode used for the compilation*)
type opcode = LOOKUP of variable | CLOS of (variable * opcode list) | APP | RET | ADD | LDC of int | LD of bool | EQ | AND

type exp = Int of int | Bool of bool | Var of variable | Lambda of variable * exp | Ap of exp * exp | Add of exp * exp | Eq of exp * exp | And of exp * exp
type closure = table * exp
and table = (variable * ans) list
and ans = Int of int | Bool of bool | App of (ans list) | VCL of closure | SC of ans * opcode list

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

(*Here, l is the list of the expressions which need to be evaluated*)
let rec compile l stack = match l with
                  [] -> stack
                  | hd :: tail -> match hd with
                                Var(x) -> compile tail (stack @ [LOOKUP(x)])
                                | Lambda(x,e) -> let c = compile [e] [] in compile tail (stack @ [CLOS(x,c @ [RET])])
                                | Ap (e1,e2) -> let c1 = compile [e1] [] in let c2 = compile [e2] [] in compile tail (stack @ c1 @ c2 @ [APP])
                                | Add (e1,e2) -> let c1 = compile [e1] [] in let c2 = compile [e2] [] in compile tail (stack @ c1 @ c2 @ [ADD])
                                | And (e1,e2) -> let c1 = compile [e1] [] in let c2 = compile [e2] [] in compile tail (stack @ c1 @ c2 @ [AND])
                                | Int i -> compile tail (stack @ [LDC(i)])
                                | Bool b -> compile tail (stack @ [LD(b)])
                                | Eq (e1,e2) -> let c1 = compile [e1] [] in let c2 = compile [e2] [] in compile tail (stack @ c1 @ c2 @ [EQ])

(* Stack Machine evaluates the compiled expression list in a Call by Value manner.*)
let rec stkmc stack gamma (ol:opcode list) = match ol with
                            | hd :: tail -> (match hd with
                                            | LOOKUP(x) -> stkmc ((find gamma x) :: stack) gamma tail
                                            | CLOS(x,cp) -> stkmc ((SC (VCL(gamma, Var(x)), cp)) :: stack) gamma tail
                                            | APP -> (match stack with
                                                    | [] -> raise (BadStack)
                                                    | hd1 :: tail1 -> match tail1 with
                                                                    [] -> raise (BadStack)
                                                                    | SC(VCL(g, Var(x)),cpp) :: tail2 -> let gp = update g hd1 x [] in stkmc ([SC (VCL(gamma,Var(x)), tail)] @ tail2) gp cpp )
                                            | RET -> (match stack with
                                                    | hd1 :: tail1 -> match tail1 with
                                                                    | SC(VCL(g,Var(x)),c) :: tail2 -> stkmc ([hd1] @ tail2) g c
                                                                    | [] -> raise (BadStack)   
                                                                    | x :: xr -> stkmc xr gamma tail
                                                    | [] -> raise (BadStack))
                                            | LDC(n) -> stkmc (Int(n) :: stack) gamma tail 
                                            | LD(b) -> stkmc (Bool(b) :: stack) gamma tail
                                            | ADD -> (match stack with
                                                    | Int(n1) :: Int(n2) :: tail2 -> stkmc (Int(n1+n2) :: tail2) gamma tail
                                                    | _ -> raise (BadStack))
                                            | AND -> (match stack with
                                                    | Bool(b1) :: Bool(b2) :: tail2 -> stkmc (Bool(b1 && b2) :: tail2) gamma tail
                                                    | _ -> raise (BadStack))
                                            | EQ -> (match stack with
                                                    | Int(n1) :: Int(n2) :: tail2 -> stkmc (Bool(n1 = n2) :: tail2) gamma tail)
                                            | _ -> raise (InvalidOpcode))
                            | [] -> match stack with
                                    | hd :: tail -> hd
                                    | [] -> raise (BadStack)

let tble = [("x",Int(2));("y",Int(3));("z",Int(4));("a",Bool(true));("b",Bool(false))]

(* Look Up Test Case *)
(*let expl = [Var("x")]
let expl1 = [Var("a")] *)

(* Arithmetic Operation Test Case*)
(*let expl = [Add (Int(2),Int(3))] 
let expl1 = [Add (Int(2), Var("x"))]
let expl2 = [Add (Var("x"), Var("z"))]
let expl3 = [Eq (Var("x") , Int(2))]
let expl4 = [Eq (Int(2) , Int(3))]
let expl5 = [Eq (Var("y"), Var("z"))]
let expl6 = [Add(Add (Int(2), Var("x")), Add (Var("x"), Var("z")))]
let expl7 = [Add(Add (Int(2), Var("x")), Ap(Lambda ("x",Add(Var("x"),Int(1))),Int(7)))]
let expl12 = [Add(Ap(Lambda ("y",Add(Var("y"),Int(4))),Int(8)), Ap(Lambda ("x",Add(Var("x"),Int(1))),Int(7)))]
let expl8 = [Eq(Ap(Lambda ("y",Add(Var("y"),Int(4))),Int(8)), Ap(Lambda ("x",Add(Var("x"),Int(5))),Int(7)))]
let expl9 = [And (Var("a"),Var("b"))]
let expl10 = [And (Var("a"),Ap(Lambda ("y",And(Var("y"),Var("a"))),Bool(true)))]
let expl11 = [And (Ap(Lambda ("y",And(Var("y"),Var("a"))),Bool(true)),Ap(Lambda ("y",And(Var("y"),Var("a"))),Bool(true)))]*)

(* Abstraction and Application Test Case*)
(*let expl1 = [Ap(Lambda ("x",Add(Var("x"),Int(1))),Int(7))]
let expl2 = [Ap(Lambda ("y",Add(Var("y"),Var("x"))),Int(8))]
let expl3 = [Ap(Lambda ("y",Eq(Var("y"),Var("x"))),Int(8))]
let expl4 = [Ap(Lambda ("y",Eq(Var("y"),Var("x"))),Int(2))]
let expl5 = [Ap(Lambda ("y",And(Var("y"),Var("a"))),Bool(true))]*)

