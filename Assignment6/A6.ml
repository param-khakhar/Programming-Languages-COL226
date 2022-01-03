open List
open Printf
type variable = string
type symbol = string
type term = V of variable | Node of symbol * (term list);;
type clause = term*term list

let get0 (a,_) = a;;
let get1 (_,a) = a;;

let s = [("f",2);("g",2);("a",0);("b",0)];;
let sub1 = [("x",Node ("f",[Node ("a",[]);V "y"]));("y",V "z")];;
let sub2 = [("x",Node ("a",[]));("y",Node ("b",[]));("z",V "y")];;
let t1 = Node ("f",[V "x";V "y"]);;
let t2 = Node ("f",[V "x";Node ("g",[V "y";Node ("a",[])])]);;
let t3 = Node ("f",[V "x";Node ("g",[V "z";Node ("a",[])])]);;

let table = [(Node ("edge",[Node ("a",[]); Node ("b",[])]),[]); (Node ("edge",[Node ("b",[]);Node ("c",[])]),[]); (Node ("edge",[Node ("c",[]); Node ("d",[])]),[]);(Node ("path",[V "A";V "B"]),[Node ("edge",[V "A";V "B"])])];;

exception NOT_UNIFIABLE

(*Function for comparing to tuples*)
let compare a e = if ((get0 a) = (get0 e)) && ((get1 a) = (get1 e)) then true
                  else false;;

(*Function for comparing two elements of the signature i.e checking equality of symbols as well as their arities *)
let compareSym a e = if ((get0 a) = (get0 e)) && ((get1 a) = (length (get1 e))) then true
                     else false;;

(*Inner loop for the comparison of elements and checking the duplicates*)
let rec checkHead s iter n n0 e = if iter == n then true
                                else
                                    if (compare (nth s iter) e) && iter != n0 then false
                                    else checkHead s (iter+1) n0 n e;;

(*The function checks whether the elements of the signature have positive arities and ensures that no duplicates do exist*)
let rec check_sig s iter n = if iter == n then true
                                else
                                    if (checkHead s 0 n iter (nth s iter)) && (get1 (nth s iter) > 0) then check_sig s (iter+1) n
                                    else false;;

(*Checks whether a particular symbol (string*int) is in the signature or not.*)
let rec syminsig s sym iter n = if (iter == n) then false
                                else 
                                   if compareSym (nth s iter) sym then true
                                   else syminsig s sym (iter+1) n;; 

(*Check whether all the terms of a list are present in a signature or not*)
let rec lstinsig s lst = match lst with
                        | [] -> true
                        | hd::tail -> match hd with 
                                | Node (x,y) -> if (syminsig s (x,y) 0 (length s)) && (lstinsig s y) then lstinsig s tail
                                            else false
                                | V v -> lstinsig s tail
                                | _ -> false;;

(*Function checks whether a particular term is valid or not. Here validity is assured by 2 conditions which is
 * presene in the signature and,
 * assuring the aritites are approapriate*)
let rec wfterm s t = match t with
                | V v -> true 
                | Node (x,y) -> if syminsig s (x,y) 0 (length s) && lstinsig s y then true
                           else false;;  

(*Function recursively computes the height of a particular term.*)
let rec ht t maxh = match t with
                | V v -> 1
                | Node (x,y) -> match y with
                                | [] -> maxh
                                | hd :: tail -> let h = ht hd maxh in
                                                if (h+1) > maxh then ht (Node (x,tail)) (1+h)
                                                else ht (Node (x,tail)) maxh;;

(*Function calculates the size of a particular term in the tree i.e it counts the total number of instances where Node is used*)
let rec size t total = match t with
                | V v -> total+1
                | Node (x,y) -> match y with
                                | [] -> (total+1)
                                | hd :: tail -> let one = size hd 0 in
                                                size (Node (x,tail)) (total+one);;

(*Utility function for vars for the computation of union*)
let rec notIn res v = match res with
                        | [] -> true
                        | hd :: tail -> if hd = v then false
                                        else notIn tail v

(*The function vars returns the list containing the variables (strings) present in a particular term*)
let rec vars t res = match t with
                        | V v -> if notIn res v then append res [v]
                                 else res
                        | Node (x,y) -> match y with
                                        | [] -> res
                                        | hd :: tail -> let v = vars hd [] in
                                        vars (Node (x,tail)) (append res v);;

(*Substitutions can be regarded as a list of variable*term. The approapriate substitutions can be performed then*)
type substitutions = variable * term list

(*The function checku checks the presence of the variable v in the substitution sub*)
let rec checku v sub = match sub with
                        | [] -> false
                        | hd :: tail -> if (get0 hd) = v then true
                                        else checku v tail;;

(*The function check is filter which filters out the recurrent variables in sub2 which are also present in sub1 and returns their concatenation*)
let rec check sub1 sub2 res  = match sub2 with
                        | [] -> res
                        | hd :: tail -> if checku (get0 hd) sub1 then check sub1 tail res
                                        else check sub1 tail (append res [hd]);;

(*The function subsiter performs a retrieval of the term for a particular variable for a particular substitution sub*)
let rec substiter v s = match s with
                        | [] -> V v
                        | hd :: tail -> if (get0 hd) = v then get1 hd
                        else substiter v tail;;

(*The function subst performs a substitution on the term t according to the given substitution s*)
let rec subst s t = match t with
                        | V v -> substiter v s 
                        | Node (x,y) -> match y with 
                                        | [] -> Node (x,y)
                                        | hd :: tail -> let left = subst s hd in
                                                        let right = subst s (Node (x,tail)) in
                                                        match right with
                                                        | Node (m,n) -> Node (x,(append [left] n));;

(*The function composeu again filters out substitutions of the form ["v",V "v"] from the list total *)
let rec composeu total res = match total with
                        | [] -> res
                        | hd :: tail -> match hd with
                                        | v1,V v2 -> if v1 = v2 then composeu tail res
                                                     else composeu tail (append res [hd])
                                        | _ -> composeu tail (append res [hd]);;

(*The function compose takes 2 substitutions as inputs and performs a composition and then returns the composed substitution incompletely.*)
let rec compose sub1 sub2 res = match sub1 with
                        | [] -> composeu res [] 
                        | hd :: tail -> let temp = subst sub2 (get1 hd) in
                                        compose tail sub2 (append res [((get0 hd),temp)]);;

(*The function composec completes the incomplete step of the function compose by making a call to check which removes redundant entries*)
let rec composec sub1 sub2 res = append (compose sub1 sub2 res) (check sub1 sub2 [])

let rec checkv1u v1 lst = match lst with
                        | [] -> false
                        | hd :: tail -> if hd = v1 then true
                                        else checkv1u v1 tail;;

(*Given that y is a term list, the function checkv1 checks the presence of the variable v1 in that list with the help of checkv1u*)
let rec checkv1 v1 y = match y with
                        | [] -> false
                        | hd :: tail -> if checkv1u v1 (vars hd []) then true
                                        else checkv1 v1 tail;;

(*The function mgu computes the most general unifier for the terms t1 and t2. It uses the function sigiter in order to perform an iteration 
 * on the terms in the term list of a particular symbol*)
let rec mgu t1 t2 = match t1,t2 with
                        | V v1,V v2 -> if v1 = v2 then [] 
                                       else [(v1,(V v2))]
                        | V v1,Node (x,[]) -> [(v1,Node (x,[]))]
                        | Node (x,[]), V v1 -> [(v1, Node (x,[]))]
                        | Node (x,[]),Node (y,[]) -> if x = y then [] 
                                                     else raise (NOT_UNIFIABLE)
                        | V v1, Node (x,y) -> if checkv1 v1 y then raise (NOT_UNIFIABLE) 
                                              else [(v1,Node (x,y))]
                        | Node (x,y), V v1 -> if checkv1 v1 y then raise (NOT_UNIFIABLE)
                                              else [(v1,Node (x,y))]
                        | Node (a,[]), Node (b,c) -> raise (NOT_UNIFIABLE)
                        | Node (b,c), Node (a,[]) -> raise (NOT_UNIFIABLE)
                        | Node (a,b), Node (c,d) -> if a = c then sigiter b d 0 (length b) []
                                                    else raise (NOT_UNIFIABLE)

and sigiter tau1 tau2 iter k comp = if iter = k then comp
                                       else let sub1 = subst comp (nth tau1 iter) in
                                            let sub2 = subst comp (nth tau2 iter) in
                                            let nr = mgu sub1 sub2 in
                                            sigiter tau1 tau2 (iter+1) k (composec comp nr [])

(*Function matches all the heads and the predicates available in the knowledge base with those present in the query, and returns the list of all suitable matches*)
let rec findC (query:term) table res = match table with
                        | [] -> res
                        | hd :: tail -> match get0(hd),query with
                                        | Node(a,b),Node(c,d) -> if a = c then
                                                if (length b) = (length d) then findC query tail (append res [hd])
                                                                    else findC query tail res
                                                                 else findC query tail res;;
(*Utility function for printing the clauses*)
let rec printClauses clauses = match clauses with
                        | [] -> ()
                        | hd :: tail -> match get0(hd) with
                                        | Node(a,b) -> printf "Node %s\n" a ;flush stdout;printClauses tail
;;

(*Utility functions for printing a term*)
let rec printTerm term = match term with
                        | Node(a,b) -> printf "C:%s\n" a;printTermlst b
                        | V a -> printf "V:%s\n" a

and printTermlst termlist = match termlist with
                        | hd :: tail -> printTerm hd; printTermlst tail
                        | [] -> ();;

(*Executes resolve on each of the query in the list queries*)
let rec resolves queries table res bt = match queries with
                        | [] -> (true,res)
                        | hd :: tail -> let c = findC hd table [] in
                        let r = resolve res (subst res hd) c table tail in
                        if get0(r) then resolves tail table (append res (get1(r))) bt
                        else (false,(get1(r)))

(*Function for resolving the user input queries, uses mgu (unification as well)*)
and resolve sub query clauses table bt = match clauses with
                        | [] -> (false,sub)
                        | hd :: tail -> match (mgu (get0(hd)) query) with
                                        | exception (NOT_UNIFIABLE) -> resolve sub query tail table bt
                                        | [] -> (true,(append sub (mgu (get0(hd)) query)))
                                        | hd1::tail1 -> match (get1(hd)) with
                                                        | x :: xs -> let r = resolves (get1(hd)) table (append sub (mgu (get0(hd)) query)) bt in
                                                                     if (get0(r)) then r
                                                                     else resolve sub query tail table bt
                                                        | [] -> let r = resolves bt table (append sub (mgu (get0(hd)) query)) [] in
                                                                if (get0(r)) then r
                                                                else resolve sub query tail table bt
;;

let rec printS sub = match sub with
                        | (v,Node (a,[])) -> printf "%s= " v;flush stdout;printf "%s" a;flush stdout
;;

let rec printV sub v = match sub with
                        | [] -> ()
                        | hd :: tail -> if (get0(hd) = v) then (printS hd)
                                        else printV tail v
;;

let rec printVars sub vs = match vs with
                        | [] -> printf" "
                        | hd :: tail -> printV sub hd; if ((length tail) > 0) then (printf",\n"; printVars sub tail) else printVars sub tail
;;

(*Function for resolving the subgoals which contain variables in them. Non-ground queries*)
let rec resolvesx (queries:term list) (table:((term*term list) list)) res vs bt = match queries with
                        | [] -> (true,res)                        
                        | hd :: tail -> let c = findC hd table [] in
                        let r = resolvex res (subst res hd) c table vs tail in
                        if get0(r) then (true,[])
                        else (false,[])

(*Function for resolving the user input queries which are terms containing variables.*)
and resolvex sub query clauses table vs bt = match clauses with
                        | [] -> (true,[])
                        | hd :: tail -> match (mgu (get0(hd)) query) with
                                    | exception (NOT_UNIFIABLE) -> resolvex sub query tail table vs bt
                                    | hd1::tail1 -> match get1(hd) with
                                                    | x::xs -> let r = resolvesx (get1(hd)) table (append sub (mgu (get0(hd)) query)) vs bt in
                                                            if get0(r) then resolvex sub query tail table vs bt
                                                            else (false,[])
                                                    | [] -> match bt with
                                                            | [] -> (printVars (append sub (mgu (get0(hd)) query)) vs; let sym = read_line() in
                                                                    match sym with
                                                                            |";" -> resolvex sub query tail table vs bt
                                                                            |"." -> (false,[])
                                                                            | _ -> (false,[]))
                                                            | y :: ys -> (let r = resolvesx bt table (append sub (mgu (get0(hd)) query)) vs bt in
                                                                    if (get0(r)) then resolvex sub query tail table vs bt else (false,[]) )
                                    | [] ->  printVars sub vs;let sym = read_line() in
                                             match sym with
                                                |";" -> (true,[])
                                                |"." -> (false,[])
                                                | _ -> (false,[])
                                        
;;
