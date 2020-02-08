(* Assignment-1, solving simultaneous Linear equations using Gaussian Elimination *)


(* The following contains the code for solving a system of simultaneous linear equations by using Gaussian Elimination. 
 * Transformations are carried to convert a matrix to the row-reduced echelon form and the product of diagonal elements would
 * be the determinant of the matrix. For calculating the inverse of the matrix, and identity matrix is augmented and reduction
 * operations are performed on the augmented matrix until the matrix is reduced to an identity matrix and then the augmented matrix is 
 * retrieved by using funtions and filteri to yield the inverse matrix.*)

open List;;

exception InvalidInput
exception UnequalVectorSize
exception UnequalMatrixShape
exception IncompatibleMatrixShape
exception SingularMatrix
exception RectangularMatrix

(* Declaring vector as a float of list with constructor Vect *)
type vector = float list;;

(* Simply returns the length of the list which was used to make the matrix *)
let rec vdim (v:vector) :int = match v with
                | [] -> 0
                | hd::tail -> 1 + vdim tail;;

(* Use to make an n dimensional 0-vector *)
let rec mkzerol (n:int) (res:vector) : vector = if n = 0 then res
                                else mkzerol (n-1) (append res [0.0]);;

(* Appends two vectors by using the function List.append for the two lists used for constructing the vector *)
let vecAppend (v1:vector) (v2:vector) : vector = append v1 v2;; 
                        
(* Uses mkzerol to construct a new 0-vector *)
let rec mkzerov (n:int): vector =  if n < 0 then raise (InvalidInput) 
                                        else mkzerol n [];; 

(* Checks whether a float list is a zero list or not.*)
let rec iszerov (v:vector) : bool = match v with
                | [] -> true
                | hd::tail -> if hd = 0. then iszerov tail
                                else false;;

(* Function to add two vectors.Recursice implementation to yield the resulting vector*)
let rec addv (v1:vector) (v2:vector) : vector = match v1,v2 with
                        | [],[] -> []
                        | hd :: tail, [] -> raise (UnequalVectorSize)
                        | [] , hd::tail -> raise (UnequalVectorSize)
                        | hd1::tail1,hd2::tail2 -> append [hd1 +. hd2] (addv tail1 tail2);;

(* Function to multiply a vector with scalar. Recursive implementation to produce resulting vector *)
let rec scalarmultv (c:float) (v:vector) : vector = match v with
                        | [] -> []
                        | hd::tail -> append [hd*.c] (scalarmultv c tail);;

(* Computes the dot products of two vectors. Recursive implementation to achieve the desired purpose. *)
let rec dotprodv (v1:vector) (v2:vector) : float = match v1,v2 with
                        | [],[] -> 0.0
                        | hd1::tail1 , [] -> raise (UnequalVectorSize)
                        | [] , hd2::tail2 -> raise (UnequalVectorSize)
                        | hd1::tail1,hd2::tail2 -> hd1*.hd2 +. dotprodv tail1 tail2;;

(* Computes the cross product of 2 3d vectors *)
let rec crossprodv (v1:vector) (v2:vector) :vector = if ((length v1) = 3) && ((length v2) = 3) then
                      let a1 = nth v1 0 in
                      let b1 = nth v1 1 in
                      let c1 = nth v1 2 in
                      let a2 = nth v2 0 in
                      let b2 = nth v2 1 in
                      let c2 = nth v2 2 in
                      [(b1*.c2 -. b2*.c1);(c1*.a2 -. a1*.c2);(a1*.b2 -. a2*.b1)]
                        else
                                raise (InvalidInput);;

type matrix = float list list;;

(*Returns the dimensions of the matrix*)
let rec mdim (m:matrix) : int * int =  length m, length (nth m 0);;

(* Checks whether the matrix is a zeroMatrix or not *)
let rec iszerom (m:matrix) : bool = match m with
  		        | [] -> true
  			| hd::tail -> if iszerov hd then iszerom tail
  					else false;;

(* Auxiliary function for making a zero matrix*)
let rec mkzero2dl size iter res = if iter = size then res
                                        else mkzero2dl size (iter+1) (append res [mkzerov size]);;

(* Function for making a zero matrix of a given dimesion *)
let rec mkzerom (m_:int) : matrix = if m_ > 0 then  mkzero2dl m_ 0 []
                                        else raise (InvalidInput);;

(* Makes a unit vector of provided dimension *)
let rec mkunitv (n:int) (p:int) (l:vector) : vector = if n = 0 then append l []
                        else if n = p then mkunitv (n-1) p (append l [1.0])
                        else mkunitv (n-1) p (append l [0.0]);;

(* Auxiliary function for making the unit matrix *)
let rec mkunit2dl (n : int) (pos : int) (l : float list list)  = if pos = 1 then append l [mkunitv n pos []]
                                                                 else mkunit2dl n (pos-1) (append l [mkunitv n pos []]);; 

(* Function to make a unit matrix *)
let rec mkunitm  (m_:int) : matrix = if m_ < 0 then raise (InvalidInput) else mkunit2dl m_ m_ [];;

(* Function checks whether the vector is a unit vector or not *)
let rec isunitl n p l res = match l with
                        | [] -> res
                        | hd::tail -> if (n = p && hd = 1.0) then isunitl (n-1) p tail true
                                      else if hd = 0.0 then isunitl (n-1) p tail res
                                      else false;;
(* Auxiliary function for checking for the unit matrix *)
let rec isunit2dl n p l = match l with
                                | [] -> true
                                | hd::tail -> if (isunitl n p hd false) then isunit2dl n (p-1) tail
                                                else false;;

(* Function to check whether a matrix is unit matrix or not *)
let rec isunitm (m:matrix) : bool =  isunit2dl (length m) (length m) m;;

(* Auxiliary function for adding two matrices, a tail recursive implementation *)
let rec sum2dl (m:matrix) (n:matrix) res = match m,n with
                        | [],[] -> res
                        | hd1::tail1,[] -> raise (UnequalMatrixShape)
                        | [],hd2::tail2 -> raise (UnequalMatrixShape)
                        | hd1::tail1, hd2::tail2 -> sum2dl tail1 tail2 (append res [addv hd1 hd2]);;

(* Function to add two matrices to produce a third matrix *)
let rec addm (m:matrix) (n:matrix) : matrix = if (length m) = (length n) then sum2dl m n []
                                                else raise (UnequalMatrixShape);;

(* Auxiliary function for perform a multiplication of a matrix by a scalar *)
let rec mult2dList n m res = match m with
                        | [] -> res
                        | hd::tail -> mult2dList n tail (append res [scalarmultv n hd]);;

(* Function to multiply a float with a matrix *)
let rec scalarmultm (n:float) (m:matrix) : matrix = mult2dList n m [];;

(* Function to return the ith columng of a matrix implmented by a tail-recursive approach *)
let rec column i m res = match m with
                        | [] -> res
                        | hd::tail -> column i tail (append res [(nth hd i)]);; 

(*m is vector, n is matrix and res is accumulator*)
let rec multVecMatrix iter m n res = if iter = (length (nth n 0)) then res
                                        else multVecMatrix (iter+1) m n (append res [dotprodv m (column iter n [])]);;
                                
(* Iterating over the rows of the matrix m1 *)
let rec mult2dList m1 m2 res = match m1 with 
                        | [] -> res
                        | hd::tail -> mult2dList tail m2 (append res [(multVecMatrix 0 hd m2 [])]);;

(* For multiplying two matrices *)
let rec multm (m:matrix) (n:matrix) : matrix = if (length (nth m 0)) = (length (n)) then mult2dList m n []
                                                else raise (IncompatibleMatrixShape)

(*Calculate transpose of a 2DList *)
let rec transpose iter m res = if iter = (length (nth m 0)) then res
                                else transpose (iter+1) m (append res [(column iter m [])]);;

(*Calculating trasnpose of a matrix*)                              
let rec transm (m:matrix) : matrix = transpose 0 m [];;

(* Function to calculate the product of the diagonal elements *)
let rec diag m res iter scale n = if iter = n then res*.scale
                                else diag m (res*.(nth (nth m iter) iter)) (iter+1) scale n;; 

(*Scales the elements of a row *)
(*r-> float list
 * iter-> index for iterating over float list
 * n -> size of float list
 * res -> result *)
let rec scalerow (r:float list) (iter:int) (n:int) (res:float list) (scale:float) : float list*float = if iter = n then 
        if res = [] then r,scale
         else res,scale
     else if (nth r iter) = 0.0 then 
            scalerow r (iter+1) n (append res []) scale
      else append res (scalarmultv (1.0/.(nth r iter)) r), scale*.(nth r iter);;


(* Function which scales the elements of a vector by and makes the first non-zero element 1. Used as a utility for reducing the row *)
let rec scalemat (m:float list list) (iter:int) (row:int) (n:int) (res:float list list) (scale:float) : float list list*float = if iter = n then res,scale
                                        else if iter = row then 
                                                let get01 ((a,_):float list * float) : float list = a in
                                                let get11 ((_,a):float list * float) : float = a in
                                                let d = scalerow (nth m iter) 0 n [] scale in
                                                scalemat m (iter+1) row n (append res [(get01 (d))]) (get11 (d))
                                        else scalemat m (iter+1) row n (append res [(nth m iter)]) scale;;

(*Swap row1 and row2 and return the updated matrix.*)
let rec exchange m row1 row2 iter store res = if iter = length m then res
                                                else if iter = row2 then exchange m row1 row2 (iter+1) (append store (nth m iter)) (append res [nth m row1])
                                                else if iter = row1 then exchange m row1 row2 (iter+1) store (append res [store])
                                                else exchange m row1 row2 (iter+1) store (append res [nth m iter]);;

(*Function swaps the error-prone row with a new row if found and returns the result accordingly *)
let rec shift m row1 (row2:int) iter res scale = if iter = length(m) then if res = [] then raise (SingularMatrix)
                                        else res
                                else if (iter > row2) && not ((nth (nth m iter) row2) = 0.0) then exchange m iter row2 0 [] []
                                else shift m row1 row2 (iter+1) res scale;;

(*This function performs subtraction over the two rows and returns the updated matrix*)
let rec subtrow m iter n row1 row2 res scale = if iter = n then res,scale
                                               else if iter = row2 then
                                                         if (nth (nth m iter) row1) = 0.0 then subtrow m (iter+1) n row1 row2 (append res [(nth m iter)]) scale
                                                         else let m1 = addv (nth m row1) (scalarmultv (-1.0/.(nth (nth m iter) row1)) (nth m iter)) in

                                                                if (nth m1 iter) = 0.0 then 
                                                                        let m2 = shift m row1 row2 0 [] (scale*.(-1.0)) in subtrow m2 (iter+1) n row1 row2
                                                                 (append res [addv (nth m2 row1) (scalarmultv (-1.0/.(nth (nth m2 iter) row1)) (nth m2 iter))]) (scale*.(-
                                                                1.0 *. (nth (nth m2 iter) row1) *.(-1.0)))
                                                                 else 
      subtrow m (iter+1) n row1 row2 (append res [addv (nth m row1) (scalarmultv (-1.0/.(nth (nth m iter) row1)) (nth m iter))]) (scale*.(-1.0 *. (nth (nth m iter) row1)))

                else subtrow m (iter+1) n row1 row2 (append res [(nth m iter)]) scale;;

(* Here m is matrix. The function iterates over the rows and calls the function subtrow which subtracts the rows along with updations to the scale.
 * A particular row is reduced by all the rows above it. *)
let rec subtmatrix m iter row n scale= if iter >= row then m,scale
                                      else 
                                              let get01 (a,_) = a in
                                              let get11 (_,a) = a in
                                              let w = subtrow m 0 n iter row [] scale in
                                              subtmatrix (get01 (w)) (iter+1) row n (get11 (w));;
                                      
(* Function for making the m[i][i] = 1 and m[i][0],m[i][1],m[i][2],....m[i][i-1] = 0. This calls the function subtmatrix (to perform subtractions)
 * Followed by scalemat which makes the first non-zero coefficient 1.0. This function the reduce the row iter in the matrix  *)
let rec reduce (m:float list list) (iter:int) (scale:float) (n:int) : float list list*float = if iter = n then m,scale
                                else 
                                        let get01 (a,_) = a in
                                        let get11 (_,a) = a in 
                                        let c = subtmatrix m 0 iter n scale in
                                        let d = scalemat (get01 (c)) 0 iter n [] (get11 (c)) 
                                        in
                                        reduce (get01 (d)) (iter+1) (get11 (d)) n;;
 
(*Calculating determinant of a matrix*)
let rec detm (m:matrix) : float = if (length m) = (length (nth m 0)) then let get01 (a,_) = a in 
             let get11 (_,a) = a in 
             let r = reduce m 0 1.0 (length m)
             in
             diag (get01 (r)) 1.0 0 (get11 (r)) (length m)
                else raise (RectangularMatrix);;

(*Function for augmenting an identity matrix with a unit matrix by a tail-recursive implementation.*)
let rec invutil m n res = match m,n with
                        | [],[] -> res
                        | hd1::tail1, hd2::tail2 -> invutil tail1 tail2 (append res [(append hd1 hd2)])
                        | [],hd::tail -> raise (UnequalMatrixShape)
                        | hd::tail, [] -> raise (UnequalMatrixShape);;

(*Subtract row2 from row1 and return a list*)
let subtrow2 (row1:float list) row2 n = let r = scalarmultv ((nth row1 n)*.(-1.0)) row2 in 
                                addv row1 r;;

(*Subtract row2 from row1 and return a new matrix. We start from the*)
let rec subtmatrix2 m iter row2 row1 (res:float list list) = if iter = length m then res
                                                else if iter = row1 then subtmatrix2 m (iter+1) row2 row1 (append res [(subtrow2 (nth m row1) (nth m row2) row2)])
                                                else subtmatrix2 m (iter+1) row2 row1 (append res [nth m iter]);;

(*Perform further reduction to yield an identity matrix. The function below represents the inner-loop of the nested loop structure. We subtract row2 from all the rows above it.*)
let rec reduce2 m iter row = if iter = row then m
                                else reduce2 (subtmatrix2 m 0 row iter []) (iter+1) row;;

(*Outer-loop for performing further reduction of the matrix to form an identity matrix*)
let rec further m row = if row = 0 then m 
                                else further (reduce2 m 0 row) (row-1);; 

(*Auxiliary function for retrieving the inverse of matrix. The inner loop which scans elements of a list and appends elements with indices >= n.*)
let rec filteri row iter n res = match row with
                                | [] -> res
                                | hd::tail -> if iter < n then filteri tail (iter+1) n res
                                                 else filteri tail (iter+1) n (append res [hd])

(*Function for retrieving the inverse after making row reductions and transformation. *)
let rec filter m iter res = let n = (length m) in
                                if iter = n then res
                                else  filter m (iter+1) (append res [(filteri (nth m iter) 0 n [])]);; 

(*Function for calculating the inverse of a matrix*)
let rec invm (m:matrix) : matrix = 
                        if (length m) = (length (nth m 0)) then
                                let get01 (a,_) = a in
                                let r = reduce (invutil m (mkunit2dl (length m) (length m) []) []) 0 1.0 (length m)
                                in 
                                filter (further (get01(r)) ((length (get01 (r)))-1)) 0 []
                        else raise(RectangularMatrix);;

