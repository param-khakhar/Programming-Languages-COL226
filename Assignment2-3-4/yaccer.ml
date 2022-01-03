type token =
  | FLOAT of (float)
  | LPARENTHESIS
  | RPARENTHESIS
  | COMMA
  | COLON
  | LBRACKET
  | RBRACKET
  | FTERM
  | NL
  | ASSIGN
  | INT of (int)
  | UOP of (string)
  | BOP of (string)

open Parsing;;
let _ = parse_error;;
# 2 "yaccer.mly"
    open Printf
    open List
    let parse_error s = 
            print_endline s;
            flush stdout
    exception InvalidInput
    exception EmptyCell
    type sheet = float list list
    type range = (int*int)*(int*int)
    type index = int*int

    let sheet = [];;
    let m = 3;;
    let n = 3;;

    let rec printl l = match l with
        [] -> printf "\n"
        | hd::tail -> if hd == nan then printf "  " else print_float hd; print_string " "; printl tail

    let rec print_list = function 
        [] -> ()
        | e::l -> print_string e ; print_string " "; print_list l

    let rec printm (s:sheet)  = match s with
        | [] -> ()
        | hd::tail -> printl hd;printm tail;;

    let rec conv l res = match l with
                        | [] -> res
                        | hd::tail -> if hd = "" then conv tail (append res [nan])
                        else conv tail (append res [float_of_string hd])

    let rec inp in_stream m n iter res = if (iter == m) then res
                                        else 
                                            let line = input_line in_stream in
                                            let split = Str.split (Str.regexp ",") in
                                            let values = split line in
                                            let con = conv values [] in
                                            inp in_stream m n (iter+1) (append res [con])
let get01 (a,_) = a;;
let get11 (_,a) = a;;
let sheet = inp (open_in Sys.argv.(1)) (int_of_string(Sys.argv.(2))) (int_of_string(Sys.argv.(3))) 0 [];;

(*Functions for printing a 2D List*)
let string_of_float_list l = String.concat " " (List.map string_of_float l)

let float_list_to_string l = String.concat "\n" (List.map string_of_float_list l)

(*Utility functions for full_count*)

(*full_countr creates a new matrix and stores the already counted valid variables at the desired index*)
let rec full_countr (s:sheet) res ir1 ic1 row iter m count = if iter == m then res
                                                             else
                                                                 if row == ir1 && iter == ic1 then
                                                                     full_countr s (append res [count]) ir1 ic1 row (iter+1) m count
                                                                 else
                                                                     full_countr s (append res [(nth (nth s row) iter)]) ir1 ic1 row (iter+1) m count;;                      

(*countr computes the total number of valid elements of a row*)
let rec countr (s:sheet) r1 c1 r2 c2 row iter m count = if iter == m then count
                                                        else
                                                                if row >= r1 && row <= r2 && iter >= c1 && iter <= c2 && (nth (nth s row) iter) != nan then
                                                                        countr s r1 c1 r2 c2 row (iter+1) m (count +. 1.0)
                                                                else
                                                                        countr s r1 c1 r2 c2 row (iter+1) m count

(*countm stores the total number of valid elements of a matrix*)
let rec countm (s:sheet) r1 c1 r2 c2 iter n count = if iter == n then count
                                                    else
                                                        let rc = countr s r1 c1 r2 c2 iter 0 (length (nth s iter)) count in
                                                        countm s r1 c1 r2 c2 (iter+1) n rc;; 

(* full_countm appends the new rows computed by full_countr to the matrix *)
let rec full_countm (s:sheet) res ir1 ic1 iter n count = if iter == n then res
                                                                else
                                                                    let r = full_countr s [] ir1 ic1 iter 0 (length (nth s iter)) count in
                                                                    full_countm s (append res [r]) ir1 ic1 (iter+1) n count;;

(*Returns a new sheet containing the count of valid elements stored at indexes with the help of utility functions
 *If the range is out *)
let rec full_count (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                           let c1 = get11(get01(r)) in
                                                           let r2 = get01(get11(r)) in
                                                           let c2 = get11(get11(r)) in
                                                           if r1 <= r2 && c1 <= c2 then
                                                                let ir1 = get01(i) in
                                                                let ic1 = get11(i) in
                                                                let tc = countm s r1 c1 r2 c2 0 (length s) 0.0 in
                                                                full_countm s [] ir1 ic1 0 (length s) tc 
                                                           else
                                                                raise (InvalidInput);;       

(*Utility functions for row_count*)

(*row_countr creates a new matrix and stores the already counted valid variables at the desired indices. It appends column containing count
 * of the valid entries of the corresponding row.*)
let rec row_countr (s:sheet) res ir1 ic1 nr1 nc1 row iter m count = if iter == m then res
                                                             else
                                                                 if row >= ir1 && row < nr1 && iter == ic1 then
                                                                     row_countr s (append res [(nth count (row-ir1))]) ir1 ic1 nr1 nc1 row (iter+1) m count
                                                                 else
                                                                     row_countr s (append res [(nth (nth s row) iter)]) ir1 ic1 nr1 nc1 row (iter+1) m count;;          

(*rcountr computes the total number of valid elements in each row. It is a tail-recursive iteration on the elements of a row.*)
let rec rcountr (s:sheet) c1 c2 row iter m count = if iter == m then count
                                                         else
                                                                if iter >= c1 && iter <= c2 then
                                                                       if (nth (nth s row) iter) == nan then rcountr s c1 c2 row (iter+1) m count
                                                                       else rcountr s c1 c2 row (iter+1) m (count+.1.0)
                                                                else
                                                                       rcountr s c1 c2 row (iter+1) m count

(*rcountm stores the total number of valid elements for each row in the list count. It is an iterative procedure for each and every row.*)
let rec rcountm (s:sheet) c1 c2 iter n count = if iter == n then count
                                                   else
                                                       let rc = rcountr s c1 c2 iter 0 (length (nth s iter)) 0.0 in
                                                       rcountm s c1 c2 (iter+1) n (append count [rc]);; 

(* row_countm appends the new rows computed by row_countr to the sheet. It is an iterative procedure and appends a new row at every iteration.*)
let rec row_countm (s:sheet) res ir1 ic1 nr1 nc1 iter n count = if iter == n then res
                                                                else
                                                                    let r = row_countr s [] ir1 ic1 nr1 nc1 iter 0 (length (nth s iter)) count in
                                                                    row_countm s (append res [r]) ir1 ic1 nr1 nc1 (iter+1) n count;;

(* row_count is the function which appends a column containing the respective number of valid entries in the range corresponding to each row*)
let rec row_count (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = rcountm s c1 c2 r1 (r2+1) [] in
                                                               row_countm s [] ir1 ic1 (ir1 + (length tc)) (ic1 + 1) 0 (length s) tc 
                                                          else
                                                               raise (InvalidInput);;

(*Utility functions for col_count*)
(*col_countr iterates over the rows and forms the new sheet. It also incorporates the newly computed 2d sheet count according to the indices.*)
let rec col_countr (s:sheet) res ir1 ic1 nr1 nc1 row iter m count = if iter == m then res
                                                             else
                                                                 if iter >= ic1 && iter < nc1 && row == ir1 then
                                                                     col_countr s (append res [(nth count (iter-ic1))]) ir1 ic1 nr1 nc1 row (iter+1) m count
                                                                 else
                                                                     col_countr s (append res [(nth (nth s row) iter)]) ir1 ic1 nr1 nc1 row (iter+1) m count;;          

(*ccountr is a tail-recursive procedure which iterates over each element of a column in the sheet. And updates the variable count accordingly.*)
let rec ccountr (s:sheet) r1 r2 col iter n count = if iter == n then count
                                                         else
                                                                if iter >= r1 && iter <= r2 then
                                                                       if (nth (nth s iter) col) == nan then ccountr s r1 r2 col (iter+1) n count 
                                                                       else ccountr s r1 r2 col (iter+1) n (count+.1.0)
                                                                else
                                                                       ccountr s r1 r2 col (iter+1) n count

(*The function ccountm is a tail-recursive procedure which iterates over each and every column of the matrix and appends the number of valid entries for each column
 * in the list count*)
let rec ccountm (s:sheet) r1 r2 iter m count = if iter == m then count
                                                   else
                                                       let rc = ccountr s r1 r2 iter 0 (length  s) 0.0 in
                                                       ccountm s r1 r2 (iter+1) m (append count [rc]);; 

(*The function col_countm forms the new matrix row-wise and and it uses the function col_countr to get the row*)
let rec col_countm (s:sheet) res ir1 ic1 nr1 nc1 iter n count = if iter == n then res
                                                                else
                                                                    let r = col_countr s [] ir1 ic1 nr1 nc1 iter 0 (length (nth s iter)) count in
                                                                    col_countm s (append res [r]) ir1 ic1 nr1 nc1 (iter+1) n count;;

(*The function col_count returns the sheet with the count of each column in the specified range*)
let rec col_count (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = ccountm s r1 r2 c1 (c2+1) [] in
                                                               col_countm s [] ir1 ic1 (ir1 + 1) (ic1 + (length tc)) 0 (length s) tc 
                                                          else
                                                               raise (InvalidInput);;

(*Utility functions for sum_count*)
(*scountr computes sum of total number of valid elements of a row. The variable count is updated during each iteration.*)
let rec scountr (s:sheet) r1 c1 r2 c2 row iter m count = if iter == m then count
                                                        else
                                                                if row >= r1 && row <= r2 && iter >= c1 && iter <= c2 then
                                                                        if (nth (nth s row ) iter) == nan then raise (EmptyCell)
                                                                        else
                                                                            scountr s r1 c1 r2 c2 row (iter+1) m (count+.(nth (nth s row) iter))
                                                                else
                                                                        scountr s r1 c1 r2 c2 row (iter+1) m count

(*It is a tail recursive function which iterates over the rows of the matrix.*)
let rec scountm (s:sheet) r1 c1 r2 c2 iter n count = if iter == n then count
                                                    else
                                                        let rc = scountr s r1 c1 r2 c2 iter 0 (length (nth s iter)) count in
                                                        scountm s r1 c1 r2 c2 (iter+1) n rc;; 

(*Returns a new sheet containing the sum of valid elements stored at desired index with the help of utility functions, uses earlier declared functions full_countm for
 * achieving the desired purpose.*)
let rec full_sum (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = scountm s r1 c1 r2 c2 0 (length s) 0.0 in
                                                               full_countm s [] ir1 ic1 0 (length s) tc 
                                                          else
                                                               raise (InvalidInput);;

(*srcountr computes the total of the valid elements of each row i.e within the columns c1 and c2*)
let rec srcountr (s:sheet) c1 c2 row iter m count = if iter == m then count
                                                         else
                                                                if iter >= c1 && iter <= c2 then
                                                                        if (nth (nth s row) iter) == nan then raise (EmptyCell)
                                                                        else
                                                                        srcountr s c1 c2 row (iter+1) m (count+.(nth (nth s row) iter))
                                                                else
                                                                       srcountr s c1 c2 row (iter+1) m count

(*srcountm stores the total of valid elements for each row in the list count*)
let rec srcountm (s:sheet) c1 c2 iter n count = if iter == n then count
                                                   else
                                                       let rc = srcountr s c1 c2 iter 0 (length (nth s iter)) 0.0 in
                                                       srcountm s c1 c2 (iter+1) n (append count [rc]);; 

(*Function for computing sum of the valid elements of each row in the spreadsheet. Uses earlier defined utility functions row_countm*)
let rec row_sum (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = srcountm s c1 c2 r1 (r2+1) [] in
                                                               row_countm s [] ir1 ic1 (ir1+length(tc)) (ic1+1) 0 (length s) tc  
                                                          else
                                                               raise (InvalidInput);;

(*sccountr computes the total number of valid elements in each column i.e between the rows r1 and r2.*)
let rec sccountr (s:sheet) r1 r2 col iter n count = if iter == n then count
                                                         else
                                                                if iter >= r1 && iter <= r2 then
                                                                        if (nth (nth s iter) col) == nan then raise (EmptyCell)
                                                                        else
                                                                            sccountr s r1 r2 col (iter+1) n (count+.(nth (nth s iter) col))
                                                                else
                                                                      sccountr s r1 r2 col (iter+1) n count

(*sccountm stores the total number of valid elements for each column in the list count*)
let rec sccountm (s:sheet) r1 r2 iter n count = if iter == n then count
                                                   else
                                                       let rc = sccountr s r1 r2 iter 0 (length s) 0.0 in
                                                       sccountm s r1 r2 (iter+1) n (append count [rc]);; 

(* col_sum is the function which appends a row containing the total of valid entries in the range corresponding to each column
 * in the range r. It uses the earlier defined function col_countm to form the new matrix*)
let rec col_sum (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = sccountm s r1 r2 c1 (c2+1) [] in
                                                               col_countm s [] ir1 ic1 (ir1+1) (ic1+(length tc)) 0 (length s) tc 
                                                          else
                                                                  raise (InvalidInput);;

(*Functions for calculating average*)
(*The function full_avg uses the function full_sum and then divides the computed sum by the number of elements in the range*)
let rec full_avg (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = scountm s r1 c1 r2 c2 0 (length s) 0.0 in
                                                               let m = tc /. (float_of_int((r2-r1+1)*(c2-c1+1))) in
                                                               full_countm s [] ir1 ic1 0 (length s) m 
                                                          else
                                                               raise (InvalidInput);;

(*asrcountr computes the total of valid elements of a row. And then the sum is divided by the number of valid elements in each row.*)
let rec asrcountr (s:sheet) c1 c2 row iter m count = if iter == m then count /. float_of_int(c2-c1+1)
                                                         else
                                                                if iter >= c1 && iter <= c2 then
                                                                        if (nth (nth s row) iter) == nan then raise (EmptyCell)
                                                                        else
                                                                        asrcountr s c1 c2 row (iter+1) m (count+.(nth (nth s row) iter))
                                                                else
                                                                       asrcountr s c1 c2 row (iter+1) m count

(*asrcountm stores the avg of valid elements for each row in the list count*)
let rec asrcountm (s:sheet) c1 c2 iter n count = if iter == n then count
                                                   else
                                                       let rc = asrcountr s c1 c2 iter 0 (length (nth s iter)) 0.0 in
                                                       asrcountm s c1 c2 (iter+1) n (append count [rc]);; 

(*Function for computing average of the valid elements of each row in the spreadsheet. Uses earlier defined function row_countm to form the new matrix.*)
let rec row_avg (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = asrcountm s c1 c2 r1 (r2+1) [] in
                                                               row_countm s [] ir1 ic1 (ir1+length(tc)) (ic1+1) 0 (length s) tc  
                                                          else
                                                               raise (InvalidInput);;

(*asccountr computes the total of valid elements in each column of the range and then divides it by the number of valid elements in each column.*)
let rec asccountr (s:sheet) r1 r2 col iter n count = if iter == n then count /. float_of_int(r2-r1+1)
                                                         else
                                                                if iter >= r1 && iter <= r2 then
                                                                        if (nth (nth s iter) col) == nan then raise (EmptyCell)
                                                                        else   
                                                                      asccountr s r1 r2 col (iter+1) n (count+.(nth (nth s iter) col))
                                                                else
                                                                      asccountr s r1 r2 col (iter+1) n count

(*asccountm stores the avg number of valid elements for each column in the list count and it iterates over all the columns.*)
let rec asccountm (s:sheet) r1 r2 iter n count = if iter == n then count
                                                   else
                                                       let rc = asccountr s r1 r2 iter 0 (length s) 0.0 in
                                                       asccountm s r1 r2 (iter+1) n (append count [rc]);; 

(* col_sum is the function which appends a row containing the total of valid entries in the range corresponding to each column in the range. It uses the
 * earlier defined function col_countm in order to form the new matrix.*)
let rec col_avg (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = asccountm s r1 r2 c1 (c2+1) [] in
                                                               col_countm s [] ir1 ic1 (ir1+1) (ic1+(length tc)) 0 (length s) tc 
                                                          else
                                                                  raise (InvalidInput);;

(*The functions full_min determines the minimum element in the entire sheet*)
(*The function minr iterates over the elements of a row and updates the minimum element obtained so far*)
let rec minr (s:sheet) r1 c1 r2 c2 row iter m (min:float) = if iter == m then min
                                                    else
                                                        if row >= r1 && row <= r2 && iter >= c1 && iter <= c2 && (nth (nth s row) iter) < min then
                                                               minr s r1 c1 r2 c2 row (iter+1) m (nth (nth s row) iter)
                                                        else
                                                               minr s r1 c1 r2 c2 row (iter+1) m min

(*minm iterates over the rows of the matrix*)
let rec minm (s:sheet) r1 c1 r2 c2 iter n min = if iter == n then min
                                                    else
                                                        let rc = minr s r1 c1 r2 c2 iter 0 (length (nth s iter)) min in
                                                        minm s r1 c1 r2 c2 (iter+1) n rc;; 

(*Stores the minimum element of the entire sheet at the desired location. Uses the earlier defined function full_countm at the desired location.*)
let rec full_min (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                         let c1 = get11(get01(r)) in
                                                         let r2 = get01(get11(r)) in
                                                         let c2 = get11(get11(r)) in
                                                         if r1 <= r2 && c1 <= c2 then
                                                                 let ir1 = get01(i) in
                                                                 let ic1 = get11(i) in
                                                                 let tc = minm s r1 c1 r2 c2 0 (length s) (nth (nth s r1) c1) in
                                                                 full_countm s [] ir1 ic1 0 (length s) tc
                                                         else
                                                                 raise (InvalidInput);;

(*mnrcountr computes the minimum amongst the valid elements of a row i.e those present between c1 and c2.*)
let rec mnrcountr (s:sheet) c1 c2 row iter m min = if iter == m then min
                                                         else
                                                                if iter >= c1 && iter <= c2 && (nth (nth s row) iter) < min then
                                                                       mnrcountr s c1 c2 row (iter+1) m (nth (nth s row) iter)
                                                                else
                                                                       mnrcountr s c1 c2 row (iter+1) m min

(*mnrcountm appends the minimum element of each row in the list count*)
let rec mnrcountm (s:sheet) c1 c2 iter n count = if iter == n then count
                                                   else
                                                       let rc = mnrcountr s c1 c2 iter 0 (length (nth s iter)) (nth (nth s iter) c1) in
                                                       mnrcountm s c1 c2 (iter+1) n (append count [rc]);; 

(*Function for computing min of the valid elements for each row in the spreadsheet. Uses earlier defined utility function row_countm in order to form the new matrix.*)
let rec row_min (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = mnrcountm s c1 c2 r1 (r2+1) [] in
                                                               row_countm s [] ir1 ic1 (ir1+length(tc)) (ic1+1) 0 (length s) tc  
                                                          else
                                                               raise (InvalidInput);;

(*mnccountr computes the total number of valid elements in each column of the range.It iterates over each and every element of a column and checks 
 * minimality for the valid elements in the range.*)
let rec mnccountr (s:sheet) r1 r2 col iter n min = if iter == n then min
                                                         else
                                                                if iter >= r1 && iter <= r2 && (nth (nth s iter) col) < min then
                                                                      mnccountr s r1 r2 col (iter+1) n (nth (nth s iter) col)
                                                                else
                                                                      mnccountr s r1 r2 col (iter+1) n min

(*mnccountm stores the total number of valid elements for each column in the list count.It iterates over all the columns of the sheet.*)
let rec mnccountm (s:sheet) r1 r2 iter n count = if iter == n then count
                                                   else
                                                       let rc = mnccountr s r1 r2 iter 0 (length s) (nth (nth s r1) iter) in
                                                       mnccountm s r1 r2 (iter+1) n (append count [rc]);; 

(* col_min is the function which appends a row containing the total of valid entries in the range corresponding to each column*)
let rec col_min (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = mnccountm s r1 r2 c1 (c2+1) [] in
                                                               col_countm s [] ir1 ic1 (ir1+1) (ic1+(length tc)) 0 (length s) tc 
                                                          else
                                                                  raise (InvalidInput);;


(*The functions full_max determines the minimum element in the entire sheet*)
(*The function iterates over the rows and updates the maximum element obtained so far*)
let rec maxr (s:sheet) r1 c1 r2 c2 row iter m max = if iter == m then max
                                                    else
                                                        if row >= r1 && row <= r2 && iter >= c1 && iter <= c2 && (nth (nth s row) iter) > max then
                                                               maxr s r1 c1 r2 c2 row (iter+1) m (nth (nth s row) iter)
                                                        else
                                                               maxr s r1 c1 r2 c2 row (iter+1) m max

(*maxm stores the total number of valid elements of a matrix*)
let rec maxm (s:sheet) r1 c1 r2 c2 iter n max = if iter == n then max
                                                    else
                                                        let rc = maxr s r1 c1 r2 c2 iter 0 (length (nth s iter)) max in
                                                        maxm s r1 c1 r2 c2 (iter+1) n rc;;

(*Stores the maximum element of the entire sheet at the desired location*)
let rec full_max (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                         let c1 = get11(get01(r)) in
                                                         let r2 = get01(get11(r)) in
                                                         let c2 = get11(get11(r)) in
                                                         if r1 <= r2 && c1 <= c2 then
                                                                 let ir1 = get01(i) in
                                                                 let ic1 = get11(i) in
                                                                 let tc = maxm s r1 c1 r2 c2 0 (length s) (nth (nth s r1) c1) in
                                                                 full_countm s [] ir1 ic1 0 (length s) tc
                                                         else
                                                                 raise (InvalidInput);;

(*mxrcountr computes the maximum of valid elements of a row by iterating over each of the elements.*)
let rec mxrcountr (s:sheet) c1 c2 row iter m max = if iter == m then max
                                                         else
                                                               if iter >= c1 && iter <= c2 && (nth (nth s row) iter) > max then
                                                                       mxrcountr s c1 c2 row (iter+1) m (nth (nth s row) iter)
                                                               else
                                                                       mxrcountr s c1 c2 row (iter+1) m max

(*mxrcountm stores the total of valid elements for each row in the list count*)
let rec mxrcountm (s:sheet) c1 c2 iter n count = if iter == n then count
                                                   else
                                                       let rc = mxrcountr s c1 c2 iter 0 (length (nth s iter)) (nth (nth s iter) c1) in
                                                       mxrcountm s c1 c2 (iter+1) n (append count [rc]);; 

(*Function for computing max of the valid elements for each row in the spreadsheet. Uses earlier defined utility function row_countm in order to form the new matrix.*)
let rec row_max (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = mxrcountm s c1 c2 r1 (r2+1) [] in
                                                               row_countm s [] ir1 ic1 (ir1+length(tc)) (ic1+1) 0 (length s) tc  
                                                          else
                                                               raise (InvalidInput);;

(*mxccountr computes the maximum element of each row in the range.It iterates over the elements of the row in the range and then updates the maximum accordingly.*)
let rec mxccountr (s:sheet) r1 r2 col iter n max = if iter == n then max
                                                         else
                                                                if iter >= r1 && iter <= r2 && (nth (nth s iter) col) > max then
                                                                      mxccountr s r1 r2 col (iter+1) n (nth (nth s iter) col)
                                                                else
                                                                      mxccountr s r1 r2 col (iter+1) n max

(*mxccountm stores the total number of valid elements for each column in the list count.It iterates over the columns in the range
 * and updates the maximum element for each column.*)
let rec mxccountm (s:sheet) r1 r2 iter n count = if iter == n then count
                                                   else
                                                       let rc = mxccountr s r1 r2 iter 0 (length s) (nth (nth s r1) iter) in
                                                       mxccountm s r1 r2 (iter+1) n (append count [rc]);; 

(*The function calculates the maximum element for the elements in the columns in the range specified and then forms the new sheet accordingly.*)
let rec col_max (s:sheet) (r:range) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                          let c1 = get11(get01(r)) in
                                                          let r2 = get01(get11(r)) in
                                                          let c2 = get11(get11(r)) in
                                                          if r1 <= r2 && c1 <= c2 then
                                                               let ir1 = get01(i) in
                                                               let ic1 = get11(i) in
                                                               let tc = mxccountm s r1 r2 c1 (c2+1) [] in
                                                               col_countm s [] ir1 ic1 (ir1 + 1) (ic1 + (length tc)) 0 (length s) tc 
                                                          else
                                                                  raise (InvalidInput);;


(*The functions op_const where op is add,subt,mult,and div perform the respective operations with the constants provided.
 * Each of the functions make a call to opcm and opcr in order to generate a new 2d subsheet. This subsheet is incorporated in the new sheet by the aid of the functions
 * ins2d and newrow which is indeed a nested iteration in order to form the new matrix.*)

let rec newrow s mat ir1 ic1 n1 m1 row iter m res = if iter == m then res
                                                else
                                                        if ic1 <= iter && iter < (ic1 + m1) && ir1 <= row && row < (ir1 + n1) then
                                                                newrow s mat ir1 ic1 n1 m1 row (iter+m1) m (append res (nth mat (row-ir1)))
                                                        else
                                                                newrow s mat ir1 ic1 n1 m1 row (iter+1) m (append res [(nth (nth s row) iter)]);;

let rec ins2d s mat ir1 ic1 iter n res = if iter == n then res
                                        else
                                                let r = newrow s mat ir1 ic1 (length mat) (length (nth mat 0)) iter 0 (length (nth s iter)) [] in
                                                ins2d s mat ir1 ic1 (iter+1) n (append res [r]);;

let rec addcr s c r1 c1 r2 c2 row iter m res = if iter == m then res
                                               else
                                                       if (nth (nth s row) iter) == nan then raise (EmptyCell)
                                                       else addcr s c r1 c1 r2 c2 row (iter+1) m (append res [(nth (nth s row) iter)+.c])

let rec addcm (s:sheet) c r1 c1 r2 c2 iter n res = if iter == n then res
                                                        else
                                                                let rs = addcr s c r1 c1 r2 c2 iter c1 (c2+1) [] in
                                                                addcm s c r1 c1 r2 c2 (iter+1) n (append res [rs]);;


let rec add_const (s:sheet) (r:range) (c:float) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                                        let c1 = get11(get01(r)) in
                                                                        let r2 = get01(get11(r)) in
                                                                        let c2 = get11(get11(r)) in
                                                                        if r1 <= r2 && c1 <= c2 then
                                                                                let ir1 = get01(i) in
                                                                                let ic1 = get11(i) in
                                                                                let st = addcm s c r1 c1 r2 c2 r1 (r2+1) [] in
                                                                                ins2d s st ir1 ic1 0 (length s) []
                                                                        else
                                                                                raise (InvalidInput);;

let rec subtcr s c r1 c1 r2 c2 row iter m res = if iter == m then res
                                               else 
                                                       if (nth (nth s row) iter) == nan then raise (EmptyCell)
                                                       else subtcr s c r1 c1 r2 c2 row (iter+1) m (append res [(nth (nth s row) iter)-.c])

let rec subtcm (s:sheet) c r1 c1 r2 c2 iter n res = if iter == n then res
                                                        else
                                                                let rs = subtcr s c r1 c1 r2 c2 iter c1 (c2+1) [] in
                                                                subtcm s c r1 c1 r2 c2 (iter+1) n (append res [rs]);;

let rec subt_const (s:sheet) (r:range) (c:float) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                                        let c1 = get11(get01(r)) in
                                                                        let r2 = get01(get11(r)) in
                                                                        let c2 = get11(get11(r)) in
                                                                        if r1 <= r2 && c1 <= c2 then
                                                                                let ir1 = get01(i) in
                                                                                let ic1 = get11(i) in
                                                                                let st = subtcm s c r1 c1 r2 c2 r1 (r2+1) [] in
                                                                                ins2d s st ir1 ic1 0 (length s) []
                                                                        else
                                                                                raise (InvalidInput);;
let rec multcr s c r1 c1 r2 c2 row iter m res = if iter == m then res
                                               else 
                                                       if (nth (nth s row) iter) == nan then raise (EmptyCell)
                                                       else multcr s c r1 c1 r2 c2 row (iter+1) m (append res [(nth (nth s row) iter)*.c])

let rec multcm (s:sheet) c r1 c1 r2 c2 iter n res = if iter == n then res
                                                        else
                                                                let rs = multcr s c r1 c1 r2 c2 iter c1 (c2+1) [] in
                                                                multcm s c r1 c1 r2 c2 (iter+1) n (append res [rs]);;

let rec mult_const (s:sheet) (r:range) (c:float) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                                        let c1 = get11(get01(r)) in
                                                                        let r2 = get01(get11(r)) in
                                                                        let c2 = get11(get11(r)) in
                                                                        if r1 <= r2 && c1 <= c2 then
                                                                                let ir1 = get01(i) in
                                                                                let ic1 = get11(i) in
                                                                                let st = multcm s c r1 c1 r2 c2 r1 (r2+1) [] in
                                                                                ins2d s st ir1 ic1 0 (length s) []
                                                                        else
                                                                                raise (InvalidInput);;
let rec divcr s c r1 c1 r2 c2 row iter m res = if iter == m then res
                                               else 
                                                   if (nth (nth s row) iter) == nan then raise (EmptyCell)
                                                   else divcr s c r1 c1 r2 c2 row (iter+1) m (append res [(nth (nth s row) iter)/.c])

let rec divcm (s:sheet) c r1 c1 r2 c2 iter n res = if iter == n then res
                                                        else
                                                                let rs = divcr s c r1 c1 r2 c2 iter c1 (c2+1) [] in
                                                                divcm s c r1 c1 r2 c2 (iter+1) n (append res [rs]);;

let rec div_const (s:sheet) (r:range) (c:float) (i:index) : sheet = let r1 = get01(get01(r)) in
                                                                        let c1 = get11(get01(r)) in
                                                                        let r2 = get01(get11(r)) in
                                                                        let c2 = get11(get11(r)) in
                                                                        if r1 <= r2 && c1 <= c2 then
                                                                                let ir1 = get01(i) in
                                                                                let ic1 = get11(i) in
                                                                                let st = divcm s c r1 c1 r2 c2 r1 (r2+1) [] in
                                                                                ins2d s st ir1 ic1 0 (length s) []
                                                                        else
                                                                                raise (InvalidInput);;
let v (s:sheet) p = nth (nth s (get01 p)) (get11 p);;

let rec addr r1 r2 iter m res = if iter == m then res
                                else addr r1 r2 (iter+1) m (append res [(nth r1 iter)+.(nth r2 iter)])

let rec addm m1 m2 iter n m res = if iter == n then res
                                else addm m1 m2 (iter+1) n m (append res [addr (nth m1 iter) (nth m2 iter) 0 m []])

(*The functions op_range are implemented in a way in which firstly, the 2d sheet is operated on the identity element for the respective operations
 * in order to obtain a sub sheet and further performing operations over the 2 sub sheets followed by the formation of the new sheet by the function ins2d*)
let rec add_range (s:sheet) (ra1:range) (ra2:range) (i:index) : sheet = let r1 = get01(get01(ra1)) in
                                                                        let c1 = get11(get01(ra1)) in
                                                                        let r2 = get01(get11(ra1)) in
                                                                        let c2 = get11(get11(ra1)) in
                                                                        let r3 = get01(get01(ra2)) in
                                                                        let c3 = get11(get01(ra2)) in
                                                                        let r4 = get01(get11(ra2)) in
                                                                        let c4 = get11(get11(ra2)) in
                                                                        if r1 <= r2 && c1 <= c2 && r3 <= r4 && c3 <= c4 && (r2-r1) == (r4-r3) && (c2-c1) == (c4-c3) then
                                                                                let ir1 = get01(i) in
                                                                                let ic1 = get11(i) in
                                                                                let st1 = addcm s 0.0 r1 c1 r2 c2 r1 (r2+1) [] in
                                                                                let st2 = addcm s 0.0 r3 c3 r4 c4 r3 (r4+1) [] in
                                                                                let st3 = addm st1 st2 0 (r2-r1+1) (c2-c1+1) [] in
                                                                                ins2d s st3 ir1 ic1 0 (length s) []
                                                                        else
                                                                                raise (InvalidInput);;

let rec subtr r1 r2 iter m res = if iter == m then res
                                else subtr r1 r2 (iter+1) m (append res [(nth r1 iter)-.(nth r2 iter)])

let rec subtm m1 m2 iter n m res = if iter == n then res
                                else subtm m1 m2 (iter+1) n m (append res [subtr (nth m1 iter) (nth m2 iter) 0 m []])

let rec subt_range (s:sheet) (ra1:range) (ra2:range) (i:index) : sheet = let r1 = get01(get01(ra1)) in
                                                                        let c1 = get11(get01(ra1)) in
                                                                        let r2 = get01(get11(ra1)) in
                                                                        let c2 = get11(get11(ra1)) in
                                                                        let r3 = get01(get01(ra2)) in
                                                                        let c3 = get11(get01(ra2)) in
                                                                        let r4 = get01(get11(ra2)) in
                                                                        let c4 = get11(get11(ra2)) in
                                                                        if r1 <= r2 && c1 <= c2 && r3 <= r4 && c3 <= c4 && (r2-r1) == (r4-r3) && (c2-c1) == (c4-c3) then
                                                                                let ir1 = get01(i) in
                                                                                let ic1 = get11(i) in
                                                                                let st1 = subtcm s 0.0 r1 c1 r2 c2 r1 (r2+1) [] in
                                                                                let st2 = subtcm s 0.0 r3 c3 r4 c4 r3 (r4+1) [] in
                                                                                let st3 = subtm st1 st2 0 (r2-r1+1) (c2-c1+1) [] in
                                                                                ins2d s st3 ir1 ic1 0 (length s) []
                                                                        else
                                                                                raise (InvalidInput);;

let rec multr r1 r2 iter m res = if iter == m then res
                                else multr r1 r2 (iter+1) m (append res [(nth r1 iter)*.(nth r2 iter)])

let rec multm m1 m2 iter n m res = if iter == n then res
                                else multm m1 m2 (iter+1) n m (append res [multr (nth m1 iter) (nth m2 iter) 0 m []])

let rec mult_range (s:sheet) (ra1:range) (ra2:range) (i:index) : sheet = let r1 = get01(get01(ra1)) in
                                                                        let c1 = get11(get01(ra1)) in
                                                                        let r2 = get01(get11(ra1)) in
                                                                        let c2 = get11(get11(ra1)) in
                                                                        let r3 = get01(get01(ra2)) in
                                                                        let c3 = get11(get01(ra2)) in
                                                                        let r4 = get01(get11(ra2)) in
                                                                        let c4 = get11(get11(ra2)) in
                                                                        if r1 <= r2 && c1 <= c2 && r3 <= r4 && c3 <= c4 && (r2-r1) == (r4-r3) && (c2-c1) == (c4-c3) then
                                                                                let ir1 = get01(i) in
                                                                                let ic1 = get11(i) in
                                                                                let st1 = multcm s 1.0 r1 c1 r2 c2 r1 (r2+1) [] in
                                                                                let st2 = multcm s 1.0 r3 c3 r4 c4 r3 (r4+1) [] in
                                                                                let st3 = multm st1 st2 0 (r2-r1+1) (c2-c1+1) [] in
                                                                                ins2d s st3 ir1 ic1 0 (length s) []
                                                                        else
                                                                                raise (InvalidInput);;

let rec divr r1 r2 iter m res = if iter == m then res
                                else divr r1 r2 (iter+1) m (append res [(nth r1 iter)/.(nth r2 iter)])

let rec divm m1 m2 iter n m res = if iter == n then res
                                else divm m1 m2 (iter+1) n m (append res [divr (nth m1 iter) (nth m2 iter) 0 m []])

let rec div_range (s:sheet) (ra1:range) (ra2:range) (i:index) : sheet = let r1 = get01(get01(ra1)) in
                                                                        let c1 = get11(get01(ra1)) in
                                                                        let r2 = get01(get11(ra1)) in
                                                                        let c2 = get11(get11(ra1)) in
                                                                        let r3 = get01(get01(ra2)) in
                                                                        let c3 = get11(get01(ra2)) in
                                                                        let r4 = get01(get11(ra2)) in
                                                                        let c4 = get11(get11(ra2)) in
                                                                        if r1 <= r2 && c1 <= c2 && r3 <= r4 && c3 <= c4 && (r2-r1) == (r4-r3) && (c2-c1) == (c4-c3) then
                                                                                let ir1 = get01(i) in
                                                                                let ic1 = get11(i) in
                                                                                let st1 = divcm s 1.0 r1 c1 r2 c2 r1 (r2+1) [] in
                                                                                let st2 = divcm s 1.0 r3 c3 r4 c4 r3 (r4+1) [] in
                                                                                let st3 = divm st1 st2 0 (r2-r1+1) (c2-c1+1) [] in
                                                                                ins2d s st3 ir1 ic1 0 (length s) []
                                                                        else
                                                                                raise (InvalidInput);;
(*Approapriate tokens and rules are defined and function calls are made which would change the earlier declared global sheet s.*)
# 735 "yaccer.ml"
let yytransl_const = [|
  258 (* LPARENTHESIS *);
  259 (* RPARENTHESIS *);
  260 (* COMMA *);
  261 (* COLON *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* FTERM *);
  265 (* NL *);
  266 (* ASSIGN *);
    0|]

let yytransl_block = [|
  257 (* FLOAT *);
  267 (* INT *);
  268 (* UOP *);
  269 (* BOP *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\005\000\004\000\000\000"

let yylen = "\002\000\
\000\000\002\000\001\000\002\000\005\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\005\000\005\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\003\000\002\000\000\000\000\000\
\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\000\007\000\011\000\009\000\008\000\012\000\006\000\000\000\
\013\000"

let yydgoto = "\002\000\
\003\000\006\000\007\000\008\000\017\000"

let yysindex = "\010\000\
\000\000\000\000\003\255\246\254\000\000\000\000\005\255\007\255\
\015\255\000\000\012\255\016\255\024\255\002\255\021\255\023\255\
\022\255\024\255\024\255\024\255\004\255\000\000\026\255\000\000\
\025\255\027\255\028\255\029\255\030\255\031\255\032\255\023\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\038\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\032\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\242\255\002\000"

let yytablesize = 41
let yytable = "\020\000\
\009\000\023\000\018\000\016\000\028\000\016\000\030\000\004\000\
\004\000\004\000\001\000\005\000\019\000\010\000\029\000\021\000\
\011\000\040\000\012\000\025\000\026\000\027\000\031\000\013\000\
\014\000\016\000\015\000\022\000\004\000\024\000\032\000\015\000\
\033\000\000\000\034\000\035\000\036\000\037\000\038\000\039\000\
\041\000"

let yycheck = "\014\000\
\011\001\016\000\001\001\002\001\001\001\002\001\021\000\006\001\
\006\001\006\001\001\000\009\001\011\001\009\001\011\001\014\000\
\010\001\032\000\004\001\018\000\019\000\020\000\021\000\012\001\
\013\001\002\001\011\001\007\001\006\001\008\001\005\001\000\000\
\008\001\255\255\008\001\008\001\008\001\008\001\008\001\008\001\
\003\001"

let yynames_const = "\
  LPARENTHESIS\000\
  RPARENTHESIS\000\
  COMMA\000\
  COLON\000\
  LBRACKET\000\
  RBRACKET\000\
  FTERM\000\
  NL\000\
  ASSIGN\000\
  "

let yynames_block = "\
  FLOAT\000\
  INT\000\
  UOP\000\
  BOP\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 729 "yaccer.mly"
                        ()
# 835 "yaccer.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : unit) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'line) in
    Obj.repr(
# 730 "yaccer.mly"
                        ()
# 843 "yaccer.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 733 "yaccer.mly"
                        ()
# 849 "yaccer.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'body) in
    Obj.repr(
# 734 "yaccer.mly"
                        (printm _1)
# 856 "yaccer.ml"
               : 'line))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'indices) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    Obj.repr(
# 737 "yaccer.mly"
                                                     (match _3 with
                                                        | "COUNT" -> full_count sheet _4 _1
                                                        | "ROWCOUNT" -> row_count sheet _4 _1
                                                        | "COLCOUNT" -> col_count sheet _4 _1
                                                        | "SUM" -> full_sum sheet _4 _1
                                                        | "ROWSUM" -> row_sum sheet _4 _1
                                                        | "COLSUM" -> col_sum sheet _4 _1
                                                        | "AVG" -> full_avg sheet _4 _1
                                                        | "ROWAVG" -> row_avg sheet _4 _1
                                                        | "COLAVG" -> col_avg sheet _4 _1
                                                        | "MIN" -> full_min sheet _4 _1
                                                        | "ROWMIN" -> row_min sheet _4 _1
                                                        | "COLMIN" -> col_min sheet _4 _1
                                                        | "MAX" -> full_max sheet _4 _1
                                                        | "ROWMAX" -> row_max sheet _4 _1
                                                        | "COLMAX" -> col_max sheet _4 _1
                                                        | _ -> sheet)
# 881 "yaccer.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'indices) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'range) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    Obj.repr(
# 755 "yaccer.mly"
                                                      (match _3 with
                                                        | "ADD" -> add_range sheet _4 _5 _1
                                                        | "SUBT" -> subt_range sheet _4 _5 _1
                                                        | "MULT" -> mult_range sheet _4 _5 _1
                                                        | "DIV" -> div_range sheet _4 _5 _1
                                                        | _ -> sheet)
# 896 "yaccer.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'indices) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    Obj.repr(
# 762 "yaccer.mly"
                                                     (match _3 with
                                                        | "ADD" -> add_const sheet _5 (float_of_int _4) _1
                                                        | "SUBT" -> subt_const sheet _5 (float_of_int _4) _1
                                                        | "MULT" -> mult_const sheet _5 (float_of_int _4) _1
                                                        | "DIV" -> div_const sheet _5 (float_of_int _4) _1 
                                                        | _ -> sheet)
# 911 "yaccer.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'indices) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'range) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 769 "yaccer.mly"
                                                     (match _3 with
                                                        | "ADD" -> add_const sheet _4 (float_of_int _5) _1
                                                        | "SUBT" -> subt_const sheet _4 (float_of_int _5) _1
                                                        | "MULT" -> mult_const sheet _4 (float_of_int _5) _1
                                                        | "DIV" -> div_const sheet _4 (float_of_int _5) _1 
                                                        | _ -> sheet)
# 926 "yaccer.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'indices) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'range) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 776 "yaccer.mly"
                                                     (match _3 with
                                                        | "ADD" -> add_const sheet _4 _5 _1
                                                        | "SUBT" -> subt_const sheet _4 _5 _1
                                                        | "MULT" -> mult_const sheet _4 _5 _1
                                                        | "DIV" -> div_const sheet _4 _5 _1
                                                        | _ -> sheet)
# 941 "yaccer.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'indices) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    Obj.repr(
# 783 "yaccer.mly"
                                                     (match _3 with
                                                        | "ADD" ->  add_const sheet _5 _4 _1
                                                        | "SUBT" -> subt_const sheet _5 _4 _1
                                                        | "MULT" -> mult_const sheet _5 _4 _1
                                                        | "DIV" -> div_const sheet _5 _4 _1
                                                        | _ -> sheet)
# 956 "yaccer.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'indices) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'indices) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'range) in
    Obj.repr(
# 790 "yaccer.mly"
                                                     (match _3 with
                                                        | "ADD" -> add_const sheet _5 (v sheet _4) _1
                                                        | "SUBT" -> subt_const sheet _5 (v sheet _4) _1
                                                        | "MULT" -> mult_const sheet _5 (v sheet _4) _1
                                                        | "DIV" -> div_const sheet _5 (v sheet _4) _1
                                                        | _ -> sheet)
# 971 "yaccer.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'indices) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'range) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'indices) in
    Obj.repr(
# 797 "yaccer.mly"
                                                     ( match _3 with
                                                        | "ADD" -> add_const sheet _4 (v sheet _5) _1
                                                        | "SUBT" -> subt_const sheet _4 (v sheet _5) _1
                                                        | "MULT" -> mult_const sheet _4 (v sheet _5) _1
                                                        | "DIV" -> div_const sheet _4 (v sheet _5) _1
                                                        | _ -> sheet)
# 986 "yaccer.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'indices) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'indices) in
    Obj.repr(
# 804 "yaccer.mly"
                                                        ((_2,_4))
# 994 "yaccer.ml"
               : 'range))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 806 "yaccer.mly"
                                                ((_2,_4))
# 1002 "yaccer.ml"
               : 'indices))
(* Entry input *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
;;
