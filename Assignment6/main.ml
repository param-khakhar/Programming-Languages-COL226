open A6
open List
open Printf

let res = []

let logPro = 
        let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
	let rec main res = 
		        let result = Yaccer.line Lexer.token lexbuf in
                        match result with
                        | (Node("end",[]),[]) -> res
                        | _ -> main (append res [result]) in
	main []
;;

let queries = 
    printf "?-"; flush stdout;
    let lexbuf = Lexing.from_channel stdin in
        while true do
            let result = Yaccer.line Lexer.token lexbuf in
                match result with
                | (Node("end",[]),[]) -> printf"Exit\n";flush stdout;exit 0
                | (query,[]) -> if length (vars query []) = 0 then 
                                    let c = findC query logPro [] in
                                    let a = resolve [] query c logPro [] in
                                    match a with
                                    | (true,_) -> printf"true.\n\n?-";flush stdout;
                                    | (false,_) -> printf"false.\n\n?-";flush stdout;
                                else
                                    let c = findC query logPro [] in
                                    let a = resolvex [] query c logPro (vars(query) []) []
                                   (* let a = resolvesx [query] logPro [] (vars(query) [])*) in
                                    match a with
                                    | (true,_) -> printf"\n?-";flush stdout;
                                    | (false,_) -> printf "\n?-";flush stdout;

                | _ -> printf "InvalidInput\n";flush stdout;
        done
;;
