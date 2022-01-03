open Printf
open Lexer
open Yaccer

let main () =
        try
                let lexbuf = Lexing.from_channel (open_in Sys.argv.(4)) in
                while true do
                        Yaccer.input Lexer.token lexbuf
                done
        with End_of_file -> exit 0
let _ = Printexc.print main ()


