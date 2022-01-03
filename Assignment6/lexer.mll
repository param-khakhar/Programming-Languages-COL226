{
        open Printf
        open Yaccer
}

let var = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*
let const = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*

rule token = parse

        (*Sign is optinoal followed by matching of single '0' and a natural number along with the decimal point and a combination of digits afterwards. 
          Return type FLOAT.*)

        | '\n'  	{NL}
        | "("   	{LPAR}
        | ")"   	{RPAR}
        | ","   	{COMMA}
	| "."		{DOT}
        | ":-"  	{ASSIGN}
	| ";"   	{SC}
	| var as v 	{V v}
	| const as c 	{C c}
	
        | _     {token lexbuf}
        | eof   {EOF}
