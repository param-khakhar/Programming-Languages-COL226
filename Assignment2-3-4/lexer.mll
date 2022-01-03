{
        open Printf
        open Yaccer
}
        (*For assignment3, removed the print statements used for the indication of matched regular expressions within the rules*)
        (*type token = 
                | FLOAT of float

                (* Introduced L and R parenthesis instead of only parenthesis. Same for bracket. Also introduced new rules in order to support these modifications.*)
                (*Changed the declaration of the tokens and removed the encapsulating char from some of the tokens *)
                (* | LPARENTHESIS of char
                | RPARENTHESIS of char
                | RBRACKET of char
                | LBRACKET of char
                | COMMA of char
                | COLON of char 
                | FTERM of char
                | Assign of string
                Also modified the definitions of rules according to the new declarations.*)

                | RPARENTHESIS
                | RBRACKET
                | LBRACKET
                | COMMA
                | COLON
                | ASSIGN
                | FTERM

                (*Introduced a token for the Newline character*)
                | NL

                (* [ i , j ]. Removing the constructors INDICESI and RANGESR,  as their detection would be done during the parsing stage.*)
                (*| INDICESI of string*)
                (* ([i,j] : [k,l]) *)
                (*| RANGESR of string*)

                | INT of int
                | UOP of string
                | BOP of string


*)
let nat = '0' | ['1' - '9']+['0'-'9']*
let digit = ['0'-'9']
let space = [' ' '\t']
let sign = ['+' '-']

rule token = parse

        (*Sign is optinoal followed by matching of single '0' and a natural number along with the decimal point and a combination of digits afterwards. 
          Return type FLOAT.*)

        | '\n'  {NL}

        | sign? (nat+ | '0') '.' digit* as fnum
                {FLOAT (float_of_string fnum)}
        
        (* Signed integers detected and the return type would be INT *)
        | sign? nat as inum
                {INT (int_of_string inum)}

        (*Regular Expression for detecting left as well as right parenthesis and brackets. Return type PARENTHESIS*)
        | "("   {LPARENTHESIS}
        | ")"   {RPARENTHESIS}
        | "["   {LBRACKET}
        | "]"   {RBRACKET}
        (* Regular expressions for the detection of comma, and colon *)
        | ","   {COMMA}
        | ":"   {COLON}
        (* Regular expression for detecting unary and binary operators *)
        | "COUNT" as uop        {UOP uop}
        | "ROWCOUNT" as uop     {UOP uop}
        | "COLCOUNT" as uop     {UOP uop}
        | "SUM" as uop          {UOP uop}
        | "ROWSUM" as uop       {UOP uop}
        | "COLSUM" as uop       {UOP uop}
        | "AVG" as uop          {UOP uop}
        | "ROWAVG" as uop       {UOP uop}
        | "COLAVG" as uop       {UOP uop}
        | "MIN" as uop          {UOP uop}
        | "ROWMIN" as uop       {UOP uop}
        | "COLMIN" as uop       {UOP uop}
        | "MAX" as uop          {UOP uop}
        | "ROWMAX" as uop       {UOP uop}
        | "COLMAX" as uop       {UOP uop}

        | "ADD" as bop          {BOP bop}               
        | "SUBT" as bop          {BOP bop}
        | "MULT" as bop         {BOP bop}
        | "DIV" as bop          {BOP bop}

        | ":="                  {ASSIGN}
        | ";"                   {FTERM}

(*        | '(' '[' nat ',' nat ']' ':' '[' nat ',' nat ']' ')' as l
 *               {printf "Range obtained:\n"; RANGESR l}
 *
 *        | '[' nat ',' nat ']' as l
 *               {printf "Indices obtained: %s\n" l; INDICESI l} *)

        | _     {token lexbuf}
        | eof   { exit 0 }
