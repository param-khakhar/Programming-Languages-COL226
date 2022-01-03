%{
    open Printf
    open List
    open String
    open A6

let parse_error s = 
        print_endline s;
        flush stdout
%}

%token <string> V C
%token LPAR RPAR COMMA SC NL ASSIGN DOT EOF

%start line
%type <A6.term * A6.term list> line

%%

line:   
        | clause NL       {(*printf "line\n";*)$1}
        | EOF        {(*printf "line3\n";*) (Node ("end",[]),[])}
;

clause: 
    	| fact		{(*printf "clause-1\n";*)$1}
    	| rule		{(*printf "rule-1\n";*)$1}
;

rule: 	term ASSIGN lst DOT {(*printf "rule\n";*) ($1,$3)}
;

fact: 	term DOT        {(*printf "fact\n";*)($1,[])}  
;

term: 	C      	              {(*printf "term-1\n";*) Node ($1,[])}
    	| V	     	      {(*printf "term-2\n";*) V $1}
    	| C LPAR lst RPAR     {(*printf"term-3\n";*)Node ($1,$3)}


lst: 	term		      {(*printf "lst-1\n" ;*) [$1]}
    	| lst COMMA term      {(*printf "lst-2\n";*) append $1 [$3]}
;

%%
