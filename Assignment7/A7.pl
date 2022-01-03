% Base Types int, bool and variable
tint(N) :- number(N).
bool(true).
bool(false).
isbool(B) :- bool(B).
v(_V).
arrow(_A,_B).

% Mathematical Operations.
add(_X,_Y).
sub(_X,_Y).
prod(_X,_Y).
div(_X,_Y).

% Logical Operations.
not(_X,_Y).
and(_X,_Y).
or(_X,_Y).

% Comparison Operations.
eq(_X,_Y).
lt(_X,_Y).
gt(_X,_Y).

%Arrow Type.
arrow([_X|_XR]).

%Cartesian Product
n_tuple([_X|_XR]).

%If_Then_Else
if_then_else(_E1,_E2,_E3).

%Let_E1_E2
let_x_e1_e2(_X,_E1,_E2).

%variable Assignmen (Simple Definition).
simpleDef(_X,_E).

% d1 || d2
parDef(_D1,_D2).

% seqDef d1;d2
seqDef(_D1,_D2).

%localDef local D1 in D2
localDef(_D1,_D2).

%member of a list
member(X,[X|_R]).
member(X,[_Y|R]) :- member(X,R).

%Union of Two Lists
union([],[],[]).
union(List1,[],List1).
union(List1, [Head2|Tail2], [Head2|Output]):-
    \+(member(Head2,List1)), union(List1,Tail2,Output).
union(List1, [Head2|Tail2], Output):-
    member(Head2,List1), union(List1,Tail2,Output).  


%Function abstract and evaluation.
funct_eval(_E1,_E2).
funct_abs(n_tuple([_X|_XR])).

%Elaborates. Forms a variable - type pair list from the input variabe list using a type assumption.
elaborate(_Gamma,[],R,R).
elaborate(Gamma,[X|XR],R,Y) :- iter(Gamma,X,T), elaborate(Gamma,XR,[(X,T) | R],Y).
elaborate(Gamma,[X|XR],R,Y) :- hasType(Gamma,X,T), elaborate(Gamma,XR,[(X,T) | R],Y).

%Check. Checks whether all the elements of the list have the same type or not.
check([],_E,R,R).
check([(X,E)|XR],E,Y,R) :- check(XR,E,[(X,E)|Y],R).

convert([(_X,T)],T1,arrow(T,T1)).    
convert([(_X,T)|XR],T2,arrow(T1,T2)) :- convert(XR,T,T1).

% Defintions %
typeElaborates(Gamma,simpleDef(X,E),[(X,T)]) :- hasType(Gamma,E,T).
typeElaborates(Gamma,parDef((X1,E1),(X2,E2)),R) :- typeElaborates(Gamma,simpleDef(X1,E1),Gamma1),typeElaborates(Gamma,simpleDef(X2,E2),Gamma2),append(Gamma1,Gamma2,R).
typeElaborates(Gamma,seqDef((X1,E1),(X2,E2)),R) :- typeElaborates(Gamma,simpleDef(X1,E1),Gamma1),append(Gamma1,Gamma,I),typeElaborates(I,simpleDef(X2,E2),Gamma2),append(Gamma1,Gamma2,R).
typeElaborates(Gamma,localDef((X1,E1),(X2,E2)),Gamma2) :- typeElaborates(Gamma,simpleDef(X1,E1),Gamma1),append(Gamma1,Gamma,I),typeElaborates(I,simpleDef(X2,E2),Gamma2).

% Expressions %
hasType(Gamma,funct_eval(E1,n_tuple(E2)),T2) :- elaborate(Gamma,E2,[],[(_X,T4)|XR]),convert(XR,T4,T1),hasType(Gamma,E1,arrow(T1,T2)).
hasType(Gamma,funct_eval(E1,n_tuple(E2)),T2) :- elaborate(Gamma,E2,[],[(_X,T4)]),hasType(Gamma,E1,arrow(T4,T2)).
hasType(Gamma,funct_abs(n_tuple(N),E),T3) :- elaborate(Gamma,N,[],Y),convert(Y,T2,T3),hasType(Gamma,E,T2).
hasType(Gamma,let_x_e1_e2(X,E1,E2),T) :- hasType(Gamma,E1,T1), hasType([(X,T1) | Gamma],E2,T).
hasType(Gamma,n_tuple([X|[]]),T) :- hasType(Gamma,X,T).
hasType(Gamma,n_tuple([X|XR]),T1*T2) :- hasType(Gamma,X,T1),hasType(Gamma,n_tuple(XR),T2).
hasType(Gamma,if_then_else(E1,E2,E3),T) :- hasType(Gamma,E1,bool), hasType(Gamma,E2,T), hasType(Gamma,E3,T).
hasType(Gamma,projection(n_tuple(X),I),T) :- nth0(I, X, R), hasType(Gamma,R,T).

% Arithmetic Operator Rtypeules
hasType(Gamma,add(X,Y),int) :- hasType(Gamma,X,int),hasType(Gamma,Y,int).
hasType(Gamma,sub(X,Y),int) :- hasType(Gamma,X,int),hasType(Gamma,Y,int).
hasType(Gamma,mul(X,Y),int) :- hasType(Gamma,X,int),hasType(Gamma,Y,int).
hasType(Gamma,div(X,Y),int) :- hasType(Gamma,X,int),hasType(Gamma,Y,int).

% Logical Operator Rules
hasType(Gamma,not(X),bool) :- hasType(Gamma,X,bool).
hasType(Gamma,and(X,Y),bool) :- hasType(Gamma,X,bool),hasType(Gamma,Y,bool).
hasType(Gamma,or(X,Y),bool) :- hasType(Gamma,X,bool),hasType(Gamma,Y,bool).
hasType(Gamma,eq(X,Y),bool) :- hasType(Gamma,X,Z),hasType(Gamma,Y,Z).
hasType(Gamma,gt(X,Y),bool) :- hasType(Gamma,X,int),hasType(Gamma,Y,int).
hasType(Gamma,lt(X,Y),bool) :- hasType(Gamma,X,int),hasType(Gamma,Y,int).

hasType(Gamma,E,T) :- iter(Gamma,E,T).

hasType(_Gamma,v(_Z),variable).
hasType(_Gamma,X,bool) :- isbool(X).
hasType(_Gamma,X,int) :- tint(X).

% Lookup in the Table
iter([(E,Type)|_Tail],E,Type).
iter([(_F,_Type)|Tail],E,T) :- iter(Tail,E,T).


% Test Cases

% Variables %
% hasType([],v("b"),T).
% hasType([(v("x"), int), (v("y"), int), (v("z"), bool), (v("a"), int), (v("b"), int)], v("c"), T).

% Numerical and Boolean Constants %

% hasType([], 2, T).
% hasType([], true, T).
% hasType([], false, T).

% Arithmetic  Operations over Arithmetic Expressions%

% hasType([],add(2, 3), T).
% hasType([],add(2, mul(sub(4,5),6)), T).
% hasType([(v("x"), int), (v("y"), int), (v("z"), bool), (v("a"), int), (v("b"), int)], add(v("z"), v("a")), T).
% hasType([(v("x"), int), (v("y"), int), (v("z"), bool), (v("a"), int), (v("b"), int)], add(v("x"), sub(v("a"), v("y"))), T).
% hasType([(v("x"), int), (v("y"), int), (v("z"), bool), (v("a"), int), (v("b"), int)], add(mul(v("y"), v("x")), sub(v("a"), v("b"))), T).

%% Logical Expressions %%

% hasType([],and(true,false),T).
% hasType([(v("x"), int),(v("y"), int), (v("z"), bool), (v("a"), bool), (v("b"), bool)], and(v("z"), or(v("a"), v("b"))),T).
% hasType([(v("x"), int),(v("y"), int), (v("z"), bool), (v("a"), bool), (v("b"), bool)], and(v("z"), not(or(v("a"), v("b")))),T).

%% Comparison Operations over Numerical Expressions %%

% hasType([(v("x"),int)] ,eq(2, add(2, v("x"))),T).
% hasType([(v("x"),int), (v("y"),int), (v("z"),bool), (v("a"),bool), (v("b"),bool)], lt(v("x"), v("y")), T).
% hasType([(v("x"),int), (v("y"),int), (v("z"),bool), (v("a"),bool), (v("b"),bool)], gt(2, v("y")), T).

%% Equality over Arbitrary Expressions %%

% hasType([],eq(2,3),T).
% hasType([],eq(true,false),T).
% hasType([(v("x"),int),(v("y"),bool)],eq(lt(2,add(2,v("x"))),and(v("y"),true)),T).

%% If_Then_Else %%

% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],if_then_else(not(v("z")),add(v("x"),v("y")),v("y")),T).
% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],if_then_else(not(v("z")),v("x"),v("z")),T).

%% Let X = E1 in E2 %%

% hasType([], let_x_e1_e2(v("x"), not(true), not(v("x"))), T).
% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)], let_x_e1_e2(v("x"),not(true),eq(v("x"),false)),T).
% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)], let_x_e1_e2(v("x"),not(true),eq(v("z"),false)),T).
% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)], let_x_e1_e2(v("a"),7,add(v("x"),mul(v("a"),v("y")))),T).
% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)], let_x_e1_e2(v("b"),not(true),add(v("x"),mul(v("a"),v("y")))),T).

%% Function Abstractions %%

% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],funct_abs(n_tuple([v("x"),v("y")]),div(v("x"),v("y"))),T).
% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],funct_abs(n_tuple([v("x"),v("y"),v("z")]),and(v("z"),eq(v("x"),v("y")))),T).
% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],funct_abs(n_tuple([v("x")]),add(v("x"),2)),T).

%% Function Application %%

% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],funct_eval(funct_abs(n_tuple([v("x")]),add(v("x"),2)),n_tuple([v("x")])),T).
% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],funct_eval(funct_abs(n_tuple([v("x"),v("y")]),div(v("x"),v("y"))),n_tuple([v("x"),v("y")])),T).
% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],funct_eval(funct_abs(n_tuple([v("x"),v("y"),v("z")]),and(v("z"),eq(v("x"),v("y")))),n_tuple([v("x"),v("y"),v("z")])),T).


%% N-Tuples %%

% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],n_tuple([v("x"),v("y")]),T).
% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],n_tuple([v("x"),true,false,2]),T).
% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],n_tuple([v("x"),2,3,4]),T).

%% Expressions using Projection Operations %%

% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],projection(n_tuple([v("x"),2,3,4]),2),T).

% hasType([])

%%% Definitions %%%

%% Simple Definitions %%

% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],simpleDef(v("x"),true),Gammau), hasType(Gammau,and(v("x"),v("z")),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],simpleDef(v("a"),2),Gammau), hasType(Gammau,add(v("a"),2),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],simpleDef(v("a"),2),Gammau), typeElaborates(Gammau,simpleDef(v("b"),3),Gammau2), hasType(Gammau2,add(v("a"),v("b")),T).

%% Sequential Definitions %%

% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],seqDef((v("b"),3),(v("a"),2)),Gammau), hasType(Gammau,add(v("a"),2),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],seqDef((v("x"),true),(v("y"),v("x"))),Gammau), hasType(Gammau,eq(v("x"),v("y")),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],seqDef((v("x"),true),(v("z"),v("x"))),Gammau), hasType(Gammau,eq(v("x"),v("z")),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],seqDef((v("z"),4),(v("x"),v("z"))),Gammau), hasType(Gammau,add(v("x"),v("z")),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],seqDef((v("z"),4),(v("x"),v("z"))),Gammau), hasType(Gammau,add(v("x"),v("y")),T).

%% Parallel Definitions %%

% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],parDef((v("b"),3),(v("a"),2)),Gammau), hasType(Gammau,add(v("a"),2),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],parDef((v("x"),true),(v("y"),v("x"))),Gammau), hasType(Gammau,eq(v("x"),v("y")),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],parDef((v("x"),true),(v("z"),v("x"))),Gammau), hasType(Gammau,eq(v("x"),v("z")),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],parDef((v("z"),4),(v("x"),v("z"))),Gammau), hasType(Gammau,add(v("x"),v("z")),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],parDef((v("z"),4),(v("x"),v("z"))),Gammau), hasType(Gammau,add(v("x"),v("y")),T).


%% Local Defintions %%

% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],localDef((v("b"),3),(v("a"),2)),Gammau), hasType(Gammau,add(v("a"),2),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],localDef((v("x"),true),(v("y"),v("x"))),Gammau), hasType(Gammau,eq(v("x"),v("y")),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],localDef((v("x"),true),(v("z"),v("x"))),Gammau), hasType(Gammau,eq(v("x"),v("z")),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],localDef((v("z"),4),(v("x"),v("z"))),Gammau), hasType(Gammau,add(v("x"),v("z")),T).
% typeElaborates([(v("x"),int),(v("y"),int),(v("z"),bool)],localDef((v("z"),4),(v("x"),v("z"))),Gammau), hasType(Gammau,add(v("x"),v("y")),T).

%% Polymorphic Type Inference %%

% hasType([(v("x"),int),(v("y"),int),(v("z"),bool)],funct_abs(n_tuple([v("a")]),add(v("a"),2)),T).
% hasType([],eq(v("x"),v("y")),T).
% hasType([],eq(F,v("y")),T).
% hasType([],n_tuple([v("x")]),T).