edge(a,b).
edge(b,c).
edge(c,d).
edge(a,c).
edge(a,d).
path(A,A).
path(A,B) :- edge(A,C), path(C,B).
tedge(A,B) :- edge(A,C),edge(C,B).
thedge(A,B) :- edge(A,D),edge(D,C),edge(C,B).
pair(a,b,c).
pair(a,c,d).
