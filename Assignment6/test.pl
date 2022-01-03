edge(a,b).
edge(b,c).
edge(c,d).
edge(c,e).
path(X,X).
path(A,B) :- edge(A,C), path(C,B).
