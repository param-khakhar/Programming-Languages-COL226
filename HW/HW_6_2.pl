leaf().
node(_A,_B,_C).

subtree(T,[],T).
subtree(node(A,B,C),[1|XR],R) :- subtree(B,XR,R).
subtree(node(A,B,C),[2|XR],R) :- subtree(C,XR,R).

% let a = node(3,node(2,node(1,leaf,leaf),node(-1,leaf,leaf)), node(-2,node(1,leaf,leaf),node(-1,leaf,leaf)));;