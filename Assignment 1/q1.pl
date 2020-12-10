ancestor(X,Y):- parent(X,Y).
ancestor(X,Y):- parent(A,Y),ancestor(X,A).

common_ancestor(X,Y,Z):- ancestor(X,Y), ancestor(X,Z).

%closest common ancestor
closest_common_ancestor(X,Y,Z):- common_ancestor(X,Y,Z), not( child_common_ancestor(X,Y,Z)).
%auxilarily predicate
child_common_ancestor(X,Y,Z):-parent(X,C),common_ancestor(C,Y,Z).

%Ancestor List
ancestorList(X,Y,[]):- parent(X,Y).
ancestorList(X,Y,[Z|P]):- parent(X,Z), ancestorList(Z,Y,P).

%Descendant List
descendantTree(X,[X]):- not(parent(X,_)).
descendantTree(X,[X|L]):- parent(X,Y),setof(G,Y^(parent(X,Y),descendantTree(Y,G)),L).




