memeber(X,[X|_]).
memeber(X,[_|Y]):-memeber(X,Y).

%holds if L1 and L2 have an element in common
intersect(L1,L2):- L1 = [F|R] , memeber(F,L2).
intersect(L1,L2):- L1 = [F|R] ,intersect(R,L2).

%holds L1 consists sublists such that every sublist of L1 intersects with L
all_intersect([], L). 
all_intersect(L1, L):- L1 = [F|R] , intersect(F,L) ,all_intersect(R,L).

%holds if atom A appears somewhere in nested list L 
member_nl(A,L):- L=[F|R] , atom(F), member(A,[F]).
member_nl(A,L):- L=[F|R] , member_nl(A,F).
member_nl(A,L):- L=[F|R] , member_nl(A,R) .

