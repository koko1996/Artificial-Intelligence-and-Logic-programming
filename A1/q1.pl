on(b1,b2).
on(b3,b4).
on(b4,b5).
on(b5,b6).
on(b11,b7).
just_left(b2,b6).
just_left(b6,b7).
	
% holds if X is somewhere above block Y in the pile
above(X,Y):- on(X,Y) .
 on(X,Z) , above(Z,Y).

% holds if X is somewhere left of block Y in the pile
left(X,Y):- just_left(X,Y).
left(X,Y):- just_left(X,YY) ,just_left(YY,Y) .
left(X,Y):- above(Y,K) ,left(X,K) .
left(X,Y):- above(X,Z) , left(Z,Y) .
left(X,Y):- above(Y,K) , above(X,Z) , left(Z,K).

% holds if X is somewhere right of block Y in the pile.
rxight(X,Y):- left(Y,X).