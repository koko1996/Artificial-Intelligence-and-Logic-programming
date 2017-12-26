:- op(800, xfy, [&]).   % Conjunction
:- op(850, xfy, [v]).   % Disjunction
:- op(870, xfy, [=>]).  % Implication
:- op(880, xfy, [<=>]). % Equivalence

% holds if X is a member of the given list (second argument)
member(X,[X|_]).
member(X,[_|Y]):- member(X,Y).


% ----------------------------------------------------------------------
%  holds if and only if interpretation L ( the values in the list are
% the true predicates of the specific interpretation) satisfies 
% the propositional logic formula F.
satisfies(L,-P):- not(satisfies(L,P)).

satisfies(L,P):- atom(P),  member(P,L).
satisfies(L,P & Q):- atom(P),  member(P,L) , atom(Q), ! , member(Q,L).
satisfies(L,P & Q):- satisfies(L,P),satisfies(L,Q).

satisfies(_,P v -P).
satisfies(_,-P v P).
satisfies(L,P v _):-atom(P),  member(P,L).
satisfies(L,_ v Q):-atom(Q),  member(Q,L).
satisfies(L,P v Q):- satisfies(L,P);satisfies(L,Q).

satisfies(L,P => _):- atom(P),  not(member(P,L)).
satisfies(L,P => Q):- atom(P),  member(P,L), atom(Q), !, member(Q,L).
satisfies(L,P => Q):-atom(P),  member(P,L), satisfies(L,Q).
satisfies(L,P => Q):- satisfies(L,P),satisfies(L,Q).
satisfies(L,P => _):- not(satisfies(L,P)).
satisfies(L,P <=> Q):- satisfies(L,P => Q),satisfies(L,Q => P).


% ----------------------------------------------------------------------
% holds if R is the resultof replacing all implications and
% double implications in propositional logic for- mula F by their
% definitions in terms of the other logical connectives

elimImpl(-P , - R ):-elimImpl(P, R).
elimImpl(P & Q, R1 & R2):- not(atom(P)), not(atom(Q)) ,elimImpl( P, R1), elimImpl( Q, R2),!.
elimImpl(P & Q, R1 & Q):- not(atom(P)), atom(Q) ,elimImpl( P, R1),!.
elimImpl(P & Q, P & R2):- not(atom(Q)),elimImpl( Q, R2),!.
elimImpl(P & Q, P & Q).


elimImpl(P v Q, R1 v R2):- not(atom(P)), not(atom(Q)) ,elimImpl( P, R1), elimImpl( Q, R2),!.
elimImpl(P v Q, R1 v Q):- not(atom(P)), atom(Q) ,elimImpl( P, R1),!.
elimImpl(P v Q, P v R2):- not(atom(Q)),elimImpl( Q, R2),!.
elimImpl(P v Q, P v Q).


elimImpl( P => Q , -(P) v Q).
elimImpl( -(P => Q) , -((P) v Q)).
elimImpl( P <=> Q , R1 & R2):- elimImpl( P => Q , R1) , elimImpl( Q => P , R2).
elimImpl( -(P <=> Q) , R1 & R2):- elimImpl( -(P => Q) , R1) , elimImpl( -(Q => P) , R2).



% ----------------------------------------------------------------------
%  holds if R is the result of putting propositional logic
% formula F in negation normal form. A propositional logic
% formula is in negation normal form if negation only appears
%  in front of propo-sitional variables and there are no nested negations.
% Assumptions : F contains no implications nor double implications.


nnf(- -Q,Q).
nnf(Q,Q):-atom(Q).
nnf(P,-Q):-dng(-P,Q),atom(Q).
nnf( -(P & Q), (-P v -Q)):-atom(P),atom(Q).
nnf( -(P & Q), (-P v Q1)):-atom(P), nnf(-Q,Q1).
nnf( -(P & Q), (P1 v -Q)):-atom(Q), nnf(-P,P1).
nnf( -(P & Q), ( P1 v Q1)):- nnf( -P ,P1) , nnf(-Q , Q1).


nnf( -(P v Q), (-P & -Q)):-atom(P),atom(Q),nnf(-P,_),nnf(-Q,_).
nnf( -(P v Q), ( P1 & Q1)):- nnf( -P ,P1) , nnf(-Q , Q1).

% ----------------------------------------------------------------------
%  holds if R is the result of putting propositional logic formula F
% in conjunctive normal form. A propositional logic formula is in 
% conjunctive normal form if it is a conjunction of disjunctions of
% literals,  where  a  literal  is  a  propositional  variable  or 
% its  negation. 
% Assumption: F contains no implications and double implications and is already in negation normal form.

dng(- -P,P).
cnf(P,P):-atom(P).
cnf(P,-Q):-dng(-P,Q),atom(Q).
cnf((P1 & Q1) v (P2 & Q2), (PP1 v PP2) & (PP1 v QQ2) & (QQ1 v PP2) & (QQ1 v QQ2)):-cnf(P1,PP1),cnf(Q1,QQ1),cnf(P2,PP2),cnf(Q2,QQ2).
cnf( P1 v (P2 & Q2), (PP1 v PP2) & (PP1 v QQ2)):-cnf(P1,PP1),cnf(P2,PP2),cnf(Q2,QQ2).
cnf( (P1 & Q1) v P2, (PP1 v PP2) & (QQ1 v PP2)):-cnf(P1,PP1),cnf(Q1,QQ1),cnf(P2,PP2).
cnf(P & Q , P1 & Q1 ):-  cnf(P,P1), cnf(Q,Q1).




