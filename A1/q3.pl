% This solution assumes that people are seated in a  table and that 
% their seating arrangment does not matter therefore there are multiple such permutations 





% source Zebra
memeber(X,[X|_]).
memeber(X,[_|Y]):-memeber(X,Y).

% source Zebra
sublist2([S1, S2], [S1, S2 | _]) .
sublist2(S, [_ | T]) :- sublist2(S, T).

% modified version of sublist2 to define across
sublist3([S1, S2, S3], [S1, S2 , S3| _]) .
sublist3(S, [_ | T]) :- sublist3(S, T).

% modified version of sublist2 to define besides for the person at the begging and the person at the end of the list 
sublist4([S1, S2, S3, S4], [S1, S2 , S3, S4]).



% round table 1 is beside 2 and 4 
%			  2 is beside 1 and 3 
%			  3 is beside 2 and 4
%			  4 is beside 1 and 3 
%			  1 is across 3   
%			  2 is across 4 
makecircle(0,[]).
makecircle(N,[human(_, _, _, _)|List])
                :- N>0, N1 is N - 1, makecircle(N1,List).


% human with Name Gender Food Drink				
human(Name,Gender,Food,Drink).

% beside  next to each other in the list or the begging and the the end of the list (since it is a rpund table)
beside(X,Y,L):-sublist4([X,Y,_,_],L).
beside(X,Y,L):-sublist4([Y,X,_,_],L).
beside(X,Y,L):-sublist4([_,_,X,Y],L).
beside(X,Y,L):-sublist4([_,_,Y,X],L).

% across each other  1st and 3rd or 2nd and 4th
across(X,Y,L):-sublist3([X,Z,Y],L).
across(X,Y,L):-sublist3([Y,Z,X],L).

				
				
				
				
				
find(PizzaEater):- makecircle(4,L),

 
%	The women sat across  from each other,
across(human(_,female,_,_),human(_,female,_,_),L), 
%	The men sat across W from each other,
across(human(_,male,_,_),human(_,male,_,_),L),

% Doreen sat beside the person who ordered steak.
beside(human(doreen,female,_,_),human(_,male,steak,_),L),

% The chicken came with a coke.
member(human(_,_,chicken,coke),L),

% The person with the lasagna sat across from the person with milk
across(human(_,_,lasagna,_),human(_,_,_,milk),L),


% someone drinks coffee.
member(human(_,_,_,coffee),L),

% one of the males is called david
member(human(david,male,_,_),L),

% Donna only drinks water.
member(human(donna,female,_,water),L),

% one of the males is called danny
member(human(danny,male,_,_),L),

% David never drinks coffee.
not(member(human(david,male,_,coffee),L)),
% Danny could not afford to order steak.
not(member(human(danny,male,steak,_),L)),
% someone ate pizza
member(human(PizzaEater,_,pizza,_),L),
display(L).











