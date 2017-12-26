/** ---------------------------------------------------------

EECS 3401 Assignment 3

Family name: NANAH JI		

Given name:	KOKO		




---------------------------------------------------------- */

/* load the three search algorithms */
:- ensure_loaded('astar.pl').
:- ensure_loaded('astarCC.pl').
:- ensure_loaded('idastar.pl').

/* ------------------------------------------------------- */

/* successors( +State, -Neighbors)

   Neighbors is a list of elements (Cost, NewState) where
   NewState is a state reachable from State by one action and
   Cost is the cost for that corresponding action (=1 in our
   case)
*/   
 
 
 successors( State, Successors) :-   stateSize(State, 1, Size), 
 findIndexOfValue(State,1,IndexOfBlank,blank),
 successorBlankToLeft(State, Size, IndexOfBlank, NewState1) ,
 successorBlankToRight(State, Size, IndexOfBlank, NewState2),
 successorBlankToDown(State, Size, IndexOfBlank, NewState3),
 successorBlankToUp(State, Size, IndexOfBlank, NewState4),!,
 appendd([],[(1,NewState1)],Accumulator1),!,
 appendd(Accumulator1,[(1,NewState2)],Accumulator2),!,
 appendd(Accumulator2,[(1,NewState3)],Accumulator3),!,
 appendd(Accumulator3,[(1,NewState4)],Successors) , !  .
 

 % APPENDS (Cost,list) to the third list except when the list is empty
appendd(L,[(_,[])],L).
appendd(L1,L2,L3):-append(L1,L2,L3).



% finds the index of given value
findIndexOfValue([GivenValue|_] , Index , Index , GivenValue). 
findIndexOfValue([_|R] , Index , IndexOfBlank , GivenValue):- Index1 is Index + 1 , findIndexOfValue(R,Index1,IndexOfBlank,GivenValue).


% finds the value at given index
findValueAtIndex([F|_] , Index, F, Index).
findValueAtIndex([_|R] , Index, Value, IndexCounter):- IndexCounter1 is IndexCounter + 1, findValueAtIndex(R,Index , Value , IndexCounter1).


% finds the size of the list 
stateSize([], Index , 0).
stateSize([F], Index , Index).
stateSize([F|R], Index , SizeOfState):- Index1 is  Index + 1, stateSize(R,Index1 , SizeOfState).


% finds The next states Left ,Right , North, South  respectively and return the
% next state if it exists otherwise it returns the empty list
successorBlankToLeft(State, Size, IndexOfBlank, NextState):- S is round(sqrt(Size)), R is mod(IndexOfBlank,S) , not(R=1), NewIndexOfBlank is IndexOfBlank - 1,findValueAtIndex(State, NewIndexOfBlank, ValueToChange,  1) ,exchangeBlank(State, ValueToChange , NextState).
successorBlankToLeft(_,_,_,[]).
successorBlankToRight(State, Size, IndexOfBlank, NextState):- S is round(sqrt(Size)), R is mod(IndexOfBlank,S) , not(R=0), NewIndexOfBlank is IndexOfBlank + 1,findValueAtIndex(State, NewIndexOfBlank, ValueToChange,  1) ,exchangeBlank(State, ValueToChange , NextState).
successorBlankToRight(_,_,_,[]).
successorBlankToDown(State, Size, IndexOfBlank, NextState):- Sqrt is round(sqrt(Size)), R is Size - Sqrt , not(IndexOfBlank>R),NewIndexOfBlank is IndexOfBlank + Sqrt,findValueAtIndex(State, NewIndexOfBlank, ValueToChange,  1) ,exchangeBlank(State, ValueToChange , NextState).
successorBlankToDown(_,_,_,[]).
successorBlankToUp(State, Size, IndexOfBlank, NextState):- Sqrt is round(sqrt(Size)), not(IndexOfBlank<Sqrt), NewIndexOfBlank is IndexOfBlank - Sqrt,findValueAtIndex(State, NewIndexOfBlank, ValueToChange,  1) ,exchangeBlank(State, ValueToChange , NextState).
successorBlankToUp(_,_,_,[]).


% exchanges blank with the given value 
exchangeBlank([],_,[]).
exchangeBlank([blank|R], ValueToChange , [ValueToChange|R1]):- exchangeBlank(R,ValueToChange,R1).
exchangeBlank([ValueToChange|R], ValueToChange , [blank|R1]):- exchangeBlank(R,ValueToChange,R1).
exchangeBlank([F|R], ValueToChange , [F|R1]):- exchangeBlank(R,ValueToChange,R1).



/* ------------------------------------------------------- */


/* equality(+S1, +S2)

   holds if and only S1 and S2 describe the same state
*/      
equality(State1,State1).



/* ------------------------------------------------------- */

/* hfn_null( +State, -V)

   V is the null heuristic for State (=0 irrelevant of the state)
*/
hfn_null(_State, 0).



/* hfn_misplaced( +State, -V)

   V is the number of misplaced tiles in State   
*/
hfn_misplaced( State, V) :-  countMisplaced(State,1,0,V),!.


% sums up the number of misplaced tiles
countMisplaced([Index], Index,  Counter , Counter).
countMisplaced([blank], Index,  Counter , Counter).
countMisplaced([NotIndex], Index,  Counter , Counter1):- Counter1 is Counter + 1.
countMisplaced([Index|R], Index,  Counter , Answer):-Index1 is Index + 1, countMisplaced(R,Index1 ,Counter,Answer).
countMisplaced([blank|R], Index,  Counter , Answer):-Index1 is Index + 1,countMisplaced(R,Index1 ,Counter,Answer).
countMisplaced([F|R], Index,  Counter , Answer):-Index1 is Index + 1, Counter1 is Counter + 1 , countMisplaced(R,Index1 ,Counter1,Answer).


/* hfn_manhattan( +State, -V)

   V is the sum over the manhattan distances between the current
   and the designated position of each tile
*/
hfn_manhattan( State, C ) :-  stateSize(State, 1, SizeOfState), sumManhattan(State,  1, 0 ,SizeOfState , C),!.


% sums up the manhattan distance
sumManhattan([],Index, Sum,SizeOfState, Sum).
sumManhattan([blank],Index, Sum, SizeOfState, Sum).
sumManhattan([Index|R],Index, Sum, SizeOfState, V):- Index1 is Index + 1 , sumManhattan(R,Index1,Sum,SizeOfState,V).
sumManhattan([blank|R],Index, Sum, SizeOfState, V):- Index1 is Index + 1 , sumManhattan(R,Index1,Sum,SizeOfState,V).
sumManhattan([F|R],Index, Sum,SizeOfState,V):- Index1 is Index + 1 , distanceManh(F,Index,SizeOfState,Dist) , Sum1 is Dist + Sum  ,sumManhattan(R,Index1,Sum1,SizeOfState,V).


distanceManh(ValueAtIndex , Index, Size , Answer):- Row is round(sqrt(Size)) , to2Dimension(Row,Index,X_value,Y_value),
to2Dimension(Row,ValueAtIndex,X_goal,Y_goal),
Answer1 is X_value - X_goal , Answer2 is Y_value - Y_goal,
Answer11 is abs(Answer1),Answer22 is abs(Answer2), Answer is Answer11 + Answer22.


% returns the X,Y cordinate for the given index
%  index 1 => 0,0
%  index 2 => 0,1
%  index 3 => 0,2
%  index 4 => 1,0
%  index 5 => 1,1
%  index 9 => 2,2
to2Dimension(RowLength,Index,X_cord,Y_cord):- Index1 is Index - 1, X_cord is Index1//RowLength , Y_cord is mod(Index1,RowLength).

/* ------------------------------------------------------- */


/* init( +Name, -State)

   State is the initial state for problem Name
*/

% My Test Cases
 init(tt1, [1, 2, 3, 4, 5, 6, 8, 7, blank]).
 init(t1, [1, 2, 3, 4, 5, 6, 7, 8, blank]).
 init(t7, [2,4,blank,6,1,7,3,5,8]).
 init(dd1, [7,5,2,3,blank,4,1,8,6]).
 init(dd2, [7,5,2,3,blank,4,1,8,6]).
 init(dd3, [5,3,2,4,6,7,1,8,blank]).
 init(dd4, [3,7,blank,4,6,2,8,5,1]).
 init(dd5, [blank,5,7,6,8,1,2,4,3]). 
 init(k1, [2, 4, blank, 6, 1, 7, 3, 5, 8]).
 init(a1,  [1, 2, 3, 4, blank, 8, 5, 6 , 7]).
 
 
 
 
 init(a,  [1, 2, 3, 4, 8, 5, blank, 7, 6]).

 init(b,  [8, 2, 6, 4, 1, 5, blank, 7, 3]). 

 init(c,  [blank, 2, 6, 4, 1, 5, 8, 7, 3]).

 init(d,  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, blank, 15, 13, 12, 11, 14]).




/* ------------------------------------------------------- */

/* goal( +State )

   holds if and oly if State is a goal state
*/   


goal(S) :- isSorted(S,1),!.

% holds if the given list is sorted and blank is at the end
isSorted([blank],_).
isSorted([Number,blank],Number):- integer(Number).
isSorted([F1,F2|R],F1):- not(F2=blank), F1 =< F2 , Index1 is F1 + 1 , isSorted([F2|R], Index1).


/* ------------------------------------------------------- */

% holds if the given state configuration is solvable
% from http://www.geeksforgeeks.org/check-instance-15-puzzle-solvable/
solvable(S):- stateSize(S, 1, SizeOfS) ,Row is round(sqrt(SizeOfS)), RowOddEven is mod(Row,2),!, solvable1(S,Row,RowOddEven).

solvable1(S,_,1):-inversionsI(S,0,Number) , Even is mod(Number,2),! , Even=0 .
solvable1(S,Row,0):-inversionsI(S,0,Number), NumberMod2 is mod(Number,2), ! , findIndexOfValue(S,0,IndexOfBlank,blank),BlankRow is IndexOfBlank // Row ,  BlankMod is mod(BlankRow,2),!, not(BlankMod = NumberMod2).


% Counts the number of inversions as defined in the question
inversionsI([F],Adder,Adder).
inversionsI([blank|L],Adder,Answer):- inversionsI(L,Adder,Answer).
inversionsI([F|L],Adder,Answer):- inversionsJ(L,F,0,AnsI), Adder1 is Adder +AnsI , inversionsI(L,Adder1,Answer).

inversionsJ([],ValueToCompare,Adder,Adder).
inversionsJ([blank],ValueToCompare,Adder,Adder).
inversionsJ([blank|L],ValueToCompare,Adder,Answer):- inversionsJ(L,ValueToCompare,Adder,Answer).
inversionsJ([F|L],ValueToCompare,Adder,Answer):- ValueToCompare > F , Adder1 is Adder + 1,inversionsJ(L,ValueToCompare,Adder1,Answer).
inversionsJ([F|L],ValueToCompare,Adder,Answer):- inversionsJ(L,ValueToCompare,Adder,Answer).

/** ---------------------------------------------------------
  calling the search algorithms
  ---------------------------------------------------------- */

go(ProblemName, HFN) :-
	init(ProblemName, Init),
	solvable(Init),
	astar(Init, successors, goal, HFN, Path, equality),
	writeln(Path).

goCC(ProblemName, HFN) :-
	init(ProblemName, Init),
	solvable(Init),
	astarCC(Init, successors, goal, HFN, Path, equality),
	writeln(Path).

goIDA(ProblemName, HFN) :-
	init(ProblemName, Init),
	solvable(Init),
	idastar(Init, successors, goal, HFN, Path, equality),
	writeln(Path).


