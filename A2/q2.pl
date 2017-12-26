:- op(700 , xfy , > ).
:- op(800 , xfy , ? ).
% | is predefined as xfy with precedence 1100
:- op(1120 , xfy , $ ).

% primitive actions 
primAction(0).

% primitive actions for testing
primAction(a).
primAction(b).
primAction(a1).
primAction(a2).
primAction(a3).

% primitive actions for given test cases
primAction(acquireLock1).
primAction(acquireLock2).
primAction(releaseLock1).
primAction(releaseLock2).
primAction(doSomething).


primAction(produce).
primAction(consume).
primAction(underflow).
primAction(overflow).
primAction(notFull).
primAction(notEmpty).



% given in the assignment with additional cuts to improve efficiency
(A > P) - A - P:-!, primAction(A).
(P1 ? _) - A - PR :- P1 - A - PR .
(_ ? P2) - A - PR :- !, P2 - A -PR.
(P1 | P2) - A - (P1R | P2) :- not(P1=0) , P1 - A - P1R.
(P1 | P2) - A - (P1 | P2R) :-  not(P2=0),! , P2 - A - P2R.
(P1 $ P2) - A - (P1R $ P2R) :-  P1 - A - P1R , P2 - A - P2R.
A - A - 0:- primAction(A),!.
PN - A - PR:- defproc(PN,PB),! , PB - A - PR.


% given in the assignment with additional cuts to improve efficiency
final(0):-!.
final(P1 ? _) :-final(P1).
final(_ ? P2) :-!,final(P2).
final(P1 | P2) :-!, final(P1), final(P2).
final(P1 $ P2) :-!, final(P1), final(P2).
final(P) :- defproc(P,B),!, final(B).

 % ---------------------------------------------------------------

% that holds iff R1 is a complete execution of process P.
% if R1 is not initialized it returns a complete execution of process P.
run(P, R1):-  defproc(P,PB),!, runn(PB,R1).
runn(P, [P]):-   final(P).
runn(P, AP):-  P - A2 - A3 , runn(A3,R1) , append([A2],[R1],A4) , append([P],A4,AP).

 % ---------------------------------------------------------------
 
 
% prints out the given list with linebreaks.
write_list([]).
write_list([H]):- write_list(H),!.
write_list([H|T]):- write(H),nl,write_list(T).


% prints complete execution of process P
print_run(P):- run(P,R) , write_list(R).

 % ---------------------------------------------------------------

 
 
% checks if X is a memeber of the list(second parameter is a list).
memeber(X,[X|_]).
memeber(X,[_|Y]):-memeber(X,Y).

% appends two lists (first two parameters are lists)
% third parameter is the appended list
append([],L,L). 
append([ F | R],L,[F | RL]):- append(R,L,RL).

% checks if there is a procedure in one of execution processes
% i.e if a1|test1 holds sine test1 is a procedure 
% a1|a3 does not hold since both a1 and a3 are primitive actions
chech_procedure(E1>_):-defproc(E1,_),!.
chech_procedure(_>E2):-defproc(E2,_),!.
chech_procedure(E1>_):-chech_procedure(E1),!.
chech_procedure(_>E2):-chech_procedure(E2),!.
chech_procedure((E1) | (_)):- defproc(E1,_),!.
chech_procedure((_) | (E2)):- defproc(E2,_),!.
chech_procedure((E1) | (_)):-  chech_procedure(E1),!.
chech_procedure((_) | (E2)):-  chech_procedure(E2),!.
chech_procedure((E1) ? (_)):- defproc(E1,_),!.
chech_procedure((_) ? (E2)):- defproc(E2,_),!.
chech_procedure((E1) ? (_)):-  chech_procedure(E1),!.
chech_procedure((_) ? (E2)):-  chech_procedure(E2),!.
chech_procedure((E1) $ (_)):- defproc(E1,_),!.
chech_procedure((_) $ (E2)):- defproc(E2,_),!.
chech_procedure((E1) $ (_)):-  chech_procedure(E1),!.
chech_procedure((_) $ (E2)):-  chech_procedure(E2),!.

chech_procedure(A):- defproc(A,_).


% checks if there is at least one procedure in E (the execution process) 
% and if there is then it adds E to L and the answer is L3 (List that has both E and L)
add_list(E,L ,L1):- chech_procedure(E), append([E],L,L1),!.
add_list(_,L ,L).


% checks if all paths are final paths
customfinal(0):-!.
customfinal(P1 ? P2) :-!,customfinal(P1),customfinal(P2).
customfinal(P1 | P2) :-!, customfinal(P1), customfinal(P2).
customfinal(P1 $ P2) :-!, customfinal(P1), customfinal(P2).
customfinal(P) :- defproc(P,B),!, customfinal(B).


 % ---------------------------------------------------------------

% holds iff process P has an infinite run.
has_infinite_run(P):- defproc(P,P).
has_infinite_run(P):- defproc(P,PB),!, infinite_run(PB, [P]).
infinite_run(PB,P):- PB - _ - A2 , not(customfinal(A2)), not(memeber(A2,P)) ,add_list(A2,P,P2), infinite_run(A2,P2).
infinite_run(PB,P):- PB - _ - A2 , memeber(A2,P).
 
 % ---------------------------------------------------------------

% holds iff process P cannot reach a deadlocked configuration.
deadlock_free(P):-  defproc(P,PB),!, not(deadlocks(PB,[P])).
deadlocks(P,_):-  not(P - _ - _ ).
deadlocks(P,L):-  P - _ - A3 , not(customfinal(A3)),  not(memeber( A3, L)),add_list( A3,L ,L1), deadlocks(A3,L1).

 % ---------------------------------------------------------------

% holds iff there is no execution of process P where action A occurs.
cannot_occur(S,A):- defproc(S,PB),!, not(can(PB , [S] , A)) .
can(P, _ , A):-  P - A - _. 
can(P, L , A):-  P - _ - A3 , not(customfinal(A3)),  not(memeber( A3, L)),add_list( A3,L ,L1) , can(A3,L1,A).
% ---------------------------------------------------------------


% holds iff in all executions of process P , whenever action A1 occurs,  action A2 occurs afterwards
whenever_eventually(S,B1,B2):- defproc(S,PB),!, not( sometime(PB,B1,B2,[S] ) ).
sometime(P,B1,B2,_):- P - B1 - A3 , A3 - B3 - _ , not(B2=B3).
sometime(P, B1 , B2, L ):- P - _ - A3 , not(customfinal(A3)) , not(memeber( A3, L)),add_list( A3,L ,L1), sometime(A3,B1,B2,L1).

% ---------------------------------------------------------------


% simple test cases. 
defproc(test,a1 ).
defproc(testt0, a1 ? a2).
defproc(testt1, a1 > a2? a1).
defproc(test0,a1 | a2).
defproc(test1,a1 > a2 > a3).
defproc(test2,(a1 > a2) | a3).
defproc(test22, a3 | (a1 > a2) ).
defproc(test3,a1 $ a1 ).	
defproc(test4,a1 $ a2 ).	
defproc(test5, user1 $ user1 ).	
defproc(test6, 0 ?  a1 > a2 >  test6).
defproc(test1s0, acquireLock1 > test1s1 ).
defproc(test1s1, releaseLock1 > test1s0 ).
defproc(test7, test6).
defproc(test8, a > b $ a > c).
defproc(test9, a | b $ b|a).
defproc(d, acquireLock2 > releaseLock1 |  acquireLock1 >  releaseLock2 $ lock1s1 | lock2s1).
defproc(dd,   acquireLock2 > acquireLock1> releaseLock1 >  releaseLock2 | acquireLock2 > releaseLock2 > releaseLock1 $ lock1s1 | lock2s0).
defproc(ddd, acquireLock1 > acquireLock2 > releaseLock2 > releaseLock1 |  acquireLock2 > acquireLock1> releaseLock1 >  releaseLock2 $ lock1s0 | lock2s0).
defproc(dddd, 0|0 $ lock1s0 | lock2s0 | iterDoSomething).


defproc(deadlockingSystem, user1 | user2 $ lock1s0 | lock2s0 | iterDoSomething).
defproc(user1, acquireLock1 > acquireLock2 > doSomething > releaseLock2 > releaseLock1).
defproc(user2, acquireLock2 > acquireLock1 > doSomething > releaseLock1 > releaseLock2).
defproc(lock1s0, acquireLock1 > lock1s1 ? 0).
defproc(lock1s1, releaseLock1 > lock1s0).
defproc(lock2s0, acquireLock2 > lock2s1 ? 0).
defproc(lock2s1,releaseLock2 > lock2s0).
defproc(iterDoSomething, doSomething > iterDoSomething ? 0).
defproc(oneUserSystem, user1 $ lock1s0 | lock2s0 | iterDoSomething).




defproc(producerConsumerSyst,producer | consumer | faults $ bufferS0).
defproc(producer, notFull > produce > producer).
defproc(consumer, notEmpty > consume > consumer).
defproc(faults, underflow ? overflow).
defproc(bufferS0, notFull > produce > bufferS1 ? produce > bufferS1 ? consume > underflow > bufferUF).
defproc(bufferUF, notFull > produce > bufferUF ? produce > bufferUF ? consume > bufferUF).
defproc(bufferS1, notFull > produce > bufferS2 ? produce > bufferS2 ? consume > bufferS0 ? notEmpty > consume > bufferS0).
defproc(bufferS2, notFull > produce > bufferS3 ? produce > bufferS3 ? consume > bufferS1 ? notEmpty > consume > bufferS1).
defproc(bufferS3, produce > overflow > bufferOF ? consume > bufferS2 ? notEmpty > consume > bufferS2).
defproc(bufferOF, produce > bufferOF ?  consume > bufferOF ? notEmpty > consume > bufferOF).
defproc(producerConsumerSystBuggy, producerB | consumerB | faults $ bufferS0).
defproc(producerB, produce > producerB).
defproc(consumerB, consume > consumerB).















