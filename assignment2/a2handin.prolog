/** ---------------------------------------------------------

EECS 3401 Fall 2020 Assignment 2

Family name: Sakib

Given name: Sadman

Student number: 215916232



---------------------------------------------------------- */

/* load the three search algorithms */
:- ensure_loaded('astar.prolog').
:- ensure_loaded('astarCC.prolog').
:- ensure_loaded('idastar.prolog').

/* ------------------------------------------------------- */

/* successors( +State, -Neighbors)

   Neighbors is a list of elements (Cost, NewState) where
   NewState is a state reachable from State by one action and
   Cost is the cost for that corresponding action (=1 in our
   case)
*/

successors(S,NS) :- bagof(N,succs(S,N),NS).

succs(S,NS) :- newDown(S,N), NS = (1,N).
succs(S,NS) :- newLeft(S,N), NS = (1,N).
succs(S,NS) :- newRight(S,N), NS = (1,N).
succs(S,NS) :- newUp(S,N), NS = (1,N).


newDown([E,F|R], NS) :- (is_list(E), member(0,E)), !,
	down(X,0,[E, F|R]),
	replace(0,X,E,R1),
	replace(X,0,F,R2),
	append([R1],[R2],R3),
	append(R3,R,NS).
newDown([E,F|R], NS) :- newDown([F|R],R1),append([E],R1,NS).

down(X,Y,[E,F|_]) :- (is_list(E), member(Y,E), nth0(I1,E,Y)),!, is_list(F), member(X,F),nth0(I2,F,X), I1 = I2.
down(X,Y,[_|R]):- down(X,Y,R).

newLeft([F|R],NS) :- (is_list(F),member(0,F)), !, left(X,0,[F|R]),
	swap(0,X,F,R1),append([R1],R,NS).
newLeft([F|R],NS) :- newLeft(R,R1), append([F],R1,NS).

left(X,Y,[X,Y|_]).
left(X,Y,[F|_]) :- (is_list(F), member(Y,F)), left(X,Y,F).
left(X,Y,[_|R]) :- left(X,Y,R).

newRight([F|R],NS) :- (is_list(F), member(0,F)), !, right(X,0,[F|R]),
	swap(0,X,F,R1), append([R1],R,NS).
newRight([F|R],NS) :- newRight(R,R1),append([F],R1,NS).

right(X,Y,[Y,X|_]).
right(X,Y,[F|_]):- (is_list(F),member(Y,F)),!,right(X,Y,F).
right(X,Y,[_|R]):- right(X,Y,R).

newUp([E,F|R],NS) :- (is_list(F),member(0,F)), !, up(X,0,[E,F|R]),
	replace(X,0,E,R1), replace(0,X,F,R2),
	append([R1],[R2],R3), append(R3,R,NS).
newUp([F|R],NS):- newUp(R,R1),append([F],R1,NS).


up(X,Y,[E,F|_]):- (is_list(F), member(Y,F),nth0(I1,F,Y)),!,is_list(E), member(X,E),
	nth0(I2,E,X), I1 = I2.
up(X,Y,[_|R]):-  up(X,Y,R).




/* ------------------------------------------------------- */


/* equality(+S1, +S2)

   holds if and only S1 and S2 describe the same state
*/
equality([], []).
equality([H1|K1], [H2|K2]) :-
    H1 = H2,
    equality(K1, K2).


/* ------------------------------------------------------- */

/* hfn_null( +State, -V)

   V is the null heuristic for State (=0 irrelevant of the state)
*/
hfn_null(_State, 0).



/* hfn_misplaced( +State, -V)

   V is the number of misplaced tiles in State
*/
hfn_misplaced(S,V) :- length(S,J), Max is J*J, goal_state(J,J,Max,1,G), flatten(S,S1), flatten(G,G1), count(S1,G1,L), length(L,Vn), V is Max-Vn.


/* hfn_manhattan( +State, -V)

   V is the sum over the manhattan distances between the current
   and the designated position of each tile
*/

hfn_manhattan( S, V ) :- length(S,J), Max is J*J, goal_state(J,J,Max,1,G), dist(Max,J,J,1,S,G,L), manhatadd(L,V).




/* ------------------------------------------------------- */


/* init( +Name, -State)

   State is the initial state for problem Name
*/

init(a,[[1,2,3],[4,8,5],[0,7,6]]) :- a = [[1,2,3].[4,8,5],[0,7,6]].

init(b,[[8,2,6],[4,1,5],[0,7,3]]) :- b = [[8,2,6],[4,1,5],[0,7,3]].

init(c,[[0,2,6],[4,1,5],[8,7,3]]) :- c = [[0,2,6],[4,1,5],[8,7,3]].

init(d,[[1,2,3,4],[5,6,7,8],[9,10,0,15],[13,12,11,14]]) :- d = [[1,2,3,4],[5,6,7,8],[9,10,0,15],[13,12,11,14]].


/* ------------------------------------------------------- */

/* goal( +State )
   holds if and oly if State is a goal state
*/

goal(S) :- length(S,J),Max is J*J, goal_state(J,J,Max,1,G), equality(S,G).





/* ------------------------------------------------------- */






/** ---------------------------------------------------------
  calling the search algorithms
  ---------------------------------------------------------- */

go(ProblemName, HFN) :-
	init(ProblemName, Init),
	astar(Init, successors, goal, HFN, Path, equality),
	writeln(Path).

goCC(ProblemName, HFN) :-
	init(ProblemName, Init),
	astarCC(Init, successors, goal, HFN, Path, equality),
	writeln(Path).

goIDA(ProblemName, HFN) :-
	init(ProblemName, Init),
	idastar(Init, successors, goal, HFN, Path, equality),
	writeln(Path).

/*
 Helper Functions
*/

post([H|_],Int,Col,Row,J,N) :- nth1(Col,H,Int),Row is N-J+1,!.
post([_|R],Int,Col,Row,J,N) :- J>0,J1 is J-1,pos(R,Int,Col,Row,J1,N),!.
post(_,_,_,_,0,_).

swap(E1,E2,[E1,E2|R],[E2,E1|R]).
swap(E1,E2,[E2,E1|R],[E1,E2|R]).
swap(E1,E2,[A|RI],[A|RO]) :- swap(E1,E2,RI,RO), !.

replace([X|T],X,Y,[Y|L]) :- replace(T,X,T,L), !.
replace([H|T],X,Y,[H|L]) :- replace(T,X,Y,L).
replace([],_,_,[]).

dist(Loop,J,N,Int,S,G,[Dist|L]) :- Loop>1,Loop1 is Loop-1, post(S,Int,ColS,RowS,J,N),
	post(G,Int,ColG,RowG,J,N), X is abs(ColS-ColG), Y is abs(RowS-RowG), Dist is X+Y,
	Int1 is Int+1,dist(Loop1,J,N,Int1,S,G,L).
dist(1,_,_,_,_,_,[]).

manhatadd([H|L],X) :- manhatadd(L,X1), X is X1+H.
manhatadd([],0).

count([I|S],[I|G],[I|L]) :- count(S,G,L), !.
count([_|S],[_|G],L) :- count(S,G,L), !.
count([],[],[]).

goal_state(N,M,Max,Uni,[H|L]) :-  N>0, N1 is N-1, increment(M,Max,Uni,H), Uni1 is Uni+M,
	goal_state(N1,M,Max,Uni1,L).
goal_state(0,_,_,_,[]).

increment(N,Max,Uni,[Uni|L]) :- N>1,N1 is N-1, Uni1 is Uni+1, incr(N1,Max,Uni1,L), !.
increment(1,Max,Max,[0|[]]).
increment(1,_,Uni,[Uni|[]]).
