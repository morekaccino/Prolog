/** ---------------------------------------------------------

EECS 3401 Fall 2020 Assignment 2

Family name: 

Given name: 

Student number: 


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
swapblank(As,I,J,Cs) :-
   same_length(As,Cs),
   append(BeforeI,[AtI|PastI],As),
   append(BeforeI,[AtJ|PastI],Bs),
   append(BeforeJ,[AtJ|PastJ],Bs),
   append(BeforeJ,[AtI|PastJ],Cs),
   length(BeforeI,I),
   length(BeforeJ,J).

up(S, NewS) :- 
	length(S, Len),
	Dim is round(sqrt(Len)),
	nth0(Index, S, 0), 
	Index >= Dim, 
	NewIndex is Index - Dim, 
	swapblank(S, Index, NewIndex, NewS).


down(S, NewS) :- 
	length(S, Len),
	Dim is round(sqrt(Len)),
	nth0(Index, S, 0), 
	Index < (Len - Dim), 
	NewIndex is Index + Dim, 
	swapblank(S, Index, NewIndex, NewS).

left(S, NewS) :- 
	length(S, Len),
	Dim is round(sqrt(Len)),
	nth0(Index, S, 0), 
	Rem is mod(Index, Dim),
	Rem > 0,
	NewIndex is Index - 1, 
	swapblank(S, Index, NewIndex, NewS).


right(S, NewS) :- 
	length(S, Len),
	Dim is round(sqrt(Len)),
	nth0(Index, S, 0), 
	Rem is mod(Index, Dim),
	(Rem + 1) < Dim,
	NewIndex is Index + 1, 
	swapblank(S, Index, NewIndex, NewS).


successorHelper(State, NewState) :-
	(right(State, NewS); left(State, NewS); up(State, NewS); down(State, NewS)), NewState = (1, NewS).

successors(State, Neighbours) :-
	bagof(NewS, successorHelper(State, NewS), Neighbours).




/* ------------------------------------------------------- */


/* equality(+S1, +S2)

   holds if and only S1 and S2 describe the same state
*/
equality(State1, State2) :-
	State1 = State2.



/* ------------------------------------------------------- */

/* hfn_null( +State, -V)

   V is the null heuristic for State (=0 irrelevant of the state)
*/
hfn_null(_State, 0).



/* hfn_misplaced( +State, -V)

   V is the number of misplaced tiles in State
*/
generateGoal(S, Goal) :-
	length(S, Len),
	Elements is Len - 1,
	numlist(1, Elements, BeforeZero),
	append(BeforeZero, [0], Goal).

hfn_misplacedHelper([], [], List, V) :-
	length(List, Len),
	V is Len.
	
hfn_misplacedHelper([SH | STail], [GH | GTail], List, V) :-
	(GH \= SH, SH \= 0) -> append(List, [1], NewList), hfn_misplacedHelper(STail, GTail, NewList, V);
	hfn_misplacedHelper(STail, GTail, List, V).

hfn_misplaced(State, V) :-
	generateGoal(State, Goal),
	hfn_misplacedHelper(State, Goal, [], V).



/* hfn_manhattan( +State, -V)

   V is the sum over the manhattan distances between the current
   and the designated position of each tile
*/
% hfn_manhattan( State, C ) :-  ...

list_sum([Item], Item).
list_sum([Item1,Item2 | Tail], Total) :-
	Sum is Item1 + Item2,
	list_sum([Sum|Tail], Total).


hfn_manhattanHelper(_, _, [], _, List, V) :-
	list_sum(List, V).
	
hfn_manhattanHelper(State, Goal, [GH | GTail], Dim, List, V) :-
	nth0(IndexG, Goal, GH),
	XofG is div(IndexG, Dim),
	YofG is mod(IndexG, Dim),
	nth0(IndexS, State, GH),
	XofS is div(IndexS, Dim),
	YofS is mod(IndexS, Dim),
	XDiff is abs(XofS - XofG),
	YDiff is abs(YofS - YofG),
	Diff is YDiff + XDiff,
	(GH \= 0) -> append(List, [Diff], NewList), hfn_manhattanHelper(State, Goal, GTail, Dim, NewList, V);
	hfn_manhattanHelper(State, Goal, GTail, Dim, List, V).

hfn_manhattan(State, V) :-
	generateGoal(State, Goal),
	length(State, Len),
	Dim is round(sqrt(Len)),
	hfn_manhattanHelper(State, Goal, Goal, Dim, [], V).



/* ------------------------------------------------------- */


/* init( +Name, -State)

   State is the initial state for problem Name
*/
init(a, [1,2,3,4,8,5,0,7,6]).
init(b, [8,2,6,4,1,5,0,7,3]).
init(c, [0,2,6,4,1,5,8,7,3]).
init(d, [1,2,3,4,5,6,7,8,9,10,0,15,13,12,11,14]).


/* ------------------------------------------------------- */

/* goal( +State )

   holds if and oly if State is a goal state
*/


/*
ordered([]) .
ordered([_]) .
ordered([X,Y|Z]) :- X =< Y , ordered( [Y|Z] ) .

replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

goal(S) :- length(S, Len), replace(0, Len, S, Sreplaced), ordered(Sreplaced).
*/

goal(S) :-
	generateGoal(S, Goal),
	S = Goal.


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
