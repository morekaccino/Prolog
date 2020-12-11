% EECS 3401 Fall 2020 Assignment 3 Starter Code for Question 2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2 Part A

%%% Primitive action declarations

primitive_action(go(Loc1,Loc2)).
primitive_action(push(B,Loc1,Loc2)).
primitive_action(climbUp(B)).
primitive_action(climbDown(B)).
primitive_action(turnOn(Sw)).
primitive_action(turnOff(Sw)).
%primitive_action(passThrough(Door)).



%%% Preconditions for Primitive Actions

poss(go(Loc1,Loc2), S) :-
	robotLoc(Loc1, S),
	onTop(floor, S),
	in(Loc1, R),
	in(Loc2, R).

poss(push(B,Loc1,Loc2), S) :-
	box(B),
	in(Loc1, R),
	in(Loc2, R),
	robotLoc(Loc1, S),
	boxLoc(B, Loc1, S).

poss(climbUp(B), S) :-
	box(B),
	boxLoc(B, Loc1, S),
	robotLoc(Loc1, S),
	onTop(floor, S).

poss(climbDown(B), S) :-
	box(B),
	boxLoc(B, Loc1, S),
	robotLoc(Loc1, S),
	onTop(B, S).

poss(turnOn(Sw), S) :-
	box(B),
	onTop(B, S),
	boxLoc(B, Loc, S),
	switchLoc(Sw, Loc),
	controls(Sw, Light).
%	not(onn(Light, S)).

poss(turnOff(Sw), S) :-
	box(B),
	onTop(B, S),
	boxLoc(B, Loc, S),
	switchLoc(Sw, Loc),
	controls(Sw, Light).
%	onn(Light, S).
	
%poss(passThrough(Door), S) :-
%	door(Door),
%	robotLoc(Loc, S),
%	doorLoc(Door, DoorLoc),
%	Loc = DoorLoc.
	




%%% Successor State Axioms for Primitive Fluents

% Pattern:
% myfluent(Arg, do(A,S)) :- /* positive effects */ ;
%                           myfluent(Arg, S), not (/*negative effecs */).

robotLoc(Loc, do(A, S)) :-
	A = go(_, Loc); A = push(B, _, Loc), box(B);
	robotLoc(Loc, S), not(A = go(_, _)).

boxLoc(Box, Loc, do(A, S)) :-
	A = push(Box, _, Loc), box(Box);
	boxLoc(Box, Loc, S), not(A = push(Box, _, _)).
	
onTop(B, do(A, S)) :-
	A = climbUp(B); A = climbDown(B);
	onTop(B, S), not(A = climbUp(B)), not(A = climbDown(B)).

up(Sw, do(A, S)) :-
	A = turnOn(Sw);
	up(Sw, S), not(A = turnOn(Sw)).
	
onn(Light, do(A, S)) :-
	controls(Sw, Light), light(Light), A = turnOn(Sw);
	onn(Light, S), not(A = turnOn(Sw)), controls(Sw, Light), light(Light).


%%% Defined Fluents
% Feel free to define your own fluents here as needed.




%%% Non-Fluent Predicates
% Describe static facts here, like the names and types of objects in the world,
% their locations, which location is in which room, etc.

box(box1).
box(box2).
box(box3).
box(box4).

room(room1).
room(room2).
room(room3).
room(room4).
room(corridor).

door(door1).
door(door2).
door(door3).
door(door4).

switch(sw1).
switch(sw2).
switch(sw3).
switch(sw4).

light(light1).
light(light2).
light(light3).
light(light4).

location(initBox1Loc).
location(initBox2Loc).
location(initBox3Loc).
location(initBox4Loc).
location(initShakeyLoc).
location(initBox1Loc).

location(door1Loc).
location(door2Loc).
location(door3Loc).
location(door4Loc).

location(sw1Loc).
location(sw2Loc).
location(sw3Loc).
location(sw4Loc).

in(initBox1Loc,room1).
in(initBox2Loc,room1).
in(initBox3Loc,room1).
in(initBox4Loc,room1).
in(initShakeyLoc,room3).

in(door1Loc, room1).
in(door2Loc, room2).
in(door3Loc, room3).
in(door4Loc, room4).
in(door1Loc, corridor).
in(door2Loc, corridor).
in(door3Loc, corridor).
in(door4Loc, corridor).

in(door1, room1).
in(door2, room2).
in(door3, room3).
in(door4, room4).
in(door1, corridor).
in(door2, corridor).
in(door3, corridor).
in(door4, corridor).

in(room1centre, room1).
in(room2centre, room2).
in(room3centre, room3).
in(room4centre, room4).

in(sw1Loc, room1).
in(sw2Loc, room2).
in(sw3Loc, room3).
in(sw4Loc, room4).

doorLoc(door1, door1Loc).
doorLoc(door2, door2Loc).
doorLoc(door3, door3Loc).
doorLoc(door4, door4Loc).

switchLoc(sw1, sw1Loc).
switchLoc(sw2, sw2Loc).
switchLoc(sw3, sw3Loc).
switchLoc(sw4, sw4Loc).

controls(sw1, light1).
controls(sw2, light2).
controls(sw3, light3).
controls(sw4, light4).

%%% Initial Situation
% Define which *fluents* are true in situation s0

boxLoc(box1,initBox1Loc, s0).
boxLoc(box2,initBox2Loc, s0).
boxLoc(box3,initBox3Loc, s0).
boxLoc(box4,initBox4Loc, s0).
robotLoc(initShakeyLoc, s0).
onTop(floor, s0).
up(sw1, s0).
up(sw4, s0).
onn(light1, s0).
onn(light4, s0).

% Restore suppressed situation arguments.
% Needed by GOLOG for technical purposes.
% Update if you are introducing additional fluents.

restoreSitArg(robotLoc(L), S, robotLoc(L, S)).
restoreSitArg(boxLoc(B, L), S, boxLoc(B, L, S)).
restoreSitArg(onTop(B), S, onTop(B, S)).
restoreSitArg(up(Sw), S, up(Sw, S)).
restoreSitArg(onn(L), S, onn(L, S)).

% do the same for the remaining fluents


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 2 Part B

% goal of having box2 in room2
% A simple condition with respect to situation S
goalPartB(S) :- boxLoc(box2, Loc, S), in(Loc, room2).


% solPartB(S) binds S to a ground situation term that achieves goalPartB(S)
solPartB(S):- S = do(push(box2, door2Loc, room2centre), do(push(box2, door1Loc, door2Loc), do(push(box2, initBox2Loc, door1Loc), do(go(door1Loc, initBox2Loc), do(go(door3Loc, door1Loc), do(go(initShakeyLoc, door3Loc), s0)))))).

%go(initShakeyLoc, door3Loc)
%go(door3Loc, door1Loc)
%go(door1Loc, initBox2Loc)
%push(box2, initBox2Loc, door1Loc)
%push(box2, door1Loc, door2Loc)
%push(box2, door2Loc, room2centre)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 2 Part C

proc(getBox2toRoom2,   % a Golog procedure that implements the plan in B
	pi(currentLocation, ?(robotLoc(currentLocation)) : 
		go(currentLocation, door3Loc) : 
		go(door3Loc, door1Loc) :
		go(door1Loc, initBox2Loc) :
		push(box2, initBox2Loc, door1Loc) :
		push(box2, door1Loc, door2Loc) :
		push(box2, door2Loc, room2centre)
	)
).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 2 Part D

% To implement this part, you need to get somewhat comfortable
% with the syntax of GOLOG. Refer to the definitions in the interpreter
% and the last section of the Assignment 3 handout.
% To keep the program intelligible, feel free to define additional procedures
% such as
%    - goTurnOnLight(L)   % Given a light, go and turn it on
%    - getBoxToLoc(B)     % Given a box, get to its location
%    - pushBoxToLoc(B,L)  % Given a box and a location, move the box there
%    - goToLoc(L)         % Given a location, get there
% and so on. These procedures abstract away from the primitive actions
% and serve as building block for the even more abstract procedure "allLightsOn".


% Given a light, it will turn it on
proc(goTurnOnLight(L),
	pi(s, ?(switch(s) & controls(s, L)) : 
		pi(slocation, ?(switchLoc(s, slocation)) : 
			pi(b, ?(box(b)) :
				pushBoxToLoc(b, slocation) : 
				climbUp(b) :
				turnOn(s)
			)
		)
	)
).


% Given a room, it goes in it
proc(goToRoom(R),
	pi(currentLocation, ?(robotLoc(currentLocation)) : 
		pi(currentRoom, ?(in(currentLocation, currentRoom)) : 
			if(R = currentRoom,
				?(true),
				pi(exitdoor, ?(door(exitdoor) & in(exitdoor, currentRoom)) : 
					pi(enterdoor, ?(door(enterdoor) & in(enterdoor, R)) : 
						pi(exitdoorlocation, ?(doorLoc(exitdoor, exitdoorlocation)) :
							pi(enterdoorlocation, ?(doorLoc(enterdoor, enterdoorlocation)) : 
								go(currentLocation, exitdoorlocation) : 
								go(exitdoorlocation, enterdoorlocation)
							)
						)
					)
				)
			)
		)
	)
).


% if on the box, it will get down
proc(getDown,
	if(some(b, box(b) & onTop(b)),
		pi(b, ?(box(b) & onTop(b)) :
			climbDown(b)
		),
		?(true)
	)
).

% given a location, it will go to it
proc(goToLoc(L),
	getDown :
	pi(roomtogo, ?(in(L, roomtogo)) :
		goToRoom(roomtogo) : 
		pi(currentLocation, ?(robotLoc(currentLocation)) : 
			if(currentLocation = L,
				?(true),
				go(currentLocation, L)
			)
		)
	)
).


% given a box, it will get to its location
proc(getBoxToLoc(B),
	pi(boxlocation, ?(box(B) & boxLoc(B, boxlocation)) :
		goToLoc(L)
	)
).


% given a box and a room, it will push the box to the room
proc(pushBoxToRoom(B,R),
	getBoxToLoc(B) :
		pi(currentLocation, ?(robotLoc(currentLocation)) : 
			pi(currentRoom, ?(in(currentLocation, currentRoom)) : 
				if(R = currentRoom,
					?(true),
					pi(exitdoor, ?(door(exitdoor) & in(exitdoor, currentRoom)) : 
						pi(enterdoor, ?(door(enterdoor) & in(enterdoor, R)) : 
							pi(exitdoorlocation, ?(doorLoc(exitdoor, exitdoorlocation)) :
								pi(enterdoorlocation, ?(doorLoc(enterdoor, enterdoorlocation)) : 
									push(B, currentLocation, exitdoorlocation) : 
									push(B, exitdoorlocation, enterdoorlocation)
								)
							)
						)
					)
				)
			)
		)

).

% given a box and a location, it will push the box to the location
proc(pushBoxToLoc(B,L),
	pi(roomtogo, ?(in(L, roomtogo)) :
		pushBoxToRoom(B, roomtogo) :
			pi(currentLocation, ?(robotLoc(currentLocation)) :
				push(B, currentLocation, L)
			)
	)
).

% makes sure all lights are on by checking for any down switch
proc(allLightsOn,
	while(some(s, switch(s) & -up(s)),
		pi(s, ?(switch(s) & -up(s)) : 
			pi(light, ?(controls(s, light)) :
				goTurnOnLight(light)
			)
		)
	)
).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 2 Part E

acceptable(A,S) :- 
	boxLoc(box2, Loc, S),
	robotLoc(RobLoc, S),
	not(RobLoc = Loc),
	A = go(_, _).

acceptable(A,S) :-
	boxLoc(box2, Loc, S),
	robotLoc(RobLoc, S),
	(RobLoc = Loc),
	A = push(box2, _, _).

restoreSitArg(acceptable(A),S,acceptable(A,S)).

% For testing???
goal(S) :- goalPartB(S).
restoreSitArg(goal,S,goal(S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% added by Yves Lesperance

% Pretty printing of situations
show_act_seq(s0).
show_act_seq(do(A,S)):- show_act_seq(S), write(A), nl.

% definition of executable (legal) situation
executable(s0).
executable(do(A,S)) :- poss(A,S), executable(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
