% EECS 3401 Fall 2020 Assignment 3 Starter Code for Question 2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2 Part A

%%% Primitive action declarations

%go from location X to location Y in the same room
primitive_action(go(X,Y)).
%push box from location X to location Y in the same room
primitive_action(push(B,X,Y)).
%Climb up onto box B
primitive_action(climbUp(B)).
%Climb down from box B
primitive_action(climbDown(B)).
%Turn on switch
primitive_action(turnOn(SW)).
%Turn off switch
primitive_action(turnOff(SW)).






%%% Preconditions for Primitive Actions


poss(go(X,Y), S ) :- in(X,R), in(Y,R), loc(robot,X,S), ((loc(B,X,S), box(B), onTop(B,S), !, fail);true).

poss(push(B,X,Y),S) :- in(X,R), in(Y,R), loc(robot,X,S), box(B), loc(B,X,S),
                        ((onTop(B,S),!,fail);true),
                        ((box(B2),loc(B2,Y,S),!,fail);true).

poss(climbUp(B),S) :- box(B), loc(B,Location,S),loc(robot,Location,S), not(onTop(B,S)).

poss(climbDown(B),S) :- box(B), loc(B,Location,S), loc(robot,Location,S), onTop(B,S).

poss(turnOn(SW),S) :- controls(L,SW), box(B), loc(B,Location,S), loc(robot,Location,S), loc(SW,Location,S),onTop(B,S),off(L,S).

poss(turnOff(SW),S) :- controls(L,S), box(B), loc(B,Location,S), loc(robot,Location,S), loc(SW,Location,S),onTop(B,S),on(L,S),up(SW,S).


%%% Successor State Axioms for Primitive Fluents

% Pattern:
% myfluent(Arg, do(A,S)) :- /* positive effects */ ;
%                           myfluent(Arg, S), not (/*negative effecs */).

loc(robot,Location,do(A,S)):- (loc(robot,X,S), in(X,R),in(Location,R),box(B), A = push(B,X,Location));
                              (loc(robot,X,S),in(Location,R), A = go(X,Location));
                              (loc(robot,Location,S)).
loc(B,Location,do(A,S)):- box(B),(loc(B,Start,S),in(Start,R),in(Location,R),A = push(B,Start,Location));
                          (loc(B,Location,S)).

onTop(Box,do(A,S)):- box(Box), (onTop(Box,S), not(A = climbDown(Box)));
                     (loc(Box,Location,S), loc(robot,Location,S), A = climbUp(Box));
                     (onTop(Box,S)).
up(Sw,do(A,S)) :- controls(Light,Sw), loc(Sw,Location,S),loc(Box,Location,S),(onTop(Box,S),off(Light,S),A = turnOn(Sw));
                  (onTop(Box,S),on(Light,S),up(Sw,S),not(A=turnOff(Sw))); (on(Light,S),up(Sw,S)).

on(Light,do(A,S)) :- controls(Light,Sw), loc(Sw,Location,S),loc(Box,Location,S),box(B),(onTop(Box,S), off(Light,S),A = turnOn(Sw));
                      (onTop(Box,S),on(Light,S),up(Sw,S), not(A= turnOff(Sw)));
                      (on(Light,S)).
off(Light,do(A,S)) :- controls(Light,Sw), loc(Sw,Location,S),loc(Box,Location,S),box(B),(onTop(Box,S),on(Light,S),A = turnOff(Sw));
                      (onTop(Box,S), off(Light,S),not(A = turnOn(Sw)));
                      (off(Light,S)).




%%% Defined Fluents
% Feel free to define your own fluents here as needed.





%%% Non-Fluent Predicates
% Describe static facts here, like the names and types of objects in the world,
% their locations, which location is in which room, etc.
%
%
%
%
/*
 * -------------------------------------------------------------||
 *                       corridor                               ||
 *_____door1________door2___________door3___________door4_______||
 *11|12|13|14|15||26|27|28|29|30||41|42|43|44|45||56|57|58|59|60||
 *6 |7 |8 |9 |10||21|22|23|24|25||36|37|38|39|40||51|52|53|54|55||
 *1 |2 |3 |4 |5 ||16|17|18|19|20||31|32|33|34|35||46|47|48|49|50||
 *     Room1    ||    Room2     ||   Room3      ||   Room4      ||
 *     switch 1 is at 11
 *     switch 2 is at 26
 *     swtich 3 is at 41
 *     switch 4 is at 56
 *     Shakey is at 37
 *     Door1 is at 13
 *     Door2 is at 27
 *     Door3 is at 42
 *     Door4 is at 57
 *     Box 1 is at 9
 *     Box 2 is at 7
 *     Box 3 is at 2
 *     Box 4 is at 4
*/




%%% Initial Situation
% Define which *fluents* are true in situation s0
%boxLoc(box1,initBox1Loc,s0). % Etc.
% ...
%door location
in(13,corridor).
in(27,corridor).
in(42,corridor).
in(57,corridor).

%room-door location
room(r1,13).
room(r2,27).
room(r3,42).
room(r4,57).

locInitRobot(37).
loc(robot,X,s0) :- locInitRobot(X).


up(s1,s0).
on(l1,s0).
up(s4,s0).
on(l4,s0).

off(l2,s0).
off(l3,s0).

box(b1).
box(b2).
box(b3).
box(b4).

%box location
loc(b1,9,s0).
loc(b2,7,s0).
loc(b3,2,s0).
loc(b4,4,s0).

%switch location
loc(s1,11,_).
loc(s2,26,_).
loc(s3,42,_).
loc(s4,56,_).

%map light to switches
controls(l1,s1).
controls(l2,s2).
controls(l3,s3).
controls(l4,s4).

%map all locations
%room1
in(1,r1). in(2,r1). in(3,r1). in(4,r1). in(5,r1).
in(6,r1). in(7,r1). in(8,r1). in(9,r1). in(10,r1).
in(11,r1). in(12,r1). in(13,r1). in(14,r1). in(15,r1).
%room2
in(16,r2). in(17,r2). in(18,r2). in(19,r2). in(20,r2).
in(21,r2). in(22,r2). in(23,r2). in(24,r2). in(25,r2).
in(26,r2). in(27,r2). in(28,r2). in(29,r2). in(30,r2).
%room3
in(31,r3). in(32,r3). in(33,r3). in(34,r3). in(35,r3).
in(36,r3). in(37,r3). in(38,r3). in(39,r3). in(40,r3).
in(41,r3). in(42,r3). in(43,r3). in(44,r3). in(45,r3).
%room4
in(46,r4). in(47,r4). in(48,r4). in(49,r4). in(50,r4).
in(51,r4). in(52,r4). in(53,r4). in(54,r4). in(55,r4).
in(56,r4). in(57,r4). in(58,r4). in(59,r4). in(60,r4).

% Restore suppressed situation arguments.
% Needed by GOLOG for technical purposes.
% Update if you are introducing additional fluents.

restoreSitArg(loc(O,L),S,loc(O,L,S)).
restoreSitArg(up(Switch),S, up(Switch,S)).
restoreSitArg(on(L),S, on(L,S)).
restoreSitArg(off(L), S,off(L,S)).
restoreSitArg(onTop(B),S,onTop(B,S)).
% do the same for the remaining fluents


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 2 Part B
%

ifBoxMove :- executable(do(push(b2,13,27),do(push(b2,7,13),do(go(13,7), do(go(42,13),do(go(37,42),s0)))))).

ifRobotMove :- do(go(37,42):go(42,13):go(13,7):push(b2,7,13):push(b2,13,27),s0,S1),goalPartB(S1).

solPartB :- ifBoxMove,ifRobotMove.

%Goal_Predicate
goalPartB(S) :- loc(b2,L,S), in(L,r2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 2 Part C

proc(getBox2toRoom2,
     %if shakey is not on top of a box
     if( loc(robot,L1) & in(L1,R1) & room(R1,D1) & loc(b2,L2) & in(L2,R2) & room(R2,D2) & room(r2,D3) &
         -onTop(B),

     % go to to room 1 where box 2 is and push it to room 2 (action)
         go(L1,D1) : go(D1,D2) : go(D2,L2) : push(b2,L2,D2) : push(b2,D2,D3),

     %else, climbDown then do (action)
         climbDown(B) : go(L1,D1) : go(D1,D2) : go(D2:L2) : push(b2,L2,D2) : push(b2,D2,D3)
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

proc(allLightsOn,
    if(-onTop(B), goToLoc: goTurnOnLights,climbDown(B) : goturnOnLights)).

proc(goTurnonLights,
   % while light is off
     while(off(Light) & controls(Light,Switch) & in(L1,R1) & loc(Switch,L1) & loc(B,L2) & box(B) & loc(robot,L2) & in(L2,R2) &room(R1,D1)
           &room(R2,D2),
           push(B,L2,D2) :
           push(B,D2,D1) :
           push(B,D1,L1) :
           climbUp(B) :
           turnOn(Switch) :
           climbDown(B)
     )
    ).
proc(goToLoc,
     %select location of a box
     ?(loc(robot,L1) & in(L1,R) & room(R,D) & loc(B,L2) & box(B) & in(L2,R2) &room(R2,DX))
     :
     go(L1,D) : go(D,DX) : go(DX,L2)
    ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 2 Part E

acceptable(A,S) :- (loc(robot,L,S),loc(b2,L,S),box(B),not(onTop(B,S)),in(L,R),in(M,R),A= push(b2,L,M));
                   (loc(robot,L,S), not(onTop(B,S)),box(B),in(L,R), in(M,R), A = go(L,M));
                   (onTop(B,S), box(B), A = climbDown(B)).

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
