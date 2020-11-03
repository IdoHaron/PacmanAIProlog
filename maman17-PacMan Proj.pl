/***********************************************
*Mamam17 - Final Project - PacMan              *
*20596 - Prolog & Artificial Intelligence      *
*----------------------------------------------*
*Programmer - Yuval Miller & Ido Haron         *
*ID - 301449120                                *
************************************************
*To run the game type the following goal:      *
*   ?- pacman.                                 *
*on the console.                               *
***********************************************/



/******************************************************
* List of dynamic utllity predicates & data structure *
******************************************************/

:- dynamic coordinate/1.     % coordinate(I,J), indices of a specific slot on board.
:- dynamic slot/3.	     % slot(BoardID,coordinate(I,J),ValueOnThisSlot).
:- dynamic next_board_no/1.  % numerator for managing grid id's.
:- dynamic player_stuck/1.   % flag indicating this player has no legal moves.
:- dynamic game_stat/6.	     % game_stat(BoardID, score, pacman_cord, ghost1_cord, ghost2_cord, ghost3_cord), BoardID = 0 is game board.
:- dynamic game_level/1.     % numerator for difining the game level.
:- dynamic biscits/4.        % biscits(Number of biscits on board, ghost1 on biscit, ghost2 on biscit, ghost3 on biscit).
:- dynamic def_board/1.  	% saves last saved board -> 0 is the original board.
:- dynamic max_score/1  	% max game score
/*****************************
* List of clues for the game *
*****************************/

%Translating player choice of move to coordinate influance.
directions(l,-1,0).
directions(r,1,0).
directions(u,0,1).
directions(d,0,-1).

%Translate items on board to their value.
scores('o',10).
scores('c',200).
scores(' ',0).



/*****************************************************
* cleanup - clear memory from all dynamic predicates *
*****************************************************/

pacman:-
	cleanup,
	initiate_board,
	initiate_game,
	print_board(1),
	select_play,
	cleanup,
	!.



/*****************************************
* Interface to select:			 *
*         1 - For computer Vs. player.   *
*         2 - For computer Vs. computer. *
*****************************************/

select_play:-
	write('Welcome to out game of PACMAN!'), nl,
	write('READY to have some FUN?!'), nl,
	write('READY to have some FUN?!'), nl,
	write('Select play mode:'), nl,
	write('  1 - Computer vs Player'), nl,
	write('  2 - Computer vs Computer'), nl,
	repeat,
	read(PlayMode),
	(integer(PlayMode), PlayMode >= 1, PlayMode =< 2 ;
	write('Illegal Input, Pick Again:'), nl, fail),
	(   PlayMode =:= 1,
	    select_level
	;
	    auto_pacman(PlayMode)).



/*********************************
* Interface to select:	         *
*         1 - For beginner mode. *
*         2 - For advanced mode. *
*         3 - For expert mode.   *
*********************************/

select_level:-
        write('Select a level:'), nl,
	write('   1 - Beginner'), nl,
	write('   2 - Advanced'), nl,
	write('   3 - Expert'), nl,
	repeat,
	read(Level),
	((integer(Level), Level >= 1, Level =< 3,
	 TempLevel is 4 + Level,
	 assert(game_level(TempLevel)),
	 write('OK, lets DO THIS!'), nl,
	 write('At any time choose e to exit the game.'), nl,
	 play_pacman(1)) ;
	write('Illegal Input, Pick Again:'), nl, fail),
	nl.



/**********************************************
* Main interface for computer Vs Player game: *
*         l\r\u\d - For a move.		      *
*         e - For exiting the game.	      *
**********************************************/

play_pacman(BoardNo):-
	write('Select where to move(l,r,u,d):'), nl,
	assert(def_board(BoardNo)),
	read(Move),
	not(check_exit(BoardNo,Move)),
	(   move(BoardNo,Move),
	    print_board(BoardNo),
	    print_score(BoardNo),
	    biscits(Val,_,_,_),
	    write(Val),nl,
	    play_pacman(BoardNo)	)
	;
	(
	    print_board(BoardNo),
	    print_score(BoardNo),
	    biscits(Val,_,_,_),
	    write(Val),nl,
	    play_pacman(BoardNo)
	).



/************************************************
* Main interface for computer Vs computer game. *
************************************************/

auto_pacman(BoardNo):-
	BoardNo.



/*************************************************
* Terminate the game if player chose e for exit. *
*************************************************/

check_exit(BoardNo,Move):-
	Move = 'e',
	write('Thank you for playing PACMAN!'),nl,
	write('This is the last game statues:'),nl,
	print_board(BoardNo),
	print_score(BoardNo),
	cleanup,
	break.



/*****************************************
* retracting all of the dynamic utility. *
*****************************************/

cleanup:-
	retractall(coordinate(_)),
	retractall(slot(_,_,_)),
	retractall(next_board_no(_)),
	retractall(player_stuck(_)),
        retractall(game_stat(_,_,_,_,_,_)),
	retractall(game_level(_)),
	retractall(biscits(_,_,_,_))
	.



/********************************
* Initiating the PAC-MAN board. *
* Overloading initiate_board.   *
********************************/

initiate_board:-
	initiate_board(1,1,1),
	assert(next_board_no(2)),
	assert(biscits(185,1,1,1))
	.



/********************************
* Initiating the PAC-MAN board. *
********************************/

initiate_board(BoardNo,Xcord,Ycord):-


%First is creating the board and fill all slots with 'o':
	(
	    Xcord=<21,
	    assert(slot(BoardNo,coordinate(Xcord,Ycord),'o')),
	    NewXcord is Xcord + 1,
	    initiate_board(BoardNo,NewXcord,Ycord)
        )
        ;
        (
	    Ycord=<19,
	    NewYcord is Ycord + 1,
	    initiate_board(BoardNo,1,NewYcord)

        )
	;

%Second is creating the walls in the board:
	(
	    initiate_walls(BoardNo),
	    initiate_empty_cells(BoardNo),
	    writeln('Board initiated'),nl
	).




/*************************************************************
* Initiating the PAC-MAN board walls by pre-determent cords. *
*************************************************************/

initiate_walls(BoardNo):-

%Initiating the horizontal walls:

%Row 2:
	initiate_horizontal_walls(BoardNo,2,9,2),
	initiate_horizontal_walls(BoardNo,13,20,2),

%Row 4:
	initiate_horizontal_walls(BoardNo,1,2,4),
	initiate_horizontal_walls(BoardNo,8,14,4),
	initiate_horizontal_walls(BoardNo,20,21,4),

%Row 6:
	initiate_horizontal_walls(BoardNo,2,4,6),
	initiate_horizontal_walls(BoardNo,6,9,6),
	initiate_horizontal_walls(BoardNo,13,16,6),
	initiate_horizontal_walls(BoardNo,18,20,6),

%Row 8:
	initiate_horizontal_walls(BoardNo,8,14,8),
	initiate_horizontal_walls(BoardNo,1,4,8),
	initiate_horizontal_walls(BoardNo,18,21,8),

%Row 9:
	initiate_horizontal_walls(BoardNo,1,4,9),
	initiate_horizontal_walls(BoardNo,18,21,9),

%Row 10:
	initiate_horizontal_walls(BoardNo,8,14,10),
	initiate_horizontal_walls(BoardNo,1,4,10),
	initiate_horizontal_walls(BoardNo,18,21,10),

%Row 11:
	initiate_horizontal_walls(BoardNo,8,14,11),

%Row 12:
	initiate_horizontal_walls(BoardNo,8,14,12),
	initiate_horizontal_walls(BoardNo,1,4,12),
	initiate_horizontal_walls(BoardNo,18,21,12),

%Row 13:
	initiate_horizontal_walls(BoardNo,1,4,13),
	initiate_horizontal_walls(BoardNo,18,21,13),

%Row 14:
	initiate_horizontal_walls(BoardNo,6,9,14),
	initiate_horizontal_walls(BoardNo,13,16,14),
        initiate_horizontal_walls(BoardNo,1,4,14),
	initiate_horizontal_walls(BoardNo,18,21,14),


%Row 16:
	initiate_horizontal_walls(BoardNo,2,4,16),
	initiate_horizontal_walls(BoardNo,8,14,16),
	initiate_horizontal_walls(BoardNo,18,20,16),

%Row 18:
	initiate_horizontal_walls(BoardNo,2,4,18),
	initiate_horizontal_walls(BoardNo,6,9,18),
	initiate_horizontal_walls(BoardNo,13,16,18),
	initiate_horizontal_walls(BoardNo,18,20,18),

%Row 19:
	initiate_horizontal_walls(BoardNo,2,4,19),
	initiate_horizontal_walls(BoardNo,6,9,19),
	initiate_horizontal_walls(BoardNo,13,16,19),
	initiate_horizontal_walls(BoardNo,18,20,19),

%Initiating the diagonal walls:

%Column 4:
	initiate_diagonal_walls(BoardNo,4,5,4),

%Column 6:
	initiate_diagonal_walls(BoardNo,3,4,6),
	initiate_diagonal_walls(BoardNo,8,10,6),
	initiate_diagonal_walls(BoardNo,12,16,6),

%Column 11:
	initiate_diagonal_walls(BoardNo,2,3,11),
	initiate_diagonal_walls(BoardNo,6,7,11),
	initiate_diagonal_walls(BoardNo,14,15,11),
	initiate_diagonal_walls(BoardNo,18,20,11),

%Column 16:
	initiate_diagonal_walls(BoardNo,3,4,16),
	initiate_diagonal_walls(BoardNo,8,10,16),
	initiate_diagonal_walls(BoardNo,12,16,16),
	initiate_diagonal_walls(BoardNo,4,5,18)
	.



/*******************************************************************
* Initiating the PAC-MAN board empty cells by pre-determent cords. *
*******************************************************************/

initiate_empty_cells(BoardNo):-

%Initiating the horizontal empty cells:

%Row 9:
	initiate_horizontal_empty_cells(BoardNo,7,15,9),

%Row 11:
	initiate_horizontal_empty_cells(BoardNo,1,4,11),
	initiate_horizontal_empty_cells(BoardNo,6,7,11),
	initiate_horizontal_empty_cells(BoardNo,15,16,11),
	initiate_horizontal_empty_cells(BoardNo,18,21,11),

%Row 13:
	initiate_horizontal_empty_cells(BoardNo,7,15,13),

%Initiating the diagonal empty cells:

%Column 7:
	initiate_diagonal_empty_cells(BoardNo,8,13,7),

%Column 10:
	initiate_diagonal_empty_cells(BoardNo,13,14,10),

%Column 12:
	initiate_diagonal_empty_cells(BoardNo,13,14,12),

%Column 15:
	initiate_diagonal_empty_cells(BoardNo,8,13,15)
	.



/*************************************************
* Initiating the PAC-MAN board horizontal walls. *
* ***********************************************/

initiate_horizontal_walls(BoardNo,From,To,Row):-
	    (	From =< To,
		retract(slot(BoardNo,coordinate(From,Row),_)),
		assert(slot(BoardNo,coordinate(From,Row),'\u25A0')),
		NewFrom is From + 1,
		initiate_horizontal_walls(BoardNo,NewFrom,To,Row)
	    );
	    !
	    .



/***********************************************
* Initiating the PAC-MAN board diagonal walls. *
* *********************************************/

initiate_diagonal_walls(BoardNo,From,To,Column):-
            (	From =< To,
		retract(slot(BoardNo,coordinate(Column,From),_)),
		assert(slot(BoardNo,coordinate(Column,From),'\u25A0')),
		NewFrom is From + 1,
		initiate_diagonal_walls(BoardNo,NewFrom,To,Column)
	    );
	    !
	    .



/*******************************************************
* Initiating the PAC-MAN board horizontal empty cells. *
* *****************************************************/

initiate_horizontal_empty_cells(BoardNo,From,To,Row):-
	    (	From =< To,
		retract(slot(BoardNo,coordinate(From,Row),_)),
		assert(slot(BoardNo,coordinate(From,Row),' ')),
		NewFrom is From + 1,
		initiate_horizontal_empty_cells(BoardNo,NewFrom,To,Row)
	    );
	    !
	    .



/*****************************************************
* Initiating the PAC-MAN board diagonal empty cells. *
* ***************************************************/

initiate_diagonal_empty_cells(BoardNo,From,To,Column):-
            (	From =< To,
		retract(slot(BoardNo,coordinate(Column,From),_)),
		assert(slot(BoardNo,coordinate(Column,From),' ')),
		NewFrom is From + 1,
		initiate_diagonal_empty_cells(BoardNo,NewFrom,To,Column)
	    );
	    !
	    .




/*************************************************************
* Initiating the PAC-MAN game:                               *
*	      1 - Initiating the game_stat dynamic utility.  *
*	      2 - Initiating the players in the board slots. *
* ***********************************************************/

initiate_game:-
	assert(game_stat(1,0,coordinate(11,5),coordinate(1,20),coordinate(12,20),coordinate(21,20))),

	retract(slot(BoardNo,coordinate(11,5),_)),
	assert(slot(BoardNo,coordinate(11,5),'P')),

	retract(slot(BoardNo,coordinate(1,20),_)),
	assert(slot(BoardNo,coordinate(1,20),'G')),
	retract(slot(BoardNo,coordinate(12,20),_)),
	assert(slot(BoardNo,coordinate(12,20),'G')),
	retract(slot(BoardNo,coordinate(21,20),_)),
	assert(slot(BoardNo,coordinate(21,20),'G'))
	.



/***************************
* Overloading print_board. *
***************************/

print_board(BoardNo):-
	print_board(BoardNo,1,20)
	.


/***********************************
* Printing the board row after row *
***********************************/

print_board(BoardNo,Xcord,Ycord):-
	(
	    Xcord=<21,
	    slot(BoardNo,coordinate(Xcord,Ycord),Val),
	    NewXcord is Xcord + 1,
            write(Val),
	    print_board(BoardNo,NewXcord,Ycord)
        )
        ;
        (
	    Ycord>=1,
	    nl,
	    NewYcord is Ycord - 1,
	    print_board(BoardNo,1,NewYcord)

        )
	;
	(
	    nl,
	    write('Board printed'),nl
	).



/**********************
* Printing the score. *
**********************/

print_score(BoardNo):-
	game_stat(BoardNo,Score,coordinate(_,_),_,_,_),
	write('Your score is: '),
	write(Score),
	nl
	.



/************************************
* Validate a move:		    *
*      1 - True if legal move.      *
*      2 - False if not legal move. *
************************************/
not_wall(BoardID, coordinate(X,Y)):-
	slot(BoardID, coordinate(X, Y),Val), 
	not(member(Val,['\u25A0','P','G'])).

ligal_place(BoardID, coordinate(X,Y)):-
	slot(BoardID, coordinate(X, Y),Val),
	not(member(Val,['\u25A0','P','G'])).
ligal_cords(coordinate(X,Y)):-
	X =< 21,
	X >=1,
	Y =< 20,
	Y >=1,
move_validate(BoardNo,coordinate(NewX,NewY)):-
	ligal_cords(coordinate(NewX,NewY)),
	ligal_place(BoardNo, NewX, NewY).



move(BoardNo,Move):-
	move_validate(BoardNo,Move),!
	,
	make_move(BoardNo,Move)
	.




make_move(BoardNo,Move):-
	retract(game_stat(BoardNo,Score,coordinate(Xcord,Ycord),Ghost1,Goast2,Ghoast3)),
	move(Move,X,Y),
	NewX is Xcord + X,
	NewY is Ycord + Y,
	retract(slot(BoardNo,coordinate(NewX,NewY),Val)),
	score(Val,AddScore),
	NewScore is Score + AddScore,
	assert(game_stat(BoardNo,NewScore,coordinate(NewX,NewY),Ghost1,Goast2,Ghoast3)),
	assert(slot(BoardNo,coordinate(NewX,NewY),'P')),
	retract(slot(BoardNo,coordinate(Xcord,Ycord),_)),
	assert(slot(BoardNo,coordinate(Xcord,Ycord),' ')),
	Val='o',
	retract(biscits(Val2,_,_,_)),
	NewVal2 is Val2 -1,
	assert(biscits(NewVal2,_,_,_))
	.


/*

make_ghosts_move(BoardNo,Move1,Move2,Move3):-
	retract(game_stat(BoardNo,Score,_,coordinate(X1,Y1),coordinate(X2,Y2),coordinate(X3,Y3))),
	move(Move,X,Y),
	NewX is Xcord + X,
	NewY is Ycord + Y,
	retract(slot(BoardNo,coordinate(NewX1,NewY1),Val)),
	assert(game_stat(BoardNo,NewScore,coordinate(NewX,NewY),coordinate(NewX1,NewY1),coordinate(NewX2,NewY2),coordinate(NewX3,NewY3))),
	assert(slot(BoardNo,coordinate(NewX1,NewY1),'G')),
	retract(slot(BoardNo,coordinate(Xcord,Ycord),_)),
	assert(slot(BoardNo,coordinate(Xcord,Ycord),' '))
	.


alphabeta(CurrentPos,Alpha,Beta,BestSuccessorPos,Val,DepthLevel,MaxDepth,Level):-

	% if not, calculate them from scratch
	(((nonvar(Alpha),nonvar(Beta)) ; (Alpha is -999999, Beta is 999999)), % init +-infinity
	 moves(CurrentPos,PosList,DepthLevel,MaxDepth),!,
	 NewDepthLevel is DepthLevel+1,	% update depth level
	 boundedbest(PosList,Alpha,Beta,BestSuccessorPos,Val,NewDepthLevel,MaxDepth,Level),
	 % save result to database for saving calculation runtime in the future
	 assert(pos_evaluation(HashKey,ActualDepth,BestSuccessorPos,Val)))
	;
	staticval(CurrentPos,Val,Level). % terminal seatchtree node - evaluate directly

boundedbest([Pos|PosList],Alpha,Beta,GoodPos,GoodVal,DepthLevel,MaxDepth,Level):-
	alphabeta(Pos,Alpha,Beta,_,Val,DepthLevel,MaxDepth,Level),
	goodenough(PosList,Alpha,Beta,Pos,Val,GoodPos,GoodVal,DepthLevel,MaxDepth,Level).

goodenough([],_,_,Pos,Val,Pos,Val,_,_,_):- !.	% No other candidate

goodenough(_,Alpha,Beta,Pos,Val,Pos,Val,_,_,_):-
	min_to_move(Pos),Val > Beta, !          % Maximizer attained upper bound
	;
	max_to_move(Pos),Val < Alpha, !.        % Minimizer attained lower bound

goodenough(PosList,Alpha,Beta,Pos,Val,GoodPos,GoodVal,DepthLevel,MaxDepth,Level):-
	newbounds(Alpha,Beta,Pos,Val,NewAlpha,NewBeta),   % Refine bounds
	boundedbest(PosList,NewAlpha,NewBeta,Pos1,Val1,DepthLevel,MaxDepth,Level),
	betterof(Pos,Val,Pos1,Val1,GoodPos,GoodVal).

/* define new interval by 2 last arguments that is narrower or equal to old interval */
/*
newbounds(Alpha,Beta,Pos,Val,Val,Beta):-
	min_to_move(Pos),Val > Alpha, !.       % Maximizer increased lower bound

newbounds(Alpha,Beta,Pos,Val,Alpha,Val):-
   max_to_move(Pos),Val < Beta, !.         % Minimizer decreased upper bound

newbounds(Alpha,Beta,_,_,Alpha,Beta).      % Otherwise bounds unchanged

% betterof(Pos,Val,Pos1,Val1,Pos,Val)     % Pos better than Pos1
betterof(Pos,Val,_,Val1,Pos,Val):-      % Pos better than Pos1
	min_to_move(Pos),Val > Val1, !
	;
	max_to_move(Pos), Val < Val1, !.

betterof(_,_,Pos1,Val1,Pos1,Val1).         % Otherwise Pos1 better

*/
copy_boards_row(Board1,Board2, coordinate(X, Y)):- % copies each row it recieves
	ligal_cords(coordinate(X, Y)),
	retract(slot(Board1, X,Y), _), retract(slot(Board2, X, Y), New),
	assert(slot(Board1, X, Y), New), assert(slot(Board2, X, Y), New),
	X is X+1, copy_boards_row(Board1,Board2, coordinate(X, Y)).

copy_boards_line(Board1,Board2, coordinate(X, Y)):- % runs on each colum, and copies each row using external func
	ligal_cords(coordinate(X, Y)),
	copy_boards_row(Board1, Board2, coordinate(X, Y)), 
	Y is Y+1, copy_boards_line(Board1,Board2, coordinate(X, Y)) . 

copy_boards(Board1,Board2):-
	retract(game_stat(Board1, S, P, G1,G2,G3)), assert(game_stat(Board2, S, P, G1,G2,G3)), assert(game_stat(Board1, S, P, G1,G2,G3)),
	copy_boards_line(Board1, Board2, coordinate(1,1)).

move_cords(BoardID, coordinate(OrigX, OrigY), coordinate(X, Y)):-
	retract(slot(BoardID, OrigX, OrigY), Origin), retract(slot(BoardID, X, Y), New),
	assert(slot(BoardID, OrigX, OrigY), New), assert(slot(BoardID, X, Y), Origin).

possible_move(BoardID, IsGhost, coordinate(X, Y), coordinate(Z, W)):-
	Z is X, W is Y,
	directions(Dir, XAdder, YAdder), % choosing a direction.
	(
		X is X+ XAdder, Y is Y + YAdder,
		not(IsGhost), not(ligal_place(BoardID, coordinate(X, Y))), !; %% if there is a wall there, not out of bound use the original cords
		move_validate(BoardID, coordinate(X,Y)), move_cords(BoardID, coordinate(Z,W), coordinate(X,Y)), Z is X, W is Y
	 ).

single_move(BoardID, IsGhost, PossibleMove):- %returns a single ligal move
	retract(game_stat(BoardID, _, P, G1, G2,G3)), retract(def_board(NewBoard)), NewBoard is NewBoard+1,
	copy_boards(BoardID, NewBoard), assert(def_board(NewBoard)),
	 (
		IsGhost, (possible_move(NewBoard, IsGhost,G1, N_G1), possible_move(NewBoard, IsGhost,G2, N_G2),possible_move(NewBoard, IsGhost,G3, N_G3)),
	 	PossibleMove is NewBoard, ! % returns a list of lists each containing original and new cords.
		;
		( possible_move(NewBoard, IsGhost, P, N_P) ), PossibleMove is NewBoard
	). 
 
moves(BoardID, IsGhostTurn, AllPossibleMoves):-
	setof(R, single_move(BoardID, IsGhostTurn, R), AllPossibleMoves). % returns all possible moves in this turn

ghost_avreage_dis(Board, Dis):-
	retract(game_stat(Board, Score, P, G1, G2, G3)), assert(game_stat(Board, Score, P, G1, G2, G3)),
	Dis is float((abs(float((P-G1)/2))+abs(float((P-G2)/2))+abs(float((P-G3)/2)))/3)
	.
ghost_rectangel(Board, Rec):-.

pacman_in_cannal_step(Board, coordinate(X, Y)):-
	retract(turn_num(TurnsLeft)), TurnsLeft>=0, assert(turn_num(TurnsLeft)), TurnsTaken is 0,
	retract(game_stat(Board, Score, P, coordinate(X1, Y1), coordinate(X2, Y2), coordinate(X3, Y3))), assert(game_stat(Board, Score, P, coordinate(X1, Y1), coordinate(X2, Y2), coordinate(X3, Y3))),
	(
		(X=X1, Y=Y1; X =X2, Y = Y2 ; X = X3, Y = Y3)
		;
		W is X+1, not_wall(Board, coordinate(W, Y)), retract(turn_num(TurnsLeft)), TurnsLeft is TurnsLeft-TurnsTaken, assert(turn_num(TurnsLeft)),
		TurnsTaken is TurnsTaken+1, pacman_in_cannal_step(Board, coordinate(W, Y))
		;
		W is X-1, not_wall(Board, coordinate(W, Y)), retract(turn_num(TurnsLeft)), TurnsLeft is TurnsLeft-TurnsTaken, assert(turn_num(TurnsLeft)),
		TurnsTaken is TurnsTaken+1, pacman_in_cannal_step(Board, coordinate(W, Y))
		;
		Z is Y+1, not_wall(Board, coordinate(W, Y)), retract(turn_num(TurnsLeft)), TurnsLeft is TurnsLeft-TurnsTaken, assert(turn_num(TurnsLeft)),
		TurnsTaken is TurnsTaken+1, pacman_in_cannal_step(Board, coordinate(X, Z))
		;
		Z is Y-1, not_wall(Board, coordinate(W, Y)), retract(turn_num(TurnsLeft)), TurnsLeft is TurnsLeft-TurnsTaken, assert(turn_num(TurnsLeft)),
		TurnsTaken is TurnsTaken+1, pacman_in_cannal_step(Board, coordinate(X, Z))
	).
pacman_in_cannal(Board, IsInCannal):-
	retract(game_stat(Board, Score, P, G1, G2, G3)), assert(game_stat(Board, Score, P, G1, G2, G3)),
	pacman_in_cannal_step(Board, P), IsInCannal = 1, !
	;
	IsInCannal = 0
	.

staticval(Board, Val):-
	retract(game_level(Level)), assert(game_level(Level)),
	retract(max_score(Max)), assert(max_score(Max)),
	retract(game_stat(Board, Score, P, G1, G2, G3)), assert(game_stat(Board, Score, P, G1, G2, G3)),
	(
		Score = Max, Val is 1000, !
		; 
		(P = G1 ; P= G2; P = G3), Val is -1000, !
		;
		Level=1, ghost_avreage_dis(Board, Dis), Val is 0.6*Score+4*Dis, !
		;
		Level=2,ghost_avreage_dis(Board, Dis),ghost_rectangel(Board, Rec), Val is 0.6*Score +3*Dis - Rec, !
		;
		Level=3,ghost_avreage_dis(Board, Dis),ghost_rectangel(Board, Rec),pacman_in_cannal(Board, IsInCannal) Val is 0.6*Score +3*Dis - Rec -100*IsInCannal, !
	).
%% alphabeta

ghosts_to_move(pos(_,1,_)). % true iff it's MAX turn to move  (x player)
pacman_to_move(pos(_,2,_)). % true iff it's MIN turn to move  (o player)




alphabeta(CurrentPos,Alpha,Beta,BestSuccessorPos,Val,DepthLevel):-

	(
	   (
              (nonvar(Alpha),nonvar(Beta));
              (Alpha is -999999, Beta is 999999) % init +-infinity
            ),
	 game_level(Level),
         MaxDepth is Level + 2,
         moves(CurrentPos,PosList,DepthLevel,MaxDepth),!,
	 NewDepthLevel is DepthLevel+1,	% update depth level
	 boundedbest(PosList,Alpha,Beta,BestSuccessorPos,Val,NewDepthLevel)
	)
	;
	staticval(CurrentPos,Val). % terminal seatchtree node - evaluate directly

boundedbest([Pos|PosList],Alpha,Beta,GoodPos,GoodVal,DepthLevel):-
	alphabeta(Pos,Alpha,Beta,_,Val,DepthLevel),
	goodenough(PosList,Alpha,Beta,Pos,Val,GoodPos,GoodVal,DepthLevel).

goodenough([],_,_,Pos,Val,Pos,Val,_):- !.	% No other candidate

goodenough(_,Alpha,Beta,Pos,Val,Pos,Val,_):-
	ghosts_to_move(Pos),Val > Beta, !          % Maximizer attained upper bound
	;
	pacman_to_move(Pos),Val < Alpha, !.        % Minimizer attained lower bound

goodenough(PosList,Alpha,Beta,Pos,Val,GoodPos,GoodVal,DepthLevel):-
	newbounds(Alpha,Beta,Pos,Val,NewAlpha,NewBeta),   % Refine bounds
	boundedbest(PosList,NewAlpha,NewBeta,Pos1,Val1,DepthLevel),
	betterof(Pos,Val,Pos1,Val1,GoodPos,GoodVal).

/* define new interval by 2 last arguments that is narrower or equal to old interval */
newbounds(Alpha,Beta,Pos,Val,Val,Beta):-
	ghosts_to_move(Pos),Val > Alpha, !.       % Maximizer increased lower bound

newbounds(Alpha,Beta,Pos,Val,Alpha,Val):-
	pacman_to_move(Pos),Val < Beta, !.         % Minimizer decreased upper bound

newbounds(Alpha,Beta,_,_,Alpha,Beta).      % Otherwise bounds unchanged

% betterof(Pos,Val,Pos1,Val1,Pos,Val)     % Pos better than Pos1
betterof(Pos,Val,_,Val1,Pos,Val):-      % Pos better than Pos1
	ghosts_to_move(Pos),Val > Val1, !
	;
	pacman_to_move(Pos), Val < Val1, !.

betterof(_,_,Pos1,Val1,Pos1,Val1).         % Otherwise Pos1 better

