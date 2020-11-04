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
:- dynamic game_stat/10.     % game_stat(BoardID, score, pacman_cord, ghost1_cord, ghost2_cord, ghost3_cord, ghost1 on biscit, ghost2 on biscit, ghost3 on biscit, Number of biscits left on board), BoardID = 0 is game board.
:- dynamic game_level/1.     % numerator for difining the game level.


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
	print_board(0),
	start_game,
	cleanup,
	!.


/*********************************
* Interface to select:	         *
*         1 - For beginner mode. *
*         2 - For advanced mode. *
*         3 - For expert mode.   *
*********************************/

start_game:-
	write('Welcome to our game of PACMAN!'), nl,
	write('READY to have some FUN?!'), nl,
	write('READY to have some FUN?!'), nl,
	write('Select a level:'), nl,
	write('   1 - Beginner'), nl,
	write('   2 - Advanced'), nl,
	write('   3 - Expert'), nl,
	repeat,
	read(Level),
	((integer(Level), Level >= 1, Level =< 3,
	 assert(game_level(Level)),
	 write('OK, lets DO THIS!'), nl,
	 write('At any time choose e to exit the game.'), nl,
	 play_pacman(0)) ;
	write('Illegal Input, Pick Again:'), nl, fail),
	nl.



/**********************************************
* Main interface for computer Vs Player game: *
*         l\r\u\d - For a move.		      *
*         e - For exiting the game.	      *
**********************************************/

play_pacman(BoardNo):-
	write('Select where to move(l,r,u,d):'), nl,
	read(Move),
        not(check_exit(BoardNo,Move)),
	(   move(BoardNo,Move),
	    print_board(BoardNo),
	    print_score(BoardNo),
	    play_pacman(BoardNo)
        )
	;
	(
	    print_board(BoardNo),
	    print_score(BoardNo),
	    play_pacman(BoardNo)
	)
.


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
        retractall(game_stat(_,_,_,_,_,_,_,_,_,_)),
	retractall(game_level(_))
	.



/********************************
* Initiating the PAC-MAN board. *
* Overloading initiate_board.   *
********************************/

initiate_board:-
	initiate_board(0,1,1),
	assert(next_board_no(1))
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
	assert(game_stat(0,0,coordinate(11,5),coordinate(1,20),coordinate(12,20),coordinate(21,20),1,1,1,185)),

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
	game_stat(BoardNo,Score,coordinate(_,_),_,_,_,_,_,_,_),
	write('Your score is: '),
	write(Score),
	nl
	.



/************************************
* Validate a move:		    *
*      1 - True if legal move.      *
*      2 - False if not legal move. *
************************************/

move_validate(BoardNo,Move):-
	member(Move,[l,r,u,d]),
	directions(Move,MoveX,MoveY),
	game_stat(BoardNo,_,coordinate(X,Y),_,_,_,_,_,_,_),
	NewX is X + MoveX,
	NewY is Y + MoveY,
	NewX =< 21,
	NewX >=1,
	NewY =< 20,
	NewY >=1,
	slot(BoardNo,coordinate(NewX,NewY),Val),
	not(member(Val,['\u25A0'])).



move(BoardNo,Move):-
	move_validate(BoardNo,Move),!
	,
	make_move(BoardNo,Move)
	.




make_move(BoardNo,Move):-
	retract(game_stat(BoardNo,Score,coordinate(Xcord,Ycord),Ghost1,Goast2,Ghoast3,Ghosts1OnB,Ghosts2OnB,Ghosts3OnB,NoOfBiscits)),
	directions(Move,X,Y),
	NewX is Xcord + X,
	NewY is Ycord + Y,
	retract(slot(BoardNo,coordinate(NewX,NewY),Val)),
	scores(Val,AddScore),
	NewScore is Score + AddScore,
	assert(game_stat(BoardNo,NewScore,coordinate(NewX,NewY),Ghost1,Goast2,Ghoast3,Ghosts1OnB,Ghosts2OnB,Ghosts3OnB,NoOfBiscits)),
	assert(slot(BoardNo,coordinate(NewX,NewY),'P')),
	retract(slot(BoardNo,coordinate(Xcord,Ycord),_)),
	assert(slot(BoardNo,coordinate(Xcord,Ycord),' '))
	.
