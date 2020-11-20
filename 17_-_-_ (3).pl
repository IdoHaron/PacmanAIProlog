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

:- dynamic coordinate/1.         % coordinate(I,J), indices of a specific slot on board.
:- dynamic slot/3.	         % slot(BoardID,coordinate(I,J),ValueOnThisSlot).
:- dynamic next_board_no/1.      % numerator for managing grid id's.
:- dynamic game_stat/10.         % game_stat(BoardID, score, pacman_cord, ghost1_cord, ghost2_cord, ghost3_cord, ghost1 on biscit, ghost2 on biscit, ghost3 on biscit, Number of biscits left on board), BoardID = 0 is game board.
:- dynamic game_level/1.         % numerator for difining the game level.



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



/*************************************
* Main process for the PAC-MAN game. *
*************************************/

pacman:-
	cleanup,
	initiate_board,
	initiate_game,
	print_board(0),
	start_game,
	cleanup,!
.


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
	(
	    (integer(Level), Level >= 1, Level =< 3,
	     assert(game_level(Level)),
	     write('OK, lets DO THIS!'), nl,
	     write('At any time choose e to exit the game.'), nl,
	     play_pacman(0)
	 )
	;
	write('Illegal Input, Pick Again:'), nl, fail),nl
.



/**************************************************
* play_pacman(-BoardNo)                           *
* BoardNo is the main board for the game to play. *
* Main interface for computer Vs Player game:     *
*         l\r\u\d - For a move.		          *
*         e - For exiting the game.	          *
**************************************************/

play_pacman(BoardNo):-
	write('Select where to move(l,r,u,d):'), nl,
	read(Move),
        not(check_exit(BoardNo,Move)),
	(   move_pacman(BoardNo,Move),
	    not(pacman_win(BoardNo)),
	    not(ghosts_win(BoardNo)),
	    alphabeta(BoardNo,_,_,BestSuccessorBoardNo,_,0),
	    copy_zero_board(BestSuccessorBoardNo),
	    print_board(BoardNo),
	    print_score(BoardNo),!,
	    not(ghosts_win(BoardNo)),
	    play_pacman(BoardNo)
        )
	;
	(
	    not(pacman_win(BoardNo)),
	    not(ghosts_win(BoardNo)),
	    alphabeta(BoardNo,_,_,BestSuccessorBoardNo,_,0),
	    copy_zero_board(BestSuccessorBoardNo),
	    print_board(BoardNo),
	    print_score(BoardNo),
	    not(ghosts_win(BoardNo)),
	    play_pacman(BoardNo)
	)
.


/*************************************************
* check_exit(-BoardNo,Move).			 *
* Terminate the game if player chose e for exit. *
* Printing the stats of the BoardNo at exit.     *
*************************************************/

check_exit(BoardNo,Move):-
	Move = 'e',
	write('Thank you for playing PACMAN!'),nl,
	write('This is the last game statues:'),nl,
	print_board(BoardNo),
	print_score(BoardNo),
	cleanup,
	break
.


/*****************************************************
* cleanup - clear memory from all dynamic predicates *
*****************************************************/


cleanup:-
	retractall(coordinate(_)),
	retractall(slot(_,_,_)),
	retractall(next_board_no(_)),
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



/********************************************
* initiate_board(+BoardNo,+Xcord,+Ycord)    *
* Initiating the PAC-MAN default board.     *
* Building the board in a recurcive manner. *
********************************************/

initiate_board(BoardNo,Xcord,Ycord):-


%First is creating the board and fill all slots with 'o':
	(
	    Xcord=<21,
	    assert(slot(BoardNo,coordinate(Xcord,Ycord),'o')),
	    NewXcord is Xcord + 1,!,
	    initiate_board(BoardNo,NewXcord,Ycord)
        )
        ;
        (
	    Ycord=<19,
	    NewYcord is Ycord + 1,!,
	    initiate_board(BoardNo,1,NewYcord)

        )
	;

%Second is creating the walls and empty cells in the board:
	(
	    initiate_walls(BoardNo),
	    initiate_empty_cells(BoardNo),!,
	    writeln('Board initiated'),nl
	)
.




/*********************************************************************
* initiate_walls(+BoardNo)                                           *
* Initiating the PAC-MAN board walls by pre-determent cords in board *
* defined by BoardNo                                                 *
* *******************************************************************/

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



/*********************************************************************
* initiate_empty_cells(+BoardNo)				     *
* Initiating the PAC-MAN board empty cells by pre-determent cords in *
* board defined by BoardNo.                                          *
* *******************************************************************/

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



/*****************************************************
* initiate_horizontal_walls(+BoardNo,+From,+To,+Row) *
* Initiating the PAC-MAN board horizontal walls at:  *
* BoardNo - the number of the board                  *
* From - the X-axis value to start the wall          *
* To - the X-axis value to end the wall		     *
* Row - the Y-axis value to build the wall           *
* Working in a rcursive manner.			     *
* ***************************************************/

initiate_horizontal_walls(BoardNo,From,To,Row):-
	    (	From =< To,
		retract(slot(BoardNo,coordinate(From,Row),_)),
		assert(slot(BoardNo,coordinate(From,Row),'\u25A0')),
		NewFrom is From + 1,
		initiate_horizontal_walls(BoardNo,NewFrom,To,Row)
	    )
	    ;
	    !
.



/******************************************************
* initiate_diagonal_walls(+BoardNo,+From,+To,+Column) *
* Initiating the PAC-MAN board diagonal walls.        *
* BoardNo - the number of the board                   *
* From - the Y-axis value to start the wall           *
* To - the Y-axis value to end the wall		      *
* Column - the X-axis value to build the wall	      *
* Working in a rcursive manner.                       *
* ****************************************************/

initiate_diagonal_walls(BoardNo,From,To,Column):-
            (	From =< To,
		retract(slot(BoardNo,coordinate(Column,From),_)),
		assert(slot(BoardNo,coordinate(Column,From),'\u25A0')),
		NewFrom is From + 1,
		initiate_diagonal_walls(BoardNo,NewFrom,To,Column)
	    )
	    ;
	    !
.



/***********************************************************
* initiate_horizontal_empty_cells(+BoardNo,+From,+To,+Row) *
* Initiating the PAC-MAN board horizontal empty cells.     *
* BoardNo - the number of the board			   *
* From - the X-axis value to start empty cells		   *
* To - the X-axis value to end empty cells		   *
* Row - the Y-axis value to initiate empty cells           *
* Working in a rcursive manner.                            *
* *********************************************************/

initiate_horizontal_empty_cells(BoardNo,From,To,Row):-
	    (	From =< To,
		retract(slot(BoardNo,coordinate(From,Row),_)),
		assert(slot(BoardNo,coordinate(From,Row),' ')),
		NewFrom is From + 1,
		initiate_horizontal_empty_cells(BoardNo,NewFrom,To,Row)
	    )
	    ;
	    !
.



/************************************************************
* initiate_diagonal_empty_cells(+BoardNo,+From,+To,+Column) *
* Initiating the PAC-MAN board diagonal empty cells.        *
* BoardNo - the number of the board			    *
* From - the Y-axis value to start empty cells		    *
* To - the Y-axis value to end empty cells		    *
* Column - the X-axis value to initiate empty cells         *
* Working in a rcursive manner.                             *
* **********************************************************/

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
*	      3 - Initiating the ghosts in the board slots.  *
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
	assert(slot(BoardNo,coordinate(21,20),'G')),!
.



/***************************
* print_board(+BoardNo)    *
* Overloading print_board. *
***************************/

print_board(BoardNo):-
	print_board(BoardNo,1,20)
.



/*****************************************
* print_board(+BoardNo,+Xcord,+Ycord)    *
* Printing the board for user interface. *
* Working in a recursive manner.         *
*****************************************/

print_board(BoardNo,Xcord,Ycord):-
	(
	    Xcord=<21,
	    slot(BoardNo,coordinate(Xcord,Ycord),Val),
	    NewXcord is Xcord + 1,
            write(Val),!,
	    print_board(BoardNo,NewXcord,Ycord)
        )
        ;
        (
	    Ycord>=1,
	    nl,
	    NewYcord is Ycord - 1,!,
	    print_board(BoardNo,1,NewYcord)

        )
	;
	(
	    nl,
	    write('Board printed'),nl
	)
.




/***********************************
* print_score(+BoardNo)            *
* Printing the score of BoardNo.   *
***********************************/

print_score(BoardNo):-
	game_stat(BoardNo,Score,coordinate(_,_),_,_,_,_,_,_,_),
	write('Your score is: '),
	write(Score),
	nl
.



/******************************************************************
* copy_zero_board(+BoardNo)                                       *
* Copying BoardNo to board no.0 and initialize next_board_no to 0.*
* ****************************************************************/

copy_zero_board(BoardNo):-
	retract(next_board_no(_)),
	assert(next_board_no(0)),
	copy_board(BoardNo,_)
.



/***********************************
* copy_board(+BoardNo,-NewBoardNo) *
* Copying BoardNo to NewBoardNo.   *
* Overloading copy_board.          *
***********************************/

copy_board(BoardNo,NewBoardNo):-
	next_board_no(NewBoardNo),
	copy_board(BoardNo,1,20,NewBoardNo),
	NewNext is NewBoardNo + 1,
	retract(next_board_no(NewBoardNo)),
	assert(next_board_no(NewNext))
.



/*************************************************
* copy_board(+BoardNo,+Xcord,+Ycord,-NewBoardNo) *
* Copying BoardNo to NewBoardNo slot after slot. *
* Working in a rcursive manner.		         *
*************************************************/

copy_board(BoardNo,Xcord,Ycord,NewBoardNo):-

	(
	    Xcord=<21,
	    slot(BoardNo,coordinate(Xcord,Ycord),Val),
	    (
		retract(slot(NewBoardNo,coordinate(Xcord,Ycord),_));
		true
	    ),
	    assert(slot(NewBoardNo,coordinate(Xcord,Ycord),Val)),

	    NewXcord is Xcord + 1,!,
	    copy_board(BoardNo,NewXcord,Ycord,NewBoardNo)
        )
        ;
        (
	    Ycord>=1,
	    NewYcord is Ycord - 1,!,
	    copy_board(BoardNo,1,NewYcord,NewBoardNo)

        )
	;
	(
	    game_stat(BoardNo,Score,Pacman,Ghost1,Ghost2,Ghost3,Ghost1OnB,Ghost2OnB,Ghost3OnB,NoOfBiscits),
	    (
		retract(game_stat(NewBoardNo,_,_,_,_,_,_,_,_,_));
		true
	    ),
	    assert(game_stat(NewBoardNo,Score,Pacman,Ghost1,Ghost2,Ghost3,Ghost1OnB,Ghost2OnB,Ghost3OnB,NoOfBiscits)),!
	)
.



/***************************************
* spec_validation(+X,+Y)               *
*True if (X,Y) is in the board limits. *
***************************************/
spec_validation(X,Y):-
	X =< 21,
	X >=1,
	Y =< 20,
	Y >=1
.



/*********************************************
* new_cords(+X,+Y,+Move,-NewX,-NewY)         *
* Recieving X,Y cords and Move.	             *
* Return NewX,NewY cords after Move is done. *
*********************************************/

new_cords(X,Y,Move,NewX,NewY):-
	member(Move,[l,r,u,d]),
	directions(Move,MoveX,MoveY),
	NewX is X + MoveX,
	NewY is Y + MoveY
.



/*********************************************
* move_validate_pacman(+BoardNo,+X,+Y)       *
* True if (X,Y) cords are legal pacman move. *
*********************************************/

move_validate_pacman(BoardNo,X,Y):-
	spec_validation(X,Y),
	slot(BoardNo,coordinate(X,Y),Val),
	not(member(Val,['\u25A0']))
.



/********************************************************************
* move_pacman(+BoardNo,+Move)                                       *
* Executing pacman Move in BoardNo if move_validate_pacman is true. *
********************************************************************/

move_pacman(BoardNo,Move):-
	game_stat(BoardNo,_,coordinate(X,Y),_,_,_,_,_,_,_),
	new_cords(X,Y,Move,NewX,NewY),
	move_validate_pacman(BoardNo,NewX,NewY),!
	,
	make_move_pacman(BoardNo,X,Y,NewX,NewY)
.



/****************************************************************
* move_pacman_alpha_beta(+BoardNo,+Move,-NewBoardNo)	        *
* If move_validate_pacman is true, Copying game from BoardNo to *
* NewBoardNo and executing pacman Move in NewBoardNo.	        *
* **************************************************************/

move_pacman_alpha_beta(BoardNo,Move,NewBoardNo):-
	game_stat(BoardNo,_,coordinate(X,Y),_,_,_,_,_,_,_),
	new_cords(X,Y,Move,NewX,NewY),!,
	move_validate_pacman(BoardNo,NewX,NewY),!,
	copy_board(BoardNo,NewBoardNo),!,
	make_move_pacman(NewBoardNo,X,Y,NewX,NewY),!
.


/**********************************************************************
* make_move_pacman(+BoardNo,+X,+Y,+NewX,+NewY)                        *
* Executing pacman Move in BoardNo from X,Y cords to NewX,NewY cords. *
* ********************************************************************/

make_move_pacman(BoardNo,X,Y,NewX,NewY):-
	retract(game_stat(BoardNo,Score,coordinate(X,Y),Ghost1,Goast2,Ghoast3,Ghosts1OnB,Ghosts2OnB,Ghosts3OnB,NoOfBiscits)),
	retract(slot(BoardNo,coordinate(NewX,NewY),Val)),
	(
	scores(Val,AddScore),
	NewScore is Score + AddScore,
	NewNoOfBiscits is NoOfBiscits - 1
	    ;
	NewScore is Score,
	NewNoOfBiscits is NoOfBiscits

	),
	assert(game_stat(BoardNo,NewScore,coordinate(NewX,NewY),Ghost1,Goast2,Ghoast3,Ghosts1OnB,Ghosts2OnB,Ghosts3OnB,NewNoOfBiscits)),
	assert(slot(BoardNo,coordinate(NewX,NewY),'P')),
	retract(slot(BoardNo,coordinate(X,Y),_)),
	assert(slot(BoardNo,coordinate(X,Y),' ')),!
.



/*********************************************
* move_validate_ghosts(+BoardNo,+X,+Y)       *
* True if (X,Y) cords are legal pacman move. *
*********************************************/

move_validate_ghosts(BoardNo,X,Y):-
	spec_validation(X,Y),
	slot(BoardNo,coordinate(X,Y),Val),
	not(member(Val,['\u25A0','G'])),!
.




/********************************************************************
* make_move_ghost1/2/3(+BoardNo,+X,+Y,+NewX,+NewY)		    *
* Executing ghost 1/2/3 Move in BoardNo from X,Y cords to NewX,NewY *
* cords.							    *
* ******************************************************************/

make_move_ghost1(BoardNo,X,Y,NewX,NewY):-
	retract(game_stat(BoardNo,Score,Pacman,coordinate(X,Y),Ghost2,Ghost3,Ghost1OnB,Ghost2OnB,Ghost3OnB,NoOfBiscits)),
	retract(slot(BoardNo,coordinate(NewX,NewY),Val)),
	(   member(Val,['o']),
	    assert(game_stat(BoardNo,Score,Pacman,coordinate(NewX,NewY),Ghost2,Ghost3,1,Ghost2OnB,Ghost3OnB,NoOfBiscits))
	    ;
	    not(member(Val,['o'])),
	    assert(game_stat(BoardNo,Score,Pacman,coordinate(NewX,NewY),Ghost2,Ghost3,0,Ghost2OnB,Ghost3OnB,NoOfBiscits))

	),
	assert(slot(BoardNo,coordinate(NewX,NewY),'G')),
	retract(slot(BoardNo,coordinate(X,Y),_)),

	(   Ghost1OnB=:=1,
	    assert(slot(BoardNo,coordinate(X,Y),'o'))
	    ;
	    Ghost1OnB=:=0,
	    assert(slot(BoardNo,coordinate(X,Y),' '))
	)
.


make_move_ghost2(BoardNo,X,Y,NewX,NewY):-
	retract(game_stat(BoardNo,Score,Pacman,Ghost1,coordinate(X,Y),Ghost3,Ghost1OnB,Ghost2OnB,Ghost3OnB,NoOfBiscits)),
	retract(slot(BoardNo,coordinate(NewX,NewY),Val)),
	(   member(Val,['o']),
	    assert(game_stat(BoardNo,Score,Pacman,Ghost1,coordinate(NewX,NewY),Ghost3,Ghost1OnB,1,Ghost3OnB,NoOfBiscits))
	    ;
	    not(member(Val,['o'])),
	    assert(game_stat(BoardNo,Score,Pacman,Ghost1,coordinate(NewX,NewY),Ghost3,Ghost1OnB,0,Ghost3OnB,NoOfBiscits))

	),
	assert(slot(BoardNo,coordinate(NewX,NewY),'G')),
	retract(slot(BoardNo,coordinate(X,Y),_)),

	(   Ghost2OnB=:=1,
	    assert(slot(BoardNo,coordinate(X,Y),'o'))
	    ;
	    Ghost2OnB=:=0,
	    assert(slot(BoardNo,coordinate(X,Y),' '))
	)
.


make_move_ghost3(BoardNo,X,Y,NewX,NewY):-
	retract(game_stat(BoardNo,Score,Pacman,Ghost1,Ghost2,coordinate(X,Y),Ghost1OnB,Ghost2OnB,Ghost3OnB,NoOfBiscits)),
	retract(slot(BoardNo,coordinate(NewX,NewY),Val)),
	(   member(Val,['o']),
	    assert(game_stat(BoardNo,Score,Pacman,Ghost1,Ghost2,coordinate(NewX,NewY),Ghost1OnB,Ghost2OnB,1,NoOfBiscits))
	    ;
	    not(member(Val,['o'])),
	    assert(game_stat(BoardNo,Score,Pacman,Ghost1,Ghost2,coordinate(NewX,NewY),Ghost1OnB,Ghost2OnB,0,NoOfBiscits))

	),
	assert(slot(BoardNo,coordinate(NewX,NewY),'G')),
	retract(slot(BoardNo,coordinate(X,Y),_)),

	(   Ghost3OnB=:=1,
	    assert(slot(BoardNo,coordinate(X,Y),'o'))
	    ;
	    Ghost3OnB=:=0,
	    assert(slot(BoardNo,coordinate(X,Y),' '))
	)
.



/*****************************************************************
* move_all_ghosts_alpha_beta(+BoardNo,+Move,-NewBoardNo)         *
* If move_validate_ghosts are true, Copying game from BoardNo to *
* NewBoardNo and executing ghost 1/2/3 Move in NewBoardNo.       *
* ***************************************************************/

move_all_ghosts_alpha_beta(BoardNo,Move1,Move2,Move3,NewBoardNo):-
	game_stat(BoardNo,_,_,coordinate(X1,Y1),coordinate(X2, Y2),coordinate(X3, Y3),_,_,_,_),!,
	new_cords(X1,Y1,Move1,NewX1,NewY1),!,
	new_cords(X2,Y2,Move2,NewX2,NewY2),!,
	new_cords(X3,Y3,Move3,NewX3,NewY3),!,
	move_validate_ghosts(BoardNo,NewX1,NewY1),!,
	move_validate_ghosts(BoardNo,NewX2,NewY2),!,
	move_validate_ghosts(BoardNo,NewX3,NewY3),!,
	copy_board(BoardNo,NewBoardNo),!
	,
	make_move_ghost1(NewBoardNo,X1,Y1,NewX1,NewY1),!,
	make_move_ghost2(NewBoardNo,X2,Y2,NewX2,NewY2),!,
	make_move_ghost3(NewBoardNo,X3,Y3,NewX3,NewY3),!
.



/************************************************
* pacman_win(+BoardNo)                          *
* True if pacman ate all the biscits in BoardNo *
* +Terminating the game.                        *
* **********************************************/

pacman_win(BoardNo):-
	game_stat(BoardNo,_,_,_,_,_,_,_,_,NoOfBiscits),
	NoOfBiscits=:=0,
	write("YOU WON!!! Congratz!"),nl,
	write("And W O W!!! What a score!"),nl,
	write("Score: "),print_score(BoardNo),nl,
	write("Thank you for playing our PACMAN game, hope you enjoy. C U Next time!"),nl,
	break
.



/*******************************************
* ghosts_win(+BoardNo)	                   *
* True if a ghost catchs pacman in BoardNo *
* +Terminating the game.                   *
* *****************************************/

ghosts_win(BoardNo):-
	game_stat(BoardNo,_,coordinate(X,Y),coordinate(X1,Y1),coordinate(X2,Y2),coordinate(X3,Y3),_,_,_,_),(
	X=:=X1,Y=:=Y1
	;
	X=:=X2,Y=:=Y2
	;
	X=:=X3,Y=:=Y3
	),
	write("YOU LOST!!!"),nl,
	write("Nothing can beat our artificial intelegance! But keep trying :-)"),nl,
	write("You made a great score though!"),nl,
	write("Score: "),print_score(BoardNo),nl,
	write("Thank you for playing our PACMAN game, hope you enjoy. C U Next time!"),nl,
	break
.




/*******************
* AlphaBeta module *
*******************/


/********************************************************************
* moves(+BoardNo,-BoardList,+DepthLevel,+MaxDepth)	            *
* BoardList is a list of all possible legal successor posistion of  *
* BoardNo. DepthLevel is current depth level, MaxDepth is max depth *
* level.							    *
* ******************************************************************/

moves(BoardNo,BoardList,DepthLevel,MaxDepth):-	%just collect results from makeLegalMove predicate
	DepthLevel =< MaxDepth,
	% just collect all possible legal moves -
	(
	DepthLevel mod 2 =:=0,     %It's the ghosts turn
	setof(NewBoard-(Move1,Move2,Move3),(member(Move1,[l,r,u,d]),member(Move2,[l,r,u,d]),member(Move3,[l,r,u,d]),move_all_ghosts_alpha_beta(BoardNo,Move1,Move2,Move3,NewBoard)),BoardList)
	;
	DepthLevel mod 2 =:= 1,    %It's PACMAN's turn
        setof(NewBoard-(Move),(member(Move,[l,r,u,d]), move_pacman_alpha_beta(BoardNo,Move,NewBoard)),BoardList)
        )
.



/********************************************************************************
* alphabeta(+CurrentBoardNo,+Alpha,+Beta,-BestSuccessorBoardNo,-Val,+DepthLevel)*
* This code is based on figure 24.5 implementation alphabeta	       *
* algorithm. CurrntBoardNo is a Board, Val is it's minimax value, best *
* move from CurrentBoardNo leads to BestSuccessorBoardNo, Alpha is the *
* minimal value that MAX is guaranteed to achieve. Beta is the maximal *
* value that MAX can hope to achieve. First call is with alpha =       *
* -infinity & Beta with +infinity. The calculation is for the ghosts   *
* so it always looks from a minimal point of view.		       *
* Working in a recursive manner.                                       *
* *********************************************************************/

alphabeta(CurrentBoardNo,Alpha,Beta,BestSuccessorBoardNo,Val,DepthLevel):-

	(
	   (
              (nonvar(Alpha),nonvar(Beta));
              (Alpha is -999999, Beta is 999999) % init +-infinity
            ),
	 game_level(Level),
         MaxDepth is Level,
         moves(CurrentBoardNo,BoardList,DepthLevel,MaxDepth),!,
	 NewDepthLevel is DepthLevel+1,	% update depth level
	 boundedbest(BoardList,Alpha,Beta,BestSuccessorBoardNo,Val,NewDepthLevel)
	)
	;
	staticval(CurrentBoardNo,Val) % evaluating static val of a CurrentBoardNo
.



/***********************************************************************
* boundedbest(+[Board|BoardList],+Alpha,+Beta,-GoodBoard,-GoodVal,+DepthLevel)
* Finds the 'good enough' Board from BoardList and his huristic GoodVal*
* *********************************************************************/

boundedbest([Board-_|BoardList],Alpha,Beta,GoodBoard,GoodVal,DepthLevel):-
	alphabeta(Board,Alpha,Beta,_,Val,DepthLevel),
	goodenough(BoardList,Alpha,Beta,Board,Val,GoodBoard,GoodVal,DepthLevel)
.


goodenough([],_,_,Board,Val,Board,Val,_):- !	% No other candidate
.


goodenough(_,Alpha,Beta,Pos,Val,Pos,Val,DepthLevel):-
	(DepthLevel mod 2) =:= 1,Val > Beta, !          % Maximizer(Ghosts) attained upper bound
	;
	(DepthLevel mod 2) =:= 0,Val < Alpha, !        % Minimizer (Pacman) attained lower bound
.


goodenough(BoardList,Alpha,Beta,Board,Val,GoodBoard,GoodVal,DepthLevel):-
	newbounds(Alpha,Beta,Val,NewAlpha,NewBeta,DepthLevel),   % Refine bounds
	boundedbest(BoardList,NewAlpha,NewBeta,Board1,Val1,DepthLevel),
	betterof(Board,Val,Board1,Val1,GoodBoard,GoodVal,DepthLevel)
.



/******************************************************************
* newbounds(Alpha,Beta,Val,Alpha,Val,DepthLevel)                  *
* Updating Alpha\Beta depending on the huristic value of a Board. *
* ****************************************************************/

newbounds(Alpha,Beta,Val,Val,Beta,DepthLevel):-
	(DepthLevel mod 2) =:= 1 ,Val > Alpha, !       % Maximizer(Ghosts) increased lower bound
.


newbounds(Alpha,Beta,Val,Alpha,Val,DepthLevel):-
	(DepthLevel mod 2) =:= 0,Val < Beta, !.         % Minimizer (Pacman) decreased upper bound


newbounds(Alpha,Beta,_,Alpha,Beta,_).      % Otherwise bounds unchanged


/********************************************************************
* betterof(+BoardNo,+Val,+BoardNo1,+Val1,-BoardNo,-Val,+DepthLevel) *
* Returning the better board for the MAX/MIN (depending on who's    *
* turn is it.                                                       *
* ******************************************************************/

betterof(BoardNo,Val,_,Val1,BoardNo,Val,DepthLevel):-      % Pos better than Pos1
	(DepthLevel mod 2) =:= 1 ,Val > Val1, !
	;
	(DepthLevel mod 2) =:= 0, Val < Val1, !
.


betterof(_,_,Board1,Val1,Board1,Val1,_).         % Otherwise Pos1 better




/**************************************************************
* pacman_win_alpha_beta(+BoardNo,-Val)                        *
* Return -500 in Val if pacman ate all the biscits in BoardNo *
* ************************************************************/

pacman_win_alpha_beta(BoardNo,Val):-
	game_stat(BoardNo,_,_,_,_,_,_,_,_,NoOfBiscits),
	NoOfBiscits=:=0,
	Val is -500
.

pacman_win_alpha_beta(_,0).


/*****************************************************
* ghosts_win_alpha_beta(+BoardNo,Val)	             *
* Return 500 if a ghost catchs pacman in BoardNo     *
* ***************************************************/

ghosts_win_alpha_beta(BoardNo,Val):-
	game_stat(BoardNo,_,coordinate(X,Y),coordinate(X1,Y1),coordinate(X2,Y2),coordinate(X3,Y3),_,_,_,_),
	(
	(((X=:=X1+1 ; X=:=X1-1),Y=:=Y1);((Y=:=Y1+1; Y=:=Y1-1),X=:=X1))
	;
	(((X=:=X2+1; X=:=X2-1),Y=:=Y2);((Y=:=Y2+1; Y=:=Y2-1),X=:=X2))
	;
	(((X=:=X3+1; X=:=X3-1),Y=:=Y3);((Y=:=Y3+1; Y=:=Y3-1),X=:=X3))
	),
	Val is 500,!
.

ghosts_win_alpha_beta(_,0).



/************************************************
* special_places(+coordinate(X,Y), -PlaceScore)	*
* Return a score of a slot in the board         *
************************************************/

special_places(coordinate(X,Y), PlaceScore):-
	((X>=1,X=<4);(X=<20,X>=17)), ((Y>=1,Y=<4);(Y=<21,Y>=18)), PlaceScore is 50, ! % corners
	;
	((X>=1, X=<4);(X>=17, X=<20)), Y=:=11 , PlaceScore is 100, ! % death tunnels
	;
	(X>=5, X=<16),(Y>=7, Y=<16), PlaceScore is -25,!  %center
.


special_places(_, 0).




/*****************************************************
* ghost_min_dis(+Board, -MinDis)	             *
* Return MinDis between Pacman and the closest ghost *
*****************************************************/

ghost_min_dis(Board, MinDis):-
	game_stat(Board, _, P, G1, G2, G3,_,_,_,_),
	point_dis(P, G1, Dis1), point_dis(P, G2, Dis2), point_dis(P, G3, Dis3),
	Short1 is min(Dis1, Dis2), MinDis is min(Short1, Dis3)
.



/******************************************************
* point_dis(+coordinate(X,Y), +coordinate(X2,Y2), -D) *
* Return D as distance between 2 coordinates          *
******************************************************/

point_dis(coordinate(X,Y), coordinate(X2,Y2), D):-
	D is integer(sqrt((X-X2)*(X-X2)+(Y-Y2)*(Y-Y2)))
.



/**********************************************************
* ghost_avreage_dis(Board, Dis)		                  *
* Return Dis as avarage distance between Pacman all ghost *
**********************************************************/

ghost_avreage_dis(Board, Dis):-
	game_stat(Board, _, P, G1, G2, G3,_,_,_,_),
	point_dis(P, G1, Dis1), point_dis(P, G2, Dis2), point_dis(P, G3, Dis3),
    Dis is integer((Dis1+Dis2+Dis3)/3)
.



/*****************************************************
* staticVal(+Board,-Val)		             *
* Val is the static value of a terminal (leaf) Board *
*****************************************************/

staticval(Board, Val):-
        game_level(Level),
	game_stat(Board,Score,P,_,_,_,_,_,_,_),
	special_places(P, Special),
	ghost_avreage_dis(Board, Dis),!,
	ghost_min_dis(Board, MinDis),!,
	pacman_win_alpha_beta(Board,WinP),
	ghosts_win_alpha_beta(Board,WinG),
    (

	    Level=:=1,
			(
				Val is integer(WinP+WinG-Score-Dis),!
			)
            ;
            Level=:=2,
		        (
				Val is integer(WinP+WinG-Score-MinDis),!

			)
            ;
            Level=:=3,(
				Val is integer(WinP+WinG-Score-Dis+Special),!

			)
    )

.



















