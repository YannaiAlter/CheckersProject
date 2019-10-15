initialState(N) :-
    retractall(p(_,_)),
    retractall(c(_,_)),
    retractall(playerNum(_)),
    retractall(computerNum(_)),
    initialStatePlayer(0,0,N), %Initializing first row
    initialStatePlayer(1,1,N), %Initializing second row
    initialStatePlayer(2,0,N), % Initializing third row
    FirstRow is N-1,
    SecondRow is N-2,
    ThirdRow is N-3,
    initialStateComputer(FirstRow,1,N), %initialazing first top row
    initialStateComputer(SecondRow,0,N), % initialiazing second top row
    initialStateComputer(ThirdRow,1,N), % initializing third top row
    Num is N/2 * 3, %each row contains N/2 players and there is 3 rows
    assert(computerNum(Num)),
    assert(playerNum(Num)),
    assert(size(N)).

initialStatePlayer(_,C,N):-
    \+ inRange(C,N),
    !.

initialStatePlayer(R,C,N):-
    assert(p(R,C)),
    NewC is C+2,
    initialStatePlayer(R,NewC,N).

initialStateComputer(_,C,N):-
    \+ inRange(C,N),
    !.

initialStateComputer(R,C,N):-
    assert(c(R,C)),
    NewC is C+2,
    initialStateComputer(R,NewC,N).



inRange(Number,N):-
    Number < N,
    Number >= 0.

possible_moves_regular_computer(c(X,Y),Res,N) :-
    NewX is X-1,
    NewY is Y+1,
    NewYSecond is Y-1,
    (   inRange(NewY,N),inRange(NewX,N),A=[[NewX,NewY]], \+p(NewX,NewY), \+c(NewX,NewY), ! ;
    A=[]),
    (   inRange(NewYSecond,N),inRange(NewX,N),B=[[NewX,NewYSecond]], \+p(NewX,NewYSecond), \+c(NewX,NewYSecond), !;
    B=[]),
    append(A,B,Res).

possible_moves_regular_player(p(X,Y),Res,N):-
    NewX is X+1,
    NewY is Y+1,
    NewYSecond is Y-1,
    (   inRange(NewY,N),inRange(NewX,N),A=[[NewX,NewY]], \+c(NewX,NewY), \+p(NewX,NewY) ,! ;
    A=[]),
    (   inRange(NewYSecond,N),inRange(NewX,N),B=[[NewX,NewYSecond]], \+ c(NewX,NewYSecond), \+p(NewX,NewYSecond), !;
    B=[]),
    append(A,B,Res).

all_possible_moves_regular_player(Res,N):-
    p(X,Y),
    possible_moves_regular_player(p(X,Y),Res,N).
all_possible_moves_regular_computer(Res,N):-
    c(X,Y),
    possible_moves_regular_computer(c(X,Y),Res,N).

isEmpty(X,Y) :- %True if position is empty
    \+ c(X,Y),
    \+ p(X,Y).


move(p(X1,Y1),p(X2,Y2)) :-
    retract(p(X1,Y1)),
    assert(p(X2,Y2)).


move(c(X1,Y1),c(X2,Y2)):-
    retract(c(X1,Y1)),
    assert(c(X2,Y2)).

printBoard() :-
    printBoard(0,0).

printBoard(Row,_):-
    size(N),
    Row>=N,!.
printBoard(Row,Column):-
    size(N),
    Column>=N,
    NewRow is Row+1,
    nl,
    printBoard(NewRow,0),!.

printBoard(Row,Column) :-
    (   c(Row,Column),write('C '),!;
    p(Row,Column),write('P '),!;
    write('- ')),
    NewColumn is Column+1,
    printBoard(Row,NewColumn).


% find_best_eat_player(pos,eatten_pos,player_pos,num) return res which
% is the position result and num which is the number of eatten players

find_best_eat_regular_player(StartPos,AllEatenPos,FinalPos,EatNumber) :-
    StartPos = p(R,C),
    (
    RightEatR is R+1,
    RightEatC is C+1,
    c(RightEatR,RightEatC), %There is a computer player at the right pos
    NewPosR is RightEatR+1,
    NewPosC is RightEatC+1,
    isEmpty(NewPosR,NewPosC), %The after-eatten pos is empty
    AllEatenPos = [c(RightEatR,RightEatC) | RightAllEatenPos],
    find_best_eat_regular_player(p(NewPosR,NewPosC),RightAllEatenPos,FinalPosR,SumRightEatNumber),
    RightEatNumber is 1+SumRightEatNumber,
    FinalPos=FinalPosR,
    EatNumber=RightEatNumber,
    ! ;
    AllEatenPos=[],
    FinalPos=StartPos,
    EatNumber=0).
performEat(StartPos,[],EndPos) :-
    retract(StartPos),
    assert(EndPos),!.
performEat(StartPos,EatArray,EndPos) :-
    EatArray = [Head|Tail],
    retract(Head),
    performEat(StartPos,Tail,EndPos).
