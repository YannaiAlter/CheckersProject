initialState(N) :-
    retractall(p(_,_,_)),
    retractall(c(_,_,_)),
    retractall(turn(_)),
    retractall(playerNum(_)),
    retractall(computerNum(_)),
    retractall(size(_)),
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
    assert(turn(c)),
    assert(size(N)).

max_to_move():-
    turn(c).

min_to_move():-
    turn(p).

staticval(Val) :-
    c(X,Y,_),
    Val is X+Y ,!.
initialStatePlayer(_,C,N):-
    \+ inRange(C,N),
    !.

initialStatePlayer(R,C,N):-
    assert(p(R,C,regular)),
    NewC is C+2,
    initialStatePlayer(R,NewC,N).

initialStateComputer(_,C,N):-
    \+ inRange(C,N),
    !.

initialStateComputer(R,C,N):-
    assert(c(R,C,regular)),
    NewC is C+2,
    initialStateComputer(R,NewC,N).



inRange(Number,N):-
    Number < N,
    Number >= 0.

possible_moves_regular_computer(c(X,Y,regular),Res) :-
    size(N),
    NewX is X-1,
    NewY is Y+1,
    NewYSecond is Y-1,
    (   inRange(NewY,N),inRange(NewX,N),A=[[NewX,NewY]], \+p(NewX,NewY,_), \+c(NewX,NewY,_), ! ;
    A=[]),
    (   inRange(NewYSecond,N),inRange(NewX,N),B=[[NewX,NewYSecond]], \+p(NewX,NewYSecond,_), \+c(NewX,NewYSecond,_), !;
    B=[]),
    append(A,B,Res).

possible_moves_regular_player(p(X,Y,regular),Res):-
    size(N),
    NewX is X+1,
    NewY is Y+1,
    NewYSecond is Y-1,
    (   inRange(NewY,N),inRange(NewX,N),A=[[NewX,NewY]], \+c(NewX,NewY,_), \+p(NewX,NewY,_) ,! ;
    A=[]),
    (   inRange(NewYSecond,N),inRange(NewX,N),B=[[NewX,NewYSecond]], \+ c(NewX,NewYSecond,_), \+p(NewX,NewYSecond,_), !;
    B=[]),
    append(A,B,Res).

all_possible_moves_regular_player(Res):-
    p(X,Y,regular),
    possible_moves_regular_player(p(X,Y,regular),Res).
all_possible_moves_regular_computer(Res):-
    c(X,Y,regular),
    possible_moves_regular_computer(c(X,Y,regular),Res).

isEmpty(X,Y) :- %True if position is empty
    \+ c(X,Y,_),
    \+ p(X,Y,_).


move(p(X1,Y1,_),p(X2,Y2,_)) :-
    turn(p),
    retract(p(X1,Y1,Type)),
    assert(p(X2,Y2,Type)),
    retract(turn(p)),
    assert(turn(c)).


move(c(X1,Y1,Type),c(X2,Y2,Type)):-
    turn(c),
    retract(c(X1,Y1,Type)),
    assert(c(X2,Y2,Type)),
    retract(turn(c)),
    assert(turn(p)).

printBoard() :-
    write("0 "),
    printBoard(0,0).

printBoard(Row,_):-
    size(N),
    Row>=N,!.
printBoard(Row,Column):-
    size(N),
    Column>=N,
    NewRow is Row+1,
    nl,
    write(NewRow),
    write(" "),
    printBoard(NewRow,0),!.

printBoard(Row,Column) :-
    (   c(Row,Column,_),write('C '),!;
    p(Row,Column,_),write('P '),!;
    write('- ')),
    NewColumn is Column+1,
    printBoard(Row,NewColumn).

% find_best_eat_player(pos,eatten_pos,player_pos,num) return res which
% is the position result and num which is the number of eatten players

find_best_eat_regular_player(StartPos,AllEatenPos,FinalPos,EatNumber) :-
    StartPos = p(R,C,regular),
    size(N),
    (
    RightEatR is R+1,
    RightEatC is C+1,
    c(RightEatR,RightEatC,regular), %There is a computer player at the right pos
    NewPosR is RightEatR+1,
    NewPosC is RightEatC+1,
    inRange(NewPosR,N),
    inRange(NewPosC,N),
    %NewPosCSecondDiagonal is RightEatC-1,
    AllEatenPos = [c(RightEatR,RightEatC,regular) | RightAllEatenPos],
   (    isEmpty(NewPosR,NewPosC), %The after-eatten pos is empty
    find_best_eat_regular_player(p(NewPosR,NewPosC),RightAllEatenPos,FinalPos,SumRightEatNumber),

   %isEmpty(NewPosR,NewPosCSecondDiagonal),
   %find_best_eat_regular_player(p(NewPosR,NewPosCSecondDiagonal),RightAllEatenPos,FinalPosR,SumRightEatNumber),
    RightEatNumber is 1+SumRightEatNumber,
    EatNumber=RightEatNumber);
    AllEatenPos=[],
    FinalPos=StartPos,
    EatNumber=0).

find_best_eat_regular_player(StartPos,AllEatenPos,FinalPos,EatNumber) :-
    StartPos = p(R,C,regular),
    size(N),
    (
    RightEatR is R+1,
    RightEatC is C-1,
    c(RightEatR,RightEatC,regular), %There is a computer player at the right pos
    NewPosR is RightEatR+1,
    NewPosC is RightEatC-1,
    inRange(NewPosR,N),
    inRange(NewPosC,N),
    %NewPosCSecondDiagonal is RightEatC-1,
    AllEatenPos = [c(RightEatR,RightEatC,regular) | RightAllEatenPos],
   (    isEmpty(NewPosR,NewPosC), %The after-eatten pos is empty
    find_best_eat_regular_player(p(NewPosR,NewPosC,regular),RightAllEatenPos,FinalPos,SumRightEatNumber),

   %isEmpty(NewPosR,NewPosCSecondDiagonal),
   %find_best_eat_regular_player(p(NewPosR,NewPosCSecondDiagonal),RightAllEatenPos,FinalPosR,SumRightEatNumber),
    RightEatNumber is 1+SumRightEatNumber,
    EatNumber=RightEatNumber);
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

% Figure 22.5  An implementation of the alpha-beta algorithm.


% The alpha-beta algorithm

alphabeta( Pos, Alpha, Beta, GoodPos, Val)  :-
  possible_moves_regular_computer(Pos,PosList), !,
  boundedbest( PosList, Alpha, Beta, GoodPos, Val);
  staticval(Val).                              % Static value of Pos

boundedbest( [Pos | PosList], Alpha, Beta, GoodPos, GoodVal)  :-
  alphabeta( Pos, Alpha, Beta, _, Val),
  goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal).

goodenough( [], _, _, Pos, Val, Pos, Val)  :-  !.    % No other candidate

goodenough( _, Alpha, Beta, Pos, Val, Pos, Val)  :-
  min_to_move(), Val > Beta, !                   % Maximizer attained upper bound
  ;
  max_to_move(), Val < Alpha, !.                 % Minimizer attained lower bound

goodenough( PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal)  :-
  newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta),    % Refine bounds
  boundedbest( PosList, NewAlpha, NewBeta, Pos1, Val1),
  betterof( Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds( Alpha, Beta, Val, Val, Beta)  :-
  min_to_move(), Val > Alpha, !.                 % Maximizer increased lower bound

newbounds( Alpha, Beta, Val, Alpha, Val)  :-
   max_to_move(), Val < Beta, !.                 % Minimizer decreased upper bound

newbounds( Alpha, Beta, _, _, Alpha, Beta).          % Otherwise bounds unchanged

betterof( Pos, Val, _, Val1, Pos, Val)  :-        % Pos better than Pos1
  min_to_move(), Val > Val1, !
  ;
  max_to_move(), Val < Val1, !.

betterof( _, _, Pos1, Val1, Pos1, Val1).             % Otherwise Pos1 better
