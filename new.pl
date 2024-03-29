build(X, N, List)  :- 
    length(List, N), 
    maplist(=(X), List),!.

build(X, N, List)  :- 
    findall(X, between(1, N, _), List).

initialState(N) :-
retractall(size(_)),
retractall(array((_))),
assert(size(N)),
BoardSize is N*N,
build(n,BoardSize,Array),
assert(array((Array,white))),
setEveryTwo(b,0,0),
setEveryTwo(b,1,1),
setEveryTwo(b,2,0),
FirstWhite is N-1,
SecondWhite is N-2,
ThirdWhite is N-3,
setEveryTwo(w,FirstWhite,1),
setEveryTwo(w,SecondWhite,0),
setEveryTwo(w,ThirdWhite,1),
array(X),
printArray(X).

setEveryTwo(_,_,StartColumn) :-
size(N),
StartColumn>=N,!.

setEveryTwo(Value,StartRow,StartColumn) :-
array((Array,_)),
set(Array,StartRow,StartColumn,Value,NewArray),
retractall(array(_)),
assert(array((NewArray,white))),
NewColumn is StartColumn+2,
setEveryTwo(Value,StartRow,NewColumn).

get(Array,Row,Column,Result):-
size(N),
NewIndex is Row*N+Column,
nth0(NewIndex,Array,Result).

set(Array,Row,Column,Value,NewArray) :-
 size(N),
 Index is Row*N + Column,
 setValue(Index,Array,Value,NewArray).

 setValue(Index, List, Value, K) :- %setting value at index Index in list.
  nth0(Index, List, _, R),
  nth0(Index, K, Value, R).

printArray(Pos) :-
Pos=(Array,Turn),
ansi_format([bold,fg(cyan)], 'Turn: ~w', [Turn]),
nl,
size(N),
printArray(Array,N),!.

printArray([],_):-!.

printArray(Array,0) :-
size(N),
nl,
printArray(Array,N),!.

printArray(Array,N) :-
Array = [Head|Tail],
(Head=w,ansi_format([bold,fg(yellow)], '~w', [Head]),!;
Head=b,ansi_format([bold,fg(red)], '~w', [Head]),!;
Head=bc,ansi_format([bold,fg(red)], 'B', [Head]),!;
Head=wc,ansi_format([bold,fg(red)], 'W', [Head]),!;
write(Head),!),
write(" "),
NewRows is N-1,
printArray(Tail,NewRows).

inRange(X,Start,End) :-
X =< End,
X >= Start.

moveBlack(Pos,Row,Column,NewPos) :- %crownBlackMove
    Pos=(Board,Turn),
    Turn=black,
    get(Board,Row,Column,Res),
    Res=bc,
    LeftMoveRow is Row-1,
    LeftMoveColumn is Column+1,
%    RightMoveRow is Row+1,
%    RightMoveColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(LeftMoveRow,0,NewN),
    inRange(LeftMoveColumn,0,NewN),
    get(Board,LeftMoveRow,LeftMoveColumn,Value), %Board[NewPos] is n
    Value=n,
    set(Board,Row,Column,n,NewBoardTemp),
    set(NewBoardTemp,LeftMoveRow,LeftMoveColumn,Res,NewBoard),
    NewPos = (NewBoard,white).

moveBlack(Pos,Row,Column,NewPos) :- %crownBlackMove
    Pos=(Board,Turn),
    Turn=black,
    get(Board,Row,Column,Res),
    Res=bc,
    LeftMoveRow is Row-1,
    LeftMoveColumn is Column-1,
%    RightMoveRow is Row+1,
%    RightMoveColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(LeftMoveRow,0,NewN),
    inRange(LeftMoveColumn,0,NewN),
    get(Board,LeftMoveRow,LeftMoveColumn,Value), %Board[NewPos] is n
    Value=n,
    set(Board,Row,Column,n,NewBoardTemp),
    set(NewBoardTemp,LeftMoveRow,LeftMoveColumn,Res,NewBoard),
    NewPos = (NewBoard,white).

moveBlack(Pos,Row,Column,NewPos) :-
    Pos=(Board,Turn),
    Turn=black,
    get(Board,Row,Column,Res),
    (Res=b,!;Res=bc),
    LeftMoveRow is Row+1,
    LeftMoveColumn is Column-1,
%    RightMoveRow is Row+1,
%    RightMoveColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(LeftMoveRow,0,NewN),
    inRange(LeftMoveColumn,0,NewN),
    get(Board,LeftMoveRow,LeftMoveColumn,Value), %Board[NewPos] is n
    Value=n,
    set(Board,Row,Column,n,NewBoardTemp),
    (LeftMoveRow = NewN, set(NewBoardTemp,LeftMoveRow,LeftMoveColumn,bc,NewBoard),!;
    set(NewBoardTemp,LeftMoveRow,LeftMoveColumn,Res,NewBoard)),
    NewPos = (NewBoard,white).

moveBlack(Pos,Row,Column,NewPos) :-
    Pos=(Board,Turn),
    Turn=black,
    get(Board,Row,Column,Res),
    (Res=b,!;Res=bc),
%    LeftMoveRow is Row+1,
%    LeftMoveColumn is Column-1,
    RightMoveRow is Row+1,
    RightMoveColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,Value), %Board[NewPos] is n
    Value=n,
    set(Board,Row,Column,n,NewBoardTemp),
    (NewN = RightMoveRow, set(NewBoardTemp,RightMoveRow,RightMoveColumn,bc,NewBoard),!;
    set(NewBoardTemp,RightMoveRow,RightMoveColumn,Res,NewBoard),!),
    NewPos=(NewBoard,white).


moveWhite(Pos,Row,Column,NewPos) :- % crown
    Pos=(Board,Turn),
    Turn=white,
    get(Board,Row,Column,Res),
    Res=wc,
    LeftMoveRow is Row-1,
    LeftMoveColumn is Column-1,
%    RightMoveRow is Row+1,
%    RightMoveColumn is Column-1,
    size(N),
    NewN is N-1,
    inRange(LeftMoveRow,0,NewN),
    inRange(LeftMoveColumn,0,NewN),
    get(Board,LeftMoveRow,LeftMoveColumn,Value), %Board[NewPos] is n
    Value=n,
    set(Board,Row,Column,n,NewBoardTemp),
    set(NewBoardTemp,LeftMoveRow,LeftMoveColumn,wc,NewBoard),
    NewPos = (NewBoard,black).

moveWhite(Pos,Row,Column,NewPos) :- %crown
    Pos=(Board,Turn),
    Turn=white,
    get(Board,Row,Column,Res),
    Res=wc,
    LeftMoveRow is Row-1,
    LeftMoveColumn is Column+1,
%    RightMoveRow is Row+1,
%    RightMoveColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(LeftMoveRow,0,NewN),
    inRange(LeftMoveColumn,0,NewN),
    get(Board,LeftMoveRow,LeftMoveColumn,Value), %Board[NewPos] is n
    Value=n,
    set(Board,Row,Column,n,NewBoardTemp),
    set(NewBoardTemp,LeftMoveRow,LeftMoveColumn,wc,NewBoard),
    NewPos = (NewBoard,black).


moveWhite(Pos,Row,Column,NewPos) :-
    Pos=(Board,Turn),
    Turn=white,
    get(Board,Row,Column,Res),
    Res=w,
    LeftMoveRow is Row-1,
    LeftMoveColumn is Column+1,
%    RightMoveRow is Row+1,
%    RightMoveColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(LeftMoveRow,0,NewN),
    inRange(LeftMoveColumn,0,NewN),
    get(Board,LeftMoveRow,LeftMoveColumn,Value), %Board[NewPos] is n
    Value=n,
    set(Board,Row,Column,n,NewBoardTemp),
    (NewN=LeftMoveRow,set(NewBoardTemp,LeftMoveRow,LeftMoveColumn,wc,NewBoard),!;
    set(NewBoardTemp,LeftMoveRow,LeftMoveColumn,w,NewBoard),!),
    NewPos = (NewBoard,black).

moveWhite(Pos,Row,Column,NewPos) :-
    Pos=(Board,Turn),
    Turn=white,
    get(Board,Row,Column,Res),
    Res=w,
%    LeftMoveRow is Row+1,
%    LeftMoveColumn is Column-1,
    RightMoveRow is Row-1,
    RightMoveColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,Value), %Board[NewPos] is n
    Value=n,
    set(Board,Row,Column,n,NewBoardTemp),
    (NewN=RightMoveRow,set(NewBoardTemp,RightMoveRow,RightMoveColumn,wc,NewBoard),!;
    set(NewBoardTemp,RightMoveRow,RightMoveColumn,w,NewBoard),!),
    NewPos=(NewBoard,black).

realMoveWhite(Row,Column,NewRow,NewColumn) :-
    array((Board,Turn)),
    Turn=white,
    size(N),
    NewN is N-1,
    inRange(NewRow,0,NewN),
    inRange(NewColumn,0,NewN),
    NewRow is Row-1,
    (NewColumn is Column+1,!;NewColumn is Column-1,!),
    get(Board,NewRow,NewColumn,Value), %Board[NewPos] is n
    Value=n,
    set(Board,Row,Column,n,NewBoard),
    (NewRow=0,set(NewBoard,NewRow,NewColumn,wc,FinalBoard),!;
    set(NewBoard,NewRow,NewColumn,w,FinalBoard),!),
    retractall(array(_)),
    assert(array((FinalBoard,black))),
    printArray((FinalBoard,black)).

range(X, L, H) :- X is L + 1, X < H.
range(X, L, H) :- L1 is L + 1, L1 < H, range(X, L1, H).

allBlackMoves(Pos,Result) :-
    size(N),
    findall(Boards,
    (range(Row,-1,N),range(Column,-1,N),moveBlack(Pos,Row,Column,Boards)),Result),
    \+ Result=[].

allWhiteMoves(Pos,Result) :-
    size(N),
    findall(Boards,
    (range(Row,-1,N),range(Column,-1,N),moveWhite(Pos,Row,Column,Boards)),Result),
    \+ Result=[].

allBlackEats(Pos,Result) :-
    size(N),
    findall(Boards,
    (range(Row,-1,N),range(Column,-1,N),blackEatWhiteAllPos(Pos,Row,Column,Boards)),ResultDistinct),
    unionAll(ResultDistinct,Result).

allWhiteEats(Pos,Result) :-
    size(N),
    findall(Boards,
    (range(Row,-1,N),range(Column,-1,N),whiteEatBlackAllPos(Pos,Row,Column,Boards)),ResultDistinct),
    unionAll(ResultDistinct,Result).

unionAll([],[]):-!.
unionAll(List,Result) :-
List=[Head|Tail],
unionAll(Tail,TailUnionResult),
append(Head,TailUnionResult,Result).

printAllMoves([]):-!.
printAllMoves(Pos):-
Pos=[Head|Tail],
printArray(Head),
nl,nl,
printAllMoves(Tail).


blackEatWhite(Pos,Row,Column,NewPos) :-
    Pos=(Board,_),
    get(Board,Row,Column,Res),
    (Res=b;Res=bc),
    RightEnemyRow is Row+1,
    RightEnemyColumn is Column-1,
    size(N),
    NewN is N-1,
    inRange(RightEnemyRow,0,NewN),
    inRange(RightEnemyColumn,0,NewN),
    get(Board,RightEnemyRow,RightEnemyColumn,Value), %Board[NewPos] is n
    (Value=w;Value=wc),
    RightMoveRow is Row+2,
    RightMoveColumn is Column-2,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,SecondValue), %Board[NewPos] is n
    SecondValue=n,
    set(Board,Row,Column,n,BoardAfterDeleteBlack),
    set(BoardAfterDeleteBlack,RightEnemyRow,RightEnemyColumn,n,BoardAfterDeleteEnemy),
    (NewN=RightMoveRow,set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,bc,NewBoard),!;
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,b,NewBoard)),
    NewPos=(NewBoard,white).
    


blackEatWhite(Pos,Row,Column,NewPos) :- %blackcrown
    Pos=(Board,_),
    get(Board,Row,Column,Res),
    Res=bc,
    RightEnemyRow is Row-1,
    RightEnemyColumn is Column-1,
    size(N),
    NewN is N-1,
    inRange(RightEnemyRow,0,NewN),
    inRange(RightEnemyColumn,0,NewN),
    get(Board,RightEnemyRow,RightEnemyColumn,Value), %Board[NewPos] is n
    (Value=w;Value=wc),
    RightMoveRow is Row-2,
    RightMoveColumn is Column-2,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,SecondValue), %Board[NewPos] is n
    SecondValue=n,
    set(Board,Row,Column,n,BoardAfterDeleteBlack),
    set(BoardAfterDeleteBlack,RightEnemyRow,RightEnemyColumn,n,BoardAfterDeleteEnemy),
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,bc,NewBoard),
    NewPos=(NewBoard,white).
    

    blackEatWhite(Pos,Row,Column,NewPos) :- %blackcrown
    Pos=(Board,_),
    get(Board,Row,Column,Res),
    Res=bc,
    RightEnemyRow is Row-1,
    RightEnemyColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(RightEnemyRow,0,NewN),
    inRange(RightEnemyColumn,0,NewN),
    get(Board,RightEnemyRow,RightEnemyColumn,Value), %Board[NewPos] is n
    (Value=w;Value=wc),
    RightMoveRow is Row-2,
    RightMoveColumn is Column+2,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,SecondValue), %Board[NewPos] is n
    SecondValue=n,
    set(Board,Row,Column,n,BoardAfterDeleteBlack),
    set(BoardAfterDeleteBlack,RightEnemyRow,RightEnemyColumn,n,BoardAfterDeleteEnemy),
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,bc,NewBoard),
    NewPos=(NewBoard,white).


    blackEatWhite(Pos,Row,Column,NewPos) :-
    Pos=(Board,_),
    get(Board,Row,Column,Res),
    (Res=b;Res=bc),
    RightEnemyRow is Row+1,
    RightEnemyColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(RightEnemyRow,0,NewN),
    inRange(RightEnemyColumn,0,NewN),
    get(Board,RightEnemyRow,RightEnemyColumn,Value), %Board[NewPos] is n
    (Value=w;Value=wc),
    RightMoveRow is Row+2,
    RightMoveColumn is Column+2,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,SecondValue), %Board[NewPos] is n
    SecondValue=n,
    set(Board,Row,Column,n,BoardAfterDeleteBlack),
    set(BoardAfterDeleteBlack,RightEnemyRow,RightEnemyColumn,n,BoardAfterDeleteEnemy),
    (NewN=RightMoveRow,set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,bc,NewBoard),!;
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,b,NewBoard),!),
    NewPos=(NewBoard,white).


  whiteEatBlack(Pos,Row,Column,NewPos) :-
    Pos=(Board,_),
    get(Board,Row,Column,Res),
    Res=wc,
    RightEnemyRow is Row+1,
    RightEnemyColumn is Column-1,
    size(N),
    NewN is N-1,
    inRange(RightEnemyRow,0,NewN),
    inRange(RightEnemyColumn,0,NewN),
    get(Board,RightEnemyRow,RightEnemyColumn,Value), %Board[NewPos] is n
    (Value=b;Value=bc),
    RightMoveRow is Row+2,
    RightMoveColumn is Column-2,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,SecondValue), %Board[NewPos] is n
    SecondValue=n,
    set(Board,Row,Column,n,BoardAfterDeleteBlack),
    set(BoardAfterDeleteBlack,RightEnemyRow,RightEnemyColumn,n,BoardAfterDeleteEnemy),
    (NewN=RightMoveRow,set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,wc,NewBoard),!;
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,wc,NewBoard),!),
    NewPos=(NewBoard,black).

whiteEatBlack(Pos,Row,Column,NewPos) :-
    Pos=(Board,_),
    get(Board,Row,Column,Res),
    Res=wc,
    RightEnemyRow is Row+1,
    RightEnemyColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(RightEnemyRow,0,NewN),
    inRange(RightEnemyColumn,0,NewN),
    get(Board,RightEnemyRow,RightEnemyColumn,Value), %Board[NewPos] is n
    (Value=b;Value=bc),
    RightMoveRow is Row+2,
    RightMoveColumn is Column+2,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,SecondValue), %Board[NewPos] is n
    SecondValue=n,
    set(Board,Row,Column,n,BoardAfterDeleteBlack),
    set(BoardAfterDeleteBlack,RightEnemyRow,RightEnemyColumn,n,BoardAfterDeleteEnemy),
    (NewN=RightMoveRow,set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,wc,NewBoard),!;
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,wc,NewBoard),!),
    NewPos=(NewBoard,black).

  whiteEatBlack(Pos,Row,Column,NewPos) :-
    Pos=(Board,_),
    get(Board,Row,Column,Res),
    Res=w,
    RightEnemyRow is Row-1,
    RightEnemyColumn is Column-1,
    size(N),
    NewN is N-1,
    inRange(RightEnemyRow,0,NewN),
    inRange(RightEnemyColumn,0,NewN),
    get(Board,RightEnemyRow,RightEnemyColumn,Value), %Board[NewPos] is n
    (Value=b;Value=bc),
    RightMoveRow is Row-2,
    RightMoveColumn is Column-2,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,SecondValue), %Board[NewPos] is n
    SecondValue=n,
    set(Board,Row,Column,n,BoardAfterDeleteBlack),
    set(BoardAfterDeleteBlack,RightEnemyRow,RightEnemyColumn,n,BoardAfterDeleteEnemy),
    (NewN=RightMoveRow,set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,wc,NewBoard),!;
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,w,NewBoard),!),
    NewPos=(NewBoard,black).

    whiteEatBlack(Pos,Row,Column,NewPos) :-
    Pos=(Board,_),
    get(Board,Row,Column,Res),
    Res=w,
    RightEnemyRow is Row-1,
    RightEnemyColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(RightEnemyRow,0,NewN),
    inRange(RightEnemyColumn,0,NewN),
    get(Board,RightEnemyRow,RightEnemyColumn,Value), %Board[NewPos] is n
    (Value=b;Value=bc),
    RightMoveRow is Row-2,
    RightMoveColumn is Column+2,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,SecondValue), %Board[NewPos] is n
    SecondValue=n,
    set(Board,Row,Column,n,BoardAfterDeleteBlack),
    set(BoardAfterDeleteBlack,RightEnemyRow,RightEnemyColumn,n,BoardAfterDeleteEnemy),
    (NewN=RightMoveRow,set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,wc,NewBoard),!;
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,w,NewBoard),!),
    NewPos=(NewBoard,black).

    blackEatWhiteAllPos([],_,_,[]):-!.
    blackEatWhiteAllPos(Pos,Row,Column,ArrayOfBoards) :-
     findall(Result,blackEatWhite(Pos,Row,Column,Result),List),
     (List=[],ArrayOfBoards=[],!;List=[RightEatPos|LeftEatPos],
     NewRow is Row+2,
     NewLeftColumn is Column-2,
     NewRightColumn is Column+2,
     blackEatWhiteAllPos(LeftEatPos,NewRow,NewLeftColumn,AllLeftEat), %changing to black in order to find more eats
     blackEatWhiteAllPos(RightEatPos,NewRow,NewRightColumn,AllRightEat),
     append(List,AllLeftEat,List1),
     append(List1,AllRightEat,ArrayOfBoards)).

    whiteEatBlackAllPos([],_,_,[]):-!.
    whiteEatBlackAllPos(Pos,Row,Column,ArrayOfBoards) :-
     findall(Result,whiteEatBlack(Pos,Row,Column,Result),List),
     (List=[],ArrayOfBoards=[],!;List=[RightEatPos|LeftEatPos],
     NewRow is Row-2,
     NewLeftColumn is Column-2,
     NewRightColumn is Column+2,
     whiteEatBlackAllPos(LeftEatPos,NewRow,NewLeftColumn,AllLeftEat), %changing to black in order to find more eats
     whiteEatBlackAllPos(RightEatPos,NewRow,NewRightColumn,AllRightEat),
     append(List,AllLeftEat,List1),
     append(List1,AllRightEat,ArrayOfBoards)).

staticval(Pos,Value) :- 
size(N),
Pos=(Board,_),
findall(Sum,(range(X,-1,N),range(Y,-1,N),get(Board,X,Y,Element),Element=w,Sum is X+Y),L1),
length(L1,WhitePlayersLeft),
TotalPlayers is N*3/2,
BlackEatWhiteCount is TotalPlayers -WhitePlayersLeft,
findall(Sum,(range(X,-1,N),range(Y,-1,N),get(Board,X,Y,Element),Element=b,Sum is X+Y),L2),
length(L2,BlackPlayersLeft),
WhiteEatBlackCount is TotalPlayers-BlackPlayersLeft,
Value is BlackEatWhiteCount - WhiteEatBlackCount.

moves(_,_,0) :- !,fail.

moves(Pos,PosList,_) :-
Pos=(_,Turn),
(
Turn=black,
allBlackMoves(Pos,MoveBoardList),
allBlackEats(Pos,EatBoardList),
append(MoveBoardList,EatBoardList,PosList),!;
Turn=white,
allWhiteMoves(Pos,MoveBoardList),
allWhiteEats(Pos,EatBoardList),
append(MoveBoardList,EatBoardList,PosList),!
).

max_to_move(Pos) :-
Pos=(_,Turn),
Turn=black.

min_to_move(Pos):-
Pos=(_,Turn),
Turn=white.
     /* alpha is the minimal value of max nodes; already guaranteed to achieve,
   beta is the maximum (worst) value of min nodes; guaranteed to achieve 
   Root's backed-up value is in the interval [alpha,beta]  */
/* Interval gets smaller as search progresses */

alphabeta(Pos,Alpha,Beta,GoodPos,Val,Depth) :-
   moves(Pos,PosList,Depth), !,    /*user-provided*/
   boundedbest(PosList,Alpha,Beta,GoodPos,Val,Depth).

alphabeta(Pos,_,_,_,Val,_) :- staticval(Pos,Val).  /*user-provided*/

boundedbest([Pos | PosList], Alpha, Beta, GoodPos, GoodVal,Depth) :-
   NewDepth is Depth-1,
   alphabeta(Pos,Alpha, Beta, _, Val,NewDepth),
   goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal,Depth).

goodenough([],_,_,Pos,Val,Pos,Val,_) :- !.
goodenough(_, _Alpha, Beta, Pos, Val, Pos, Val,_) :-
   min_to_move(Pos), Val>Beta, !.    /*Maximizer attained upper bound*/
goodenough(_,Alpha,_Beta,Pos,Val,Pos,Val,_) :- 
   max_to_move(Pos), Val<Alpha, !.   /*Minimizer attained lower bound*/

goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal,Depth) :-
   newbounds(Alpha, Beta, Pos,Val, NewAlpha, NewBeta),
   boundedbest(PosList, NewAlpha, NewBeta, Pos1, Val1,Depth),
   betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds(Alpha, Beta, Pos, Val, Val, Beta) :-
   min_to_move(Pos), Val>Alpha, !.   /*Maximizer increased the lower bound*/
newbounds(Alpha,Beta, Pos, Val, Alpha, Val) :- 
   max_to_move(Pos), Val<Beta, !.    /*Minimizer decreased the upper bound*/
newbounds(Alpha, Beta,_,_,Alpha, Beta).

betterof(Pos, Val, _Pos1, Val1, Pos, Val) :-
   min_to_move(Pos), Val>Val1, !.
betterof(Pos, Val, _Pos1, Val1, Pos,Val) :-
   max_to_move(Pos), Val<Val1, !.
betterof(_,_,Pos1,Val1,Pos1,Val1).

changeTurn():-
array((Board,_)),
retractall(array(_)),
assert(array((Board,black))).

makeBlackMove() :-
    array(CurPos),
    CurPos = (_,Turn),
    Turn=black,
    retractall(array(_)),
    alphabeta(CurPos,-1000,1000,X,_,2),
    assert(array(X)),
    printArray(X).

setAllNil(Array,[],Array) :- write(Array).

setAllNil(Array,ListOfPositions,FinalArray) :-
    ListOfPositions = [Head | Tail],
    Head=(X,Y),
    set(Array,X,Y,n,NewArray),
    setAllNil(NewArray,Tail,FinalArray).

eatBlack(Row,Column,EatList,TargetRow,TargetColumn) :-
    array(CurPos),
    CurPos = (Array,Turn),
    Turn=white,
    get(Array,Row,Column,Result),
    Result=w,
    get(Array,TargetRow,TargetColumn,SecondResult),
    SecondResult=n,
    set(Array,TargetRow,TargetColumn,w,NewArrayOne),
    set(NewArrayOne,Row,Column,n,NewArrayTwo),
    setAllNil(NewArrayTwo,EatList,FinalArray),
    whiteEatBlackAllPos(CurPos,Row,Column,AllEatPositions),
    member((FinalArray,black),AllEatPositions),
    retractall(array(_)),
    assert(array((FinalArray,black))),
    printArray((FinalArray,black)),!.
    % if final array can be found on whiteeatblackallpos then it is correct move
