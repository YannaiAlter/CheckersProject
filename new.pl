build(X, N, List)  :- 
    length(List, N), 
    maplist(=(X), List),!.

build(X, N, List)  :- 
    findall(X, between(1, N, _), List).

initialState(N) :-
retractall(size(_)),
retractall(array(_)),
assert(size(N)),
BoardSize is N*N,
build(nil,BoardSize,Array),
assert(array(Array)),
setEveryTwo(b,0,0),
setEveryTwo(b,1,1),
setEveryTwo(b,2,0),
FirstWhite is N-1,
SecondWhite is N-2,
ThirdWhite is N-3,
setEveryTwo(w,FirstWhite,1),
setEveryTwo(w,SecondWhite,0),
setEveryTwo(w,ThirdWhite,1).

setEveryTwo(_,_,StartColumn) :-
size(N),
StartColumn>=N,!.

setEveryTwo(Value,StartRow,StartColumn) :-
array(Array),
set(Array,StartRow,StartColumn,Value,NewArray),
retractall(array(_)),
assert(array(NewArray)),
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

printArray(Array) :-
size(N),
printArray(Array,N),!.

printArray([],_):-!.

printArray(Array,0) :-
size(N),
nl,
printArray(Array,N),!.

printArray(Array,N) :-
Array = [Head|Tail],
write(Head),
write(" "),
NewRows is N-1,
printArray(Tail,NewRows).

inRange(X,Start,End) :-
X =< End,
X >= Start.

moveBlack(Board,Row,Column,NewBoard) :-
    get(Board,Row,Column,Res),
    Res=b,
    LeftMoveRow is Row+1,
    LeftMoveColumn is Column-1,
%    RightMoveRow is Row+1,
%    RightMoveColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(LeftMoveRow,0,NewN),
    inRange(LeftMoveColumn,0,NewN),
    get(Board,LeftMoveRow,LeftMoveColumn,Value), %Board[NewPos] is nil
    Value=nil,
    set(Board,Row,Column,nil,NewBoardTemp),
    set(NewBoardTemp,LeftMoveRow,LeftMoveColumn,b,NewBoard).


moveBlack(Board,Row,Column,NewBoard) :-
    get(Board,Row,Column,Res),
    Res=b,
%    LeftMoveRow is Row+1,
%    LeftMoveColumn is Column-1,
    RightMoveRow is Row+1,
    RightMoveColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,Value), %Board[NewPos] is nil
    Value=nil,
    set(Board,Row,Column,nil,NewBoardTemp),
    set(NewBoardTemp,RightMoveRow,RightMoveColumn,b,NewBoard).


range(X, L, H) :- X is L + 1, X < H.
range(X, L, H) :- L1 is L + 1, L1 < H, range(X, L1, H).

allBlackMoves(Board,Result) :-
    size(N),
    findall(Boards,
    (range(Row,-1,N),range(Column,-1,N),moveBlack(Board,Row,Column,Boards)),Result),
    \+ Result=[].

printAllMoves([]):-!.
printAllMoves(Board):-
Board=[Head|Tail],
printArray(Head),
nl,nl,
printAllMoves(Tail).


blackEatWhite(Board,Row,Column,NewBoard) :-
    get(Board,Row,Column,Res),
    Res=b,
    RightEnemyRow is Row+1,
    RightEnemyColumn is Column-1,
    size(N),
    NewN is N-1,
    inRange(RightEnemyRow,0,NewN),
    inRange(RightEnemyColumn,0,NewN),
    get(Board,RightEnemyRow,RightEnemyColumn,Value), %Board[NewPos] is nil
    Value=w,
    RightMoveRow is Row+2,
    RightMoveColumn is Column-2,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,SecondValue), %Board[NewPos] is nil
    SecondValue=nil,
    set(Board,Row,Column,nil,BoardAfterDeleteBlack),
    set(BoardAfterDeleteBlack,RightEnemyRow,RightEnemyColumn,nil,BoardAfterDeleteEnemy),
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,b,NewBoard).
    
    
    blackEatWhite(Board,Row,Column,NewBoard) :-
    get(Board,Row,Column,Res),
    Res=b,
    RightEnemyRow is Row+1,
    RightEnemyColumn is Column+1,
    size(N),
    NewN is N-1,
    inRange(RightEnemyRow,0,NewN),
    inRange(RightEnemyColumn,0,NewN),
    get(Board,RightEnemyRow,RightEnemyColumn,Value), %Board[NewPos] is nil
    Value=w,
    RightMoveRow is Row+2,
    RightMoveColumn is Column+2,
    inRange(RightMoveRow,0,NewN),
    inRange(RightMoveColumn,0,NewN),
    get(Board,RightMoveRow,RightMoveColumn,SecondValue), %Board[NewPos] is nil
    SecondValue=nil,
    set(Board,Row,Column,nil,BoardAfterDeleteBlack),
    set(BoardAfterDeleteBlack,RightEnemyRow,RightEnemyColumn,nil,BoardAfterDeleteEnemy),
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,b,NewBoard).

    blackEatWhiteAllPos([],_,_,[]):-!.
    blackEatWhiteAllPos(Board,Row,Column,ArrayOfBoards) :-
     findall(Result,blackEatWhite(Board,Row,Column,Result),List),
     (List=[],ArrayOfBoards=[],!;List=[LeftEat|RightEat],
     blackEatWhiteAllPos(LeftEat,Row,Column,AllLeftEat),
     blackEatWhiteAllPos(RightEat,Row,Column,AllRightEat),
     append(List,AllLeftEat,List1),
     append(List1,AllRightEat,ArrayOfBoards)).