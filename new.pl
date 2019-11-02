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

moveBlack(Pos,Row,Column,NewPos) :-
    Pos=(Board,Turn),
    Turn=black,
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
    set(NewBoardTemp,LeftMoveRow,LeftMoveColumn,b,NewBoard),
    NewPos = (NewBoard,white).

moveBlack(Pos,Row,Column,NewPos) :-
    Pos=(Board,Turn),
    Turn=black,
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
    set(NewBoardTemp,RightMoveRow,RightMoveColumn,b,NewBoard),
    NewPos=(NewBoard,white).


range(X, L, H) :- X is L + 1, X < H.
range(X, L, H) :- L1 is L + 1, L1 < H, range(X, L1, H).

allBlackMoves(Pos,Result) :-
    size(N),
    findall(Boards,
    (range(Row,-1,N),range(Column,-1,N),moveBlack(Pos,Row,Column,Boards)),MovesNoEats),
    findall(EatBoards,
    (range(Row,-1,N),range(Column,-1,N),moveBlack(Pos,Row,Column,EatBoards)),EatMoves),
    append(MovesNoEats,EatMoves,Result),
    \+ Result=[].

allBlackEats(Pos,Result) :-
    size(N),
    findall(Boards,
    (range(Row,-1,N),range(Column,-1,N),blackEatWhiteAllPos(Pos,Row,Column,Boards)),ResultDistinct),
    unionAll(ResultDistinct,Result).

unionAll([],[]):-!.
unionAll(List,Result) :-
List=[Head|Tail],
unionAll(Tail,TailUnionResult),
append(Head,TailUnionResult,Result).

printAllMoves([]):-!.
printAllMoves(Pos):-
Pos=[Head|Tail],
Head=(Board,_),
printArray(Board),
nl,nl,
printAllMoves(Tail).


blackEatWhite(Pos,Row,Column,NewPos) :-
    Pos=(Board,_),
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
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,b,NewBoard),
    NewPos=(NewBoard,white).
    
    
    blackEatWhite(Pos,Row,Column,NewPos) :-
    Pos=(Board,_),
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
    set(BoardAfterDeleteEnemy,RightMoveRow,RightMoveColumn,b,NewBoard),
    NewPos=(NewBoard,white).


    blackEatWhiteAllPos([],_,_,[]):-!.
    blackEatWhiteAllPos(Pos,Row,Column,ArrayOfBoards) :-
     findall(Result,blackEatWhite(Pos,Row,Column,Result),List),
     (List=[],ArrayOfBoards=[],!;List=[LeftEatPos|RightEatPos],
     NewRow is Row+2,
     NewLeftColumn is Column-2,
     NewRightColumn is Column+2,
     blackEatWhiteAllPos(LeftEatPos,NewRow,NewLeftColumn,AllLeftEat), %changing to black in order to find more eats
     blackEatWhiteAllPos(RightEatPos,NewRow,NewRightColumn,AllRightEat),
     append(List,AllLeftEat,List1),
     append(List1,AllRightEat,ArrayOfBoards)).

staticval(_,Value) :- Value=1.
moves(Pos,PosList) :-
Pos=(_,Turn),
Turn=black,
allBlackMoves(Pos,PosList).

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

alphabeta(Pos,Alpha,Beta,GoodPos,Val) :-
   moves(Pos,PosList), !,    /*user-provided*/
   boundedbest(PosList,Alpha,Beta,GoodPos,Val).
alphabeta(Pos,_,_,_,Val) :- staticval(Pos,Val).  /*user-provided*/

boundedbest([Pos | PosList], Alpha, Beta, GoodPos, GoodVal) :-
   alphabeta(Pos,Alpha, Beta, _, Val),
   goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal).

goodenough([],_,_,Pos,Val,Pos,Val) :- !.
goodenough(_, _Alpha, Beta, Pos, Val, Pos, Val) :-
   min_to_move(Pos), Val>Beta, !.    /*Maximizer attained upper bound*/
goodenough(_,Alpha,_Beta,Pos,Val,Pos,Val) :- 
   max_to_move(Pos), Val<Alpha, !.   /*Minimizer attained lower bound*/

goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal) :-
   newbounds(Alpha, Beta, Pos,Val, NewAlpha, NewBeta),
   boundedbest(PosList, NewAlpha, NewBeta, Pos1, Val1),
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
