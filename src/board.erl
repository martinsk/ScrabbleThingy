-module(board).

-export([new/2, get/3, set/4, pretty_print/1, foreach/2, fold/3]).

-record(board, {n, m, data}).

new(N,M) ->
    #board{ n = N, m = M, data = array:new([{size, N*M},
                                            {fixed, true},
                                            {default, unset}])}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   Getter and Setter
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(X,Y, Board) ->
    array:get(X + Y*Board#board.n, Board#board.data).

set(X,Y, Value, Board) ->
    Data = array:set(X + Y*Board#board.n, Value, Board#board.data),
    N = Board#board.n,
    M = Board#board.m,
    #board{n = N, m = M, data = Data}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   foreach, the fold that does not fold
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
foreach(EvalFun, Board) ->
    N = Board#board.n,
    M = Board#board.m,
    Visit = fun(_I, J, _Fun)  when(J =:= M) -> 
                    ok;
               (I, J, Fun) when (I =:= N) ->
                    Fun(0, J +1, Fun);     
               (I,J, Fun) ->
                    EvalFun(I,J, get(I,J,Board)),
                    Fun(I+1, J, Fun)
            end,
    Visit(0, 0, Visit).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   The fold that folds on the dataset.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fold(EvalFun,Init, Board) ->
    N = Board#board.n,
    M = Board#board.m,
    Visit = fun(_I, J, Acc, _Fun)  when(J =:= M) -> 
                    Acc;
               (I, J, Acc, Fun) when (I =:= N) ->
                    Fun(0, J +1, Acc, Fun);     
               (I,J, Acc, Fun) ->
                    Fun(I+1, J, EvalFun(I,J, get(I,J,Board), Acc),Fun)
            end,
    Visit(0, 0, Init, Visit).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   A pretty printing method
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pretty_print(Board) ->
    N = Board#board.n,
    Print = fun(unset) -> 
                    io_lib:format("~s", ['_']);                    
               (Value) ->
                    io_lib:format("~s", [[Value]])               
            end,
    foreach(fun(I,_J, Value) when I =:= N-1 ->
                    io:fwrite("~s ~n", [Print(Value)]);
               (_I,_J, Value) ->
                    io:fwrite("~s ", [Print(Value)])
            end, Board).

