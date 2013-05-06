-module(gaddag).

-export([construct_from_dictionary/1, 
         construct_sigma/1, 
         construct_gaddag/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%    Loads the dict from file and returns Dict, Gaddag
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
construct_from_dictionary(FileName) ->
    {ok, Data} = file:read_file(FileName),
    Words = string:tokens(binary_to_list(Data), "\r\n"),
    Dict  = lists:foldr(fun (Word, D) -> dict:store(Word, true, D) end, dict:new(), Words),
%%    io:fwrite("\"~s\" ~n", [construct_sigma(Words)]),
    Dict.
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%    Returns the alphabeth
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
construct_sigma(Words) ->
    Array = lists:foldr(
              fun(W, Acc) ->
                      lists:foldr(
                        fun(X, Arr) ->
                                array:set(X, set, Arr)
                        end, Acc, W)
              end, array:new([{size, 256},
                              {fixed, true},
                              {default, unset}]), Words),
    array:foldr(
      fun (Idx, set, Letters)  -> 
              [Idx|Letters];
          (_idx, unset, Letters) ->
              Letters
      end, [], Array).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%    Constructs the gaddag
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
construct_gaddag(Words) ->
    Fun = fun ([], Ys, _F) -> 
                  io:fwrite("~s ~n", [Ys]);
              ([X | Xs], Ys, F) ->
                  case Ys of
                      [] ->
                          io:fwrite("~s ~n", [[ X | (Xs ++ Ys)]]);
                      _ -> 
                          io:fwrite("~s ~n", [[ X | (Xs ++ ["_" | Ys])]])
                      end,
                  F(Xs, [X|Ys], F)
          end,
    Fun(Words, [], Fun).


