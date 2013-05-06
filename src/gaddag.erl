-module(gaddag).

-export([construct_from_dictionary/1, construct_sigma/1, construct_sigma_1/1, construct_gaddag/1, validate_row/2,
         create_board/2, print_board/1
        ]).

construct_from_dictionary(FileName) ->
    {ok, Data} = file:read_file(FileName),
    Words = string:tokens(binary_to_list(Data), "\r\n"),
    io:fwrite("\"~s\" ~n", [construct_sigma(Words)]),
    io:fwrite("\"~s\" ~n", [construct_sigma_1(Words)]),
    ok.
    
    

construct_sigma_1(Words) ->
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



construct_sigma(Words) ->
    Sorter = fun(Word) -> lists:sort(fun(S1, S2) -> 
                                             S1 < S2
                                     end, Word)
             end, 
    UniqueLetters =  fun(Word) ->
                             Sorted = Sorter(Word),
                             {_, Sigma} = lists:foldr( fun (X, {Last, Letters} ) ->
                                                               case X =:= "~r" of
                                                                   true ->
                                                                       {Last, Letters};
                                                                   false ->
                                                                       case X =:= Last of 
                                                                           true -> 
                                                                    {X, Letters};
                                                                           false ->
                                                                               {X, [X |  Letters]}
                                                                       end
                                                               end
                                                       end, {'.', []}, Sorted),
                             Sigma
                     end,
                      
    MergeLists =  fun ([], Ys, _F) -> Ys;
                      (Xs, [], _F) -> Xs;
                      ([X | Xs], [Y | Ys], F)  when X =:= Y -> F(Xs, [Y | Ys], F);
                      ([X | Xs], [Y | Ys], F)  when X  <  Y -> [X | F(Xs, [Y | Ys], F)];
                      ([X | Xs], [Y | Ys], F) -> [Y | F([X | Xs], Ys, F)]
                  end,
      
    lists:foldr(fun(Word, Acc) -> 
                        Sigma = UniqueLetters(Word),
                        %%io:fwrite("~s ~n", [Sigma]),
                        MergeLists(Sigma, Acc, MergeLists)
                end, [], Words).


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
                                
validate_row(Row, Dict) ->
    Visit = fun ([], "", _F) ->
                    true;
                ([], Word, _F) ->
                    io:fwrite("~s, ~n", [Word]),
                    case dict:find(lists:reverse(Word), Dict) of
                        {ok, _Value} -> true;
                        error -> false
                    end;
                ([95| Xs], [], F) -> 
                    F(Xs, "", F);
                ([95 | Xs], Word, F) ->
                    io:fwrite("~s, ~n", [Word]),
                    case dict:find(lists:reverse(Word), Dict) of
                        {ok, _Value} -> F(Xs, [], F);
                        error -> false
                    end;
                ([X| Xs], Word,F) -> 
                    F(Xs, [X | Word], F)
            end,
    Visit(Row, "", Visit).


create_board(N, M) ->
    Create_Row = fun(0, _Default, _Fun) ->
                         [];
                    (Nat, Default, Fun) ->
                         [Default | Fun(Nat-1, Default, Fun)]
                 end,
    Row = Create_Row(N, 95, Create_Row),
    {board, N,M, Create_Row(M, Row, Create_Row)}.




print_board({board, N, M, Data}) ->
    lists:foreach(fun(Row) ->
                          lists:foreach(fun(Ch) ->
                                                io:fwrite("~s ", [[Ch]])
                                        end, Row),
                          io:fwrite("~n", [])                          
                  end, Data).

validate_board({board, N, M, Data}) ->
    lists:foldr(fun(Row, Acc) ->
                        Acc and  validate_row(Row)
                end, true, Data) and validate_board(transpose_board({board, N,M, Data})).


transpose_board({board, N, M, Data}) ->
    Create_board
