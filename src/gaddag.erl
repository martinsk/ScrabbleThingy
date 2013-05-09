-module(gaddag).

-export([construct_from_dictionary/1, 
         construct_sigma/1, 
         construct_gaddag/1,
         construct_single_word_gaddag/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%    Loads the dict from file and returns Dict, Gaddag
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
construct_from_dictionary(FileName) ->
    {ok, Data} = file:read_file(FileName),
    Words = string:tokens(binary_to_list(Data), "\r\n"),
    Dict  = lists:foldr(fun (Word, D) ->
                                dict:store(Word, true, D)
                        end, dict:new(), Words),
%%    io:fwrite("\"~s\" ~n", [construct_sigma(Words)]),
    Dict.
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%    Returns the alphabeth
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
construct_sigma(Words) ->
    Array = lists:foldr(fun(W, Acc) ->
                                lists:foldr(fun(X, Arr) ->
                                                    array:set(X, set, Arr)
                                            end, Acc, W)
                        end, array:new([{size, 256},
                                        {fixed, true},
                                        {default, unset}]), Words),
    array:foldr( fun (Idx, set, Letters)  -> 
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%    Construct the gaddag
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
construct_single_word_gaddag(Word) ->
    G = digraph:new([acyclic, private]),
    Root = lists:foldl(fun (Letter, Acc) ->
                               V = digraph:add_vertex(G),
                               digraph:add_edge(G, Acc, V, Letter),
                               V
                       end, digraph:add_vertex(G, 
terminal), Word),
    
    PrintGraph = fun(Vertex, F) ->
                         io:fwrite("*~n", []),
                         lists:foreach(fun (E) ->
                                               {_Edge, _V1, V2, Label} = digraph:edge(G,E),
                                               io:fwrite(" -[~c]-> ", [Label]),
                                               F(V2, F)
                                       end, digraph:out_edges(G, Vertex))
                 end,
    PrintGraph(Root, PrintGraph).

    %% {digraph:vertices(G), lists:foldr(fun(E, Es) ->
    %%                                           [digraph:edge(G,E) | Es]
    %%                                   end, [],  digraph:edges(G))}.








