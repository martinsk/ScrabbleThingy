-module(digraph_to_dot).
-export([write_to_file/2,
         generate_vertices/1,
         generate_edges/2,
         print_vertices/1,
         print_edges/1]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   Generates a dictionary with Vertices
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_vertices(G) ->
    Vertices = digraph:vertices(G),
    {Dict, _Counter} = lists:foldr(fun(V, {D, Counter}) ->
                                           {dict:store(V, "node" ++ integer_to_list(Counter), D), Counter +1}
                                   end, {dict:new(), 0}, Vertices),
    Dict.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   Generates edge list
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_edges(Vertices, G) ->
    EdgeList = digraph:edges(G),
    lists:foldr(fun(E, Edges) ->
                        {E, V1, V2, Label} = digraph:edge(G, E),
                        [{E, dict:find(V1, Vertices), dict:find(V2, Vertices), Label} | Edges]
                end, [], EdgeList).
                            

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   Make list for vertices
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_vertices(Vertices) ->
    lists:foldr(fun(V, Str) ->
                        dict:find(V, Vertices) ++ Str
                end, "", dict:fetch_keys(Vertices)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   print edges
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_edges(Edges) ->
    lists:foldr(fun({_E, V1, V2, Label}, Str) -> 
                        V1 ++ "->" ++ V2 ++ "[label=\"" ++ Label ++ "\"] ~n" ++ Str
                end, "", Edges). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   write entire graph to file
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_to_file(_FileName, G) ->
    Vertices = generate_vertices(G),
    Edges    = generate_edges(G,Vertices),
    print_vertices(Vertices) ++ print_edges(Edges).
                                   
    
