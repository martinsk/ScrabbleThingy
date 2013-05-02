-module(gaddag).

-export([load_dictionary/1, construct_dawg/1, construct_gaddag/1]).

load_dictionary(FileName) ->
    {ok, Data} = file:read_file(FileName),
    Words = string:tokens(binary_to_list(Data), "\n"),
    Gaddag = construct_gaddag(Words),
    lists:foreach(fun(W) ->
                          io:fwrite("~s ~n", [W])
                  end, Words),
    ok.

construct_dawg(Word) ->
    %% list:fold(fun (Idx, Arr) ->
    %%                   PreList = list:sublist(Word, Idx),
    %%                   SufList = list:sublist(Word, Idx, string:len(Word), string:len(Word)- Idx),
    %%                   array:set(Idx, construct_dawg(Word), Arr)
    %%           end,0,array:new([{size, len[Word]},{fixed, true}])),
    ok.
construct_gaddag(Word) ->
    ok.
    

