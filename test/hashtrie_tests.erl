%% @copyright 2010-2014 Takeru Ohta <phjgt308@gmail.com>
-module(hashtrie_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
store_test() ->
    Trie0 = hashtrie:new(),
    ?assertEqual(0, hashtrie:size(Trie0)),
    
    Trie1 = hashtrie:store(lisp, 3, Trie0),
    ?assertEqual(1, hashtrie:size(Trie1)),
    
    ?assertEqual({ok, 3}, hashtrie:find(lisp, Trie1)).

from_list_test() ->
    Input    = entries(),
    Expected = sorted_unique_entires(),

    Trie = hashtrie:from_list(Input),
    ?assertEqual(Expected, lists:sort(hashtrie:to_list(Trie))).

find_test() ->
    ?assertEqual(error, hashtrie:find(erlang, hashtrie:new())),
    
    Trie = hashtrie:from_list(entries()),
    ?assertEqual({ok, 10}, hashtrie:find(erlang, Trie)).

get_value_test() ->
    Trie = hashtrie:from_list(entries()),

    %% with default
    ?assertEqual(10,   hashtrie:fetch(erlang, Trie, none)),
    ?assertEqual(none, hashtrie:fetch(scala, Trie, none)),
    ?assertEqual(30,   hashtrie:fetch(python, Trie, none)),

    %% without default
    ?assertEqual(10,                   hashtrie:fetch(erlang, Trie)),
    ?assertError({no_such_key, scala}, hashtrie:fetch(scala, Trie)).

erase_test() ->
    Trie0 = hashtrie:from_list(entries()),
    ExpectedSize0 = length(sorted_unique_entires()),
    ?assertEqual(ExpectedSize0, hashtrie:size(Trie0)),
    
    Trie1 = hashtrie:erase(erlang, Trie0),
    ExpectedSize1 = ExpectedSize0 - 1,
    ?assertEqual(ExpectedSize1, hashtrie:size(Trie1)),
    ?assertEqual(error, hashtrie:find(erlang, Trie1)),
        
    Trie2 = hashtrie:erase(scala, Trie1),
    ExpectedSize2 = ExpectedSize1,    
    ?assertEqual(ExpectedSize2, hashtrie:size(Trie2)),
    
    lists:foldl(fun ({Key, _}, AccTrie0) ->
                        AccTrie1 = hashtrie:erase(Key, AccTrie0),
                        ?assertEqual(error, hashtrie:find(Key, AccTrie1)),
                        AccTrie1
                end,
                Trie2,
                entries()).

fold_test() ->
    TrieSum = hashtrie:fold(fun (_, V, Sum) -> V + Sum end,
                            0,
                            hashtrie:from_list(entries())),
        
    ListSum = lists:foldr(fun ({_, V}, Sum) -> V + Sum end,
                          0,
                          sorted_unique_entires()),
    ?assertEqual(ListSum, TrieSum).

large_data_test() ->
    Entries = shuffle([{N, N} || N <- lists:seq(1, 100000)]),

    Trie = hashtrie:from_list(Entries),
    ?assertEqual(length(Entries), hashtrie:size(Trie)),
    
    lists:foldl(
      fun ({K, V}, AccTrie0) ->
              ?assertEqual({ok, V}, hashtrie:find(K, Trie)),
              ?assertEqual({ok, V}, hashtrie:find(K, AccTrie0)),
              
              AccTrie1 = hashtrie:erase(K, AccTrie0),
              ?assertEqual(error, hashtrie:find(K, AccTrie1)),
              ?assertEqual(hashtrie:size(AccTrie0) - 1, hashtrie:size(AccTrie1)),

              AccTrie1
      end,
      Trie,
      Entries).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
entries() ->
    [{forth, 100},
     {erlang, 10},
     {java, 123},
     {lisp, 3},
     {ruby, 3},
     {python, 30},
     {lisp, 20},
     {java, 0}].

sorted_unique_entires() ->
    [{erlang,10},
     {forth,100},
     {java,0},
     {lisp,20},
     {python,30},
     {ruby,3}].

shuffle(List) ->
    [X||{_,X} <- lists:keysort(1, [{random:uniform(), N} || N <- List])].
