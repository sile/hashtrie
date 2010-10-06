-module(hashtrie_test).
-export([gen_int_entries/2, gen_str_entries/1]).
-export([store_entries/2, store_loop/2, find_entries/2]).
-export([dict_store_entries/2, dict_store_loop/2, dict_find_entries/2]).
-export([gbtree_store_entries/2, gbtree_store_loop/2, gbtree_find_entries/2]).

gen_int_entries(Start,End) ->
    lists:map(fun (X) -> {X,X} end, lists:seq(Start,End-1)).

gen_str_entries(Filepath) ->
    {ok, In} = file:open(Filepath, read),
    read_lines(In,[],0).

read_lines(In, Lines, LineNum) ->
    case file:read_line(In) of
        {ok, Line} -> read_lines(In,
                                 [{string:strip(Line,right,$\n),LineNum}|Lines], 
                                 LineNum+1);
        eof -> lists:reverse(Lines)
    end.

store_entries(Entries, Trie) ->
    lists:foldl(fun ({K,V},T) -> hashtrie:store(K, V, T) end,
                Trie,
                Entries).

store_loop(_, 0) -> done;
store_loop(Entries, LoopCount) ->
    store_entries(Entries, hashtrie:new()),
    store_loop(Entries, LoopCount-1).

find_entries(Entries, Trie) ->
    lists:foldl(fun ({K,_}, Cnt) -> 
                        case hashtrie:find(K, Trie) of
                            {K,_} -> Cnt+1;
                            false -> Cnt
                        end
                end,
                0,
                Entries).

dict_store_entries(Entries, Dict) ->
    lists:foldl(fun ({K,V},D) -> dict:store(K, V, D) end,
                Dict,
                Entries).

dict_find_entries(Entries, Dict) ->
    lists:foldl(fun ({K,_}, Cnt) -> 
                        case dict:find(K, Dict) of
                            {ok,_} -> Cnt+1;
                            error -> Cnt
                        end
                end,
                0,
                Entries).

dict_store_loop(_, 0) -> done;
dict_store_loop(Entries, LoopCount) ->
    dict_store_entries(Entries, dict:new()),
    dict_store_loop(Entries, LoopCount-1).

gbtree_store_entries(Entries, Tree) ->
    lists:foldl(fun ({K,V},T) -> gb_trees:insert(K, V, T) end,
                Tree,
                Entries).

gbtree_find_entries(Entries, Tree) ->
    lists:foldl(fun ({K,_}, Cnt) -> 
                        case gb_trees:lookup(K, Tree) of
                            {value,_} -> Cnt+1;
                            none -> Cnt
                        end
                end,
                0,
                Entries).

gbtree_store_loop(_, 0) -> done;
gbtree_store_loop(Entries, LoopCount) ->
    gbtree_store_entries(Entries, gb_trees:empty()),
    gbtree_store_loop(Entries, LoopCount-1).

%% Words = hashtrie_test:gen_str_entries("/home/ohta/data/text/ipa.keys"), length(Words).
%% Words = hashtrie_test:gen_int_entries(0,10).

%% f(T), {Time,T} = timer:tc(hashtrie_test, store_entries, [Words, hashtrie:new()]), Time. f(Time).
%% timer:tc(hashtrie_test, store_loop, [Words, 10000]).
%% {Time,_} = timer:tc(hashtrie_test, find_entries, [Words, T]), Time. f(Time).

%% f(D), {Time,D} = timer:tc(hashtrie_test, dict_store_entries, [Words, dict:new()]), Time. f(Time).
%% timer:tc(hashtrie_test, dict_store_loop, [Words, 10000]).
%% {Time,_} = timer:tc(hashtrie_test, dict_find_entries, [Words, D]), Time. f(Time).

%% f(G), {Time,G} = timer:tc(hashtrie_test, gbtree_store_entries, [Words, gb_trees:empty()]), Time. f(Time).
%% timer:tc(hashtrie_test, gbtree_store_loop, [Words, 10000]).
%% {Time,_} = timer:tc(hashtrie_test, gbtree_find_entries, [Words, G]), Time. f(Time).

