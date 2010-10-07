-module(hashtrie_test).
-export([gen_int_entries/2, gen_str_entries/1]).
-export([store_entries/2, store_entries/3, store_loop/4, find_entries/3]).
-export([store_time/2, store_time/3, find_time/3]).
-export([store_memory/2]).

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

store_entries(Entries, InitObj, StoreFn) ->
    lists:foldl(fun ({K,V},T) -> StoreFn(K, V, T) end,
                InitObj,
                Entries).

store_entries(Entries, hashtrie) ->
    hashtrie_test:store_entries(Entries, hashtrie:new(), fun hashtrie:store/3);
store_entries(Entries, dict) ->
    hashtrie_test:store_entries(Entries, dict:new(), fun dict:store/3);
store_entries(Entries, gb_trees) ->
    hashtrie_test:store_entries(Entries, gb_trees:empty(), fun gb_trees:insert/3).

store_loop(_, _, _, 0) -> done;
store_loop(Entries, InitObj, StoreFn, LoopCount) ->
    store_entries(Entries, InitObj, StoreFn),
    store_loop(Entries, InitObj, StoreFn, LoopCount-1).

find_entries(Entries, Obj, FindFn) ->
    lists:foldl(fun ({K,_}, Cnt) -> 
                        case FindFn(K, Obj) of
                            {_,_} -> Cnt+1;
                            _     -> Cnt
                        end
                end,
                0,
                Entries).

extract_time({Time,_}) -> Time.

store_time(Entries, MapType) ->
    extract_time(timer:tc(?MODULE, store_entries, [Entries, MapType])). 

store_time(Entries, hashtrie, LoopCount) ->
    extract_time(timer:tc(?MODULE, store_loop, 
                          [Entries, hashtrie:new(), fun hashtrie:store/3, LoopCount]));
store_time(Entries, dict, LoopCount) ->
    extract_time(timer:tc(?MODULE, store_loop, 
                          [Entries, dict:new(), fun dict:store/3, LoopCount]));
store_time(Entries, gb_trees, LoopCount) ->
    extract_time(timer:tc(?MODULE, store_loop, 
                          [Entries, gb_trees:empty(), fun gb_trees:insert/3, LoopCount])).

find_time(Entries, Trie, hashtrie) ->
    extract_time(timer:tc(?MODULE, find_entries,
                          [Entries, Trie, fun hashtrie:find/2]));
find_time(Entries, Dict, dict) ->
    extract_time(timer:tc(?MODULE, find_entries,
                          [Entries, Dict, fun dict:find/2]));
find_time(Entries, Tree, gb_trees) ->
    extract_time(timer:tc(?MODULE, find_entries,
                          [Entries, Tree, fun gb_trees:lookup/2])).

store_memory(Entries, MapType) ->
    erlang:garbage_collect(),
    erlang:garbage_collect(),
    Pre = erlang:memory(processes),
    _Map = store_entries(Entries, MapType),
    Post = erlang:memory(processes),
    Post-Pre.

%% Entries = hashtrie_test:gen_int_entries(0,100000).
%% Entries = hashtrie_test:gen_str_entries("/hoge/hoge.txt").

%% hashtrie_test:store_time(Entries, hashtrie).
%% hashtrie_test:store_time(Entries, dict).
%% hashtrie_test:store_time(Entries, gb_trees).

%% hashtrie_test:store_time(Entries, hashtrie, 10).
%% hashtrie_test:store_time(Entries, dict, 10).
%% hashtrie_test:store_time(Entries, gb_trees, 10).

%% hashtrie_test:store_memory(Entries, hashtrie).
%% hashtrie_test:store_memory(Entries, dict).
%% hashtrie_test:store_memory(Entries, gb_trees).

%% T = hashtrie_test:store_entries(Entries, hashtrie).
%% D = hashtrie_test:store_entries(Entries, dict).
%% G = hashtrie_test:store_entries(Entries, gb_trees).

%% hashtrie_test:find_time(Entries, T, hashtrie).
%% hashtrie_test:find_time(Entries, D, dict).
%% hashtrie_test:find_time(Entries, G, gb_trees).



