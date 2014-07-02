-module(hashtrie_test).
-export([gen_int_entries/2, gen_str_entries/1]).
-export([store_entries/2, store_entries/3, store_loop/4, find_entries/3]).
-export([store_time/2, store_time/3, find_time/3]).
-export([store_memory/2]).

gen_int_entries(Start,End) ->
    lists:map(fun (X) -> {X,X} end, lists:seq(Start,End-1)).

gen_str_entries(Filepath) ->
    {ok, In} = file:open(Filepath, [read]),
    read_lines(In,[],0).

read_lines(In, Lines, LineNum) ->
    case file:read_line(In) of
        {ok, Line} -> 
            BinaryLine = list_to_binary(string:strip(Line,right,$\n)),
            read_lines(In, [{BinaryLine,LineNum}|Lines], LineNum+1);
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
    Before = erlang:memory(processes),
    _Map = store_entries(Entries, MapType),
    After = erlang:memory(processes),
    After-Before.

%% Num10 = hashtrie_test:gen_int_entries(0, 10).
%% Num100 = hashtrie_test:gen_int_entries(0, 100).
%% Num100000 = hashtrie_test:gen_int_entries(0, 100000).
%% Str100000 = hashtrie_test:gen_str_entries("words.100000").
%% Str300000 = hashtrie_test:gen_str_entries("words").

%% hashtrie_test:store_time(Num10, hashtrie, 10000).
%% hashtrie_test:store_time(Num10, dict, 10000).    
%% hashtrie_test:store_time(Num10, gb_trees, 10000).

%% hashtrie_test:store_time(Num100, hashtrie, 1000).
%% hashtrie_test:store_time(Num100, dict, 1000).    
%% hashtrie_test:store_time(Num100, gb_trees, 1000).

%% hashtrie_test:store_time(Num100000, hashtrie).
%% hashtrie_test:store_time(Num100000, dict).    
%% hashtrie_test:store_time(Num100000, gb_trees).

%% hashtrie_test:store_time(Str100000, hashtrie).
%% hashtrie_test:store_time(Str100000, dict).
%% hashtrie_test:store_time(Str100000, gb_trees).

%% hashtrie_test:store_memory(Str300000, hashtrie).
%% hashtrie_test:store_memory(Str300000, dict).
%% hashtrie_test:store_memory(Str300000, gb_trees).

%% Tn = hashtrie_test:store_entries(Num100000, hashtrie), done.
%% Dn = hashtrie_test:store_entries(Num100000, dict), done.
%% Gn = hashtrie_test:store_entries(Num100000, gb_trees), done.

%% Ts = hashtrie_test:store_entries(Str100000, hashtrie), done.
%% Ds = hashtrie_test:store_entries(Str100000, dict), done.
%% Gs = hashtrie_test:store_entries(Str100000, gb_trees), done.

%% hashtrie_test:find_time(Num100000, Tn, hashtrie).
%% hashtrie_test:find_time(Num100000, Dn, dict).    
%% hashtrie_test:find_time(Num100000, Gn, gb_trees).

%% hashtrie_test:find_time(Str300000, Ts, hashtrie).
%% hashtrie_test:find_time(Str300000, Ds, dict).    
%% hashtrie_test:find_time(Str300000, Gs, gb_trees).
