%% File : hashtrie.erl
-module(hashtrie).
-export([new/0, size/1, find/2, store/3, remove/2, foreach/2]).
-vsn("0.0.3").

%% define
-define(EMPTY_TABLE, {[], [], [], [], [], [], [], [],[], [], [], [], [], [], [], []}).
-define(hash(Key), erlang:phash2(Key)).
-define(index(HashCode), ((HashCode band 2#1111)+1)).
-define(nth_index(HashCode,N), (((HashCode bsr (4*N)) band 2#1111)+1)).
-define(next(HashCode), (HashCode bsr 4)).
-define(resize(Idx,Tab,Dep,N), resize_impl(element(Idx, Tab), Dep-1,N)).
-define(relocate(Idx,Tab,N), relocate_entries(element(Idx, Tab), N)).

%% record: hashtrie
-record(hashtrie, {count = 0                  :: integer(),
                   next_resize_trigger = 16*4 :: integer(),
                   root_depth = 0             :: integer(),
                   root = ?EMPTY_TABLE        :: tuple() }).
-opaque hashtrie() :: #hashtrie{}.

%% function: new
-spec new() -> hashtrie().
new() ->
    #hashtrie{}. 

%% function: size
-spec size(hashtrie()) -> integer().
size(#hashtrie{count=Cnt}) ->
    Cnt.

%% function: find
-spec find(any(), hashtrie()) -> {value,any()} | false.
find(Key, #hashtrie{root=Tab, root_depth=Dep}) ->
    lists:keysearch(Key, 1, find_candidates(?hash(Key), Tab, Dep)).

find_candidates(Hash, Tab, 0) ->
    element(?index(Hash), Tab);
find_candidates(Hash, Tab, Dep) ->
    find_candidates(?next(Hash), element(?index(Hash),Tab), Dep-1).

%% function: store
-spec store(any(), any(), hashtrie()) -> hashtrie().
store(Key, Value, #hashtrie{count=Cnt,next_resize_trigger=Cnt}=Trie) ->
    store(Key, Value, resize(Trie));
store(Key, Value, #hashtrie{root=Tab, root_depth=Dep, count=Cnt}=Trie) ->
    {NewTab, NewCnt} = store_impl(Key, Value, ?hash(Key), Tab, Dep, Cnt),
    Trie#hashtrie{root=NewTab, count=NewCnt}.

store_impl(Key, Value, Hash, Tab, 0, Cnt) ->
    Idx = ?index(Hash),
    {NewEntries, NewCnt} = list_insert(Key, Value, element(Idx,Tab), [], Cnt),
    {setelement(Idx,Tab,NewEntries), NewCnt};
store_impl(Key, Value, Hash, Tab, Dep, Cnt) ->
    Idx = ?index(Hash),
    {NewSubTab, NewCnt} = store_impl(Key, Value, ?next(Hash), element(Idx,Tab), Dep-1, Cnt),
    {setelement(Idx,Tab,NewSubTab), NewCnt}.

list_insert(Key, Value, [{Key,_}|Entries], Acc, Count) ->
    {[{Key,Value}|Acc]++Entries, Count};
list_insert(Key, Value, [], Acc, Count) ->
    {[{Key,Value}|Acc], Count+1};
list_insert(Key, Value, [Head|Entries], Acc, Count) ->
    list_insert(Key, Value, Entries, [Head|Acc], Count).

resize(#hashtrie{root=Tab,root_depth=Dep,next_resize_trigger=Size}=Trie) ->
    NewTab = resize_impl(Tab, Dep, Dep+1),
    Trie#hashtrie{root=NewTab, root_depth=Dep+1, next_resize_trigger=Size*16}.

resize_impl(Tab, 0, N) ->
    {?relocate(01,Tab,N),?relocate(02,Tab,N),?relocate(03,Tab,N),?relocate(04,Tab,N),
     ?relocate(05,Tab,N),?relocate(06,Tab,N),?relocate(07,Tab,N),?relocate(08,Tab,N),
     ?relocate(09,Tab,N),?relocate(10,Tab,N),?relocate(11,Tab,N),?relocate(12,Tab,N),
     ?relocate(13,Tab,N),?relocate(14,Tab,N),?relocate(15,Tab,N),?relocate(16,Tab,N)};
resize_impl(Tab, Dep, N) ->
    {?resize(01,Tab,Dep,N),?resize(02,Tab,Dep,N),?resize(03,Tab,Dep,N),?resize(04,Tab,Dep,N),
     ?resize(05,Tab,Dep,N),?resize(06,Tab,Dep,N),?resize(07,Tab,Dep,N),?resize(08,Tab,Dep,N),
     ?resize(09,Tab,Dep,N),?resize(10,Tab,Dep,N),?resize(11,Tab,Dep,N),?resize(12,Tab,Dep,N),
     ?resize(13,Tab,Dep,N),?resize(14,Tab,Dep,N),?resize(15,Tab,Dep,N),?resize(16,Tab,Dep,N)}.

relocate_entries(Entries, N) ->
    lists:foldl(fun({K,_}=Entry, NewTab) ->
                        Idx = ?nth_index(?hash(K),N),
                        setelement(Idx,NewTab,[Entry|element(Idx,NewTab)])
                end,
                ?EMPTY_TABLE,
                Entries).

%% function: remove
-spec remove(any(), hashtrie()) -> hashtrie().
remove(Key, #hashtrie{root=Tab, root_depth=Dep, count=Cnt}=Trie) ->
    {NewTab,NewCnt} = remove_impl(Key, ?hash(Key), Tab, Dep, Cnt),
    Trie#hashtrie{root=NewTab,count=NewCnt}.

remove_impl(Key, Hash, Tab, 0, Cnt) ->
    Idx = ?index(Hash),
    {NewEntries, NewCnt} = list_remove(Key, element(Idx,Tab), [], Cnt),
    {setelement(Idx,Tab,NewEntries), NewCnt};
remove_impl(Key, Hash, Tab, Dep, Cnt) ->
    Idx = ?index(Hash),
    {NewSubTab, NewCnt} = remove_impl(Key, ?next(Hash), element(Idx,Tab), Dep-1, Cnt),
    {setelement(Idx,Tab,NewSubTab), NewCnt}.

list_remove(_, [], Acc, Cnt)                  -> {Acc, Cnt};
list_remove(Key, [{Key,_}|Entries], Acc, Cnt) -> {Acc++Entries, Cnt-1};
list_remove(Key, [Head|Entries], Acc, Cnt)    -> list_remove(Key, Entries, [Head|Acc], Cnt).

%% function: foreach
-spec foreach(function(), hashtrie()) -> done.
foreach(Fn, #hashtrie{root=Tab, root_depth=Dep}) ->
    foreach_impl(Fn, Tab, 1, Dep).

foreach_impl(Fn, Entries, _, -1) ->
    lists:foreach(Fn, Entries);
foreach_impl(_, _, 33, _) -> 
    done;
foreach_impl(Fn, Tab, Idx, Dep) ->
    foreach_impl(Fn, element(Idx, Tab), 1, Dep-1),
    foreach_impl(Fn, Tab, Idx+1, Dep).
