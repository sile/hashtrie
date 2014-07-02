%% @copyright 2010-2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Hask link Trie
-module(hashtrie).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         new/0,
         size/1,
         find/2,
         store/3,
         remove/2,
         %% TODO: fold/3,
         foreach/2
        ]).

-export_type([
              hashtrie/0, hashtrie/2,
              key/0,
              value/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(EMPTY_TABLE, {[], [], [], [], [], [], [], [],[], [], [], [], [], [], [], []}).
-define(hash(Key), erlang:phash2(Key)).
-define(index(HashCode), ((HashCode band 2#1111)+1)).
-define(nth_index(HashCode,N), (((HashCode bsr (4*N)) band 2#1111)+1)).
-define(next(HashCode), (HashCode bsr 4)).
-define(resize(Idx,Tab,Dep,N), resize_impl(element(Idx, Tab), Dep-1,N)).
-define(relocate(Idx,Tab,N), relocate_entries(element(Idx, Tab), N)).

-record(hashtrie, {count = 0                  :: count(),
                   next_resize_trigger = 16*4 :: count(),
                   root_depth = 0             :: depth(),
                   root = ?EMPTY_TABLE        :: table()}).

-opaque hashtrie()             :: #hashtrie{}.
-opaque hashtrie(_Key, _Value) :: #hashtrie{}.

-type key()      :: term().
-type value()    :: term().
-type entry()    :: {key(), value()}.
-type hashcode() :: non_neg_integer().
-type index()    :: non_neg_integer().
-type count()    :: non_neg_integer().
-type depth()    :: non_neg_integer().
-type table()    :: {child(), child(), child(), child(), child(), child(), child(), child(),
                     child(), child(), child(), child(), child(), child(), child(), child()}.
-type child()    :: table() | [entry()].

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new() -> hashtrie().
new() -> #hashtrie{}. 

-spec size(hashtrie()) -> non_neg_integer().
size(#hashtrie{count=Cnt}) -> Cnt.

-spec find(key(), hashtrie()) -> {ok, value()} | error.
find(Key, #hashtrie{root=Tab, root_depth=Dep}) ->
    case lists:keyfind(Key, 1, find_candidates(?hash(Key), Tab, Dep)) of
        false      -> error;
        {_, Value} -> {ok, Value}
    end.

-spec store(key(), value(), hashtrie()) -> hashtrie().
store(Key, Value, #hashtrie{count=Cnt,next_resize_trigger=Cnt}=Trie) ->
    store(Key, Value, resize(Trie));
store(Key, Value, #hashtrie{root=Tab, root_depth=Dep, count=Cnt}=Trie) ->
    {NewTab, NewCnt} = store_impl(Key, Value, ?hash(Key), Tab, Dep, Cnt),
    Trie#hashtrie{root=NewTab, count=NewCnt}.

-spec remove(key(), hashtrie()) -> hashtrie().
remove(Key, #hashtrie{root=Tab, root_depth=Dep, count=Cnt}=Trie) ->
    {NewTab,NewCnt} = remove_impl(Key, ?hash(Key), Tab, Dep, Cnt),
    Trie#hashtrie{root=NewTab,count=NewCnt}.

-spec foreach(function(), hashtrie()) -> ok.
foreach(Fn, #hashtrie{root=Tab, root_depth=Dep}) ->
    foreach_impl(Fn, Tab, 1, Dep).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec find_candidates(hashcode(), table(), depth()) -> [entry()].
find_candidates(Hash, Tab, 0) ->
    element(?index(Hash), Tab);
find_candidates(Hash, Tab, Dep) ->
    find_candidates(?next(Hash), element(?index(Hash),Tab), Dep-1).

-spec store_impl(key(), value(), hashcode(), table(), depth(), count()) -> {table(), count()}.
store_impl(Key, Value, Hash, Tab, 0, Cnt) ->
    Idx = ?index(Hash),
    {NewEntries, NewCnt} = list_insert(Key, Value, element(Idx,Tab), [], Cnt),
    {setelement(Idx,Tab,NewEntries), NewCnt};
store_impl(Key, Value, Hash, Tab, Dep, Cnt) ->
    Idx = ?index(Hash),
    {NewSubTab, NewCnt} = store_impl(Key, Value, ?next(Hash), element(Idx,Tab), Dep-1, Cnt),
    {setelement(Idx,Tab,NewSubTab), NewCnt}.

-spec list_insert(key(), value(), [entry()], [entry()], count()) -> {[entry()], count()}.
list_insert(Key, Value, [{Key,_}|Entries], Acc, Count) ->
    {[{Key,Value}|Acc]++Entries, Count};
list_insert(Key, Value, [], Acc, Count) ->
    {[{Key,Value}|Acc], Count+1};
list_insert(Key, Value, [Head|Entries], Acc, Count) ->
    list_insert(Key, Value, Entries, [Head|Acc], Count).

-spec resize(hashtrie()) -> hashtrie().
resize(#hashtrie{root=Tab,root_depth=Dep,next_resize_trigger=Size}=Trie) ->
    NewTab = resize_impl(Tab, Dep, Dep+1),
    Trie#hashtrie{root=NewTab, root_depth=Dep+1, next_resize_trigger=Size*16}.

-spec resize_impl(table(), depth(), depth()) -> table().
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

-spec relocate_entries([entry()], depth()) -> table().
relocate_entries(Entries, N) ->
    lists:foldl(fun({K,_}=Entry, NewTab) ->
                        Idx = ?nth_index(?hash(K),N),
                        setelement(Idx,NewTab,[Entry|element(Idx,NewTab)])
                end,
                ?EMPTY_TABLE,
                Entries).

-spec remove_impl(key(), hashcode(), table(), depth(), count()) -> {table(), count()}.
remove_impl(Key, Hash, Tab, 0, Cnt) ->
    Idx = ?index(Hash),
    {NewEntries, NewCnt} = list_remove(Key, element(Idx,Tab), [], Cnt),
    {setelement(Idx,Tab,NewEntries), NewCnt};
remove_impl(Key, Hash, Tab, Dep, Cnt) ->
    Idx = ?index(Hash),
    {NewSubTab, NewCnt} = remove_impl(Key, ?next(Hash), element(Idx,Tab), Dep-1, Cnt),
    {setelement(Idx,Tab,NewSubTab), NewCnt}.

-spec list_remove(key(), [entry()], [entry()], count()) -> {[entry()], count()}.
list_remove(_, [], Acc, Cnt)                  -> {Acc, Cnt};
list_remove(Key, [{Key,_}|Entries], Acc, Cnt) -> {Acc++Entries, Cnt-1};
list_remove(Key, [Head|Entries], Acc, Cnt)    -> list_remove(Key, Entries, [Head|Acc], Cnt).

-spec foreach_impl(function(), table(), index(), depth()) -> ok;
                  (function(), [entry()], index(), -1)    -> ok.
foreach_impl(Fn, Entries, _, -1) ->
    lists:foreach(Fn, Entries);
foreach_impl(_, _, 33, _) -> 
    ok;
foreach_impl(Fn, Tab, Idx, Dep) ->
    foreach_impl(Fn, element(Idx, Tab), 1, Dep-1),
    foreach_impl(Fn, Tab, Idx+1, Dep).
