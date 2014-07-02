%% @copyright 2010-2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Hask link Trie
-module(hashtrie).

-compile(inline).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         new/0,
         size/1,
         find/2,
         fetch/2, fetch/3,
         store/3,
         erase/2,
         fold/3,
         foreach/2,
         to_list/1,
         from_list/1
        ]).

-export_type([
              hashtrie/0, hashtrie/2,
              key/0,
              value/0,
              fold_fun/0,
              acc/0,
              foreach_fun/0
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

-type key()        :: term().
-type value()      :: term().
-type entry()      :: {key(), value()}.
-type hashcode()   :: 0..16#FFFFFFFF.
-type count()      :: non_neg_integer().
-type depth()      :: 0..7.
-type table()      :: {child(), child(), child(), child(), child(), child(), child(), child(),
                       child(), child(), child(), child(), child(), child(), child(), child()}.
-type table_size() :: 17.
-type index()      :: 1..16.
-type child()      :: table() | [entry()].

-type acc()         :: term().
-type fold_fun()    :: fun ((key(), value(), acc()) -> acc()).
-type foreach_fun() :: fun ((key(), value()) -> any()).

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

-spec fetch(key(), hashtrie()) -> value().
fetch(Key, Trie) -> 
    case find(Key, Trie) of
        error       -> error({no_such_key, Key}, [Key, Trie]);
        {ok, Value} -> Value
    end.

-spec fetch(key(), hashtrie(), value()) -> value().
fetch(Key, Trie, Default) -> 
    case find(Key, Trie) of
        error       -> Default;
        {ok, Value} -> Value
    end.
            
-spec store(key(), value(), hashtrie()) -> hashtrie().
store(Key, Value, #hashtrie{count=Cnt,next_resize_trigger=Cnt}=Trie) ->
    store(Key, Value, resize(Trie));
store(Key, Value, #hashtrie{root=Tab, root_depth=Dep, count=Cnt}=Trie) ->
    {NewTab, NewCnt} = store_impl(Key, Value, ?hash(Key), Tab, Dep, Cnt),
    Trie#hashtrie{root=NewTab, count=NewCnt}.

-spec erase(key(), hashtrie()) -> hashtrie().
erase(Key, #hashtrie{root=Tab, root_depth=Dep, count=Cnt}=Trie) ->
    {NewTab,NewCnt} = erase_impl(Key, ?hash(Key), Tab, Dep, Cnt),
    Trie#hashtrie{root=NewTab,count=NewCnt}.

-spec fold(fold_fun(), acc(), hashtrie()) -> acc().
fold(Fun, Initial, #hashtrie{root=Tab, root_depth=Dep}) ->
    fold_impl(Fun, Tab, 1, Dep, Initial).
    
-spec foreach(foreach_fun(), hashtrie()) -> ok.
foreach(Fun, Trie) ->
    fold(fun (Key, Value, _Acc) ->
                 _ = Fun(Key, Value),
                 ok
         end, ok, Trie).

-spec to_list(hashtrie()) -> [{key(), value()}].
to_list(Trie) ->
    fold(fun (Key, Value, Acc) -> [{Key, Value} | Acc] end, [], Trie).

-spec from_list([{key(), value()}]) -> hashtrie().
from_list(Entries) ->
    lists:foldl(fun ({Key, Value}, Acc) -> store(Key, Value, Acc) end,
                new(),
                Entries).

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
    Entries0 = element(Idx, Tab),
    case lists:keytake(Key, 1, Entries0) of
        false                -> {setelement(Idx, Tab, [{Key, Value} | Entries0]), Cnt + 1};
        {value, _, Entries1} -> {setelement(Idx, Tab, [{Key, Value} | Entries1]), Cnt}
    end;
store_impl(Key, Value, Hash, Tab, Dep, Cnt) ->
    Idx = ?index(Hash),
    {NewSubTab, NewCnt} = store_impl(Key, Value, ?next(Hash), element(Idx,Tab), Dep-1, Cnt),
    {setelement(Idx,Tab,NewSubTab), NewCnt}.

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
relocate_entries([], _)      -> ?EMPTY_TABLE;
relocate_entries(Entries, N) ->
    [{HeadIdx, HeadEntry} | IdxToEntry] =
        lists:keysort(1, [{?nth_index(?hash(K), N), E} || {K, _} = E <- Entries]),

    IdxToEntries =
        lists:foldl(fun ({Idx, E}, [{Idx, Es} | Tail]) -> [{Idx, [E | Es]} | Tail];
                        ({Idx, E}, List)               -> [{Idx, [E]} | List]
                    end,
                    [{HeadIdx, [HeadEntry]}],
                    IdxToEntry),

    erlang:make_tuple(16, [], IdxToEntries).

-spec erase_impl(key(), hashcode(), table(), depth(), count()) -> {table(), count()}.
erase_impl(Key, Hash, Tab, 0, Cnt) ->
    Idx = ?index(Hash),
    case lists:keytake(Key, 1, element(Idx, Tab)) of
        false               -> {Tab, Cnt};
        {value, _, Entries} -> {setelement(Idx, Tab, Entries), Cnt - 1}
    end;
erase_impl(Key, Hash, Tab, Dep, Cnt) ->
    Idx = ?index(Hash),
    {NewSubTab, NewCnt} = erase_impl(Key, ?next(Hash), element(Idx,Tab), Dep-1, Cnt),
    {setelement(Idx,Tab,NewSubTab), NewCnt}.

-spec fold_impl(fold_fun(), table(), index() | table_size(), depth(), acc()) -> acc();
               (fold_fun(), [entry()], index(), -1, acc())                   -> acc().
fold_impl(Fun, Entries, _, -1, Acc) ->
    lists:foldl(Fun, Acc, Entries);
fold_impl(_, _, 17, _, Acc) ->
    Acc;
fold_impl(Fun, Tab, Idx, Dep, Acc0) ->
    Acc1 = fold_impl(Fun, element(Idx, Tab), 1, Dep - 1, Acc0),
    Acc2 = fold_impl(Fun, Tab, Idx + 1, Dep, Acc1),
    Acc2.
