%% @copyright 2010-2014 Takeru Ohta <phjgt308@gmail.com>
-module(hashtrie_bench).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         bench/0,
         print_bench/0
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
%% -define(USE_SPLAY_TREE, true).
%% -define(USE_MAPS, true).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec bench() -> [PerResult] when
      PerResult :: {KeyType, Method, Order, Case, module(), ElapsedMicroSeconds},
      KeyType   :: integer | binary | tuple,
      Order     :: sorted | random,
      Case      :: {1, 100000}  % {Size, LoopCount}
                 | {10, 10000}
                 | {100, 1000}
                 | {10000, 10}
                 | {100000, 1},
      Method    :: store | successful_find | successful_erase,
      ElapsedMicroSeconds :: non_neg_integer().
bench() ->
    Result0 = do_bench(integer, fun generate_sorted_integer_entries/1),
    Result1 = do_bench(binary, fun generate_sorted_binary_entries/1),
    Result2 = do_bench(tuple, fun generate_sorted_tuple_entries/1),
    lists:sort(Result0 ++ Result1 ++ Result2).

-spec print_bench() -> ok.
print_bench() ->
    io:format("\n"),
    lists:foreach(
      fun ({KeyType, Method, Order, {Size, LoopCount}, Module, Elapsed}) ->
              io:format("~s\t~s\t~s\t~p\t~p\t~10s\t~p\n",
                        [KeyType, Method, Order, Size, LoopCount, Module, Elapsed])
      end,
      bench()).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
do_bench(KeyType, GenerateFun) ->
    lists:flatmap(
      fun ({Size, LoopCount}) ->
              io:format(standard_error, "# [~s] ~p, ~p\n", [KeyType, Size, LoopCount]),

              Sorted = GenerateFun(Size),
              Random = shuffle(Sorted),
              lists:flatmap(
                fun (MapModule) ->
                        case lists:member(KeyType, method(MapModule, exclude)) of
                            true  -> [];
                            false ->
                                Empty = (method(MapModule, from_list))([]),
                                Map   = (method(MapModule, from_list))(Sorted),
                                [
                                 {KeyType, store, sorted, {Size, LoopCount}, MapModule, times(LoopCount, Sorted, Empty, method(MapModule, store))},
                                 {KeyType, store, random, {Size, LoopCount}, MapModule, times(LoopCount, Random, Empty, method(MapModule, store))},
                                 {KeyType, successful_find, sorted, {Size, LoopCount}, MapModule, times(LoopCount, Sorted, Map, method(MapModule, find))},
                                 {KeyType, successful_find, random, {Size, LoopCount}, MapModule, times(LoopCount, Random, Map, method(MapModule, find))},
                                 {KeyType, successful_erase, sorted, {Size, LoopCount}, MapModule, times(LoopCount, Sorted, Map, method(MapModule, erase))},
                                 {KeyType, successful_erase, random, {Size, LoopCount}, MapModule, times(LoopCount, Random, Map, method(MapModule, erase))}
                                ]
                        end
                end,
                map_modules())
      end,
      case_list()).

case_list() ->
    [
     {1, 100000},
     {10, 10000},
     {100, 1000},
     {1000, 100},
     {10000, 10},
     {100000, 1}
    ].

-spec map_modules() -> [module()].
map_modules() -> [dict, gb_trees, hashtrie, array] ++ map_modules_splay_tree() ++ map_modules_maps().

-spec map_modules_splay_tree() -> list().
-ifdef(USE_SPLAY_TREE).
map_modules_splay_tree() -> [splay_tree].
-else.
map_modules_splay_tree() -> [].
-endif.

-spec map_modules_maps() -> list().
-ifdef(USE_MAPS).
map_modules_maps() -> [maps].
-else.
map_modules_maps() -> [].
-endif.

-spec method(module(), Method) -> term() when
      Method :: exclude | from_list | store | find | erase.
method(dict, exclude)   -> [];
method(dict, from_list) -> fun dict:from_list/1;
method(dict, store)     -> fun ({K, V}, M) -> dict:store(K, V, M) end;
method(dict, find)      -> fun ({K, _}, M) -> _ = dict:find(K, M), M end;
method(dict, erase)     -> fun ({K, _}, M) -> dict:erase(K, M) end;

method(gb_trees, exclude)   -> [];
method(gb_trees, from_list) -> fun gb_trees:from_orddict/1;
method(gb_trees, store)     -> fun ({K, V}, M) -> gb_trees:enter(K, V, M) end;
method(gb_trees, find)      -> fun ({K, _}, M) -> _ = gb_trees:lookup(K, M), M end;
method(gb_trees, erase)     -> fun ({K, _}, M) -> gb_trees:delete_any(K, M) end;

method(hashtrie, exclude)   -> [];
method(hashtrie, from_list) -> fun hashtrie:from_list/1;
method(hashtrie, store)     -> fun ({K, V}, M) -> hashtrie:store(K, V, M) end;
method(hashtrie, find)      -> fun ({K, _}, M) -> _ = hashtrie:find(K, M), M end;
method(hashtrie, erase)     -> fun ({K, _}, M) -> hashtrie:erase(K, M) end;

method(array, exclude)   -> [binary, tuple];
method(array, from_list) -> fun array:from_orddict/1;
method(array, store)     -> fun ({K, V}, M) -> array:set(K, V, M) end;
method(array, find)      -> fun ({K, _}, M) -> _ = array:get(K, M), M end;
method(array, erase)     -> fun ({K, _}, M) -> array:reset(K, M) end;

method(splay_tree, Method) -> splay_tree_method(Method);
method(maps,       Method) -> maps_method(Method).

-ifdef(USE_SPLAY_TREE).
splay_tree_method(exclude)   -> [];
splay_tree_method(from_list) -> fun (List) -> splay_tree:from_list(shuffle(List)) end;
splay_tree_method(store)     -> fun ({K, V}, M) -> splay_tree:store(K, V, M) end;
splay_tree_method(find)      -> fun ({K, _}, M) -> element(2, splay_tree:find(K, M)) end;
splay_tree_method(erase)     -> fun ({K, _}, M) -> splay_tree:erase(K, M) end.
-else.
splay_tree_method(Method) -> error(badarg, [Method]).
-endif.

-ifdef(USE_MAPS).
maps_method(exclude)   -> [];
maps_method(from_list) -> fun (List) -> maps:from_list(shuffle(List)) end;
maps_method(store)     -> fun ({K, V}, M) -> maps:put(K, V, M) end;
maps_method(find)      -> fun ({K, _}, M) -> _ = maps:find(K, M), M end;
maps_method(erase)     -> fun ({K, _}, M) -> maps:remove(K, M) end.
-else.
maps_method(Method) -> error(badarg, [Method]).
-endif.

times(LoopCount, InputData, Map, Fun) ->
    true = garbage_collect(),
    {Elapsed, _} =
        timer:tc(
          fun () ->
                  loop(LoopCount, InputData, Map, Fun)
          end),
    Elapsed.

loop(0, _, _, _) ->
    ok;
loop(LoopCount, InputData, Map, Fun) ->
    _ = lists:foldl(fun (X, Acc) -> Fun(X, Acc) end, Map, InputData),
    loop(LoopCount - 1, InputData, Map, Fun).

generate_sorted_integer_entries(Count) ->
    [{X, X} || X <- lists:seq(0, Count - 1)].

generate_sorted_binary_entries(Count) ->
    generate_sorted_binary_entries(Count, []).

generate_sorted_binary_entries(Count, Acc) ->
    List0 = 
        [begin
             X = crypto:rand_bytes(random:uniform(100)),
             {X, X}
         end || _ <- lists:seq(0, Count - 1)],
    List1 = lists:usort(List0),
    List2 = lists:umerge(Acc, List1),
    case (length(Acc) + Count) - length(List2) of
        0 -> List2;
        N -> generate_sorted_binary_entries(N, List2)
    end.

generate_sorted_tuple_entries(Count) ->
    lists:sort(
      lists:zip(shuffle(generate_sorted_integer_entries(Count)),
                shuffle(generate_sorted_binary_entries(Count)))).

shuffle(List) ->
    [X||{_,X} <- lists:keysort(1, [{random:uniform(), N} || N <- List])].
