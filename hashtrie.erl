-module(hashtrie).
-compile(export_all).

-define(ROOT, {[], [], [], [], [], [], [], [],
               [], [], [], [], [], [], [], [],
               [], [], [], [], [], [], [], [],
               [], [], [], [], [], [], [], []}).

-define(arc(HashCode), ((HashCode band 2#11111)+1)).
-define(narc(HashCode,N), (((HashCode bsr (5*N)) band 2#11111)+1)).
-define(next(HashCode), (HashCode bsr 5)).
-define(hash(Key), erlang:phash2(Key)).
-define(tuplize(Arc,Table,N), resize_impl2(element(Arc, Table), N)).
-define(resize(Arc,Table,Depth,N), resize_impl(element(Arc, Table), Depth-1,N)).

-record(hashtrie, {count = 0,
                   resize_border = 32*8,
                   root_depth = 0,
                   root = ?ROOT}).

add_list([], Hamt) -> Hamt;
add_list([{Key,Value}|Pairs], Hamt) ->
    add_list(Pairs, add(Key,Value,Hamt)).

sh1([],_) -> done;
sh1([{K,_}|Pairs],H) ->
    case find(K,H) of
        {K,_} -> sh1(Pairs,H);
        R -> {failed,R,K}
    end.

new() ->
    #hashtrie{}.

find(Key, #hashtrie{root=Table,root_depth=Depth}) ->
    lists:keyfind(Key, 1, find_candidates(?hash(Key), Table, Depth)).

find_candidates(Hash, Table, 0) ->
    element(?arc(Hash), Table);
find_candidates(Hash, Table, Depth) ->
    find_candidates(?next(Hash), element(?arc(Hash),Table), Depth-1).

resize(#hashtrie{root=Table,root_depth=Depth,resize_border=Border}=Trie) ->
    NewTable = resize_impl(Table, Depth, Depth+1),
    Trie#hashtrie{root=NewTable, root_depth=Depth+1, resize_border=Border*32}.

resize_impl2(Pairs, N) ->
    lists:foldl(fun({K,_}=Pair, Sub) ->
                        Arc = ?narc(?hash(K),N),
                        setelement(Arc,Sub,[Pair|element(Arc,Sub)])
                end,
                ?ROOT,
                Pairs). % XXX:
     
resize_impl(Table, 0, N) ->
    {?tuplize(01,Table,N),?tuplize(02,Table,N),?tuplize(03,Table,N),?tuplize(04,Table,N),
     ?tuplize(05,Table,N),?tuplize(06,Table,N),?tuplize(07,Table,N),?tuplize(08,Table,N),
     ?tuplize(09,Table,N),?tuplize(10,Table,N),?tuplize(11,Table,N),?tuplize(12,Table,N),
     ?tuplize(13,Table,N),?tuplize(14,Table,N),?tuplize(15,Table,N),?tuplize(16,Table,N),
     ?tuplize(17,Table,N),?tuplize(18,Table,N),?tuplize(19,Table,N),?tuplize(20,Table,N),
     ?tuplize(21,Table,N),?tuplize(22,Table,N),?tuplize(23,Table,N),?tuplize(24,Table,N),
     ?tuplize(25,Table,N),?tuplize(26,Table,N),?tuplize(27,Table,N),?tuplize(28,Table,N),
     ?tuplize(29,Table,N),?tuplize(30,Table,N),?tuplize(31,Table,N),?tuplize(32,Table,N)};
resize_impl(Table, Depth, N) ->
    {?resize(01,Table,Depth,N),?resize(02,Table,Depth,N),?resize(03,Table,Depth,N),?resize(04,Table,Depth,N),
     ?resize(05,Table,Depth,N),?resize(06,Table,Depth,N),?resize(07,Table,Depth,N),?resize(08,Table,Depth,N),
     ?resize(09,Table,Depth,N),?resize(10,Table,Depth,N),?resize(11,Table,Depth,N),?resize(12,Table,Depth,N),
     ?resize(13,Table,Depth,N),?resize(14,Table,Depth,N),?resize(15,Table,Depth,N),?resize(16,Table,Depth,N),
     ?resize(17,Table,Depth,N),?resize(18,Table,Depth,N),?resize(19,Table,Depth,N),?resize(20,Table,Depth,N),
     ?resize(21,Table,Depth,N),?resize(22,Table,Depth,N),?resize(23,Table,Depth,N),?resize(24,Table,Depth,N),
     ?resize(25,Table,Depth,N),?resize(26,Table,Depth,N),?resize(27,Table,Depth,N),?resize(28,Table,Depth,N),
     ?resize(29,Table,Depth,N),?resize(30,Table,Depth,N),?resize(31,Table,Depth,N),?resize(32,Table,Depth,N)}.

add(Key, Value, #hashtrie{count=Count,resize_border=Count}=Trie) ->
    add(Key, Value, resize(Trie));
add(Key, Value, #hashtrie{root=Table,root_depth=Depth,count=Count}=Trie) ->
    {NewTable,NewCount} = add_impl(Key, Value, ?hash(Key), Table, Depth, Count),
    Trie#hashtrie{root=NewTable, count=NewCount}.

add_impl(Key, Value, Hash, Table, 0, Count) ->
    Arc=?arc(Hash),
    {NewList,NewCount} = insert(Key, Value, element(Arc,Table), [], Count),
    {setelement(Arc,Table,NewList), NewCount};
add_impl(Key, Value, Hash, Table, Depth, Count) ->
    Arc=?arc(Hash),
    {SubNode, NewCount} = add_impl(Key, Value, 
                                   ?next(Hash), element(Arc,Table), 
                                   Depth-1, Count),
    {setelement(Arc,Table,SubNode), NewCount}.

insert(Key, Value, [{Key,_}|Pairs], Acc, Count) ->
    {[{Key,Value}|Acc]++Pairs, Count};
insert(Key, Value, [], Acc, Count) ->
    {[{Key,Value}|Acc], Count+1};
insert(Key, Value, [Head|Pairs], Acc, Count) ->
    insert(Key, Value, Pairs, [Head|Acc], Count).
