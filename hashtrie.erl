-module(hashtrie).
-compile(export_all).

-define(ROOT, {[], [], [], [], [], [], [], [],
               [], [], [], [], [], [], [], [],
               [], [], [], [], [], [], [], [],
               [], [], [], [], [], [], [], []}).

-define(arc(HashCode), ((HashCode band 2#11111)+1)).
-define(narc(HashCode,N), (((HashCode bsr (5*N)) band 2#11111)+1)).
-define(next(HashCode), (HashCode bsr 5)).
-define(hash(Key), erlang:phash(Key,16#FFFFFFFF)).

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
    {resize_impl2(element(01, Table), N),
     resize_impl2(element(02, Table), N),
     resize_impl2(element(03, Table), N),
     resize_impl2(element(04, Table), N),
     resize_impl2(element(05, Table), N),
     resize_impl2(element(06, Table), N),
     resize_impl2(element(07, Table), N),
     resize_impl2(element(08, Table), N),
     resize_impl2(element(09, Table), N),
     resize_impl2(element(10, Table), N),
     resize_impl2(element(11, Table), N),
     resize_impl2(element(12, Table), N),
     resize_impl2(element(13, Table), N),
     resize_impl2(element(14, Table), N),
     resize_impl2(element(15, Table), N),
     resize_impl2(element(16, Table), N),
     resize_impl2(element(17, Table), N),
     resize_impl2(element(18, Table), N),
     resize_impl2(element(19, Table), N),
     resize_impl2(element(20, Table), N),
     resize_impl2(element(21, Table), N),
     resize_impl2(element(22, Table), N),
     resize_impl2(element(23, Table), N),
     resize_impl2(element(24, Table), N),
     resize_impl2(element(25, Table), N),
     resize_impl2(element(26, Table), N),
     resize_impl2(element(27, Table), N),
     resize_impl2(element(28, Table), N),
     resize_impl2(element(29, Table), N),
     resize_impl2(element(30, Table), N),
     resize_impl2(element(31, Table), N),
     resize_impl2(element(32, Table), N)};
resize_impl(Table, Depth, N) ->
    {resize_impl(element(01, Table), Depth-1, N),
     resize_impl(element(02, Table), Depth-1, N),
     resize_impl(element(03, Table), Depth-1, N),
     resize_impl(element(04, Table), Depth-1, N),
     resize_impl(element(05, Table), Depth-1, N),
     resize_impl(element(06, Table), Depth-1, N),
     resize_impl(element(07, Table), Depth-1, N),
     resize_impl(element(08, Table), Depth-1, N),
     resize_impl(element(09, Table), Depth-1, N),
     resize_impl(element(10, Table), Depth-1, N),
     resize_impl(element(11, Table), Depth-1, N),
     resize_impl(element(12, Table), Depth-1, N),
     resize_impl(element(13, Table), Depth-1, N),
     resize_impl(element(14, Table), Depth-1, N),
     resize_impl(element(15, Table), Depth-1, N),
     resize_impl(element(16, Table), Depth-1, N),
     resize_impl(element(17, Table), Depth-1, N),
     resize_impl(element(18, Table), Depth-1, N),
     resize_impl(element(19, Table), Depth-1, N),
     resize_impl(element(20, Table), Depth-1, N),
     resize_impl(element(21, Table), Depth-1, N),
     resize_impl(element(22, Table), Depth-1, N),
     resize_impl(element(23, Table), Depth-1, N),
     resize_impl(element(24, Table), Depth-1, N),
     resize_impl(element(25, Table), Depth-1, N),
     resize_impl(element(26, Table), Depth-1, N),
     resize_impl(element(27, Table), Depth-1, N),
     resize_impl(element(28, Table), Depth-1, N),
     resize_impl(element(29, Table), Depth-1, N),
     resize_impl(element(30, Table), Depth-1, N),
     resize_impl(element(31, Table), Depth-1, N),
     resize_impl(element(32, Table), Depth-1, N)}.
      

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
