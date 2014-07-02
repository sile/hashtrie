README
======

概要
----

* Erlangによるマップ構造の実装
* 実装者が試した限りではErlang標準のdictやgb_treesよりも高速
  * 末尾参考リンクを参照
* もともとは [HAMT][hamt] の永続版の実装を試すことから派生
  * ただし [HAMT][hamt] を素直に実装するとかなり非効率だったため修正を続けるうちに、ほとんど別物に ...

[hamt]: http://lampwww.epfl.ch/papers/idealhashtrees.pdf "Hash Array Mapped Trie"

API
---

#### hashtrie:new() -> hashtrie()

    hashtrieインスタンスを作成する。


#### hashtrie:size(HashTrie) -> integer()

    HashTrieに格納されている要素数を返す。

#### hashtrie:find(Key, HashTrie) -> {ok, Value} | error

    キーに紐付く値をHashTrieから検索する。
    該当の値が存在する場合は{ok, 値}を、存在しない場合はerrorを返す。

#### hashtrie:fetch(Key, Hashtrie) -> Value

    キーに紐付く値をHashTrieから検索する。
    該当の値が存在しない場合はエラーが送出される。

#### hashtrie:fetch(Key, Hashtrie, Default) -> Value

    キーに紐付く値をHashTrieから検索する。
    該当の値が存在しない場合は Default が返される

#### hashtrie:store(Key, Value, HashTrie) -> hashtrie()

    要素(キーとそれに紐付く値)をHashTrieに格納する。
    既にキーが存在する場合は、その値が更新される。
    キーが存在しない場合は、キーと値のペア(要素)が新規に追加される。
    要素格納後のHashTrieインスタンスを返す。

#### hashtrie:erase(Key, HashTrie) -> hashtrie()

    キーに該当する要素をHashTrie内から(もし存在するなら)削除する。
    返り値は、要素削除後のHashTrieインスタンス。

#### hashtrie:fold(Fn, Initial, Hashtrie) -> Result

    要素の畳み込みを行う。
    畳み込み:
     1] 初めの要素に対して Fn(Key, Value, Initial) を適用する
     2] 二番目以降の要素に対してFn(Key, Value, 一つ前の適用結果)を実行する
     3] 一番最後に要素に対する適用結果が Result となり、fold関数呼び出し元に返される

#### hashtrie:foreach(Fn, HashTrie) -> ok

    HashTrieに格納されている全要素を走査し、各要素に対して、Fn関数を適用する。
    Fnのシグネチャは、Fn({Key,Value})->any()。

#### hashtrie:to_list(Hashtrie) -> [{Key, Value}]

    連想リストに変換する

#### hashtrie:from_list([{Key, Value}]) -> Hashtrie

    連想リストから生成する

参考
----

* [実装概略や計時結果など](http://d.hatena.ne.jp/sile/20101008/1286558755)

ベンチマーク
-----------

```bash
$ make start
Erlang/OTP 17 [erts-6.1] [source-d2a4c20] [64-bit] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]

> hashtrie_bench:print_bench().

# keytype method order element_count loop_count module elapsed((micr-seconds)
binary  store   random  1       100000        dict      40715
binary  store   random  1       100000    gb_trees      16156
binary  store   random  1       100000    hashtrie      26115
binary  store   random  10      10000         dict      46685
binary  store   random  10      10000     gb_trees      48922
binary  store   random  10      10000     hashtrie      26598
binary  store   random  100     1000          dict      66094
binary  store   random  100     1000      gb_trees      105902
binary  store   random  100     1000      hashtrie      60440
binary  store   random  1000    100           dict      101085
binary  store   random  1000    100       gb_trees      181185
binary  store   random  1000    100       hashtrie      56210
binary  store   random  10000   10            dict      156527
binary  store   random  10000   10        gb_trees      301739
binary  store   random  10000   10        hashtrie      84512
binary  store   random  100000  1             dict      966303
binary  store   random  100000  1         gb_trees      515256
binary  store   random  100000  1         hashtrie      211893
binary  store   sorted  1       100000        dict      43674
binary  store   sorted  1       100000    gb_trees      16312
binary  store   sorted  1       100000    hashtrie      26021
binary  store   sorted  10      10000         dict      50534
binary  store   sorted  10      10000     gb_trees      82369
binary  store   sorted  10      10000     hashtrie      26280
binary  store   sorted  100     1000          dict      69398
binary  store   sorted  100     1000      gb_trees      215936
binary  store   sorted  100     1000      hashtrie      59371
binary  store   sorted  1000    100           dict      105413
binary  store   sorted  1000    100       gb_trees      387721
binary  store   sorted  1000    100       hashtrie      54196
binary  store   sorted  10000   10            dict      159516
binary  store   sorted  10000   10        gb_trees      564711
binary  store   sorted  10000   10        hashtrie      83437
binary  store   sorted  100000  1             dict      1065637
binary  store   sorted  100000  1         gb_trees      899640
binary  store   sorted  100000  1         hashtrie      213461
binary  successful_erase        random  1       100000        dict      43674
binary  successful_erase        random  1       100000    gb_trees      11675
binary  successful_erase        random  1       100000    hashtrie      29664
binary  successful_erase        random  10      10000         dict      56894
binary  successful_erase        random  10      10000     gb_trees      13723
binary  successful_erase        random  10      10000     hashtrie      27976
binary  successful_erase        random  100     1000          dict      77509
binary  successful_erase        random  100     1000      gb_trees      20780
binary  successful_erase        random  100     1000      hashtrie      36548
binary  successful_erase        random  1000    100           dict      79254
binary  successful_erase        random  1000    100       gb_trees      26614
binary  successful_erase        random  1000    100       hashtrie      47484
binary  successful_erase        random  10000   10            dict      120651
binary  successful_erase        random  10000   10        gb_trees      35885
binary  successful_erase        random  10000   10        hashtrie      67618
binary  successful_erase        random  100000  1             dict      1512829
binary  successful_erase        random  100000  1         gb_trees      43578
binary  successful_erase        random  100000  1         hashtrie      182356
binary  successful_erase        sorted  1       100000        dict      43503
binary  successful_erase        sorted  1       100000    gb_trees      11876
binary  successful_erase        sorted  1       100000    hashtrie      26994
binary  successful_erase        sorted  10      10000         dict      56184
binary  successful_erase        sorted  10      10000     gb_trees      13941
binary  successful_erase        sorted  10      10000     hashtrie      27799
binary  successful_erase        sorted  100     1000          dict      74468
binary  successful_erase        sorted  100     1000      gb_trees      19974
binary  successful_erase        sorted  100     1000      hashtrie      36034
binary  successful_erase        sorted  1000    100           dict      77964
binary  successful_erase        sorted  1000    100       gb_trees      26678
binary  successful_erase        sorted  1000    100       hashtrie      46878
binary  successful_erase        sorted  10000   10            dict      115002
binary  successful_erase        sorted  10000   10        gb_trees      35667
binary  successful_erase        sorted  10000   10        hashtrie      67060
binary  successful_erase        sorted  100000  1             dict      1528297
binary  successful_erase        sorted  100000  1         gb_trees      42773
binary  successful_erase        sorted  100000  1         hashtrie      149938
binary  successful_find random  1       100000        dict      22350
binary  successful_find random  1       100000    gb_trees      11044
binary  successful_find random  1       100000    hashtrie      16880
binary  successful_find random  10      10000         dict      26925
binary  successful_find random  10      10000     gb_trees      17130
binary  successful_find random  10      10000     hashtrie      17007
binary  successful_find random  100     1000          dict      32385
binary  successful_find random  100     1000      gb_trees      33274
binary  successful_find random  100     1000      hashtrie      20837
binary  successful_find random  1000    100           dict      31884
binary  successful_find random  1000    100       gb_trees      54693
binary  successful_find random  1000    100       hashtrie      29254
binary  successful_find random  10000   10            dict      34588
binary  successful_find random  10000   10        gb_trees      85554
binary  successful_find random  10000   10        hashtrie      35208
binary  successful_find random  100000  1             dict      73289
binary  successful_find random  100000  1         gb_trees      143126
binary  successful_find random  100000  1         hashtrie      75929
binary  successful_find sorted  1       100000        dict      22275
binary  successful_find sorted  1       100000    gb_trees      11195
binary  successful_find sorted  1       100000    hashtrie      16793
binary  successful_find sorted  10      10000         dict      27110
binary  successful_find sorted  10      10000     gb_trees      17251
binary  successful_find sorted  10      10000     hashtrie      17004
binary  successful_find sorted  100     1000          dict      32886
binary  successful_find sorted  100     1000      gb_trees      32991
binary  successful_find sorted  100     1000      hashtrie      20984
binary  successful_find sorted  1000    100           dict      31584
binary  successful_find sorted  1000    100       gb_trees      52257
binary  successful_find sorted  1000    100       hashtrie      28401
binary  successful_find sorted  10000   10            dict      34175
binary  successful_find sorted  10000   10        gb_trees      75310
binary  successful_find sorted  10000   10        hashtrie      35173
binary  successful_find sorted  100000  1             dict      67527
binary  successful_find sorted  100000  1         gb_trees      97556
binary  successful_find sorted  100000  1         hashtrie      73328
integer store   random  1       100000        dict      41968
integer store   random  1       100000    gb_trees      16309
integer store   random  1       100000    hashtrie      24367
integer store   random  10      10000         dict      56702
integer store   random  10      10000     gb_trees      33281
integer store   random  10      10000     hashtrie      22794
integer store   random  100     1000          dict      56038
integer store   random  100     1000      gb_trees      67052
integer store   random  100     1000      hashtrie      44428
integer store   random  1000    100           dict      77129
integer store   random  1000    100       gb_trees      102466
integer store   random  1000    100       hashtrie      38683
integer store   random  10000   10            dict      93188
integer store   random  10000   10        gb_trees      134494
integer store   random  10000   10        hashtrie      53323
integer store   random  100000  1             dict      265307
integer store   random  100000  1         gb_trees      212701
integer store   random  100000  1         hashtrie      95462
integer store   sorted  1       100000        dict      49005
integer store   sorted  1       100000    gb_trees      16075
integer store   sorted  1       100000    hashtrie      24816
integer store   sorted  10      10000         dict      36694
integer store   sorted  10      10000     gb_trees      49903
integer store   sorted  10      10000     hashtrie      22966
integer store   sorted  100     1000          dict      60053
integer store   sorted  100     1000      gb_trees      124527
integer store   sorted  100     1000      hashtrie      44790
integer store   sorted  1000    100           dict      80742
integer store   sorted  1000    100       gb_trees      232519
integer store   sorted  1000    100       hashtrie      40484
integer store   sorted  10000   10            dict      93854
integer store   sorted  10000   10        gb_trees      311953
integer store   sorted  10000   10        hashtrie      54673
integer store   sorted  100000  1             dict      267780
integer store   sorted  100000  1         gb_trees      411660
integer store   sorted  100000  1         hashtrie      97307
integer successful_erase        random  1       100000        dict      42527
integer successful_erase        random  1       100000    gb_trees      13469
integer successful_erase        random  1       100000    hashtrie      21280
integer successful_erase        random  10      10000         dict      41166
integer successful_erase        random  10      10000     gb_trees      16653
integer successful_erase        random  10      10000     hashtrie      18719
integer successful_erase        random  100     1000          dict      63295
integer successful_erase        random  100     1000      gb_trees      27322
integer successful_erase        random  100     1000      hashtrie      32780
integer successful_erase        random  1000    100           dict      60483
integer successful_erase        random  1000    100       gb_trees      38130
integer successful_erase        random  1000    100       hashtrie      35598
integer successful_erase        random  10000   10            dict      79268
integer successful_erase        random  10000   10        gb_trees      49391
integer successful_erase        random  10000   10        hashtrie      41792
integer successful_erase        random  100000  1             dict      346257
integer successful_erase        random  100000  1         gb_trees      61139
integer successful_erase        random  100000  1         hashtrie      75287
integer successful_erase        sorted  1       100000        dict      42166
integer successful_erase        sorted  1       100000    gb_trees      13431
integer successful_erase        sorted  1       100000    hashtrie      21261
integer successful_erase        sorted  10      10000         dict      56702
integer successful_erase        sorted  10      10000     gb_trees      16770
integer successful_erase        sorted  10      10000     hashtrie      19468
integer successful_erase        sorted  100     1000          dict      63137
integer successful_erase        sorted  100     1000      gb_trees      26550
integer successful_erase        sorted  100     1000      hashtrie      33140
integer successful_erase        sorted  1000    100           dict      60238
integer successful_erase        sorted  1000    100       gb_trees      36133
integer successful_erase        sorted  1000    100       hashtrie      34836
integer successful_erase        sorted  10000   10            dict      78061
integer successful_erase        sorted  10000   10        gb_trees      50606
integer successful_erase        sorted  10000   10        hashtrie      44096
integer successful_erase        sorted  100000  1             dict      339798
integer successful_erase        sorted  100000  1         gb_trees      61395
integer successful_erase        sorted  100000  1         hashtrie      81465
integer successful_find random  1       100000        dict      22640
integer successful_find random  1       100000    gb_trees      11655
integer successful_find random  1       100000    hashtrie      15911
integer successful_find random  10      10000         dict      22115
integer successful_find random  10      10000     gb_trees      12321
integer successful_find random  10      10000     hashtrie      11724
integer successful_find random  100     1000          dict      24520
integer successful_find random  100     1000      gb_trees      19004
integer successful_find random  100     1000      hashtrie      13632
integer successful_find random  1000    100           dict      23256
integer successful_find random  1000    100       gb_trees      26367
integer successful_find random  1000    100       hashtrie      15395
integer successful_find random  10000   10            dict      23625
integer successful_find random  10000   10        gb_trees      37405
integer successful_find random  10000   10        hashtrie      20771
integer successful_find random  100000  1             dict      40784
integer successful_find random  100000  1         gb_trees      70834
integer successful_find random  100000  1         hashtrie      41908
integer successful_find sorted  1       100000        dict      23658
integer successful_find sorted  1       100000    gb_trees      11771
integer successful_find sorted  1       100000    hashtrie      16000
integer successful_find sorted  10      10000         dict      33281
integer successful_find sorted  10      10000     gb_trees      12983
integer successful_find sorted  10      10000     hashtrie      11235
integer successful_find sorted  100     1000          dict      22181
integer successful_find sorted  100     1000      gb_trees      18485
integer successful_find sorted  100     1000      hashtrie      13565
integer successful_find sorted  1000    100           dict      21435
integer successful_find sorted  1000    100       gb_trees      24457
integer successful_find sorted  1000    100       hashtrie      15022
integer successful_find sorted  10000   10            dict      21883
integer successful_find sorted  10000   10        gb_trees      31302
integer successful_find sorted  10000   10        hashtrie      20291
integer successful_find sorted  100000  1             dict      39173
integer successful_find sorted  100000  1         gb_trees      41624
integer successful_find sorted  100000  1         hashtrie      42781
tuple   store   random  1       100000        dict      46409
tuple   store   random  1       100000    gb_trees      16404
tuple   store   random  1       100000    hashtrie      28616
tuple   store   random  10      10000         dict      46375
tuple   store   random  10      10000     gb_trees      29966
tuple   store   random  10      10000     hashtrie      24928
tuple   store   random  100     1000          dict      93099
tuple   store   random  100     1000      gb_trees      69934
tuple   store   random  100     1000      hashtrie      44757
tuple   store   random  1000    100           dict      150872
tuple   store   random  1000    100       gb_trees      105048
tuple   store   random  1000    100       hashtrie      43744
tuple   store   random  10000   10            dict      221494
tuple   store   random  10000   10        gb_trees      161612
tuple   store   random  10000   10        hashtrie      64585
tuple   store   random  100000  1             dict      1118597
tuple   store   random  100000  1         gb_trees      351754
tuple   store   random  100000  1         hashtrie      214694
tuple   store   sorted  1       100000        dict      48632
tuple   store   sorted  1       100000    gb_trees      16633
tuple   store   sorted  1       100000    hashtrie      28895
tuple   store   sorted  10      10000         dict      48570
tuple   store   sorted  10      10000     gb_trees      53556
tuple   store   sorted  10      10000     hashtrie      25482
tuple   store   sorted  100     1000          dict      97793
tuple   store   sorted  100     1000      gb_trees      139203
tuple   store   sorted  100     1000      hashtrie      48564
tuple   store   sorted  1000    100           dict      149742
tuple   store   sorted  1000    100       gb_trees      229003
tuple   store   sorted  1000    100       hashtrie      46598
tuple   store   sorted  10000   10            dict      216375
tuple   store   sorted  10000   10        gb_trees      338923
tuple   store   sorted  10000   10        hashtrie      68662
tuple   store   sorted  100000  1             dict      1502592
tuple   store   sorted  100000  1         gb_trees      577909
tuple   store   sorted  100000  1         hashtrie      210750
tuple   successful_erase        random  1       100000        dict      53078
tuple   successful_erase        random  1       100000    gb_trees      15377
tuple   successful_erase        random  1       100000    hashtrie      31463
tuple   successful_erase        random  10      10000         dict      66667
tuple   successful_erase        random  10      10000     gb_trees      19663
tuple   successful_erase        random  10      10000     hashtrie      32130
tuple   successful_erase        random  100     1000          dict      131195
tuple   successful_erase        random  100     1000      gb_trees      32639
tuple   successful_erase        random  100     1000      hashtrie      40460
tuple   successful_erase        random  1000    100           dict      134561
tuple   successful_erase        random  1000    100       gb_trees      57356
tuple   successful_erase        random  1000    100       hashtrie      54798
tuple   successful_erase        random  10000   10            dict      218473
tuple   successful_erase        random  10000   10        gb_trees      64612
tuple   successful_erase        random  10000   10        hashtrie      87793
tuple   successful_erase        random  100000  1             dict      1543059
tuple   successful_erase        random  100000  1         gb_trees      84963
tuple   successful_erase        random  100000  1         hashtrie      166002
tuple   successful_erase        sorted  1       100000        dict      52478
tuple   successful_erase        sorted  1       100000    gb_trees      14854
tuple   successful_erase        sorted  1       100000    hashtrie      30899
tuple   successful_erase        sorted  10      10000         dict      69601
tuple   successful_erase        sorted  10      10000     gb_trees      19652
tuple   successful_erase        sorted  10      10000     hashtrie      32664
tuple   successful_erase        sorted  100     1000          dict      135418
tuple   successful_erase        sorted  100     1000      gb_trees      33615
tuple   successful_erase        sorted  100     1000      hashtrie      40628
tuple   successful_erase        sorted  1000    100           dict      151534
tuple   successful_erase        sorted  1000    100       gb_trees      44341
tuple   successful_erase        sorted  1000    100       hashtrie      54815
tuple   successful_erase        sorted  10000   10            dict      216805
tuple   successful_erase        sorted  10000   10        gb_trees      64296
tuple   successful_erase        sorted  10000   10        hashtrie      77804
tuple   successful_erase        sorted  100000  1             dict      1539361
tuple   successful_erase        sorted  100000  1         gb_trees      82275
tuple   successful_erase        sorted  100000  1         hashtrie      170348
tuple   successful_find random  1       100000        dict      25703
tuple   successful_find random  1       100000    gb_trees      11932
tuple   successful_find random  1       100000    hashtrie      18688
tuple   successful_find random  10      10000         dict      23806
tuple   successful_find random  10      10000     gb_trees      12637
tuple   successful_find random  10      10000     hashtrie      14620
tuple   successful_find random  100     1000          dict      41536
tuple   successful_find random  100     1000      gb_trees      20650
tuple   successful_find random  100     1000      hashtrie      16262
tuple   successful_find random  1000    100           dict      44149
tuple   successful_find random  1000    100       gb_trees      29053
tuple   successful_find random  1000    100       hashtrie      19737
tuple   successful_find random  10000   10            dict      45354
tuple   successful_find random  10000   10        gb_trees      41836
tuple   successful_find random  10000   10        hashtrie      24803
tuple   successful_find random  100000  1             dict      127052
tuple   successful_find random  100000  1         gb_trees      84582
tuple   successful_find random  100000  1         hashtrie      52292
tuple   successful_find sorted  1       100000        dict      26388
tuple   successful_find sorted  1       100000    gb_trees      11307
tuple   successful_find sorted  1       100000    hashtrie      20214
tuple   successful_find sorted  10      10000         dict      23595
tuple   successful_find sorted  10      10000     gb_trees      12364
tuple   successful_find sorted  10      10000     hashtrie      14797
tuple   successful_find sorted  100     1000          dict      40201
tuple   successful_find sorted  100     1000      gb_trees      19544
tuple   successful_find sorted  100     1000      hashtrie      16323
tuple   successful_find sorted  1000    100           dict      44287
tuple   successful_find sorted  1000    100       gb_trees      26184
tuple   successful_find sorted  1000    100       hashtrie      19207
tuple   successful_find sorted  10000   10            dict      45494
tuple   successful_find sorted  10000   10        gb_trees      34484
tuple   successful_find sorted  10000   10        hashtrie      26218
tuple   successful_find sorted  100000  1             dict      112963
tuple   successful_find sorted  100000  1         gb_trees      48392
tuple   successful_find sorted  100000  1         hashtrie      52314
```
