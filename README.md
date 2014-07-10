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

* [実装概略や計時結果など(2010)](http://d.hatena.ne.jp/sile/20101008/1286558755)
* [ベンチマーク(2014)](http://sile.hatenablog.jp/entry/2014/07/04/102201)
