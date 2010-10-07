# README
## 概要
* Erlangによるマップ構造の実装
* 実装者が試した限りではErlang標準のdictやgb_treesよりも高速
  * 末尾参考リンクを参照
* もともとは [HAMT][hamt] の永続版の実装を試すことから派生
  * ただし [HAMT][hamt] を素直に実装するとかなり非効率だったため修正を続けるうちに、ほとんど別物に ...

[hamt]: http://lampwww.epfl.ch/papers/idealhashtrees.pdf "Hash Array Mapped Trie"

## API
#### hashtrie:new() -> hashtrie()

    hashtrieインスタンスを作成する。


#### hashtrie:size(HashTrie) -> integer()

    HashTrieに格納されている要素数を返す。

#### hashtrie:find(Key, HashTrie) -> {value,any()} | false

    キーに紐付く値をHashTrieから検索する。
    該当の値が存在する場合は{value, 値}を、存在しない場合はfalseを返す。

#### hashtrie:store(Key, Value, HashTrie) -> hashtrie()

    要素(キーとそれに紐付く値)をHashTrieに格納する。
    既にキーが存在する場合は、その値が更新される。
    キーが存在しない場合は、キーと値のペア(要素)が新規に追加される。
    要素格納後のHashTrieインスタンスを返す。

#### hashtrie:remove(Key, HashTrie) -> hashtrie()

    キーに該当する要素をHashTrie内から(もし存在するなら)削除する。
    返り値は、要素削除後のHashTrieインスタンス。

#### hashtrie:foreach(Fn, HashTrie) -> done

    HashTrieに格納されている全要素を走査し、各要素に対して、Fn関数を適用する。
    Fnのシグネチャは、Fn({Key,Value})->any()。

## 参考
* [実装概略や計時結果など](http://d.hatena.ne.jp/sile/20101007/)