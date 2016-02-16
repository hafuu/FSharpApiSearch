# FSharpApiSearch
F# API Search Engineはシグネチャや名前でF#のAPIを検索できる検索エンジンです。
Haskellの[Hoogle](https://www.haskell.org/hoogle/)、OCamlの[OCaml◎Scope](http://ocamloscope.herokuapp.com/)に影響を受けています。

**ベータ版なので仕様が不安定です。**

## プロジェクト一覧
| プロジェクト名          | 概要                                                     |
|-------------------------|----------------------------------------------------------|
| FSharpApiSearch         | 検索エンジン本体                                         |
| FSharpApiSearch.Console | 検索エンジンのフロントエンド(コンソールアプリケーション) |

### 導入方法
[リリースページ](https://github.com/hafuu/FSharpApiSearch/releases)からzipファイルをダウンロードし、展開してください。

## 使い方
FSharpApiSearch.Console.exeにクエリを与えずに実行するとインタラクティブモードで起動します。
インタラクティブモードを終了するには`#q`を入力してください。

    FSharpApiSearch.Console.exe

引数にクエリを渡すと一度だけ検索を行います。起動が遅いのでおすすめしません。

    FSharpApiSearch.Console.exe "int -> int"

デフォルトで検索できるアセンブリは`FSharp.Core`、`mscorlib`、`System`、`System.Core`です。
`--target`オプションを指定すると検索対象を追加できます。

    FSharpApiSearch.Console.exe --target:TargetAssembly1 --target:TargetAssembly2

検索対象のアセンブリが依存しているアセンブリも検索対象に追加する場合は`--target`オプションを使いますが、
検索対象のアセンブリが依存しているアセンブリを検索対象にしたくない場合は`--reference`オプションを使います。

    FSharpApiSearch.Console.exe --target:TargetAssembly --reference:DependentAssembly1 --reference:DependentAssembly2

## クエリ仕様
基本的にはF# のシグネチャと同じです。FSharpApiSearchの拡張のみ説明します。
また、`> `はFSharpApiSearch.Console.exeをインタラクティブモードで起動したときのプロンプトです。

### 名前検索
`name : signature`と書きます。シグネチャを指定しない場合は、シグネチャ部分に`_`を指定します。

    > id : 'a -> 'a
    Microsoft.FSharp.Core.Operators.id: 'T -> 'T, module value, distance: 0

    > choose : _
    Microsoft.FSharp.Collections.Array.choose: ('T -> option<'U>) -> 'T[] -> 'U[], module value, distance: 0
    Microsoft.FSharp.Collections.ArrayModule.Parallel.choose: ('T -> option<'U>) -> 'T[] -> 'U[], module value, distance: 0
    Microsoft.FSharp.Collections.List.choose: ('T -> option<'U>) -> list<'T> -> list<'U>, module value, distance: 0
    Microsoft.FSharp.Collections.Seq.choose: ('T -> option<'U>) -> seq<'T> -> seq<'U>, module value, distance: 0
    Microsoft.FSharp.Control.Event.choose: ('T -> option<'U>) -> IEvent<'Del, 'T> -> IEvent<'U>, module value, distance: 0
    Microsoft.FSharp.Control.Observable.choose: ('T -> option<'U>) -> IObservable<'T> -> IObservable<'U>, module value, distance: 0

### ワイルドカード
通常、`'a`などの型変数と`int`などの型名はマッチしません。
しかし、どちらのケースもまとめて検索したい場合があります。
このような場合に、ワイルドカード`?`が使えます。

    > ? -> List<?> -> ?
    Microsoft.FSharp.Collections.List.Cons: 'T * list<'T> -> list<'T>, static method, distance: 0
    Microsoft.FSharp.Collections.List.append: list<'T> -> list<'T> -> list<'T>, module value, distance: 0
    Microsoft.FSharp.Collections.List.averageBy: ('T -> 'U) -> list<'T> -> 'U, module value, distance: 0
    Microsoft.FSharp.Collections.List.choose: ('T -> option<'U>) -> list<'T> -> list<'U>, module value, distance: 0
    Microsoft.FSharp.Collections.List.chunkBySize: int -> list<'T> -> list<list<'T>>, module value, distance: 0
    ...

また、ワイルドカードに名前を付けることで、同じ名前を持つワイルドカードの位置には同一の型名(もしくは型変数)が入るという条件を追加できます。
例えば、`? -> ?`は以下のすべての関数にマッチします。

* `'a -> 'a`
* `int -> int`
* `'a -> int`
* `int -> string`

しかし、`?a -> ?a`のように名前を付けると、上の例では`'a -> int`や`int -> string`にはマッチしなくなります。

### メンバ検索
#### インスタンスメンバ
`receiver => signature`と書きます。

メソッドを検索する場合は`receiver => arg -> returnType`と書きます。

** 多引数メソッドの検索方法は議論中です **
多引数メソッドを検索するには`receiver => arg1 -> arg2 -> returnType`と書きます。
.NET形式のように引数部分をタプルのクエリ`receiver -> argA * argB -> returnType`は、多引数ではなくタプルが引数のメソッドを検索してしまうので注意して下さい。

プロパティを検索する場合は`receiver => propertyType`と書きます。
インデックス付きプロパティは`receiver => index -> propertyType`と書きます。

インスタンスメンバの検索は次の特別規則があります。

1. `arg -> receiver -> returnType` という形式の関数にマッチします。
2. 引数なしの検索（`receiver => propertyType`）は、`receiver => unit -> propertyType`という形式のインスタンスメンバともマッチします。

例は次の通りです。

    > string => int
    System.String.Length: int, instance property with get, distance: 0
    Microsoft.FSharp.Core.LanguagePrimitives.ParseInt32: string -> int32, module value, distance: 1
    Microsoft.FSharp.Core.String.length: string -> int, module value, distance: 1
    System.String.GetHashCode: unit -> int, instance method, distance: 1

`System.String.Length`は`int`を返すプロパティなのでマッチしていますが、
それに加えて`Microsoft.FSharp.Core.String.length`もマッチしています。
これは、特別規則1に該当するため返されています。

また、`System.String.GetHashCode`は特別規則2に該当するため返されています。

#### 静的メンバ
モジュール内の値と同じクエリで検索できます。多引数メソッドはインスタンスメソッドと同様に`arg1 -> arg2 -> returnType`と書きます。

### 類似検索モード
FSharpApiSearch.Console.exeに`--similarity+`オプションを付けて起動するか、
インタラクティブモードで`#similarity enable`を実行すると類似検索が有効になります。

類似検索を有効にしていると、型変数と型名、型名と型変数がマッチするようになり、
一致度が高い順に並び替えられて表示されます。

#### 型変数、型名の固定
類似検索モードで型名の前に`!`を付ける (`!int`、`!'a`等) と、その部分を固定(類似検索の部分的な無効化)して検索できます。

## 検索可能なAPI
| API                          | 例                             |
|------------------------------|--------------------------------|
| モジュールの関数と値         | `int -> string`                |
| レコード、構造体のフィールド | `Ref<'a> => 'a`                |
| メソッド、プロパティ         | `'a list => int`               |
| コンストラクタ               | `string -> Uri`                |
| 型略称                       | `string` means `System.String` |
| 名前 (関数名、メソッド名等)  | `head : 'a list -> 'a`         |

## 対応予定のAPI
* 型名
* 判別共用体
* アクティブパターン
* 関数の型略称
* 拡張メソッド
* 測定単位
* 型制約
* 可視性
* コンピュテーション式

## FSharp.Compiler.Service の制限により対応できないAPI
* C# で定義されたクラス、構造体のフィールド

## 動作環境
* Windows
* .Net Framework 4.5
* F# 4.0

## 使用ライブラリ
* [FSharp.Compiler.Service](http://fsharp.github.io/FSharp.Compiler.Service/)
* [FParsec](http://www.quanttec.com/fparsec/)
