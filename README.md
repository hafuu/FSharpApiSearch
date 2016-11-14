# FSharpApiSearch
F# API Search Engineはシグネチャや名前でF#のAPIを検索できる検索エンジンです。

## プロジェクト一覧
| プロジェクト名           | 概要                                                     |
|--------------------------|----------------------------------------------------------|
| FSharpApiSearch          | 検索エンジン本体                                         |
| FSharpApiSearch.Database | 検索エンジンのデータベース作成ツール                     |
| FSharpApiSearch.Console  | 検索エンジンのフロントエンド(コンソールアプリケーション) |

### 導入方法
[リリースページ](https://github.com/hafuu/FSharpApiSearch/releases)からzipファイルをダウンロードし、展開してください。

## 使い方
まず最初にFSharpApiSearch.Database.exeを実行してデータベースを作成します。

    FSharpApiSearch.Database.exe

FSharpApiSearch.Console.exeにクエリを与えずに実行するとインタラクティブモードで起動します。
インタラクティブモードを終了するには`#q`を入力してください。

    FSharpApiSearch.Console.exe

引数にクエリを渡すと一度だけ検索を行います。

    FSharpApiSearch.Console.exe "int -> int"

デフォルトで検索できるアセンブリは`FSharp.Core`、`mscorlib`、`System`、`System.Core`です。
データベース作成時にアセンブリを指定すると検索対象を追加できます。`--lib`オプションでアセンブリを検索するディレクトリを指定できます。
指定するアセンブリが依存するアセンブリも指定して下さい。

    FSharpApiSearch.Database.exe --lib:TargetAssemblyDirectory TargetAssembly1 TargetAssembly2 DependentAssembly

データベースを作成した後、実際に検索で使用するには`--target`オプションを使用します。
FSharpApiSearch.Console.exeの`--target`オプションを使用するとデータベースを作成したアセンブリを検索対象に指定できます。

    FSharpApiSearch.Console.exe --target:TargetAssembly1 --target:TargetAssembly2

`--target`オプションを使用するとデフォルトの`FSharp.Core`、`mscorlib`、`System`、`System.Core`は検索対象に含まれなくなるため、
検索対象に含めたい場合は明示的に指定します。

    FSharpApiSearch.Console.exe --target:TargetAssembly1 --target:TargetAssembly2 --target:FSharp.Core --target:mscorlib --target:System --target:System.Core

## クエリ仕様
基本的にはF# のシグネチャと同じです。FSharpApiSearchの拡張のみ詳細を説明します。
また、`> `はFSharpApiSearch.Console.exeをインタラクティブモードで起動したときのプロンプトです。

### 検索可能なAPI
| API                          | クエリ例                                                 |
|------------------------------|----------------------------------------------------------|
| モジュールの関数と値         | `int -> string`                                          |
| レコード、構造体のフィールド | `Ref<'a> => 'a`                                          |
| 判別共用体                   | `'a -> Option<'a>`                                       |
| メソッド、プロパティ         | `'a list -> int`<br>`'a list => int`                     |
| コンストラクタ               | `Uri : _`<br>`Uri.new : _`<br>`Uri..ctor : _`            |
| 名前 (関数名、メソッド名等)  | `head : 'a list -> 'a`                                   |
| アクティブパターン           | `(||) : ... -> Expr -> ?`                                |
| 型、型略称、モジュール       | `List<'T>`                                               |
| コンピュテーション式         | `{ let! } : Async<'T>`                                   | 

### 名前検索
`name : signature`と書きます。シグネチャを指定しない場合は、シグネチャ部分に`_`を指定します。

    > id : 'a -> 'a
    Microsoft.FSharp.Core.Operators.id: 'T -> 'T, module value, FSharp.Core

    > choose : _
    Microsoft.FSharp.Collections.Array.Parallel.choose: ('T -> option<'U>) -> 'T[] -> 'U[], module value, FSharp.Core
    Microsoft.FSharp.Collections.Array.choose: ('T -> option<'U>) -> 'T[] -> 'U[], module value, FSharp.Core
    Microsoft.FSharp.Collections.List.choose: ('T -> option<'U>) -> list<'T> -> list<'U>, module value, FSharp.Core
    Microsoft.FSharp.Collections.Seq.choose: ('T -> option<'U>) -> seq<'T> -> seq<'U>, module value, FSharp.Core
    Microsoft.FSharp.Control.Event.choose: ('T -> option<'U>) -> IEvent<'Del, 'T> -> IEvent<'U>, module value, FSharp.Core
      when 'Del : delegate and 'Del :> Delegate
    Microsoft.FSharp.Control.Observable.choose: ('T -> option<'U>) -> IObservable<'T> -> IObservable<'U>, module value, FSharp.Core

アスタリスク(`*`)を使用すると部分一致検索ができます。
例えば、`FSharp.Core.String.* : _`は`FSharp.Core.String`モジュールの全てのAPIを表示します。

    > FSharp.Core.String.* : _
    Microsoft.FSharp.Core.String.collect: (char -> string) -> string -> string, module value, FSharp.Core
    Microsoft.FSharp.Core.String.concat: string -> seq<string> -> string, module value, FSharp.Core
    Microsoft.FSharp.Core.String.exists: (char -> bool) -> string -> bool, module value, FSharp.Core
    Microsoft.FSharp.Core.String.filter: (char -> bool) -> string -> string, module value, FSharp.Core
    ...

### ワイルドカード
通常、`'a`などの型変数と`int`などの型名はマッチしません。
しかし、どちらのケースもまとめて検索したい場合があります。
このような場合に、ワイルドカード`?`が使えます。

    > ? -> list<?> -> ?
    Microsoft.FSharp.Collections.List.append: list<'T> -> list<'T> -> list<'T>, module value, FSharp.Core
    Microsoft.FSharp.Collections.List.averageBy: ('T -> 'U) -> list<'T> -> 'U, module value, FSharp.Core
      when 'U : (static member op_Addition : 'U * 'U -> 'U) and 'U : (static member DivideByInt : 'U * int -> 'U) and 'U : (static member get_Zero : unit -> 'U)
    Microsoft.FSharp.Collections.List.choose: ('T -> option<'U>) -> list<'T> -> list<'U>, module value, FSharp.Core
    Microsoft.FSharp.Collections.List.chunkBySize: int -> list<'T> -> list<list<'T>>, module value, FSharp.Core
    ...

また、ワイルドカードに名前を付けることで、同じ名前を持つワイルドカードの位置には同一の型名が入るという条件を追加できます。
例えば、`? -> ?`は以下のすべての関数にマッチします。

* `'a -> 'a`
* `int -> int`
* `'a -> int`
* `int -> string`

しかし、`?a -> ?a`のように名前を付けると、上の例では`'a -> int`や`int -> string`にはマッチしなくなります。

### メンバ検索
#### インスタンスメンバ
`receiver -> signature`と書きます。

メソッドを検索する場合は`receiver -> arg -> returnType`と書きます。

多引数のメソッドを検索するには`receiver -> arg1 -> arg2 -> returnType`または`receiver -> arg1 * arg2 -> returnType`と書きます。
通常ではメソッドの引数がタプル形式（`arg1 * arg2`）とカリー化形式（`arg1 -> arg2`）を区別せずに検索します。
引数の形式を区別して検索したい場合は`ignore-param-style`オプションを無効にします。

プロパティを検索する場合は`receiver -> propertyType`と書きます。
インデックス付きプロパティは`receiver -> index -> propertyType`と書きます。

`receiver -> signature`形式の検索ではインスタンスメンバと関数の両方を検索範囲としますが、
`receiver`と`signature`の区切りに`->`ではなく`=>`を使用すると、検索範囲をインスタンスメンバに限定できます。

`=>`を使ったインスタンスメンバ限定の検索には次の特別規則があります。

1. `arg -> receiver -> returnType` という形式の関数にマッチします。
2. 引数なし（プロパティ）の検索（`receiver => propertyType`）は、`receiver => unit -> propertyType`という形式のインスタンスメソッドともマッチします。

例は次の通りです。

    > string => int
    System.String.Length: int, instance property with get, mscorlib
    Microsoft.FSharp.Core.String.length: string -> int, module value, FSharp.Core
    System.String.GetHashCode: unit -> int, instance method, mscorlib

`System.String.Length`は`int`を返すプロパティなのでマッチしていますが、
それに加えて`Microsoft.FSharp.Core.String.length`もマッチしています。
これは、特別規則1に該当するため返されています。

また、`System.String.GetHashCode`は特別規則2に該当するため返されています。

#### 静的メンバ
モジュール内の値や関数と同じクエリで検索できます。多引数メソッドはインスタンスメソッドと同様に`arg1 -> arg2 -> returnType`または`arg1 * arg2 -> returnType`と書きます。

### アクティブパターン
`(||) : (args ->) inputType -> returnType`と書きます。
パーシャルアクティブパターンを検索する場合は`(|_|) : (args ->) inputType -> returnType`と書きます。

`inputType`の部分にはアクティブパターンで扱う型を指定します。例えば、`Expr`に対するアクティブパターンを検索したい場合は`(||) : ... -> Expr -> ?`と書きます。

`args`の部分にはアクティブパターンの引数を指定します。
引数があるアクティブパターンを検索したい場合は`(||) : arg1 -> arg2 -> inputType -> returnType`と書きます。
引数が無いアクティブパターンを検索したい場合は`(||) : inputType -> returnType`と書きます。
引数の有無を区別せず検索する場合は引数部分に`...`というキーワードを用いて、`(||) : ... -> inputType -> returnType`と書きます。

`retyrnType`の部分にはアクティブパターンの実態である関数が返す型を指定します。
アクティブパターン関数の戻り値は、ケースが1つ、複数、パーシャルアクティブパターンそれぞれで異なります。
対応する任意の型、`option<_>`、`Choice<_,...,_>`を指定して下さい。
通常はワイルドカード（`?`）を使うことをお勧めします。

### コンピュテーション式
`{ syntax } : type`と書きます。指定した構文が使えて、指定した型を扱えるビルダーを検索します。

`syntax`には`let!`、`yield`、`yield!`、`return`、`return!`、`use`、`use!`、`if/then`、`for`、`while`、`try/with`、`try/finally`と任意のカスタムオペレーション名を指定できます。
複数指定する場合は`;`で区切ります。


## 検索オプション

### `respect-name-difference`オプション
`respect-name-difference`オプションが有効の場合は、クエリ中の異なる型変数や名前付きワイルドカードの名前の違いを検索に反映します。
異なる名前同士は同じ型にマッチしません。
例えば、`?a -> ?a`というクエリは`int -> int`というシグネチャにマッチしますが、`?a -> ?b`というクエリは`int -> int`にマッチしません。

このオプションに無効にした場合は、`?a -> ?b`というクエリで`int -> int`にマッチします。

### `greedy-matching`オプション
`greedy-matching`オプションが有効の場合は、型変数と他の型がそれぞれマッチするようになり、一致度が高い順に並び替えられて表示されます。
また、検索に型制約が考慮されるようになります。

### `ignore-param-style`オプション
関数、メソッドの引数の形式には、カリー化形式（`arg1 -> arg2 -> returnType`）とタプル形式（`arg1 * arg2 -> returnType`）の2種類があります。
`ignore-param-style`オプションが有効の場合は、カリー化形式とタプル形式を無視してマッチします。

### `ignore-case`オプション
`ignore-case`オプションが有効の場合は、API名、型名とのマッチング時に大文字と小文字を区別しません。

### `swap-order`オプション
`swap-order`オプションが有効の場合は、引数とタプルの順番を入れ替えて検索します。入れ替え回数が2回を超えた場合はマッチしません。
例えば、`a -> b -> c`というクエリで`b -> a -> c`にマッチします。

### `complement`オプション
`complement`オプションが有効の場合は、不足している引数とタプルの要素を補完して検索します。補完した数が2個を超えた場合はマッチしません。
例えば、`a * c`というクエリで`a * b * c`にマッチします。

### `xmldoc`オプション
`xmldoc`オプションが有効の場合は、検索結果にXMLドキュメントを表示します。

## FSharp.Compiler.Service の制限により対応できないAPI
* C# で定義されたクラス、構造体のフィールド

## 動作環境
* Windows
* .Net Framework 4.5
* F# 4.0

## 使用ライブラリ
* [FSharp.Compiler.Service](http://fsharp.github.io/FSharp.Compiler.Service/)
* [FParsec](http://www.quanttec.com/fparsec/)
* [FsPickler](http://mbraceproject.github.io/FsPickler/)
* [FSharp.Collections.ParallelSeq](http://fsprojects.github.io/FSharp.Collections.ParallelSeq/)
