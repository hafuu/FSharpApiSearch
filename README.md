# FSharpApiSearch
F# API search engine

ベータ版なので仕様が不安定です。

## 使い方
FSharpApiSearchはコンソールアプリケーションです。クエリを与えずに実行するとインタラクティブモードで起動します。
`#q`を入力してインタラクティブモードを終了します。

    FSharpApiSearch.Console.exe

引数にクエリを渡すと一度だけ検索を行います。起動が遅いのでおすすめしません。

    FSharpApiSearch.Console.exe "int -> int"

デフォルトで検索できるアセンブリは`FSharp.Core`、`mscorlib`、`System`、`System.Core`です。
`--target`オプションを指定すると検索対象を追加できます。

    FSharpApiSearch.Console.exe --target:TargetAssembly

検索対象のアセンブリに依存するアセンブリをすべて指定して下さい。
依存するが検索対象にしたくないアセンブリは、`--reference`オプションを使って追加します。

    FSharpApiSearch.Console.exe --target:TargetAssembly --reference:DependenceAssembly

## クエリ仕様
基本的にはF# のシグネチャと同じです。FSharpApiSearchの拡張のみ説明します。

### 名前検索
`name : signature`と書きます。シグネチャを指定しない場合は、シグネチャ部分に`_`を指定します。

### ワイルドカード
通常の検索では型変数と型名はマッチしませんが、どちらでも良い場合があります。
`?`を使うと何にでもマッチするようになります。

また、`?`の後に名前（`?a`）を付けると、同じ名前のワイルドカードが同じ型にマッチするように指示します。
例えば`?a -> ?a`というクエリは`int -> int`にマッチしますが、`int -> string`にはマッチしません。

### メンバ検索
#### インスタンスメンバ
`receiver => signature`と書きます。

メソッドを検索する場合は`receiver => arg -> returnType`と書きます。
多引数メソッドの場合は`signature`部分を多引数関数と同じ形式（`receiver => arg1 -> arg2 -> returnType`）で記述します。
もし引数部分をタプル形式（`receiver => arg1 * arg2 -> returnType`）で書いた場合は、引数がタプルのメソッドを検索します。

プロパティを検索する場合は`receiver => propertyType`と書きます。
インデックス付きプロパティは`receiver => index -> propertyType`と書きます。

インスタンスメンバの検索は次の特別扱いがあります。
1. `arg -> receiver -> returnType` という形式の関数にマッチします。
2. 引数なしの検索（`receiver => propertyType`）は、`receiver => unit -> propertyType`という形式のインスタンスメンバともマッチします。

#### 静的メンバ
モジュール内の値と同じクエリで検索できます。インスタンスメソッドと同じ様に多引数とタプル引数の区別に注意して下さい。

### 型変数、型名の固定
類似検索を有効にしていると型変数と型名がマッチしてしまいます。
型変数や型名の前に`!`を付けると（`!int`、`!'a`等）その部分を固定して検索できます。

## 検索可能なAPI
| API    | 例 |
|--------|----|
| モジュールの関数と値 | `int -> string` |
| レコード、構造体のフィールド | `Ref<'a> => 'a` |
| 型（クラス、レコード等）のメソッド、プロパティ | `'a list => int` |
| クラスのコンストラクタ | `string -> Uri` |
| 型略称 | `string` means `System.String` |
| 名前（関数名、メソッド名等）| `head : 'a list -> 'a` |

## 対応予定のAPI
* 型名
* 判別共用体
* アクティブパターン
* 関数の型略称
* 拡張メソッド
* 測定単位
* 型制約
* 可視性

## FSharp.Compiler.Service の制限により対応できないAPI
* C# で定義されたクラス、構造体のフィールド

## 動作環境
* Windows
* .Net Framework 4.5
* F# 4.0

## 使用ライブラリ
* FSharp.Compiler.Service
* FParsec