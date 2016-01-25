# FSharpApiSearch

[![Join the chat at https://gitter.im/hafuu/FSharpApiSearch](https://badges.gitter.im/hafuu/FSharpApiSearch.svg)](https://gitter.im/hafuu/FSharpApiSearch?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
F# API search engine 名前はテキトーなので何か思いついたら変えたい。

## 使い方
コマンドラインの引数にクエリを渡して検索します。
```
> FSharpApiSearch.Console.exe "int -> int"
```

引数を省略した場合はプログラムを終了するまで何度でも検索できます。`#q`を入力すると終了します。

## Query Specification
基本的にはF# のシグネチャと同じ形式で検索できます。

### Type Name
一致する型名を検索します。
#### 例
```fsharp
string -> int
int -> int
```
### Type Variable
型変数により任意の型を検索します。
型変数は強力で、`'a`と検索した場合はジェネリック型(例: `list<string>`)、関数(例: `(int -> int)`)等のすべての型にもマッチします。

クエリ中で違う型変数を使用した場合、検索対象には別の型を期待します。
`'a -> 'b`という検索をした場合は、`int -> string`にはマッチしますが、`int -> int`にはマッチしません。
#### 例
```fsharp
'a -> int
'a -> 'b -> 'b
```
### Generic
.Net形式、ML形式のクエリでジェネリック型を検索します。ジェネリック型には型名、型変数のみ使用できます。型パラメータの記述には制限はありません。

高階型での検索もできます。
#### 例
```fsharp
list<'a> -> 'a
'a list -> 'a list
'a<string, 'b> -> string -> 'b
```
### Function
引数の数が同じで、引数、戻り値の型がマッチする関数を検索します。ネストした関数は括弧で表現します。
#### 例
```fsharp
int -> int -> int
('a -> 'b) -> 'a 'c -> 'b 'c
```
### Tuple
タプル型を検索します。ネストしたタプルは括弧で表現します。
#### 例
```fsharp
'a * 'b -> 'a
('a * 'b) * 'c
```
