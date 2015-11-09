# Project Euler を Haskell で解く
Haskell の勉強ついでに Project Euler を Haskell で解いていく。

## 回答
src/main/Problem/ に入っている。

src/main/Common/ には共通で使う関数などが入っている。

```sh
$ cd /path/to/this/project
$ runhaskell -isrc/main src/main/Problem/Pxxx.hs
```
で実行できる。xxx は問題の番号。

## テスト
src/test/Problem/ に入っている。

1つの問題に対するテストを実行したいなら
```sh
$ cd /path/to/this/project
$ runhaskell -isrc/main src/test/Problem/PxxxSpec.hs
```
で実行できる。xxx は問題の番号。

全部のテストを一括実行する場合は
```sh
$ cd /path/to/this/project
$ runhaskell -isrc/main -isrc/test src/test/Spec.hs
```
で実行できる。

## コンパイル警告
コンパイル時の警告をチェックしたいなら、上記のコマンドに `-Wall` オプションをつければよい。

```sh
# 例) 普通に回答を実行する場合
$ runhaskell -Wall -isrc/main src/main/Problem/Pxxx.hs
```

テストのときにつけるとメインの方の警告も出るので一石二鳥。

```sh
# 例) テストでコンパイル警告も出す
$ runhaskell -Wall -isrc/main src/test/Problem/PxxxSpec.hs
```
