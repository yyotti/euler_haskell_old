# Project Euler を Haskell で解く
Haskell の勉強ついでに Project Euler を Haskell で解いていく。

## 回答
src/main/Problem/ に各問題がモジュールとして入っている。

src/main/Common/ には共通で使う関数などが入っている。

src/main/Main.hs で回答を計算・表示できる。

```sh
$ cd /path/to/this/project
$ runhaskell -isrc/main src/main/Main.hs XXX
```

XXX は問題の番号。番号を指定した場合は所要時間(概算)も表示される。

問題番号を指定せず

```sh
$ cd /path/to/this/project
$ runhaskell -isrc/main src/main/Main.hs
```

これだけで実行すると、登録されている問題全ての回答を計算・表示する。

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
テストのときにつけるとメインの方の警告も出るので一石二鳥。

```sh
$ runhaskell -Wall -isrc/main src/test/Problem/PxxxSpec.hs
```

ただしビルドするわけではないので、その問題に関係するファイルに対する警告しか出ない。
