scala-regex-sample
========
# 説明
正規表現によるマッチを行うプログラムのサンプルです。

Scala 2.9.1、sbt 0.11.3で動作します。
テストはSpecs 2 1.11を使っています。

# やってること
1. 正規表現を受け取り、パーサコンビネータで抽象構文木の生成を行う
2. 抽象構文木から非決定性有限オートマトンを生成する
3. 文字列とオートマトンからマッチできるかを探索する

# 取り扱う正規表現

    regex ::= A
    A ::= B | B or A
    B ::= C | C B
    C ::= D* | D
    D ::= (A) | [a-zA-Z]

# 備考
- 探索時にメモ化などしてないので、効率は非常に悪いです
- ライセンスは修正BSDってことで
