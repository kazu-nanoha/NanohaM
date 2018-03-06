NanohaM, Nanoha under MIT License version
=========================================

# Overview

NanohaM is a free USI shogi(Japanese-chess) engine derived from Gull 3.  
The original Gull chess source code is "public domain".  
This software is released under the MIT License.

# なのは(MIT ver)

「なのは」は、USIに対応した将棋プログラムです。  
この版は MIT ライセンスにするため、Public domain の Gullというチェスプログラムをベースに変更しています。

なるべくビルドは通るようにしていますが、まだ動きません！！ 
特に今は無理やりビルドを通しているので、プログラムが破綻しています…。 
オリジナルのプログラムはそのままでビルドが通らないし、動くようになるのかな…？


オリジナルのGull3のプログラムがファイル1つで、いろいろと扱いにくいため、  
・ファイル分割  
・リファクタリング  
・並列探索をマルチスレッドに変更  
・将棋化  
をする予定。

進め方

  1. ファイル分割              <-- ほぼ対応
  2. リファクタリング          <-- Now
  3. シングルスレッドで将棋化
  4. マルチスレッド化

ちゃんと動くところまでできるか、わかりません。  
進め方3の段階でとても弱かったら、それ以上開発を進めるかもわかりません :-( 
リファクタリングというより、設計し直しに近いような…。

オリジナルはよくこれで開発できると感心します。このソースだとちょっとした変更も手を出しにくい…。
というか、オリジナルソースはどうやってビルドするのでしょうか？？
手元の環境だとそのままではコンパイルすら通らない…。

とりあえず、最初は現在公開している「なのは」同様に配列ベースのデータ構造で作る予定。 
気が向いたら bitboard 版も作るかも？ 
いずれにしてもすぐに動くようにはできないと思うので、WCSC28 はこちらでは出ない予定(間に合わない)。
WCSC28向けに並行して Stockfish-9 ベースにする作業も進めないと…。

WCSC でも上位に入るためには本番だけでなく、事前準備にも大規模な演算能力が必要になってきたので辟易します。

# 使用条件および免責等
MITライセンスに従います。
