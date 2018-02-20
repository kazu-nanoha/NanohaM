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

オリジナルのGull3のプログラムがファイル1つで、いろいろと扱いにくいため、
・ファイル分割
・リファクタリング
・並列探索をマルチスレッドに変更
・将棋化
をする予定。

進め方
(1) ファイル分割              <-- Now
(2) リファクタリング
(3) シングルスレッドで将棋化
(4) マルチスレッド化

ちゃんと動くところまでできるか、わかりません。
進め方(3)の段階でとても弱かったら、それ以上開発を進めるかもわかりません :-(
