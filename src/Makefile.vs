# NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
# The original Gull chess source code is "public domain".
#
# Copyright (c) 2018 Kazuyuki Kawabata
#
# This software is released under the MIT License, see "LICENSE.txt".

EXE = NanohaM.exe
PGD = NanohaM.pgd
PGOLOG = NanohaM_prof.txt
#EVAL_VER=EVAL_NANO
#EVAL_OBJ=evaluate.obj
EVAL_VER=EVAL_MINI
EVAL_OBJ=evaluate.obj
#EVAL_VER=EVAL_APERY
#EVAL_OBJ=evaluate_apery.obj
#EVAL_VER=EVAL_TWIG
#EVAL_OBJ=evaluate_twig.obj

USE_DFPN=no

OBJS=Gull.obj TT.obj evaluate.obj misc.obj uci.obj position.obj genmove.obj \
	search.obj

CC=cl
LD=link

# Compile Options
#
# Visual C++オプション
#
# /D_CRT_SECURE_NO_WARNINGS
#                   secureな関数を使っていないときの警告を出さない
# /Zc:forScope      スコープ ループに標準 C++ を適用する
# /Wall             警告をすべて有効にする
# /GS[-]            セキュリティ チェックを有効にする
# /favor:<blend|AMD64|EM64T> 最適化するプロセッサ
# /GL[-]            リンク時のコード生成を行う
# /RTCs             スタック フレーム ランタイム チェック
# /RTCu             初期化されていないローカル変数のチェック
# -DNDEBUG 
FLAGS = /favor:AMD64 /EHsc /D_CRT_SECURE_NO_WARNINGS /GL /Zc:forScope \
	 -DNDEBUG -D$(EVAL_VER) -DNANOHA

!IF "$(USE_DFPN)" == "yes"
FLAGS = $(FLAGS) -DUSE_DFPN
OBJS = $(OBJS) SearchMateDFPN.obj
!ENDIF

#CXXFLAGS=$(FLAGS) /MT /W4 /Wall /nologo /Od /GS /RTCsu
CXXFLAGS=$(FLAGS) /MD /W3 /nologo /Ox /Ob2 /GS- /Gm /Zi
#CXXFLAGS=$(FLAGS) /MD /W3 /nologo /Od /GS /Gs /Zi /RTCsu
LDFLAGS=/NOLOGO /STACK:16777216,32768 /out:$(EXE) /LTCG /DEBUG
PGOLDFLAGS1=/NOLOGO /STACK:33554432,32768 /out:$(EXE) /LTCG:PGI
PGOLDFLAGS2=/NOLOGO /STACK:33554432,32768 /out:$(EXE) /LTCG:PGO


all: $(EXE)

$(EXE) : $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) User32.lib

.cpp.obj :
	$(CC) $(CXXFLAGS) /c $*.cpp

clean :
	del /q *.obj
	del /q *.idb
	del /q *.pdb
	del /q *.pgc
	del /q *.pgd
	del /q *.suo
	del    $(PGOLOG)
	del    $(EXE)

pgo: $(OBJS)
	$(LD) $(PGOLDFLAGS1) $(OBJS) User32.lib
	$(EXE) bench 128 1 8
	pgomgr /merge $(PGD)
	pgomgr /summary $(PGD) > $(PGOLOG)
	$(LD) $(PGOLDFLAGS2) $(OBJS) User32.lib

pgo-mate1: $(OBJS)
	$(LD) $(PGOLDFLAGS1) $(OBJS) User32.lib
	$(EXE) mate1
	pgomgr /merge $(PGD)
	pgomgr /summary $(PGD) > $(PGOLOG)
	$(LD) $(PGOLDFLAGS2) $(OBJS) User32.lib

pgo-genmove: $(OBJS)
	$(LD) $(PGOLDFLAGS1) $(OBJS) User32.lib
	$(EXE) genmove
	pgomgr /merge $(PGD)
	pgomgr /summary $(PGD) > $(PGOLOG)
	$(LD) $(PGOLDFLAGS2) $(OBJS) User32.lib

pgo-eval: $(OBJS)
	$(LD) $(PGOLDFLAGS1) $(OBJS) User32.lib
	$(EXE) eval
	pgomgr /merge $(PGD)
	pgomgr /summary $(PGD) > $(PGOLOG)
	$(LD) $(PGOLDFLAGS2) $(OBJS) User32.lib

pgo-moves: $(OBJS)
	$(LD) $(PGOLDFLAGS1) $(OBJS) User32.lib
	$(EXE) moves
	pgomgr /merge $(PGD)
	pgomgr /summary $(PGD) > $(PGOLOG)
	$(LD) $(PGOLDFLAGS2) $(OBJS) User32.lib

prof-clean:
	del /q *.pgc
	del /q *.pgd
	del    $(PGOLOG)
	del    $(EXE)

pack:
	mkdir src
	copy *.cpp src
	copy *.h src
	copy Makefile src
	copy Makefile.vs src
	copy *.txt src
