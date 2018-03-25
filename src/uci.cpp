/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#include <Windows.h>	// ToDo: 不要にする

#include <string>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "types.h"
#include "misc.h"
#include "TT.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "uci.h"

#define PRG_NAME  "Nanoha"
#define AUTHOR    "Kazuyuki Kawabata"

#define MovesTg 30		// ToDo: この数値の意味は？
#define TimeRatio 120	// ToDo: この数値の意味は？
#define PonderRatio 120	// ToDo: この数値の意味は？

///namespace {
///	Position root_pos;
///}

// オプションの種類.
// check      false/true
// spin       数値
// combo      ポップアップメニュー
// button     ボタン
// string     文字列
// filename   ファイル名

Options::Options()
: ponder("Ponder", false), 
  hash("Hash", 64, 64, 8192),
  m_threads("Threads", 2, 1, 32),
  multiPV("MultiPV", 1, 1, 16),
  aspiration("Aspiration", true),
  useBook("UseBook", true),
  bookFile("BookFile", "Book_40.jsk")
{
	// ToDo: コア数によってスレッド数の上限を変更する.
}

namespace {
	std::ostream& operator<<(std::ostream& os, const Option_Check& op)
	{
		std::cout << "option name " << op.name << " type check default "
		          << (op.def_value ? "true" : "false") << std::endl;
		return os;
	}
	std::ostream& operator<<(std::ostream& os, const Option_Spin& op)
	{
		std::cout << "option name " << op.name << " type spin min "
		          << op._min << " max " << op._max << " default " << op.def_value << std::endl;
		return os;
	}
	std::ostream& operator<<(std::ostream& os, const Option_String& op)
	{
		std::cout << "option name " << op.name << "type string default " << op.def_value << std::endl;
		return os;
	}
}

std::ostream& operator<<(std::ostream& os, const Options& op)
{
	os << op.ponder;
	os << op.hash;
	os << op.m_threads;
	os << op.multiPV;
	os << op.aspiration;
	os << op.useBook;
	os << op.bookFile;
	return os;
}

/**

  USI(Universal Shogi Interface)について

  通信GUI→ENGを「>」、ENG→GUIを「<」で表現する。
  1. 起動時
    usiに対して、usiokを2秒以内に返す。時間がかかる処理をusiokまでに入れない。
    >usi
    <id name <program name>
    <id author <program author>
    <option name XXXX
    <option name XXXX
    <usiok

  2.準備確認
    isready～readyokで時間のかかる初期化をする(定跡や評価関数の読み込み等)
    >setoption name XXX value XXX
    >setoption name XXX value XXX
    >isready
    <readyok

  3.対局開始
    >usinewgame

  4.局面の指定
    >position [sfen <sfenstring> | startpos] moves <move1> <movei>

  5.探索
    探索開始前に局面を指定する。
    時間の単位はms。
    切れ負けはwtime/btimeのみ指定。フィッシャールールはbinc/wincがつく。
    秒読みはbyoyomiがつく。
    エンジンは探索中に適宜infoで状況を返す。
    最善手をbestmoveで返す。

  5-1 先読みなし
    goコマンドでENGは探索を開始し、bestmoveで最善手を返す。
    >go btime <x> wtime <y>
    >go btime <x> wtime <y> binc <a> winc <b>
    >go btime <x> wtime <y> byoyomi <z>
    <info XXX
    <bestmove [<move1> [ponder <move2>] | resign | win]

  5-2 先読みあり
    go ponderコマンドでENGは探索を開始する。
    先読みが当たった場合は、ponderhitが来る。その場合は探索を継続しbestmoveで最善手を返す。
    >go ponder ...
    >ponderhit
    <bestmove ...

    先読みが外れた場合は、stopが来る。その時点の読みをbestmoveで返し、改めて送られてくるgoコマンドで探索を行う。
    >go ponder XXX
    >stop
    <bestmove ...
    >position ...
    >go XXX
    <bestmove ...

  6.終局
    終局すると結果がGUIからエンジンに通知される
    >gameover [win | lose | draw]

  7.エンジンの終了
    GUIからquitが送られてきたら、エンジンのプロセスを終了する
    >quit

  8.詰探索
    go mateで探索を開始する。
    詰む場合は手順を返す。未実装時はnotimplemented、時間切れで読み切れないときはtimeout、詰まないときはnomateを返す。
    >go mate [<x> | infinite]
    <checkmate {<move1>...<movei> | notimplemented | timeout | nomate }

  9.info コマンドのオプション
    depth <x>
    seldepth <x>   depthとセットで返す.
    time <x>    ms単位で探索時間を返す。pvとセット
    nodes <x>   思考開試からの探索ノード数
    pv <move1> ... <movei>
    multipv <x>  評価値が最も高い手に1を指定. 2, 3, ...と順に低くなる。
    score [cp <x> {lowerbound | upperbound] |mate <y>| mate+ | mate-}
                 エンジンからの評価値(エンジンが有利だと正、不利だと負)
    currmove <move>
    hashfull <x>   ハッシュの使用率をパーミルで返す
    nps <x>
    string <str>
    string XXX

*/

namespace {
	void unknown(std::istringstream& iss, const char* opt, const char* var = NULL)
	{
		std::string name;
		cout_lock();
		std::cout << "info string Unknown " << opt <<":";
		if (var) std::cout << var << " ";
		do {
			std::cout << name << " ";
		} while (iss >> name);
		std::cout << std::endl;
		cout_unlock();
	}
	void setoption(std::istringstream& iss)
	{
		// setoption name ...に対して、issはname ...になって渡される
		std::string name;
		std::string value;
		int v;
		if (iss >> name) {
			if (name == "Threads") {
				iss >> name;	// "value"を読み飛ばす.
				iss >> value;
				v = std::stoi(value);
				options.set_threads(v);
			} else {
				unknown(iss, "option");
				return;
			}
		}
#if 0
		ptr = strtok(mstring," ");
		for (ptr = strtok(NULL," "); ptr != NULL; ptr = strtok(NULL," ")) {
			if (!memcmp(ptr,"Hash",4) && !Searching) {
				ptr += 11;
				value = atoi(ptr);
				if (value < 1) value = 1;
#ifdef W32_BUILD
				if (value > 1024) value = 1024;
#else
				if (value > 65536) value = 65536;
#endif
				value = (Bit(msb(value)) * Convert(1024 * 1024, int64_t)) / Convert(sizeof(GEntry), int64_t);
				TT.resize(value);
			} else if (!memcmp(ptr, "Threads", 7) && !Searching) {
				ptr += 14;
				value = atoi(ptr);
				if (value != PrN) {
					NewPrN = Max(1, Min(MaxPrN, value));
					ResetHash = 0;
					longjmp(ResetJump, 1);
				}
			} else if (!memcmp(ptr, "MultiPV", 7)) {
				ptr += 14;
			    PVN = atoi(ptr);
				Stop = 1;
			} else if (!memcmp(ptr,"Ponder",6)) {
				ptr += 13;
				if (ptr[0] == 't') Ponder = 1;
				else Ponder = 0;
///			} else if (!memcmp(ptr,"Clear",5)) {
///				init_search(1);
///				break;
			} else if (!memcmp(ptr,"PV",2)) {
				ptr += 14;
				if (ptr[0] == 't') PVHashing = 1;
				else PVHashing = 0;
			}
        }
#endif
	}

	void position(Position& pos, std::istringstream& iss)
	{
		std::string token, sfen;
		Move m;

		// コマンドは次の形式(issからpositionは除外されている)
		// (1) position startpos [moves <move1> <movei>]
		// (2) position sfen <sfen string> [moves <move1> <movei>]
		// ToDo:
///		if (F(Searching)) get_position(mstring);
		iss >> token;
		if (token == "startpos") {
			sfen = StartSFEN;
			iss >> token;
		} else if (token == "sfen") {
			while (iss >> token && token != "moves") {
				sfen += token + " ";
			}
		} else {
			unknown(iss, "position");
			return;
		}
		pos.set_sfen(sfen);
		while (iss >> token) {
			m = move_from_string(token.c_str());
			bool ret = pos.cur_turn() ? pos.is_legal<1>(m) : pos.is_legal<0>(m);
			if (ret) {
				pos.cur_turn() ? pos.do_move<1>(m) : pos.do_move<0>(m);
			} else {
				unknown(iss, "moves", token.c_str());
				return;
			}
		}
		// ToDo: 
	}

	void go(Position& pos, std::istringstream& iss)
	{
		// コマンドは次の形式(issからgoは除外されている)
		// (1) go <時間指定>
		// (2) go ponder <時間指定>
		// (2) go mate <時間指定>
		// ToDo: 探索開始
		// 以下、 get_time_limit() の処理を記載.

		std::string token;
		int i, time, inc, wtime, btime, winc, binc, moves, pondering, movetime = 0, exp_moves = MovesTg - 1;

		Infinite = 1;
		MoveTime = 0;
		SearchMoves = 0;
		SMPointer = 0;
		pondering = 0;
		TimeLimit1 = 0;
		TimeLimit2 = 0;
		wtime = btime = 0;
		winc = binc = 0;
		moves = 0;
		Stop = 0;
		DepthLimit = 128;
	    while (iss >> token) {
			if (token == "binc") {
				iss >> token;
				binc = stoi(token);
				Infinite = 0;
			} else if (token == "btime") { 
				iss >> token; 
				btime = stoi(token);
				Infinite = 0;
			} else if (token == "depth") { 
				iss >> token; 
				DepthLimit = 2 * stoi(token) + 2; 
				Infinite = 1;
			} else if (token == "infinite") { 
				Infinite = 1; 
			} else if (token == "movestogo") { 
				iss >> token; 
				moves = stoi(token);
				Infinite = 0;
			} else if (token == "winc") { 
				iss >> token; 
				winc = stoi(token);
				Infinite = 0;
			} else if (token == "wtime") { 
				iss >> token; 
				wtime = stoi(token); 
				Infinite = 0;
			} else if (token == "movetime") { 
				iss >> token;
				movetime = stoi(token);
				MoveTime = 1;
				Infinite = 0;
			} else if (token == "searchmoves") {
				if (F(SearchMoves)) {
					for (i = 0; i < 256; i++) SMoves[i] = 0;
				}
			    SearchMoves = 1;
				while (iss >> token) {
					SMoves[SMPointer] = move_from_string(token.c_str());
					SMPointer++;
				}
			} else if (token == "ponder") pondering = 1;
	    }

		if (pondering) Infinite = 1;
		if (pos.cur_turn() == White) {
			time = wtime;
			inc = winc;
		} else {
			time = btime;
			inc = binc;
		}
		if (moves) moves = Max(moves - 1, 1);
		int time_max = Max(time - Min(1000, time/2), 0);
		int nmoves;
		if (moves) nmoves = moves;
		else {
			nmoves = MovesTg - 1;
			if (pos.ply() > 40) nmoves += Min(pos.ply() - 40, (100 - pos.ply())/2);
			exp_moves = nmoves;
		}
		TimeLimit1 = Min(time_max, (time_max + (Min(exp_moves, nmoves) * inc))/Min(exp_moves, nmoves));
		TimeLimit2 = Min(time_max, (time_max + (Min(exp_moves, nmoves) * inc))/Min(3,Min(exp_moves, nmoves)));
		TimeLimit1 = Min(time_max, (TimeLimit1 * TimeRatio)/100);
		if (Ponder) TimeLimit1 = (TimeLimit1 * PonderRatio)/100;
		if (MoveTime) {
			TimeLimit2 = movetime;
			TimeLimit1 = TimeLimit2 * 100;
		}
	    InfoTime = StartTime = get_time();
		Searching = 1;
///		if (MaxPrN > 1) SET_BIT_64(Smpi->searching, 0);
		if (F(Infinite)) PVN = 1;
		if (pos.cur_turn() == White) root<0>(); else root<1>();
	}
	void gameover(std::istringstream& iss)
	{
		// 特に何もしない
		//   何かするなら、終局までの手、読み筋、評価値等を保存しておいて、結果を踏まえて調整する.
		std::string param;
		iss >> param;
		sync_cout << "info string " << param << sync_endl;
	}

}

Options options;

void USI::init()
{
}

void USI::loop(int argc, char** argv)
{
	std::string line;
	std::string cmd, param;
///	Position root_pos;

	while (--argc) {
		line += *++argv;
		line += " ";
	}
	bool once = (line.length() > 0);

	do {
		if (std::cin.eof()) break;
		if (once == false) getline(std::cin, line);
		if (line.length() == 0) continue;
	
		std::istringstream iss(line);
		iss >> cmd;
		if (cmd == "usi") {
			sync_cout << "id name " PRG_NAME "\n"
			          << "id author " AUTHOR << sync_endl;
			// ToDo: option を返す
			sync_cout;
			std::cout << options;
			std::cout << "usiok" << sync_endl;
#if 0
			sync_cout << "option name Ponder type check default false\n"
			          << "option name Hash type spin min 64 max 8192 default 64\n"
			          << "option name Threads type spin min 1 max 32 default 1\n"
			          << "option name MultiPV type spin min 1 max 16 default 1\n"
			          << "option name PV_Hash type check default true\n"
			          << "option name Aspiration window type check default true\n"
			          << "option name UseBook type check default true\n"
			          << "option name BookFile type string default book_40.jsk\n"
			          << "usiok" << sync_endl;
#endif
///			if (F(Searching)) init_search(root_pos, 1);
		} else if (cmd == "isready") {
			// 時間がかかる初期化はここで行う.
			// ToDo: 評価ベクトルの読み込み.
			// ToDo: 定跡データの読み込み.
			sync_cout << "readyok" << sync_endl;
		} else if (cmd == "usinewgame") {
			// ToDo: 新しい対局
			Stop = 0;
			init_search(root_pos, 1);
		} else if (cmd == "setoption") {
			// option設定.
			setoption(iss);
		} else if (cmd == "position") {
			// ToDo: 局面設定
			position(root_pos, iss);
		} else if (cmd == "go") {
			// ToDo: 探索開始
			go(root_pos, iss);
		} else if (cmd == "stop") {
			// ToDo: 停止処理して、その時点の最善手を送る.
			Stop = 1;
			if (F(Searching)) send_best_move(root_pos);
		} else if (cmd == "ponderhit") {
			// 先読み当たりで探索を継続する.
			// ToDo: 探索終了していたらその時点の最善手を送る.
			Infinite = 0;
			if (!RootList[1]) Stop = 1;
			if (F(CurrentSI->Bad) && F(CurrentSI->FailLow) && time_to_stop(BaseSI, LastTime, 0)) Stop = 1;
			if (F(Searching)) send_best_move(root_pos);
		} else if (cmd == "gameover") {
			gameover(iss);
		} else if (cmd == "quit") {
			break;
		} else {
			// 未定義または非対応のコマンド.
			sync_cout << "info string Unknown: " << line << sync_endl;
		}
	} while (once == false);

	// 終了処理をする.
}


