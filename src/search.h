/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(SEARCH_H_INCLUDED)
#define SEARCH_H_INCLUDED

// Memo: L403
#define halt_check if ((Current - Data) >= 126) {evaluate(pos); return Current->score;} \
    if (Current->ply >= 100) return 0; \
	for (i = 4; i <= Current->ply; i+= 2) if (Stack[sp-i] == Current->key) return 0
#define ExtFlag(ext) ((ext) << 16)
#define Ext(flags) (((flags) >> 16) & 0xF)
#define FlagHashCheck (1 << 20) // first 20 bits are reserved for the hash killer and extension
#define FlagHaltCheck (1 << 21)
#define FlagCallEvaluation (1 << 22)
#define FlagDisableNull (1 << 23)
#define FlagNeatSearch (FlagHashCheck | FlagHaltCheck | FlagCallEvaluation)
#define FlagNoKillerUpdate (1 << 24)
#define FlagReturnBestMove (1 << 25)

// Memo: L529
extern uint16_t PV[128];

// Memo: L535
extern int pvp;
extern int pv_length;
extern int best_move, best_score;
// int TimeLimit1, TimeLimit2, Console, HardwarePopCnt;
extern int TimeLimit1, TimeLimit2;
// int DepthLimit, LastDepth, LastTime, LastValue, LastExactValue, PrevMove, InstCnt;
extern int DepthLimit, LastDepth, LastValue, LastExactValue, PrevMove, InstCnt;
extern int64_t LastSpeed;

// Memo: L804
void init_search(int clear_hash);


class Position;
// Memo: L829
template <bool me, bool pv> int q_search(Position& pos, int alpha, int beta, int depth, int flags);
template <bool me, bool pv> int q_evasion(Position& pos, int alpha, int beta, int depth, int flags);
template <bool me, bool exclusion> int search(Position& pos, int beta, int depth, int flags);
template <bool me, bool exclusion> int search_evasion(Position& pos, int beta, int depth, int flags);
template <bool me, bool root> int pv_search(Position& pos, int alpha, int beta, int depth, int flags);
template <bool me> void root();

void send_pv(Position& pos, int depth, int alpha, int beta, int score);
void send_multipv(Position& pos, int depth, int curr_number);

#endif
