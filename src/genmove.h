/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(GENMOVE_H_INCLUDED)
#define GENMOVE_H_INCLUDED


// Memo: L436
enum {
	stage_search, s_hash_move, s_good_cap, s_special, s_quiet, s_bad_cap, s_none,
	stage_evasion, e_hash_move, e_ev, e_none, 
	stage_razoring, r_hash_move, r_cap, r_checks, r_none
};
#define StageNone ((1 << s_none) | (1 << e_none) | (1 << r_none))


// Memo: L488
extern int16_t History[16 * 64]; 
#define HistoryScore(piece,from,to) History[((piece) << 6) | (to)]
#define HistoryP(piece,from,to) ((Convert(HistoryScore(piece,from,to) & 0xFF00,int)/Convert(HistoryScore(piece,from,to) & 0x00FF,int)) << 16)
#define History(from,to) HistoryP(Square(from),from,to)
#define HistoryM(move) HistoryScore(Square(From(move)),From(move),To(move))
#define HistoryInc(depth) Min(((depth) >> 1) * ((depth) >> 1), 64)
#define HistoryGood(move) if ((HistoryM(move) & 0x00FF) >= 256 - HistoryInc(depth)) \
	HistoryM(move) = ((HistoryM(move) & 0xFEFE) >> 1) + ((HistoryInc(depth) << 8) | HistoryInc(depth)); \
	else HistoryM(move) += ((HistoryInc(depth) << 8) | HistoryInc(depth))
#define HistoryBad(move) if ((HistoryM(move) & 0x00FF) >= 256 - HistoryInc(depth)) \
	HistoryM(move) = ((HistoryM(move) & 0xFEFE) >> 1) + HistoryInc(depth); else HistoryM(move) += HistoryInc(depth)


// Memo: L505
struct GRef {
	uint16_t ref[2];
	uint16_t check_ref[2];
};
#ifndef TUNER
extern GRef Ref[16 * 64];
#else 
extern GRef RefOne[16 * 64];
extern GRef RefTwo[16 * 64];
extern GRef * Ref = RefOne;
#endif
#define RefPointer(piece,from,to) Ref[((piece) << 6) | (to)]
#define RefM(move) RefPointer(Square(To(move)),From(move),To(move))
#define UpdateRef(ref_move) if (T(Current->move) && RefM(Current->move).ref[0] != (ref_move)) { \
	RefM(Current->move).ref[1] = RefM(Current->move).ref[0]; RefM(Current->move).ref[0] = (ref_move); }
#define UpdateCheckRef(ref_move) if (T(Current->move) && RefM(Current->move).check_ref[0] != (ref_move)) { \
	RefM(Current->move).check_ref[1] = RefM(Current->move).check_ref[0]; RefM(Current->move).check_ref[0] = (ref_move); }



// Memo: L819
int pick_move();
template <bool me, bool root> int get_move();
template <bool me> int see(int move, int margin);
template <bool me> void gen_root_moves();
template <bool me> int * gen_captures(int * list);
template <bool me> int * gen_evasions(int * list);
void mark_evasions(int * list);
template <bool me> int * gen_quiet_moves(int * list);
template <bool me> int * gen_checks(int * list);
template <bool me> int * gen_delta_moves(int * list);


#endif
