/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(GENMOVE_H_INCLUDED)
#define GENMOVE_H_INCLUDED

// Memo: L488
extern int16_t History[16 * 64]; 
#define HistoryScore(piece,from,to) History[((piece) << 6) | (to)]
#define HistoryP(piece,from,to) ((Convert(HistoryScore(piece,from,to) & 0xFF00,int)/Convert(HistoryScore(piece,from,to) & 0x00FF,int)) << 16)
#define History(from,to) HistoryP(Square(from),from,to)


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
