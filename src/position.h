/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(POSITION_H_INCLUDED)
#define POSITION_H_INCLUDED

#include <string>

// Memo: L427
struct GPosData {
	uint64_t key, pawn_key;
	uint16_t move;
	uint8_t turn, castle_flags, ply, ep_square, piece, capture;
	uint8_t square[64];
	int pst, material;
};

struct GBoard {
	bitboard_t bb[16];
	uint8_t square[64];
};

struct GData {
	uint64_t key, pawn_key, eval_key, att[2], patt[2], passer;
	bitboard_t xray[2], pin[2], threat, mask;
	uint8_t turn, castle_flags, ply, ep_square, capture;
///	uint8_t gen_flags;
	uint8_t piece;
///	uint8_t stage;
	uint8_t mul, dummy;
	int16_t score;
	uint16_t move, killer[3], ref[2];
	int best;
	int material, pst;
	int margin;

	// ToDo: 手の制御として、killer[]と合わせてここから外に出したい.
///	int *start;
///	int *current;
///	int moves[230];
};

struct GEvalInfo;
struct GPawnEntry;
struct GPawnEvalInfo;

class Position {
	Position(const Position&) = delete;
	Position& operator=(const Position&) = delete;
public:
	Position();
	~Position();
	bool set_sfen(const std::string&);
	std::string sfen();

	void init_search();
	template <bool me> void do_move(int move);
	template <bool me> void undo_move(int move);
	void do_null();
	void undo_null();
	template <bool me> int is_legal(int move);
	template <bool me> int is_check(int move);

	void rewind() { Current = Data; }
	void reset_current() { Data[0] = *Current; Current = Data; }
	void clear_forward() { memset(Data + 1, 0, 127 * sizeof(GData)); }

	bitboard_t bb(int i) const { return Board->bb[i]; }
	uint8_t square(int sq) const { return Board->square[sq]; }

	uint64_t key() const { return Current->key; }
	int cur_move() const { return Current->move; }
	int margin() const { return Current->margin; }
	bitboard_t mask() const { return Current->mask; }
	void set_mask(bitboard_t bb) { Current->mask = bb; }
	bitboard_t xray(int me) { return Current->xray[me]; }
	uint64_t att(int turn) const { return Current->att[turn]; }
	uint8_t ep_square() const { return Current->ep_square; }
///	uint8_t& gen_flags() {return Current->gen_flags; }
	uint8_t cur_turn() const { return Current->turn; }
	uint8_t castle_flags() const { return Current->castle_flags; }
	uint16_t ply() const { return Current->ply; }
	int16_t score() const { return Current->score; }
	void set_score(int16_t sc) const { Current->score = sc; }
	uint16_t& killer(int i) { return Current->killer[i]; }
	uint16_t ref(int i) const { return Current->ref[i]; }
	int& best() { return Current->best; }
	int sel_depth() const { int d; for (d=1; d<127 && T(Data[d].att[0]); d++); return d-1; }
	int height() const { return (int)(Current - Data); }
	bool is_repeat() const;


	template <bool me> int see(int move, int margin);

	// evaluate.cpp
	template <bool me, bool HPopCnt> void eval_pawns(GPawnEntry * PawnEntry, GPawnEvalInfo &PEI);
	template <bool HPopCnt> void eval_pawn_structure(GPawnEntry * PawnEntry);
	template <bool me, bool HPopCnt> void eval_queens(GEvalInfo &EI);
	template <bool me, bool HPopCnt> void eval_rooks(GEvalInfo &EI);
	template <bool me, bool HPopCnt> void eval_bishops(GEvalInfo &EI);
	template <bool me, bool HPopCnt> void eval_knights(GEvalInfo &EI);
	template <bool me, bool HPopCnt> void eval_king(GEvalInfo &EI);
	template <bool me, bool HPopCnt> void eval_passer(GEvalInfo &EI);
	template <bool me, bool HPopCnt> void eval_pieces(GEvalInfo &EI);
	template <bool me, bool HPopCnt> void eval_endgame(GEvalInfo &EI);
	template <bool HPopCnt> void eval_unusual_material(const int turn, GEvalInfo &EI);
	template <bool HPopCnt> void evaluation();

private:
public:		// ToDo: privateでビルドできるようにする.
	GBoard Board[1];
	GData Data[128];
	GData *Current;
	uint64_t Stack[2048];
	int sp, save_sp;
};

inline bool Position::is_repeat() const
{
	int16_t d;
	for (d = sp - 4; d >= 0; d -= 2) {
		if (Stack[d] == key()) return true;
	}
	return false;
}


#define Check(me) T(pos.att((me) ^ 1) & King(me))
#define IsIllegal(me,move) ((T(pos.xray(opp) & Bit(From(move))) && F(Bit(To(move)) & FullLine[lsb(King(me))][From(move)])) \
	|| (IsEP(move) && T(Line[Rank(From(move))] & King(me)) && T(Line[Rank(From(move))] & Major(opp)) && \
	T(RookAttacks(lsb(King(me)),PieceAll ^ Bit(From(move)) ^ Bit(pos.ep_square() - Push(me))) & Major(opp))))
#define IsRepetition(margin,move) ((margin) > 0 && Current->ply >= 2 && (Current-1)->move == ((To(move) << 6) | From(move)) && F(Square(To(move))) && F((move) & 0xF000))

#if 0
template <bool me> void do_move(int move);
template <bool me> void undo_move(int move);
void do_null();
void undo_null();
template <bool me> int is_legal(int move);
template <bool me> int is_check(int move);
#endif

extern Position root_pos;
extern GBoard Board[1];
extern uint64_t Stack[2048];	// ToDo: DEL
extern int sp, save_sp;			// ToDo: DEL
extern GData Data[128];	// [ToDo] 数値の意味を確認する.
extern GData *Current;

#endif
