/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if defined(__GNUC__)
#include <x86intrin.h>
#else
#include <intrin.h>
#endif

#include "TT.h"
#include "evaluate.h"
#include "genmove.h"
#include "misc.h"
#include "position.h"
#include "types.h"

// ToDo: root_pos をアクセスするのはコンパイルを通すためだけの不正な処理なので、ちゃんと考える。
// ToDo: BB(i) の定義をちゃんとする.
#undef BB
#define BB(i) pos.bb(i)
// ToDo: Square(sq) の定義をちゃんとする.
#undef Square
#define Square(sq) pos.square(sq)

// Memo: L333
#define AddMove(from, to, flags, score)                                                                                \
	{                                                                                                                  \
		*list = ((from) << 6) | (to) | (flags) | (score);                                                              \
		list++;                                                                                                        \
	}
#define AddCapture(from, to, flags) AddMove(from, to, flags, MvvLva[Square(from)][Square(to)])
#define AddCaptureP(piece, from, to, flags) AddMove(from, to, flags, MvvLva[piece][Square(to)])
#define AddHistoryP(piece, from, to, flags) AddMove(from, to, flags, HistoryP(piece, from, to))
#define AddHistory(from, to) AddMove(from, to, 0, History(from, to))
///#define AddDeltaP(piece,from,to,flags) AddMove(from,to,flags,Convert(DeltaScore(piece,from,to)+(int16_t)0x4000,int)
///<< 16) #define AddDelta(from,to) AddMove(from,to,0,Convert(Delta(from,to)+(int16_t)0x4000,int) << 16)
#define AddCDeltaP(piece, from, to, flags)                                                                             \
	{                                                                                                                  \
		if (DeltaScore(piece, from, to) >= margin)                                                                     \
			AddMove(from, to, flags, int(DeltaScore(piece, from, to) + (int16_t)0x4000) << 16)                         \
	}
#define AddCDelta(from, to)                                                                                            \
	{                                                                                                                  \
		if (Delta(from, to) >= margin)                                                                                 \
			AddMove(from, to, 0, int(Delta(from, to) + (int16_t)0x4000) << 16)                                         \
	}

#define MvvLvaPromotion (MvvLva[WhiteQueen][BlackQueen])
#define MvvLvaPromotionKnight (MvvLva[WhiteKnight][BlackKnight])
#define MvvLvaPromotionCap(capture)                                                                                    \
	(MvvLva[((capture) < WhiteRook) ? WhiteRook : ((capture) >= WhiteQueen ? WhiteKing : WhiteKnight)][BlackQueen])
#define MvvLvaPromotionKnightCap(capture) (MvvLva[WhiteKing][capture])
#define MvvLvaXray (MvvLva[WhiteQueen][WhitePawn])
#define MvvLvaXrayCap(capture) (MvvLva[WhiteKing][capture])
#define RefOneScore ((0xFF << 16) | (3 << 24))
#define RefTwoScore ((0xFF << 16) | (2 << 24))
#define KillerOneScore ((0xFF << 16) | (1 << 24))
#define KillerTwoScore (0xFF << 16)

#define FlagSort (1 << 0)
#define FlagNoBcSort (1 << 1)

int16_t History[16 * 64];

namespace
{
void sort(int *start, int *finish)
{
	for (int *p = start; p < finish - 1; p++) {
		int *best = p;
		int value = *p;
		int previous = *p;
		for (int *q = p + 1; q < finish; q++)
			if ((*q) > value) {
				value = *q;
				best = q;
			}
		*best = previous;
		*p = value;
	}
}
}

MoveList::MoveList() : stage(s_none), gen_flags(0), start(moves), cur(moves) {}

MoveList::~MoveList() {}

int MoveList::pick_move()
{
	int move, *p, *best;
	move = *cur;
	if (!(move))
		return 0;
	best = cur;
	for (p = cur + 1; *p != 0; p++) {
		if ((*p) > move) {
			best = p;
			move = *p;
		}
	}
	*best = *cur;
	*cur = move;
	cur++;
	return move & 0xFFFF;
}

template <bool me> void MoveList::gen_next_moves(Position &pos)
{
	constexpr bool opp = !me;
	int *p, *q, *r;
	gen_flags &= ~FlagSort;
	switch (stage) {
	case s_hash_move:
	case r_hash_move:
	case e_hash_move:
		moves[0] = killers[0];
		moves[1] = 0;
		return;
	case s_good_cap:
		pos.set_mask(Piece(opp));
		r = gen_captures<me>(pos);
		for (q = r - 1, p = moves; q >= p;) {
			int move = (*q) & 0xFFFF;
			if (!pos.see<me>(move, 0)) {
				int next = *p;
				*p = *q;
				*q = next;
				p++;
			} else
				q--;
		}
		start = p;
		cur = p;
		sort(p, r);
		return;
	case s_special:
		cur = start;
		p = start;
		if (killers[1]) {
			*p = killers[1];
			p++;
		}
		if (killers[2]) {
			*p = killers[2];
			p++;
		}
		if (ref[0] && ref[0] != killers[1] && ref[0] != killers[2]) {
			*p = ref[0];
			p++;
		}
		if (ref[1] && ref[1] != killers[1] && ref[1] != killers[2]) {
			*p = ref[1];
			p++;
		}
		*p = 0;
		return;
	case s_quiet:
		gen_quiet_moves<me>(pos, start);
		cur = start;
		gen_flags |= FlagSort;
		return;
	case s_bad_cap:
		*(start) = 0;
		cur = moves;
		if (!(gen_flags & FlagNoBcSort))
			sort(moves, start);
		return;
	case r_cap:
		r = gen_captures<me>(pos);
		cur = moves;
		sort(moves, r);
		return;
	case r_checks:
		r = gen_checks<me>(pos);
		cur = moves;
		sort(moves, r);
		return;
	case e_ev:
		pos.set_mask(Filled);
		r = gen_evasions<me>(pos);
		mark_evasions(pos);
		sort(moves, r);
		cur = moves;
		return;
	default:
		// 来ることはない.
		break;
	}
}

template <bool me, bool root> int MoveList::get_move(Position &pos)
{
	int move;

	if (root) {
		move = (*cur) & 0xFFFF;
		cur++;
		return move;
	}
start:
	if (!(*cur)) {
		stage++;
		if ((1 << stage) & StageNone)
			return 0;
		gen_next_moves<me>(pos);
		goto start;
	}
	if (gen_flags & FlagSort)
		move = pick_move();
	else {
		move = (*cur) & 0xFFFF;
		cur++;
	}
	if (stage == s_quiet) {
		if (move == killers[1] || move == killers[2] || move == ref[0] || move == ref[1])
			goto start;
	} else if (stage == s_special && (Square(To(move)) || !pos.is_legal<me>(move)))
		goto start;
	return move;
}

template <bool me> void MoveList::gen_root_moves(Position &pos)
{
	constexpr bool opp = !me;
	int i, *p, killer, depth = -256, move;
	GEntry *Entry;
	GPVEntry *PVEntry;

	killer = 0;
	if (Entry = TT.probe(pos.key())) {
		if (Entry->move16 != 0 && Entry->low_depth > depth) {
			depth = Entry->low_depth;
			killer = Entry->move16;
		}
	}
	if (PVEntry = PVHASH.probe(pos.key())) {
		if (PVEntry->depth > depth && PVEntry->move16 != 0) {
			depth = PVEntry->depth;
			killer = PVEntry->move16;
		}
	}

	killers[0] = killer;
	if (Check(me)) {
		stage = stage_evasion;
	} else {
		stage = stage_search;
		ref[0] = RefM(pos.cur_move()).ref[0];
		ref[1] = RefM(pos.cur_move()).ref[1];
	}
	gen_flags = 0;
	p = RootList;
	cur = moves;
	moves[0] = 0;
	while (move = get_move<me, 0>(pos)) {
		if (IsIllegal(me, move))
			continue;
		if (p > RootList && move == killer)
			continue;
		if (SearchMoves) {
			for (i = 0; i < SMPointer; i++)
				if (SMoves[i] == move)
					goto keep_move;
			continue;
		}
	keep_move:
		*p = move;
		p++;
	}
	*p = 0;
}

template <bool me> int *MoveList::gen_captures(Position &pos)
{
	constexpr bool opp = !me;
	bitboard_t u, v;
	int *list = moves;

	for (u = Pawn(me) & Line(me, 6); u != Empty; Cut(u))
		if (!(Square(lsb(u) + Push(me)))) {
			AddMove(lsb(u), lsb(u) + Push(me), FlagPQueen, MvvLvaPromotion)
		}
	for (v = ShiftW(opp, pos.mask()) & Pawn(me) & Line(me, 6); v != Empty; Cut(v)) {
		AddMove(lsb(v), lsb(v) + PushE(me), FlagPQueen, MvvLvaPromotionCap(Square(lsb(v) + PushE(me))))
	}
	for (v = ShiftE(opp, pos.mask()) & Pawn(me) & Line(me, 6); v != Empty; Cut(v)) {
		AddMove(lsb(v), lsb(v) + PushW(me), FlagPQueen, MvvLvaPromotionCap(Square(lsb(v) + PushW(me))))
	}
	if (testz_bb(pos.att(me), pos.mask()))
		goto finish;
	for (v = ShiftW(opp, pos.mask()) & Pawn(me) & (~Line(me, 6)); v != Empty; Cut(v)) {
		AddCaptureP(IPawn(me), lsb(v), lsb(v) + PushE(me), 0)
	}
	for (v = ShiftE(opp, pos.mask()) & Pawn(me) & (~Line(me, 6)); v != Empty; Cut(v)) {
		AddCaptureP(IPawn(me), lsb(v), lsb(v) + PushW(me), 0)
	}
	for (v = SArea[lsb(King(me))] & pos.mask() & (~pos.att(opp)); v != Empty; Cut(v)) {
		AddCaptureP(IKing(me), lsb(King(me)), lsb(v), 0)
	}
	for (u = Knight(me); u != Empty; Cut(u)) {
	}
	for (v = NAtt[lsb(u)] & pos.mask(); v != Empty; Cut(v)) {
		AddCaptureP(IKnight(me), lsb(u), lsb(v), 0)
	}
	for (u = Bishop(me); u != Empty; Cut(u)) {
		for (v = BishopAttacks(lsb(u), PieceAll) & pos.mask(); v != Empty; Cut(v)) {
			AddCapture(lsb(u), lsb(v), 0)
		}
	}
	for (u = Rook(me); u != Empty; Cut(u)) {
		for (v = RookAttacks(lsb(u), PieceAll) & pos.mask(); v != Empty; Cut(v)) {
			AddCaptureP(IRook(me), lsb(u), lsb(v), 0)
		}
	}
	for (u = Queen(me); u != Empty; Cut(u)) {
		for (v = QueenAttacks(lsb(u), PieceAll) & pos.mask(); v != Empty; Cut(v)) {
			AddCaptureP(IQueen(me), lsb(u), lsb(v), 0)
		}
	}
finish:
	*list = 0;
	return list;
}

template <bool me> int *MoveList::gen_evasions(Position &pos)
{
	constexpr bool opp = !me;
	bitboard_t king;
	bitboard_t att, esc, b, u;
	uint64_t att_sq, from;
	int *list = moves;

	king = lsb(King(me));
	att = (NAtt[king] & Knight(opp)) | (PAtt[me][king] & Pawn(opp));
	for (u = (BMask[king] & BSlider(opp)) | (RMask[king] & RSlider(opp)); u != Empty; u ^= b) {
		b = Bit(lsb(u));
		if (testz_bb(Between[king][lsb(u)], PieceAll))
			att |= b;
	}
	att_sq = lsb(att);
	esc = SArea[king] & (~(Piece(me) | pos.att(opp))) & pos.mask();
	if (Square(att_sq) >= WhiteLight)
		esc &= ~FullLine[king][att_sq];
	Cut(att);
	if (att != Empty) {
		att_sq = lsb(att);
		if (Square(att_sq) >= WhiteLight)
			esc &= ~FullLine[king][att_sq];
		for (; esc != Empty; Cut(esc))
			AddCaptureP(IKing(me), king, lsb(esc), 0) *list = 0;
		return list;
	}
	for (u = PAtt[opp][att_sq] & Pawn(me); u != Empty; Cut(u)) {
		from = lsb(u);
		if (Bit(att_sq) & Line(me, 7))
			AddMove(from, att_sq, FlagPQueen, MvvLvaPromotionCap(Square(att_sq))) else if (Bit(att_sq) & pos.mask())
			    AddCaptureP(IPawn(me), from, att_sq, 0)
	}
	for (; esc != 0; Cut(esc))
		AddCaptureP(IKing(me), king, lsb(esc), 0) att = Between[king][att_sq];
	for (u = Shift(opp, att) & Pawn(me); u != Empty; Cut(u)) {
		from = lsb(u);
		if (Bit(from) & Line(me, 6))
			AddMove(from, from + Push(me), FlagPQueen, MvvLvaPromotion) else if (!(~pos.mask()))
			    AddMove(from, from + Push(me), 0, 0)
	}
	if (!(~pos.mask())) {
		for (u = Shift(opp, Shift(opp, att)) & Line(me, 1) & Pawn(me); u != Empty; Cut(u))
			if (!(Square(lsb(u) + Push(me))))
				AddMove(lsb(u), lsb(u) + 2 * Push(me), 0, 0)
	}
	att |= Bit(att_sq);
	for (u = Knight(me); u != Empty; Cut(u))
		for (esc = NAtt[lsb(u)] & att; esc != 0; esc ^= b) {
			b = Bit(lsb(esc));
			if (b & pos.mask())
				AddCaptureP(IKnight(me), lsb(u), lsb(esc), 0)
		}
	for (u = Bishop(me); u != Empty; Cut(u))
		for (esc = BishopAttacks(lsb(u), PieceAll) & att; esc != 0; esc ^= b) {
			b = Bit(lsb(esc));
			if (b & pos.mask())
				AddCapture(lsb(u), lsb(esc), 0)
		}
	for (u = Rook(me); u != Empty; Cut(u))
		for (esc = RookAttacks(lsb(u), PieceAll) & att; esc != 0; esc ^= b) {
			b = Bit(lsb(esc));
			if (b & pos.mask())
				AddCaptureP(IRook(me), lsb(u), lsb(esc), 0)
		}
	for (u = Queen(me); u != Empty; Cut(u))
		for (esc = QueenAttacks(lsb(u), PieceAll) & att; esc != 0; esc ^= b) {
			b = Bit(lsb(esc));
			if (b & pos.mask())
				AddCaptureP(IQueen(me), lsb(u), lsb(esc), 0)
		}
	*list = 0;
	return list;
}

void MoveList::mark_evasions(Position &pos)
{
	int *list = moves;
	for (; *list != 0; list++) {
		int move = (*list) & 0xFFFF;
		if (!(Square(To(move))) && !(move & 0xE000)) {
			if (move == ref[0])
				*list |= RefOneScore;
			else if (move == ref[1])
				*list |= RefTwoScore;
			else if (move == killers[1])
				*list |= KillerOneScore;
			else if (move == killers[2])
				*list |= KillerTwoScore;
			else
				*list |= HistoryP(Square(From(move)), From(move), To(move));
		}
	}
}

template <bool me> int *MoveList::gen_quiet_moves(Position &pos, int *list)
{
	constexpr bool opp = !me;
	int to;
	bitboard_t u, v, free, occ;

	occ = PieceAll;
	free = ~occ;
	for (v = Shift(me, Pawn(me)) & free & (~Line(me, 7)); v != Empty; Cut(v)) {
		to = lsb(v);
		if ((Bit(to) & Line(me, 2)) != 0 && !(Square(to + Push(me))))
			AddHistoryP(IPawn(me), to - Push(me), to + Push(me), 0) AddHistoryP(IPawn(me), to - Push(me), to, 0)
	}
	for (u = Knight(me); u != Empty; Cut(u)) {
		for (v = free & NAtt[lsb(u)]; v != 0; Cut(v))
			AddHistoryP(IKnight(me), lsb(u), lsb(v), 0)
	}
	for (u = Bishop(me); u != Empty; Cut(u)) {
		for (v = free & BishopAttacks(lsb(u), occ); v != 0; Cut(v))
			AddHistory(lsb(u), lsb(v))
	}
	for (u = Rook(me); u != Empty; Cut(u)) {
		for (v = free & RookAttacks(lsb(u), occ); v != 0; Cut(v))
			AddHistoryP(IRook(me), lsb(u), lsb(v), 0)
	}
	for (u = Queen(me); u != Empty; Cut(u)) {
		for (v = free & QueenAttacks(lsb(u), occ); v != 0; Cut(v))
			AddHistoryP(IQueen(me), lsb(u), lsb(v), 0)
	}
	for (v = SArea[lsb(King(me))] & free & (~pos.att(opp)); v != Empty; Cut(v)) {
		AddHistoryP(IKing(me), lsb(King(me)), lsb(v), 0)
	}
	*list = 0;
	return list;
}

template <bool me> int *MoveList::gen_checks(Position &pos)
{
	constexpr bool opp = !me;
	int king, from;
	uint64_t u, v, target, b_target, r_target, clear, xray;
	int *list = moves;

	clear = ~(Piece(me) | pos.mask());
	king = lsb(King(opp));
	for (u = pos.xray(me) & Piece(me); u != 0; Cut(u)) {
		from = lsb(u);
		target = clear & (~FullLine[king][from]);
		if (Square(from) == IPawn(me)) {
			if (!(Bit(from + Push(me)) & Line(me, 7))) {
				if ((Bit(from + Push(me)) & target) != 0 && !(Square(from + Push(me))))
					AddMove(from, from + Push(me), 0, MvvLvaXray) for (v = PAtt[me][from] & target & Piece(opp); v != 0;
					                                                   Cut(v))
					    AddMove(from, lsb(v), 0, MvvLvaXrayCap(Square(lsb(v))))
			}
		} else {
			if (Square(from) < WhiteLight)
				v = NAtt[from] & target;
			else if (Square(from) < WhiteRook)
				v = BishopAttacks(from, PieceAll) & target;
			else if (Square(from) < WhiteQueen)
				v = RookAttacks(from, PieceAll) & target;
			else if (Square(from) < WhiteKing)
				v = QueenAttacks(from, PieceAll) & target;
			else
				v = SArea[from] & target & (~pos.att(opp));
			for (; v != 0; Cut(v))
				AddMove(from, lsb(v), 0, MvvLvaXrayCap(Square(lsb(v))))
		}
	}
	xray = ~(pos.xray(me) & BB(me));
	for (u = Knight(me) & NArea[king] & xray; u != 0; Cut(u))
		for (v = NAtt[king] & NAtt[lsb(u)] & clear; v != 0; Cut(v))
			AddCaptureP(IKnight(me), lsb(u), lsb(v), 0) for (u = DArea[king] & Pawn(me) & (~Line(me, 6)) & xray; u != 0;
			                                                 Cut(u))
			{
				from = lsb(u);
				for (v = PAtt[me][from] & PAtt[opp][king] & clear & Piece(opp); v != 0; Cut(v))
					AddCaptureP(IPawn(me), from, lsb(v),
					            0) if (!(Square(from + Push(me))) && (Bit(from + Push(me)) & PAtt[opp][king]) != 0)
					    AddMove(from, from + Push(me), 0, 0)
			}
	b_target = BishopAttacks(king, PieceAll) & clear;
	r_target = RookAttacks(king, PieceAll) & clear;
	for (u = (Odd(king ^ rank_of(king)) ? BB(WhiteLight | me) : BB(WhiteDark | me)) & xray; u != 0; Cut(u))
		for (v = BishopAttacks(lsb(u), PieceAll) & b_target; v != 0; Cut(v))
			AddCapture(lsb(u), lsb(v), 0) for (u = Rook(me) & xray; u != 0;
			                                   Cut(u)) for (v = RookAttacks(lsb(u), PieceAll) & r_target; v != 0;
			                                                Cut(v))
			    AddCaptureP(IRook(me), lsb(u), lsb(v),
			                0) for (u = Queen(me) & xray; u != 0;
			                        Cut(u)) for (v = QueenAttacks(lsb(u), PieceAll) & (b_target | r_target); v != 0;
			                                     Cut(v)) AddCaptureP(IQueen(me), lsb(u), lsb(v), 0) *list = 0;
	return list;
}

template <bool me> int *MoveList::gen_delta_moves(Position &pos)
{
	int to;
	uint64_t u, v, free, occ;
	const int margin = pos.margin();
	constexpr bool opp = !me;
	int *list = moves;

	occ = PieceAll;
	free = ~occ;
	for (v = Shift(me, Pawn(me)) & free & (~Line(me, 7)); v != 0; Cut(v)) {
		to = lsb(v);
		if ((Bit(to) & Line(me, 2)) != 0 && !(Square(to + Push(me))))
			AddCDeltaP(IPawn(me), to - Push(me), to + Push(me), 0) AddCDeltaP(IPawn(me), to - Push(me), to, 0)
	}
	for (u = Knight(me); u != 0; Cut(u))
		for (v = free & NAtt[lsb(u)]; v != 0; Cut(v))
			AddCDeltaP(IKnight(me), lsb(u), lsb(v), 0) for (u = Bishop(me); u != 0;
			                                                Cut(u)) for (v = free & BishopAttacks(lsb(u), occ); v != 0;
			                                                             Cut(v))
			    AddCDelta(lsb(u), lsb(v)) for (u = Rook(me); u != 0; Cut(u)) for (v = free & RookAttacks(lsb(u), occ);
			                                                                      v != 0; Cut(v))
			        AddCDeltaP(IRook(me), lsb(u), lsb(v), 0) for (u = Queen(me); u != 0;
			                                                      Cut(u)) for (v = free & QueenAttacks(lsb(u), occ);
			                                                                   v != 0; Cut(v))
			            AddCDeltaP(IQueen(me), lsb(u), lsb(v),
			                       0) for (v = SArea[lsb(King(me))] & free & (~pos.att(opp)); v != 0; Cut(v))
			                AddCDeltaP(IKing(me), lsb(King(me)), lsb(v), 0) *list = 0;
	return list;
}

// インスタンス化.
#if 0
template int get_move<false, false>();
template int get_move<false, true>();
template int get_move<true , false>();
template int get_move<true , true>();
template int see<false>(int move, int margin);
template int see<true>(int move, int margin);
template void gen_root_moves<false>();
template void gen_root_moves<true>();
template int * gen_evasions<false>(int * list);
template int * gen_evasions<true>(int * list);
template int * gen_delta_moves<false>(int * list);
template int * gen_delta_moves<true>(int * list);
#endif

template int MoveList::get_move<0, 0>(Position &pos);
template int MoveList::get_move<0, 1>(Position &pos);
template int MoveList::get_move<1, 0>(Position &pos);
template int MoveList::get_move<1, 1>(Position &pos);

template void MoveList::gen_root_moves<false>(Position &pos);
template void MoveList::gen_root_moves<true>(Position &pos);

template int *MoveList::gen_delta_moves<0>(Position &pos);
template int *MoveList::gen_delta_moves<1>(Position &pos);
