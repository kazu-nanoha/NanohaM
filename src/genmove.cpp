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

#include "types.h"
#include "misc.h"
#include "TT.h"
#include "position.h"
#include "evaluate.h"
#include "genmove.h"

// Memo: L333
#define AddMove(from,to,flags,score) { *list = ((from) << 6) | (to) | (flags) | (score); list++; }
#define AddCapture(from,to,flags) AddMove(from,to,flags,MvvLva[Square(from)][Square(to)])
#define AddCaptureP(piece,from,to,flags) AddMove(from,to,flags,MvvLva[piece][Square(to)])
#define AddHistoryP(piece,from,to,flags) AddMove(from,to,flags,HistoryP(piece,from,to))
#define AddHistory(from,to) AddMove(from,to,0,History(from,to))
#define AddDeltaP(piece,from,to,flags) AddMove(from,to,flags,Convert(DeltaScore(piece,from,to)+(int16_t)0x4000,int) << 16)
#define AddDelta(from,to) AddMove(from,to,0,Convert(Delta(from,to)+(int16_t)0x4000,int) << 16)
#define AddCDeltaP(piece,from,to,flags) {if (DeltaScore(piece,from,to) >= Current->margin) AddMove(from,to,flags,Convert(DeltaScore(piece,from,to)+(int16_t)0x4000,int) << 16)}
#define AddCDelta(from,to) {if (Delta(from,to) >= Current->margin) AddMove(from,to,0,Convert(Delta(from,to)+(int16_t)0x4000,int) << 16)}


#define MvvLvaPromotion (MvvLva[WhiteQueen][BlackQueen])
#define MvvLvaPromotionKnight (MvvLva[WhiteKnight][BlackKnight])
#define MvvLvaPromotionCap(capture) (MvvLva[((capture) < WhiteRook) ? WhiteRook : ((capture) >= WhiteQueen ? WhiteKing : WhiteKnight)][BlackQueen])
#define MvvLvaPromotionKnightCap(capture) (MvvLva[WhiteKing][capture])
#define MvvLvaXray (MvvLva[WhiteQueen][WhitePawn])
#define MvvLvaXrayCap(capture) (MvvLva[WhiteKing][capture])
#define RefOneScore ((0xFF << 16) | (3 << 24))
#define RefTwoScore ((0xFF << 16) | (2 << 24))
#define KillerOneScore ((0xFF << 16) | (1 << 24))
#define KillerTwoScore (0xFF << 16)



#define FlagSort (1 << 0)
#define FlagNoBcSort (1 << 1)
// Memo: L436
enum {
	stage_search, s_hash_move, s_good_cap, s_special, s_quiet, s_bad_cap, s_none,
	stage_evasion, e_hash_move, e_ev, e_none, 
	stage_razoring, r_hash_move, r_cap, r_checks, r_none
};
#define StageNone ((1 << s_none) | (1 << e_none) | (1 << r_none))


// Memo: L3016
void sort(int * start, int * finish) {
	for (int * p = start; p < finish - 1; p++) {
		int * best = p;
		int value = *p;
		int previous = *p;
		for (int * q = p + 1; q < finish; q++) if ((*q) > value) {
			value = *q;
			best = q;
		}
		*best = previous;
		*p = value;
	}
}

int pick_move() {
	register int move, *p, *best;
	move = *(Current->current);
	if (F(move)) return 0;
	best = Current->current;
	for (p = Current->current + 1; T(*p); p++) {
		if ((*p) > move) {
			best = p;
			move = *p;
		}
	}
	*best = *(Current->current);
	*(Current->current) = move;
	Current->current++;
	return move & 0xFFFF;
}



// Memo: L3055
template <bool me> void gen_next_moves() {
	int *p, *q, *r;
	Current->gen_flags &= ~FlagSort;
	switch (Current->stage) {
	case s_hash_move: case r_hash_move: case e_hash_move:
		Current->moves[0] = Current->killer[0];
		Current->moves[1] = 0;
		return;
	case s_good_cap: 
		Current->mask = Piece(opp);
		r = gen_captures<me>(Current->moves);
		for (q = r - 1, p = Current->moves; q >= p;) {
		    int move = (*q) & 0xFFFF;
		    if (!see<me>(move,0)) {
			    int next = *p;
			    *p = *q;
			    *q = next;
			    p++;
		    } else q--;
	    }
		Current->start = p;
		Current->current = p;
		sort(p, r);
		return;
	case s_special:
		Current->current = Current->start;
		p = Current->start;
		if (Current->killer[1]) {*p = Current->killer[1]; p++;}
		if (Current->killer[2]) {*p = Current->killer[2]; p++;}
		if (Current->ref[0] && Current->ref[0] != Current->killer[1] && Current->ref[0] != Current->killer[2]) {*p = Current->ref[0]; p++;}
		if (Current->ref[1] && Current->ref[1] != Current->killer[1] && Current->ref[1] != Current->killer[2]) {*p = Current->ref[1]; p++;}
		*p = 0;
		return;
	case s_quiet: 
		gen_quiet_moves<me>(Current->start);
		Current->current = Current->start;
		Current->gen_flags |= FlagSort;
		return;
	case s_bad_cap:
		*(Current->start) = 0;
		Current->current = Current->moves;
		if (!(Current->gen_flags & FlagNoBcSort)) sort(Current->moves, Current->start);
		return;
	case r_cap:
		r = gen_captures<me>(Current->moves);
		Current->current = Current->moves;
		sort(Current->moves, r);
		return;
	case r_checks:
		r = gen_checks<me>(Current->moves);
		Current->current = Current->moves; 
		sort(Current->moves, r);
		return;
	case e_ev:
		Current->mask = Filled;
		r = gen_evasions<me>(Current->moves);
		mark_evasions(Current->moves);
		sort(Current->moves, r);
		Current->current = Current->moves;
		return;
	}
}


// Memo: L3118
template <bool me, bool root> int get_move() {
	int move;
	
	if (root) {
		move = (*Current->current) & 0xFFFF;
		Current->current++;
		return move;
	}
start:
	if (F(*Current->current)) {
		Current->stage++;
		if ((1 << Current->stage) & StageNone) return 0;
		gen_next_moves<me>();
		goto start;
	}
	if (Current->gen_flags & FlagSort) move = pick_move();
	else {
		move = (*Current->current) & 0xFFFF;
		Current->current++;
	}
	if (Current->stage == s_quiet) { 
		if (move == Current->killer[1] || move == Current->killer[2] || move == Current->ref[0] || move == Current->ref[1]) goto start;
	} else if (Current->stage == s_special && (Square(To(move)) || !is_legal<me>(move))) goto start;
	return move;
}

// Memo: L3144
template <bool me> int see(int move, int margin) {
	int from, to, piece, capture, delta, sq, pos;
	uint64_t clear, def, att, occ, b_area, r_slider_att, b_slider_att, r_slider_def, b_slider_def, r_area, u, new_att, my_bishop, opp_bishop;
	from = From(move);
	to = To(move);
	piece = SeeValue[Square(from)];
	capture = SeeValue[Square(to)];
	delta = piece - capture;
	if (delta <= -margin) return 1;
	if (piece == SeeValue[WhiteKing]) return 1;
	if (Current->xray[me] & Bit(from)) return 1;
	if (T(Current->pin[me] & Bit(from)) && piece <= SeeValue[WhiteDark]) return 1;
	if (piece > (SeeValue[WhiteKing] >> 1)) return 1;
	if (IsEP(move)) return 1;
	if (F(Current->att[opp] & Bit(to))) return 1;
	att = PAtt[me][to] & Pawn(opp);
	if (T(att) && delta + margin > SeeValue[WhitePawn]) return 0;
	clear = ~Bit(from);
	def = PAtt[opp][to] & Pawn(me) & clear;
	if (T(def) && delta + SeeValue[WhitePawn] + margin <= 0) return 1;
	att |= NAtt[to] & Knight(opp);
	if (T(att) && delta > SeeValue[WhiteDark] - margin) return 0;
	occ = PieceAll & clear;
    b_area = BishopAttacks(to,occ);
	opp_bishop = Bishop(opp);
	if (delta > SeeValue[IDark(me)] - margin) if (b_area & opp_bishop) return 0;
	my_bishop = Bishop(me);
    b_slider_att = BMask[to] & (opp_bishop | Queen(opp));
	r_slider_att = RMask[to] & Major(opp);
	b_slider_def = BMask[to] & (my_bishop | Queen(me)) & clear;
	r_slider_def = RMask[to] & Major(me) & clear;
	att |= (b_slider_att & b_area);
	def |= NAtt[to] & Knight(me) & clear;
	r_area = RookAttacks(to,occ);
	att |= (r_slider_att & r_area);
	def |= (b_slider_def & b_area);
	def |= (r_slider_def & r_area);
	att |= SArea[to] & King(opp);
	def |= SArea[to] & King(me) & clear;
	while (true) {
		if (u = (att & Pawn(opp))) {
			capture -= piece;
			piece = SeeValue[WhitePawn];
			sq = lsb(u);
			occ ^= Bit(sq);
			att ^= Bit(sq);
			for (new_att = FullLine[to][sq] & b_slider_att & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(att,pos);
					break;
				}
			}
		} else if (u = (att & Knight(opp))) {
			capture -= piece;
			piece = SeeValue[WhiteKnight];
			att ^= (~(u-1)) & u;
		} else if (u = (att & opp_bishop)) {
            capture -= piece;
			piece = SeeValue[WhiteDark];
			sq = lsb(u);
			occ ^= Bit(sq);
			att ^= Bit(sq);
			for (new_att = FullLine[to][sq] & b_slider_att & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(att,pos);
					break;
				}
			}
		} else if (u = (att & Rook(opp))) {
            capture -= piece;
			piece = SeeValue[WhiteRook];
			sq = lsb(u);
			occ ^= Bit(sq);
			att ^= Bit(sq);
			for (new_att = FullLine[to][sq] & r_slider_att & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(att,pos);
					break;
				}
			}
		} else if (u = (att & Queen(opp))) {
            capture -= piece;
			piece = SeeValue[WhiteQueen];
			sq = lsb(u);
			occ ^= Bit(sq);
			att ^= Bit(sq);
			for (new_att = FullLine[to][sq] & (r_slider_att | b_slider_att) & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(att,pos);
					break;
				}
			}
		} else if (u = (att & King(opp))) {
            capture -= piece;
			piece = SeeValue[WhiteKing];
		} else return 1;
		if (capture < -(SeeValue[WhiteKing] >> 1)) return 0;
		if (piece + capture < margin) return 0;
		if (u = (def & Pawn(me))) {
            capture += piece;
			piece = SeeValue[WhitePawn];
            sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & b_slider_def & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(def,pos);
					break;
				}
			}
		} else if (u = (def & Knight(me))) {
            capture += piece;
			piece = SeeValue[WhiteKnight];
			def ^= (~(u-1)) & u;
		} else if (u = (def & my_bishop)) {
            capture += piece;
			piece = SeeValue[WhiteDark];
            sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & b_slider_def & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(def,pos);
					break;
				}
			}
		} else if (u = (def & Rook(me))) {
            capture += piece;
			piece = SeeValue[WhiteRook];
            sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & r_slider_def & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(def,pos);
					break;
				}
			}
		} else if (u = (def & Queen(me))) {
            capture += piece;
			piece = SeeValue[WhiteQueen];
			sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & (r_slider_def | b_slider_def) & occ & (~att); T(new_att); Cut(new_att)) {
                pos = lsb(new_att);
				if (F(Between[to][pos] & occ)) {
                    Add(def,pos);
					break;
				}
			}
		} else if (u = (def & King(me))) {
            capture += piece;
			piece = SeeValue[WhiteKing];
		} else return 0;
		if (capture > (SeeValue[WhiteKing] >> 1)) return 1;
		if (capture - piece >= margin) return 1;
	}
}

// Memo: L3311
template <bool me> void gen_root_moves() {
	int i, *p, killer, depth = -256, move;
	GEntry * Entry;
	GPVEntry * PVEntry;

	killer = 0;
	if (Entry = TT.probe(Current->key)) {
		if (T(Entry->move16) && Entry->low_depth > depth) {
			depth = Entry->low_depth;
			killer = Entry->move16;
		}
	}
	if (PVEntry = PVHASH.probe(Current->key)) {
		if (PVEntry->depth > depth && T(PVEntry->move16)) {
			depth = PVEntry->depth;
			killer = PVEntry->move16;
		}
	}

	Current->killer[0] = killer;
	if (Check(me)) Current->stage = stage_evasion;
	else {
		Current->stage = stage_search;
		Current->ref[0] = RefM(Current->move).ref[0];
	    Current->ref[1] = RefM(Current->move).ref[1];
	}
	Current->gen_flags = 0;
	p = RootList;
	Current->current = Current->moves;
	Current->moves[0] = 0;
	while (move = get_move<me,0>()) {
		if (IsIllegal(me,move)) continue;
		if (p > RootList && move == killer) continue;
		if (SearchMoves) {
			for (i = 0; i < SMPointer; i++)
				if (SMoves[i] == move) goto keep_move;
			continue;
	    }
keep_move:
		*p = move;
		p++;
	}
	*p = 0;
}

// Memo: L3356
template <bool me> int * gen_captures(int * list) {
	uint64_t u, v;

	if (Current->ep_square)
		for (v = PAtt[opp][Current->ep_square] & Pawn(me); T(v); Cut(v)) AddMove(lsb(v),Current->ep_square,FlagEP,MvvLva[IPawn(me)][IPawn(opp)])
	for (u = Pawn(me) & Line(me,6); T(u); Cut(u))
    	if (F(Square(lsb(u) + Push(me)))) {
			AddMove(lsb(u),lsb(u) + Push(me),FlagPQueen,MvvLvaPromotion)
			if (NAtt[lsb(King(opp))] & Bit(lsb(u) + Push(me))) AddMove(lsb(u),lsb(u) + Push(me),FlagPKnight,MvvLvaPromotionKnight)
		}
	for (v = ShiftW(opp,Current->mask) & Pawn(me) & Line(me,6); T(v); Cut(v)) {
		AddMove(lsb(v),lsb(v)+PushE(me),FlagPQueen,MvvLvaPromotionCap(Square(lsb(v)+PushE(me))))
		if (NAtt[lsb(King(opp))] & Bit(lsb(v) + PushE(me))) AddMove(lsb(v),lsb(v)+PushE(me),FlagPKnight,MvvLvaPromotionKnightCap(Square(lsb(v)+PushE(me))))
	}
	for (v = ShiftE(opp,Current->mask) & Pawn(me) & Line(me,6); T(v); Cut(v)) {
		AddMove(lsb(v),lsb(v)+PushW(me),FlagPQueen,MvvLvaPromotionCap(Square(lsb(v)+PushW(me))))
		if (NAtt[lsb(King(opp))] & Bit(lsb(v) + PushW(me))) AddMove(lsb(v),lsb(v)+PushW(me),FlagPKnight,MvvLvaPromotionKnightCap(Square(lsb(v)+PushE(me))))
	}
	if (F(Current->att[me] & Current->mask)) goto finish;
	for (v = ShiftW(opp,Current->mask) & Pawn(me) & (~Line(me,6)); T(v); Cut(v)) AddCaptureP(IPawn(me),lsb(v),lsb(v)+PushE(me),0)
	for (v = ShiftE(opp,Current->mask) & Pawn(me) & (~Line(me,6)); T(v); Cut(v)) AddCaptureP(IPawn(me),lsb(v),lsb(v)+PushW(me),0)
	for (v = SArea[lsb(King(me))] & Current->mask & (~Current->att[opp]); T(v); Cut(v)) AddCaptureP(IKing(me),lsb(King(me)),lsb(v),0)
	for (u = Knight(me); T(u); Cut(u))
		for (v = NAtt[lsb(u)] & Current->mask; T(v); Cut(v)) AddCaptureP(IKnight(me),lsb(u),lsb(v),0)
	for (u = Bishop(me); T(u); Cut(u))
		for (v = BishopAttacks(lsb(u),PieceAll) & Current->mask; T(v); Cut(v)) AddCapture(lsb(u),lsb(v),0)
	for (u = Rook(me); T(u); Cut(u))
		for (v = RookAttacks(lsb(u),PieceAll) & Current->mask; T(v); Cut(v)) AddCaptureP(IRook(me),lsb(u),lsb(v),0)
	for (u = Queen(me); T(u); Cut(u))
		for (v = QueenAttacks(lsb(u),PieceAll) & Current->mask; T(v); Cut(v)) AddCaptureP(IQueen(me),lsb(u),lsb(v),0)
finish:
	*list = 0;
	return list;
}

// Memo: L3391
template <bool me> int * gen_evasions(int * list) {
	int king, att_sq, from;
	uint64_t att, esc, b, u;

	king = lsb(King(me));
	att = (NAtt[king] & Knight(opp)) | (PAtt[me][king] & Pawn(opp));
	for (u = (BMask[king] & BSlider(opp)) | (RMask[king] & RSlider(opp)); T(u); u ^= b) {
		b = Bit(lsb(u));
		if (F(Between[king][lsb(u)] & PieceAll)) att |= b;
	}
	att_sq = lsb(att);
	esc = SArea[king] & (~(Piece(me) | Current->att[opp])) & Current->mask;
	if (Square(att_sq) >= WhiteLight) esc &= ~FullLine[king][att_sq];
	Cut(att);
	if (att) {
		att_sq = lsb(att);
		if (Square(att_sq) >= WhiteLight) esc &= ~FullLine[king][att_sq];
		for (; T(esc); Cut(esc)) AddCaptureP(IKing(me),king,lsb(esc),0)
		*list = 0;
		return list;
	}
	if (Bit(att_sq) & Current->mask) {
	    if (T(Current->ep_square) && Current->ep_square == att_sq + Push(me))
		    for (u = PAtt[opp][att_sq + Push(me)] & Pawn(me); T(u); Cut(u)) AddMove(lsb(u),att_sq + Push(me),FlagEP,MvvLva[IPawn(me)][IPawn(opp)])
	}
	for (u = PAtt[opp][att_sq] & Pawn(me); T(u); Cut(u)) {
        from = lsb(u);
		if (Bit(att_sq) & Line(me,7)) AddMove(from,att_sq,FlagPQueen,MvvLvaPromotionCap(Square(att_sq)))
		else if (Bit(att_sq) & Current->mask) AddCaptureP(IPawn(me),from,att_sq,0)
	}
	for ( ; T(esc); Cut(esc)) AddCaptureP(IKing(me),king,lsb(esc),0)
	att = Between[king][att_sq];
	for (u = Shift(opp,att) & Pawn(me); T(u); Cut(u)) {
        from = lsb(u);
		if (Bit(from) & Line(me,6)) AddMove(from,from + Push(me),FlagPQueen,MvvLvaPromotion)
		else if (F(~Current->mask)) AddMove(from,from + Push(me),0,0)
	}
	if (F(~Current->mask)) {
	    for (u = Shift(opp,Shift(opp,att)) & Line(me, 1) & Pawn(me); T(u); Cut(u))
            if (F(Square(lsb(u)+Push(me)))) AddMove(lsb(u),lsb(u) + 2 * Push(me),0,0)
    }
	att |= Bit(att_sq);
	for (u = Knight(me); T(u); Cut(u))
        for (esc = NAtt[lsb(u)] & att; T(esc); esc ^= b) {
			b = Bit(lsb(esc));
			if (b & Current->mask) AddCaptureP(IKnight(me),lsb(u),lsb(esc),0)
		}
	for (u = Bishop(me); T(u); Cut(u))
        for (esc = BishopAttacks(lsb(u),PieceAll) & att; T(esc); esc ^= b) {
			b = Bit(lsb(esc));
			if (b & Current->mask) AddCapture(lsb(u),lsb(esc),0)
		}
	for (u = Rook(me); T(u); Cut(u))
        for (esc = RookAttacks(lsb(u),PieceAll) & att; T(esc); esc ^= b) {
			b = Bit(lsb(esc));
			if (b & Current->mask) AddCaptureP(IRook(me),lsb(u),lsb(esc),0)
		}
	for (u = Queen(me); T(u); Cut(u))
        for (esc = QueenAttacks(lsb(u),PieceAll) & att; T(esc); esc ^= b) {
			b = Bit(lsb(esc));
			if (b & Current->mask) AddCaptureP(IQueen(me),lsb(u),lsb(esc),0)
		}
	*list = 0;
	return list;
}

// Memo: L3457
void mark_evasions(int * list) {
	for (; T(*list); list++) {
		register int move = (*list) & 0xFFFF;
	    if (F(Square(To(move))) && F(move & 0xE000)) {
			if (move == Current->ref[0]) *list |= RefOneScore;
			else if (move == Current->ref[1]) *list |= RefTwoScore;
			else if (move == Current->killer[1]) *list |= KillerOneScore;
			else if (move == Current->killer[2]) *list |= KillerTwoScore;
			else *list |= HistoryP(Square(From(move)),From(move),To(move));
		}
	}
}

// Memo: L3470
template <bool me> int * gen_quiet_moves(int * list) {
	int to;
	uint64_t u, v, free, occ;

    occ = PieceAll;
	free = ~occ;
	if (me == White) {
		if (T(Current->castle_flags & CanCastle_OO) && F(occ & 0x60) && F(Current->att[Black] & 0x70)) AddHistoryP(IKing(White),4,6,FlagCastling)
	    if (T(Current->castle_flags & CanCastle_OOO) && F(occ & 0xE) && F(Current->att[Black] & 0x1C)) AddHistoryP(IKing(White),4,2,FlagCastling)
	} else {
		if (T(Current->castle_flags & CanCastle_oo) && F(occ & 0x6000000000000000) && F(Current->att[White] & 0x7000000000000000)) AddHistoryP(IKing(Black),60,62,FlagCastling)
	    if (T(Current->castle_flags & CanCastle_ooo) && F(occ & 0x0E00000000000000) && F(Current->att[White] & 0x1C00000000000000)) AddHistoryP(IKing(Black),60,58,FlagCastling)
	}
	for (v = Shift(me,Pawn(me)) & free & (~Line(me,7)); T(v); Cut(v)) {
        to = lsb(v);
	    if (T(Bit(to) & Line(me,2)) && F(Square(to + Push(me)))) AddHistoryP(IPawn(me),to - Push(me),to + Push(me),0)
		AddHistoryP(IPawn(me),to - Push(me),to,0)
	}
	for (u = Knight(me); T(u); Cut(u))
		for (v = free & NAtt[lsb(u)]; T(v); Cut(v)) AddHistoryP(IKnight(me),lsb(u),lsb(v),0)
	for (u = Bishop(me); T(u); Cut(u))
		for (v = free & BishopAttacks(lsb(u),occ); T(v); Cut(v)) AddHistory(lsb(u),lsb(v))
	for (u = Rook(me); T(u); Cut(u))
		for (v = free & RookAttacks(lsb(u),occ); T(v); Cut(v)) AddHistoryP(IRook(me),lsb(u),lsb(v),0)
	for (u = Queen(me); T(u); Cut(u))
		for (v = free & QueenAttacks(lsb(u),occ); T(v); Cut(v)) AddHistoryP(IQueen(me),lsb(u),lsb(v),0)
	for (v = SArea[lsb(King(me))] & free & (~Current->att[opp]); T(v); Cut(v)) AddHistoryP(IKing(me),lsb(King(me)),lsb(v),0)
	*list = 0;
	return list;
}

// Memo: L3501
template <bool me> int * gen_checks(int * list) {
	int king, from;
    uint64_t u, v, target, b_target, r_target, clear, xray;

	clear = ~(Piece(me) | Current->mask);
    king = lsb(King(opp));
	for (u = Current->xray[me] & Piece(me); T(u); Cut(u)) {
		from = lsb(u);
		target = clear & (~FullLine[king][from]);
		if (Square(from) == IPawn(me)) {
			if (F(Bit(from + Push(me)) & Line(me,7))) {
			    if (T(Bit(from + Push(me)) & target) && F(Square(from + Push(me)))) AddMove(from,from + Push(me),0,MvvLvaXray)
				for (v = PAtt[me][from] & target & Piece(opp); T(v); Cut(v)) AddMove(from,lsb(v),0,MvvLvaXrayCap(Square(lsb(v))))
			}
		} else {
			if (Square(from) < WhiteLight) v = NAtt[from] & target;
			else if (Square(from) < WhiteRook) v = BishopAttacks(from,PieceAll) & target;
			else if (Square(from) < WhiteQueen) v = RookAttacks(from,PieceAll) & target;
			else if (Square(from) < WhiteKing) v = QueenAttacks(from,PieceAll) & target;
			else v = SArea[from] & target & (~Current->att[opp]);
			for ( ; T(v); Cut(v)) AddMove(from,lsb(v),0,MvvLvaXrayCap(Square(lsb(v))))
		}
	}
	xray = ~(Current->xray[me] & Board->bb[me]);
	for (u = Knight(me) & NArea[king] & xray; T(u); Cut(u))
		for (v = NAtt[king] & NAtt[lsb(u)] & clear; T(v); Cut(v)) AddCaptureP(IKnight(me),lsb(u),lsb(v),0)
    for (u = DArea[king] & Pawn(me) & (~Line(me,6)) & xray; T(u); Cut(u)) {
		from = lsb(u);
		for (v = PAtt[me][from] & PAtt[opp][king] & clear & Piece(opp); T(v); Cut(v)) AddCaptureP(IPawn(me),from,lsb(v),0)
		if (F(Square(from + Push(me))) && T(Bit(from + Push(me)) & PAtt[opp][king])) AddMove(from,from + Push(me),0,0)
	}
	b_target = BishopAttacks(king,PieceAll) & clear;
	r_target = RookAttacks(king,PieceAll) & clear;
	for (u = (Odd(king ^ Rank(king)) ? Board->bb[WhiteLight | me] : Board->bb[WhiteDark | me]) & xray; T(u); Cut(u))
		for (v = BishopAttacks(lsb(u),PieceAll) & b_target; T(v); Cut(v)) AddCapture(lsb(u),lsb(v),0)
	for (u = Rook(me) & xray; T(u); Cut(u)) 
		for (v = RookAttacks(lsb(u),PieceAll) & r_target; T(v); Cut(v)) AddCaptureP(IRook(me),lsb(u),lsb(v),0)
	for (u = Queen(me) & xray; T(u); Cut(u)) 
		for (v = QueenAttacks(lsb(u),PieceAll) & (b_target | r_target); T(v); Cut(v)) AddCaptureP(IQueen(me),lsb(u),lsb(v),0)
	*list = 0;
	return list;
}

// Memo: L3544
template <bool me> int * gen_delta_moves(int * list) {
	int to;
	uint64_t u, v, free, occ;

    occ = PieceAll;
	free = ~occ;
	if (me == White) {
		if (T(Current->castle_flags & CanCastle_OO) && F(occ & 0x60) && F(Current->att[Black] & 0x70)) AddCDeltaP(IKing(White),4,6,FlagCastling)
	    if (T(Current->castle_flags & CanCastle_OOO) && F(occ & 0xE) && F(Current->att[Black] & 0x1C)) AddCDeltaP(IKing(White),4,2,FlagCastling)
	} else {
		if (T(Current->castle_flags & CanCastle_oo) && F(occ & 0x6000000000000000) && F(Current->att[White] & 0x7000000000000000)) AddCDeltaP(IKing(Black),60,62,FlagCastling)
	    if (T(Current->castle_flags & CanCastle_ooo) && F(occ & 0x0E00000000000000) && F(Current->att[White] & 0x1C00000000000000)) AddCDeltaP(IKing(Black),60,58,FlagCastling)
	}
	for (v = Shift(me,Pawn(me)) & free & (~Line(me,7)); T(v); Cut(v)) {
        to = lsb(v);
	    if (T(Bit(to) & Line(me,2)) && F(Square(to + Push(me)))) AddCDeltaP(IPawn(me),to - Push(me),to + Push(me),0)
		AddCDeltaP(IPawn(me),to - Push(me),to,0)
	}
	for (u = Knight(me); T(u); Cut(u))
		for (v = free & NAtt[lsb(u)]; T(v); Cut(v)) AddCDeltaP(IKnight(me),lsb(u),lsb(v),0)
	for (u = Bishop(me); T(u); Cut(u))
		for (v = free & BishopAttacks(lsb(u),occ); T(v); Cut(v)) AddCDelta(lsb(u),lsb(v))
	for (u = Rook(me); T(u); Cut(u))
		for (v = free & RookAttacks(lsb(u),occ); T(v); Cut(v)) AddCDeltaP(IRook(me),lsb(u),lsb(v),0)
	for (u = Queen(me); T(u); Cut(u))
		for (v = free & QueenAttacks(lsb(u),occ); T(v); Cut(v)) AddCDeltaP(IQueen(me),lsb(u),lsb(v),0)
	for (v = SArea[lsb(King(me))] & free & (~Current->att[opp]); T(v); Cut(v)) AddCDeltaP(IKing(me),lsb(King(me)),lsb(v),0)
	*list = 0;
	return list;
}

// インスタンス化.
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
