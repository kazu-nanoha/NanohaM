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
#include "evaluate.h" // ToDo: 将来的には不要かな？.
#include "misc.h"
#include "position.h"
#include "types.h"

Position root_pos;

#define Square(sq) Board->square[sq]

Position::Position() : Current(Data), sp(0), save_sp(0) {}

Position::~Position() {}

bool Position::set_sfen(const std::string &sfen)
{
	// ToDo: 局面をセットする.
	return true;
}

void Position::init_search() { clear_forward(); }

template <bool me> void Position::do_move(int move)
{
	constexpr bool opp = !me;
	GEntry *Entry;
	int from, to, piece, capture;
	GData *Next;
	uint64_t mask_from, mask_to;

	to = To(move);
	Next = Current + 1;
	capture = Square(to);
	if (!(capture)) {
		Next->capture = 0;
		goto non_capture;
	}
	from = From(move);
	piece = Square(from);
	Next->turn = opp;
	Next->capture = capture;
	Square(from) = 0;
	Square(to) = piece;
	Next->piece = piece;
	mask_from = Bit(from);
	mask_to = Bit(to);
	BB(piece) ^= mask_from;
	Piece(me) ^= mask_from;
	BB(capture) ^= mask_to;
	Piece(opp) ^= mask_to;
	BB(piece) |= mask_to;
	Piece(me) |= mask_to;
	Next->pst = Current->pst + Pst(piece, to) - Pst(piece, from) - Pst(capture, to);
	Next->key = Current->key ^ PieceKey[piece][from] ^ PieceKey[piece][to] ^ PieceKey[capture][to];
	Next->material = Current->material - MatCode[capture];
	if ((Current->material & FlagUnusualMaterial) != 0 && capture >= WhiteKnight) {
		if (popcnt(BB(WhiteQueen)) <= 2 && popcnt(BB(BlackQueen)) <= 2) {
			if (popcnt(BB(WhiteLight)) <= 1 && popcnt(BB(BlackLight)) <= 1 && popcnt(BB(WhiteKnight)) <= 2 &&
			    popcnt(BB(BlackKnight)) <= 2 && popcnt(BB(WhiteRook)) <= 2 && popcnt(BB(BlackRook)) <= 2)
				Next->material ^= FlagUnusualMaterial;
		}
	}
	if (piece == IPawn(me)) {
		if (IsPromotion(move)) {
			piece = Promotion(move, me);
			Square(to) = piece;
			Next->material += MatCode[piece] - MatCode[IPawn(me)];
			if (piece < WhiteRook) {
				if (piece >= WhiteLight && BB(piece) != 0)
					Next->material |= FlagUnusualMaterial;
				if (Multiple(BB(piece)))
					Next->material |= FlagUnusualMaterial;
			} else if (Multiple(BB(piece)))
				Next->material |= FlagUnusualMaterial;
			Pawn(me) ^= mask_to;
			BB(piece) |= mask_to;
			Next->pst += Pst(piece, to) - Pst(IPawn(me), to);
			Next->key ^= PieceKey[piece][to] ^ PieceKey[IPawn(me)][to];
		}
	}
	if (!(Next->material & FlagUnusualMaterial))
		prefetch((char *)(Material + Next->material), _MM_HINT_NTA);
	Next->turn = Current->turn ^ 1;
	Next->key ^= TurnKey;
	Entry = TT.top(Next->key);
	prefetch((char *)Entry, _MM_HINT_NTA);
	Next->ply = 0;
	goto finish;
non_capture:
	from = From(move);
	Next->ply = Current->ply + 1;
	piece = Square(from);
	Square(from) = 0;
	Square(to) = piece;
	Next->piece = piece;
	mask_from = Bit(from);
	mask_to = Bit(to);
	BB(piece) ^= mask_from;
	Piece(me) ^= mask_from;
	BB(piece) |= mask_to;
	Piece(me) |= mask_to;
	Next->pst = Current->pst + Pst(piece, to) - Pst(piece, from);
	Next->key = Current->key ^ PieceKey[piece][to] ^ PieceKey[piece][from];
	Next->material = Current->material;
	if (piece == IPawn(me)) {
		Next->ply = 0;
		if (IsPromotion(move)) {
			piece = Promotion(move, me);
			Square(to) = piece;
			Next->material += MatCode[piece] - MatCode[IPawn(me)];
			if (piece < WhiteRook) {
				if (piece >= WhiteLight && BB(piece) != 0)
					Next->material |= FlagUnusualMaterial;
				if (Multiple(BB(piece)))
					Next->material |= FlagUnusualMaterial;
			} else if (Multiple(BB(piece)))
				Next->material |= FlagUnusualMaterial;
			Pawn(me) ^= mask_to;
			BB(piece) |= mask_to;
			Next->pst += Pst(piece, to) - Pst(IPawn(me), to);
			Next->key ^= PieceKey[piece][to] ^ PieceKey[IPawn(me)][to];
		}
	}

	Next->turn = opp;
	Next->key ^= TurnKey;
	Entry = TT.top(Next->key);
	prefetch((char *)Entry, _MM_HINT_NTA);
finish:
	sp++;
	Stack[sp] = Next->key;
	Next->move = move;
	///	Next->gen_flags = 0;
	Current++;
	nodes++;
}

template <bool me> void Position::undo_move(int move)
{
	constexpr bool opp = !me;
	int to, from, piece;
	from = From(move);
	to = To(move);
	if (IsPromotion(move)) {
		BB(Square(to)) ^= Bit(to);
		piece = IPawn(me);
	} else
		piece = Square(to);
	Square(from) = piece;
	BB(piece) |= Bit(from);
	Piece(me) |= Bit(from);
	BB(piece) &= ~Bit(to);
	Piece(me) ^= Bit(to);
	Square(to) = Current->capture;
	if (Current->capture) {
		BB(Current->capture) |= Bit(to);
		Piece(opp) |= Bit(to);
	}
	Current--;
	sp--;
}

void Position::do_null()
{
	GData *Next;
	GEntry *Entry;

	Next = Current + 1;
	Next->key = Current->key ^ TurnKey;
	Entry = TT.top(Next->key);
	prefetch((char *)Entry, _MM_HINT_NTA);
	Next->eval_key = 0;
	Next->turn = Current->turn ^ 1;
	Next->material = Current->material;
	Next->pst = Current->pst;
	Next->ply = 0;
	Next->capture = 0;
	sp++;
	Next->att[White] = Current->att[White];
	Next->att[Black] = Current->att[Black];
	Next->patt[White] = Current->patt[White];
	Next->patt[Black] = Current->patt[Black];
	Next->xray[White] = Current->xray[White];
	Next->xray[Black] = Current->xray[Black];
	Next->pin[White] = Current->pin[White];
	Next->pin[Black] = Current->pin[Black];
	Stack[sp] = Next->key;
	Next->threat = Current->threat;
	Next->passer = Current->passer;
	Next->score = -Current->score;
	Next->move = 0;
	///	Next->gen_flags = 0;
	Current++;
	nodes++;
}

void Position::undo_null()
{
	Current--;
	sp--;
}

template <bool me> int Position::is_legal(int move)
{
	constexpr bool opp = !me;
	int from, to, piece, capture;
	uint64_t u, occ;

	from = From(move);
	to = To(move);
	piece = Board->square[from];
	capture = Board->square[to];
	if (piece == 0)
		return 0;
	if ((piece & 1) != Current->turn)
		return 0;
	if (capture) {
		if ((capture & 1) == (piece & 1))
			return 0;
		if (capture >= WhiteKing)
			return 0;
	}
	occ = PieceAll;
	u = Bit(to);
	if (piece >= WhiteLight && piece < WhiteKing) {
		if ((QMask[from] & u) == 0)
			return 0;
		if (Between[from][to] & occ)
			return 0;
	}
	if (IsPromotion(move) && Board->square[from] >= WhiteKnight)
		return 0;
	if (piece == IPawn(me)) {
		if (u & PMove[me][from]) {
			if (capture)
				return 0;
			if ((u & Line(me, 7)) != 0 && !IsPromotion(move))
				return 0;
			return 1;
		} else if (to == (from + 2 * Push(me))) {
			if (capture)
				return 0;
			if (Square(to - Push(me)))
				return 0;
			if (!(u & Line(me, 3)))
				return 0;
			return 1;
		} else if (u & PAtt[me][from]) {
			if (capture == 0)
				return 0;
			if ((u & Line(me, 7)) != 0 && !IsPromotion(move))
				return 0;
			return 1;
		} else
			return 0;
	} else if (piece == IKing(me)) {
		if (!(SArea[from] & u))
			return 0;
		if (Current->att[opp] & u)
			return 0;
		return 1;
	}
	piece = (piece >> 1) - 2;
	if (piece == 0) {
		if (u & NAtt[from])
			return 1;
		else
			return 0;
	} else {
		if (piece <= 2) {
			if (BMask[from] & u)
				return 1;
		} else if (piece == 3) {
			if (RMask[from] & u)
				return 1;
		} else
			return 1;
		return 0;
	}
}

template <bool me> int Position::is_check(int move)
{ // doesn't detect castling and ep checks
	constexpr bool opp = !me;
	uint64_t king;
	int from, to, piece, king_sq;

	from = From(move);
	to = To(move);
	king = King(opp);
	king_sq = lsb(king);
	piece = Square(from);
	if ((Bit(from) & Current->xray[me]) != 0 && !(FullLine[king_sq][from] & Bit(to)))
		return 1;
	if (piece < WhiteKnight) {
		if (PAtt[me][to] & king)
			return 1;
		if ((Bit(to) & Line(me, 7)) != 0 && (king & Line(me, 7)) != 0 && !(Between[to][king_sq] & PieceAll))
			return 1;
	} else if (piece < WhiteLight) {
		if (NAtt[to] & king)
			return 1;
	} else if (piece < WhiteRook) {
		if (BMask[to] & king)
			if (!(Between[king_sq][to] & PieceAll))
				return 1;
	} else if (piece < WhiteQueen) {
		if (RMask[to] & king)
			if (!(Between[king_sq][to] & PieceAll))
				return 1;
	} else if (piece < WhiteKing) {
		if (QMask[to] & king)
			if (!(Between[king_sq][to] & PieceAll))
				return 1;
	}
	return 0;
}

///---------------------------------------------------------------------------

template <bool me> int Position::see(int move, int margin)
{
	constexpr bool opp = !me;
	int from, to, piece, capture, delta, sq, pos;
	uint64_t clear, def, att, occ, b_area, r_slider_att, b_slider_att, r_slider_def, b_slider_def, r_area, u, new_att,
	    my_bishop, opp_bishop;
	from = From(move);
	to = To(move);
	piece = SeeValue[Square(from)];
	capture = SeeValue[Square(to)];
	delta = piece - capture;
	if (delta <= -margin)
		return 1;
	if (piece == SeeValue[WhiteKing])
		return 1;
	if (Current->xray[me] & Bit(from))
		return 1;
	if ((Current->pin[me] & Bit(from)) != 0 && piece <= SeeValue[WhiteDark])
		return 1;
	if (piece > (SeeValue[WhiteKing] >> 1))
		return 1;
	if (!(Current->att[opp] & Bit(to)))
		return 1;
	att = PAtt[me][to] & Pawn(opp);
	if (att != 0 && delta + margin > SeeValue[WhitePawn])
		return 0;
	clear = ~Bit(from);
	def = PAtt[opp][to] & Pawn(me) & clear;
	if (def != 0 && delta + SeeValue[WhitePawn] + margin <= 0)
		return 1;
	att |= NAtt[to] & Knight(opp);
	if (att != 0 && delta > SeeValue[WhiteDark] - margin)
		return 0;
	occ = PieceAll & clear;
	b_area = BishopAttacks(to, occ);
	opp_bishop = Bishop(opp);
	if (delta > SeeValue[IDark(me)] - margin)
		if (b_area & opp_bishop)
			return 0;
	my_bishop = Bishop(me);
	b_slider_att = BMask[to] & (opp_bishop | Queen(opp));
	r_slider_att = RMask[to] & Major(opp);
	b_slider_def = BMask[to] & (my_bishop | Queen(me)) & clear;
	r_slider_def = RMask[to] & Major(me) & clear;
	att |= (b_slider_att & b_area);
	def |= NAtt[to] & Knight(me) & clear;
	r_area = RookAttacks(to, occ);
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
			for (new_att = FullLine[to][sq] & b_slider_att & occ & (~att); new_att != 0; Cut(new_att)) {
				pos = lsb(new_att);
				if (!(Between[to][pos] & occ)) {
					Add(att, pos);
					break;
				}
			}
		} else if (u = (att & Knight(opp))) {
			capture -= piece;
			piece = SeeValue[WhiteKnight];
			att ^= (~(u - 1)) & u;
		} else if (u = (att & opp_bishop)) {
			capture -= piece;
			piece = SeeValue[WhiteDark];
			sq = lsb(u);
			occ ^= Bit(sq);
			att ^= Bit(sq);
			for (new_att = FullLine[to][sq] & b_slider_att & occ & (~att); new_att != 0; Cut(new_att)) {
				pos = lsb(new_att);
				if (!(Between[to][pos] & occ)) {
					Add(att, pos);
					break;
				}
			}
		} else if (u = (att & Rook(opp))) {
			capture -= piece;
			piece = SeeValue[WhiteRook];
			sq = lsb(u);
			occ ^= Bit(sq);
			att ^= Bit(sq);
			for (new_att = FullLine[to][sq] & r_slider_att & occ & (~att); new_att != 0; Cut(new_att)) {
				pos = lsb(new_att);
				if (!(Between[to][pos] & occ)) {
					Add(att, pos);
					break;
				}
			}
		} else if (u = (att & Queen(opp))) {
			capture -= piece;
			piece = SeeValue[WhiteQueen];
			sq = lsb(u);
			occ ^= Bit(sq);
			att ^= Bit(sq);
			for (new_att = FullLine[to][sq] & (r_slider_att | b_slider_att) & occ & (~att); new_att != 0;
			     Cut(new_att)) {
				pos = lsb(new_att);
				if (!(Between[to][pos] & occ)) {
					Add(att, pos);
					break;
				}
			}
		} else if (u = (att & King(opp))) {
			capture -= piece;
			piece = SeeValue[WhiteKing];
		} else
			return 1;
		if (capture < -(SeeValue[WhiteKing] >> 1))
			return 0;
		if (piece + capture < margin)
			return 0;
		if (u = (def & Pawn(me))) {
			capture += piece;
			piece = SeeValue[WhitePawn];
			sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & b_slider_def & occ & (~att); new_att != 0; Cut(new_att)) {
				pos = lsb(new_att);
				if (!(Between[to][pos] & occ)) {
					Add(def, pos);
					break;
				}
			}
		} else if (u = (def & Knight(me))) {
			capture += piece;
			piece = SeeValue[WhiteKnight];
			def ^= (~(u - 1)) & u;
		} else if (u = (def & my_bishop)) {
			capture += piece;
			piece = SeeValue[WhiteDark];
			sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & b_slider_def & occ & (~att); new_att != 0; Cut(new_att)) {
				pos = lsb(new_att);
				if (!(Between[to][pos] & occ)) {
					Add(def, pos);
					break;
				}
			}
		} else if (u = (def & Rook(me))) {
			capture += piece;
			piece = SeeValue[WhiteRook];
			sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & r_slider_def & occ & (~att); new_att != 0; Cut(new_att)) {
				pos = lsb(new_att);
				if (!(Between[to][pos] & occ)) {
					Add(def, pos);
					break;
				}
			}
		} else if (u = (def & Queen(me))) {
			capture += piece;
			piece = SeeValue[WhiteQueen];
			sq = lsb(u);
			occ ^= Bit(sq);
			def ^= Bit(sq);
			for (new_att = FullLine[to][sq] & (r_slider_def | b_slider_def) & occ & (~att); new_att != 0;
			     Cut(new_att)) {
				pos = lsb(new_att);
				if (!(Between[to][pos] & occ)) {
					Add(def, pos);
					break;
				}
			}
		} else if (u = (def & King(me))) {
			capture += piece;
			piece = SeeValue[WhiteKing];
		} else
			return 0;
		if (capture > (SeeValue[WhiteKing] >> 1))
			return 1;
		if (capture - piece >= margin)
			return 1;
	}
}

// インスタンス化.
template void Position::do_move<true>(int move);
template void Position::do_move<false>(int move);
template void Position::undo_move<true>(int move);
template void Position::undo_move<false>(int move);
template int Position::is_legal<true>(int move);
template int Position::is_legal<false>(int move);
template int Position::is_check<true>(int move);
template int Position::is_check<false>(int move);

template int Position::see<0>(int move, int margin);
template int Position::see<1>(int move, int margin);
