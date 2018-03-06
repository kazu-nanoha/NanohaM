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
#include "evaluate.h"	// ToDo: 将来的には不要かな？.

Position root_pos;

// Memo: L159
constexpr uint8_t UpdateCastling[64] =
{
    0xFF^CanCastle_OOO,0xFF,0xFF,0xFF,0xFF^(CanCastle_OO|CanCastle_OOO),
        0xFF,0xFF,0xFF^CanCastle_OO,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF^CanCastle_ooo,0xFF,0xFF,0xFF,0xFF^(CanCastle_oo|CanCastle_ooo),
        0xFF,0xFF,0xFF^CanCastle_oo
};

Position::Position()
: Current(Data)
{
}

Position::~Position()
{
}

template <bool me> void Position::do_move(int move)
{
	constexpr bool opp = !me;
	GEntry * Entry;
	GPawnEntry * PawnEntry;
	int from, to, piece, capture;
	GData * Next;
	uint64_t u, mask_from, mask_to;

	to = To(move);
	Next = Current + 1;
	Next->ep_square = 0;
	capture = Square(to);
    if (F(capture)) {
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
	Next->castle_flags = Current->castle_flags & UpdateCastling[to] & UpdateCastling[from];
	Next->pst = Current->pst + Pst(piece,to) - Pst(piece,from) - Pst(capture,to);
	Next->key = Current->key ^ PieceKey[piece][from] ^ PieceKey[piece][to] ^ PieceKey[capture][to] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
	if (capture != IPawn(opp)) Next->pawn_key = Current->pawn_key ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags]; // of course we can put a lot of operations inside this "if {}" but the speedup won't be worth the effort
	else Next->pawn_key = Current->pawn_key ^ PieceKey[IPawn(opp)][to] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
	Next->material = Current->material - MatCode[capture];
	if (T(Current->material & FlagUnusualMaterial) && capture >= WhiteKnight) {
		if (popcnt(BB(WhiteQueen)) <= 2 && popcnt(BB(BlackQueen)) <= 2) {
			if (popcnt(BB(WhiteLight)) <= 1 && popcnt(BB(BlackLight)) <= 1 && popcnt(BB(WhiteKnight)) <= 2
				&& popcnt(BB(BlackKnight)) <= 2 && popcnt(BB(WhiteRook)) <= 2 && popcnt(BB(BlackRook)) <= 2)
				Next->material ^= FlagUnusualMaterial;
		}
	}
	if (piece == IPawn(me)) {
		Next->pawn_key ^= PieceKey[IPawn(me)][from] ^ PieceKey[piece][to];
		if (IsPromotion(move)) {
			piece = Promotion(move,me);
			Square(to) = piece;
		    Next->material += MatCode[piece] - MatCode[IPawn(me)];
			if (piece < WhiteRook) {
				if (piece >= WhiteLight && T(BB(piece))) Next->material |= FlagUnusualMaterial;
				if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			} else if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			Pawn(me) ^= mask_to;
			BB(piece) |= mask_to;
			Next->pst += Pst(piece,to) - Pst(IPawn(me),to);
			Next->key ^= PieceKey[piece][to] ^ PieceKey[IPawn(me)][to];
			Next->pawn_key ^= PieceKey[IPawn(me)][to];
		}
		PawnEntry = PAWNHASH.entry(Next->pawn_key);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	} else if (piece >= WhiteKing) {
		Next->pawn_key ^= PieceKey[piece][from] ^ PieceKey[piece][to];
		PawnEntry = PAWNHASH.entry(Next->pawn_key);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	} else if (capture < WhiteKnight) {
		PawnEntry = PAWNHASH.entry(Next->pawn_key);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	}
	if (F(Next->material & FlagUnusualMaterial)) prefetch((char *)(Material + Next->material), _MM_HINT_NTA); 
	if (Current->ep_square) Next->key ^= EPKey[File(Current->ep_square)];
	Next->turn = Current->turn ^ 1;
	Next->key ^= TurnKey;
	Entry = TT.top(Next->key);
	prefetch((char *)Entry,_MM_HINT_NTA);
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
	Next->castle_flags = Current->castle_flags & UpdateCastling[to] & UpdateCastling[from];
	Next->pst = Current->pst + Pst(piece,to) - Pst(piece,from);
	Next->key = Current->key ^ PieceKey[piece][to] ^ PieceKey[piece][from] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
	Next->material = Current->material;
	if (piece == IPawn(me)) {
		Next->ply = 0;
		Next->pawn_key = Current->pawn_key ^ PieceKey[IPawn(me)][to] ^ PieceKey[IPawn(me)][from] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
		if (IsEP(move)) {
			Square(to ^ 8) = 0;
			u = Bit(to ^ 8);
			Next->key ^= PieceKey[IPawn(opp)][to ^ 8];
			Next->pawn_key ^= PieceKey[IPawn(opp)][to ^ 8];
			Next->pst -= Pst(IPawn(opp),to ^ 8);
			Pawn(opp) &= ~u;
			Piece(opp) &= ~u;
			Next->material -= MatCode[IPawn(opp)];
		} else if (IsPromotion(move)) {
			piece = Promotion(move,me);
			Square(to) = piece;
		    Next->material += MatCode[piece] - MatCode[IPawn(me)];
			if (piece < WhiteRook) {
				if (piece >= WhiteLight && T(BB(piece))) Next->material |= FlagUnusualMaterial;
				if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			} else if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			Pawn(me) ^= mask_to;
			BB(piece) |= mask_to;
			Next->pst += Pst(piece,to) - Pst(IPawn(me),to);
			Next->key ^= PieceKey[piece][to] ^ PieceKey[IPawn(me)][to];
			Next->pawn_key ^= PieceKey[IPawn(me)][to];
		} else if ((to ^ from) == 16) {
			if (PAtt[me][(to + from) >> 1] & Pawn(opp)) {
				Next->ep_square = (to + from) >> 1;
				Next->key ^= EPKey[File(Next->ep_square)];
			}
		}
		PawnEntry = PAWNHASH.entry(Next->pawn_key);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	} else {
		if (piece < WhiteKing) Next->pawn_key = Current->pawn_key ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
		else {
			Next->pawn_key = Current->pawn_key ^ PieceKey[piece][to] ^ PieceKey[piece][from] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
			PawnEntry = PAWNHASH.entry(Next->pawn_key);
	        prefetch((char *)PawnEntry,_MM_HINT_NTA);
		}
		if (IsCastling(move)) {
			int rold, rnew;
			Next->ply = 0;
			if (to == 6) {
			    rold = 7; 
			    rnew = 5;
		    } else if (to == 2) {
                rold = 0; 
			    rnew = 3;
		    } else if (to == 62) {
                rold = 63;
			    rnew = 61;
		    } else if (to == 58) {
                rold = 56; 
			    rnew = 59;
		    }
			Add(mask_to,rnew);
			Square(rold) = 0;
			Square(rnew) = IRook(me);
			BB(IRook(me)) ^= Bit(rold);
			Piece(me) ^= Bit(rold);
			BB(IRook(me)) |= Bit(rnew);
			Piece(me) |= Bit(rnew);
			Next->pst += Pst(IRook(me),rnew) - Pst(IRook(me),rold);
			Next->key ^= PieceKey[IRook(me)][rnew] ^ PieceKey[IRook(me)][rold];
		}
	}

	if (Current->ep_square) Next->key ^= EPKey[File(Current->ep_square)];
	Next->turn = opp;
	Next->key ^= TurnKey;
	Entry = TT.top(Next->key);
	prefetch((char *)Entry,_MM_HINT_NTA);
finish:
	sp++;
	Stack[sp] = Next->key;
	Next->move = move;
	Next->gen_flags = 0;
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
	} else piece = Square(to);
	Square(from) = piece;
	BB(piece) |= Bit(from);
	Piece(me) |= Bit(from);
	BB(piece) &= ~Bit(to);
	Piece(me) ^= Bit(to);
	Square(to) = Current->capture;
	if (Current->capture) {
	    BB(Current->capture) |= Bit(to);
	    Piece(opp) |= Bit(to);
	} else {
		if (IsCastling(move)) {
			int rold, rnew;
			if (to == 6) {
			    rold = 7; 
			    rnew = 5;
		    } else if (to == 2) {
                rold = 0; 
			    rnew = 3;
		    } else if (to == 62) {
                rold = 63;
			    rnew = 61;
		    } else if (to == 58) {
                rold = 56; 
			    rnew = 59;
		    }
			Square(rnew) = 0;
			Square(rold) = IRook(me);
			Rook(me) ^= Bit(rnew);
			Piece(me) ^= Bit(rnew);
			Rook(me) |= Bit(rold);
			Piece(me) |= Bit(rold);
		} else if (IsEP(move)) {
			to = to ^ 8;
			piece = IPawn(opp);
			Square(to) = piece;
			Piece(opp) |= Bit(to);
			Pawn(opp) |= Bit(to);
		}
	}
	Current--;
	sp--;
}

void Position::do_null() {
	GData * Next;
	GEntry * Entry;

	Next = Current + 1;
	Next->key = Current->key ^ TurnKey;
	Entry = TT.top(Next->key);
	prefetch((char *)Entry,_MM_HINT_NTA);
	Next->pawn_key = Current->pawn_key;
	Next->eval_key = 0;
	Next->turn = Current->turn ^ 1;
	Next->material = Current->material;
	Next->pst = Current->pst;
	Next->ply = 0;
	Next->castle_flags = Current->castle_flags;
	Next->ep_square = 0;
	Next->capture = 0;
	if (Current->ep_square) Next->key ^= EPKey[File(Current->ep_square)];
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
	Next->gen_flags = 0;
	Current++;
	nodes++;
}

void Position::undo_null() {
	Current--;
	sp--;
}

template <bool me> int Position::is_legal(int move) {
	constexpr bool opp = !me;
	int from, to, piece, capture;
	uint64_t u, occ;

    from = From(move);
	to = To(move);
	piece = Board->square[from];
	capture = Board->square[to];
	if (piece == 0) return 0;
	if ((piece & 1) != Current->turn) return 0;
	if (capture) {
		if ((capture & 1) == (piece & 1)) return 0;
		if (capture >= WhiteKing) return 0;
	}
	occ = PieceAll;
	u = Bit(to);
	if (piece >= WhiteLight && piece < WhiteKing) {
	    if ((QMask[from] & u) == 0) return 0;
		if (Between[from][to] & occ) return 0;
	}
	if (IsEP(move)) {
		if (piece >= WhiteKnight) return 0;
		if (Current->ep_square != to) return 0;
		return 1;
	}
	if (IsCastling(move) && Board->square[from] < WhiteKing) return 0;
	if (IsPromotion(move) && Board->square[from] >= WhiteKnight) return 0;
	if (piece == IPawn(me)) {
		if (u & PMove[me][from]) {
            if (capture) return 0;
			if (T(u & Line(me,7)) && !IsPromotion(move)) return 0;
			return 1;
		} else if (to == (from + 2 * Push(me))) {
            if (capture) return 0;
			if (Square(to - Push(me))) return 0;
			if (F(u & Line(me,3))) return 0;
			return 1;
		} else if (u & PAtt[me][from]) {
            if (capture == 0) return 0;
			if (T(u & Line(me,7)) && !IsPromotion(move)) return 0;
			return 1;
		} else return 0;
	} else if (piece == IKing(me)) {
		if (me == White) {
		    if (IsCastling(move)) {
			    if (u & 0x40) {
                    if (((Current->castle_flags) & CanCastle_OO) == 0) return 0;
					if (occ & 0x60) return 0;
					if (Current->att[Black] & 0x70) return 0;
				} else {
					if (((Current->castle_flags) & CanCastle_OOO) == 0) return 0;
					if (occ & 0xE) return 0;
					if (Current->att[Black] & 0x1C) return 0;
				}
				return 1;
			}
		} else {
            if (IsCastling(move)) {
				if (u & 0x4000000000000000) {
                    if (((Current->castle_flags) & CanCastle_oo) == 0) return 0;
					if (occ & 0x6000000000000000) return 0;
					if (Current->att[White] & 0x7000000000000000) return 0;
				} else {
					if (((Current->castle_flags) & CanCastle_ooo) == 0) return 0;
					if (occ & 0x0E00000000000000) return 0;
					if (Current->att[White] & 0x1C00000000000000) return 0;
				}
				return 1;
			}
		}
        if (F(SArea[from] & u)) return 0;
	    if (Current->att[opp] & u) return 0;
		return 1;
	}
	piece = (piece >> 1) - 2;
	if (piece == 0) {
        if (u & NAtt[from]) return 1;
		else return 0;
	} else {
		if (piece <= 2) {
			if (BMask[from] & u) return 1;
		} else if (piece == 3) {
			if (RMask[from] & u) return 1;
		} else return 1;
		return 0;
	}
}

template <bool me> int Position::is_check(int move) { // doesn't detect castling and ep checks
	constexpr bool opp = !me;
	uint64_t king;
	int from, to, piece, king_sq;

	from = From(move);
	to = To(move);
	king = King(opp);
	king_sq = lsb(king);
	piece = Square(from);
	if (T(Bit(from) & Current->xray[me]) && F(FullLine[king_sq][from] & Bit(to))) return 1;
	if (piece < WhiteKnight) {
		if (PAtt[me][to] & king) return 1;
		if (T(Bit(to) & Line(me, 7)) && T(king & Line(me, 7)) && F(Between[to][king_sq] & PieceAll)) return 1;
	} else if (piece < WhiteLight) {
		if (NAtt[to] & king) return 1;
	} else if (piece < WhiteRook) {
		if (BMask[to] & king) if (F(Between[king_sq][to] & PieceAll)) return 1;
	} else if (piece < WhiteQueen) {
		if (RMask[to] & king) if (F(Between[king_sq][to] & PieceAll)) return 1;
	} else if (piece < WhiteKing) {
		if (QMask[to] & king) if (F(Between[king_sq][to] & PieceAll)) return 1;
	}
	return 0;
}

///---------------------------------------------------------------------------

#if 0	// ToDo:: DEL
extern GData *Current;

// Memo: L2508
template <bool me> void do_move(int move)
{
	GEntry * Entry;
	GPawnEntry * PawnEntry;
	int from, to, piece, capture;
	GData * Next;
	uint64_t u, mask_from, mask_to;

	to = To(move);
	Next = Current + 1;
	Next->ep_square = 0;
	capture = Square(to);
    if (F(capture)) {
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
	Next->castle_flags = Current->castle_flags & UpdateCastling[to] & UpdateCastling[from];
	Next->pst = Current->pst + Pst(piece,to) - Pst(piece,from) - Pst(capture,to);
	Next->key = Current->key ^ PieceKey[piece][from] ^ PieceKey[piece][to] ^ PieceKey[capture][to] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
	if (capture != IPawn(opp)) Next->pawn_key = Current->pawn_key ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags]; // of course we can put a lot of operations inside this "if {}" but the speedup won't be worth the effort
	else Next->pawn_key = Current->pawn_key ^ PieceKey[IPawn(opp)][to] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
	Next->material = Current->material - MatCode[capture];
	if (T(Current->material & FlagUnusualMaterial) && capture >= WhiteKnight) {
		if (popcnt(BB(WhiteQueen)) <= 2 && popcnt(BB(BlackQueen)) <= 2) {
			if (popcnt(BB(WhiteLight)) <= 1 && popcnt(BB(BlackLight)) <= 1 && popcnt(BB(WhiteKnight)) <= 2
				&& popcnt(BB(BlackKnight)) <= 2 && popcnt(BB(WhiteRook)) <= 2 && popcnt(BB(BlackRook)) <= 2)
				Next->material ^= FlagUnusualMaterial;
		}
	}
	if (piece == IPawn(me)) {
		Next->pawn_key ^= PieceKey[IPawn(me)][from] ^ PieceKey[piece][to];
		if (IsPromotion(move)) {
			piece = Promotion(move,me);
			Square(to) = piece;
		    Next->material += MatCode[piece] - MatCode[IPawn(me)];
			if (piece < WhiteRook) {
				if (piece >= WhiteLight && T(BB(piece))) Next->material |= FlagUnusualMaterial;
				if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			} else if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			Pawn(me) ^= mask_to;
			BB(piece) |= mask_to;
			Next->pst += Pst(piece,to) - Pst(IPawn(me),to);
			Next->key ^= PieceKey[piece][to] ^ PieceKey[IPawn(me)][to];
			Next->pawn_key ^= PieceKey[IPawn(me)][to];
		}
		PawnEntry = PAWNHASH.entry(Next->pawn_key);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	} else if (piece >= WhiteKing) {
		Next->pawn_key ^= PieceKey[piece][from] ^ PieceKey[piece][to];
		PawnEntry = PAWNHASH.entry(Next->pawn_key);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	} else if (capture < WhiteKnight) {
		PawnEntry = PAWNHASH.entry(Next->pawn_key);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	}
	if (F(Next->material & FlagUnusualMaterial)) prefetch((char *)(Material + Next->material), _MM_HINT_NTA); 
	if (Current->ep_square) Next->key ^= EPKey[File(Current->ep_square)];
	Next->turn = Current->turn ^ 1;
	Next->key ^= TurnKey;
	Entry = TT.top(Next->key);
	prefetch((char *)Entry,_MM_HINT_NTA);
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
	Next->castle_flags = Current->castle_flags & UpdateCastling[to] & UpdateCastling[from];
	Next->pst = Current->pst + Pst(piece,to) - Pst(piece,from);
	Next->key = Current->key ^ PieceKey[piece][to] ^ PieceKey[piece][from] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
	Next->material = Current->material;
	if (piece == IPawn(me)) {
		Next->ply = 0;
		Next->pawn_key = Current->pawn_key ^ PieceKey[IPawn(me)][to] ^ PieceKey[IPawn(me)][from] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
		if (IsEP(move)) {
			Square(to ^ 8) = 0;
			u = Bit(to ^ 8);
			Next->key ^= PieceKey[IPawn(opp)][to ^ 8];
			Next->pawn_key ^= PieceKey[IPawn(opp)][to ^ 8];
			Next->pst -= Pst(IPawn(opp),to ^ 8);
			Pawn(opp) &= ~u;
			Piece(opp) &= ~u;
			Next->material -= MatCode[IPawn(opp)];
		} else if (IsPromotion(move)) {
			piece = Promotion(move,me);
			Square(to) = piece;
		    Next->material += MatCode[piece] - MatCode[IPawn(me)];
			if (piece < WhiteRook) {
				if (piece >= WhiteLight && T(BB(piece))) Next->material |= FlagUnusualMaterial;
				if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			} else if (Multiple(BB(piece))) Next->material |= FlagUnusualMaterial;
			Pawn(me) ^= mask_to;
			BB(piece) |= mask_to;
			Next->pst += Pst(piece,to) - Pst(IPawn(me),to);
			Next->key ^= PieceKey[piece][to] ^ PieceKey[IPawn(me)][to];
			Next->pawn_key ^= PieceKey[IPawn(me)][to];
		} else if ((to ^ from) == 16) {
			if (PAtt[me][(to + from) >> 1] & Pawn(opp)) {
				Next->ep_square = (to + from) >> 1;
				Next->key ^= EPKey[File(Next->ep_square)];
			}
		}
		PawnEntry = PAWNHASH.entry(Next->pawn_key);
	    prefetch((char *)PawnEntry,_MM_HINT_NTA);
	} else {
		if (piece < WhiteKing) Next->pawn_key = Current->pawn_key ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
		else {
			Next->pawn_key = Current->pawn_key ^ PieceKey[piece][to] ^ PieceKey[piece][from] ^ CastleKey[Current->castle_flags] ^ CastleKey[Next->castle_flags];
			PawnEntry = PAWNHASH.entry(Next->pawn_key);
	        prefetch((char *)PawnEntry,_MM_HINT_NTA);
		}
		if (IsCastling(move)) {
			int rold, rnew;
			Next->ply = 0;
			if (to == 6) {
			    rold = 7; 
			    rnew = 5;
		    } else if (to == 2) {
                rold = 0; 
			    rnew = 3;
		    } else if (to == 62) {
                rold = 63;
			    rnew = 61;
		    } else if (to == 58) {
                rold = 56; 
			    rnew = 59;
		    }
			Add(mask_to,rnew);
			Square(rold) = 0;
			Square(rnew) = IRook(me);
			BB(IRook(me)) ^= Bit(rold);
			Piece(me) ^= Bit(rold);
			BB(IRook(me)) |= Bit(rnew);
			Piece(me) |= Bit(rnew);
			Next->pst += Pst(IRook(me),rnew) - Pst(IRook(me),rold);
			Next->key ^= PieceKey[IRook(me)][rnew] ^ PieceKey[IRook(me)][rold];
		}
	}

	if (Current->ep_square) Next->key ^= EPKey[File(Current->ep_square)];
	Next->turn = opp;
	Next->key ^= TurnKey;
	Entry = TT.top(Next->key);
	prefetch((char *)Entry,_MM_HINT_NTA);
finish:
	sp++;
	Stack[sp] = Next->key;
	Next->move = move;
	Next->gen_flags = 0;
	Current++;
	nodes++;
}

template <bool me> void undo_move(int move) {
	int to, from, piece;
	from = From(move);
	to = To(move);
	if (IsPromotion(move)) {
		BB(Square(to)) ^= Bit(to);
		piece = IPawn(me);
	} else piece = Square(to);
	Square(from) = piece;
	BB(piece) |= Bit(from);
	Piece(me) |= Bit(from);
	BB(piece) &= ~Bit(to);
	Piece(me) ^= Bit(to);
	Square(to) = Current->capture;
	if (Current->capture) {
	    BB(Current->capture) |= Bit(to);
	    Piece(opp) |= Bit(to);
	} else {
		if (IsCastling(move)) {
			int rold, rnew;
			if (to == 6) {
			    rold = 7; 
			    rnew = 5;
		    } else if (to == 2) {
                rold = 0; 
			    rnew = 3;
		    } else if (to == 62) {
                rold = 63;
			    rnew = 61;
		    } else if (to == 58) {
                rold = 56; 
			    rnew = 59;
		    }
			Square(rnew) = 0;
			Square(rold) = IRook(me);
			Rook(me) ^= Bit(rnew);
			Piece(me) ^= Bit(rnew);
			Rook(me) |= Bit(rold);
			Piece(me) |= Bit(rold);
		} else if (IsEP(move)) {
			to = to ^ 8;
			piece = IPawn(opp);
			Square(to) = piece;
			Piece(opp) |= Bit(to);
			Pawn(opp) |= Bit(to);
		}
	}
	Current--;
	sp--;
}

void do_null() {
	GData * Next;
	GEntry * Entry;

	Next = Current + 1;
	Next->key = Current->key ^ TurnKey;
	Entry = TT.top(Next->key);
	prefetch((char *)Entry,_MM_HINT_NTA);
	Next->pawn_key = Current->pawn_key;
	Next->eval_key = 0;
	Next->turn = Current->turn ^ 1;
	Next->material = Current->material;
	Next->pst = Current->pst;
	Next->ply = 0;
	Next->castle_flags = Current->castle_flags;
	Next->ep_square = 0;
	Next->capture = 0;
	if (Current->ep_square) Next->key ^= EPKey[File(Current->ep_square)];
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
	Next->gen_flags = 0;
	Current++;
	nodes++;
}

void undo_null() {
	Current--;
	sp--;
}

template <bool me> int is_legal(int move) {
	int from, to, piece, capture;
	uint64_t u, occ;

    from = From(move);
	to = To(move);
	piece = Board->square[from];
	capture = Board->square[to];
	if (piece == 0) return 0;
	if ((piece & 1) != Current->turn) return 0;
	if (capture) {
		if ((capture & 1) == (piece & 1)) return 0;
		if (capture >= WhiteKing) return 0;
	}
	occ = PieceAll;
	u = Bit(to);
	if (piece >= WhiteLight && piece < WhiteKing) {
	    if ((QMask[from] & u) == 0) return 0;
		if (Between[from][to] & occ) return 0;
	}
	if (IsEP(move)) {
		if (piece >= WhiteKnight) return 0;
		if (Current->ep_square != to) return 0;
		return 1;
	}
	if (IsCastling(move) && Board->square[from] < WhiteKing) return 0;
	if (IsPromotion(move) && Board->square[from] >= WhiteKnight) return 0;
	if (piece == IPawn(me)) {
		if (u & PMove[me][from]) {
            if (capture) return 0;
			if (T(u & Line(me,7)) && !IsPromotion(move)) return 0;
			return 1;
		} else if (to == (from + 2 * Push(me))) {
            if (capture) return 0;
			if (Square(to - Push(me))) return 0;
			if (F(u & Line(me,3))) return 0;
			return 1;
		} else if (u & PAtt[me][from]) {
            if (capture == 0) return 0;
			if (T(u & Line(me,7)) && !IsPromotion(move)) return 0;
			return 1;
		} else return 0;
	} else if (piece == IKing(me)) {
		if (me == White) {
		    if (IsCastling(move)) {
			    if (u & 0x40) {
                    if (((Current->castle_flags) & CanCastle_OO) == 0) return 0;
					if (occ & 0x60) return 0;
					if (Current->att[Black] & 0x70) return 0;
				} else {
					if (((Current->castle_flags) & CanCastle_OOO) == 0) return 0;
					if (occ & 0xE) return 0;
					if (Current->att[Black] & 0x1C) return 0;
				}
				return 1;
			}
		} else {
            if (IsCastling(move)) {
				if (u & 0x4000000000000000) {
                    if (((Current->castle_flags) & CanCastle_oo) == 0) return 0;
					if (occ & 0x6000000000000000) return 0;
					if (Current->att[White] & 0x7000000000000000) return 0;
				} else {
					if (((Current->castle_flags) & CanCastle_ooo) == 0) return 0;
					if (occ & 0x0E00000000000000) return 0;
					if (Current->att[White] & 0x1C00000000000000) return 0;
				}
				return 1;
			}
		}
        if (F(SArea[from] & u)) return 0;
	    if (Current->att[opp] & u) return 0;
		return 1;
	}
	piece = (piece >> 1) - 2;
	if (piece == 0) {
        if (u & NAtt[from]) return 1;
		else return 0;
	} else {
		if (piece <= 2) {
			if (BMask[from] & u) return 1;
		} else if (piece == 3) {
			if (RMask[from] & u) return 1;
		} else return 1;
		return 0;
	}
}

template <bool me> int is_check(int move) { // doesn't detect castling and ep checks
	uint64_t king;
	int from, to, piece, king_sq;

	from = From(move);
	to = To(move);
	king = King(opp);
	king_sq = lsb(king);
	piece = Square(from);
	if (T(Bit(from) & Current->xray[me]) && F(FullLine[king_sq][from] & Bit(to))) return 1;
	if (piece < WhiteKnight) {
		if (PAtt[me][to] & king) return 1;
		if (T(Bit(to) & Line(me, 7)) && T(king & Line(me, 7)) && F(Between[to][king_sq] & PieceAll)) return 1;
	} else if (piece < WhiteLight) {
		if (NAtt[to] & king) return 1;
	} else if (piece < WhiteRook) {
		if (BMask[to] & king) if (F(Between[king_sq][to] & PieceAll)) return 1;
	} else if (piece < WhiteQueen) {
		if (RMask[to] & king) if (F(Between[king_sq][to] & PieceAll)) return 1;
	} else if (piece < WhiteKing) {
		if (QMask[to] & king) if (F(Between[king_sq][to] & PieceAll)) return 1;
	}
	return 0;
}
// Memo: L2889
#endif

template <bool me> int Position::see(int move, int margin) {
	constexpr bool opp = !me;
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
