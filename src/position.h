/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(POSITION_H_INCLUDED)
#define POSITION_H_INCLUDED

#define Check(me) T(Current->att[(me) ^ 1] & King(me))
#define IsIllegal(me,move) ((T(Current->xray[opp] & Bit(From(move))) && F(Bit(To(move)) & FullLine[lsb(King(me))][From(move)])) \
	|| (IsEP(move) && T(Line[Rank(From(move))] & King(me)) && T(Line[Rank(From(move))] & Major(opp)) && \
	T(RookAttacks(lsb(King(me)),PieceAll ^ Bit(From(move)) ^ Bit(Current->ep_square - Push(me))) & Major(opp))))
#define IsRepetition(margin,move) ((margin) > 0 && Current->ply >= 2 && (Current-1)->move == ((To(move) << 6) | From(move)) && F(Square(To(move))) && F((move) & 0xF000))

template <bool me> void do_move(int move);
template <bool me> void undo_move(int move);
void do_null();
void undo_null();
template <bool me> int is_legal(int move);
template <bool me> int is_check(int move);

#endif
