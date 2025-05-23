﻿/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(EVALUATE_H_INCLUDED)
#define EVALUATE_H_INCLUDED

#include <cstdint>

extern const int SeeValue[16];

extern int16_t Delta[16 * 4096];
#define DeltaScore(piece, from, to) Delta[((piece) << 12) | ((from) << 6) | (to)]
#define Delta(from, to) DeltaScore(Square(from), from, to)
#define DeltaM(move) Delta(From(move), To(move))
#define DeltaMarginP(piece, from, to) (DeltaScore(piece, from, to) >= Current->margin)
#define DeltaMargin(from, to) (Delta(from, to) >= Current->margin)

// Memo: L524
extern int Pst[16 * 64];
#define Pst(piece, sq) Pst[((piece) << 6) | (sq)]
extern int MvvLva[16][16]; // [piece][capture]

extern void print_eval();

extern void init_eval();
/// extern void evaluate();

class Position;
extern void evaluate(Position &pos);

#endif
