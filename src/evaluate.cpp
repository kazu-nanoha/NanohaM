/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#include "evaluate.h"
#include "TT.h"
#include "position.h"
#include "types.h"
#include <cinttypes>
#include <cmath>
#include <cstdio>

int Pst[16 * 64];
int MvvLva[16][16]; // [piece][capture]

// Memo: L523
int16_t Delta[16 * 4096];

const int SeeValue[16] = {0, 0, 90, 90, 325, 325, 325, 325, 325, 325, 510, 510, 975, 975, 30000, 30000};

// Memo: L1331
void print_eval()
{
}

// Memo: L2420
void init_eval()
{
}

void evaluate([[maybe_unused]] Position &pos)
{
	const auto turn = Current->turn;
	Current->score = 0;
	if (turn)
		Current->score = -Current->score;
}
