/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(BBTABLES_H_INCLUDED)
#define BBTABLES_H_INCLUDED

#include <cstdint>
#include <intrin.h>

#include "types.h"
#include "bitboard.h"

#if defined(SHOGI)
constexpr bitboard_t Empty(__m128i{0, 0});
constexpr bitboard_t Filled(__m128i{0x7FFFFFFFFFFFFFFFLL, 0x3FFFFLL});

constexpr bitboard_t FileA(__m128i{0x00000000000001FFLL, 0x00000});
constexpr bitboard_t Line0(__m128i{0x0040201008040201LL, 0x00201LL});
#else
constexpr bitboard_t Empty = 0ULL;
constexpr bitboard_t Filled = ~Empty;
constexpr bitboard_t Interior = 0x007E7E7E7E7E7E00ULL;
constexpr bitboard_t Boundary = ~Interior;
constexpr bitboard_t WhiteArea = 0x00000000FFFFFFFFULL;
constexpr bitboard_t BlackArea = ~WhiteArea;
constexpr bitboard_t LightArea = 0x55AA55AA55AA55AAULL;
constexpr bitboard_t DarkArea = ~LightArea;
constexpr bitboard_t FileA = 0x0101010101010101ULL;
constexpr bitboard_t Line0 = 0x00000000000000FFULL;
#endif

#if defined(SHOGI)
constexpr bitboard_t File[9] = {
	bitboard_t(__m128i{int64_t(0x00000000000001FFLL <<  0), 0}),
	bitboard_t{__m128i{int64_t(0x00000000000001FFLL <<  9), 0}},
	bitboard_t{__m128i{int64_t(0x00000000000001FFLL << 18), 0}},
	bitboard_t{__m128i{int64_t(0x00000000000001FFLL << 27), 0}},
	bitboard_t{__m128i{int64_t(0x00000000000001FFLL << 36), 0}},
	bitboard_t{__m128i{int64_t(0x00000000000001FFLL << 45), 0}},
	bitboard_t{__m128i{int64_t(0x00000000000001FFLL << 54), 0}},
	bitboard_t{__m128i{0, int64_t(0x00000000000001FFLL << 0)}},
	bitboard_t{__m128i{0, int64_t(0x00000000000001FFLL << 9)}},
};

const bitboard_t Line[9] = {
	bitboard_t(__m128i{int64_t(0x0040201008040201LL <<  0), int64_t(0x00201LL << 0)}),
	bitboard_t(__m128i{int64_t(0x0040201008040201LL <<  1), int64_t(0x00201LL << 1)}),
	bitboard_t(__m128i{int64_t(0x0040201008040201LL <<  2), int64_t(0x00201LL << 2)}),
	bitboard_t(__m128i{int64_t(0x0040201008040201LL <<  3), int64_t(0x00201LL << 3)}),
	bitboard_t(__m128i{int64_t(0x0040201008040201LL <<  4), int64_t(0x00201LL << 4)}),
	bitboard_t(__m128i{int64_t(0x0040201008040201LL <<  5), int64_t(0x00201LL << 5)}),
	bitboard_t(__m128i{int64_t(0x0040201008040201LL <<  6), int64_t(0x00201LL << 6)}),
	bitboard_t(__m128i{int64_t(0x0040201008040201LL <<  7), int64_t(0x00201LL << 7)}),
	bitboard_t(__m128i{int64_t(0x0040201008040201LL <<  8), int64_t(0x00201LL << 8)}),
};

// ToDo:
constexpr bitboard_t Boundary(__m128i{int64_t(0x0040201008040201LL <<  8), int64_t(0x00201LL << 8)});
constexpr bitboard_t Interior(__m128i{int64_t(0x0040201008040201LL <<  8), int64_t(0x00201LL << 8)});
#else
constexpr bitboard_t File[8] = {FileA,      FileA << 1, FileA << 2, FileA << 3,
                                FileA << 4, FileA << 5, FileA << 6, FileA << 7};
constexpr bitboard_t Line[8] = {Line0,         (Line0 << 8),  (Line0 << 16), (Line0 << 24),
                                (Line0 << 32), (Line0 << 40), (Line0 << 48), (Line0 << 56)};
#endif // defined(SHOGI)

extern bitboard_t Forward[2][8];
extern bitboard_t West[8];
extern bitboard_t East[8];
extern bitboard_t PIsolated[8];
extern bitboard_t RMask[64];
extern bitboard_t BMask[64];
extern bitboard_t QMask[64];
extern bitboard_t BMagicMask[64];
extern bitboard_t RMagicMask[64];
extern bitboard_t NAtt[64]; // Knightの利き
extern bitboard_t SArea[64];
extern bitboard_t DArea[64];
extern bitboard_t NArea[64];
extern bitboard_t BishopForward[2][64];
extern bitboard_t PAtt[2][64];
extern bitboard_t PMove[2][64];
extern bitboard_t PWay[2][64];
extern bitboard_t PSupport[2][64];
extern bitboard_t Between[64][64];
extern bitboard_t FullLine[64][64];

#if defined(SHOGI)
extern bitboard_t BBSilver_attackB[SQ_SIZE];
extern bitboard_t BBSilver_attackW[SQ_SIZE];
constexpr bitboard_t *BBSilver_attack[2] = {BBSilver_attackB, BBSilver_attackW};
#endif // !defined(BITBOARD)

#endif // !defined(BBTABLES_H_INCLUDED)
