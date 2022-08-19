/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(BITBOARD_H_INCLUDED)
#define BITBOARD_H_INCLUDED

#include <cstdint>
#include <x86intrin.h>

#if defined(SHOGI)
struct alignas(16) bitboard_t;
struct NotBB {
	const bitboard_t &bb;
	constexpr NotBB(const bitboard_t &a) : bb(a) {}
	operator bitboard_t() const;
};
struct alignas(16) bitboard_t {
	union {
		uint64_t u64[2];
		__m128i xmm;
	};
	constexpr NotBB operator~() const { return NotBB(*this); }

	bitboard_t &operator|=(const bitboard_t &b)
	{
		this->xmm = _mm_or_si128(xmm, b.xmm);
		return *this;
	}
	bitboard_t &operator^=(const bitboard_t &b)
	{
		this->xmm = _mm_xor_si128(xmm, b.xmm);
		return *this;
	}
	bitboard_t &operator&=(const bitboard_t &b)
	{
		this->xmm = _mm_and_si128(xmm, b.xmm);
		return *this;
	}
	bitboard_t &operator&=(const NotBB &b)
	{
		this->xmm = _mm_andnot_si128(b.bb.xmm, xmm);
		return *this;
	}

	bitboard_t operator|(const bitboard_t &b) const { return bitboard_t(*this) |= b; }
	bitboard_t operator^(const bitboard_t &b) const { return bitboard_t(*this) ^= b; }
	bitboard_t operator&(const bitboard_t &b) const { return bitboard_t(*this) &= b; }
	bitboard_t operator&(const NotBB &b) const { return bitboard_t(*this) &= b; }

	bool operator!=(const bitboard_t &b) const
	{
		bitboard_t a = {.xmm = _mm_xor_si128(xmm, b.xmm)};
		return !_mm_testz_si128(a.xmm, a.xmm);
	}
	bool operator==(const bitboard_t &b) const
	{
		bitboard_t a = {.xmm = _mm_xor_si128(xmm, b.xmm)};
		return _mm_testz_si128(a.xmm, a.xmm);
	}
};
#else
using bitboard_t = uint64_t;
#endif

#if defined(SHOGI)
constexpr bitboard_t Empty = {{0ULL, 0ULL}};
constexpr bitboard_t Filled = {{0x7FFFFFFFFFFFFFFFULL, 0x3FFFFULL}};

constexpr bitboard_t FileA = {{0x00000000000001FFULL, 0x00000}};
constexpr bitboard_t Line0 = {{0x0040201008040201ULL, 0x00201}};
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
// clang-format off
constexpr bitboard_t File[9] = {
	{(FileA.u64[0] <<  0), Empty.u64[1]},
	{(FileA.u64[0] <<  9), Empty.u64[1]},
	{(FileA.u64[0] << 18), Empty.u64[1]},
	{(FileA.u64[0] << 27), Empty.u64[1]},
	{(FileA.u64[0] << 36), Empty.u64[1]},
	{(FileA.u64[0] << 45), Empty.u64[1]},
	{(FileA.u64[0] << 54), Empty.u64[1]},
	{Empty.u64[0], (FileA.u64[0] << 0)},
	{Empty.u64[0], (FileA.u64[0] << 9)},
};
constexpr bitboard_t Line[9] = {
	{(Line0.u64[0] <<  0), (Line0.u64[1] << 0)},
	{(Line0.u64[0] <<  1), (Line0.u64[1] << 1)},
	{(Line0.u64[0] <<  2), (Line0.u64[1] << 2)},
	{(Line0.u64[0] <<  3), (Line0.u64[1] << 3)},
	{(Line0.u64[0] <<  4), (Line0.u64[1] << 4)},
	{(Line0.u64[0] <<  5), (Line0.u64[1] << 5)},
	{(Line0.u64[0] <<  6), (Line0.u64[1] << 6)},
	{(Line0.u64[0] <<  7), (Line0.u64[1] << 7)},
	{(Line0.u64[0] <<  8), (Line0.u64[1] << 8)},
};
// clang-format on
constexpr bitboard_t Boundary = {File[0].u64[0] | File[8].u64[0] | Line[0].u64[0] | Line[8].u64[0],
                                 File[0].u64[1] | File[8].u64[1] | Line[0].u64[1] | Line[8].u64[1]};
constexpr bitboard_t Interior = {Filled.u64[0] ^ Boundary.u64[0], Filled.u64[1] ^ Boundary.u64[1]};
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
extern bitboard_t HLine[64];
extern bitboard_t VLine[64];
extern bitboard_t NDiag[64];
extern bitboard_t SDiag[64];
extern bitboard_t RMask[64];
extern bitboard_t BMask[64];
extern bitboard_t QMask[64];
extern bitboard_t BMagicMask[64];
extern bitboard_t RMagicMask[64];
extern bitboard_t NAtt[64];
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
inline void Cut(bitboard_t &bb)
{
	if (bb.u64[0] != 0)
		bb.u64[0] &= bb.u64[0] - 1;
	else
		bb.u64[1] &= bb.u64[1] - 1;
}
constexpr int lsb(const bitboard_t bb)
{
	return (bb.u64[0] != 0) ? __builtin_ctzll(bb.u64[0]) : 64 + __builtin_ctzll(bb.u64[1]);
}
constexpr int msb(const bitboard_t bb)
{
	return (bb.u64[1] != 0) ? 64 + (63 ^ __builtin_clzll(bb.u64[1])) : (63 ^ __builtin_clzll(bb.u64[0]));
}
#endif
#endif // !defined(BITBOARD)