/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(BITBOARD_H_INCLUDED)
#define BITBOARD_H_INCLUDED

#include <cstdint>
#include <intrin.h>

#include "types.h"

#if defined(SHOGI)
struct alignas(16) bitboard_t {
	union {
		__m128i xmm;
		uint64_t u64[2];
	};
	struct NotBB {
		const __m128i temp;
		// ~bb0 & bb1
		bitboard_t operator& (const bitboard_t& bb) const {
			return bitboard_t{ _mm_andnot_si128(temp, bb.xmm) };
		}
		// 通常のnot
		operator bitboard_t() const {
			constexpr __m128i mask = {-1LL, -1LL }; ///_mm_set1_epi32(0xFFFFFFFF);
			return bitboard_t{ _mm_xor_si128(temp, mask) };
		}
		constexpr explicit NotBB(const bitboard_t& bb) : temp(bb.xmm) {}
	};
	bitboard_t::NotBB operator~() const {
		return bitboard_t::NotBB{*this};
	}

	explicit bitboard_t() {}
	constexpr explicit bitboard_t(__m128i m) : xmm(m) {}
///	constexpr explicit bitboard_t(uint64_t l, uint64_t h) {u64[0] = l; u64[1] = h;}
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
		this->xmm = _mm_andnot_si128(b.temp, xmm);
		return *this;
	}

	bitboard_t operator|(const bitboard_t &b) const { return bitboard_t(*this) |= b; }
	bitboard_t operator^(const bitboard_t &b) const { return bitboard_t(*this) ^= b; }
	bitboard_t operator&(const bitboard_t &b) const { return bitboard_t(*this) &= b; }
	bitboard_t operator&(const NotBB &b) const { return bitboard_t(*this) &= b; }

	bool operator!=(const bitboard_t &b) const
	{
		bitboard_t a(_mm_xor_si128(xmm, b.xmm));
		return !_mm_testz_si128(a.xmm, a.xmm);
	}
	bool operator==(const bitboard_t &b) const
	{
		bitboard_t a(_mm_xor_si128(xmm, b.xmm));
		return _mm_testz_si128(a.xmm, a.xmm);
	}
};
inline bool testz_bb(const bitboard_t &a, const bitboard_t &b) { return _mm_testz_si128(a.xmm, b.xmm); }

#else
using bitboard_t = uint64_t;
inline bool testz_bb(const bitboard_t &a, const bitboard_t &b) { return (a & b) == 0; }
#endif

#if defined(SHOGI)
inline int popcnt(const bitboard_t &bb) { return popcnt64(bb.u64[0]) + popcnt64(bb.u64[1]); }

inline bool Multiple(const bitboard_t &bb)
{
	int n = popcnt(bb);
	return n > 1;
}
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
extern bitboard_t BishopAttacks(int sq, const bitboard_t &occ);
extern bitboard_t RookAttacks(int sq, const bitboard_t &occ);
extern bitboard_t ShiftNW(const bitboard_t &target);
extern bitboard_t ShiftNE(const bitboard_t &target);
extern bitboard_t ShiftSE(const bitboard_t &target);
extern bitboard_t ShiftSW(const bitboard_t &target);
inline bitboard_t ShiftW(int me, const bitboard_t &target) { return ((me) ? ShiftSW(target) : ShiftNW(target)); }
inline bitboard_t ShiftE(int me, const bitboard_t &target) { return ((me) ? ShiftSE(target) : ShiftNE(target)); }
extern bitboard_t ShiftN(const bitboard_t &target);
extern bitboard_t ShiftS(const bitboard_t &target);
inline bitboard_t Shift(int me, const bitboard_t &target) { return ((me) ? ShiftS(target) : ShiftN(target)); }
#else
#define BishopAttacks(sq, occ) (*(BOffsetPointer[sq] + (((BMagicMask[sq] & (occ)) * BMagic[sq]) >> BShift[sq])))
#define RookAttacks(sq, occ) (*(ROffsetPointer[sq] + (((RMagicMask[sq] & (occ)) * RMagic[sq]) >> RShift[sq])))
#define ShiftNW(target) (((target) & (~(File[0] | Line[7]))) << 7)
#define ShiftNE(target) (((target) & (~(File[7] | Line[7]))) << 9)
#define ShiftSE(target) (((target) & (~(File[7] | Line[0]))) >> 7)
#define ShiftSW(target) (((target) & (~(File[0] | Line[0]))) >> 9)
#define ShiftW(me, target) ((me) ? ShiftSW(target) : ShiftNW(target))
#define ShiftE(me, target) ((me) ? ShiftSE(target) : ShiftNE(target))
#define ShiftN(target) ((target) << 8)
#define ShiftS(target) ((target) >> 8)
#define Shift(me, target) ((me) ? ShiftS(target) : ShiftN(target))
#endif

#if defined(SHOGI)
constexpr bitboard_t Bit(uint32_t x)
{
		bitboard_t a(__m128i{(x < 63) ? 1LL << x : 0, (x < 63) ? 0 : 1LL << (x - 63)});
	return a;
}
#else
constexpr uint64_t Bit(int x) { return (1ULL << x); }
#endif
#if defined(SHOGI)
inline void put(bitboard_t &bb, int sq) { bb |= Bit(sq); }
inline void remove(bitboard_t &bb, int sq) { bb &= ~Bit(sq); }
#endif // defined(SHOGI)

#endif // !defined(BITBOARD_H_INCLUDED)
