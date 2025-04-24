/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(TYPES_H_INCLUDED)
#define TYPES_H_INCLUDED

#include <csetjmp>
#include <intrin.h>
#include <inttypes.h>

extern int lsb(uint64_t x);
extern int msb(uint64_t x);
inline int popcnt64(const uint64_t x) { return _mm_popcnt_u64(x); }

// bitboard
#include "bitboard.h"

using Move = uint16_t;

constexpr int Sgn(int x) { return (x == 0 ? 0 : (x > 0 ? 1 : (-1))); }
constexpr bool Odd(int x) { return (((x)&1) != 0); }
///#define Combine(x,y) ((x) | ((y) << 16))
#define Compose(x, y) ((x) + ((y) << 16))
///#define Compose64(x,y) Compose((x)/64,(y)/64)
#define Opening(x) (int16_t((x)&0xFFFF))
#define Endgame(x) ((((x) >> 15) & 1) + (int16_t((x) >> 16)))

constexpr int file_of(int sq)
{
#if defined(SHOGI)
	return sq / 9;
#else
	return sq & 7;
#endif
}

constexpr int rank_of(int sq)
{
#if defined(SHOGI)
	return sq % 9;
#else
	return sq >> 3;
#endif
}

constexpr int make_sq(int rank, int file)
{
#if defined(SHOGI)
	return rank + 9 * file;
#else
	return rank * 8 + file;
#endif
}

#define CRank(me, x) ((me) ? (7 - rank_of(x)) : rank_of(x))
#define NDiag(x) (7 - file_of(x) + rank_of(x))
#define SDiag(x) (file_of(x) + rank_of(x))
#define Dist(x, y) std::max(std::abs(rank_of(x) - rank_of(y)), std::abs(file_of(x) - file_of(y)))
#define VarC(var, me) ((me) ? (var##_b) : (var##_w))
#define PVarC(prefix, var, me) ((me) ? (prefix.var##_b) : (prefix.var##_w))
inline void Cut(uint64_t &x) { x &= (x - 1); }
constexpr bool Multiple(int x) { return ((x) & ((x)-1)) != 0; }
constexpr bool Single(int x) { return ((x) & ((x)-1)) == 0; }

#define Add(x, b) (x |= Bit(b))

constexpr int From(Move move) { return (((move) >> 6) & 0x3f); }
constexpr int To(Move move) { return ((move)&0x3f); }
inline void SetScore(int &move, int score) { (move = ((move & 0xFFFF) | (score << 16))); }

// Memo: L299
#define BB(i) Board->bb[i]
#define Pawn(me) (BB(WhitePawn | (me)))
#define Knight(me) (BB(WhiteKnight | (me)))
#define Bishop(me) (BB(WhiteLight | (me)) | BB(WhiteDark | (me)))
#define Rook(me) (BB(WhiteRook | (me)))
#define Queen(me) (BB(WhiteQueen | (me)))
#define King(me) (BB(WhiteKing | (me)))
#define Piece(me) (BB(me))
#define NonPawn(me) (Piece(me) ^ Pawn(me))
#define NonPawnKing(me) (NonPawn(me) ^ King(me))
#define BSlider(me) (Bishop(me) | Queen(me))
#define RSlider(me) (Rook(me) | Queen(me))
#define Major(me) RSlider(me)
#define Minor(me) (Knight(me) | Bishop(me))
#define Slider(me) (BSlider(me) | RSlider(me))
#define PieceAll (Piece(White) | Piece(Black))
#define SliderAll (Slider(White) | Slider(Black))
#define PawnAll (Pawn(White) | Pawn(Black))
#define NonPawnKingAll (NonPawnKing(White) | NonPawnKing(Black))
#define KingPos(me) (lsb(King(me)))

// Memo: L106
constexpr uint32_t High32(uint64_t x) { return static_cast<uint32_t>((x) >> 32); }
constexpr uint32_t Low32(uint64_t x) { return static_cast<uint32_t>(x); }

#define White 0
#define Black 1
#define WhitePawn 2
#define BlackPawn 3
#define WhiteKnight 4
#define BlackKnight 5
#define WhiteLight 6
#define BlackLight 7
#define WhiteDark 8
#define BlackDark 9
#define WhiteRook 10
#define BlackRook 11
#define WhiteQueen 12
#define BlackQueen 13
#define WhiteKing 14
#define BlackKing 15

// clang-format off
enum File_e {
#if defined(SHOGI)
	File_1, File_2, File_3, File_4, File_5, File_6, File_7, File_8, File_9, NFile
#else
	File_A, File_B, File_C, File_D, File_E, File_F, File_G, File_H, NFile
#endif
};
enum Rank_e {
#if defined(SHOGI)
	Rank_A, Rank_B, Rank_C, Rank_D, Rank_E, Rank_F, Rank_G, Rank_H, Rank_I, NRank
#else
	Rank_1, Rank_2, Rank_3, Rank_4, Rank_5, Rank_6, Rank_7, Rank_8, NRank
#endif
};
enum Square_e {
#if defined(SHOGI)
	SQ_1A, SQ_1B, SQ_1C, SQ_1D, SQ_1E, SQ_1F, SQ_1G, SQ_1H, SQ_1I,
	SQ_2A, SQ_2B, SQ_2C, SQ_2D, SQ_2E, SQ_2F, SQ_2G, SQ_2H, SQ_2I,
	SQ_3A, SQ_3B, SQ_3C, SQ_3D, SQ_3E, SQ_3F, SQ_3G, SQ_3H, SQ_3I,
	SQ_4A, SQ_4B, SQ_4C, SQ_4D, SQ_4E, SQ_4F, SQ_4G, SQ_4H, SQ_4I,
	SQ_5A, SQ_5B, SQ_5C, SQ_5D, SQ_5E, SQ_5F, SQ_5G, SQ_5H, SQ_5I,
	SQ_6A, SQ_6B, SQ_6C, SQ_6D, SQ_6E, SQ_6F, SQ_6G, SQ_6H, SQ_6I,
	SQ_7A, SQ_7B, SQ_7C, SQ_7D, SQ_7E, SQ_7F, SQ_7G, SQ_7H, SQ_7I,
	SQ_8A, SQ_8B, SQ_8C, SQ_8D, SQ_8E, SQ_8F, SQ_8G, SQ_8H, SQ_8I,
	SQ_9A, SQ_9B, SQ_9C, SQ_9D, SQ_9E, SQ_9F, SQ_9G, SQ_9H, SQ_9I, SQ_SIZE
#else
	A8, B8, C8, D8, E8, F8, G8, H8,
	A7, B7, C7, D7, E7, F7, G7, H7,
	A6, B6, C6, D6, E6, F6, G6, H6,
	A5, B5, C5, D5, E5, F5, G5, H5,
	A4, B4, C4, D4, E4, F4, G4, H4,
	A3, B3, C3, D3, E3, F3, G3, H3,
	A2, B2, C2, D2, E2, F2, G2, H2,
	A1, B1, C1, D1, E1, F1, G1, H1, SQ_SIZE
#endif
};
// clang-format on

#define FlagPQueen 0xC000

constexpr bool IsPromotion(Move move) { return ((move & 0xC000) != 0); }
constexpr int Promotion(Move move, int side) { return ((side) + (((move)&0xF000) >> 12)); }

#define QueenAttacks(sq, occ) (BishopAttacks(sq, occ) | RookAttacks(sq, occ))

#define MatWQ 1
#define MatBQ 3
#define MatWR (3 * 3)
#define MatBR (3 * 3 * 3)
#define MatWL (3 * 3 * 3 * 3)
#define MatBL (3 * 3 * 3 * 3 * 2)
#define MatWD (3 * 3 * 3 * 3 * 2 * 2)
#define MatBD (3 * 3 * 3 * 3 * 2 * 2 * 2)
#define MatWN (3 * 3 * 3 * 3 * 2 * 2 * 2 * 2)
#define MatBN (3 * 3 * 3 * 3 * 2 * 2 * 2 * 2 * 3)
#define MatWP (3 * 3 * 3 * 3 * 2 * 2 * 2 * 2 * 3 * 3)
#define MatBP (3 * 3 * 3 * 3 * 2 * 2 * 2 * 2 * 3 * 3 * 9)
#define TotalMat                                                                                                       \
	((2 * (MatWQ + MatBQ) + MatWL + MatBL + MatWD + MatBD + 2 * (MatWR + MatBR + MatWN + MatBN) +                      \
	  8 * (MatWP + MatBP)) +                                                                                           \
	 1)

#define magic_size 107648 // 2*64*29*29

#define FlagUnusualMaterial (1 << 30)

// Memo: L280
constexpr int MatCode[16] = {0,     0,     MatWP, MatBP, MatWN, MatBN, MatWL, MatBL,
                             MatWD, MatBD, MatWR, MatBR, MatWQ, MatBQ, 0,     0};

#define IPawn(me) (WhitePawn | (me))
#define IKnight(me) (WhiteKnight | (me))
#define ILight(me) (WhiteLight | (me))
#define IDark(me) (WhiteDark | (me))
#define IRook(me) (WhiteRook | (me))
#define IQueen(me) (WhiteQueen | (me))
#define IKing(me) (WhiteKing | (me))

constexpr int PushW(bool me) { return ((me) ? (-9) : (7)); }
constexpr int PushE(bool me) { return ((me) ? (-7) : (9)); }
constexpr int Push(bool me) { return ((me) ? (-8) : (8)); }
constexpr int Dir(bool me) { return ((me) ? (-1) : (1)); }
#define IsGreater(me, x, y) ((me) ? ((x) < (y)) : ((x) > (y)))

#define Line(me, n) ((me) ? Line[7 - n] : Line[n])

#define IncV(var, x) (me ? (var -= (x)) : (var += (x)))
#define DecV(var, x) IncV(var, -(x))

#define KpkValue 300
#define EvalValue 30000
#define MateValue 32760

extern bitboard_t Kpk[2][64][64];

// Memo: L408
#define MSBZ(x) ((x) ? msb(x) : 63)
#define LSBZ(x) ((x) ? lsb(x) : 0)
#define NB(me, x) ((me) ? msb(x) : lsb(x))
#define NBZ(me, x) ((me) ? MSBZ(x) : LSBZ(x))

// Memo: L447
extern int RootList[256];

#define prefetch(a, mode) _mm_prefetch(a, mode)

#if defined(SHOGI)
constexpr bool valid_rank(int rank) { return (unsigned)rank < 9; }
constexpr bool valid_file(int file) { return (unsigned)file < 9; }
constexpr bool valid_sq(int sq) { return (unsigned)sq < 81; }
#else
constexpr bool valid_rank(int rank) { return (rank & ~7) == 0; }
constexpr bool valid_file(int file) { return (file & ~7) == 0; }
constexpr bool valid_sq(int sq) { return (sq & ~63) == 0; }
#endif

// Memo: L479
extern uint64_t TurnKey;
extern uint64_t PieceKey[16][64];
extern uint16_t date;

// Memo: L510
extern bitboard_t MagicAttacks[magic_size];
struct GMaterial {
	int16_t score;
	uint8_t phase, flags;
	uint8_t mul[2], pieces[2];
};
extern GMaterial Material[];
#define FlagSingleBishop_w (1 << 0)
#define FlagSingleBishop_b (1 << 1)
#define FlagCallEvalEndgame_w (1 << 2)
#define FlagCallEvalEndgame_b (1 << 3)

// Memo: L530
extern char info_string[1024];
extern char pv_string[1024];
extern char score_string[16];
extern char mstring[65536];
extern int MultiPV[256];

// Memo: L538
// int TimeLimit1, TimeLimit2, Console, HardwarePopCnt;
extern int Console;

extern int LastTime;
extern int PVN, Stop, Print, PVHashing, Infinite, MoveTime, Ponder, Searching, Previous;
struct GSearchInfo {
	int Bad, Change, Singular, Early, FailLow, FailHigh;
};
extern GSearchInfo CurrentSI[1], BaseSI[1];

// Memo: L550
extern int Aspiration;
#define TimeSingTwoMargin 20
#define TimeSingOneMargin 30

extern int64_t StartTime, InfoTime, CurrTime;

// 指定した手のみを探索する.
// GUIからgoコマンドのオプションでsearchmovesを指定されたときの制御に使う.
//   SearchMoves   … フラグ.
//   SMPointer     … 指定された手の数.
//   SMoves[]      … 指定された手を保持する.
extern int SearchMoves, SMPointer;
extern uint16_t SMoves[256]; // ToDo: 256の意味
extern jmp_buf Jump, ResetJump;

// SMP
#define MaxPrN 32 // mustn't exceed 32

// int PrN = 1, CPUs = 1, HT = 0, parent = 1, child = 0, WinParId, Id = 0, ResetHash = 1, NewPrN = 0;
extern int PrN, CPUs, parent, Id, ResetHash, NewPrN;

#define ArrayIndex(width, row, column) (((row) * (width)) + (column))
#define Av(x, width, row, column) (*((x) + ArrayIndex(width, row, column)))
#define TrAv(x, w, r, c) Av(x, 0, 0, (((r) * (2 * (w) - (r) + 1)) / 2) + (c))

#define Sa(x, y) Av(x, 0, 0, y)
#define Ca(x, y) Compose(Av(x, 0, 0, ((y)*2)), Av(x, 0, 0, ((y)*2) + 1))

//
void move_to_string(int move, char string[]);
int move_from_string(const char string[]);

// ToDo: 適切なところに移動。
class Position;
void send_best_move(Position &pos);

void get_position(char string[]);
void get_time_limit(char string[]);
int time_to_stop(GSearchInfo *SI, int time, int searching);
void check_time(int searching);
void check_time(int time, int searching);
void check_state();

extern uint64_t nodes, check_node, check_node_smp;

#define FlagSort (1 << 0)
#define FlagNoBcSort (1 << 1)

extern const uint64_t BMagic[64];
extern const uint64_t RMagic[64];
extern const int32_t BShift[64];
extern const int32_t BOffset[64];
extern const int32_t RShift[64];
extern const int32_t ROffset[64];
extern uint64_t *BOffsetPointer[64];
extern uint64_t *ROffsetPointer[64];

extern int MinF(int x, int y);
extern int MaxF(int x, int y);
extern double MinF(double x, double y);
extern double MaxF(double x, double y);
template <bool HPopCnt> int popcount(uint64_t x);
extern uint64_t BMagicAttacks(int i, uint64_t occ);
extern uint64_t RMagicAttacks(int i, uint64_t occ);

#endif
