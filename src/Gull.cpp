///#define W32_BUILD
///#undef W32_BUILD

#ifdef W32_BUILD
#define NTDDI_VERSION 0x05010200
#define _WIN32_WINNT 0x0501
#endif

///#ifndef W32_BUILD
///#define HNI
///#undef HNI
///#endif

#include <cinttypes>
#include <cmath>
#include <iostream>
#include <mutex>
#include <thread>

#include <intrin.h>
#include <setjmp.h>

#if defined(_MSC_VER)
#define NOMINMAX
#endif
#include <windows.h>

#include "TT.h"
#include "evaluate.h"
#include "genmove.h"
#include "misc.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "types.h"
#include "uci.h"
#include "bbtables.h"

///#define MP_NPS	// --> search.cpp
/////#undef MP_NPS

///#define TIME_TO_DEPTH
/////#undef TIME_TO_DEPTH

using namespace std;

#if 0 // ToDo: DEL
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
#endif

const uint64_t BMagic[64] = {
    0x0048610528020080, 0x00c4100212410004, 0x0004180181002010, 0x0004040188108502, 0x0012021008003040,
    0x0002900420228000, 0x0080808410c00100, 0x000600410c500622, 0x00c0056084140184, 0x0080608816830050,
    0x00a010050200b0c0, 0x0000510400800181, 0x0000431040064009, 0x0000008820890a06, 0x0050028488184008,
    0x00214a0104068200, 0x004090100c080081, 0x000a002014012604, 0x0020402409002200, 0x008400c240128100,
    0x0001000820084200, 0x0024c02201101144, 0x002401008088a800, 0x0003001045009000, 0x0084200040981549,
    0x0001188120080100, 0x0048050048044300, 0x0008080000820012, 0x0001001181004003, 0x0090038000445000,
    0x0010820800a21000, 0x0044010108210110, 0x0090241008204e30, 0x000c04204004c305, 0x0080804303300400,
    0x00a0020080080080, 0x0000408020220200, 0x0000c08200010100, 0x0010008102022104, 0x0008148118008140,
    0x0008080414809028, 0x0005031010004318, 0x0000603048001008, 0x0008012018000100, 0x0000202028802901,
    0x004011004b049180, 0x0022240b42081400, 0x00c4840c00400020, 0x0084009219204000, 0x000080c802104000,
    0x0002602201100282, 0x0002040821880020, 0x0002014008320080, 0x0002082078208004, 0x0009094800840082,
    0x0020080200b1a010, 0x0003440407051000, 0x000000220e100440, 0x00480220a4041204, 0x00c1800011084800,
    0x000008021020a200, 0x0000414128092100, 0x0000042002024200, 0x0002081204004200};

const uint64_t RMagic[64] = {
    0x00800011400080a6, 0x004000100120004e, 0x0080100008600082, 0x0080080016500080, 0x0080040008000280,
    0x0080020005040080, 0x0080108046000100, 0x0080010000204080, 0x0010800424400082, 0x00004002c8201000,
    0x000c802000100080, 0x00810010002100b8, 0x00ca808014000800, 0x0002002884900200, 0x0042002148041200,
    0x00010000c200a100, 0x00008580004002a0, 0x0020004001403008, 0x0000820020411600, 0x0002120021401a00,
    0x0024808044010800, 0x0022008100040080, 0x00004400094a8810, 0x0000020002814c21, 0x0011400280082080,
    0x004a050e002080c0, 0x00101103002002c0, 0x0025020900201000, 0x0001001100042800, 0x0002008080022400,
    0x000830440021081a, 0x0080004200010084, 0x00008000c9002104, 0x0090400081002900, 0x0080220082004010,
    0x0001100101000820, 0x0000080011001500, 0x0010020080800400, 0x0034010224009048, 0x0002208412000841,
    0x000040008020800c, 0x001000c460094000, 0x0020006101330040, 0x0000a30010010028, 0x0004080004008080,
    0x0024000201004040, 0x0000300802440041, 0x00120400c08a0011, 0x0080006085004100, 0x0028600040100040,
    0x00a0082110018080, 0x0010184200221200, 0x0040080005001100, 0x0004200440104801, 0x0080800900220080,
    0x000a01140081c200, 0x0080044180110021, 0x0008804001001225, 0x00a00c4020010011, 0x00001000a0050009,
    0x0011001800021025, 0x00c9000400620811, 0x0032009001080224, 0x001400810044086a};

const int32_t BShift[64] = {58, 59, 59, 59, 59, 59, 59, 58, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 57, 57, 57, 57,
                            59, 59, 59, 59, 57, 55, 55, 57, 59, 59, 59, 59, 57, 55, 55, 57, 59, 59, 59, 59, 57, 57,
                            57, 57, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 58, 59, 59, 59, 59, 59, 59, 58};

const int32_t BOffset[64] = {0,    64,   96,   128,  160,  192,  224,  256,  320,  352,  384,  416,  448,
                             480,  512,  544,  576,  608,  640,  768,  896,  1024, 1152, 1184, 1216, 1248,
                             1280, 1408, 1920, 2432, 2560, 2592, 2624, 2656, 2688, 2816, 3328, 3840, 3968,
                             4000, 4032, 4064, 4096, 4224, 4352, 4480, 4608, 4640, 4672, 4704, 4736, 4768,
                             4800, 4832, 4864, 4896, 4928, 4992, 5024, 5056, 5088, 5120, 5152, 5184};

const int32_t RShift[64] = {52, 53, 53, 53, 53, 53, 53, 52, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54,
                            54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54,
                            54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 52, 53, 53, 53, 53, 53, 53, 52};

const int32_t ROffset[64] = {5248,  9344,  11392, 13440, 15488, 17536, 19584, 21632, 25728, 27776, 28800,  29824, 30848,
                             31872, 32896, 33920, 35968, 38016, 39040, 40064, 41088, 42112, 43136, 44160,  46208, 48256,
                             49280, 50304, 51328, 52352, 53376, 54400, 56448, 58496, 59520, 60544, 61568,  62592, 63616,
                             64640, 66688, 68736, 69760, 70784, 71808, 72832, 73856, 74880, 76928, 78976,  80000, 81024,
                             82048, 83072, 84096, 85120, 87168, 91264, 93312, 95360, 97408, 99456, 101504, 103552};
uint64_t *BOffsetPointer[64];
uint64_t *ROffsetPointer[64];

#define FlagUnusualMaterial (1 << 30)

#if 0 // Memo
const int MatCode[16] = {0,0,MatWP,MatBP,MatWN,MatBN,MatWL,MatBL,MatWD,MatBD,MatWR,MatBR,MatWQ,MatBQ,0,0};
const uint64_t File[8] = {FileA,FileA<<1,FileA<<2,FileA<<3,FileA<<4,FileA<<5,FileA<<6,FileA<<7};
const uint64_t Line[8] = {Line0,(Line0<<8),(Line0<<16),(Line0<<24),(Line0<<32),(Line0<<40),(Line0<<48),(Line0<<56)};
#endif

/*
general move:
0 - 11: from & to
12 - 15: flags
16 - 23: history
24 - 25: spectial moves: killers, refutations...
26 - 30: MvvLva
delta move:
0 - 11: from & to
12 - 15: flags
16 - 31: int16_t delta + (int16_t)0x4000
*/
/// const int MvvLvaVictim[16] = {0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3};
const int MvvLvaAttacker[16] = {0, 0, 5, 5, 4, 4, 3, 3, 3, 3, 2, 2, 1, 1, 6, 6};
const int MvvLvaAttackerKB[16] = {0, 0, 9, 9, 7, 7, 5, 5, 5, 5, 3, 3, 1, 1, 11, 11};
#define PawnCaptureMvvLva(attacker) (MvvLvaAttacker[attacker])
#define MaxPawnCaptureMvvLva (MvvLvaAttacker[15]) // 6
#define KnightCaptureMvvLva(attacker) (MaxPawnCaptureMvvLva + MvvLvaAttackerKB[attacker])
#define MaxKnightCaptureMvvLva (MaxPawnCaptureMvvLva + MvvLvaAttackerKB[15]) // 17
#define BishopCaptureMvvLva(attacker) (MaxPawnCaptureMvvLva + MvvLvaAttackerKB[attacker] + 1)
#define MaxBishopCaptureMvvLva (MaxPawnCaptureMvvLva + MvvLvaAttackerKB[15] + 1) // 18
#define RookCaptureMvvLva(attacker) (MaxBishopCaptureMvvLva + MvvLvaAttacker[attacker])
#define MaxRookCaptureMvvLva (MaxBishopCaptureMvvLva + MvvLvaAttacker[15]) // 24
#define QueenCaptureMvvLva(attacker) (MaxRookCaptureMvvLva + MvvLvaAttacker[attacker])

#if 0
#define halt_check                                                                                                     \
	if ((Current - Data) >= 126) {                                                                                     \
		evaluate();                                                                                                    \
		return Current->score;                                                                                         \
	}                                                                                                                  \
	if (Current->ply >= 100)                                                                                           \
		return 0;                                                                                                      \
	for (i = 4; i <= Current->ply; i += 2)                                                                             \
		if (Stack[sp - i] == Current->key)                                                                             \
	return 0
#endif

alignas(64) GBoard Board[1];
uint64_t Stack[2048];
int sp, save_sp;
uint64_t nodes, check_node, check_node_smp;

alignas(64) GData Data[128]; // [ToDo] 数値の意味を確認する.
GData *Current = Data;

int RootList[256];

///#define prefetch(a,mode) _mm_prefetch(a,mode)

/// uint64_t * MagicAttacks;
/// GMaterial * Material;
bitboard_t MagicAttacks[magic_size];
GMaterial Material[TotalMat];
///#define FlagSingleBishop_w (1 << 0)
///#define FlagSingleBishop_b (1 << 1)
///#define FlagCallEvalEndgame_w (1 << 2)
///#define FlagCallEvalEndgame_b (1 << 3)

uint64_t TurnKey;
uint64_t PieceKey[16][64];
uint16_t date;

bitboard_t Kpk[2][64][64];

#if 0 // ToDo: DEL
int16_t History[16 * 64];
#define HistoryScore(piece, from, to) History[((piece) << 6) | (to)]
#define HistoryP(piece, from, to)                                                                                      \
	((Convert(HistoryScore(piece, from, to) & 0xFF00, int) / Convert(HistoryScore(piece, from, to) & 0x00FF, int))     \
	 << 16)
#define History(from, to) HistoryP(Square(from), from, to)
#define HistoryM(move) HistoryScore(Square(From(move)), From(move), To(move))
#define HistoryInc(depth) std::min(((depth) >> 1) * ((depth) >> 1), 64)
#define HistoryGood(move)                                                                                              \
	if ((HistoryM(move) & 0x00FF) >= 256 - HistoryInc(depth))                                                          \
		HistoryM(move) = ((HistoryM(move) & 0xFEFE) >> 1) + ((HistoryInc(depth) << 8) | HistoryInc(depth));            \
	else                                                                                                               \
		HistoryM(move) += ((HistoryInc(depth) << 8) | HistoryInc(depth))
#define HistoryBad(move)                                                                                               \
	if ((HistoryM(move) & 0x00FF) >= 256 - HistoryInc(depth))                                                          \
		HistoryM(move) = ((HistoryM(move) & 0xFEFE) >> 1) + HistoryInc(depth);                                         \
	else                                                                                                               \
		HistoryM(move) += HistoryInc(depth)
#endif

GRef Ref[16 * 64];

uint64_t seed = 1;
uint8_t PieceFromChar[256];
uint16_t PV[128];
char info_string[1024];
char pv_string[1024];
char score_string[16];
char mstring[65536];
int MultiPV[256];
int pvp;
int pv_length;
int best_move, best_score;
int TimeLimit1, TimeLimit2, Console, HardwarePopCnt;
int DepthLimit, LastDepth, LastTime, LastValue, LastExactValue, PrevMove, InstCnt;
int64_t LastSpeed;
int PVN, Stop, Print, Input = 1, PVHashing = 1, Infinite, MoveTime, SearchMoves, SMPointer, Ponder, Searching, Previous;
GSearchInfo CurrentSI[1], BaseSI[1];
int Aspiration = 1;
#define TimeSingTwoMargin 20
#define TimeSingOneMargin 30
#define TimeNoPVSCOMargin 60
#define TimeNoChangeMargin 70
#define TimeRatio 120
#define PonderRatio 120
#define MovesTg 30
#define InfoLag 5000
#define InfoDelay 1000
int64_t StartTime, InfoTime, CurrTime;
uint16_t SMoves[256];

jmp_buf Jump, ResetJump;
HANDLE StreamHandle;

// EVAL

const int8_t DistC[8] = {3, 2, 1, 0, 0, 1, 2, 3};
const int8_t RankR[8] = {-3, -2, -1, 0, 1, 2, 3, 4};

/// const int SeeValue[16] = {0, 0, 90, 90, 325, 325, 325, 325, 325, 325, 510, 510, 975, 975, 30000, 30000};
const int PieceType[16] = {0, 0, 0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5};

#define V(x) (x)

// EVAL WEIGHTS

// tuner: start
enum { // tuner: enum
	IMatLinear,
	IMatQuadMe = IMatLinear + 5,
	IMatQuadOpp = IMatQuadMe + 14,
	IBishopPairQuad = IMatQuadOpp + 10,
	IMatSpecial = IBishopPairQuad + 9,
	IPstQuadWeights = IMatSpecial + 20,
	IPstLinearWeights = IPstQuadWeights + 48,
	IPstQuadMixedWeights = IPstLinearWeights + 48,
	IMobilityLinear = IPstQuadMixedWeights + 24,
	IMobilityLog = IMobilityLinear + 8,
	IShelterValue = IMobilityLog + 8,
	IStormQuad = IShelterValue + 15,
	IStormLinear = IStormQuad + 5,
	IStormHof = IStormLinear + 5,
	IPasserQuad = IStormHof + 2,
	IPasserLinear = IPasserQuad + 18,
	IPasserAttDefQuad = IPasserLinear + 18,
	IPasserAttDefLinear = IPasserAttDefQuad + 4,
	IPasserSpecial = IPasserAttDefLinear + 4,
	IIsolated = IPasserSpecial + 4,
	IUnprotected = IIsolated + 10,
	IBackward = IUnprotected + 6,
	IDoubled = IBackward + 4,
	IRookSpecial = IDoubled + 4,
	ITactical = IRookSpecial + 20,
	IKingDefence = ITactical + 12,
	IPawnSpecial = IKingDefence + 8,
	IBishopSpecial = IPawnSpecial + 8,
	IKnightSpecial = IBishopSpecial + 4,
	IPin = IKnightSpecial + 10,
	IKingRay = IPin + 10,
	IKingAttackWeight = IKingRay + 6
};

const int Phase[5] = {0, 325, 325, 510, 975};
#define MaxPhase (16 * Phase[0] + 4 * Phase[1] + 4 * Phase[2] + 4 * Phase[3] + 2 * Phase[4])
#define PhaseMin (2 * Phase[3] + Phase[1] + Phase[2])
#define PhaseMax (MaxPhase - Phase[1] - Phase[2])

const int MatLinear[5] = { // tuner: type=array, var=50, active=0
    3, 0, 3, 19, 0};
// pawn, knight, bishop, rook, queen
const int MatQuadMe[14] = { // tuner: type=array, var=1000, active=0
    -33, 17, -23, -155, -247, 15, 296, -105, -83, -162, 327, 315, -861, -1013};
const int MatQuadOpp[10] = { // tuner: type=array, var=1000, active=0
    -14, 47, -20, -278, 35, 39, 49, 9, -2, 75};
const int BishopPairQuad[9] = { // tuner: type=array, var=1000, active=0
    -38, 164, 99, 246, -84, -57, -184, 88, -186};

enum { MatRB, MatRN, MatQRR, MatQRB, MatQRN, MatQ3, MatBBR, MatBNR, MatNNR, MatM };
const int MatSpecial[20] = { // tuner: type=array, var=30, active=0
    13, -13, 10, -9, 8, 12, 4, 6, 5, 9, -3, -8, -4, 7, 2, 0, 0, -6, 1, 3};

// piece type (6) * direction (4: h center dist, v center dist, diag dist, rank) * phase (2)
const int PstQuadWeights[48] = { // tuner: type=array, var=100, active=0
    -15, -19, -70, -13, 33, -20, 0,  197, -36, -122, 0,  -60, -8, -3, -17, -28, -27, -63, -17,  -7,  14, 0,   -24, -5,
    -64, -2,  0,   -38, -8, 0,   77, 11,  -67, 3,    -4, -92, -2, 12, -13, -42, -62, -84, -175, -42, -2, -17, 40,  -19};
const int PstLinearWeights[48] = { // tuner: type=array, var=500, active=0
    -107, 67,   -115, 83,   -55, 67,  92,   443, -177, 5,    -82, -61,  -106, -104, 273, 130,
    0,    -145, -105, -58,  -99, -37, -133, 14,  -185, -43,  -67, -53,  53,   -65,  174, 134,
    -129, 7,    98,   -231, 107, -40, -27,  311, 256,  -117, 813, -181, 2,    -215, -44, 344};
// piece type (6) * type (2: h * v, h * rank) * phase (2)
const int PstQuadMixedWeights[24] = { // tuner: type=array, var=100, active=0
    14, -6, 1, -4, -8, -2, 4, -4, 1, -7, -12, 0, -2, -1, -5, 4, 5, -10, 0, 4, -2, 5, 4, -2};

// tuner: stop

// END EVAL WEIGHTS

// SMP

int PrN = 1, CPUs = 1, HT = 0, parent = 1, child = 0, WinParId, Id = 0, ResetHash = 1, NewPrN = 0;
HANDLE ChildPr[MaxPrN];
///#define FlagClaimed (1 << 1)
///#define FlagFinished (1 << 2)

/// GSMPI * Smpi;

/// jmp_buf CheckJump;

HANDLE SHARED = NULL, HASH = NULL;

inline unsigned get_num_cpus(void) { return std::thread::hardware_concurrency(); }

// END SMP

int lsb(uint64_t x);
int msb(uint64_t x);
int popcnt(uint64_t x);
int MinF(int x, int y);
int MaxF(int x, int y);
double MinF(double x, double y);
double MaxF(double x, double y);
template <bool HPopCnt> int popcount(uint64_t x);
uint64_t BMagicAttacks(int i, uint64_t occ);
uint64_t RMagicAttacks(int i, uint64_t occ);
uint16_t rand16();
uint64_t rand64();
void init_pst();
void init();

#ifndef W32_BUILD
int lsb(uint64_t x)
{
	unsigned long y;
	_BitScanForward64(&y, x);
	return y;
}

int msb(uint64_t x)
{
	unsigned long y;
	_BitScanReverse64(&y, x);
	return y;
}

int popcnt(uint64_t x)
{
	x = x - ((x >> 1) & 0x5555555555555555);
	x = (x & 0x3333333333333333) + ((x >> 2) & 0x3333333333333333);
	x = (x + (x >> 4)) & 0x0f0f0f0f0f0f0f0f;
	return (x * 0x0101010101010101) >> 56;
}

template <bool HPopCnt> int popcount(uint64_t x) { return HPopCnt ? _mm_popcnt_u64(x) : popcnt(x); }
template int popcount<true>(uint64_t x);
#else
__forceinline int lsb(uint64_t x)
{
	_asm {
		mov eax, dword ptr x[0]
			test eax, eax
			jz l_high
			bsf eax, eax
			jmp l_ret
		l_high : bsf eax, dword ptr x[4]
				 add eax, 20h
			 l_ret :
	}
}

__forceinline int msb(uint64_t x)
{
	_asm {
		mov eax, dword ptr x[4]
			test eax, eax
			jz l_low
			bsr eax, eax
			add eax, 20h
			jmp l_ret
		l_low : bsr eax, dword ptr x[0]
			l_ret :
	}
}

__forceinline int popcnt(uint64_t x)
{
	unsigned int x1, x2;
	x1 = (unsigned int)(x & 0xFFFFFFFF);
	x1 -= (x1 >> 1) & 0x55555555;
	x1 = (x1 & 0x33333333) + ((x1 >> 2) & 0x33333333);
	x1 = (x1 + (x1 >> 4)) & 0x0F0F0F0F;
	x2 = (unsigned int)(x >> 32);
	x2 -= (x2 >> 1) & 0x55555555;
	x2 = (x2 & 0x33333333) + ((x2 >> 2) & 0x33333333);
	x2 = (x2 + (x2 >> 4)) & 0x0F0F0F0F;
	return ((x1 * 0x01010101) >> 24) + ((x2 * 0x01010101) >> 24);
}

template <bool HPopCnt> __forceinline int popcount(uint64_t x)
{
	return HPopCnt ? (__popcnt((int)x) + __popcnt(x >> 32)) : popcnt(x);
}
#endif

int MinF(int x, int y) { return std::min(x, y); }
int MaxF(int x, int y) { return std::max(x, y); }
double MinF(double x, double y) { return std::min(x, y); }
double MaxF(double x, double y) { return std::max(x, y); }

uint16_t rand16()
{
	seed = (seed * 6364136223846793005ULL) + 1442695040888963407ULL;
	return (uint16_t)((seed >> 32) & 0xFFFF);
}

bitboard_t BMagicAttacks(int i, bitboard_t occ)
{
	bitboard_t att = Empty;
	for (bitboard_t u = BMask[i]; u != Empty; Cut(u)) {
		if ((Between[i][lsb(u)] & occ) == Empty)
			att |= Between[i][lsb(u)] | Bit(lsb(u));
	}
	return att;
}

bitboard_t RMagicAttacks(int i, bitboard_t occ)
{
	bitboard_t att = Empty;
	for (bitboard_t u = RMask[i]; u != Empty; Cut(u)) {
		if ((Between[i][lsb(u)] & occ) == Empty)
			att |= Between[i][lsb(u)] | Bit(lsb(u));
	}
	return att;
}

uint64_t rand64()
{
	uint64_t key = uint64_t(rand16());
	key <<= 16;
	key |= uint64_t(rand16());
	key <<= 16;
	key |= uint64_t(rand16());
	key <<= 16;
	return key | uint64_t(rand16());
}

void init_misc()
{
	int i, j, k, l, n;
	bitboard_t u;
	bitboard_t HLine[64];
	bitboard_t VLine[64];
	bitboard_t NDiag[64];
	bitboard_t SDiag[64];

	for (i = 0; i < 64; i++) {
		HLine[i] = VLine[i] = NDiag[i] = SDiag[i] = RMask[i] = BMask[i] = QMask[i] = Empty;
		BMagicMask[i] = RMagicMask[i] = NAtt[i] = SArea[i] = DArea[i] = NArea[i] = Empty;
		PAtt[0][i] = PAtt[1][i] = PMove[0][i] = PMove[1][i] = PWay[0][i] = PWay[1][i] = PSupport[0][i] =
		    PSupport[1][i] = BishopForward[0][i] = BishopForward[1][i] = Empty;
		for (j = 0; j < 64; j++)
			Between[i][j] = FullLine[i][j] = Empty;
	}

	for (i = 0; i < SQ_SIZE; i++)
		for (j = 0; j < SQ_SIZE; j++)
			if (i != j) {
				u = Bit(j);
				if (file_of(i) == file_of(j))
					VLine[i] |= u;
				if (rank_of(i) == rank_of(j))
					HLine[i] |= u;
				if (NDiag(i) == NDiag(j))
					NDiag[i] |= u;
				if (SDiag(i) == SDiag(j))
					SDiag[i] |= u;
				if (Dist(i, j) <= 2) {
					DArea[i] |= u;
					if (Dist(i, j) <= 1)
						SArea[i] |= u;
					if (std::abs(rank_of(i) - rank_of(j)) + std::abs(file_of(i) - file_of(j)) == 3)
						NAtt[i] |= u;
				}
				if (j == i + 8)
					PMove[0][i] |= u;
				if (j == i - 8)
					PMove[1][i] |= u;
				if (std::abs(file_of(i) - file_of(j)) == 1) {
					if (rank_of(j) >= rank_of(i)) {
						PSupport[1][i] |= u;
						if (rank_of(j) - rank_of(i) == 1)
							PAtt[0][i] |= u;
					}
					if (rank_of(j) <= rank_of(i)) {
						PSupport[0][i] |= u;
						if (rank_of(i) - rank_of(j) == 1)
							PAtt[1][i] |= u;
					}
				} else if (file_of(i) == file_of(j)) {
					if (rank_of(j) > rank_of(i))
						PWay[0][i] |= u;
					else
						PWay[1][i] |= u;
				}
			}
	for (i = 0; i < 64; i++) {
		RMask[i] = HLine[i] | VLine[i];
		BMask[i] = NDiag[i] | SDiag[i];
		QMask[i] = RMask[i] | BMask[i];
		BMagicMask[i] = BMask[i] & Interior;
		RMagicMask[i] = RMask[i];
		if (file_of(i) > 0)
			RMagicMask[i] &= ~File[0];
		if (rank_of(i) > 0)
			RMagicMask[i] &= ~Line[0];
		if (file_of(i) < 7)
			RMagicMask[i] &= ~File[7];
		if (rank_of(i) < 7)
			RMagicMask[i] &= ~Line[7];
		for (j = 0; j < 64; j++)
			if ((NAtt[i] & NAtt[j]) != Empty)
				Add(NArea[i], j);
	}
	for (i = 0; i < 8; i++) {
		West[i] = Empty;
		East[i] = Empty;
		Forward[0][i] = Forward[1][i] = Empty;
		PIsolated[i] = Empty;
		for (j = 0; j < 8; j++) {
			if (i < j)
				Forward[0][i] |= Line[j];
			else if (i > j)
				Forward[1][i] |= Line[j];
			if (i < j)
				East[i] |= File[j];
			else if (i > j)
				West[i] |= File[j];
		}
		if (i > 0)
			PIsolated[i] |= File[i - 1];
		if (i < 7)
			PIsolated[i] |= File[i + 1];
	}
	for (i = 0; i < 64; i++) {
		for (u = QMask[i]; u != Empty; Cut(u)) {
			j = lsb(u);
			k = Sgn(rank_of(j) - rank_of(i));
			l = Sgn(file_of(j) - file_of(i));
			for (n = i + 8 * k + l; n != j; n += (8 * k + l))
				Add(Between[i][j], n);
		}
		for (u = BMask[i]; u != Empty; Cut(u)) {
			j = lsb(u);
			FullLine[i][j] = BMask[i] & BMask[j];
		}
		for (u = RMask[i]; u != Empty; Cut(u)) {
			j = lsb(u);
			FullLine[i][j] = RMask[i] & RMask[j];
		}
		BishopForward[0][i] |= PWay[0][i];
		BishopForward[1][i] |= PWay[1][i];
		for (j = 0; j < 64; j++) {
			if (((PWay[1][j] | Bit(j)) & BMask[i] & Forward[0][rank_of(i)]) != Empty)
				BishopForward[0][i] |= Bit(j);
			if (((PWay[0][j] | Bit(j)) & BMask[i] & Forward[1][rank_of(i)]) != Empty)
				BishopForward[1][i] |= Bit(j);
		}
	}

	for (i = 0; i < 16; i++)
		for (j = 0; j < 16; j++) {
			if (j < WhitePawn)
				MvvLva[i][j] = 0;
			else if (j < WhiteKnight)
				MvvLva[i][j] = PawnCaptureMvvLva(i) << 26;
			else if (j < WhiteLight)
				MvvLva[i][j] = KnightCaptureMvvLva(i) << 26;
			else if (j < WhiteRook)
				MvvLva[i][j] = BishopCaptureMvvLva(i) << 26;
			else if (j < WhiteQueen)
				MvvLva[i][j] = RookCaptureMvvLva(i) << 26;
			else
				MvvLva[i][j] = QueenCaptureMvvLva(i) << 26;
		}

	for (i = 0; i < 256; i++)
		PieceFromChar[i] = 0;
	PieceFromChar['B'] = 6;
	PieceFromChar['K'] = 14;
	PieceFromChar['N'] = 4;
	PieceFromChar['P'] = 2;
	PieceFromChar['Q'] = 12;
	PieceFromChar['R'] = 10;
	PieceFromChar['b'] = 7;
	PieceFromChar['k'] = 15;
	PieceFromChar['n'] = 5;
	PieceFromChar['p'] = 3;
	PieceFromChar['q'] = 13;
	PieceFromChar['r'] = 11;

	TurnKey = rand64();
	for (i = 0; i < 16; i++)
		for (j = 0; j < 64; j++) {
			if (i == 0)
				PieceKey[i][j] = 0;
			else
				PieceKey[i][j] = rand64();
		}
	///	for (i = 0; i < 16; i++) LogDist[i] = (int)(10.0 * log(1.01 + (double)i));
}

void init_magic()
{
#if !defined(SHOGI)
	int i;
	uint64_t j;
	int k, index, bits, bit_list[16];
	bitboard_t u;
	for (i = 0; i < 64; i++) {
		bits = 64 - BShift[i];
		for (u = BMagicMask[i], j = 0; u != Empty; Cut(u), j++)
			bit_list[j] = lsb(u);
		for (j = 0; j < Bit(bits); j++) {
			u = Empty;
			for (k = 0; k < bits; k++)
				if (Odd(j >> k))
					Add(u, bit_list[k]);
#ifndef HNI
			index = int(BOffset[i] + ((BMagic[i] * u) >> BShift[i]));
#else
			index = int(BOffset[i] + _pext_u64(u, BMagicMask[i]));
#endif
			MagicAttacks[index] = BMagicAttacks(i, u);
		}
		bits = 64 - RShift[i];
		for (u = RMagicMask[i], j = 0; u != Empty; Cut(u), j++)
			bit_list[j] = lsb(u);
		for (j = 0; j < Bit(bits); j++) {
			u = 0;
			for (k = 0; k < bits; k++)
				if (Odd(j >> k))
					Add(u, bit_list[k]);
#ifndef HNI
			index = int(ROffset[i] + ((RMagic[i] * u) >> RShift[i]));
#else
			index = int(ROffset[i] + _pext_u64(u, RMagicMask[i]));
#endif
			MagicAttacks[index] = RMagicAttacks(i, u);
		}
	}
#endif // !defined(SHOGI)
}

void gen_kpk()
{
	int turn, wp, wk, bk, to, cnt, old_cnt, un;
	bitboard_t bwp, bwk;
	bitboard_t bbk, u;
	uint8_t Kpk_gen[2][64][64][64];

	memset(Kpk_gen, 0, 2 * 64 * 64 * 64);

	cnt = 0;
	old_cnt = 1;
start:
	if (cnt == old_cnt)
		goto end;
	old_cnt = cnt;
	cnt = 0;
	for (turn = 0; turn < 2; turn++) {
		for (wp = 0; wp < 64; wp++) {
			for (wk = 0; wk < 64; wk++) {
				for (bk = 0; bk < 64; bk++) {
					if (Kpk_gen[turn][wp][wk][bk])
						continue;
					cnt++;
					if (wp < 8 || wp >= 56)
						goto set_draw;
					if (wp == wk || wk == bk || bk == wp)
						goto set_draw;
					bwp = Bit(wp);
					bwk = Bit(wk);
					bbk = Bit(bk);
					if ((PAtt[White][wp] & bbk) != Empty) {
						if (turn == White)
							goto set_draw;
						else if ((SArea[wk] & bwp) == Empty)
							goto set_draw;
					}
					un = 0;
					if (turn == Black) {
						u = SArea[bk] & (~(SArea[wk] | PAtt[White][wp]));
						if ((u) == Empty)
							goto set_draw;
						for (; u != Empty; Cut(u)) {
							to = lsb(u);
							if (Kpk_gen[turn ^ 1][wp][wk][to] == 1)
								goto set_draw;
							else if (Kpk_gen[turn ^ 1][wp][wk][to] == 0)
								un++;
						}
						if (!(un))
							goto set_win;
					} else {
						for (u = SArea[wk] & (~(SArea[bk] | bwp)); u != Empty; Cut(u)) {
							to = lsb(u);
							if (Kpk_gen[turn ^ 1][wp][to][bk] == 2)
								goto set_win;
							else if (Kpk_gen[turn ^ 1][wp][to][bk] == 0)
								un++;
						}
						to = wp + 8;
						if (to != wk && to != bk) {
							if (to >= 56) {
								if ((SArea[to] & bbk) == Empty)
									goto set_win;
								if ((SArea[to] & bwk) == Empty)
									goto set_win;
							} else {
								if (Kpk_gen[turn ^ 1][to][wk][bk] == 2)
									goto set_win;
								else if (Kpk_gen[turn ^ 1][to][wk][bk] == 0)
									un++;
								if (to < 24) {
									to += 8;
									if (to != wk && to != bk) {
										if (Kpk_gen[turn ^ 1][to][wk][bk] == 2)
											goto set_win;
										else if (Kpk_gen[turn ^ 1][to][wk][bk] == 0)
											un++;
									}
								}
							}
						}
						if (!(un))
							goto set_draw;
					}
					continue;
				set_draw:
					Kpk_gen[turn][wp][wk][bk] = 1;
					continue;
				set_win:
					Kpk_gen[turn][wp][wk][bk] = 2;
					continue;
				}
			}
		}
	}
	if (cnt)
		goto start;
end:
	for (turn = 0; turn < 2; turn++) {
		for (wp = 0; wp < 64; wp++) {
			for (wk = 0; wk < 64; wk++) {
				Kpk[turn][wp][wk] = Empty;
				for (bk = 0; bk < 64; bk++) {
					if (Kpk_gen[turn][wp][wk][bk] == 2)
						Kpk[turn][wp][wk] |= Bit(bk);
				}
			}
		}
	}
}
void init_pst()
{
	/// ToDo: ここの処理.
#if 0
	int i, j, k, op, eg, index, r, f, d, e, distQ[4], distL[4], distM[2];
	memset(Pst,0,16 * 64 * sizeof(int));

	for (i = 0; i < 64; i++) {
		r = rank_of(i);
		f = file_of(i);
		d = std::abs(f - r);
		e = std::abs(f + r - 7);
		distQ[0] = DistC[f] * DistC[f]; distL[0] = DistC[f];
		distQ[1] = DistC[r] * DistC[r]; distL[1] = DistC[r];
		distQ[2] = RankR[d] * RankR[d] + RankR[e] * RankR[e]; distL[2] = RankR[d] + RankR[e];
		distQ[3] = RankR[r] * RankR[r]; distL[3] = RankR[r];
		distM[0] = DistC[f] * DistC[r]; distM[1] = DistC[f] * RankR[r];
		for (j = 2; j < 16; j += 2) {
			index = PieceType[j];
			op = eg = 0;
			for (k = 0; k < 2; k++) {
				op += Av(PstQuadMixedWeights, 4, index, (k * 2)) * distM[k];
				eg += Av(PstQuadMixedWeights, 4, index, (k * 2) + 1) * distM[k];
			}
			for (k = 0; k < 4; k++) {
				op += Av(PstQuadWeights,8,index,(k * 2)) * distQ[k];
				eg += Av(PstQuadWeights,8,index,(k * 2) + 1) * distQ[k];
				op += Av(PstLinearWeights,8,index,(k * 2)) * distL[k];
				eg += Av(PstLinearWeights,8,index,(k * 2) + 1) * distL[k];
			}
			Pst(j,i) = Compose(op/64, eg/64);
		}
	}

	Pst(WhiteKnight,56) -= Compose(100, 0);
	Pst(WhiteKnight,63) -= Compose(100, 0);
	for (i = 0; i < 64; i++) {
		for (j = 3; j < 16; j+=2) {
			op = Opening(Pst(j-1,63-i));
			eg = Endgame(Pst(j-1,63-i));
			Pst(j,i) = Compose(-op,-eg);
		}
	}
	Current->pst = 0;
	for (i = 0; i < 64; i++)
	if (Square(i)) Current->pst += Pst(Square(i),i);
#endif
}
void calc_material(int index)
{
	int pawns[2], knights[2], light[2], dark[2], rooks[2], queens[2], bishops[2], major[2], minor[2], tot[2], mat[2],
	    mul[2], quad[2], score, phase, me, i = index;
	queens[White] = i % 3;
	i /= 3;
	queens[Black] = i % 3;
	i /= 3;
	rooks[White] = i % 3;
	i /= 3;
	rooks[Black] = i % 3;
	i /= 3;
	light[White] = i % 2;
	i /= 2;
	light[Black] = i % 2;
	i /= 2;
	dark[White] = i % 2;
	i /= 2;
	dark[Black] = i % 2;
	i /= 2;
	knights[White] = i % 3;
	i /= 3;
	knights[Black] = i % 3;
	i /= 3;
	pawns[White] = i % 9;
	i /= 9;
	pawns[Black] = i % 9;
	for (me = 0; me < 2; me++) {
		bishops[me] = light[me] + dark[me];
		major[me] = rooks[me] + queens[me];
		minor[me] = bishops[me] + knights[me];
		tot[me] = 3 * minor[me] + 5 * rooks[me] + 9 * queens[me];
		mat[me] = mul[me] = 32;
		quad[me] = 0;
	}
	score = (SeeValue[WhitePawn] + Av(MatLinear, 0, 0, 0)) * (pawns[White] - pawns[Black]) +
	        (SeeValue[WhiteKnight] + Av(MatLinear, 0, 0, 1)) * (knights[White] - knights[Black]) +
	        (SeeValue[WhiteLight] + Av(MatLinear, 0, 0, 2)) * (bishops[White] - bishops[Black]) +
	        (SeeValue[WhiteRook] + Av(MatLinear, 0, 0, 3)) * (rooks[White] - rooks[Black]) +
	        (SeeValue[WhiteQueen] + Av(MatLinear, 0, 0, 4)) * (queens[White] - queens[Black]) +
	        50 * ((bishops[White] / 2) - (bishops[Black] / 2));
	phase = Phase[PieceType[WhitePawn]] * (pawns[White] + pawns[Black]) +
	        Phase[PieceType[WhiteKnight]] * (knights[White] + knights[Black]) +
	        Phase[PieceType[WhiteLight]] * (bishops[White] + bishops[Black]) +
	        Phase[PieceType[WhiteRook]] * (rooks[White] + rooks[Black]) +
	        Phase[PieceType[WhiteQueen]] * (queens[White] + queens[Black]);
	Material[index].phase = std::min((std::max(phase - PhaseMin, 0) * 128) / (PhaseMax - PhaseMin), 128);

	int special = 0;
	for (me = 0; me < 2; me++) {
		const int opp = (1 ^ me);
		if (queens[me] == queens[opp]) {
			if (rooks[me] - rooks[opp] == 1) {
				if (knights[me] == knights[opp] && bishops[opp] - bishops[me] == 1)
					IncV(special, Ca(MatSpecial, MatRB));
				else if (bishops[me] == bishops[opp] && knights[opp] - knights[me] == 1)
					IncV(special, Ca(MatSpecial, MatRN));
				else if (knights[me] == knights[opp] && bishops[opp] - bishops[me] == 2)
					DecV(special, Ca(MatSpecial, MatBBR));
				else if (bishops[me] == bishops[opp] && knights[opp] - knights[me] == 2)
					DecV(special, Ca(MatSpecial, MatNNR));
				else if (bishops[opp] - bishops[me] == 1 && knights[opp] - knights[me] == 1)
					DecV(special, Ca(MatSpecial, MatBNR));
			} else if (rooks[me] == rooks[opp] && minor[me] - minor[opp] == 1)
				IncV(special, Ca(MatSpecial, MatM));
		} else if (queens[me] - queens[opp] == 1) {
			if (rooks[opp] - rooks[me] == 2 && minor[opp] - minor[me] == 0)
				IncV(special, Ca(MatSpecial, MatQRR));
			else if (rooks[opp] - rooks[me] == 1 && knights[opp] == knights[me] && bishops[opp] - bishops[me] == 1)
				IncV(special, Ca(MatSpecial, MatQRB));
			else if (rooks[opp] - rooks[me] == 1 && knights[opp] - knights[me] == 1 && bishops[opp] == bishops[me])
				IncV(special, Ca(MatSpecial, MatQRN));
			else if ((major[opp] + minor[opp]) - (major[me] + minor[me]) >= 2)
				IncV(special, Ca(MatSpecial, MatQ3));
		}
	}
	score += (Opening(special) * Material[index].phase + Endgame(special) * (128 - (int)Material[index].phase)) / 128;

	for (me = 0; me < 2; me++) {
		const int opp = (1 ^ me);
		quad[me] += pawns[me] * (pawns[me] * TrAv(MatQuadMe, 5, 0, 0) + knights[me] * TrAv(MatQuadMe, 5, 0, 1) +
		                         bishops[me] * TrAv(MatQuadMe, 5, 0, 2) + rooks[me] * TrAv(MatQuadMe, 5, 0, 3) +
		                         queens[me] * TrAv(MatQuadMe, 5, 0, 4));
		quad[me] += knights[me] * (knights[me] * TrAv(MatQuadMe, 5, 1, 0) + bishops[me] * TrAv(MatQuadMe, 5, 1, 1) +
		                           rooks[me] * TrAv(MatQuadMe, 5, 1, 2) + queens[me] * TrAv(MatQuadMe, 5, 1, 3));
		quad[me] += bishops[me] * (bishops[me] * TrAv(MatQuadMe, 5, 2, 0) + rooks[me] * TrAv(MatQuadMe, 5, 2, 1) +
		                           queens[me] * TrAv(MatQuadMe, 5, 2, 2));

		quad[me] += rooks[me] * (rooks[me] * TrAv(MatQuadMe, 5, 3, 0) + queens[me] * TrAv(MatQuadMe, 5, 3, 1));
		quad[me] += pawns[me] * (knights[opp] * TrAv(MatQuadOpp, 4, 0, 0) + bishops[opp] * TrAv(MatQuadOpp, 4, 0, 1) +
		                         rooks[opp] * TrAv(MatQuadOpp, 4, 0, 2) + queens[opp] * TrAv(MatQuadOpp, 4, 0, 3));
		quad[me] += knights[me] * (bishops[opp] * TrAv(MatQuadOpp, 4, 1, 0) + rooks[opp] * TrAv(MatQuadOpp, 4, 1, 1) +
		                           queens[opp] * TrAv(MatQuadOpp, 4, 1, 2));
		quad[me] += bishops[me] * (rooks[opp] * TrAv(MatQuadOpp, 4, 2, 0) + queens[opp] * TrAv(MatQuadOpp, 4, 2, 1));
		quad[me] += rooks[me] * queens[opp] * TrAv(MatQuadOpp, 4, 3, 0);

		if (bishops[me] >= 2)
			quad[me] += pawns[me] * Av(BishopPairQuad, 0, 0, 0) + knights[me] * Av(BishopPairQuad, 0, 0, 1) +
			            rooks[me] * Av(BishopPairQuad, 0, 0, 2) + queens[me] * Av(BishopPairQuad, 0, 0, 3) +
			            pawns[opp] * Av(BishopPairQuad, 0, 0, 4) + knights[opp] * Av(BishopPairQuad, 0, 0, 5) +
			            bishops[opp] * Av(BishopPairQuad, 0, 0, 6) + rooks[opp] * Av(BishopPairQuad, 0, 0, 7) +
			            queens[opp] * Av(BishopPairQuad, 0, 0, 8);
	}
	score += (quad[White] - quad[Black]) / 100;

	for (me = 0; me < 2; me++) {
		const int opp = (1 ^ me);
		if (tot[me] - tot[opp] <= 3) {
			if (!pawns[me]) {
				if (tot[me] <= 3)
					mul[me] = 0;
				if (tot[me] == tot[opp] && major[me] == major[opp] && minor[me] == minor[opp])
					mul[me] = major[me] + minor[me] <= 2 ? 0 : (major[me] + minor[me] <= 3 ? 16 : 32);
				else if (minor[me] + major[me] <= 2) {
					if (bishops[me] < 2)
						mat[me] = (bishops[me] && rooks[me]) ? 8 : 1;
					else if (bishops[opp] + rooks[opp] >= 1)
						mat[me] = 1;
					else
						mat[me] = 32;
				} else if (tot[me] - tot[opp] < 3 && minor[me] + major[me] - minor[opp] - major[opp] <= 1)
					mat[me] = 4;
				else if (minor[me] + major[me] <= 3)
					mat[me] = 8 * (1 + bishops[me]);
				else
					mat[me] = 8 * (2 + bishops[me]);
			}
			if (pawns[me] <= 1) {
				mul[me] = std::min(28, mul[me]);
				if (rooks[me] == 1 && queens[me] + minor[me] == 0 && rooks[opp] == 1)
					mat[me] = std::min(23, mat[me]);
			}
		}
		if (!major[me]) {
			if (!minor[me]) {
				if (!tot[me] && pawns[me] < pawns[opp])
					DecV(score, (pawns[opp] - pawns[me]) * SeeValue[WhitePawn]);
			} else if (minor[me] == 1) {
				if (pawns[me] <= 1 && minor[opp] >= 1)
					mat[me] = 1;
				if (bishops[me] == 1) {
					if (minor[opp] == 1 && bishops[opp] == 1 && light[me] != light[opp]) {
						mul[me] = std::min(mul[me], 15);
						if (pawns[me] - pawns[opp] <= 1)
							mul[me] = std::min(mul[me], 11);
					}
				}
			} else if (!pawns[me] && knights[me] == 2 && !bishops[me]) {
				if (!tot[opp] && pawns[opp])
					mat[me] = 6;
				else
					mul[me] = 0;
			}
		}
		if (!mul[me])
			mat[me] = 0;
		if (mat[me] <= 1 && tot[me] != tot[opp])
			mul[me] = std::min(mul[me], 8);
	}
	if (bishops[White] == 1 && bishops[Black] == 1 && light[White] != light[Black]) {
		mul[White] = std::min(mul[White], 24 + 2 * (knights[Black] + major[Black]));
		mul[Black] = std::min(mul[Black], 24 + 2 * (knights[White] + major[White]));
	} else if (!minor[White] && !minor[Black] && major[White] == 1 && major[Black] == 1 &&
	           rooks[White] == rooks[Black]) {
		mul[White] = std::min(mul[White], 25);
		mul[Black] = std::min(mul[Black], 25);
	}
	for (me = 0; me < 2; me++) {
		Material[index].mul[me] = mul[me];
		Material[index].pieces[me] = major[me] + minor[me];
	}
	if (score > 0)
		score = (score * mat[White]) / 32;
	else
		score = (score * mat[Black]) / 32;
	Material[index].score = score;
	for (me = 0; me < 2; me++) {
		const int opp = (1 ^ me);
		if (major[me] == 0 && minor[me] == bishops[me] && minor[me] <= 1)
			Material[index].flags |= VarC(FlagSingleBishop, me);
		if (((major[me] == 0 || minor[me] == 0) && major[me] + minor[me] <= 1) || major[opp] + minor[opp] == 0 ||
		    (!pawns[me] && major[me] == rooks[me] && major[me] == 1 && minor[me] == bishops[me] && minor[me] == 1 &&
		     rooks[opp] == 1 && !minor[opp] && !queens[opp]))
			Material[index].flags |= VarC(FlagCallEvalEndgame, me);
	}
}

void init_material()
{
	memset(Material, 0, TotalMat * sizeof(GMaterial));
	for (int index = 0; index < TotalMat; index++)
		calc_material(index);
}

void init()
{
	///	init_shared();
	init_misc();
	if (parent)
		init_magic();
#if 0 // ToDo
	for (int i = 0; i < 64; i++) {
		BOffsetPointer[i] = MagicAttacks + BOffset[i];
		ROffsetPointer[i] = MagicAttacks + ROffset[i];
	}
#endif
	gen_kpk();
	init_pst();
	init_eval();
	if (parent)
		init_material();
	USI::init();
	TT.init();
}

void move_to_string(int move, char string[])
{
	int pos = 0;
	string[pos++] = ((move >> 6) & 7) + 'a';
	string[pos++] = ((move >> 9) & 7) + '1';
	string[pos++] = (move & 7) + 'a';
	string[pos++] = ((move >> 3) & 7) + '1';
	if (IsPromotion(move)) {
		if ((move & 0xF000) == FlagPQueen)
			string[pos++] = 'q';
	}
	string[pos] = 0;
}

int move_from_string(const char string[])
{
	int from, to, move;
	from = ((string[1] - '1') * 8) + (string[0] - 'a');
	to = ((string[3] - '1') * 8) + (string[2] - 'a');
	move = (from << 6) | to;
	if (string[4] != 0) {
		if (string[4] == 'q')
			move |= FlagPQueen;
	}
	return move;
}

// -->search.cpp
////void pick_pv()
////template <bool me> int draw_in_pv()

// -->position.cpp
////template <bool me> void do_move(int move)
////template <bool me> void undo_move(int move)
////void do_null()
////void undo_null()
////template <bool me> int is_legal(int move)
////template <bool me> int is_check(int move)  // doesn't detect castling and ep checks

// -->search.cpp
////void hash_high(int value, int depth)
////void hash_low(int move, int value, int depth)
////void hash_exact(int move, int value, int depth, int exclusion, int ex_depth, int knodes)
////template <bool pv> __forceinline int extension(int move, int depth)

// -->genmove.cpp
////void sort(int * start, int * finish)

// -->search.cpp
////void sort_moves(int * start, int * finish)

// -->genmove.cpp
////__forceinline int pick_move()
////template <bool me> void gen_next_moves()
////template <bool me, bool root> int get_move()
////template <bool me> int see(int move, int margin)
////template <bool me> void gen_root_moves()
////template <bool me> int * gen_captures(int * list)
////template <bool me> int * gen_evasions(int * list)
////void mark_evasions(int * list)
////template <bool me> int * gen_quiet_moves(int * list)
////template <bool me> int * gen_checks(int * list)
////template <bool me> int * gen_delta_moves(int * list)

// -->search.cpp
////template <bool me> int singular_extension(int ext, int prev_ext, int margin_one, int margin_two, int depth, int
/// killer) /template <bool me> __forceinline void capture_margin(int alpha, int &score) /template <bool me, bool pv>
/// int q_search(int alpha, int beta, int depth, int flags) /template <bool me, bool pv> int q_evasion(int alpha, int
/// beta, int depth, int flags) /void send_position(GPos * Pos) /void retrieve_board(GPos * Pos) /void
/// retrieve_position(GPos * Pos, int copy_stack) /void halt_all(GSP * Sp, int locked) /void halt_all(int from, int to)
/// /void init_sp(GSP * Sp, int alpha, int beta, int depth, int pv, int singular, int height) /template <bool me> int
/// smp_search(GSP * Sp) /template <bool me, bool exclusion> int search(int beta, int depth, int flags) /template <bool
/// me, bool exclusion> int search_evasion(int beta, int depth, int flags) /template <bool me, bool root> int
/// pv_search(int alpha, int beta, int depth, int flags) /template <bool me> void root() /template <bool me> int
/// multipv(int depth) /void send_pv(int depth, int alpha, int beta, int score) /void send_best_move()

void get_position(char string[])
{
#if 0 // ToDo: ちゃんと考える。
	const char * fen;
    char * moves;
    const char * ptr;
    int move, move1 = 0;

    fen = strstr(string,"fen ");
    moves = strstr(string,"moves ");
    if (fen != NULL) get_board(fen+4);
    else get_board("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
	PrevMove = 0;
    if (moves != NULL) {
        ptr = moves+6;
        while (*ptr != 0) {
            pv_string[0] = *ptr++;
            pv_string[1] = *ptr++;
            pv_string[2] = *ptr++;
            pv_string[3] = *ptr++;
            if (*ptr == 0 || *ptr == ' ') pv_string[4] = 0;
            else { 
				pv_string[4] = *ptr++; 
				pv_string[5] = 0; 
			}
			evaluate();
            move = move_from_string(pv_string);
			PrevMove = move1;
			move1 = move;
            if (Current->turn) do_move<1>(move);
	        else do_move<0>(move);
			memcpy(Data,Current,sizeof(GData));
			Current = Data;
            while (*ptr == ' ') ptr++;
        }
    }
	memcpy(Stack, Stack + sp - Current->ply, (Current->ply + 1) * sizeof(uint64_t));
	sp = Current->ply;
#endif
}

void get_time_limit(char string[])
{
	const char *ptr;
	int i, time, inc, wtime, btime, winc, binc, moves, pondering, movetime = 0, exp_moves = MovesTg - 1;

	Infinite = 1;
	MoveTime = 0;
	SearchMoves = 0;
	SMPointer = 0;
	pondering = 0;
	TimeLimit1 = 0;
	TimeLimit2 = 0;
	wtime = btime = 0;
	winc = binc = 0;
	moves = 0;
	Stop = 0;
	DepthLimit = 128;
	ptr = strtok(string, " ");
	for (ptr = strtok(NULL, " "); ptr != NULL; ptr = strtok(NULL, " ")) {
		if (!strcmp(ptr, "binc")) {
			ptr = strtok(NULL, " ");
			binc = atoi(ptr);
			Infinite = 0;
		} else if (!strcmp(ptr, "btime")) {
			ptr = strtok(NULL, " ");
			btime = atoi(ptr);
			Infinite = 0;
		} else if (!strcmp(ptr, "depth")) {
			ptr = strtok(NULL, " ");
			DepthLimit = 2 * atoi(ptr) + 2;
			Infinite = 1;
		} else if (!strcmp(ptr, "infinite")) {
			Infinite = 1;
		} else if (!strcmp(ptr, "movestogo")) {
			ptr = strtok(NULL, " ");
			moves = atoi(ptr);
			Infinite = 0;
		} else if (!strcmp(ptr, "winc")) {
			ptr = strtok(NULL, " ");
			winc = atoi(ptr);
			Infinite = 0;
		} else if (!strcmp(ptr, "wtime")) {
			ptr = strtok(NULL, " ");
			wtime = atoi(ptr);
			Infinite = 0;
		} else if (!strcmp(ptr, "movetime")) {
			ptr = strtok(NULL, " ");
			movetime = atoi(ptr);
			MoveTime = 1;
			Infinite = 0;
		} else if (!strcmp(ptr, "searchmoves")) {
			if (!(SearchMoves)) {
				for (i = 0; i < 256; i++)
					SMoves[i] = 0;
			}
			SearchMoves = 1;
			ptr += 12;
			while (ptr != NULL && ptr[0] >= 'a' && ptr[0] <= 'h' && ptr[1] >= '1' && ptr[1] <= '8') {
				pv_string[0] = *ptr++;
				pv_string[1] = *ptr++;
				pv_string[2] = *ptr++;
				pv_string[3] = *ptr++;
				if (*ptr == 0 || *ptr == ' ')
					pv_string[4] = 0;
				else {
					pv_string[4] = *ptr++;
					pv_string[5] = 0;
				}
				SMoves[SMPointer] = move_from_string(pv_string);
				SMPointer++;
				ptr = strtok(NULL, " ");
			}
		} else if (!strcmp(ptr, "ponder"))
			pondering = 1;
	}
	if (pondering)
		Infinite = 1;
	if (Current->turn == White) {
		time = wtime;
		inc = winc;
	} else {
		time = btime;
		inc = binc;
	}
	if (moves)
		moves = std::max(moves - 1, 1);
	int time_max = std::max(time - std::min(1000, time / 2), 0);
	int nmoves;
	if (moves)
		nmoves = moves;
	else {
		nmoves = MovesTg - 1;
		if (Current->ply > 40)
			nmoves += std::min(Current->ply - 40, (100 - Current->ply) / 2);
		exp_moves = nmoves;
	}
	TimeLimit1 = std::min(time_max, (time_max + (std::min(exp_moves, nmoves) * inc)) / std::min(exp_moves, nmoves));
	TimeLimit2 =
	    std::min(time_max, (time_max + (std::min(exp_moves, nmoves) * inc)) / std::min(3, std::min(exp_moves, nmoves)));
	TimeLimit1 = std::min(time_max, (TimeLimit1 * TimeRatio) / 100);
	if (Ponder)
		TimeLimit1 = (TimeLimit1 * PonderRatio) / 100;
	if (MoveTime) {
		TimeLimit2 = movetime;
		TimeLimit1 = TimeLimit2 * 100;
	}
	InfoTime = StartTime = get_time();
	Searching = 1;
	///	if (MaxPrN > 1) SET_BIT_64(Smpi->searching, 0);
	if (!(Infinite))
		PVN = 1;
	if (Current->turn == White)
		root<0>();
	else
		root<1>();
}

int time_to_stop(GSearchInfo *SI, int time, int searching)
{
	if (Infinite)
		return 0;
	if (time > TimeLimit2)
		return 1;
	if (searching)
		return 0;
	if (2 * time > TimeLimit2 && !(MoveTime))
		return 1;
	if (SI->Bad)
		return 0;
	if (time > TimeLimit1)
		return 1;
	if (SI->Change || SI->FailLow)
		return 0;
	if (time * 100 > TimeLimit1 * TimeNoChangeMargin)
		return 1;
	if (!(SI->Early))
		return 0;
	if (time * 100 > TimeLimit1 * TimeNoPVSCOMargin)
		return 1;
	if (SI->Singular < 1)
		return 0;
	if (time * 100 > TimeLimit1 * TimeSingOneMargin)
		return 1;
	if (SI->Singular < 2)
		return 0;
	if (time * 100 > TimeLimit1 * TimeSingTwoMargin)
		return 1;
	return 0;
}

void check_time(int searching)
{
	///	while (!Stop && input()) uci();
	int Time;
	if (Stop)
		goto jump;
	CurrTime = get_time();
	Time = int(CurrTime - StartTime);
	if (Print && Time > InfoLag && CurrTime - InfoTime > InfoDelay) {
		InfoTime = CurrTime;
		if (info_string[0]) {
			fprintf(stdout, "%s", info_string);
			info_string[0] = 0;
			fflush(stdout);
		}
	}
	if (time_to_stop(CurrentSI, Time, searching))
		goto jump;
	return;
jump:
	Stop = 1;
	longjmp(Jump, 1);
}

void check_time(int time, int searching)
{
	///	while (!Stop && input()) uci();
	int Time;
	if (Stop)
		goto jump;
	CurrTime = get_time();
	Time = int(CurrTime - StartTime);
	if (Print && Time > InfoLag && CurrTime - InfoTime > InfoDelay) {
		InfoTime = CurrTime;
		if (info_string[0]) {
			fprintf(stdout, "%s", info_string);
			info_string[0] = 0;
			fflush(stdout);
		}
	}
	if (time_to_stop(CurrentSI, time, searching))
		goto jump;
	return;
jump:
	Stop = 1;
	longjmp(Jump, 1);
}

#if 0 // ToDo: ちゃんと考える。
void check_state() {
	GSP *Sp, *Spc;
	int n, nc, score, best, pv, alpha, beta, new_depth, r_depth, ext, move, value;
	GMove * M;

	if (parent) {
		for (uint64_t u = TEST_RESET(Smpi->fail_high); u; Cut(u)) {
			Sp = &Smpi->Sp[lsb(u)];
			LOCK(Sp->lock);
			if (Sp->active && Sp->finished) {
				UNLOCK(Sp->lock);
				longjmp(Sp->jump, 1);
			}
			UNLOCK(Sp->lock);
		}
		return;
	}

start:
	if (TEST_RESET_BIT(Smpi->stop, Id)) longjmp(CheckJump, 1);
	if (Smpi->searching & Bit(Id)) return;
	if (!(Smpi->searching & 1)) {
		Sleep(1);
		return;
	}
	while ((Smpi->searching & 1) && !Smpi->active_sp) _mm_pause();
	while ((Smpi->searching & 1) && !(Smpi->searching & Bit(Id - 1))) _mm_pause();

	Sp = NULL; best = -0x7FFFFFFF;
	for (uint64_t u = Smpi->active_sp; u; Cut(u)) {
		Spc = &Smpi->Sp[lsb(u)];
		if (!Spc->active || Spc->finished || Spc->lock) continue;
		for (nc = Spc->current + 1; nc < Spc->move_number; nc++) if (!(Spc->move[nc].flags & FlagClaimed)) break;
		if (nc < Spc->move_number) score = 1024 * 1024 + 512 * 1024 * (Spc->depth >= 20) + 128 * 1024 * (!(Spc->split))
			+ ((Spc->depth + 2 * Spc->singular) * 1024) - (((16 * 1024) * (nc - Spc->current)) / nc);
		else continue;
		if (score > best) {
			best = score;
			Sp = Spc;
			n = nc;
		}
	}

	if (Sp == NULL) goto start;
	if (!Sp->active || Sp->finished || (Sp->move[n].flags & FlagClaimed) || n <= Sp->current || n >= Sp->move_number) goto start;
	if (Sp->lock) goto start;

	LOCK(Sp->lock);
	if (!Sp->active || Sp->finished || (Sp->move[n].flags & FlagClaimed) || n <= Sp->current || n >= Sp->move_number) {
		UNLOCK(Sp->lock);
		goto start;
	}

	M = &Sp->move[n];
	M->flags |= FlagClaimed;
	M->id = Id;
	Sp->split |= Bit(Id);
	pv = Sp->pv;
	alpha = Sp->alpha;
	beta = Sp->beta;
	new_depth = M->reduced_depth;
	r_depth = M->research_depth;
	ext = M->ext;
	move = M->move;

	Current = Data;
	retrieve_position(Sp->Pos, 1);
	evaluate();
	SET_BIT_64(Smpi->searching, Id);
	UNLOCK(Sp->lock);

	if (setjmp(CheckJump)) {
		ZERO_BIT_64(Smpi->searching, Id);
		return;
	}
	if (Current->turn == White) {
		do_move<0>(move);
		if (pv) {
			value = -search<1, 0>(-alpha, new_depth, FlagNeatSearch | ExtFlag(ext));
			if (value > alpha) value = -pv_search<1, 0>(-beta, -alpha, r_depth, ExtFlag(ext));
		} else {
			value = -search<1, 0>(1 - beta, new_depth, FlagNeatSearch | ExtFlag(ext));
			if (value >= beta && new_depth < r_depth) value = -search<1, 0>(1 - beta, r_depth, FlagNeatSearch | FlagDisableNull | ExtFlag(ext));
		}
		undo_move<0>(move);
	} else {
		do_move<1>(move);
		if (pv) {
			value = -search<0, 0>(-alpha, new_depth, FlagNeatSearch | ExtFlag(ext));
			if (value > alpha) value = -pv_search<0, 0>(-beta, -alpha, r_depth, ExtFlag(ext));
		} else {
			value = -search<0, 0>(1 - beta, new_depth, FlagNeatSearch | ExtFlag(ext));
			if (value >= beta && new_depth < r_depth) value = -search<0, 0>(1 - beta, r_depth, FlagNeatSearch | FlagDisableNull | ExtFlag(ext));
		}
		undo_move<1>(move);
	}

	LOCK(Sp->lock);
	ZERO_BIT_64(Smpi->searching, Id);
	if (TEST_RESET_BIT(Smpi->stop, Id)) {
		UNLOCK(Sp->lock);
		return;
	}
	M->flags |= FlagFinished;
	if (value > Sp->alpha) {
		Sp->alpha = std::min(value, beta);
		Sp->best_move = move;
		if (value >= beta) {
			Sp->finished = 1;
			SET_BIT_64(Smpi->fail_high, (int)(Sp - Smpi->Sp));
		}
	}
	UNLOCK(Sp->lock);
}
#endif

HANDLE CreateChildProcess(int child_id)
{
	char name[1024];
	TCHAR szCmdline[1024];
	PROCESS_INFORMATION piProcInfo;
	STARTUPINFO siStartInfo;
	BOOL bSuccess = FALSE;

	ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION));
	ZeroMemory(&siStartInfo, sizeof(STARTUPINFO));
	ZeroMemory(szCmdline, 1024 * sizeof(TCHAR));
	ZeroMemory(name, 1024);

	siStartInfo.cb = sizeof(STARTUPINFO);
	siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

	GetModuleFileName(NULL, name, 1024);
	sprintf(szCmdline, " child %d %d", WinParId, child_id);

	bSuccess = CreateProcess(TEXT(name), TEXT(szCmdline), NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, NULL, &siStartInfo,
	                         &piProcInfo);

	if (bSuccess) {
		CloseHandle(piProcInfo.hThread);
		return piProcInfo.hProcess;
	} else {
		fprintf(stdout, "Error %ld\n", GetLastError());
		return NULL;
	}
}

int main(int argc, char *argv[])
{
	DWORD p;

	if (argc >= 2)
		if (!memcmp(argv[1], "child", 5)) {
			child = 1;
			parent = 0;
			WinParId = atoi(argv[2]);
			Id = atoi(argv[3]);
		}

	int CPUInfo[4] = {-1};
	///	__cpuid(CPUInfo, 1);
	///	HardwarePopCnt = (CPUInfo[2] >> 23) & 1;
	HardwarePopCnt = 1;

	if (parent) {
		if (((CPUInfo[3] >> 28) & 1) &&
		    GetProcAddress(GetModuleHandle(TEXT("kernel32")), "GetLogicalProcessorInformation") != NULL) {
			SYSTEM_LOGICAL_PROCESSOR_INFORMATION syslogprocinfo[1];
			p = sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
#ifndef W32_BUILD
			GetLogicalProcessorInformation(syslogprocinfo, &p);
			if (syslogprocinfo->ProcessorCore.Flags == 1)
				HT = 1;
#endif
		}
		WinParId = GetProcessId(GetCurrentProcess());
		HANDLE JOB = CreateJobObject(NULL, NULL);
		JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli = {0};
		jeli.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
		SetInformationJobObject(JOB, JobObjectExtendedLimitInformation, &jeli, sizeof(jeli));
		AssignProcessToJobObject(JOB, GetCurrentProcess());
		if (MaxPrN > 1) {
			PrN = get_num_cpus();
		}
	}
	init();

	StreamHandle = GetStdHandle(STD_INPUT_HANDLE);
	Console = GetConsoleMode(StreamHandle, &p);
	if (Console) {
		SetConsoleMode(StreamHandle, p & (~(ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT)));
		FlushConsoleInputBuffer(StreamHandle);
	}

	setbuf(stdout, NULL);
	setbuf(stdin, NULL);
	setvbuf(stdout, NULL, _IONBF, 0);
	setvbuf(stdin, NULL, _IONBF, 0);
	fflush(NULL);

#if 0 // ToDo
reset_jump:
	if (parent) {
		if (setjmp(ResetJump)) {
			for (i = 1; i < PrN; i++) TerminateProcess(ChildPr[i], 0);
			for (i = 1; i < PrN; i++) {
				WaitForSingleObject(ChildPr[i], INFINITE);
				CloseHandle(ChildPr[i]);
			}
			Smpi->searching = Smpi->active_sp = Smpi->stop = 0;
			for (i = 0; i < MaxSplitPoints; i++) Smpi->Sp->active = Smpi->Sp->claimed = 0;
				
			Smpi->hash_size = hash_size;
			if (NewPrN) Smpi->PrN = PrN = NewPrN;
			goto reset_jump;
		}
		Smpi->hash_size = hash_size;
		Smpi->PrN = PrN;
	} else {
		hash_size = Smpi->hash_size;
		PrN = Smpi->PrN;
	}
#endif
	///	if (ResetHash) init_hash();
	if (ResetHash)
		TT.init();
		///	init_search(0);

#if 0 // ToDo: ちゃんと考える。
	if (child) while (true) check_state();
#endif
	//	if (parent) for (i = 1; i < PrN; i++) ChildPr[i] = CreateChildProcess(i);

	Threads.init();

	USI::loop(argc, argv);
	Threads.set_size(0);

	return 0;
}
