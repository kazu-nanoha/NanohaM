/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(TYPES_H_INCLUDED)
#define TYPES_H_INCLUDED

#include <csetjmp>
#include <cinttypes>

#define Convert(x,type) ((type)(x))

#define Abs(x) ((x) > 0 ? (x) : (-(x)))
#define Sgn(x) ((x) == 0 ? 0 : ((x) > 0 ? 1 : (-1)))
#define Min(x,y) ((x) < (y) ? (x) : (y))
#define Max(x,y) ((x) > (y) ? (x) : (y))
#define Sqr(x) ((x) * (x))
#define T(x) ((x) != 0)
#define F(x) ((x) == 0)
#define Even(x) F((x) & 1)
#define Odd(x) T((x) & 1)
#define Combine(x,y) ((x) | ((y) << 16))
#define Compose(x,y) ((x) + ((y) << 16))
#define Compose16(x,y) Compose((x)/16,(y)/16)
#define Compose64(x,y) Compose((x)/64,(y)/64)
#define Compose256(x,y) Compose((x)/256,(y)/256)
#define Opening(x) Convert((x) & 0xFFFF,int16_t)
#define Endgame(x) ((((x) >> 15) & 1) + Convert((x) >> 16,int16_t))

#define File(x) ((x) & 7)
#define Rank(x) ((x) >> 3)
#define CRank(me,x) ((me) ? (7 - Rank(x)) : Rank(x))
#define NDiag(x) (7 - File(x) + Rank(x))
#define SDiag(x) (File(x) + Rank(x))
#define Dist(x,y) Max(Abs(Rank(x)-Rank(y)),Abs(File(x)-File(y)))
#define VarC(var,me) ((me) ? (var##_b) : (var##_w))
#define PVarC(prefix,var,me) ((me) ? (prefix.var##_b) : (prefix.var##_w))

#define Bit(x) (Convert(1,uint64_t) << (x))
#define Cut(x) (x &= (x) - 1)
#define Multiple(x) T((x) & ((x) - 1))
#define Single(x) F((x) & ((x) - 1))
#define Add(x,b) (x |= Bit(b))

#define From(move) (((move) >> 6) & 0x3f)
#define To(move) ((move) & 0x3f)
#define SetScore(move,score) ((move) = (((move) & 0xFFFF) | ((score) << 16)))

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
#define Empty Convert(0,uint64_t)
#define Filled (~Empty)
#define Interior Convert(0x007E7E7E7E7E7E00,uint64_t)
#define Boundary (~Interior)
#define WhiteArea Convert(0x00000000FFFFFFFF,uint64_t)
#define BlackArea (~WhiteArea)
#define LightArea Convert(0x55AA55AA55AA55AA,uint64_t)
#define DarkArea (~LightArea)
#define FileA Convert(0x0101010101010101,uint64_t)
#define Line0 Convert(0x00000000000000FF,uint64_t)

#define High32(x) ((x) >> 32)
#define Low32(x) (static_cast<uint32_t>(x))

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

#define IsSlider(x) T(0x3FC0 & Bit(x))

#define CanCastle_OO 1
#define CanCastle_oo 2
#define CanCastle_OOO 4
#define CanCastle_ooo 8

#define FlagCastling 0x1000
#define FlagEP 0x2000
#define FlagPKnight 0x4000
#define FlagPLight 0x6000
#define FlagPDark 0x8000
#define FlagPRook 0xA000
#define FlagPQueen 0xC000

#define IsPromotion(move) T((move) & 0xC000)
#define IsCastling(move) T((move) & 0x1000)
#define IsEP(move) (((move) & 0xF000) == 0x2000)
#define Promotion(move,side) ((side) + (((move) & 0xF000) >> 12))

#define BishopAttacks(sq, occ)                             \
    (*(BOffsetPointer[sq] +                                \
        (((BMagicMask[sq] & (occ)) *                       \
            BMagic[sq]) >> BShift[sq])))
#define RookAttacks(sq, occ)                               \
    (*(ROffsetPointer[sq] +                                \
        (((RMagicMask[sq] & (occ)) *                       \
            RMagic[sq]) >> RShift[sq])))

#define QueenAttacks(sq,occ) (BishopAttacks(sq,occ) | RookAttacks(sq,occ))

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
#define TotalMat ((2*(MatWQ+MatBQ)+MatWL+MatBL+MatWD+MatBD+2*(MatWR+MatBR+MatWN+MatBN)+8*(MatWP+MatBP)) + 1)

#define magic_size 107648

#define FlagUnusualMaterial (1 << 30)

// Memo: L280
constexpr int MatCode[16] = {0,0,MatWP,MatBP,MatWN,MatBN,MatWL,MatBL,MatWD,MatBD,MatWR,MatBR,MatWQ,MatBQ,0,0};
constexpr uint64_t File[8] = {FileA,FileA<<1,FileA<<2,FileA<<3,FileA<<4,FileA<<5,FileA<<6,FileA<<7};
constexpr uint64_t Line[8] = {Line0,(Line0<<8),(Line0<<16),(Line0<<24),(Line0<<32),(Line0<<40),(Line0<<48),(Line0<<56)};

///#define opp (1 ^ (me))

#define IPawn(me) (WhitePawn | (me))
#define IKnight(me) (WhiteKnight | (me))
#define ILight(me) (WhiteLight | (me))
#define IDark(me) (WhiteDark | (me))
#define IRook(me) (WhiteRook | (me))
#define IQueen(me) (WhiteQueen | (me))
#define IKing(me) (WhiteKing | (me))



#define ShiftNW(target) (((target) & (~(File[0] | Line[7]))) << 7)
#define ShiftNE(target) (((target) & (~(File[7] | Line[7]))) << 9)
#define ShiftSE(target) (((target) & (~(File[7] | Line[0]))) >> 7)
#define ShiftSW(target) (((target) & (~(File[0] | Line[0]))) >> 9)
#define ShiftW(me,target) ((me) ? ShiftSW(target) : ShiftNW(target))
#define ShiftE(me,target) ((me) ? ShiftSE(target) : ShiftNE(target))
#define ShiftN(target) ((target) << 8)
#define ShiftS(target) ((target) >> 8)
#define Shift(me,target) ((me) ? ShiftS(target) : ShiftN(target))
#define PushW(me) ((me) ? (-9) : (7))
#define PushE(me) ((me) ? (-7) : (9))
#define Push(me) ((me) ? (-8) : (8))
#define Dir(me) ((me) ? (-1) : (1))
#define IsGreater(me,x,y) ((me) ? ((x) < (y)) : ((x) > (y)))

#define Line(me,n) ((me) ? Line[7 - n] : Line[n])
#define Square(sq) Board->square[sq]

// Memo: L339
#define Check(me) T(Current->att[(me) ^ 1] & King(me))



#define IncV(var,x) (me ? (var -= (x)) : (var += (x)))
#define DecV(var,x) IncV(var,-(x))

#define KpkValue 300
#define EvalValue 30000
#define MateValue 32760

extern uint64_t Kpk[2][64][64]; 


// Memo: L408
#define MSBZ(x) ((x) ? msb(x) : 63)
#define LSBZ(x) ((x) ? lsb(x) : 0)
#define NB(me, x) ((me) ? msb(x) : lsb(x))
#define NBZ(me, x) ((me) ? MSBZ(x) : LSBZ(x))

// Memo: L427
#if 0
struct GPosData {
	uint64_t key, pawn_key;
	uint16_t move;
	uint8_t turn, castle_flags, ply, ep_square, piece, capture;
	uint8_t square[64];
	int pst, material;
};
#endif

// Memo: L447
extern int RootList[256];

#define prefetch(a,mode) _mm_prefetch(a,mode)

extern uint64_t Forward[2][8];
extern uint64_t West[8];
extern uint64_t East[8];
extern uint64_t PIsolated[8];
extern uint64_t HLine[64];
extern uint64_t VLine[64];
extern uint64_t NDiag[64];
extern uint64_t SDiag[64];
extern uint64_t RMask[64];
extern uint64_t BMask[64];
extern uint64_t QMask[64];
extern uint64_t BMagicMask[64];
extern uint64_t RMagicMask[64];
extern uint64_t NAtt[64];
extern uint64_t SArea[64];
extern uint64_t DArea[64];
extern uint64_t NArea[64];
extern uint64_t BishopForward[2][64];
extern uint64_t PAtt[2][64];
extern uint64_t PMove[2][64];
extern uint64_t PWay[2][64];
extern uint64_t PSupport[2][64];
extern uint64_t Between[64][64];
extern uint64_t FullLine[64][64];


// Memo: L479
extern uint64_t TurnKey;
extern uint64_t PieceKey[16][64];
extern uint64_t CastleKey[16];
extern uint64_t EPKey[8];
extern uint16_t date;


// Memo: L510
extern uint64_t * MagicAttacks;
struct GMaterial {
	int16_t score;
	uint8_t phase, flags;
	uint8_t mul[2], pieces[2];
};
extern GMaterial * Material;
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
//int TimeLimit1, TimeLimit2, Console, HardwarePopCnt;
extern int Console;

extern int LastTime;
///extern int PVN, Stop, Print, Input = 1, PVHashing = 1, Infinite, MoveTime, SearchMoves, SMPointer, Ponder, Searching, Previous;
extern int PVN, Stop, Print, PVHashing, Infinite, MoveTime, SearchMoves, SMPointer, Ponder, Searching, Previous;
struct GSearchInfo {
	int Bad, Change, Singular, Early, FailLow, FailHigh;
};
extern GSearchInfo CurrentSI[1], BaseSI[1];


// Memo: L550
extern int Aspiration;
#define TimeSingTwoMargin 20
#define TimeSingOneMargin 30


extern int64_t StartTime, InfoTime, CurrTime;
extern uint16_t SMoves[256];	// ToDo: 256の意味
extern jmp_buf Jump, ResetJump;




// SMP
#define MaxPrN 32 // mustn't exceed 32

//int PrN = 1, CPUs = 1, HT = 0, parent = 1, child = 0, WinParId, Id = 0, ResetHash = 1, NewPrN = 0;
extern int PrN, CPUs, parent, Id, ResetHash, NewPrN;

#define ArrayIndex(width,row,column) (((row) * (width)) + (column))
#define Av(x,width,row,column) (*((x) + ArrayIndex(width,row,column)))
#define TrAv(x,w,r,c) Av(x,0,0,(((r)*(2*(w)-(r)+1))/2)+(c))

#define Sa(x,y) Av(x,0,0,y)
#define Ca(x,y) Compose(Av(x,0,0,((y) * 2)),Av(x,0,0,((y) * 2)+1))

// Memo: L777
///enum { PasserOnePiece, PasserOpKingControl, PasserOpMinorControl, PasserOpRookBlock };
///const int PasserSpecial[4] = { // tuner: type=array, var=100, active=0
///	0, 0, 0, 13
///};

// Memo: L783
extern int PasserGeneral[8];
extern int PasserBlocked[8];
extern int PasserFree[8];
extern int PasserSupported[8];
extern int PasserProtected[8];
extern int PasserConnected[8];
extern int PasserOutside[8];
extern int PasserCandidate[8];
extern int PasserClear[8];

// bitboard
typedef uint64_t bitboard_t;


//
void setup_board();
void get_board(const char fen[]);
void move_to_string(int move, char string[]);
int move_from_string(const char string[]);

// ToDo: 適切なところに移動。
class Position;
void send_best_move(Position& pos);

void get_position(char string[]);
void get_time_limit(char string[]);
int time_to_stop(GSearchInfo * SI, int time, int searching);
void check_time(int searching);
void check_time(int time, int searching);
void check_state();


// Memo: L819
///enum { TacticalMajorPawn, TacticalMinorPawn, TacticalMajorMinor, TacticalMinorMinor, TacticalThreat, TacticalDoubleThreat };
///const int Tactical[12] = { // tuner: type=array, var=20, active=0
///	-1, 5, 0, 5, 11, 29, 23, 32, 19, 11, 41, 12
///};

// Memo: L829
///enum { PawnChainLinear, PawnChain, PawnBlocked, PawnFileSpan };
///const int PawnSpecial[8] = { // tuner: type=array, var=10, active=0
///	11, 9, 9, 4, 0, 9, 1, 1
///};


//struct GBoard {
//	uint64_t bb[16];
//	uint8_t square[64];
//};
///extern GBoard Board[1];			// ToDo: DEL
///extern uint64_t Stack[2048];	// ToDo: DEL
///extern int sp, save_sp;			// ToDo: DEL
//extern uint64_t nodes, check_node, check_node_smp;
extern uint64_t nodes, check_node, check_node_smp;

#define FlagSort (1 << 0)
#define FlagNoBcSort (1 << 1)
#if 0
struct GData {
	uint64_t key, pawn_key, eval_key, att[2], patt[2], passer, xray[2], pin[2], threat, mask;
	uint8_t turn, castle_flags, ply, ep_square, capture, gen_flags, piece, stage, mul, dummy;
	int16_t score;
	uint16_t move, killer[3], ref[2];
	int best;
	int material, pst;
	int margin, *start, *current;
	int moves[230];
};
extern GData Data[128];	// [ToDo] 数値の意味を確認する.
extern GData *Current;
#endif
extern const uint64_t BMagic[64];
extern const uint64_t RMagic[64];
extern const int32_t BShift[64];
extern const int32_t BOffset[64];
extern const int32_t RShift[64];
extern const int32_t ROffset[64];
extern uint64_t * BOffsetPointer[64];
extern uint64_t * ROffsetPointer[64];


extern int lsb(uint64_t x);
extern int msb(uint64_t x);
extern int popcnt(uint64_t x);
extern int MinF(int x, int y);
extern int MaxF(int x, int y);
extern double MinF(double x, double y);
extern double MaxF(double x, double y);
template <bool HPopCnt> int popcount(uint64_t x);
extern uint64_t BMagicAttacks(int i, uint64_t occ);
extern uint64_t RMagicAttacks(int i, uint64_t occ);

#endif
