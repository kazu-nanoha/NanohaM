/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#include <cinttypes>
#include <cstdio>
#include <cmath>
#include "types.h"
#include "TT.h"
#include "position.h"
#include "evaluate.h"

// Memo: L275
#define FlagUnusualMaterial (1 << 30)

#define Square(sq) Board->square[sq]

#define Compose16(x,y) Compose((x)/16,(y)/16)
#define Compose256(x,y) Compose((x)/256,(y)/256)


// Memo: L480
///uint64_t * MagicAttacks;
///GMaterial * Material;
#define FlagSingleBishop_w (1 << 0)
#define FlagSingleBishop_b (1 << 1)
#define FlagCallEvalEndgame_w (1 << 2)
#define FlagCallEvalEndgame_b (1 << 3)

int Pst[16 * 64];
int MvvLva[16][16]; // [piece][capture]


// Memo: L502
///uint64_t Kpk[2][64][64]; 

// Memo: L523
int16_t Delta[16 * 4096];

#define DeltaScore(piece,from,to) Delta[((piece) << 12) | ((from) << 6) | (to)]

#define UpdateDelta if (!(Current->capture) && (Current->move != 0) && !(Current->move & 0xE000) && Current > Data) { \
	if (DeltaScore(Current->piece,From(Current->move),To(Current->move)) <= -Current->score - ((Current - 1)->score)) \
	DeltaScore(Current->piece,From(Current->move),To(Current->move)) = -Current->score - ((Current - 1)->score); \
	else DeltaScore(Current->piece,From(Current->move),To(Current->move))--; }


// Memo: L606
// EVAL

const int SeeValue[16] = {0, 0, 90, 90, 325, 325, 325, 325, 325, 325, 510, 510, 975, 975, 30000, 30000};
const int PieceType[16] = {0, 0, 0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5};


// Memo: L739
// piece type (4) * phase (2)
const int MobilityLinear[8] = { // tuner: type=array, var=300, active=0
	328, 171, 311, 102, 284, 164, 155, 288
};
const int MobilityLog[8] = { // tuner: type=array, var=500, active=0
	485, -21, 388, 389, -168, 313, 438, -276
};
int Mobility[4][32];

// file type (3) * distance from 2d rank/open (5)
const int ShelterValue[15] = {  // tuner: type=array, var=10, active=0
	2, 9, 11, 0, 0, 12, 18, 11, 0, 2, 24, 7, 8, 0, 0
};
int16_t Shelter[3][8];

enum { StormBlockedMul, StormShelterAttMul, StormConnectedMul, StormOpenMul, StormFreeMul };
const int StormQuad[5] = { // tuner: type=array, var=250, active=0
	126, 328, 463, 215, 89
};
const int StormLinear[5] = { // tuner: type=array, var=500, active=0
	83, 156, 438, 321, 12
};
enum { StormHofValue, StormOfValue };
const int StormHof[2] = { // tuner: type=array, var=20, active=1
	0, 22
};
int16_t StormBlocked[4];
int16_t StormShelterAtt[4];
int16_t StormConnected[4];
int16_t StormOpen[4];
int16_t StormFree[4];

// type (7: general, blocked, free, supported, protected, connected, outside, candidate, clear) * phase (2)
const int PasserQuad[18] = { // tuner: type=array, var=50, active=0
	19, 13, 21, 3, -24, 126, 0, 65, 32, 56, 27, -5, 32, -16, 13, 4, 1, 1
};
const int PasserLinear[18] = { // tuner: type=array, var=200, active=0
	41, 2, 111, 86, 178, 113, 202, 15, -61, 21, 93, 166, 86, 92, 27, 34, -18, -7
};
// type (2: att, def) * scaling (2: linear, log) 
const int PasserAttDefQuad[4] = { // tuner: type=array, var=500, active=0
	191, 51, 83, 19
};
const int PasserAttDefLinear[4] = { // tuner: type=array, var=500, active=0
	634, 4, 233, 66
};
enum { PasserOnePiece, PasserOpKingControl, PasserOpMinorControl, PasserOpRookBlock };
const int PasserSpecial[4] = { // tuner: type=array, var=100, active=0
	0, 0, 0, 13
};

uint8_t LogDist[16];
int PasserGeneral[8];
int PasserBlocked[8];
int PasserFree[8];
int PasserSupported[8];
int PasserProtected[8];
int PasserConnected[8];
int PasserOutside[8];
int PasserCandidate[8];
int PasserClear[8];
int16_t PasserAtt[8];
int16_t PasserDef[8];
int16_t PasserAttLog[8];
int16_t PasserDefLog[8];



// Memo: L797
enum { IsolatedOpen, IsolatedClosed, IsolatedBlocked, IsolatedDoubledOpen, IsolatedDoubledClosed };
const int Isolated[10] = { // tuner: type=array, var=10, active=0
	6, 6, 8, 2, -8, 0, -1, 10, 7, 9
};
enum { UpBlocked, PasserTarget, ChainRoot };
const int Unprotected[6] = { // tuner: type=array, var=10, active=0
	4, 5, -5, -1, 9, -1
};
enum { BackwardOpen, BackwardClosed };
const int Backward[4] = { // tuner: type=array, var=10, active=0
	17, 10, 4, 1
};
enum { DoubledOpen, DoubledClosed };
const int Doubled[4] = { // tuner: type=array, var=10, active=0
	3, 0, 1, 0
};

enum { RookHof, RookHofWeakPAtt, RookOf, RookOfOpen, RookOfMinorFixed, RookOfMinorHaging, RookOfKingAtt, Rook7th, Rook7thK8th, Rook7thDoubled };
const int RookSpecial[20] = { // tuner: type=array, var=10, active=0
	8, 0, 2, 0, 11, 8, -1, 2, -1, -1, 14, -1, 5, -5, -5, 0, -6, 8, -7, 31
};

enum { TacticalMajorPawn, TacticalMinorPawn, TacticalMajorMinor, TacticalMinorMinor, TacticalThreat, TacticalDoubleThreat };
const int Tactical[12] = { // tuner: type=array, var=20, active=0
	-1, 5, 0, 5, 11, 29, 23, 32, 19, 11, 41, 12
};

enum { KingDefKnight, KingDefBishop, KingDefRook, KingDefQueen };
const int KingDefence[8] = { // tuner: type=array, var=5, active=0
	2, 0, 0, 1, 0, 0, 4, 0
};

enum { PawnChainLinear, PawnChain, PawnBlocked, PawnFileSpan };
const int PawnSpecial[8] = { // tuner: type=array, var=10, active=0
	11, 9, 9, 4, 0, 9, 1, 1
};

enum { BishopNonForwardPawn, BishopPawnBlock };
const int BishopSpecial[4] = { // tuner: type=array, var=5, active=0
	0, 0, 0, 3
};

const uint64_t Outpost[2] = { 0x00007E7E3C000000ULL, 0x0000003C7E7E0000ULL };
enum { KnightOutpost, KnightOutpostProtected, KnightOutpostPawnAtt, KnightOutpostBishopAtt, KnightOutpostKingAtt };
const int KnightSpecial[10] = { // tuner: type=array, var=10, active=0
	11, 7, 23, 0, 13, 6, 1, 5, 26, 6
};

enum { WeakPin, StrongPin, ThreatPin, SelfPawnPin, SelfPiecePin };
const int Pin[10] = { // tuner: type=array, var=20, active=0
	21, 39, 6, 80, 45, 29, 8, 9, 48, 27
};

enum { QKingRay, RKingRay, BKingRay };
const int KingRay[6] = { // tuner: type=array, var=20, active=0
	4, 8, -4, 11, 11, -3
};

const int KingAttackWeight[7] = { // tuner: type=array, var=20, active=0
	17, 14, 22, 45, 48, 64, 64
};
#define KingNAttack Compose(1, Av(KingAttackWeight, 0, 0, 0))
#define KingBAttack Compose(1, Av(KingAttackWeight, 0, 0, 1))
#define KingRAttack Compose(1, Av(KingAttackWeight, 0, 0, 2))
#define KingQAttack Compose(1, Av(KingAttackWeight, 0, 0, 3))
#define KingAttack Compose(1, 0)
#define KingAttackSquare Av(KingAttackWeight, 0, 0, 4)
#define KingNoMoves Av(KingAttackWeight, 0, 0, 5)
#define KingShelterQuad Av(KingAttackWeight, 0, 0, 6)

const int KingAttackScale[16] = { 0, 1, 4, 9, 16, 25, 36, 49, 64, 64, 64, 64, 64, 64, 64, 64 };

#if 0
// Memo: L1331
void print_eval() {
	int i, j;
	FILE * f = fopen("eval.txt", "w");
	fprintf(f, "Pst\n");
	for (j = 2; j < 16; j += 2) {
		if (j == 8) continue;
		fprintf(f, "%d:\n", j);
		for (i = 0; i < 64; i++) {
			fprintf(f, "(%d,%d), ", Opening(Pst(j, i)), Endgame(Pst(j, i)));
			if ((i + 1) % 8 == 0) fprintf(f, "\n");
		}
	}
	fprintf(f, "Mobility\n");
	for (j = 0; j < 4; j++) {
		fprintf(f, "%d:\n", j);
		for (i = 0; i < 32; i++) fprintf(f, "(%d,%d), ", Opening(Mobility[j][i]), Endgame(Mobility[j][i]));
		fprintf(f, "\n");
	}
	fprintf(f, "PasserGeneral\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserGeneral[i]), Endgame(PasserGeneral[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserBlocked\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserBlocked[i]), Endgame(PasserBlocked[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserFree\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserFree[i]), Endgame(PasserFree[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserSupported\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserSupported[i]), Endgame(PasserSupported[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserProtected\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserProtected[i]), Endgame(PasserProtected[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserConnected\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserConnected[i]), Endgame(PasserConnected[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserOutside\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserOutside[i]), Endgame(PasserOutside[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserCandidate\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserCandidate[i]), Endgame(PasserCandidate[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserClear\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserClear[i]), Endgame(PasserClear[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserAtt\n");
	for (i = 0; i < 8; i++) fprintf(f, "%d, ", PasserAtt[i]);
	fprintf(f, "\n");

	fprintf(f, "PasserDef\n");
	for (i = 0; i < 8; i++) fprintf(f, "%d, ", PasserDef[i]);
	fprintf(f, "\n");

	fprintf(f, "PasserAttLog\n");
	for (i = 0; i < 8; i++) fprintf(f, "%d, ", PasserAttLog[i]);
	fprintf(f, "\n");

	fprintf(f, "PasserDefLog\n");
	for (i = 0; i < 8; i++) fprintf(f, "%d, ", PasserDefLog[i]);
	fprintf(f, "\n");

	fprintf(f, "StormBlocked\n");
	for (i = 0; i < 4; i++) fprintf(f, "%d, ", StormBlocked[i]);
	fprintf(f, "\n");

	fprintf(f, "StormShelterAtt\n");
	for (i = 0; i < 4; i++) fprintf(f, "%d, ", StormShelterAtt[i]);
	fprintf(f, "\n");

	fprintf(f, "StormConnected\n");
	for (i = 0; i < 4; i++) fprintf(f, "%d, ", StormConnected[i]);
	fprintf(f, "\n");

	fprintf(f, "StormOpen\n");
	for (i = 0; i < 4; i++) fprintf(f, "%d, ", StormOpen[i]);
	fprintf(f, "\n");

	fprintf(f, "StormFree\n");
	for (i = 0; i < 4; i++) fprintf(f, "%d, ", StormFree[i]);
	fprintf(f, "\n");

	fclose(f);
}


// Memo: L2420
void init_eval() {
	int i, j, k, index;
	memset(Mobility,0,4 * 32 * sizeof(int));
	for (i = 0; i < 4; i++) for (j = 0; j < 32; j++) {
		index = i * 2;
		double op = (double)(Av(MobilityLinear,8,0,index) * j) + log(1.01 + (double)j) * (double)(Av(MobilityLog,8,0,index));
		index = i * 2 + 1;
		double eg = (double)(Av(MobilityLinear,8,0,index) * j) + log(1.01 + (double)j) * (double)(Av(MobilityLog,8,0,index));
		Mobility[i][j] = Compose((int)(op/64.0),(int)(eg/64.0));
	}
	
	for (i = 0; i < 3; i++) for (j = 7; j >= 0; j--) {
		Shelter[i][j] = 0;
		if (j > 1) for (k = 1; k < std::min(j, 5); k++) Shelter[i][j] += Av(ShelterValue, 0, 0, (i * 5) + k - 1);
		if (!j) Shelter[i][j] = Shelter[i][7] + Av(ShelterValue, 0, 0, (i * 5) + 4);
	}

	for (i = 0; i < 4; i++) {
		StormBlocked[i] = ((Sa(StormQuad, StormBlockedMul) * i * i) + (Sa(StormLinear, StormBlockedMul) * (i + 1))) / 100;
		StormShelterAtt[i] = ((Sa(StormQuad, StormShelterAttMul) * i * i) + (Sa(StormLinear, StormShelterAttMul) * (i + 1))) / 100;
		StormConnected[i] = ((Sa(StormQuad, StormConnectedMul) * i * i) + (Sa(StormLinear, StormConnectedMul) * (i + 1))) / 100;
		StormOpen[i] = ((Sa(StormQuad, StormOpenMul) * i * i) + (Sa(StormLinear, StormOpenMul) * (i + 1))) / 100;
		StormFree[i] = ((Sa(StormQuad, StormFreeMul) * i * i) + (Sa(StormLinear, StormFreeMul) * (i + 1))) / 100;
	}

	for (i = 0; i < 8; i++) {
		int l = std::max(i - 2, 0);
		int q = l * l;
		PasserGeneral[i] = Compose16(Av(PasserQuad, 2, 0, 0) * q + Av(PasserLinear, 2, 0, 0) * l, Av(PasserQuad, 2, 0, 1) * q + Av(PasserLinear, 2, 0, 1) * l);
		PasserBlocked[i] = Compose16(Av(PasserQuad, 2, 1, 0) * q + Av(PasserLinear, 2, 1, 0) * l, Av(PasserQuad, 2, 1, 1) * q + Av(PasserLinear, 2, 1, 1) * l);
		PasserFree[i] = Compose16(Av(PasserQuad, 2, 2, 0) * q + Av(PasserLinear, 2, 2, 0) * l, Av(PasserQuad, 2, 2, 1) * q + Av(PasserLinear, 2, 2, 1) * l);
		PasserSupported[i] = Compose16(Av(PasserQuad, 2, 3, 0) * q + Av(PasserLinear, 2, 3, 0) * l, Av(PasserQuad, 2, 3, 1) * q + Av(PasserLinear, 2, 3, 1) * l);
		PasserProtected[i] = Compose16(Av(PasserQuad, 2, 4, 0) * q + Av(PasserLinear, 2, 4, 0) * l, Av(PasserQuad, 2, 4, 1) * q + Av(PasserLinear, 2, 4, 1) * l);
		PasserConnected[i] = Compose16(Av(PasserQuad, 2, 5, 0) * q + Av(PasserLinear, 2, 5, 0) * l, Av(PasserQuad, 2, 5, 1) * q + Av(PasserLinear, 2, 5, 1) * l);
		PasserOutside[i] = Compose16(Av(PasserQuad, 2, 6, 0) * q + Av(PasserLinear, 2, 6, 0) * l, Av(PasserQuad, 2, 6, 1) * q + Av(PasserLinear, 2, 6, 1) * l);
		PasserCandidate[i] = Compose16(Av(PasserQuad, 2, 7, 0) * q + Av(PasserLinear, 2, 7, 0) * l, Av(PasserQuad, 2, 7, 1) * q + Av(PasserLinear, 2, 7, 1) * l);
		PasserClear[i] = Compose16(Av(PasserQuad, 2, 8, 0) * q + Av(PasserLinear, 2, 8, 0) * l, Av(PasserQuad, 2, 8, 1) * q + Av(PasserLinear, 2, 8, 1) * l);

		PasserAtt[i] = Av(PasserAttDefQuad, 2, 0, 0) * q + Av(PasserAttDefLinear, 2, 0, 0) * l;
		PasserDef[i] = Av(PasserAttDefQuad, 2, 1, 0) * q + Av(PasserAttDefLinear, 2, 1, 0) * l;
		PasserAttLog[i] = Av(PasserAttDefQuad, 2, 0, 1) * q + Av(PasserAttDefLinear, 2, 0, 1) * l;
		PasserDefLog[i] = Av(PasserAttDefQuad, 2, 1, 1) * q + Av(PasserAttDefLinear, 2, 1, 1) * l;
	}
	for (i = 0; i < 16; i++) LogDist[i] = (int)(10.0 * log(1.01 + (double)i));
}


// Memo: L2687
template <bool me> int krbkrx() {
	constexpr bool opp = !me;
	if (King(opp) & Interior) return 1;
	return 16;
}
template <bool me> int kpkx() {
	constexpr bool opp = !me;
	uint64_t u;
	if (me == White) u = Kpk[turn][lsb(Pawn(White))][lsb(King(White))] & Bit(lsb(King(Black)));
	else u = Kpk[turn ^ 1][63 - lsb(Pawn(Black))][63 - lsb(King(Black))] & Bit(63 - lsb(King(White)));
	if (u) return 32;
	else if (Piece(opp) ^ King(opp)) return 1;
	else return 0;
}
template <bool me> int knpkx() {
	constexpr bool opp = !me;
	if (Pawn(me) & Line(me, 6) & (File[0] | File[7])) {
		int sq = lsb(Pawn(me));
		if (SArea[sq] & King(opp) & (Line(me, 6) | Line(me, 7))) return 0;
		if (Square(sq + Push(me)) == IKing(me) && (SArea[lsb(King(me))] && SArea[lsb(King(opp))] & Line(me, 7))) return 0;
	} else if (Pawn(me) & Line(me, 5) & (File[0] | File[7])) {
		int sq = lsb(Pawn(me));
		if (Square(sq + Push(me)) == IPawn(opp)) {
			if (SArea[sq + Push(me)] & King(opp) & Line(me, 7)) return 0;
			if ((SArea[sq + Push(me)] & SArea[lsb(King(opp))] & Line(me, 7)) && (!(NAtt[sq + Push(me)] & Knight(me)) || turn == opp)) return 0;
		}
	}
	return 32;
}
template <bool me> int krpkrx() {
	constexpr bool opp = !me;
	int mul = 32;
	int sq = lsb(Pawn(me));
	int rrank = CRank(me, sq);
	int o_king = lsb(King(opp));
	int o_rook = lsb(Rook(opp));
	int m_king = lsb(King(me));
	int add_mat = T(Piece(opp) ^ King(opp) ^ Rook(opp));
	int clear = !(add_mat) || !((PWay[opp][sq] | PIsolated[file_of(sq)]) & Forward[opp][rank_of(sq + Push(me))] & (Piece(opp) ^ King(opp) ^ Rook(opp)));

	if (!clear) return 32;
	if (!add_mat && !(Pawn(me) & (File[0] | File[7]))) {
		int m_rook = lsb(Rook(me));
		if (CRank(me, o_king) < CRank(me, m_rook) && CRank(me, m_rook) < rrank && CRank(me, m_king) >= rrank - 1 && CRank(me, m_king) > CRank(me, m_rook)
			&& ((SArea[m_king] & Pawn(me)) || (turn == me && std::abs(file_of(sq) - file_of(m_king)) <= 1 && std::abs(rrank - CRank(me, m_king)) <= 2))) return 128;
		if (SArea[m_king] & Pawn(me)) {
			if (rrank >= 4) {
				if ((file_of(sq) < file_of(m_rook) && file_of(m_rook) < file_of(o_king)) || (file_of(sq) > file_of(m_rook) && file_of(m_rook) > file_of(o_king))) return 128;
			} else if (rrank >= 2) {
				if (!(Pawn(me) & (File[1] | File[6])) && rrank + std::abs(file_of(sq) - file_of(m_rook)) > 4
					&& ((file_of(sq) < file_of(m_rook) && file_of(m_rook) < file_of(o_king)) || (file_of(sq) > file_of(m_rook) && file_of(m_rook) > file_of(o_king)))) return 128;
			}
		}
	}

	if (PWay[me][sq] & King(opp)) {
		if (Pawn(me) & (File[0] | File[7])) mul = std::min(mul, add_mat << 3);
		if (rrank <= 3) mul = std::min(mul, add_mat << 3);
		if (rrank == 4 && CRank(me, m_king) <= 4 && CRank(me, o_rook) == 5 && T(King(opp) & (Line(me, 6) | Line(me, 7)))
			&& (turn != me || !(PAtt[me][sq] & RookAttacks(lsb(Rook(me)), PieceAll) & (~SArea[o_king])))) mul = std::min(mul, add_mat << 3);
		if (rrank >= 5 && CRank(me, o_rook) <= 1 && (turn != me || Check(me) || Dist(m_king, sq) >= 2)) mul = std::min(mul, add_mat << 3);
		if (T(King(opp) & (File[1] | File[2] | File[6] | File[7])) && T(Rook(opp) & Line(me, 7)) && T(Between[o_king][o_rook] & (File[3] | File[4])) && !(Rook(me) & Line(me, 7))) mul = std::min(mul, add_mat << 3);
		return mul;
	} else if (rrank == 6 && (Pawn(me) & (File[0] | File[7])) && ((PSupport[me][sq] | PWay[opp][sq]) & Rook(opp)) && CRank(me, o_king) >= 6) {
		int dist = std::abs(file_of(sq) - file_of(o_king));
		if (dist <= 3)  mul = std::min(mul, add_mat << 3);
		if (dist == 4 && ((PSupport[me][o_king] & Rook(me)) || turn == opp)) mul = std::min(mul, add_mat << 3);
	}

	if (SArea[o_king] & PWay[me][sq] & Line(me, 7)) {
		if (rrank <= 4 && CRank(me, m_king) <= 4 && CRank(me, o_rook) == 5) mul = std::min(mul, add_mat << 3);
		if (rrank == 5 && CRank(me, o_rook) <= 1 && turn != me || (!(SArea[m_king] & PAtt[me][sq] & (~SArea[o_king])) && (Check(me) || Dist(m_king, sq) >= 2)))
			mul = std::min(mul, add_mat << 3);
	}

	if (T(PWay[me][sq] & Rook(me)) && T(PWay[opp][sq] & Rook(opp))) {
		if (King(opp) & (File[0] | File[1] | File[6] | File[7]) & Line(me, 6)) mul = std::min(mul, add_mat << 3);
		else if ((Pawn(me) & (File[0] | File[7])) && (King(opp) & (Line(me, 5) | Line(me, 6))) && std::abs(file_of(sq) - file_of(o_king)) <= 2 && file_of(sq) != file_of(o_king)) mul = std::min(mul, add_mat << 3);
	}

	if (std::abs(file_of(sq) - file_of(o_king)) <= 1 && std::abs(file_of(sq) - file_of(o_rook)) <= 1 && CRank(me, o_rook) > rrank && CRank(me, o_king) > rrank) mul = std::min(mul, (Pawn(me) & (File[3] | File[4])) ? 12 : 16);

	return mul;
}
template <bool me> int krpkbx() {
	constexpr bool opp = !me;
	if (!(Pawn(me) & Line(me, 5))) return 32;
	int sq = lsb(Pawn(me));
	if (!(PWay[me][sq] & King(opp))) return 32;
	int diag_sq = NB(me, BMask[sq + Push(me)]);
	if (CRank(me, diag_sq) > 1) return 32;
	uint64_t mdiag = FullLine[sq + Push(me)][diag_sq] | Bit(sq + Push(me)) | Bit(diag_sq);
	int check_sq = NB(me, BMask[sq - Push(me)]);
	uint64_t cdiag = FullLine[sq - Push(me)][check_sq] | Bit(sq - Push(me)) | Bit(check_sq);
	if ((mdiag | cdiag) & (Piece(opp) ^ King(opp) ^ Bishop(opp))) return 32;
	if (cdiag & Bishop(opp)) return 0;
	if ((mdiag & Bishop(opp)) && (turn == opp || !(King(me) & PAtt[opp][sq + Push(me)]))) return 0;
	return 32;
}
template <bool me> int kqkp() {
	constexpr bool opp = !me;
	if (!(SArea[lsb(King(opp))] & Pawn(opp) & Line(me, 1) & (File[0] | File[2] | File[5] | File[7]))) return 32;
	if (PWay[opp][lsb(Pawn(opp))] & (King(me) | Queen(me))) return 32;
	if (Pawn(opp) & (File[0] | File[7])) return 1;
	else return 4;
}
template <bool me> int kqkrpx() {
	constexpr bool opp = !me;
	int rsq = lsb(Rook(opp));
	uint64_t pawns = SArea[lsb(King(opp))] & PAtt[me][rsq] & Pawn(opp) & Interior & Line(me, 6);
	if (pawns && CRank(me, lsb(King(me))) <= 4) return 0;
	return 32;
}
template <bool me> int krkpx() {
	constexpr bool opp = !me;
	if (T(SArea[lsb(King(opp))] & Pawn(opp) & Line(me, 1)) & !(PWay[opp][NB(me, Pawn(opp))] & King(me))) return 0;
	return 32;
}
template <bool me> int krppkrpx() {
	constexpr bool opp = !me;
	if (Current->passer & Pawn(me)) {
		if (Single(Current->passer & Pawn(me))) {
			int sq = lsb(Current->passer & Pawn(me));
			if (PWay[me][sq] & King(opp) & (File[0] | File[1] | File[6] | File[7])) {
				int opp_king = lsb(King(opp));
				if (SArea[opp_king] & Pawn(opp)) {
					int king_file = file_of(opp_king);
					if (!((~(File[king_file] | PIsolated[king_file])) & Pawn(me))) return 1;
				}
			}
		}
		return 32;
	}
	if (!((~(PWay[opp][lsb(King(opp))] | PSupport[me][lsb(King(opp))])) & Pawn(me))) return 0;
	return 32;
}
template <bool me> int krpppkrppx() {
	constexpr bool opp = !me;
	if (T(Current->passer & Pawn(me)) || !((SArea[lsb(Pawn(opp))] | SArea[msb(Pawn(opp))]) & Pawn(opp))) return 32;
	if (!((~(PWay[opp][lsb(King(opp))] | PSupport[me][lsb(King(opp))])) & Pawn(me))) return 0;
	return 32;
}
template <bool me> int kbpkbx() {
	constexpr bool opp = !me;
	int sq = lsb(Pawn(me));
	uint64_t u;
	if ((T(Board->bb[ILight(me)]) && T(Board->bb[IDark(opp)])) || (T(Board->bb[IDark(me)]) && T(Board->bb[ILight(opp)]))) {
		if (CRank(me, sq) <= 4) return 0;
		if (T(PWay[me][sq] & King(opp)) && CRank(me, sq) <= 5) return 0;
		for (u = Bishop(opp); u != 0; Cut(u)) {
			if (CRank(me, lsb(u)) <= 4 && T(BishopAttacks(lsb(u), PieceAll) & PWay[me][sq])) return 0;
			if (turn == opp && T(BishopAttacks(lsb(u), PieceAll) & Pawn(me))) return 0;
		}
	} else if (T(PWay[me][sq] & King(opp)) && T(King(opp) & LightArea) != T(Bishop(me) & LightArea)) return 0;
	return 32;
}
template <bool me> int kbpknx() {
	constexpr bool opp = !me;
	uint64_t u;
	if (T(PWay[me][lsb(Pawn(me))] & King(opp)) && T(King(opp) & LightArea) != T(Bishop(me) & LightArea)) return 0;
	if (turn == opp)
	for (u = Knight(opp); u != 0; Cut(u))
	if (NAtt[lsb(u)] & Pawn(me)) return 0;
	return 32;
}
template <bool me> int kbppkbx() {
	constexpr bool opp = !me;
	int sq1 = NB(me, Pawn(me));
	int sq2 = NB(opp, Pawn(me));
	int o_king = lsb(King(opp));
	int o_bishop = lsb(Bishop(opp));

	if (file_of(sq1) == file_of(sq2)) {
		if (CRank(me, sq2) <= 3) return 0;
		if (T(PWay[me][sq2] & King(opp)) && CRank(me, sq2) <= 5) return 0;
	} else if (PIsolated[file_of(sq1)] & Pawn(me)) {
		if (T(King(opp) & LightArea) != T(Bishop(me) & LightArea)) {
			if (T((SArea[o_king] | King(opp)) & Bit(sq2 + Push(me))) && T(BishopAttacks(o_bishop, PieceAll) & Bit(sq2 + Push(me))))
			if (T((SArea[o_king] | King(opp)) & Bit((sq2 & 0xFFFFFFF8) | file_of(sq1))) && T(BishopAttacks(o_bishop, PieceAll) & Bit((sq2 & 0xFFFFFFF8) | file_of(sq1)))) return 0;
		}
	}
	return 32;
}
template <bool me> int krppkrx() {
	constexpr bool opp = !me;
	int sq1 = NB(me, Pawn(me));
	int sq2 = NB(opp, Pawn(me));

	if ((Piece(opp) ^ King(opp) ^ Rook(opp)) & Forward[me][rank_of(sq1 - Push(me))]) return 32;
	if (file_of(sq1) == file_of(sq2)) {
		if (T(PWay[me][sq2] & King(opp))) return 16;
		return 32;
	}
	if (T(PIsolated[file_of(sq2)] & Pawn(me)) && T((File[0] | File[7]) & Pawn(me)) && T(King(opp) & Shift(me, Pawn(me)))) {
		if (CRank(me, sq2) == 5 && CRank(me, sq1) == 4 && T(Rook(opp) & (Line(me, 5) | Line(me, 6)))) return 10;
		else if (CRank(me, sq2) < 5) return 16;
	}
	return 32;
}
struct GPawnEvalInfo {
	int king_w, king_b, score;
	uint64_t patt_w, patt_b, double_att_w, double_att_b;
};

template <bool me, bool HPopCnt> void eval_pawns(GPawnEntry * PawnEntry, GPawnEvalInfo &PEI) {
	constexpr bool opp = !me;
	int kf = file_of(PVarC(PEI, king, me));
	int kr = rank_of(PVarC(PEI, king, me));
	int start, inc;
	if (kf <= 3) {
		start = std::max(kf - 1, 0);
		inc = 1;
	} else {
		start = std::min(kf + 1, 7);
		inc = -1;
	}
	int shelter = 0;
	uint64_t mpawns = Pawn(me) & Forward[me][me ? std::min(kr + 1, 7) : std::max(kr - 1, 0)];
	for (int file = start, i = 0; i < 3; file += inc, i++) {
		shelter += Shelter[i][CRank(me, NBZ(me, mpawns & File[file]))];
		int rank;
		if (Pawn(opp) & File[file]) {
			int sq = NB(me, Pawn(opp) & File[file]);
			if ((rank = CRank(opp, sq)) < 6) {
				if (rank >= 3) shelter += StormBlocked[rank - 3];
				if (uint64_t u = (PIsolated[file_of(sq)] & Forward[opp][rank_of(sq)] & Pawn(me))) {
					int square = NB(opp, u);
					uint64_t att_sq = PAtt[me][square] & PWay[opp][sq]; // may be zero
					if ((File[file_of(square)] | PIsolated[file_of(square)]) & King(me)) if (!(PVarC(PEI, double_att, me) & att_sq) || (Current->patt[opp] & att_sq)) {
						if (PWay[opp][square] & Pawn(me)) continue;
						if (!(PawnAll & PWay[opp][sq] & Forward[me][rank_of(square)])) {
							if (rank >= 3) {
								shelter += StormShelterAtt[rank - 3];
								if (PVarC(PEI, patt, opp) & Bit(sq + Push(opp))) shelter += StormConnected[rank - 3];
								if (!(PWay[opp][sq] & PawnAll)) shelter += StormOpen[rank - 3];
							}
							if (!((File[file_of(sq)] | PIsolated[file_of(sq)]) & King(opp)) && rank <= 4) shelter += StormFree[rank - 1];
						}
					}
				}
			}
		} else {
			shelter += Sa(StormHof, StormHofValue);
			if (!(Pawn(me) & File[file])) shelter += Sa(StormHof, StormOfValue);
		}
	}
	PawnEntry->shelter[me] = shelter;

	uint64_t b;
	int min_file = 7, max_file = 0;
	for (uint64_t u = Pawn(me); u != 0; u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		int rank = rank_of(sq);
		int rrank = CRank(me, sq);
		int file = file_of(sq);
		uint64_t way = PWay[me][sq];
		int next = Square(sq + Push(me));
		if (file < min_file) min_file = file;
		if (file > max_file) max_file = file;

		int isolated = !(Pawn(me) & PIsolated[file]);
		int doubled = T(Pawn(me) & (File[file] ^ b));
		int open = !(PawnAll & way);
		int up = !(PVarC(PEI, patt, me) & b);

		if (isolated) {
			if (open) DecV(PEI.score, Ca(Isolated, IsolatedOpen));
			else {
				DecV(PEI.score, Ca(Isolated, IsolatedClosed));
				if (next == IPawn(opp)) DecV(PEI.score, Ca(Isolated, IsolatedBlocked));
			}
			if (doubled) {
				if (open) DecV(PEI.score, Ca(Isolated, IsolatedDoubledOpen));
				else DecV(PEI.score, Ca(Isolated, IsolatedDoubledClosed));
			}
		} else {
			if (doubled) {
				if (open) DecV(PEI.score, Ca(Doubled, DoubledOpen));
				else DecV(PEI.score, Ca(Doubled, DoubledClosed));
			}
			if (rrank >= 3 && (b & (File[2] | File[3] | File[4] | File[5])) && next != IPawn(opp) && (PIsolated[file] & Line[rank] & Pawn(me)))
				IncV(PEI.score, Ca(PawnSpecial, PawnChainLinear) * (rrank - 3) + Ca(PawnSpecial, PawnChain));
		}
		int backward = 0;
		if (!(PSupport[me][sq] & Pawn(me))) {
			if (isolated) backward = 1;
			else if (uint64_t v = (PawnAll | PVarC(PEI, patt, opp)) & way) if (IsGreater(me, NB(me, PVarC(PEI, patt, me) & way), NB(me, v))) backward = 1;
		}
		if (backward) {
			if (open) DecV(PEI.score, Ca(Backward, BackwardOpen));
			else DecV(PEI.score, Ca(Backward, BackwardClosed));
		} else if (open) if (!(Pawn(opp) & PIsolated[file]) || popcount<HPopCnt>(Pawn(me) & PIsolated[file]) >= popcount<HPopCnt>(Pawn(opp) & PIsolated[file])) IncV(PEI.score,PasserCandidate[rrank]); // IDEA: more precise pawn counting for the case of, say, white e5 candidate with black pawn on f5 or f4...
		if (up && next == IPawn(opp)) {
			DecV(PEI.score, Ca(Unprotected, UpBlocked));
			if (backward) {
				if (rrank <= 2) { // IDEA (based on weird passer target tuning result): may be score unprotected/backward depending on rank/file?
					DecV(PEI.score, Ca(Unprotected, PasserTarget));
					if (rrank <= 1) DecV(PEI.score, Ca(Unprotected, PasserTarget));
				}
				for (uint64_t v = PAtt[me][sq] & Pawn(me); v; Cut(v)) if ((PSupport[me][lsb(v)] & Pawn(me)) == b) {
					DecV(PEI.score, Ca(Unprotected, ChainRoot));
					break;
				}
			}
		}
		if (open && !(PIsolated[file] & Forward[me][rank] & Pawn(opp))) {
			PawnEntry->passer[me] |= (uint8_t)(1 << file);
			if (rrank <= 2) continue;
			IncV(PEI.score, PasserGeneral[rrank]);
			int dist_att = Dist(PVarC(PEI, king, opp), sq + Push(me)); // IDEA: average the distance with the distance to the promotion square? or just use the latter?
			int dist_def = Dist(PVarC(PEI, king, me), sq + Push(me));
			IncV(PEI.score, Compose256(0, dist_att * (int)PasserAtt[rrank] + LogDist[dist_att] * (int)PasserAttLog[rrank] - dist_def * (int)PasserDef[rrank] - (int)LogDist[dist_def] * (int)PasserDefLog[rrank]));
			if (PVarC(PEI, patt, me) & b) IncV(PEI.score, PasserProtected[rrank]);
			if (!(Pawn(opp) & West[file]) || !(Pawn(opp) & East[file])) IncV(PEI.score, PasserOutside[rrank]);
		}
	}
	uint64_t files = 0;
	for (int i = 1; i < 7; i++) files |= (Pawn(me) >> (i << 3)) & 0xFF;
	int file_span = (files ? (msb(files) - lsb(files)) : 0);
	IncV(PEI.score, Ca(PawnSpecial, PawnFileSpan) * file_span);
	PawnEntry->draw[me] = (7 - file_span) * std::max(5 - popcount<HPopCnt>(files), 0);
}

template <bool HPopCnt> void eval_pawn_structure(GPawnEntry * PawnEntry) {
	GPawnEvalInfo PEI;
	for (size_t i = 0; i < sizeof(GPawnEntry) / sizeof(int); i++) *(((int*)PawnEntry) + i) = 0;
	PawnEntry->key = Current->pawn_key;

	PEI.patt_w = ShiftW(White, Pawn(White)) | ShiftE(White, Pawn(White));
	PEI.patt_b = ShiftW(Black, Pawn(Black)) | ShiftE(Black, Pawn(Black));
	PEI.double_att_w = ShiftW(White, Pawn(White)) & ShiftE(White, Pawn(White));
	PEI.double_att_b = ShiftW(Black, Pawn(Black)) & ShiftE(Black, Pawn(Black));
	PEI.king_w = lsb(King(White));
	PEI.king_b = lsb(King(Black));
	PEI.score = 0;

	eval_pawns<White, HPopCnt>(PawnEntry, PEI);
	eval_pawns<Black, HPopCnt>(PawnEntry, PEI);

	PawnEntry->score = PEI.score;
}


struct GEvalInfo {
	int score, king_w, king_b, mul;
	uint64_t occ, area_w, area_b, free_w, free_b;
	uint32_t king_att_w, king_att_b;
	GPawnEntry * PawnEntry;
	GMaterial * material;
};


// Memo: L862
#define KingAttack Compose(1, 0)

template <bool me, bool HPopCnt> void eval_queens(GEvalInfo &EI) {
	constexpr bool opp = !me;
	uint64_t u, b;
	for (u = Queen(me); u != 0; u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64_t att = QueenAttacks(sq,EI.occ);
		Current->att[me] |= att;
		if (QMask[sq] & King(opp)) if (uint64_t v = Between[PVarC(EI,king,opp)][sq] & EI.occ) if (Single(v)) {
			Current->xray[me] |= v;
			uint64_t square = lsb(v); int piece = Square(square); int katt = 0;
			if (piece == IPawn(me)) {
				if (!Square(square + Push(me))) IncV(EI.score, Ca(Pin, SelfPawnPin));
			} else if ((piece & 1) == me) {
				IncV(EI.score, Ca(Pin, SelfPiecePin));
				katt = 1;
			} else if (piece != IPawn(opp) && !(((BMask[sq] & Bishop(opp)) | (RMask[sq] & Rook(opp)) | Queen(opp)) & v)) {
				IncV(EI.score, Ca(Pin, WeakPin));
				if (!(Current->patt[opp] & v)) katt = 1;
			}
			if (katt && !(att & PVarC(EI, area, opp))) PVarC(EI, king_att, me) += KingAttack;
		} else if (v == (v & Minor(opp))) IncV(EI.score, Ca(KingRay, QKingRay));
		if (att & PVarC(EI, area, opp)) {
			PVarC(EI, king_att, me) += KingQAttack;
			for (uint64_t v = att & PVarC(EI, area, opp); v != 0; Cut(v))
			if (FullLine[sq][lsb(v)] & att & ((Rook(me) & RMask[sq]) | (Bishop(me) & BMask[sq]))) PVarC(EI, king_att, me)++;
		}
		IncV(EI.score,Mobility[PieceType[WhiteQueen] - 1][popcount<HPopCnt>(att & PVarC(EI,free,me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorPawn));
		if (att & PVarC(EI, free, me) & Minor(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefQueen));
	}
}
template <bool me, bool HPopCnt> void eval_rooks(GEvalInfo &EI) {
	constexpr bool opp = !me;
	uint64_t u, b;
	for (u = Rook(me); u != 0; u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64_t att = RookAttacks(sq,EI.occ);
		Current->att[me] |= att;
		if (RMask[sq] & King(opp)) if (uint64_t v = Between[PVarC(EI, king, opp)][sq] & EI.occ) if (Single(v)) {
			Current->xray[me] |= v;
			uint64_t square = lsb(v); int piece = Square(square); int katt = 0;
			if (piece == IPawn(me)) {
				if (!Square(square + Push(me))) IncV(EI.score, Ca(Pin, SelfPawnPin));
			} else if ((piece & 1) == me) {
				IncV(EI.score, Ca(Pin, SelfPiecePin));
				katt = 1;
			} else if (piece != IPawn(opp)) {
				if (piece < IRook(opp)) {
					IncV(EI.score, Ca(Pin, WeakPin));
					if (!(Current->patt[opp] & v)) katt = 1;
				} else if (piece == IQueen(opp)) IncV(EI.score, Ca(Pin, ThreatPin));
			}
			if (katt && !(att & PVarC(EI, area, opp))) PVarC(EI, king_att, me) += KingAttack;
		} else if (v == (v & (Minor(opp) | Queen(opp)))) IncV(EI.score, Ca(KingRay, RKingRay));
		if (att & PVarC(EI, area, opp)) {
			PVarC(EI, king_att, me) += KingRAttack;
			for (uint64_t v = att & PVarC(EI, area, opp); v != 0; Cut(v))
			if (FullLine[sq][lsb(v)] & att & Major(me)) PVarC(EI, king_att, me)++;
		}
		IncV(EI.score,Mobility[PieceType[WhiteRook] - 1][popcount<HPopCnt>(att & PVarC(EI,free,me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorPawn));
		if (att & PVarC(EI, free, me) & Minor(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefRook));
		Current->threat |= att & Queen(opp);
		if (!(PWay[me][sq] & Pawn(me))) {
			IncV(EI.score, Ca(RookSpecial, RookHof));
			int hof_score = 0;
			if (!(PWay[me][sq] & Pawn(opp))) {
				IncV(EI.score, Ca(RookSpecial, RookOf));
				if (att & Line(me, 7)) hof_score += Ca(RookSpecial, RookOfOpen);
				else if (uint64_t target = att & PWay[me][sq] & Minor(opp)) {
					if (!(Current->patt[opp] & target)) {
						hof_score += Ca(RookSpecial, RookOfMinorHaging);
						if (PWay[me][sq] & King(opp)) hof_score += Ca(RookSpecial, RookOfKingAtt);
					} else hof_score += Ca(RookSpecial, RookOfMinorFixed);
				}
			} else if (att & PWay[me][sq] & Pawn(opp)) {
				uint64_t square = lsb(att & PWay[me][sq] & Pawn(opp));
				if (!(PSupport[opp][square] & Pawn(opp))) hof_score += Ca(RookSpecial, RookHofWeakPAtt);
			}
			IncV(EI.score, hof_score);
			if (PWay[opp][sq] & att & Major(me)) IncV(EI.score, hof_score);
		}
		if ((b & Line(me, 6)) && ((King(opp) | Pawn(opp)) & (Line(me, 6) | Line(me, 7)))) {
			IncV(EI.score, Ca(RookSpecial, Rook7th));
			if (King(opp) & Line(me, 7)) IncV(EI.score, Ca(RookSpecial, Rook7thK8th));
			if (Major(me) & att & Line(me, 6)) IncV(EI.score, Ca(RookSpecial, Rook7thDoubled));
		}
	}
}
template <bool me, bool HPopCnt> void eval_bishops(GEvalInfo &EI) {
	constexpr bool opp = !me;
	uint64_t u, b;
	for (u = Bishop(me); u != 0; u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64_t att = BishopAttacks(sq, EI.occ);
		Current->att[me] |= att;
		if (BMask[sq] & King(opp)) if (uint64_t v = Between[PVarC(EI, king, opp)][sq] & EI.occ) if (Single(v)) {
			Current->xray[me] |= v;
			uint64_t square = lsb(v); int piece = Square(square); int katt = 0;
			if (piece == IPawn(me)) {
				if (!Square(square + Push(me))) IncV(EI.score, Ca(Pin, SelfPawnPin));
			} else if ((piece & 1) == me) {
				IncV(EI.score, Ca(Pin, SelfPiecePin));
				katt = 1;
			} else if (piece != IPawn(opp)) {
				if (piece < ILight(opp)) {
					IncV(EI.score, Ca(Pin, StrongPin));
					if (!(Current->patt[opp] & v)) katt = 1;
				} else if (piece >= IRook(opp)) IncV(EI.score, Ca(Pin, ThreatPin));
			}
			if (katt && !(att & PVarC(EI, area, opp))) PVarC(EI, king_att, me) += KingAttack;
		} else if (v == (v & (Knight(opp) | Major(opp)))) IncV(EI.score, Ca(KingRay, BKingRay));
		if (att & PVarC(EI, area, opp)) PVarC(EI, king_att, me) += KingBAttack;
		IncV(EI.score, Mobility[PieceType[WhiteLight] - 1][popcount<HPopCnt>(att & PVarC(EI, free, me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorPawn));
		if (att & PVarC(EI, free, me) & Knight(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefBishop));
		Current->threat |= att & Major(opp);
		if (b & LightArea) {
			for (uint64_t v = ((~BishopForward[me][sq]) | (att & Forward[me][rank_of(sq)])) & Pawn(opp) & (~Current->patt[opp]) & LightArea; v; Cut(v)) {
				uint64_t square = lsb(v);
				if (!((PSupport[opp][square] | PWay[opp][square]) & Pawn(opp))) IncV(EI.score, Ca(BishopSpecial, BishopNonForwardPawn));
			}
			uint64_t v = BishopForward[me][sq] & Pawn(me) & LightArea;
			v |= (v & (File[2] | File[3] | File[4] | File[5] | BMask[sq])) >> 8;
			DecV(EI.score, Ca(BishopSpecial, BishopPawnBlock) * popcount<HPopCnt>(v));
		} else {
			for (uint64_t v = ((~BishopForward[me][sq]) | (att & Forward[me][rank_of(sq)])) & Pawn(opp) & (~Current->patt[opp]) & DarkArea; v; Cut(v)) {
				uint64_t square = lsb(v);
				if (!((PSupport[opp][square] | PWay[opp][square]) & Pawn(opp))) IncV(EI.score, Ca(BishopSpecial, BishopNonForwardPawn));
			}
			uint64_t v = BishopForward[me][sq] & Pawn(me) & DarkArea;
			v |= (v & (File[2] | File[3] | File[4] | File[5] | BMask[sq])) >> 8;
			DecV(EI.score, Ca(BishopSpecial, BishopPawnBlock) * popcount<HPopCnt>(v));
		}
	}
}
template <bool me, bool HPopCnt> void eval_knights(GEvalInfo &EI) {
	constexpr bool opp = !me;
	uint64_t u, b;
	for (u = Knight(me); u != 0; u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64_t att = NAtt[sq];
		Current->att[me] |= att;
		if (att & PVarC(EI, area, opp)) PVarC(EI, king_att, me) += KingNAttack;
		IncV(EI.score, Mobility[PieceType[WhiteKnight] - 1][popcount<HPopCnt>(att & PVarC(EI, free, me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorPawn));
		if (att & PVarC(EI, free, me) & Bishop(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefKnight));
		Current->threat |= att & Major(opp);
		if ((b & Outpost[me]) && !(Pawn(opp) & PIsolated[file_of(sq)] & Forward[me][rank_of(sq)])) {
			IncV(EI.score, Ca(KnightSpecial, KnightOutpost));
			if (Current->patt[me] & b) {
				IncV(EI.score, Ca(KnightSpecial, KnightOutpostProtected));
				if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(KnightSpecial, KnightOutpostPawnAtt));
				if (att & PVarC(EI, free, me) & Bishop(opp)) IncV(EI.score, Ca(KnightSpecial, KnightOutpostBishopAtt));
			}
		}
	}
}
template <bool me, bool HPopCnt> void eval_king(GEvalInfo &EI) {
	constexpr bool opp = !me;
	int cnt = Opening(PVarC(EI, king_att, me));
	int score = Endgame(PVarC(EI, king_att, me));
	if (cnt >= 2 && T(Queen(me))) {
		score += (EI.PawnEntry->shelter[opp] * KingShelterQuad)/64;
		if (uint64_t u = Current->att[me] & PVarC(EI, area, opp) & (~Current->att[opp])) score += popcount<HPopCnt>(u) * KingAttackSquare;
		if (!(SArea[PVarC(EI, king, opp)] & (~(Piece(opp) | Current->att[me])))) score += KingNoMoves;
	}
	int adjusted = ((score * KingAttackScale[cnt]) >> 3) + EI.PawnEntry->shelter[opp];
	if (!Queen(me)) adjusted /= 2;
	IncV(EI.score, adjusted);
}
template <bool me, bool HPopCnt> void eval_passer(GEvalInfo &EI) {
	for (uint64_t u = EI.PawnEntry->passer[me]; u != 0; Cut(u)) {
		int file = lsb(u);
		int sq = NB(opp, File[file] & Pawn(me));
		int rank = CRank(me, sq);
		Current->passer |= Bit(sq);
		if (rank <= 2) continue;
		if (!Square(sq + Push(me))) IncV(EI.score, PasserBlocked[rank]);
		uint64_t way = PWay[me][sq];
		int connected = 0, supported = 0, hooked = 0, free = 0;	// , unsupported = 0
		if (!(way & Piece(opp))) {
			IncV(EI.score, PasserClear[rank]);
			if (PWay[opp][sq] & Major(me)) {
				int square = NB(opp, PWay[opp][sq] & Major(me));
				if (!(Between[sq][square] & EI.occ)) supported = 1;
			}
			if (PWay[opp][sq] & Major(opp)) {
				int square = NB(opp, PWay[opp][sq] & Major(opp));
				if (!(Between[sq][square] & EI.occ)) hooked = 1;
			}
			for (uint64_t v = PAtt[me][sq - Push(me)] & Pawn(me); v != 0; Cut(v)) {
				int square = lsb(v);
				if (!(Pawn(opp) & (File[file_of(square)] | PIsolated[file_of(square)]) & Forward[me][rank_of(square)])) connected++;
			}
			if (connected) IncV(EI.score, PasserConnected[rank]);
			if (!hooked && !(Current->att[opp] & way)) {
				IncV(EI.score, PasserFree[rank]);
				free = 1;
			} else {
				uint64_t attacked = Current->att[opp] | (hooked ? way : 0);
				if (supported || (!hooked && connected) || (!(Major(me) & way) && !(attacked & (~Current->att[me])))) IncV(EI.score, PasserSupported[rank]);
///				else unsupported = 1;
			}
		}
		if (rank == 6) {
			if ((way & Rook(me)) && !Minor(me) && !Queen(me) && Single(Rook(me))) DecV(EI.score, Compose(0, Sa(PasserSpecial, PasserOpRookBlock)));
			if (!Major(opp) && (!NonPawnKing(opp) || Single(NonPawnKing(opp)))) {
				IncV(EI.score, Compose(0, Sa(PasserSpecial, PasserOnePiece)));
				if (!free) {
					if (!(SArea[sq + Push(me)] & King(opp))) IncV(EI.score, Compose(0, Sa(PasserSpecial, PasserOpMinorControl)));
					else IncV(EI.score, Compose(0, Sa(PasserSpecial, PasserOpKingControl)));
				}
			}
		}
	}
}
template <bool me, bool HPopCnt> void eval_pieces(GEvalInfo &EI) {
	constexpr bool opp = !me;
	Current->threat |= Current->att[opp] & (~Current->att[me]) & Piece(me);
	if (uint64_t u = Current->threat & Piece(me)) {
		DecV(EI.score, Ca(Tactical, TacticalThreat));
		Cut(u);
		if (u) {
			DecV(EI.score, Ca(Tactical, TacticalThreat) + Ca(Tactical, TacticalDoubleThreat));
			for (Cut(u); u; Cut(u)) DecV(EI.score, Ca(Tactical, TacticalThreat));
		}
	}
}
template <bool me, bool HPopCnt> void eval_endgame(GEvalInfo &EI) {
	constexpr bool opp = !me;
	if ((EI.material->flags & VarC(FlagSingleBishop, me)) && Pawn(me)) {
		int sq = (Board->bb[ILight(me)] ? (me ? 0 : 63) : (Board->bb[IDark(me)] ? (me ? 7 : 56) : (file_of(lsb(King(opp))) <= 3 ? (me ? 0 : 56) : (me ? 7 : 63))));
		if (!(Pawn(me) & (~PWay[opp][sq]))) {
			if ((SArea[sq] | Bit(sq)) & King(opp)) EI.mul = 0;
			else if ((SArea[sq] & SArea[lsb(King(opp))] & Line(me, 7)) && Square(sq - Push(me)) == IPawn(opp) && Square(sq - 2 * Push(me)) == IPawn(me)) EI.mul = 0;
		} else if ((King(opp) & Line(me, 6) | Line(me, 7)) && std::abs(file_of(sq) - file_of(lsb(King(opp)))) <= 3 && !(Pawn(me) & (~PSupport[me][sq])) && (Pawn(me) & Line(me, 5) & Shift(opp, Pawn(opp)))) EI.mul = 0;
		if (Single(Pawn(me))) {
			if (!Bishop(me)) {
				EI.mul = MinF(EI.mul, kpkx<me>());
				if (Piece(opp) == King(opp) && EI.mul == 32) IncV(Current->score, KpkValue);
			} else {
				sq = lsb(Pawn(me));
				if ((Pawn(me) & (File[1] | File[6]) & Line(me, 5)) && Square(sq + Push(me)) == IPawn(opp) && ((PAtt[me][sq + Push(me)] | PWay[me][sq + Push(me)]) & King(opp))) EI.mul = 0;
			}
		}
		if (Bishop(opp) && Single(Bishop(opp)) && T(BB(ILight(me))) != T(BB(ILight(opp)))) {
			int pcnt = 0;
			if (T(King(opp) & LightArea) == T(Bishop(opp) & LightArea)) {
				for (uint64_t u = Pawn(me); u; Cut(u)) {
					if (pcnt >= 2) goto check_for_partial_block;
					pcnt++;
					int sq = lsb(u);
					if (!(PWay[me][sq] & (PAtt[me][PVarC(EI, king, opp)] | PAtt[opp][PVarC(EI, king, opp)]))) {
						if (!(PWay[me][sq] & Pawn(opp))) goto check_for_partial_block;
						int bsq = lsb(Bishop(opp));
						uint64_t att = BishopAttacks(bsq, EI.occ);
						if (!(att & PWay[me][sq] & Pawn(opp))) goto check_for_partial_block;
						if (!(BishopForward[me][bsq] & att & PWay[me][sq] & Pawn(opp)) && popcount<HPopCnt>(FullLine[lsb(att & PWay[me][sq] & Pawn(opp))][bsq] & att) <= 2)  goto check_for_partial_block;
					}
				}
				EI.mul = 0;
				return;
			}
		check_for_partial_block:
			if (pcnt <= 2 && Multiple(Pawn(me)) && !Pawn(opp) && !(Pawn(me) & Boundary) && EI.mul) {
				int sq1 = lsb(Pawn(me));
				int sq2 = msb(Pawn(me));
				int fd = std::abs(file_of(sq2) - file_of(sq1));
				if (fd >= 5) EI.mul = 32;
				else if (fd >= 4) EI.mul = 26;
				else if (fd >= 3) EI.mul = 20;
			}
			if ((SArea[PVarC(EI, king, opp)] | Current->patt[opp]) & Bishop(opp)) {
				uint64_t push = Shift(me, Pawn(me));
				if (!(push & (~(Piece(opp) | Current->att[opp]))) && (King(opp) & (Board->bb[ILight(opp)] ? LightArea : DarkArea))) {
					EI.mul = std::min(EI.mul, 8);
					int bsq = lsb(Bishop(opp));
					uint64_t att = BishopAttacks(bsq, EI.occ);
					uint64_t prp = (att | SArea[PVarC(EI, king, opp)]) & Pawn(opp) & (Board->bb[ILight(opp)] ? LightArea : DarkArea);
					uint64_t patt = ShiftW(opp, prp) | ShiftE(opp, prp);
					if ((SArea[PVarC(EI, king, opp)] | patt) & Bishop(opp)) {
						uint64_t double_att = (SArea[PVarC(EI, king, opp)] & patt) | (patt & att) | (SArea[PVarC(EI, king, opp)] & att);
						if (!(push & (~(King(opp) | Bishop(opp) | prp | double_att)))) {
							EI.mul = 0;
							return;
						}
					}
				}
			}
		}
	}
	if (!(Major(me))) {
		if (T(Bishop(me)) && !(Knight(me)) && Single(Bishop(me)) && T(Pawn(me))) {
			int number = popcount<HPopCnt>(Pawn(me));
			if (number == 1) {
				if (Bishop(opp)) EI.mul = MinF(EI.mul, kbpkbx<me>());
				else if (Knight(opp)) EI.mul = MinF(EI.mul, kbpknx<me>());
			} else if (number == 2 && T(Bishop(opp))) EI.mul = MinF(EI.mul, kbppkbx<me>());
		} else if (!Bishop(me) && Knight(me) && Single(Knight(me)) && Pawn(me) && Single(Pawn(me))) EI.mul = MinF(EI.mul, knpkx<me>());
	} else if (!(Minor(me))) {
		if (!(Pawn(me)) && !(Rook(me)) && T(Queen(me)) && T(Pawn(opp))) {
			if (!(NonPawnKing(opp)) && Single(Pawn(opp))) EI.mul = MinF(EI.mul, kqkp<me>());
			else if (Rook(opp)) EI.mul = MinF(EI.mul, kqkrpx<me>());
		} else if (!(Queen(me)) && T(Rook(me)) && Single(Rook(me))) {
			int number = popcount<HPopCnt>(Pawn(me));
			if (number <= 3) {
				if (number == 0) {
					if (Pawn(opp)) EI.mul = MinF(EI.mul, krkpx<me>());
				} else if (Rook(opp)) {
					if (number == 1) {
						int new_mul = krpkrx<me>();
						EI.mul = (new_mul <= 32 ? std::min(EI.mul, new_mul) : new_mul);
					} else {
						if (number == 2) EI.mul = MinF(EI.mul, krppkrx<me>());
						if (Pawn(opp)) {
							if (number == 2) EI.mul = MinF(EI.mul, krppkrpx<me>());
							else if (Multiple(Pawn(opp))) EI.mul = MinF(EI.mul, krpppkrppx<me>());
						}
					}
				} else if (number == 1 && Bishop(opp)) EI.mul = MinF(EI.mul, krpkbx<me>());
			}
		}
	} else if (!Pawn(me) && Single(Rook(me)) && !Queen(me) && Single(Bishop(me)) && !Knight(me) && Rook(opp)) EI.mul = MinF(EI.mul, krbkrx<me>());
	if (!(NonPawnKing(opp)) && turn == opp && !(Current->att[me] & King(opp)) && !(SArea[PVarC(EI, king, opp)] & (~(Current->att[me] | Piece(opp))))
		&& !(Current->patt[opp] & Piece(me)) && !(Shift(opp, Pawn(opp)) & (~EI.occ)))
		EI.mul = 0;
}
template <bool HPopCnt> void eval_unusual_material(GEvalInfo &EI) {
	int wp, bp, wlight, blight, wr, br, wq, bq;
	wp = popcount<HPopCnt>(Pawn(White));
	bp = popcount<HPopCnt>(Pawn(Black));
	wlight = popcount<HPopCnt>(Minor(White));
	blight = popcount<HPopCnt>(Minor(Black));
	wr = popcount<HPopCnt>(Rook(White));
	br = popcount<HPopCnt>(Rook(Black));
	wq = popcount<HPopCnt>(Queen(White));
	bq = popcount<HPopCnt>(Queen(Black));
	int phase = std::min(24, (wlight + blight) + 2 * (wr + br) + 4 * (wq + bq));
	int mat_score = SeeValue[WhitePawn] * (wp - bp) + SeeValue[WhiteKnight] * (wlight - blight) + SeeValue[WhiteRook] * (wr - br) + SeeValue[WhiteQueen] * (wq - bq);
	mat_score = Compose(mat_score,mat_score);
	Current->score = (((Opening(mat_score + EI.score) * phase) + (Endgame(mat_score + EI.score) * (24 - phase)))/24);
	if (turn) Current->score = -Current->score;
	UpdateDelta
}

template <bool HPopCnt> void evaluation() {
	GEvalInfo EI;
	
	if (Current->eval_key == Current->key) return;
	Current->eval_key = Current->key;

	EI.king_w = lsb(King(White));
	EI.king_b = lsb(King(Black));
	EI.occ = PieceAll;
	Current->patt[White] = ShiftW(White,Pawn(White)) | ShiftE(White,Pawn(White));
	Current->patt[Black] = ShiftW(Black,Pawn(Black)) | ShiftE(Black,Pawn(Black));
	EI.area_w = (SArea[EI.king_w] | King(White)) & ((~Current->patt[White]) | Current->patt[Black]);
	EI.area_b = (SArea[EI.king_b] | King(Black)) & ((~Current->patt[Black]) | Current->patt[White]);
	Current->att[White] = Current->patt[White];
	Current->att[Black] = Current->patt[Black];
	Current->passer = 0;
	Current->threat = (Current->patt[White] & NonPawn(Black)) | (Current->patt[Black] & NonPawn(White));
	EI.score = Current->pst;

#define me  White
#define opp Black
	Current->xray[me] = 0;
	PVarC(EI, free, me) = Queen(opp) | King(opp) | (~(Current->patt[opp] | Pawn(me) | King(me)));
	DecV(EI.score, popcount<HPopCnt>(Shift(opp, EI.occ) & Pawn(me)) * Ca(PawnSpecial, PawnBlocked));
	if (Current->patt[me] & PVarC(EI, area, opp)) PVarC(EI, king_att, me) = KingAttack;
	else PVarC(EI, king_att, me) = 0;
	eval_queens<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Rook(opp);
	eval_rooks<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Minor(opp);
	eval_bishops<me, HPopCnt>(EI);
	eval_knights<me, HPopCnt>(EI);
#undef me
#undef opp
#define me  Black
#define opp White
	Current->xray[me] = 0;
	PVarC(EI, free, me) = Queen(opp) | King(opp) | (~(Current->patt[opp] | Pawn(me) | King(me)));
	DecV(EI.score, popcount<HPopCnt>(Shift(opp, EI.occ) & Pawn(me)) * Ca(PawnSpecial, PawnBlocked));
	if (Current->patt[me] & PVarC(EI, area, opp)) PVarC(EI, king_att, me) = KingAttack;
	else PVarC(EI, king_att, me) = 0;
	eval_queens<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Rook(opp);
	eval_rooks<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Minor(opp);
	eval_bishops<me, HPopCnt>(EI);
	eval_knights<me, HPopCnt>(EI);
#undef me
#undef opp

	EI.PawnEntry = PAWNHASH.entry(Current->pawn_key);
	if (Current->pawn_key != EI.PawnEntry->key) eval_pawn_structure<HPopCnt>(EI.PawnEntry);
	EI.score += EI.PawnEntry->score;

	eval_king<White, HPopCnt>(EI);
	eval_king<Black, HPopCnt>(EI);
	Current->att[White] |= SArea[EI.king_w];
	Current->att[Black] |= SArea[EI.king_b];

	eval_passer<White, HPopCnt>(EI);
	eval_pieces<White, HPopCnt>(EI);
	eval_passer<Black, HPopCnt>(EI);
	eval_pieces<Black, HPopCnt>(EI);

	if (Current->material & FlagUnusualMaterial) {
		eval_unusual_material<HPopCnt>(EI);
		return;
	}
	EI.material = &Material[Current->material];
	Current->score = EI.material->score + (((Opening(EI.score) * EI.material->phase) + (Endgame(EI.score) * (128 - (int)EI.material->phase)))/128);

	if (Current->ply >= 50) Current->score /= 2;
	if (Current->score > 0) {
		EI.mul = EI.material->mul[White];
		if (EI.material->flags & FlagCallEvalEndgame_w) eval_endgame<White, HPopCnt>(EI);
		Current->score -= (std::min(Current->score, 100) * (int)EI.PawnEntry->draw[White]) / 64;
	} else if (Current->score < 0) {
		EI.mul = EI.material->mul[Black];
		if (EI.material->flags & FlagCallEvalEndgame_b) eval_endgame<Black, HPopCnt>(EI);
		Current->score += (std::min(-Current->score, 100) * (int)EI.PawnEntry->draw[Black]) / 64;
	} else EI.mul = std::min(EI.material->mul[White], EI.material->mul[Black]);
	Current->score = (Current->score * EI.mul)/32;

	if (turn) Current->score = -Current->score;
	UpdateDelta
}

void evaluate() {
////	HardwarePopCnt ? evaluation<1>() : evaluation<0>();
	evaluation<1>();
}
#endif

// Memo: L1331
void print_eval() {
	int i, j;
	FILE * f = fopen("eval.txt", "w");
	fprintf(f, "Pst\n");
	for (j = 2; j < 16; j += 2) {
		if (j == 8) continue;
		fprintf(f, "%d:\n", j);
		for (i = 0; i < 64; i++) {
			fprintf(f, "(%d,%d), ", Opening(Pst(j, i)), Endgame(Pst(j, i)));
			if ((i + 1) % 8 == 0) fprintf(f, "\n");
		}
	}
	fprintf(f, "Mobility\n");
	for (j = 0; j < 4; j++) {
		fprintf(f, "%d:\n", j);
		for (i = 0; i < 32; i++) fprintf(f, "(%d,%d), ", Opening(Mobility[j][i]), Endgame(Mobility[j][i]));
		fprintf(f, "\n");
	}
	fprintf(f, "PasserGeneral\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserGeneral[i]), Endgame(PasserGeneral[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserBlocked\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserBlocked[i]), Endgame(PasserBlocked[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserFree\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserFree[i]), Endgame(PasserFree[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserSupported\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserSupported[i]), Endgame(PasserSupported[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserProtected\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserProtected[i]), Endgame(PasserProtected[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserConnected\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserConnected[i]), Endgame(PasserConnected[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserOutside\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserOutside[i]), Endgame(PasserOutside[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserCandidate\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserCandidate[i]), Endgame(PasserCandidate[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserClear\n");
	for (i = 0; i < 8; i++) fprintf(f, "(%d,%d), ", Opening(PasserClear[i]), Endgame(PasserClear[i]));
	fprintf(f, "\n");

	fprintf(f, "PasserAtt\n");
	for (i = 0; i < 8; i++) fprintf(f, "%d, ", PasserAtt[i]);
	fprintf(f, "\n");

	fprintf(f, "PasserDef\n");
	for (i = 0; i < 8; i++) fprintf(f, "%d, ", PasserDef[i]);
	fprintf(f, "\n");

	fprintf(f, "PasserAttLog\n");
	for (i = 0; i < 8; i++) fprintf(f, "%d, ", PasserAttLog[i]);
	fprintf(f, "\n");

	fprintf(f, "PasserDefLog\n");
	for (i = 0; i < 8; i++) fprintf(f, "%d, ", PasserDefLog[i]);
	fprintf(f, "\n");

	fprintf(f, "StormBlocked\n");
	for (i = 0; i < 4; i++) fprintf(f, "%d, ", StormBlocked[i]);
	fprintf(f, "\n");

	fprintf(f, "StormShelterAtt\n");
	for (i = 0; i < 4; i++) fprintf(f, "%d, ", StormShelterAtt[i]);
	fprintf(f, "\n");

	fprintf(f, "StormConnected\n");
	for (i = 0; i < 4; i++) fprintf(f, "%d, ", StormConnected[i]);
	fprintf(f, "\n");

	fprintf(f, "StormOpen\n");
	for (i = 0; i < 4; i++) fprintf(f, "%d, ", StormOpen[i]);
	fprintf(f, "\n");

	fprintf(f, "StormFree\n");
	for (i = 0; i < 4; i++) fprintf(f, "%d, ", StormFree[i]);
	fprintf(f, "\n");

	fclose(f);
}


// Memo: L2420
void init_eval() {
	int i, j, k, index;
	memset(Mobility,0,4 * 32 * sizeof(int));
	for (i = 0; i < 4; i++) for (j = 0; j < 32; j++) {
		index = i * 2;
		double op = (double)(Av(MobilityLinear,8,0,index) * j) + log(1.01 + (double)j) * (double)(Av(MobilityLog,8,0,index));
		index = i * 2 + 1;
		double eg = (double)(Av(MobilityLinear,8,0,index) * j) + log(1.01 + (double)j) * (double)(Av(MobilityLog,8,0,index));
		Mobility[i][j] = Compose((int)(op/64.0),(int)(eg/64.0));
	}
	
	for (i = 0; i < 3; i++) for (j = 7; j >= 0; j--) {
		Shelter[i][j] = 0;
		if (j > 1) for (k = 1; k < std::min(j, 5); k++) Shelter[i][j] += Av(ShelterValue, 0, 0, (i * 5) + k - 1);
		if (!j) Shelter[i][j] = Shelter[i][7] + Av(ShelterValue, 0, 0, (i * 5) + 4);
	}

	for (i = 0; i < 4; i++) {
		StormBlocked[i] = ((Sa(StormQuad, StormBlockedMul) * i * i) + (Sa(StormLinear, StormBlockedMul) * (i + 1))) / 100;
		StormShelterAtt[i] = ((Sa(StormQuad, StormShelterAttMul) * i * i) + (Sa(StormLinear, StormShelterAttMul) * (i + 1))) / 100;
		StormConnected[i] = ((Sa(StormQuad, StormConnectedMul) * i * i) + (Sa(StormLinear, StormConnectedMul) * (i + 1))) / 100;
		StormOpen[i] = ((Sa(StormQuad, StormOpenMul) * i * i) + (Sa(StormLinear, StormOpenMul) * (i + 1))) / 100;
		StormFree[i] = ((Sa(StormQuad, StormFreeMul) * i * i) + (Sa(StormLinear, StormFreeMul) * (i + 1))) / 100;
	}

	for (i = 0; i < 8; i++) {
		int l = std::max(i - 2, 0);
		int q = l * l;
		PasserGeneral[i] = Compose16(Av(PasserQuad, 2, 0, 0) * q + Av(PasserLinear, 2, 0, 0) * l, Av(PasserQuad, 2, 0, 1) * q + Av(PasserLinear, 2, 0, 1) * l);
		PasserBlocked[i] = Compose16(Av(PasserQuad, 2, 1, 0) * q + Av(PasserLinear, 2, 1, 0) * l, Av(PasserQuad, 2, 1, 1) * q + Av(PasserLinear, 2, 1, 1) * l);
		PasserFree[i] = Compose16(Av(PasserQuad, 2, 2, 0) * q + Av(PasserLinear, 2, 2, 0) * l, Av(PasserQuad, 2, 2, 1) * q + Av(PasserLinear, 2, 2, 1) * l);
		PasserSupported[i] = Compose16(Av(PasserQuad, 2, 3, 0) * q + Av(PasserLinear, 2, 3, 0) * l, Av(PasserQuad, 2, 3, 1) * q + Av(PasserLinear, 2, 3, 1) * l);
		PasserProtected[i] = Compose16(Av(PasserQuad, 2, 4, 0) * q + Av(PasserLinear, 2, 4, 0) * l, Av(PasserQuad, 2, 4, 1) * q + Av(PasserLinear, 2, 4, 1) * l);
		PasserConnected[i] = Compose16(Av(PasserQuad, 2, 5, 0) * q + Av(PasserLinear, 2, 5, 0) * l, Av(PasserQuad, 2, 5, 1) * q + Av(PasserLinear, 2, 5, 1) * l);
		PasserOutside[i] = Compose16(Av(PasserQuad, 2, 6, 0) * q + Av(PasserLinear, 2, 6, 0) * l, Av(PasserQuad, 2, 6, 1) * q + Av(PasserLinear, 2, 6, 1) * l);
		PasserCandidate[i] = Compose16(Av(PasserQuad, 2, 7, 0) * q + Av(PasserLinear, 2, 7, 0) * l, Av(PasserQuad, 2, 7, 1) * q + Av(PasserLinear, 2, 7, 1) * l);
		PasserClear[i] = Compose16(Av(PasserQuad, 2, 8, 0) * q + Av(PasserLinear, 2, 8, 0) * l, Av(PasserQuad, 2, 8, 1) * q + Av(PasserLinear, 2, 8, 1) * l);

		PasserAtt[i] = Av(PasserAttDefQuad, 2, 0, 0) * q + Av(PasserAttDefLinear, 2, 0, 0) * l;
		PasserDef[i] = Av(PasserAttDefQuad, 2, 1, 0) * q + Av(PasserAttDefLinear, 2, 1, 0) * l;
		PasserAttLog[i] = Av(PasserAttDefQuad, 2, 0, 1) * q + Av(PasserAttDefLinear, 2, 0, 1) * l;
		PasserDefLog[i] = Av(PasserAttDefQuad, 2, 1, 1) * q + Av(PasserAttDefLinear, 2, 1, 1) * l;
	}
	for (i = 0; i < 16; i++) LogDist[i] = (int)(10.0 * log(1.01 + (double)i));
}


// Memo: L2687
template <bool me> int krbkrx(const GBoard* Board) {
	constexpr bool opp = !me;
	if (King(opp) & Interior) return 1;
	return 16;
}
template <bool me> int kpkx(const int turn, const GBoard* Board) {
	constexpr bool opp = !me;
	uint64_t u;
	if (me == White) u = Kpk[turn][lsb(Pawn(White))][lsb(King(White))] & Bit(lsb(King(Black)));
	else u = Kpk[turn ^ 1][63 - lsb(Pawn(Black))][63 - lsb(King(Black))] & Bit(63 - lsb(King(White)));

	if (u) return 32;
	else if (Piece(opp) ^ King(opp)) return 1;
	else return 0;
}
template <bool me> int knpkx(const int turn, const GBoard* Board) {
	constexpr bool opp = !me;
	if (Pawn(me) & Line(me, 6) & (File[0] | File[7])) {
		int sq = lsb(Pawn(me));
		if (SArea[sq] & King(opp) & (Line(me, 6) | Line(me, 7))) return 0;
		if (Square(sq + Push(me)) == IKing(me) && (SArea[lsb(King(me))] && SArea[lsb(King(opp))] & Line(me, 7))) return 0;
	} else if (Pawn(me) & Line(me, 5) & (File[0] | File[7])) {
		int sq = lsb(Pawn(me));
		if (Square(sq + Push(me)) == IPawn(opp)) {
			if (SArea[sq + Push(me)] & King(opp) & Line(me, 7)) return 0;
			if ((SArea[sq + Push(me)] & SArea[lsb(King(opp))] & Line(me, 7)) && (!(NAtt[sq + Push(me)] & Knight(me)) || turn == opp)) return 0;
		}
	}
	return 32;
}

#define AtCheck(me)  ((att[(me) ^ 1] & King(me)) != 0)
template <bool me> int krpkrx(const int turn, const uint64_t att[], const GBoard* Board) {
	constexpr bool opp = !me;
	int mul = 32;
	int sq = lsb(Pawn(me));
	int rrank = CRank(me, sq);
	int o_king = lsb(King(opp));
	int o_rook = lsb(Rook(opp));
	int m_king = lsb(King(me));
	int add_mat = (Piece(opp) ^ King(opp) ^ Rook(opp)) != 0;
	int clear = !(add_mat) || !((PWay[opp][sq] | PIsolated[file_of(sq)]) & Forward[opp][rank_of(sq + Push(me))] & (Piece(opp) ^ King(opp) ^ Rook(opp)));

	if (!clear) return 32;
	if (!add_mat && !(Pawn(me) & (File[0] | File[7]))) {
		int m_rook = lsb(Rook(me));
		if (CRank(me, o_king) < CRank(me, m_rook) && CRank(me, m_rook) < rrank && CRank(me, m_king) >= rrank - 1 && CRank(me, m_king) > CRank(me, m_rook)
			&& ((SArea[m_king] & Pawn(me)) || (turn == me && std::abs(file_of(sq) - file_of(m_king)) <= 1 && std::abs(rrank - CRank(me, m_king)) <= 2))) return 128;
		if (SArea[m_king] & Pawn(me)) {
			if (rrank >= 4) {
				if ((file_of(sq) < file_of(m_rook) && file_of(m_rook) < file_of(o_king)) || (file_of(sq) > file_of(m_rook) && file_of(m_rook) > file_of(o_king))) return 128;
			} else if (rrank >= 2) {
				if (!(Pawn(me) & (File[1] | File[6])) && rrank + std::abs(file_of(sq) - file_of(m_rook)) > 4
					&& ((file_of(sq) < file_of(m_rook) && file_of(m_rook) < file_of(o_king)) || (file_of(sq) > file_of(m_rook) && file_of(m_rook) > file_of(o_king)))) return 128;
			}
		}
	}

	if (PWay[me][sq] & King(opp)) {
		if (Pawn(me) & (File[0] | File[7])) mul = std::min(mul, add_mat << 3);
		if (rrank <= 3) mul = std::min(mul, add_mat << 3);
		if (rrank == 4 && CRank(me, m_king) <= 4 && CRank(me, o_rook) == 5 && (King(opp) & (Line(me, 6) | Line(me, 7))) != 0
			&& (turn != me || !(PAtt[me][sq] & RookAttacks(lsb(Rook(me)), PieceAll) & (~SArea[o_king])))) mul = std::min(mul, add_mat << 3);
		if (rrank >= 5 && CRank(me, o_rook) <= 1 && (turn != me || AtCheck(me) || Dist(m_king, sq) >= 2)) mul = std::min(mul, add_mat << 3);
		if ((King(opp) & (File[1] | File[2] | File[6] | File[7])) && (Rook(opp) & Line(me, 7)) && (Between[o_king][o_rook] & (File[3] | File[4])) && !(Rook(me) & Line(me, 7))) mul = std::min(mul, add_mat << 3);
		return mul;
	} else if (rrank == 6 && (Pawn(me) & (File[0] | File[7])) && ((PSupport[me][sq] | PWay[opp][sq]) & Rook(opp)) && CRank(me, o_king) >= 6) {
		int dist = std::abs(file_of(sq) - file_of(o_king));
		if (dist <= 3)  mul = std::min(mul, add_mat << 3);
		if (dist == 4 && ((PSupport[me][o_king] & Rook(me)) || turn == opp)) mul = std::min(mul, add_mat << 3);
	}

	if (SArea[o_king] & PWay[me][sq] & Line(me, 7)) {
		if (rrank <= 4 && CRank(me, m_king) <= 4 && CRank(me, o_rook) == 5) mul = std::min(mul, add_mat << 3);
		if (rrank == 5 && CRank(me, o_rook) <= 1 && turn != me || (!(SArea[m_king] & PAtt[me][sq] & (~SArea[o_king])) && (AtCheck(me) || Dist(m_king, sq) >= 2)))
			mul = std::min(mul, add_mat << 3);
	}

	if (((PWay[me][sq] & Rook(me)) != 0) && ((PWay[opp][sq] & Rook(opp))) != 0) {
		if (King(opp) & (File[0] | File[1] | File[6] | File[7]) & Line(me, 6)) mul = std::min(mul, add_mat << 3);
		else if ((Pawn(me) & (File[0] | File[7])) && (King(opp) & (Line(me, 5) | Line(me, 6))) && std::abs(file_of(sq) - file_of(o_king)) <= 2 && file_of(sq) != file_of(o_king)) mul = std::min(mul, add_mat << 3);
	}

	if (std::abs(file_of(sq) - file_of(o_king)) <= 1 && std::abs(file_of(sq) - file_of(o_rook)) <= 1 && CRank(me, o_rook) > rrank && CRank(me, o_king) > rrank) mul = std::min(mul, (Pawn(me) & (File[3] | File[4])) ? 12 : 16);

	return mul;
}
template <bool me> int krpkbx(const int turn, const GBoard* Board) {
	constexpr bool opp = !me;
	if (!(Pawn(me) & Line(me, 5))) return 32;
	int sq = lsb(Pawn(me));
	if (!(PWay[me][sq] & King(opp))) return 32;
	int diag_sq = NB(me, BMask[sq + Push(me)]);
	if (CRank(me, diag_sq) > 1) return 32;
	uint64_t mdiag = FullLine[sq + Push(me)][diag_sq] | Bit(sq + Push(me)) | Bit(diag_sq);
	int check_sq = NB(me, BMask[sq - Push(me)]);
	uint64_t cdiag = FullLine[sq - Push(me)][check_sq] | Bit(sq - Push(me)) | Bit(check_sq);
	if ((mdiag | cdiag) & (Piece(opp) ^ King(opp) ^ Bishop(opp))) return 32;
	if (cdiag & Bishop(opp)) return 0;
	if ((mdiag & Bishop(opp)) && (turn == opp || !(King(me) & PAtt[opp][sq + Push(me)]))) return 0;
	return 32;
}
template <bool me> int kqkp(const GBoard* Board) {
	constexpr bool opp = !me;
	if (!(SArea[lsb(King(opp))] & Pawn(opp) & Line(me, 1) & (File[0] | File[2] | File[5] | File[7]))) return 32;
	if (PWay[opp][lsb(Pawn(opp))] & (King(me) | Queen(me))) return 32;
	if (Pawn(opp) & (File[0] | File[7])) return 1;
	else return 4;
}
template <bool me> int kqkrpx(const GBoard* Board) {
	constexpr bool opp = !me;
	int rsq = lsb(Rook(opp));
	uint64_t pawns = SArea[lsb(King(opp))] & PAtt[me][rsq] & Pawn(opp) & Interior & Line(me, 6);
	if (pawns && CRank(me, lsb(King(me))) <= 4) return 0;
	return 32;
}
template <bool me> int krkpx(const GBoard* Board) {
	constexpr bool opp = !me;
	if (((SArea[lsb(King(opp))] & Pawn(opp) & Line(me, 1)) != 0) & !(PWay[opp][NB(me, Pawn(opp))] & King(me))) return 0;
	return 32;
}
template <bool me> int krppkrpx(const uint64_t passer, const GBoard* Board) {
	constexpr bool opp = !me;
	if (passer & Pawn(me)) {
		if (Single(passer & Pawn(me))) {
			int sq = lsb(passer & Pawn(me));
			if (PWay[me][sq] & King(opp) & (File[0] | File[1] | File[6] | File[7])) {
				int opp_king = lsb(King(opp));
				if (SArea[opp_king] & Pawn(opp)) {
					int king_file = file_of(opp_king);
					if (!((~(File[king_file] | PIsolated[king_file])) & Pawn(me))) return 1;
				}
			}
		}
		return 32;
	}
	if (!((~(PWay[opp][lsb(King(opp))] | PSupport[me][lsb(King(opp))])) & Pawn(me))) return 0;
	return 32;
}
template <bool me> int krpppkrppx(const uint64_t passer, const GBoard* Board) {
	constexpr bool opp = !me;
	if (((passer & Pawn(me)) != 0) || !((SArea[lsb(Pawn(opp))] | SArea[msb(Pawn(opp))]) & Pawn(opp))) return 32;
	if (!((~(PWay[opp][lsb(King(opp))] | PSupport[me][lsb(King(opp))])) & Pawn(me))) return 0;
	return 32;
}
template <bool me> int kbpkbx(const int turn, const GBoard* Board) {
	constexpr bool opp = !me;
	int sq = lsb(Pawn(me));
	uint64_t u;
	if (((Board->bb[ILight(me)] != 0) && (Board->bb[IDark(opp)] != 0)) || ((Board->bb[IDark(me)] != 0) && (Board->bb[ILight(opp)] != 0))) {
		if (CRank(me, sq) <= 4) return 0;
		if (((PWay[me][sq] & King(opp)) != 0) && CRank(me, sq) <= 5) return 0;
		for (u = Bishop(opp); u != 0; Cut(u)) {
			if (CRank(me, lsb(u)) <= 4 && ((BishopAttacks(lsb(u), PieceAll) & PWay[me][sq]) != 0)) return 0;
			if (turn == opp && ((BishopAttacks(lsb(u), PieceAll) & Pawn(me)) != 0)) return 0;
		}
	} else if (((PWay[me][sq] & King(opp)) != 0) && ((King(opp) & LightArea) != 0) != ((Bishop(me) & LightArea) != 0)) return 0;
	return 32;
}
template <bool me> int kbpknx(const int turn, const GBoard* Board) {
	constexpr bool opp = !me;
	uint64_t u;
	if (((PWay[me][lsb(Pawn(me))] & King(opp)) != 0) && ((King(opp) & LightArea) != 0) != ((Bishop(me) & LightArea) != 0)) return 0;
	if (turn == opp)
	for (u = Knight(opp); u != 0; Cut(u))
	if (NAtt[lsb(u)] & Pawn(me)) return 0;
	return 32;
}
template <bool me> int kbppkbx(const GBoard* Board) {
	constexpr bool opp = !me;
	int sq1 = NB(me, Pawn(me));
	int sq2 = NB(opp, Pawn(me));
	int o_king = lsb(King(opp));
	int o_bishop = lsb(Bishop(opp));

	if (file_of(sq1) == file_of(sq2)) {
		if (CRank(me, sq2) <= 3) return 0;
		if (((PWay[me][sq2] & King(opp)) != 0) && CRank(me, sq2) <= 5) return 0;
	} else if (PIsolated[file_of(sq1)] & Pawn(me)) {
		if (((King(opp) & LightArea) != 0) != ((Bishop(me) & LightArea) != 0)) {
			if ((((SArea[o_king] | King(opp)) & Bit(sq2 + Push(me))) != 0) && ((BishopAttacks(o_bishop, PieceAll) & Bit(sq2 + Push(me))) != 0))
			if ((((SArea[o_king] | King(opp)) & Bit((sq2 & 0xFFFFFFF8) | file_of(sq1))) != 0) && ((BishopAttacks(o_bishop, PieceAll) & Bit((sq2 & 0xFFFFFFF8) | file_of(sq1))) != 0)) return 0;
		}
	}
	return 32;
}
template <bool me> int krppkrx(const GBoard* Board) {
	constexpr bool opp = !me;
	int sq1 = NB(me, Pawn(me));
	int sq2 = NB(opp, Pawn(me));

	if ((Piece(opp) ^ King(opp) ^ Rook(opp)) & Forward[me][rank_of(sq1 - Push(me))]) return 32;
	if (file_of(sq1) == file_of(sq2)) {
		if ((PWay[me][sq2] & King(opp))) return 16;
		return 32;
	}
	if (((PIsolated[file_of(sq2)] & Pawn(me)) != 0) && (((File[0] | File[7]) & Pawn(me)) != 0) && ((King(opp) & Shift(me, Pawn(me))) != 0)) {
		if (CRank(me, sq2) == 5 && CRank(me, sq1) == 4 && ((Rook(opp) & (Line(me, 5) | Line(me, 6))) != 0)) return 10;
		else if (CRank(me, sq2) < 5) return 16;
	}
	return 32;
}

struct GEvalInfo {
	int score, king_w, king_b, mul;
	uint64_t occ, area_w, area_b, free_w, free_b;
	uint32_t king_att_w, king_att_b;
	GMaterial * material;
};


// Memo: L862
#define KingAttack Compose(1, 0)

template <bool me, bool HPopCnt> void Position::eval_queens(GEvalInfo &EI) {
	constexpr bool opp = !me;
	uint64_t u, b;
	for (u = Queen(me); u != 0; u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64_t att = QueenAttacks(sq,EI.occ);
		Current->att[me] |= att;
		if (QMask[sq] & King(opp)) if (uint64_t v = Between[PVarC(EI,king,opp)][sq] & EI.occ) if (Single(v)) {
			Current->xray[me] |= v;
			uint64_t square = lsb(v); int piece = Square(square); int katt = 0;
			if (piece == IPawn(me)) {
				if (!Square(square + Push(me))) IncV(EI.score, Ca(Pin, SelfPawnPin));
			} else if ((piece & 1) == me) {
				IncV(EI.score, Ca(Pin, SelfPiecePin));
				katt = 1;
			} else if (piece != IPawn(opp) && !(((BMask[sq] & Bishop(opp)) | (RMask[sq] & Rook(opp)) | Queen(opp)) & v)) {
				IncV(EI.score, Ca(Pin, WeakPin));
				if (!(Current->patt[opp] & v)) katt = 1;
			}
			if (katt && !(att & PVarC(EI, area, opp))) PVarC(EI, king_att, me) += KingAttack;
		} else if (v == (v & Minor(opp))) IncV(EI.score, Ca(KingRay, QKingRay));
		if (att & PVarC(EI, area, opp)) {
			PVarC(EI, king_att, me) += KingQAttack;
			for (uint64_t v = att & PVarC(EI, area, opp); v != 0; Cut(v))
			if (FullLine[sq][lsb(v)] & att & ((Rook(me) & RMask[sq]) | (Bishop(me) & BMask[sq]))) PVarC(EI, king_att, me)++;
		}
		IncV(EI.score,Mobility[PieceType[WhiteQueen] - 1][popcount<HPopCnt>(att & PVarC(EI,free,me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorPawn));
		if (att & PVarC(EI, free, me) & Minor(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefQueen));
	}
}
template <bool me, bool HPopCnt> void Position::eval_rooks(GEvalInfo &EI) {
	constexpr bool opp = !me;
	uint64_t u, b;
	for (u = Rook(me); u != 0; u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64_t att = RookAttacks(sq,EI.occ);
		Current->att[me] |= att;
		if (RMask[sq] & King(opp)) if (uint64_t v = Between[PVarC(EI, king, opp)][sq] & EI.occ) if (Single(v)) {
			Current->xray[me] |= v;
			uint64_t square = lsb(v); int piece = Square(square); int katt = 0;
			if (piece == IPawn(me)) {
				if (!Square(square + Push(me))) IncV(EI.score, Ca(Pin, SelfPawnPin));
			} else if ((piece & 1) == me) {
				IncV(EI.score, Ca(Pin, SelfPiecePin));
				katt = 1;
			} else if (piece != IPawn(opp)) {
				if (piece < IRook(opp)) {
					IncV(EI.score, Ca(Pin, WeakPin));
					if (!(Current->patt[opp] & v)) katt = 1;
				} else if (piece == IQueen(opp)) IncV(EI.score, Ca(Pin, ThreatPin));
			}
			if (katt && !(att & PVarC(EI, area, opp))) PVarC(EI, king_att, me) += KingAttack;
		} else if (v == (v & (Minor(opp) | Queen(opp)))) IncV(EI.score, Ca(KingRay, RKingRay));
		if (att & PVarC(EI, area, opp)) {
			PVarC(EI, king_att, me) += KingRAttack;
			for (uint64_t v = att & PVarC(EI, area, opp); v != 0; Cut(v))
			if (FullLine[sq][lsb(v)] & att & Major(me)) PVarC(EI, king_att, me)++;
		}
		IncV(EI.score,Mobility[PieceType[WhiteRook] - 1][popcount<HPopCnt>(att & PVarC(EI,free,me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorPawn));
		if (att & PVarC(EI, free, me) & Minor(opp)) IncV(EI.score, Ca(Tactical, TacticalMajorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefRook));
		Current->threat |= att & Queen(opp);
		if (!(PWay[me][sq] & Pawn(me))) {
			IncV(EI.score, Ca(RookSpecial, RookHof));
			int hof_score = 0;
			if (!(PWay[me][sq] & Pawn(opp))) {
				IncV(EI.score, Ca(RookSpecial, RookOf));
				if (att & Line(me, 7)) hof_score += Ca(RookSpecial, RookOfOpen);
				else if (uint64_t target = att & PWay[me][sq] & Minor(opp)) {
					if (!(Current->patt[opp] & target)) {
						hof_score += Ca(RookSpecial, RookOfMinorHaging);
						if (PWay[me][sq] & King(opp)) hof_score += Ca(RookSpecial, RookOfKingAtt);
					} else hof_score += Ca(RookSpecial, RookOfMinorFixed);
				}
			} else if (att & PWay[me][sq] & Pawn(opp)) {
				uint64_t square = lsb(att & PWay[me][sq] & Pawn(opp));
				if (!(PSupport[opp][square] & Pawn(opp))) hof_score += Ca(RookSpecial, RookHofWeakPAtt);
			}
			IncV(EI.score, hof_score);
			if (PWay[opp][sq] & att & Major(me)) IncV(EI.score, hof_score);
		}
		if ((b & Line(me, 6)) && ((King(opp) | Pawn(opp)) & (Line(me, 6) | Line(me, 7)))) {
			IncV(EI.score, Ca(RookSpecial, Rook7th));
			if (King(opp) & Line(me, 7)) IncV(EI.score, Ca(RookSpecial, Rook7thK8th));
			if (Major(me) & att & Line(me, 6)) IncV(EI.score, Ca(RookSpecial, Rook7thDoubled));
		}
	}
}
template <bool me, bool HPopCnt> void Position::eval_bishops(GEvalInfo &EI) {
	constexpr bool opp = !me;
	uint64_t u, b;
	for (u = Bishop(me); u != 0; u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64_t att = BishopAttacks(sq, EI.occ);
		Current->att[me] |= att;
		if (BMask[sq] & King(opp)) if (uint64_t v = Between[PVarC(EI, king, opp)][sq] & EI.occ) if (Single(v)) {
			Current->xray[me] |= v;
			uint64_t square = lsb(v); int piece = Square(square); int katt = 0;
			if (piece == IPawn(me)) {
				if (!Square(square + Push(me))) IncV(EI.score, Ca(Pin, SelfPawnPin));
			} else if ((piece & 1) == me) {
				IncV(EI.score, Ca(Pin, SelfPiecePin));
				katt = 1;
			} else if (piece != IPawn(opp)) {
				if (piece < ILight(opp)) {
					IncV(EI.score, Ca(Pin, StrongPin));
					if (!(Current->patt[opp] & v)) katt = 1;
				} else if (piece >= IRook(opp)) IncV(EI.score, Ca(Pin, ThreatPin));
			}
			if (katt && !(att & PVarC(EI, area, opp))) PVarC(EI, king_att, me) += KingAttack;
		} else if (v == (v & (Knight(opp) | Major(opp)))) IncV(EI.score, Ca(KingRay, BKingRay));
		if (att & PVarC(EI, area, opp)) PVarC(EI, king_att, me) += KingBAttack;
		IncV(EI.score, Mobility[PieceType[WhiteLight] - 1][popcount<HPopCnt>(att & PVarC(EI, free, me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorPawn));
		if (att & PVarC(EI, free, me) & Knight(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefBishop));
		Current->threat |= att & Major(opp);
		if (b & LightArea) {
			for (uint64_t v = ((~BishopForward[me][sq]) | (att & Forward[me][rank_of(sq)])) & Pawn(opp) & (~Current->patt[opp]) & LightArea; v; Cut(v)) {
				uint64_t square = lsb(v);
				if (!((PSupport[opp][square] | PWay[opp][square]) & Pawn(opp))) IncV(EI.score, Ca(BishopSpecial, BishopNonForwardPawn));
			}
			uint64_t v = BishopForward[me][sq] & Pawn(me) & LightArea;
			v |= (v & (File[2] | File[3] | File[4] | File[5] | BMask[sq])) >> 8;
			DecV(EI.score, Ca(BishopSpecial, BishopPawnBlock) * popcount<HPopCnt>(v));
		} else {
			for (uint64_t v = ((~BishopForward[me][sq]) | (att & Forward[me][rank_of(sq)])) & Pawn(opp) & (~Current->patt[opp]) & DarkArea; v; Cut(v)) {
				uint64_t square = lsb(v);
				if (!((PSupport[opp][square] | PWay[opp][square]) & Pawn(opp))) IncV(EI.score, Ca(BishopSpecial, BishopNonForwardPawn));
			}
			uint64_t v = BishopForward[me][sq] & Pawn(me) & DarkArea;
			v |= (v & (File[2] | File[3] | File[4] | File[5] | BMask[sq])) >> 8;
			DecV(EI.score, Ca(BishopSpecial, BishopPawnBlock) * popcount<HPopCnt>(v));
		}
	}
}
template <bool me, bool HPopCnt> void Position::eval_knights(GEvalInfo &EI) {
	constexpr bool opp = !me;
	uint64_t u, b;
	for (u = Knight(me); u != 0; u ^= b) {
		int sq = lsb(u);
		b = Bit(sq);
		uint64_t att = NAtt[sq];
		Current->att[me] |= att;
		if (att & PVarC(EI, area, opp)) PVarC(EI, king_att, me) += KingNAttack;
		IncV(EI.score, Mobility[PieceType[WhiteKnight] - 1][popcount<HPopCnt>(att & PVarC(EI, free, me))]);
		if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorPawn));
		if (att & PVarC(EI, free, me) & Bishop(opp)) IncV(EI.score, Ca(Tactical, TacticalMinorMinor));
		if (att & PVarC(EI, area, me)) IncV(EI.score, Ca(KingDefence, KingDefKnight));
		Current->threat |= att & Major(opp);
		if ((b & Outpost[me]) && !(Pawn(opp) & PIsolated[file_of(sq)] & Forward[me][rank_of(sq)])) {
			IncV(EI.score, Ca(KnightSpecial, KnightOutpost));
			if (Current->patt[me] & b) {
				IncV(EI.score, Ca(KnightSpecial, KnightOutpostProtected));
				if (att & PVarC(EI, free, me) & Pawn(opp)) IncV(EI.score, Ca(KnightSpecial, KnightOutpostPawnAtt));
				if (att & PVarC(EI, free, me) & Bishop(opp)) IncV(EI.score, Ca(KnightSpecial, KnightOutpostBishopAtt));
			}
		}
	}
}
template <bool me, bool HPopCnt> void Position::eval_king(GEvalInfo &EI) {
	constexpr bool opp = !me;
	int cnt = Opening(PVarC(EI, king_att, me));
	int score = Endgame(PVarC(EI, king_att, me));
	if (cnt >= 2 && (Queen(me) != 0)) {
		if (uint64_t u = Current->att[me] & PVarC(EI, area, opp) & (~Current->att[opp])) score += popcount<HPopCnt>(u) * KingAttackSquare;
		if (!(SArea[PVarC(EI, king, opp)] & (~(Piece(opp) | Current->att[me])))) score += KingNoMoves;
	}
	int adjusted = ((score * KingAttackScale[cnt]) >> 3);
	if (!Queen(me)) adjusted /= 2;
	IncV(EI.score, adjusted);
}
template <bool me, bool HPopCnt> void Position::eval_pieces(GEvalInfo &EI) {
	constexpr bool opp = !me;
	Current->threat |= Current->att[opp] & (~Current->att[me]) & Piece(me);
	if (uint64_t u = Current->threat & Piece(me)) {
		DecV(EI.score, Ca(Tactical, TacticalThreat));
		Cut(u);
		if (u) {
			DecV(EI.score, Ca(Tactical, TacticalThreat) + Ca(Tactical, TacticalDoubleThreat));
			for (Cut(u); u; Cut(u)) DecV(EI.score, Ca(Tactical, TacticalThreat));
		}
	}
}
template <bool me, bool HPopCnt> void Position::eval_endgame(GEvalInfo &EI) {
	constexpr bool opp = !me;
	if ((EI.material->flags & VarC(FlagSingleBishop, me)) && Pawn(me)) {
		int sq = (Board->bb[ILight(me)] ? (me ? 0 : 63) : (Board->bb[IDark(me)] ? (me ? 7 : 56) : (file_of(lsb(King(opp))) <= 3 ? (me ? 0 : 56) : (me ? 7 : 63))));
		if (!(Pawn(me) & (~PWay[opp][sq]))) {
			if ((SArea[sq] | Bit(sq)) & King(opp)) EI.mul = 0;
			else if ((SArea[sq] & SArea[lsb(King(opp))] & Line(me, 7)) && Square(sq - Push(me)) == IPawn(opp) && Square(sq - 2 * Push(me)) == IPawn(me)) EI.mul = 0;
		} else if ((King(opp) & Line(me, 6) | Line(me, 7)) && std::abs(file_of(sq) - file_of(lsb(King(opp)))) <= 3 && !(Pawn(me) & (~PSupport[me][sq])) && (Pawn(me) & Line(me, 5) & Shift(opp, Pawn(opp)))) EI.mul = 0;
		if (Single(Pawn(me))) {
			if (!Bishop(me)) {
				EI.mul = MinF(EI.mul, kpkx<me>(Current->turn, Board));
				if (Piece(opp) == King(opp) && EI.mul == 32) IncV(Current->score, KpkValue);
			} else {
				sq = lsb(Pawn(me));
				if ((Pawn(me) & (File[1] | File[6]) & Line(me, 5)) && Square(sq + Push(me)) == IPawn(opp) && ((PAtt[me][sq + Push(me)] | PWay[me][sq + Push(me)]) & King(opp))) EI.mul = 0;
			}
		}
		if (Bishop(opp) && Single(Bishop(opp)) && ((BB(ILight(me))) != 0) != ((BB(ILight(opp))) != 0)) {
			int pcnt = 0;
			if (((King(opp) & LightArea) != 0) == ((Bishop(opp) & LightArea) != 0)) {
				for (uint64_t u = Pawn(me); u; Cut(u)) {
					if (pcnt >= 2) goto check_for_partial_block;
					pcnt++;
					int sq = lsb(u);
					if (!(PWay[me][sq] & (PAtt[me][PVarC(EI, king, opp)] | PAtt[opp][PVarC(EI, king, opp)]))) {
						if (!(PWay[me][sq] & Pawn(opp))) goto check_for_partial_block;
						int bsq = lsb(Bishop(opp));
						uint64_t att = BishopAttacks(bsq, EI.occ);
						if (!(att & PWay[me][sq] & Pawn(opp))) goto check_for_partial_block;
						if (!(BishopForward[me][bsq] & att & PWay[me][sq] & Pawn(opp)) && popcount<HPopCnt>(FullLine[lsb(att & PWay[me][sq] & Pawn(opp))][bsq] & att) <= 2)  goto check_for_partial_block;
					}
				}
				EI.mul = 0;
				return;
			}
		check_for_partial_block:
			if (pcnt <= 2 && Multiple(Pawn(me)) && !Pawn(opp) && !(Pawn(me) & Boundary) && EI.mul) {
				int sq1 = lsb(Pawn(me));
				int sq2 = msb(Pawn(me));
				int fd = std::abs(file_of(sq2) - file_of(sq1));
				if (fd >= 5) EI.mul = 32;
				else if (fd >= 4) EI.mul = 26;
				else if (fd >= 3) EI.mul = 20;
			}
			if ((SArea[PVarC(EI, king, opp)] | Current->patt[opp]) & Bishop(opp)) {
				uint64_t push = Shift(me, Pawn(me));
				if (!(push & (~(Piece(opp) | Current->att[opp]))) && (King(opp) & (Board->bb[ILight(opp)] ? LightArea : DarkArea))) {
					EI.mul = std::min(EI.mul, 8);
					int bsq = lsb(Bishop(opp));
					uint64_t att = BishopAttacks(bsq, EI.occ);
					uint64_t prp = (att | SArea[PVarC(EI, king, opp)]) & Pawn(opp) & (Board->bb[ILight(opp)] ? LightArea : DarkArea);
					uint64_t patt = ShiftW(opp, prp) | ShiftE(opp, prp);
					if ((SArea[PVarC(EI, king, opp)] | patt) & Bishop(opp)) {
						uint64_t double_att = (SArea[PVarC(EI, king, opp)] & patt) | (patt & att) | (SArea[PVarC(EI, king, opp)] & att);
						if (!(push & (~(King(opp) | Bishop(opp) | prp | double_att)))) {
							EI.mul = 0;
							return;
						}
					}
				}
			}
		}
	}
	if (!(Major(me))) {
		if ((Bishop(me) != 0) && !(Knight(me)) && Single(Bishop(me)) && (Pawn(me) != 0)) {
			int number = popcount<HPopCnt>(Pawn(me));
			if (number == 1) {
				if (Bishop(opp)) EI.mul = MinF(EI.mul, kbpkbx<me>(Current->turn, Board));
				else if (Knight(opp)) EI.mul = MinF(EI.mul, kbpknx<me>(Current->turn, Board));
			} else if (number == 2 && (Bishop(opp) != 0)) EI.mul = MinF(EI.mul, kbppkbx<me>(Board));
		} else if (!Bishop(me) && Knight(me) && Single(Knight(me)) && Pawn(me) && Single(Pawn(me))) EI.mul = MinF(EI.mul, knpkx<me>(Current->turn, Board));
	} else if (!(Minor(me))) {
		if (!(Pawn(me)) && !(Rook(me)) && (Queen(me) != 0) && (Pawn(opp) != 0)) {
			if (!(NonPawnKing(opp)) && Single(Pawn(opp))) EI.mul = MinF(EI.mul, kqkp<me>(Board));
			else if (Rook(opp)) EI.mul = MinF(EI.mul, kqkrpx<me>(Board));
		} else if (!(Queen(me)) && (Rook(me) != 0) && Single(Rook(me))) {
			int number = popcount<HPopCnt>(Pawn(me));
			if (number <= 3) {
				if (number == 0) {
					if (Pawn(opp)) EI.mul = MinF(EI.mul, krkpx<me>(Board));
				} else if (Rook(opp)) {
					if (number == 1) {
						int new_mul = krpkrx<me>(Current->turn, Current->att, Board);
						EI.mul = (new_mul <= 32 ? std::min(EI.mul, new_mul) : new_mul);
					} else {
						if (number == 2) EI.mul = MinF(EI.mul, krppkrx<me>(Board));
						if (Pawn(opp)) {
							if (number == 2) EI.mul = MinF(EI.mul, krppkrpx<me>(Current->passer, Board));
							else if (Multiple(Pawn(opp))) EI.mul = MinF(EI.mul, krpppkrppx<me>(Current->passer, Board));
						}
					}
				} else if (number == 1 && Bishop(opp)) EI.mul = MinF(EI.mul, krpkbx<me>(Current->turn, Board));
			}
		}
	} else if (!Pawn(me) && Single(Rook(me)) && !Queen(me) && Single(Bishop(me)) && !Knight(me) && Rook(opp)) EI.mul = MinF(EI.mul, krbkrx<me>(Board));
	if (!(NonPawnKing(opp)) && Current->turn == opp && !(Current->att[me] & King(opp)) && !(SArea[PVarC(EI, king, opp)] & (~(Current->att[me] | Piece(opp))))
		&& !(Current->patt[opp] & Piece(me)) && !(Shift(opp, Pawn(opp)) & (~EI.occ)))
		EI.mul = 0;
}
template <bool HPopCnt> void Position::eval_unusual_material(const int turn, GEvalInfo &EI) {
	int wp, bp, wlight, blight, wr, br, wq, bq;
	wp = popcount<HPopCnt>(Pawn(White));
	bp = popcount<HPopCnt>(Pawn(Black));
	wlight = popcount<HPopCnt>(Minor(White));
	blight = popcount<HPopCnt>(Minor(Black));
	wr = popcount<HPopCnt>(Rook(White));
	br = popcount<HPopCnt>(Rook(Black));
	wq = popcount<HPopCnt>(Queen(White));
	bq = popcount<HPopCnt>(Queen(Black));
	int phase = std::min(24, (wlight + blight) + 2 * (wr + br) + 4 * (wq + bq));
	int mat_score = SeeValue[WhitePawn] * (wp - bp) + SeeValue[WhiteKnight] * (wlight - blight) + SeeValue[WhiteRook] * (wr - br) + SeeValue[WhiteQueen] * (wq - bq);
	mat_score = Compose(mat_score,mat_score);
	Current->score = (((Opening(mat_score + EI.score) * phase) + (Endgame(mat_score + EI.score) * (24 - phase)))/24);
	if (turn) Current->score = -Current->score;
	UpdateDelta
}

template <bool HPopCnt> void Position::evaluation() {
	GEvalInfo EI;
	const auto turn = Current->turn;

	if (Current->eval_key == Current->key) return;
	Current->eval_key = Current->key;

	EI.king_w = lsb(King(White));
	EI.king_b = lsb(King(Black));
	EI.occ = PieceAll;
	Current->patt[White] = ShiftW(White,Pawn(White)) | ShiftE(White,Pawn(White));
	Current->patt[Black] = ShiftW(Black,Pawn(Black)) | ShiftE(Black,Pawn(Black));
	EI.area_w = (SArea[EI.king_w] | King(White)) & ((~Current->patt[White]) | Current->patt[Black]);
	EI.area_b = (SArea[EI.king_b] | King(Black)) & ((~Current->patt[Black]) | Current->patt[White]);
	Current->att[White] = Current->patt[White];
	Current->att[Black] = Current->patt[Black];
	Current->passer = 0;
	Current->threat = (Current->patt[White] & NonPawn(Black)) | (Current->patt[Black] & NonPawn(White));
	EI.score = Current->pst;

#define me  White
#define opp Black
	Current->xray[me] = 0;
	PVarC(EI, free, me) = Queen(opp) | King(opp) | (~(Current->patt[opp] | Pawn(me) | King(me)));
	DecV(EI.score, popcount<HPopCnt>(Shift(opp, EI.occ) & Pawn(me)) * Ca(PawnSpecial, PawnBlocked));
	if (Current->patt[me] & PVarC(EI, area, opp)) PVarC(EI, king_att, me) = KingAttack;
	else PVarC(EI, king_att, me) = 0;
	eval_queens<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Rook(opp);
	eval_rooks<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Minor(opp);
	eval_bishops<me, HPopCnt>(EI);
	eval_knights<me, HPopCnt>(EI);
#undef me
#undef opp
#define me  Black
#define opp White
	Current->xray[me] = 0;
	PVarC(EI, free, me) = Queen(opp) | King(opp) | (~(Current->patt[opp] | Pawn(me) | King(me)));
	DecV(EI.score, popcount<HPopCnt>(Shift(opp, EI.occ) & Pawn(me)) * Ca(PawnSpecial, PawnBlocked));
	if (Current->patt[me] & PVarC(EI, area, opp)) PVarC(EI, king_att, me) = KingAttack;
	else PVarC(EI, king_att, me) = 0;
	eval_queens<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Rook(opp);
	eval_rooks<me, HPopCnt>(EI);
	PVarC(EI, free, me) |= Minor(opp);
	eval_bishops<me, HPopCnt>(EI);
	eval_knights<me, HPopCnt>(EI);
#undef me
#undef opp

	eval_king<White, HPopCnt>(EI);
	eval_king<Black, HPopCnt>(EI);
	Current->att[White] |= SArea[EI.king_w];
	Current->att[Black] |= SArea[EI.king_b];

	eval_pieces<White, HPopCnt>(EI);
	eval_pieces<Black, HPopCnt>(EI);

	if (Current->material & FlagUnusualMaterial) {
		eval_unusual_material<HPopCnt>(turn, EI);
		return;
	}
	EI.material = &Material[Current->material];
	Current->score = EI.material->score + (((Opening(EI.score) * EI.material->phase) + (Endgame(EI.score) * (128 - (int)EI.material->phase)))/128);

	if (Current->ply >= 50) Current->score /= 2;
	if (Current->score > 0) {
		EI.mul = EI.material->mul[White];
		if (EI.material->flags & FlagCallEvalEndgame_w) eval_endgame<White, HPopCnt>(EI);
	} else if (Current->score < 0) {
		EI.mul = EI.material->mul[Black];
		if (EI.material->flags & FlagCallEvalEndgame_b) eval_endgame<Black, HPopCnt>(EI);
	} else EI.mul = std::min(EI.material->mul[White], EI.material->mul[Black]);
	Current->score = (Current->score * EI.mul)/32;

	if (turn) Current->score = -Current->score;
	UpdateDelta
}

void evaluate(Position& pos) {
////	HardwarePopCnt ? evaluation<1>() : evaluation<0>();
	pos.evaluation<1>();
}


