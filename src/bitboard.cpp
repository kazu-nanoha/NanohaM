#include <cstdio>

#include "bbtables.h"

bitboard_t Forward[2][8];
bitboard_t West[8];
bitboard_t East[8];
bitboard_t PIsolated[8];
bitboard_t RMask[64];
bitboard_t BMask[64];
bitboard_t QMask[64];
bitboard_t BMagicMask[64];
bitboard_t RMagicMask[64];
bitboard_t NAtt[64];
bitboard_t SArea[64];
bitboard_t DArea[64];
bitboard_t NArea[64];
bitboard_t BishopForward[2][64];
bitboard_t PAtt[2][64];
bitboard_t PMove[2][64];
bitboard_t PWay[2][64];
bitboard_t PSupport[2][64];
bitboard_t Between[64][64];
bitboard_t FullLine[64][64];

#if defined(SHOGI)
extern bitboard_t BBSilver_attackB[SQ_SIZE];
extern bitboard_t BBSilver_attackW[SQ_SIZE];

void init_silver()
{
	constexpr int dx[] = {-1, 0, +1, -1, +1};
	constexpr int dy[] = {-1, -1, -1, +1, +1};
	for (int sq = 0; sq < SQ_SIZE; sq++) {
		int rank = rank_of(sq);
		int file = file_of(sq);
		BBSilver_attackB[sq] = Empty;
		for (int i = 0; i < 5; i++) {
			if (valid_file(file + dx[i]) && valid_rank(rank + dy[i])) {
				BBSilver_attackB[sq] |= Bit(make_sq(rank + dy[i], file + dx[i]));
			}
			if (valid_file(file - dx[i]) && valid_rank(rank - dy[i])) {
				BBSilver_attackB[sq] |= Bit(make_sq(rank - dy[i], file - dx[i]));
			}
		}
	}
}
#endif // defined(SHOGI)

#if defined(BBTEST)
NotBB::operator bitboard_t() const
{
	bitboard_t a = {.xmm = _mm_xor_si128(bb.xmm, Filled.xmm)};
	return a;
}
#endif // defined(BBTEST)

#if defined(BBTEST)
#include <cstdio>

#include "types.h"

bitboard_t a = {0xFEDCBA9876543210ULL, 0xFEDCBA9876543210ULL};

template <bool lf = true> void print_bb(const bitboard_t bb, const char *str = nullptr)
{
	if (str)
		printf("%s:", str);
	printf("0x%016llX %016llX", bb.u64[1], bb.u64[0]);
	if (lf)
		printf("\n");
}
void PrintBB(const bitboard_t bb, const char *str = nullptr)
{
	print_bb(bb, str);
	for (int r = 0; r < 9; r++) {
		for (int f = 8; f >= 0; f--) {
			printf(" %d ", (bb & Bit(make_sq(r, f))) != Empty ? 1 : 0);
		}
		printf("\n");
	}
}

int main()
{
	print_bb(a, "a   = ");
	print_bb(Bit(1), "Bit(1) = ");
	print_bb(Bit(4), "Bit(4) = ");
	print_bb(Bit(5), "Bit(5) = ");
	print_bb(a & Bit(4), "a & Bit(4) = ");
	print_bb(a & Bit(5), "a & Bit(5) = ");
	print_bb(Empty, "Empty  = ");
	print_bb(Filled, "Filled = ");

	PrintBB(Boundary, "Boundary");
	PrintBB(Interior, "Interior");

	return 0;
}

#endif
