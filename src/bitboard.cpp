#include <cstdio>

#include "bitboard.h"

bitboard_t Forward[2][8];
bitboard_t West[8];
bitboard_t East[8];
bitboard_t PIsolated[8];
bitboard_t HLine[64];
bitboard_t VLine[64];
bitboard_t NDiag[64];
bitboard_t SDiag[64];
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
