/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(THREAD_H_INCLUDED)
#define THREAD_H_INCLUDED

// Memo: L721
#define MaxSplitPoints 64 // mustn't exceed 64

struct GPos {
	GPosData Position[1];
	uint64_t stack[100];
	uint16_t killer[16][2];
	int sp, date;
};


// Memo: L732
#define FlagClaimed (1 << 1)
#define FlagFinished (1 << 2)

struct GMove {
	volatile uint16_t move;
	volatile uint8_t reduced_depth, research_depth, stage, ext, id, flags;
};

struct GSP {
	volatile LONG lock;
	volatile int claimed, active, finished, pv, move_number, current, depth, alpha, beta, singular, split, best_move, height; 
	GMove move[128];
	jmp_buf jump;
	GPos Pos[1];
};

struct GSMPI {
	volatile long long nodes, active_sp, searching;
#ifndef W32_BUILD
	volatile long long stop, fail_high;
#else
	volatile long stop, fail_high;
#endif
	volatile int64_t hash_size;
	volatile int PrN;
	GSP Sp[MaxSplitPoints];
};

#define SharedMaterialOffset (sizeof(GSMPI))
#define SharedMagicOffset (SharedMaterialOffset + TotalMat * sizeof(GMaterial))
#define SharedPVHashOffset (SharedMagicOffset + magic_size * sizeof(uint64_t))

extern GSMPI * Smpi;


// Memo: L770
#ifndef W32_BUILD
#define SET_BIT(var,bit) (InterlockedOr(&(var),1 << (bit)))
#define SET_BIT_64(var,bit) (InterlockedOr64(&(var),Bit(bit)));
#define ZERO_BIT_64(var,bit) (InterlockedAnd64(&(var),~Bit(bit)));
#define TEST_RESET_BIT(var,bit) (InterlockedBitTestAndReset64(&(var),bit))
#define TEST_RESET(var) (InterlockedExchange64(&(var),0))
#else
#define SET_BIT(var,bit) (_InterlockedOr(&(var),1 << (bit)))
#define SET_BIT_64(var,bit) {if ((bit) < 32) _InterlockedOr((LONG*)&(var),1 << (bit)); else _InterlockedOr(((LONG*)(&(var))) + 1,1 << ((bit) - 32));}
#define ZERO_BIT_64(var,bit) {if ((bit) < 32) _InterlockedAnd((LONG*)&(var),~(1 << (bit))); else _InterlockedAnd(((LONG*)(&(var))) + 1,~(1 << ((bit) - 32)));}
#define TEST_RESET_BIT(var,bit) (InterlockedBitTestAndReset(&(var),bit))
#define TEST_RESET(var) (InterlockedExchange(&(var),0))
#endif
#define SET(var,value) (InterlockedExchange(&(var),value))

#define LOCK(lock) {while (InterlockedCompareExchange(&(lock),1,0)) _mm_pause();}
#define UNLOCK(lock) {SET(lock,0);}

void retrieve_position(GPos * Pos, int copy_stack);

void halt_all(GSP * Sp, int locked);
void halt_all(int from, int to);

#endif
