/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(TT_H_INCLUDED)
#define TT_H_INCLUDED

#include <cstring>
#include "types.h"

struct GPVEntry {
	uint32_t key32;
	uint16_t date;
	uint16_t move16;
	int16_t value;
	int16_t exclusion;
	uint8_t depth;
	uint8_t ex_depth;
	int knodes;
	int ply;
};

struct GEntry {
    uint32_t key32;
	uint16_t date;
	uint16_t move16;
	int16_t low;
	int16_t high;
	uint16_t flags;
	uint8_t low_depth;
	uint8_t high_depth;
};

static_assert(sizeof(GPVEntry)==24, "GPVEntry size error!");
static_assert(sizeof(GEntry)==16, "GEntry size error!");

constexpr uint32_t pv_cluster_size = 4;
struct PVHash {
	static void init();
	static GPVEntry * probe(uint64_t key);
	static GPVEntry * top(uint64_t key);
	static void rewind() {uint32_t i; GPVEntry * PVEntry; for (PVEntry = hash_table, i = 0; i < hash_size; i++, PVEntry++) PVEntry->date = 1;}
	static void clear() {memset(hash_table, 0, hash_size * sizeof(GPVEntry));}

	static constexpr uint32_t hash_size = (1024 * 1024);
	static constexpr uint32_t hash_mask = (1024 * 1024 - pv_cluster_size);
	static GPVEntry hash_table[hash_size];
};

constexpr uint32_t TT_cluster_size = 4;
struct TranspositionTable {
	static void init();
	static GEntry * probe(uint64_t key);
	static GEntry * top(uint64_t key);
	static void resize(size_t hashSize);
	static void rewind() {GEntry *Entry = hash_table; for (uint64_t i = 0; i < hash_size; i++) {(Entry++)->date = 1;}}
	static void clear() {memset(hash_table, 0, hash_size * sizeof(GEntry));}
	static void free() {delete[] hash_table; hash_table = nullptr;}

	static GEntry * hash_table;
	static uint16_t date;
	static uint64_t hash_size;
	static uint64_t hash_mask;
};

extern PVHash PVHASH;
extern TranspositionTable TT;

#endif
