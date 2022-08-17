/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#include <cstdio>
#include <cstdint>
#include <cassert>

#include "misc.h"
#include "TT.h"

namespace {
///	int date;
	constexpr int initial_hash_size = (1024 * 1024);
}

PVHash PVHASH;
TranspositionTable TT;

GPVEntry PVHash::hash_table[PVHash::hash_size];
GEntry * TranspositionTable::hash_table;
uint16_t TranspositionTable::date;
uint64_t TranspositionTable::hash_size = initial_hash_size;
uint64_t TranspositionTable::hash_mask = (initial_hash_size - TT_cluster_size);

GPVEntry * PVHash::top(uint64_t key)
{
	return hash_table + (High32(key) & hash_mask);
}

GPVEntry * PVHash::probe(uint64_t key)
{
    uint32_t i;
    GPVEntry * PVEntry = top(key);
    for (i = 0; i < pv_cluster_size; i++, PVEntry++) {
        if (Low32(key) == PVEntry->key32) {
            PVEntry->date = date;
            return PVEntry;
        }
    }
    return NULL;
}

void TranspositionTable::init()
{
	hash_table = new GEntry[hash_size];
}

GEntry * TranspositionTable::probe(uint64_t key)
{
	ASSERT(hash_table);
    for (GEntry * Entry = hash_table + (High32(key) & hash_mask); Entry < (hash_table + (High32(key) & hash_mask)) + 4; Entry++) if (Low32(key) == Entry->key32) {
        Entry->date = date;
        return Entry;
    }
    return NULL;
}

GEntry * TranspositionTable::top(uint64_t key) {
	ASSERT(hash_table);
    return hash_table + (High32(key) & hash_mask);
}

void TranspositionTable::resize(size_t hashSize)
{
	if (hash_size == hashSize / sizeof(GEntry)) return;

    hash_size = hashSize / sizeof(GEntry);
    ASSERT((hash_size & (hash_size - 1)) == 0);
	hash_table = new GEntry[hash_size];
    hash_mask = hash_size - 4;
}
