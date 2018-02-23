/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#include <chrono>
#include "misc.h"

/*
 * Get the time in milliseconds.
 */
int64_t get_time(void)
{
	auto t = std::chrono::steady_clock::now().time_since_epoch();
	std::chrono::milliseconds p = std::chrono::duration_cast<std::chrono::seconds>(t);
	return static_cast<int64_t>(p.count());
}
