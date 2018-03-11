/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#include <chrono>
#include <mutex>
#include "misc.h"

namespace {
	std::mutex mtx_cout;


	void cout_lock()
	{
		mtx_cout.lock();
	}
	void cout_unlock()
	{
		mtx_cout.unlock();
	}
} // namespace

std::ostream& operator<<(std::ostream& os, IOSTREAM_LOCK m)
{
	if (m == IO_LOCK) cout_lock();
	else              cout_unlock();
	return os;
}


/*
 * Get the time in milliseconds.
 */
int64_t get_time(void)
{
	auto t = std::chrono::steady_clock::now().time_since_epoch();
	std::chrono::milliseconds p = std::chrono::duration_cast<std::chrono::seconds>(t);
	return static_cast<int64_t>(p.count());
}
