/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2017 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt"
*/

#if !defined(MISC_H_INCLUDED)
#define MISC_H_INCLUDED

#include <cstdint>
#include <iostream>

// assert()だとcygwinのgdbでは意図したところで止まってくれないので、例外を発生させて強制的に停止させる.
#define ASSERT(x)   if (!(x)) do { std::cout << "\nError:" << __FILE__ << ":" << __LINE__ << ":" #x << std::endl; *(int*)0 = 0; } while (0)

enum IOSTREAM_LOCK { IO_LOCK, IO_UNLOCK };
std::ostream& operator<<(std::ostream&, IOSTREAM_LOCK);

#define sync_cout std::cout << IO_LOCK
#define sync_endl std::endl << IO_UNLOCK

///extern void cout_lock();
///extern void cout_unlock();

extern int64_t get_time();

#endif
