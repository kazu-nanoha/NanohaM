/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#include <cstring>
#include "types.h"
#include "misc.h"
#include "position.h"	// ToDo: 不要か判断する.
#include "thread.h"
#include "usi.h"

ThreadPool Threads;

Thread::Thread(int n)
: id(n)
{
	// ToDo:
	req_exit = is_searching = is_ready = false;
	is_waiting = true;

	std::unique_lock<std::mutex> lk(mtx);
	thr = std::thread(&Thread::worker, this);
	condi_req.notify_one();
	while (is_waiting.load(std::memory_order_acquire)) {
		condi_ans.wait(lk);
	}
}

Thread::~Thread()
{
	// ToDo:
	is_searching = true;
	req_exit.store(true, std::memory_order_release);
	condi_req.notify_one();
	thr.join();
	sync_cout << "Detach thread No." << id << sync_endl;
}

void Thread::search()
{
	// ToDo:
}

void Thread::worker()
{
	std::unique_lock<std::mutex> lk(mtx);
	condi_req.wait(lk, [&]{return is_waiting.load(std::memory_order_acquire);});
	condi_ans.notify_one();
	// 立ち上がりを通知する.
	is_waiting.store(false, std::memory_order_release);
	condi_ans.notify_one();
	lk.unlock();

	// ToDo:
	while (req_exit == false) {
		// 上位からのコマンド(go, quit)を待つ.
		//   (stop, ponderhit等は探索中(または探索終了後)に来る)
		wait_for_go();
		// 初期化をする
		// 探索を開始する
	}
}

void Thread::go()
{
	// ToDo:
}

void Thread::wait_for_go()
{
	// ToDo:
	std::unique_lock<std::mutex> lk(mtx);
	condi_req.wait(lk, [&]{return is_searching.load(std::memory_order_acquire);});
}

ThreadPool::ThreadPool()
{
	// ToDo:
	set_size(options.threads());
}

ThreadPool::~ThreadPool()
{
	// ToDo:
}

void ThreadPool::init()
{
	// ToDo:
	set_size(options.threads());
}

void ThreadPool::set_size(int n)
{
	int old = options.threads();

	while (size() > n) {
		delete back();
		pop_back();
	}
	while (size() < n) {
		push_back(new Thread(size()));
	}
}

void ThreadPool::go(Position& pos)
{
}
