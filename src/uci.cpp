/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "types.h"
#include "TT.h"
#include "uci.h"

void uci() {
    char *ptr = NULL;
	int i;
	int64_t value;

    (void)fgets(mstring, 65536, stdin);
    if (feof(stdin)) exit(0);
    ptr = strchr(mstring, '\n');
    if (ptr != NULL) *ptr = 0;
    if (!strcmp(mstring, "uci")) {
#ifndef W32_BUILD
		fprintf(stdout,"id name Gull 3 x64\n");
#else
		fprintf(stdout,"id name Gull 3\n");
#endif
        fprintf(stdout,"id author ThinkingALot\n");
#ifndef W32_BUILD
		fprintf(stdout,"option name Hash type spin min 1 max 65536 default 16\n");
#else
		fprintf(stdout,"option name Hash type spin min 1 max 1024 default 16\n");
#endif
		fprintf(stdout,"option name Ponder type check default false\n");
		fprintf(stdout,"option name MultiPV type spin min 1 max 64 default 1\n");
		fprintf(stdout,"option name Clear Hash type button\n");
		fprintf(stdout,"option name PV Hash type check default true\n");
		fprintf(stdout,"option name Aspiration window type check default true\n");
#ifdef CPU_TIMING
		fprintf(stdout, "option name CPUTiming type check default false\n");
		fprintf(stdout, "option name MaxDepth type spin min 0 max 128 default 0\n");
		fprintf(stdout, "option name MaxKNodes type spin min 0 max 65536 default 0\n");
		fprintf(stdout, "option name BaseTime type spin min 0 max 1000000 default 1000\n");
		fprintf(stdout, "option name IncTime type spin min 0 max 1000000 default 5\n");
#endif
		fprintf(stdout, "option name Threads type spin min 1 max %d default %d\n", Min(CPUs, MaxPrN), PrN);
#ifdef LARGE_PAGES
		fprintf(stdout, "option name Large memory pages type check default true\n");
#endif
        fprintf(stdout,"uciok\n");
		if (F(Searching)) init_search(1);
    } else if (!strcmp(mstring,"ucinewgame")) {
        Stop = 0;
		init_search(1);
    } else if (!strcmp(mstring,"isready")) {
        fprintf(stdout,"readyok\n");
		fflush(stdout);
    }  else if (!memcmp(mstring,"position",8)) {
        if (F(Searching)) get_position(mstring);
    } else if (!memcmp(mstring,"go",2)) {
        if (F(Searching)) get_time_limit(mstring);
    } else if (!memcmp(mstring,"setoption",9)) {
		ptr = strtok(mstring," ");
		for (ptr = strtok(NULL," "); ptr != NULL; ptr = strtok(NULL," ")) {
			if (!memcmp(ptr,"Hash",4) && !Searching) {
				ptr += 11;
				value = atoi(ptr);
				if (value < 1) value = 1;
#ifdef W32_BUILD
				if (value > 1024) value = 1024;
#else
				if (value > 65536) value = 65536;
#endif
				value = (Bit(msb(value)) * Convert(1024 * 1024, int64_t)) / Convert(sizeof(GEntry), int64_t);
				TT.resize(value);
			} else if (!memcmp(ptr, "Threads", 7) && !Searching) {
				ptr += 14;
				value = atoi(ptr);
				if (value != PrN) {
					NewPrN = Max(1, Min(MaxPrN, value));
					ResetHash = 0;
					longjmp(ResetJump, 1);
				}
			} else if (!memcmp(ptr, "MultiPV", 7)) {
				ptr += 14;
			    PVN = atoi(ptr);
				Stop = 1;
			} else if (!memcmp(ptr,"Ponder",6)) {
				ptr += 13;
				if (ptr[0] == 't') Ponder = 1;
				else Ponder = 0;
			} else if (!memcmp(ptr,"Clear",5)) {
				init_search(1);
				break;
			} else if (!memcmp(ptr,"PV",2)) {
				ptr += 14;
				if (ptr[0] == 't') PVHashing = 1;
				else PVHashing = 0;
			} else if (!memcmp(ptr, "Large", 5) && !Searching) {
				ptr += 25;
				if (ptr[0] == 't') {
					if (LargePages) return;
					LargePages = 1;
				} else {
					if (!LargePages) return;
					LargePages = 0;
				}
				ResetHash = 1;
				longjmp(ResetJump, 1);
			} else if (!memcmp(ptr, "Aspiration", 10)) {
				ptr += 24;
				if (ptr[0] == 't') Aspiration = 1;
				else Aspiration = 0;
			}
#ifdef CPU_TIMING
			else if (!memcmp(ptr, "CPUTiming", 9)) {
				ptr += 16;
				if (ptr[0] == 't') CpuTiming = 1;
				else CpuTiming = 0;
			} else if (!memcmp(ptr, "MaxDepth", 8)) UciMaxDepth = atoi(ptr + 15);
			else if (!memcmp(ptr, "MaxKNodes", 9)) UciMaxKNodes = atoi(ptr + 16);
			else if (!memcmp(ptr, "BaseTime", 8)) UciBaseTime = atoi(ptr + 15);
			else if (!memcmp(ptr, "IncTime", 7)) UciIncTime = atoi(ptr + 14);
#endif
        }
	} else if (!strcmp(mstring,"stop")) {
		Stop = 1;
		if (F(Searching)) send_best_move();
	} else if (!strcmp(mstring,"ponderhit")) {
		Infinite = 0;
		if (!RootList[1]) Stop = 1;
		if (F(CurrentSI->Bad) && F(CurrentSI->FailLow) && time_to_stop(BaseSI, LastTime, 0)) Stop = 1;
		if (F(Searching)) send_best_move();
	} else if (!strcmp(mstring, "quit")) {
		// ToDo: 
		for (i = 1; i < PrN; i++) {
///			TerminateProcess(ChildPr[i], 0);
///			CloseHandle(ChildPr[i]);
		}
		exit(0);
	}
}


