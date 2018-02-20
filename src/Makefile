# NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
# The original Gull chess source code is "public domain".
#
# Copyright (c) 2018 Kazuyuki Kawabata
#
# This software is released under the MIT License, see "LICENSE.txt".

#CC=clang++
CC=g++

TARGET=Nanoha.exe
SRCS=Gull.cpp TT.cpp evaluate.cpp misc.cpp uci.cpp position.cpp genmove.cpp
OBJS=Gull.o   TT.o   evaluate.o   misc.o   uci.o   position.o   genmove.o

.cpp.o:
	$(CC) -c $(CFLAGS) $<

CFLAGS=-fno-exceptions -fno-rtti -Wno-parentheses -D_WIN32_WINNT=0x0601 -Wall -std=gnu++11 -mpopcnt
STRIP=strip

ifeq ($(DEBUG),yes)
	CFLAGS += -O0 -g
else
	CFLAGS += -O3
endif

build: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -D WINDOWS $(OBJS) $(LDFLAGS) -o $(TARGET)

release: $(TARGET)
	$(STRIP) $(TARGET)

debug: 
	make DEBUG=yes build

clean:
	rm -f $(TARGET) $(OBJS)

