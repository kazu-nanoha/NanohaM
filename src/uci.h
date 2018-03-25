/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#if !defined(UCI_H_INCLUDED)
#define UCI_H_INCLUDED

#include <vector>
#include <string>
#include <iostream>

#define StartSFEN  "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL - "

struct Option_Check {
	Option_Check(const char *n, bool v = false) : name(n), value(v), def_value(v) {}
	std::string name;
	bool value;
	bool def_value;
};

struct Option_Spin {
	Option_Spin(const char *n, int v, int minv, int maxv) : name(n), value(v), def_value(v), _min(minv), _max(maxv) {}
	std::string name;
	int value;
	int def_value;
	int _min, _max;
};

struct Option_String {
	Option_String(const char *n, const char *v) : name(n), value(v), def_value(v) {}
	std::string name;
	std::string value;
	std::string def_value;
};

class Options {
	Option_Check ponder;
	Option_Spin hash;
	Option_Spin m_threads;
	Option_Spin multiPV;
	Option_Check aspiration;
	Option_Check useBook;
	Option_String bookFile;
	int size;
public:
	Options();
	int threads() const { return m_threads.value; }
	void set_threads(int n) { m_threads.value = n; }

	friend std::ostream& operator<<(std::ostream& os, const Options& op);
};

std::ostream& operator<<(std::ostream& os, const Options& options);

extern Options options;

namespace USI {
	void init();
	void loop(int argc, char** argv);
}

#endif
