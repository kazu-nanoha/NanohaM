/**
NanohaM, a USI shogi(Japanese-chess) playing engine, derived from Gull chess.
The original Gull chess source code is "public domain".

Copyright (c) 2018 Kazuyuki Kawabata

This software is released under the MIT License, see "LICENSE.txt".
*/

#include <intrin.h>
#include <Windows.h>
#include <cstdio>
#include "types.h"
#include "misc.h"
#include "genmove.h"
#include "TT.h"
#include "evaluate.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "uci.h"

#define MP_NPS

#undef BB
#define BB(sq)  pos.bb(sq)
#undef Square
#define Square(sq) pos.square(sq)

// Memo: L403
#if 0
#define halt_check if ((Current - Data) >= 126) {evaluate(pos); return Current->score;} \
    if (Current->ply >= 100) return 0; \
	for (i = 4; i <= Current->ply; i+= 2) if (Stack[sp-i] == Current->key) return 0
#else
#define halt_check if (pos.height() >= 126) {evaluate(pos); return pos.score();} \
    if (pos.ply() >= 100) return 0; \
if (pos.is_repeat()) return 0
#endif

// Memo: L425
GBoard SaveBoard[1];

// Memo: L438
GData SaveData[1];

Position position[1];

// Memo: L566
#define ExclSingle(depth) 8
#define ExclDouble(depth) 16
#define ExclSinglePV(depth) 8
#define ExclDoublePV(depth) 16


// Memo: L721
///#define SplitDepth 10
///#define SplitDepthPV 4
///#define MaxSplitPoints 64 // mustn't exceed 64


// Memo: L2304
void init_search(Position& pos, int clear_hash)
{
	memset(History,1,16 * 64 * sizeof(int16_t));
	memset(Delta,0,16 * 4096 * sizeof(int16_t));
	memset(Ref,0,16 * 64 * sizeof(GRef));
///	memset(Data + 1, 0, 127 * sizeof(GData));
	pos.init_search();
	if (clear_hash) {
		date = 0;
		date = 1;
		TT.clear();
		PVHASH.clear();
	}
///	get_board("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
	pos.set_sfen(StartSFEN);
	nodes = 0;
	best_move = best_score = 0;
	LastTime = LastValue = LastExactValue = InstCnt = 0;
	LastSpeed = 0;
	PVN = 1;
	Infinite = 1;
	SearchMoves = 0;
	TimeLimit1 = TimeLimit2 = 0;
	Stop = Searching = 0;
///	if (MaxPrN > 1) ZERO_BIT_64(Smpi->searching, 0);
	DepthLimit = 128;
	LastDepth = 128;
	Print = 1;
	memset(CurrentSI,0,sizeof(GSearchInfo));
	memset(BaseSI,0,sizeof(GSearchInfo));
}

namespace {
// Memo: L2467
void pick_pv(Position& pos) {
	GEntry * Entry;
	GPVEntry * PVEntry;
	int depth, move;
	if (pvp >= Min(pv_length,64)) {
		PV[pvp] = 0;
		return;
	}
	move = 0;
	depth = -256;
	if (Entry = TT.probe(pos.key())) if (T(Entry->move16) && Entry->low_depth > depth) {
		depth = Entry->low_depth;
		move = Entry->move16;
	}
	if (PVEntry = PVHASH.probe(pos.key())) if (T(PVEntry->move16) && PVEntry->depth > depth) {
		depth = PVEntry->depth;
		move = PVEntry->move16;
	}
	evaluate(pos);
	if (pos.att(pos.cur_turn()) & King(pos.cur_turn() ^ 1)) PV[pvp] = 0;
	else if (move && (pos.cur_turn() ? pos.is_legal<1>(move) : pos.is_legal<0>(move))) {
		PV[pvp] = move;
		pvp++;
		if (pos.cur_turn()) pos.do_move<1>(move);
		else pos.do_move<0>(move);
		if (pos.ply() >= 100) goto finish;
///		for (i = 4; i <= pos.ply(); i+= 2) if (Stack[sp-i] == pos.key()) {
///			PV[pvp] = 0;
///			goto finish;
///		}
		if (pos.is_repeat()) {
			PV[pvp] = 0;
			goto finish;
		}
		pick_pv(pos);
finish:
		if (pos.cur_turn() ^ 1) pos.undo_move<1>(move);
		else pos.undo_move<0>(move);
	} else PV[pvp] = 0;
}
}

template <bool me> int draw_in_pv(Position& pos) {
	constexpr bool opp = !me;
	if (pos.height() >= 126) return 1;
	if (pos.ply() >= 100) return 1;
///	for (int i = 4; i <= pos.ply(); i += 2) if (Stack[sp - i] == pos.key()) return 1;
	if (pos.is_repeat()) return 1;
	if (GPVEntry * PVEntry = PVHASH.probe(pos.key())) {
		if (!PVEntry->value) return 1;
		if (int move = PVEntry->move16) {
			pos.do_move<me>(move);
			int value = draw_in_pv<opp>(pos);
			pos.undo_move<me>(move);
			return value;
		}
	}
	return 0;
}

namespace {
void hash_high(const uint64_t key, const int value, const int depth) {
	uint32_t i;
	int score, min_score;
	GEntry *best, *Entry;

	min_score = 0x70000000;
	for (i = 0, best = Entry = TT.top(key); i < TT_cluster_size; i++, Entry++) {
		if (Entry->key32 == Low32(key)) {
			Entry->date = date;
			if (depth > Entry->high_depth || (depth == Entry->high_depth && value < Entry->high)) {
				if (Entry->low <= value) { 
				    Entry->high_depth = depth;
				    Entry->high = value;
				} else if (Entry->low_depth < depth) {
					Entry->high_depth = depth;
				    Entry->high = value;
					Entry->low = value;
				}
			}
			return;
		} else score = (Convert(Entry->date,int) << 3) + Convert(Max(Entry->high_depth, Entry->low_depth),int);
		if (score < min_score) {
			min_score = score;
			best = Entry;
		}
	}
	best->date = date;
	best->key32 = Low32(key);
	best->high = value;
	best->high_depth = depth;
	best->low = 0;
	best->low_depth = 0;
	best->move16 = 0;
	best->flags = 0;
	return;
}

void hash_low(const uint64_t key, int move, const int value, const int depth) {
	uint32_t i;
	int score, min_score;
	GEntry *best, *Entry;

	min_score = 0x70000000;
	move &= 0xFFFF;
	for (i = 0, best = Entry = TT.top(key); i < TT_cluster_size; i++, Entry++) {
		if (Entry->key32 == Low32(key)) {
			Entry->date = date;
			if (depth > Entry->low_depth || (depth == Entry->low_depth && value > Entry->low)) {
				if (move) Entry->move16 = move;
				if (Entry->high >= value) {
				    Entry->low_depth = depth;
				    Entry->low = value;
				} else if (Entry->high_depth < depth) {
					Entry->low_depth = depth;
				    Entry->low = value;
					Entry->high = value;
				}
			} else if (F(Entry->move16)) Entry->move16 = move;
			return;
		} else score = (Convert(Entry->date,int) << 3) + Convert(Max(Entry->high_depth, Entry->low_depth),int);
		if (score < min_score) {
			min_score = score;
			best = Entry;
		}
	}
	best->date = date;
	best->key32 = Low32(key);
	best->high = 0;
	best->high_depth = 0;
	best->low = value;
	best->low_depth = depth;
	best->move16 = move;
	best->flags = 0;
	return;
}

void hash_exact(const uint64_t key, int move, int value, int ply, int depth, int exclusion, int ex_depth, int knodes) {
	uint32_t i;
	int score, min_score;
	GPVEntry *best;
	GPVEntry * PVEntry;

	min_score = 0x70000000;
	for (i = 0, best = PVEntry = PVHASH.top(key); i < pv_cluster_size; i++, PVEntry++) {
		if (PVEntry->key32 == Low32(key)) {
			PVEntry->date = date;
			PVEntry->knodes += knodes;
			if (PVEntry->depth <= depth) {
				PVEntry->value = value;
				PVEntry->depth = depth;
				PVEntry->move16 = move;
				PVEntry->ply = ply;
				if (ex_depth) {
					PVEntry->exclusion = exclusion;
					PVEntry->ex_depth = ex_depth;
				}
			}
			return;
		}
		score = (Convert(PVEntry->date,int) << 3) + Convert(PVEntry->depth,int);
		if (score < min_score) {
			min_score = score;
			best = PVEntry;
		}
	}
	best->key32 = Low32(key);
	best->date = date;
	best->value = value;
	best->depth = depth;
	best->move16 = move;
	best->exclusion = exclusion;
	best->ex_depth = ex_depth;
	best->knodes = knodes;
	best->ply = ply;
}

template <bool pv> int extension(const Position& pos, int move, int depth) {
	int ext = 0;
	if (pv) {
		if (T(pos.Current->passer & Bit(From(move))) && CRank(pos.cur_turn(), From(move)) >= 5 && depth < 16) ext = 2;
	} else {
		if (T(pos.Current->passer & Bit(From(move))) && CRank(pos.cur_turn(), From(move)) >= 5 && depth < 16) ext = 1; 
	}
	return ext;
}

void sort_moves(int * start, int * finish) {
	for (int * p = start + 1; p < finish; p++) for (int * q = p - 1; q >= start; q--) if (((*q) >> 16) < ((*(q+1)) >> 16)) {
		int move = *q;
		*q = *(q+1);
		*(q+1)=move;
	}
}

template <bool me> int singular_extension(Position& pos, int ext, int prev_ext, int margin_one, int margin_two, int depth, int killer) {
	int value = -MateValue;
	int singular = 0;
	if (ext < 1 + (prev_ext < 1)) {
		if (Check(me)) value = search_evasion<me, 1>(pos, margin_one, depth, killer); 
		else value = search<me, 1>(pos, margin_one, depth, killer); 
		if (value < margin_one) singular = 1;
	}
	if (value < margin_one && ext < 2 + (prev_ext < 1) - (prev_ext >= 2)) {
		if (Check(me)) value = search_evasion<me, 1>(pos, margin_two, depth, killer); 
		else value = search<me, 1>(pos, margin_two, depth, killer); 
		if (value < margin_two) singular = 2;
	}
	return singular;
}

template <bool me> void capture_margin(Position& pos, int alpha, int &score) {
	constexpr bool opp = !me;
	if (pos.score() + 200 < alpha) {
		if (pos.att(me) & Pawn(opp)) {
			pos.set_mask(pos.mask() ^ Pawn(opp));
			score = pos.score() + 200;
		}
		if (pos.score() + 500 < alpha) {
			if (pos.att(me) & Minor(opp)) {
				pos.set_mask(pos.mask() ^ Minor(opp));
				score = pos.score() + 500;
			}
			if (pos.score() + 700 < alpha) {
				if (pos.att(me) & Rook(opp)) {
					pos.set_mask(pos.mask() ^ Rook(opp));
					score = pos.score() + 700;
				}
				if (pos.score() + 1400 < alpha && (pos.att(me) & Queen(opp))) {
					pos.set_mask(pos.mask() ^ Queen(opp));
					score = pos.score() + 1400;
				}
			}
		}
	}
}

///void send_position(GPos * Pos) {
///	Pos->Position->key = Current->key;
///	Pos->Position->pawn_key = Current->pawn_key;
///	Pos->Position->move = Current->move;
///	Pos->Position->capture = Current->capture;
///	Pos->Position->turn = Current->turn;
///	Pos->Position->castle_flags = Current->castle_flags;
///	Pos->Position->ply = Current->ply;
///	Pos->Position->ep_square = Current->ep_square;
///	Pos->Position->piece = Current->piece;
///	Pos->Position->pst = Current->pst;
///	Pos->Position->material = Current->material;
///	for (int i = 0; i < 64; i++) Pos->Position->square[i] = Board->square[i];
///	Pos->date = date;
///	Pos->sp = sp;
///	for (int i = 0; i <= Current->ply; i++) Pos->stack[i] = Stack[sp - i];
///	for (int i = 0; i < Min(16, 126 - (int)(Current - Data)); i++) {
///		Pos->killer[i][0] = (Current + i + 1)->killer[1];
///		Pos->killer[i][1] = (Current + i + 1)->killer[2];
///	}
///	for (int i = Min(16, 126 - (int)(Current - Data)); i < 16; i++) Pos->killer[i][0] = Pos->killer[i][1] = 0;
///}
#if 0
void retrieve_board(GPos * Pos) {
	for (int i = 0; i < 16; i++) Board->bb[i] = 0;
	for (int i = 0; i < 64; i++) {
		int piece = Pos->Position->square[i];
		Board->square[i] = piece;
		if (piece) {
			Board->bb[piece & 1] |= Bit(i);
			Board->bb[piece] |= Bit(i);
		}
	}
}
#endif
///void init_sp(GSP * Sp, int alpha, int beta, int depth, int pv, int singular, int height) {
///	Sp->claimed = 1;
///	Sp->active = Sp->finished = 0;
///	Sp->best_move = 0;
///	Sp->alpha = alpha;
///	Sp->beta = beta;
///	Sp->depth = depth;
///	Sp->split = 0;
///	Sp->singular = singular;
///	Sp->height = height;
///	Sp->move_number = 0;
///	Sp->pv = pv;
///}
#if 0
template <bool me> int smp_search(GSP * Sp, Position& pos)
{
	constexpr bool opp = !me;
	int i, value, move, alpha, iter = 0;
	if (!Sp->move_number) return Sp->alpha;
	send_position(Sp->Pos);
	if (setjmp(Sp->jump)) {
		LOCK(Sp->lock);
		halt_all(Sp, 1);
		UNLOCK(Sp->lock);
		halt_all(Sp->height + 1, 127);
		Current = Data + Sp->height;
		sp = Sp->Pos->sp;
		retrieve_board(Sp->Pos);
		return Sp->beta;
	}
	LOCK(Sp->lock);
	SET_BIT_64(Smpi->active_sp, (int)(Sp - Smpi->Sp));
	Sp->active = 1;
	Sp->claimed = Sp->finished = 0;
loop:
	for (i = 0; i < Sp->move_number; i++) {
		GMove * M = &Sp->move[i];
		if (!iter) Sp->current = i;
		if (M->flags & FlagFinished) continue;
		if (!iter) {
			if (M->flags & FlagClaimed) continue;
			M->flags |= FlagClaimed;
			M->id = Id;
		} else if (M->flags & FlagClaimed) {
			SET_BIT_64(Smpi->stop, M->id);
			M->id = Id;
		}
		move = M->move;
		alpha = Sp->alpha;
		UNLOCK(Sp->lock);
		pos.do_move<me>(move);
		value = -search<opp, 0>(pos, -alpha, M->reduced_depth, FlagNeatSearch | ExtFlag(M->ext));
		if (value > alpha && (Sp->pv || M->reduced_depth < M->research_depth)) {
			if (Sp->pv) value = -pv_search<opp, 0>(pos, -Sp->beta, -Sp->alpha, M->research_depth, FlagNeatSearch | ExtFlag(M->ext));
			else value = -search<opp, 0>(pos, -alpha, M->research_depth, FlagNeatSearch | FlagDisableNull | ExtFlag(M->ext));
		}
		pos.undo_move<me>(move);
		LOCK(Sp->lock);
		if (Sp->finished) goto cut;
		M->flags |= FlagFinished;
		if (value > Sp->alpha) {
			Sp->best_move = move;
			Sp->alpha = Min(value, Sp->beta);
			if (value >= Sp->beta) goto cut;
		}
	}
	if (!iter) {
		iter++;
		goto loop;
	}
	halt_all(Sp, 1);
	UNLOCK(Sp->lock);
	return Sp->alpha;
cut:
	halt_all(Sp, 1);
	UNLOCK(Sp->lock);
	return Sp->beta;
}
#endif
template <bool me> int multipv(Position& pos, int depth)
{
	constexpr bool opp = !me;
	int move, low = MateValue, value, i, cnt, ext, new_depth = depth;
	fprintf(stdout,"info depth %d\n",(depth/2)); fflush(stdout);
	for (cnt = 0; cnt < PVN && T(move = (MultiPV[cnt] & 0xFFFF)); cnt++) {
		MultiPV[cnt] = move;
		move_to_string(move,score_string);
		if (T(Print)) sprintf(info_string,"info currmove %s currmovenumber %d\n",score_string,cnt + 1);
		new_depth = depth - 2 + (ext = extension<1>(pos, move, depth));
		pos.do_move<me>(move);
		value = -pv_search<opp, 0>(pos, -MateValue,MateValue,new_depth,ExtFlag(ext));
		MultiPV[cnt] |= value << 16;
		if (value < low) low = value;
		pos.undo_move<me>(move);
		for (i = cnt - 1; i >= 0; i--) {
			if ((MultiPV[i] >> 16) < value) {
				MultiPV[i + 1] = MultiPV[i];
				MultiPV[i] = move | (value << 16);
			}
		}
		best_move = MultiPV[0] & 0xFFFF;
		pos.set_score(MultiPV[0] >> 16);
		send_multipv(pos, (depth/2), cnt);
	}
	for (;T(move = (MultiPV[cnt] & 0xFFFF)); cnt++) {
		MultiPV[cnt] = move;
		move_to_string(move,score_string);
		if (T(Print)) sprintf(info_string,"info currmove %s currmovenumber %d\n",score_string,cnt + 1);
		new_depth = depth - 2 + (ext = extension<1>(pos, move, depth));
		pos.do_move<me>(move);
		value = -search<opp, 0>(pos, -low, new_depth, FlagNeatSearch | ExtFlag(ext));
		if (value > low) value = -pv_search<opp, 0>(pos, -MateValue,-low,new_depth,ExtFlag(ext));
		MultiPV[cnt] |= value << 16;
		pos.undo_move<me>(move);
		if (value > low) {
			for (i = cnt; i >= PVN; i--) MultiPV[i] = MultiPV[i - 1];
			MultiPV[PVN - 1] = move | (value << 16);
			for (i = PVN - 2; i >= 0; i--) {
				if ((MultiPV[i] >> 16) < value) {
					MultiPV[i + 1] = MultiPV[i];
					MultiPV[i] = move | (value << 16);
				}
			}
			best_move = MultiPV[0] & 0xFFFF;
		    pos.set_score(MultiPV[0] >> 16);
			low = MultiPV[PVN - 1] >> 16;
			send_multipv(pos, (depth/2), cnt);
		}
	}
	return pos.score();
}

}	// namespace

template <bool me, bool pv> int q_search(Position& pos, int alpha, int beta, int depth, int flags)
{
	constexpr bool opp = !me;
	uint32_t i;
	int value, score, move, hash_move, hash_depth, cnt;
	GEntry * Entry;
	MoveList ml;

	if (flags & FlagHaltCheck) halt_check;
	if (flags & FlagCallEvaluation) evaluate(pos);
	if (Check(me)) return q_evasion<me, pv>(pos, alpha, beta, depth, FlagHashCheck);
	score = pos.score() + 3;
	if (score > alpha) {
		alpha = score;
		if (score >= beta) return score;
	}

	hash_move = hash_depth = 0;
	if (flags & FlagHashCheck) {
	    for (i = 0, Entry = TT.top(pos.key()); i < TT_cluster_size; Entry++, i++) {
		    if (Low32(pos.key()) == Entry->key32) {
			    if (T(Entry->low_depth)) {
				    if (Entry->low >= beta && !pv) return Entry->low;
				    if (Entry->low_depth > hash_depth && T(Entry->move16)) {
					    hash_move = Entry->move16;
					    hash_depth = Entry->low_depth;
				    }
			    }
			    if (T(Entry->high_depth) && Entry->high <= alpha && !pv) return Entry->high;
				break;
		    }
	    }
	}

	pos.set_mask(Piece(opp));
	capture_margin<me>(pos, alpha, score);

	cnt = 0;
	if (T(hash_move)) {
		if (F(Bit(To(hash_move)) & pos.mask()) && F(hash_move & 0xE000) && (depth < -8 || (pos.score() + DeltaM(hash_move) <= alpha && F(pos.is_check<me>(hash_move))))) goto skip_hash_move;
		if (pos.is_legal<me>(move = hash_move)) {
			if (IsIllegal(me,move)) goto skip_hash_move;
			if (SeeValue[Square(To(move))] > SeeValue[Square(From(move))]) cnt++;
			pos.do_move<me>(move);
		    value = -q_search<opp, pv>(pos, -beta, -alpha, depth - 1, FlagNeatSearch);
		    pos.undo_move<me>(move);
			if (value > score) {
			    score = value;
			    if (value > alpha) {
				    alpha = value;
			        if (value >= beta) goto cut;
			    }
		    }
			if (F(Bit(To(hash_move)) & pos.mask()) && F(hash_move & 0xE000) && (depth < -2 || depth <= -1 && pos.score() + 50 < alpha) && alpha >= beta - 1 && !pv) return alpha;
		}
	}
skip_hash_move:
	ml.gen_captures<me>(pos);
	ml.cur = ml.moves;
	while (move = ml.pick_move()) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		if (F(pos.see<me>(move,-50))) continue;
		if (SeeValue[Square(To(move))] > SeeValue[Square(From(move))]) cnt++;
		pos.do_move<me>(move);
		value = -q_search<opp, pv>(pos, -beta, -alpha, depth - 1, FlagNeatSearch);
		pos.undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value > alpha) {
				alpha = value;
			    if (value >= beta) goto cut;
			}
		}
	}

	if (depth < -2) goto finish;
	if (depth <= -1 && pos.score() + 50 < alpha) goto finish;
	ml.gen_checks<me>(pos);
	ml.cur = ml.moves;
	while (move = ml.pick_move()) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		if (IsRepetition(alpha + 1,move)) continue;
		if (F(pos.see<me>(move,-50))) continue;
		pos.do_move<me>(move);
		value = -q_evasion<opp, pv>(pos, -beta, -alpha, depth - 1, FlagNeatSearch);
		pos.undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value > alpha) {
				alpha = value;
			    if (value >= beta) goto cut;
			}
		}
	}

	if (T(cnt) || pos.score() + 30 < alpha || T(Current->threat & Piece(me)) || T((Current->xray[opp] | Current->pin[opp]) & NonPawn(opp)) 
		|| T(Pawn(opp) & Line(me, 1) & Shift(me,~PieceAll))) goto finish;
	// ToDo: 6の意味.
	Current->margin = alpha - pos.score() + 6;
	ml.gen_delta_moves<me>(pos);
	ml.cur = ml.moves;
	while (move = ml.pick_move()) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		if (IsRepetition(alpha + 1,move)) continue;
		if (F(pos.see<me>(move,-50))) continue;
		cnt++;
		pos.do_move<me>(move);
		value = -q_search<opp, pv>(pos, -beta, -alpha, depth - 1, FlagNeatSearch);
		pos.undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value > alpha) {
				alpha = value;
			    if (value >= beta) {
					if (Current->killer[1] != move) {
						Current->killer[2] = Current->killer[1];
						Current->killer[1] = move;
					}
					goto cut;
				}
			}
		}
		if (cnt >= 3) break; 
	}

finish:
	if (depth >= -2 && (depth >= 0 || pos.score() + 50 >= alpha)) hash_high(pos.key(), score, 1);
	return score;
cut:
	hash_low(pos.key(), move, score, 1);
	return score;
}

template <bool me, bool pv> int q_evasion(Position& pos, int alpha, int beta, int depth, int flags)
{
	constexpr bool opp = !me;
	uint32_t i;
	int value, pext, score, move, cnt, hash_move, hash_depth;
	int *p;
	GEntry * Entry;

	score = pos.height() - MateValue;
	if (flags & FlagHaltCheck) halt_check;

	hash_move = hash_depth = 0;
	if (flags & FlagHashCheck) {
	    for (i = 0, Entry = TT.top(pos.key()); i < TT_cluster_size; Entry++, i++) {
		    if (Low32(pos.key()) == Entry->key32) {
			    if (T(Entry->low_depth)) {
				    if (Entry->low >= beta && !pv) return Entry->low;
				    if (Entry->low_depth > hash_depth && T(Entry->move16)) {
					    hash_move = Entry->move16;
					    hash_depth = Entry->low_depth;
				    }
			    }
			    if (T(Entry->high_depth) && Entry->high <= alpha && !pv) return Entry->high;
				break;
		    }
	    }
	}

	MoveList ml;
	if (flags & FlagCallEvaluation) evaluate(pos);
	pos.set_mask(Filled);
	if (pos.score() - 10 <= alpha && !pv) {
		pos.set_mask(Piece(opp));
		score = pos.score() - 10;
		capture_margin<me>(pos, alpha, score);
	}

	alpha = Max(score, alpha);
	pext = 0;
	ml.gen_evasions<me>(pos);
	ml.cur = ml.moves;
	if (F(ml.moves[0])) return score;
	if (F(ml.moves[1])) pext = 1;
	else {
		Current->ref[0] = RefM(pos.cur_move()).check_ref[0];
		Current->ref[1] = RefM(pos.cur_move()).check_ref[1];
		ml.mark_evasions(pos);
	    if (T(hash_move) && (T(Bit(To(hash_move)) & pos.mask()) || T(hash_move & 0xE000))) {
	        for (p = ml.moves; T(*p); p++) {
		        if (((*p) & 0xFFFF) == hash_move) {
				    *p |= 0x7FFF0000;
				    break;
			    }
	        }
	    }
	}
	cnt = 0;
	while (move = ml.pick_move()) {
		if (IsIllegal(me,move)) continue;
		cnt++;
		if (IsRepetition(alpha + 1,move)) {
			score = Max(0, score);
			continue;
		}
		if (F(Square(To(move))) && F(move & 0xE000)) {
			if (cnt > 3 && F(pos.is_check<me>(move)) && !pv) continue;
			if ((value = pos.score() + DeltaM(move) + 10) <= alpha && !pv) {
				score = Max(value, score);
				continue;
			}
		}
		pos.do_move<me>(move);
		value = -q_search<opp, pv>(pos, -beta, -alpha, depth - 1 + pext, FlagNeatSearch);
		pos.undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value > alpha) {
				alpha = value;
			    if (value >= beta) goto cut;
			}
		}
	}
	return score;
cut:
	return score;
}
#if 0
void retrieve_position(GPos * Pos, int copy_stack) {
	Current->key = Pos->Position->key;
	Current->pawn_key = Pos->Position->pawn_key;
	Current->move = Pos->Position->move;
	Current->capture = Pos->Position->capture;
	Current->turn = Pos->Position->turn;
	Current->castle_flags = Pos->Position->castle_flags;
	Current->ply = Pos->Position->ply;
	Current->ep_square = Pos->Position->ep_square;
	Current->piece = Pos->Position->piece;
	Current->pst = Pos->Position->pst;
	Current->material = Pos->Position->material;
	retrieve_board(Pos);
	date = Pos->date;
	if (copy_stack) {
		sp = Current->ply;
		for (int i = 0; i <= sp; i++) Stack[sp - i] = Pos->stack[i];
	} else sp = Pos->sp;
	for (int i = 0; i < 16; i++) {
		(Current + i + 1)->killer[1] = Pos->killer[i][0];
		(Current + i + 1)->killer[2] = Pos->killer[i][1];
	}
}
#endif
///void halt_all(GSP * Sp, int locked) {
///	GMove * M;
///	if (!locked) LOCK(Sp->lock);
///	if (Sp->active) {
///		for (int i = 0; i < Sp->move_number; i++) {
///			M = &Sp->move[i];
///			if ((M->flags & FlagClaimed) && !(M->flags & FlagFinished) && M->id != Id) SET_BIT_64(Smpi->stop, M->id);
///		}
///		Sp->active = Sp->claimed = 0;
///		ZERO_BIT_64(Smpi->active_sp, (int)(Sp - Smpi->Sp));
///	}
///	if (!locked) UNLOCK(Sp->lock);
///}

///void halt_all(int from, int to) {
///	for (uint64_t u = Smpi->active_sp; u; Cut(u)) {
///		GSP * Sp = &Smpi->Sp[lsb(u)];
///		LOCK(Sp->lock);
///		if (Sp->height >= from && Sp->height <= to) halt_all(Sp, 1);
///		UNLOCK(Sp->lock);
///	}
///}

template <bool me, bool exclusion> int search(Position& pos, int beta, int depth, int flags) {
	constexpr bool opp = !me;
///	int i;
	int value, cnt, flag, moves_to_play, check, score, move, ext, margin;
	int hash_move, move_back;
	int singular, played,
		high_depth, high_value, hash_value, new_depth, hash_depth, *p;
	int height = pos.height();
///	GSP * Sp;

#if 0	// ToDo: ちゃんと考える。
	if (nodes > check_node_smp + 0x10) {
#ifndef W32_BUILD
		InterlockedAdd64(&Smpi->nodes, (long long)(nodes)-(long long)(check_node_smp));
#else
		Smpi->nodes += (long long)(nodes)-(long long)(check_node_smp);
#endif
		check_node_smp = nodes;
		check_state();
		if (nodes > check_node + 0x4000 && parent) {
			check_node = nodes;
			check_time(1);
			if (Searching) SET_BIT_64(Smpi->searching, Id); // BUG, don't know why this is necessary
		}
	}
#endif

	if (depth <= 1) return q_search<me, 0>(pos, beta - 1, beta, 1, flags);
	if (flags & FlagHaltCheck) {
	    if (height - MateValue >= beta) return beta;
	    if (MateValue - height < beta) return beta - 1;
	    halt_check;
	}

	MoveList ml;	// gotoの前に定義が必要.
	if (exclusion) {
		cnt = high_depth = singular = played = 0;
		flag = 1;
		score = beta - 1;
		high_value = MateValue; 
		hash_value = -MateValue;
		hash_depth = -1;
		hash_move = flags & 0xFFFF;
		goto skip_hash_move;
	}

	if (flags & FlagCallEvaluation) evaluate(pos);
	if (Check(me)) return search_evasion<me, 0>(pos, beta, depth, flags & (~(FlagHaltCheck | FlagCallEvaluation)));

	if ((value = pos.score() - 90 - (depth << 3) - (Max(depth - 5, 0) << 5)) >= beta && F(Pawn(opp) & Line(me, 1) & Shift(me,~PieceAll)) && T(NonPawnKing(me)) && F(flags & (FlagReturnBestMove | FlagDisableNull)) && depth <= 13) return value;
	if ((value = pos.score() + 50) < beta && depth <= 3) return MaxF(value, q_search<me, 0>(pos, beta - 1, beta, 1, FlagHashCheck | (flags & 0xFFFF)));

	high_depth = 0;
	high_value = MateValue;
	hash_value = -MateValue;
	hash_depth = -1;
	pos.best() = hash_move = flags & 0xFFFF;
	if (GEntry * Entry = TT.probe(pos.key())) {
		if (Entry->high_depth > high_depth) {
			high_depth = Entry->high_depth;
			high_value = Entry->high;
		}
		if (Entry->high < beta && Entry->high_depth >= depth) return Entry->high;
		if (T(Entry->move16) && Entry->low_depth > hash_depth) {
			pos.best() = hash_move = Entry->move16;
			hash_depth = Entry->low_depth;
			if (Entry->low_depth) hash_value = Entry->low;
		}
		if (Entry->low >= beta && Entry->low_depth >= depth) {
			if (Entry->move16) {
				pos.best() = Entry->move16;
				if (F(Square(To(Entry->move16))) && F(Entry->move16 & 0xE000)) {
					if (Current->killer[1] != Entry->move16 && F(flags & FlagNoKillerUpdate)) {
						Current->killer[2] = Current->killer[1];
						Current->killer[1] = Entry->move16;
					}
					UpdateRef(Entry->move16);
				}
				return Entry->low;
			}
			if (F(flags & FlagReturnBestMove)) return Entry->low;
		}
	}
	if (depth >= 20) if (GPVEntry * PVEntry = PVHASH.probe(pos.key())) {
		hash_low(pos.key(), PVEntry->move16,PVEntry->value,PVEntry->depth);
		hash_high(pos.key(), PVEntry->value,PVEntry->depth);
		if (PVEntry->depth >= depth) {
			if (PVEntry->move16) pos.best() = PVEntry->move16;
			if (F(flags & FlagReturnBestMove) && ((pos.ply() <= 50 && PVEntry->ply <= 50) || (pos.ply() >= 50 && PVEntry->ply >= 50))) return PVEntry->value;
		}
		if (T(PVEntry->move16) && PVEntry->depth > hash_depth) {
			pos.best() = hash_move = PVEntry->move16;
			hash_depth = PVEntry->depth;
			hash_value = PVEntry->value;
		}
	}
	if (depth < 10) score = height - MateValue;
	else score = beta - 1;
	if (depth >= 12 && (F(hash_move) || hash_value < beta || hash_depth < depth - 12) && (high_value >= beta || high_depth < depth - 12) && F(flags & FlagDisableNull)) {
		new_depth = depth - 8;
		value = search<me, 0>(pos, beta, new_depth, FlagHashCheck | FlagNoKillerUpdate | FlagDisableNull | FlagReturnBestMove | hash_move);
		if (value >= beta) {
			if (pos.best()) hash_move = pos.best();
			hash_depth = new_depth;
			hash_value = beta;
		}
	}
	if (depth >= 4 && pos.score() + 3 >= beta && F(flags & (FlagDisableNull | FlagReturnBestMove))
		&& (high_value >= beta || high_depth < depth - 10) && (depth < 12 || (hash_value >= beta && hash_depth >= depth - 12)) && beta > -EvalValue && T(NonPawnKing(me))) {
		new_depth = depth - 8;
		pos.do_null();
	    value = -search<opp, 0>(pos, 1 - beta, new_depth, FlagHashCheck);
		pos.undo_null();
		if (value >= beta) {
			if (depth < 12) hash_low(pos.key(), 0, value, depth);
			return value;
		}
	}

	cnt = flag = singular = played = 0;
	if (T(hash_move) && pos.is_legal<me>(move = hash_move)) {
		if (IsIllegal(me,move)) goto skip_hash_move;
		cnt++;
		check = pos.is_check<me>(move);
		if (check) ext = 1 + (depth < 16);
		else ext = extension<0>(pos, move, depth);
		if (depth >= 16 && hash_value >= beta && hash_depth >= (new_depth = depth - Min(12, depth/2))) {
			int margin_one = beta - ExclSingle(depth);
			int margin_two = beta - ExclDouble(depth);
			int prev_ext = Ext(flags);
			singular = singular_extension<me>(pos, ext,prev_ext,margin_one,margin_two,new_depth,hash_move);
			if (singular) ext = Max(ext, singular + (prev_ext < 1) - (singular >= 2 && prev_ext >= 2));
		}
		if (depth < 16 && To(move) == To(Current->move) && T(Square(To(move)))) ext = Max(ext, 2);
		new_depth = depth - 2 + ext;
		pos.do_move<me>(move);
		value = -search<opp, 0>(pos, 1 - beta, new_depth, FlagNeatSearch | ((hash_value >= beta && hash_depth >= depth - 12) ? FlagDisableNull : 0) | ExtFlag(ext));
		pos.undo_move<me>(move);
		played++;
		if (value > score) {
			score = value;
			if (value >= beta) goto cut;
		}
	}
skip_hash_move:
	Current->killer[0] = 0;
	ml.stage = stage_search;
	ml.gen_flags = 0;
	Current->ref[0] = RefM(Current->move).ref[0];
	Current->ref[1] = RefM(Current->move).ref[1];
	move_back = 0;
	if (beta > 0 && pos.ply() >= 2) {
		if (F((Current - 1)->move & 0xF000)) {
			move_back = (To((Current - 1)->move) << 6) | From((Current - 1)->move);
			if (Square(To(move_back))) move_back = 0;
		}
	}
	moves_to_play = 3 + (depth * depth)/6;
	margin = pos.score() + 70 + (depth << 3) + (Max(depth - 7, 0) << 5);
	if ((value = margin) < beta && depth <= 19) {
		flag = 1;
		score = Max(value, score);
		ml.stage = stage_razoring;
		pos.set_mask(Piece(opp));
		if ((value = pos.score() + 200 + (depth << 5)) < beta) {
			score = Max(value, score);
			pos.set_mask(pos.mask() ^ Pawn(opp));
		}
	}
	ml.cur = ml.moves;
	ml.moves[0] = 0;
	if (depth <= 5) ml.gen_flags |= FlagNoBcSort;

///	do_split = sp_init = 0;
///	if (depth >= SplitDepth && PrN > 1 && parent && !exclusion) do_split = 1;

	while (move = ml.get_move<me,0>(pos)) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		cnt++;
		if (move == move_back) {
			score = Max(0, score);
			continue;
		}
		if (ml.stage == r_checks) check = 1;
		else check = pos.is_check<me>(move);
		if (T(check) && T(pos.see<me>(move, 0))) ext = 1 + (depth < 16);
		else ext = extension<0>(pos, move, depth);
		new_depth = depth - 2 + ext;
		if (F(Square(To(move))) && F(move & 0xE000)) {
			if (move != Current->killer[1] && move != Current->killer[2]) {
				if (F(check) && cnt > moves_to_play) {
				    ml.gen_flags &= ~FlagSort;
				    continue;
		        }
				if (depth >= 6) {
					int reduction = msb(cnt);
					if (move == Current->ref[0] || move == Current->ref[1]) reduction = Max(0, reduction - 1);
					if (reduction >= 2 && !(Queen(White) | Queen(Black)) && popcnt(NonPawnKingAll) <= 4) reduction += reduction / 2;
					if (new_depth - reduction > 3)
						if (F(pos.see<me>(move, -50))) reduction += 2;
					if (T(reduction) && reduction < 2 && new_depth - reduction > 3) {
						if (cnt > 3) reduction = 2;
						else reduction = 0;
					}
					new_depth = Max(3, new_depth - reduction);
				}
		    }
			if (F(check)) {
			    if ((value = pos.score() + DeltaM(move) + 10) < beta && depth <= 3) {
				    score = Max(value, score);
				    continue;
			    }
				if (cnt > 7 && (value = margin + DeltaM(move) - 25 * msb(cnt)) < beta && depth <= 19) {
					score = Max(value, score);
					continue;
				}
			}
			if (depth <= 9 && T(NonPawnKing(me)) && F(pos.see<me>(move,-50))) continue;
		} else {
			if (ml.stage == r_cap) {
				if (F(check) && depth <= 9 && F(pos.see<me>(move,-50))) continue;
			} else if (ml.stage == s_bad_cap && F(check) && depth <= 5) continue;
		}
///		if (do_split && played >= 1) {
///			if (!sp_init) {
///				sp_init = 1;
///				uint64_t u = ~Smpi->active_sp;
///				if (!u) {
///					do_split = 0;
///					goto make_move;
///				}
///				Sp = &Smpi->Sp[lsb(u)];
///				init_sp(Sp, beta - 1, beta, depth, 0, singular, height);
///			}
///			GMove * M = &Sp->move[Sp->move_number++];
///			M->ext = ext;
///			M->flags = 0;
///			M->move = move;
///			M->reduced_depth = new_depth;
///			M->research_depth = depth - 2 + ext;
///			M->stage = Current->stage;
///			continue;
///		}
///make_move:
		pos.do_move<me>(move);
		value = -search<opp, 0>(pos, 1 - beta, new_depth, FlagNeatSearch | ExtFlag(ext));
		if (value >= beta && new_depth < depth - 2 + ext) value = -search<opp, 0>(pos, 1 - beta, depth - 2 + ext, FlagNeatSearch | FlagDisableNull | ExtFlag(ext));
		pos.undo_move<me>(move);
		played++;
		if (value > score) {
			score = value;
			if (value >= beta) goto cut;
		}
	}
///	if (do_split && sp_init) {
///		value = smp_search<me>(Sp, pos);
///		if (value >= beta && Sp->best_move) {
///			score = beta;
///			pos.best() = move = Sp->best_move;
///			for (i = 0; i < Sp->move_number; i++) {
///				GMove * M = &Sp->move[i];
///				if ((M->flags & FlagFinished) && M->stage == s_quiet && M->move != move) HistoryBad(M->move);
///			}
///		}
///		if (value >= beta) goto cut;
///	}
	if (F(cnt) && F(flag)) {
		hash_high(pos.key(), 0, 127);
		hash_low(pos.key(), 0, 0, 127);
		return 0;
	}
	if (F(exclusion)) hash_high(pos.key(), score, depth);
	return score;
cut:
	if (exclusion) return score;
	pos.best() = move;
	if (depth >= 10) score = Min(beta, score);
	hash_low(pos.key(), move, score, depth);
	if (F(Square(To(move))) && F(move & 0xE000)) {
		if (Current->killer[1] != move && F(flags & FlagNoKillerUpdate)) {
			Current->killer[2] = Current->killer[1];
			Current->killer[1] = move;
		}
		HistoryGood(move);
		if (move != hash_move && ml.stage == s_quiet) for (p = ml.start; p < (ml.cur - 1); p++) HistoryBad(*p);
		UpdateRef(move);
	}
	return score;
}

template <bool me, bool exclusion> int search_evasion(Position& pos, int beta, int depth, int flags)
{
	constexpr bool opp = !me;
///	int i;
	int value, score, pext, move, cnt, hash_value = -MateValue, hash_depth, hash_move, new_depth, ext, check, moves_to_play;
	int height = pos.height();
	MoveList ml;

	if (depth <= 1) return q_evasion<me, 0>(pos, beta - 1, beta, 1, flags);
	score = height - MateValue;
	if (flags & FlagHaltCheck) {
	    if (score >= beta) return beta;
	    if (MateValue - height < beta) return beta - 1;
	    halt_check;
	}

	hash_depth = -1;
	hash_move = flags & 0xFFFF;
	if (exclusion) {
		cnt = pext = 0;
		score = beta - 1;
		ml.gen_evasions<me>(pos);
	    if (F(ml.moves[0])) return score;
		goto skip_hash_move;
	}
	pos.best() = hash_move;
	if (GEntry * Entry = TT.probe(pos.key())) {
		if (Entry->high < beta && Entry->high_depth >= depth) return Entry->high;
		if (T(Entry->move16) && Entry->low_depth > hash_depth) {
			pos.best() = hash_move = Entry->move16;
			hash_depth = Entry->low_depth;
		}
		if (Entry->low >= beta && Entry->low_depth >= depth) {
			if (Entry->move16) {
				pos.best() = Entry->move16;
				if (F(Square(To(Entry->move16))) && F(Entry->move16 & 0xE000)) UpdateCheckRef(Entry->move16);
			}
			return Entry->low;
		}
		if (Entry->low_depth >= depth - 8 && Entry->low > hash_value) hash_value = Entry->low;
	}

	if (depth >= 20) if (GPVEntry * PVEntry  = PVHASH.probe(pos.key())) {
		hash_low(pos.key(), PVEntry->move16,PVEntry->value,PVEntry->depth);
		hash_high(pos.key(), PVEntry->value,PVEntry->depth);
		if (PVEntry->depth >= depth) {
			if (PVEntry->move16) pos.best() = PVEntry->move16;
			return PVEntry->value;
		}
		if (T(PVEntry->move16) && PVEntry->depth > hash_depth) {
			pos.best() = hash_move = PVEntry->move16;
			hash_depth = PVEntry->depth;
			hash_value = PVEntry->value;
		}
	}

	if (hash_depth >= depth && hash_value > -EvalValue) score = Min(beta - 1, Max(score, hash_value));
	if (flags & FlagCallEvaluation) evaluate(pos);

	pos.set_mask(Filled);
	if (pos.score() - 10 < beta && depth <= 3) {
		pos.set_mask(Piece(opp));
		score = pos.score() - 10;
	    capture_margin<me>(pos, beta, score);
	}
	cnt = 0;
	pext = 0;
    ml.gen_evasions<me>(pos);
	if (F(ml.moves[0])) return score;
	if (F(ml.moves[1])) pext = 2;

	if (T(hash_move) && pos.is_legal<me>(move = hash_move)) {
		if (IsIllegal(me,move)) goto skip_hash_move;
		cnt++;
		check = pos.is_check<me>(move);
		if (check) ext = Max(pext, 1 + (depth < 16));
		else ext = MaxF(pext, extension<0>(pos, move, depth));
		if (depth >= 16 && hash_value >= beta && hash_depth >= (new_depth = depth - Min(12, depth/2))) {
			int margin_one = beta - ExclSingle(depth);
			int margin_two = beta - ExclDouble(depth);
			int prev_ext = Ext(flags);
			int singular = singular_extension<me>(pos, ext,prev_ext,margin_one,margin_two,new_depth,hash_move);
			if (singular) ext = Max(ext, singular + (prev_ext < 1) - (singular >= 2 && prev_ext >= 2));
		}
		new_depth = depth - 2 + ext;
		pos.do_move<me>(move);
		evaluate(pos);
		if (pos.att(opp) & King(me)) {
			pos.undo_move<me>(move);
			cnt--;
			goto skip_hash_move;
		}
		value = -search<opp, 0>(pos, 1 - beta, new_depth, FlagHaltCheck | FlagHashCheck | ((hash_value >= beta && hash_depth >= depth - 12) ? FlagDisableNull : 0) | ExtFlag(ext));
		pos.undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value >= beta) goto cut;
		}
	}
skip_hash_move:
	moves_to_play = 3 + ((depth * depth) / 6); 
	Current->ref[0] = RefM(pos.cur_move()).check_ref[0];
	Current->ref[1] = RefM(pos.cur_move()).check_ref[1];
	ml.mark_evasions(pos);
	ml.cur = ml.moves;
	while (move = ml.pick_move()) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		cnt++;
		if (IsRepetition(beta,move)) {
			score = Max(0, score);
			continue;
		}
		check = pos.is_check<me>(move);
		if (check) ext = Max(pext, 1 + (depth < 16));
		else ext = MaxF(pext, extension<0>(pos, move, depth));
		new_depth = depth - 2 + ext;
		if (F(Square(To(move))) && F(move & 0xE000)) {
			if (F(check)) {
				if (cnt > moves_to_play) continue;
				if ((value = pos.score() + DeltaM(move) + 10) < beta && depth <= 3) {
					score = Max(value, score);
					continue;
				}
			}
			if (depth >= 6 && cnt > 3) {
				int reduction = msb(cnt);
				if (reduction >= 2 && !(Queen(White) | Queen(Black)) && popcnt(NonPawnKingAll) <= 4) reduction += reduction / 2;
				new_depth = Max(3, new_depth - reduction);
			}
		}
		pos.do_move<me>(move);
		value = -search<opp, 0>(pos, 1 - beta, new_depth, FlagNeatSearch | ExtFlag(ext));
		if (value >= beta && new_depth < depth - 2 + ext) value = -search<opp, 0>(pos, 1 - beta, depth - 2 + ext, FlagNeatSearch | FlagDisableNull | ExtFlag(ext));
		pos.undo_move<me>(move);
		if (value > score) {
			score = value;
			if (value >= beta) goto cut;
		}
	}
	if (F(exclusion)) hash_high(pos.key(), score, depth);
	return score;
cut:
	if (exclusion) return score;
	pos.best() = move;
	hash_low(pos.key(), move, score, depth);
	if (F(Square(To(move))) && F(move & 0xE000)) UpdateCheckRef(move);
	return score;
}

template <bool me, bool root> int pv_search(Position& pos, int alpha, int beta, int depth, int flags)
{
	constexpr bool opp = !me;
	int i, value, move, cnt, pext = 0, ext, check, hash_value = -MateValue, margin;
	int singular = 0, played = 0,
		new_depth;
	int hash_move;
	int hash_depth, old_alpha = alpha, old_best, ex_depth = 0, ex_value = 0, start_knodes = (nodes >> 10);
///	GSP * Sp;
	int height = pos.height();
	MoveList ml;

	if (root) {
		depth = Max(depth, 2);
		flags |= ExtFlag(1);
		if (F(RootList[0])) return 0;
	    if (Print) {
			fprintf(stdout,"info depth %d\n",(depth/2)); 
			fflush(stdout);
		}
		int * p;
		for (p = RootList; *p; p++);
		sort_moves(RootList,p);
		for (p = RootList; *p; p++) *p &= 0xFFFF;
		SetScore(RootList[0],2);
		goto check_hash;
	}
	if (depth <= 1) return q_search<me, 1>(pos, alpha, beta, 1, FlagNeatSearch);
	if (height - MateValue >= beta) return beta;
	if (MateValue - height <= alpha) return alpha;
	halt_check;

check_hash:
	hash_depth = -1;
	pos.best() = hash_move = 0;
    if (GPVEntry * PVEntry = PVHASH.probe(pos.key())) {
		hash_low(pos.key(), PVEntry->move16,PVEntry->value,PVEntry->depth);
		hash_high(pos.key(), PVEntry->value,PVEntry->depth);
		if (PVEntry->depth >= depth && T(PVHashing)) {
			if (PVEntry->move16) pos.best() = PVEntry->move16;
			if ((pos.ply() <= 50 && PVEntry->ply <= 50) || (pos.ply() >= 50 && PVEntry->ply >= 50)) if (!PVEntry->value || !draw_in_pv<me>(pos)) return PVEntry->value;
		}
		if (T(PVEntry->move16) && PVEntry->depth > hash_depth) {
			pos.best() = hash_move = PVEntry->move16;
			hash_depth = PVEntry->depth;
			hash_value = PVEntry->value;
		}
	}
	if (GEntry * Entry = TT.probe(pos.key())) {
		if (T(Entry->move16) && Entry->low_depth > hash_depth) {
			pos.best() = hash_move = Entry->move16;
			hash_depth = Entry->low_depth;
			if (Entry->low_depth) hash_value = Entry->low;
		}
	}

	if (root) {
		hash_move = RootList[0];
		hash_value = Previous;
		hash_depth = Max(0, depth - 2);
	}

	evaluate(pos);

	if (F(root) && depth >= 6 && (F(hash_move) || hash_value <= alpha || hash_depth < depth - 8)) {
		if (F(hash_move)) new_depth = depth - 2;
		else new_depth = depth - 4;
		value = pv_search<me, 0>(pos, alpha, beta, new_depth, hash_move);
		if (value > alpha) {
hash_move_found:
			if (pos.best()) hash_move = pos.best();
		    hash_depth = new_depth;
		    hash_value = value;
			goto skip_iid;
		} else {
			i = 0;		
			new_depth = depth - 8;
iid_loop:
			margin = alpha - (8 << i);
			if (T(hash_move) && hash_depth >= Min(new_depth, depth - 8) && hash_value >= margin) goto skip_iid;
			value = search<me, 0>(pos, margin, new_depth, FlagHashCheck | FlagNoKillerUpdate | FlagDisableNull | FlagReturnBestMove | hash_move);
			if (value >= margin) goto hash_move_found;
			i++;
			if (i < 5) goto iid_loop;
		}
	}
skip_iid:
	if (F(root) && Check(me)) {
		alpha = Max(height - MateValue, alpha);
		pos.set_mask(Filled);
		ml.gen_evasions<me>(pos);
		if (F(ml.moves[0])) return height - MateValue; 
	    if (F(ml.moves[1])) pext = 2;
	}

	cnt = 0;
	if (hash_move && pos.is_legal<me>(move = hash_move)) {
		cnt++;
		if (root) {
			/// memset(Data + 1, 0, 127 * sizeof(GData));
			pos.clear_forward();
			move_to_string(move,score_string);
			if (Print) sprintf(info_string,"info currmove %s currmovenumber %d\n",score_string,cnt);
		}
		check = pos.is_check<me>(move);
		if (check) ext = 2;
		else ext = MaxF(pext, extension<1>(pos, move, depth));
		if (depth >= 12 && hash_value > alpha && hash_depth >= (new_depth = depth - Min(12,depth/2))) {
			int margin_one = hash_value - ExclSinglePV(depth);
			int margin_two = hash_value - ExclDoublePV(depth);
			int prev_ext = Ext(flags);
			singular = singular_extension<me>(pos, root ? 0 : ext,root ? 0 : prev_ext,margin_one,margin_two,new_depth,hash_move);
			if (singular) {
				ext = Max(ext, singular + (prev_ext < 1) - (singular >= 2 && prev_ext >= 2));
				if (root) CurrentSI->Singular = singular;
				ex_depth = new_depth;
				ex_value = (singular >= 2 ? margin_two : margin_one) - 1;
			}
		}
		new_depth = depth - 2 + ext;
		pos.do_move<me>(move);
		if (PrN > 1) {
			evaluate(pos);
			if (pos.att(opp) & King(me)) {
				pos.undo_move<me>(move);
				cnt--;
				goto skip_hash_move;
			}
		}
		value = -pv_search<opp, 0>(pos, -beta, -alpha, new_depth, ExtFlag(ext));
		pos.undo_move<me>(move);
		played++;
		if (value > alpha) {
			if (root) {
				CurrentSI->FailLow = 0;
			    best_move = move;
			    best_score = value;
				hash_low(pos.key(), best_move,value,depth);
				if (depth >= 14 || T(Console)) send_pv(pos, depth/2, old_alpha, beta, value);
			}
		    alpha = value;
			pos.best() = move;
			if (value >= beta) goto cut;
		} else if (root) {
			CurrentSI->FailLow = 1;
			CurrentSI->FailHigh = 0;
			CurrentSI->Singular = 0;
			if (depth >= 14 || T(Console)) send_pv(pos, depth/2, old_alpha, beta, value);
		}
	}
skip_hash_move:
	ml.gen_flags = 0;
	if (F(Check(me))) {
		ml.stage = stage_search;
		ml.ref[0] = RefM(pos.cur_move()).ref[0];
	    ml.ref[1] = RefM(pos.cur_move()).ref[1];
	} else {
		ml.stage = stage_evasion;
		ml.ref[0] = RefM(pos.cur_move()).check_ref[0];
		ml.ref[1] = RefM(pos.cur_move()).check_ref[1];
	}
	pos.killer(0) = 0;
	ml.moves[0] = 0;
	/// ToDo: 
///	if (root) Current->current = RootList + 1;
///	else Current->current = Current->moves;
	ml.cur = ml.moves;

///	if (PrN > 1 && !root && parent && depth >= SplitDepthPV) do_split = 1;

	while (move = ml.get_move<me,root>(pos)) {
		if (move == hash_move) continue;
		if (IsIllegal(me,move)) continue;
		cnt++;
		if (root) {
///			memset(Data + 1, 0, 127 * sizeof(GData));
			pos.clear_forward();
			move_to_string(move,score_string);
			if (Print) sprintf(info_string,"info currmove %s currmovenumber %d\n",score_string,cnt);
		}
		if (IsRepetition(alpha + 1,move)) continue;
		check = pos.is_check<me>(move);
		if (check) ext = 2;
		else ext = MaxF(pext, extension<1>(pos, move, depth));
		new_depth = depth - 2 + ext;
		if (depth >= 6 && F(move & 0xE000) && F(Square(To(move))) && (T(root) || (move != pos.killer(1) && move != pos.killer(2)) || T(Check(me))) && cnt > 3) {
			int reduction = msb(cnt) - 1;
			if (move == pos.ref(0) || move == pos.ref(1)) reduction = Max(0, reduction - 1);
			if (reduction >= 2 && !(Queen(White) | Queen(Black)) && popcnt(NonPawnKingAll) <= 4) reduction += reduction / 2;
			new_depth = Max(3, new_depth - reduction);
		}
///		if (do_split && played >= 1) {
///			if (!sp_init) {
///				sp_init = 1;
///				uint64_t u = ~Smpi->active_sp;
///				if (!u) {
///					do_split = 0;
///					goto make_move;
///				}
///				Sp = &Smpi->Sp[lsb(u)];
///				init_sp(Sp, alpha, beta, depth, 1, singular, height);
///			}
///			GMove * M = &Sp->move[Sp->move_number++];
///			M->ext = ext;
///			M->flags = 0;
///			M->move = move;
///			M->reduced_depth = new_depth;
///			M->research_depth = depth - 2 + ext;
///			M->stage = Current->stage;
///			continue;
///		}
///make_move:
		pos.do_move<me>(move);
		if (new_depth <= 1) value = -pv_search<opp, 0>(pos, -beta, -alpha, new_depth, ExtFlag(ext));
		else value = -search<opp, 0>(pos, -alpha, new_depth, FlagNeatSearch | ExtFlag(ext));
		if (value > alpha && new_depth > 1) {
			if (root) {
			    SetScore(RootList[cnt - 1],1);
			    CurrentSI->Early = 0;
			    old_best = best_move;
			    best_move = move;
			}
			new_depth = depth - 2 + ext;
			value = -pv_search<opp, 0>(pos, -beta, -alpha, new_depth, ExtFlag(ext));
			if (T(root) && value <= alpha) best_move = old_best;
		}
		pos.undo_move<me>(move);
		played++;
		if (value > alpha) {
			if (root) {
				SetScore(RootList[cnt - 1],cnt + 3);
				CurrentSI->Change = 1;
				CurrentSI->FailLow = 0;
			    best_move = move;
			    best_score = value;
				hash_low(pos.key(), best_move, value, depth);
				if (depth >= 14 || T(Console)) send_pv(pos, depth/2, old_alpha, beta, value);
			}
		    alpha = value;
			pos.best() = move;
			if (value >= beta) goto cut;
		}
	}
///	if (do_split && sp_init) {
///		value = smp_search<me>(Sp, pos);
///		if (value > alpha && Sp->best_move) {
///			alpha = value;
///			pos.best() = move = Sp->best_move;
///		}
///		if (value >= beta) goto cut;
///	}
	if (F(cnt) && F(Check(me))) {
		hash_high(pos.key(), 0, 127);
		hash_low(pos.key(), 0, 0, 127);
		hash_exact(pos.key(), 0, 0, pos.ply(), 127, 0, 0, 0);
	    return 0;
	}
	if (F(root) || F(SearchMoves)) hash_high(pos.key(), alpha, depth);
	if (alpha > old_alpha) {
		hash_low(pos.key(), pos.best(), alpha,depth); 
		if (pos.best() != hash_move) ex_depth = 0;
		if (F(root) || F(SearchMoves)) hash_exact(pos.key(), pos.best(), alpha, pos.ply(), depth, ex_value, ex_depth, Convert(nodes >> 10,int) - start_knodes); 
	}
	return alpha;
cut:
	hash_low(pos.key(), move, alpha, depth);
	return alpha;
}

template <bool me> void root()
{
	int i, depth, value, alpha, beta, delta, start_depth = 2, hash_depth = 0, hash_value, store_time = 0, time_est, ex_depth = 0, ex_value, prev_time = 0, knodes = 0;
	int64_t time;
	GPVEntry * PVEntry;
	Position& pos = position[0];
	MoveList ml;

	date++;
	nodes = check_node = check_node_smp = 0;
///	if (parent) Smpi->nodes = 0;
///	memcpy(Data, Current, sizeof(GData));
///	Current = Data;
	pos.reset_current();
	evaluate(pos);
	ml.gen_root_moves<me>(pos);
	if (PVN > 1) {
		memset(MultiPV,0,128 * sizeof(int));
		for (i = 0; MultiPV[i] = RootList[i]; i++);
	}
	best_move = RootList[0];
	if (F(best_move)) return;
	if (F(Infinite) && !RootList[1]) {
		Infinite = 1;
		value = pv_search<me, 1>(pos, -MateValue, MateValue, 4, FlagNeatSearch);
		Infinite = 0;
		LastDepth = 128;
		send_pv(pos, 6, -MateValue, MateValue, value);
		send_best_move(pos);
		Searching = 0;
///		if (MaxPrN > 1) ZERO_BIT_64(Smpi->searching, 0);
		// ToDo: 思考をストップさせる
		return;
	}

	memset(CurrentSI,0,sizeof(GSearchInfo));
	memset(BaseSI,0,sizeof(GSearchInfo));
	Previous = -MateValue;
	if (PVEntry = PVHASH.probe(pos.key())) {
		if (pos.is_legal<me>(PVEntry->move16) && PVEntry->move16 == best_move && PVEntry->depth > hash_depth) {
			hash_depth = PVEntry->depth;
			hash_value = PVEntry->value;
			ex_depth = PVEntry->ex_depth;
			ex_value = PVEntry->exclusion;
			knodes = PVEntry->knodes;
		}
	}
	if (T(hash_depth) && PVN == 1) {
		Previous = best_score = hash_value;
		depth = hash_depth;
		if (PVHashing) {
	        send_pv(pos, depth/2, -MateValue, MateValue, best_score);
		    start_depth = (depth + 2) & (~1);
		}
		if ((depth >= LastDepth - 8 || T(store_time)) && LastValue >= LastExactValue && hash_value >= LastExactValue && T(LastTime) && T(LastSpeed)) {
			time = TimeLimit1;
			if (ex_depth >= depth - Min(12, depth/2) && ex_value <= hash_value - ExclSinglePV(depth)) {
				BaseSI->Early = 1;
				BaseSI->Singular = 1;
				if (ex_value <= hash_value - ExclDoublePV(depth)) {
					time = (time * TimeSingTwoMargin)/100;
					BaseSI->Singular = 2;
				}
				else time = (time * TimeSingOneMargin)/100;
			}
			time_est = Min(LastTime,(knodes << 10)/LastSpeed);
			time_est = Max(time_est, store_time);
set_prev_time:
			LastTime = prev_time = time_est;
			if (prev_time >= time && F(Infinite)) {
				InstCnt++;
				if (time_est <= store_time) InstCnt = 0;
				if (InstCnt > 2) {
					if (T(store_time) && store_time < time_est) {
						time_est = store_time;
						goto set_prev_time;
					}
					LastSpeed = 0;
					LastTime = 0;
					prev_time = 0;
					goto set_jump;
				}
				if (hash_value > 0 && pos.ply() >= 2 && F(pos.square(To(best_move))) && F(best_move & 0xF000) && PrevMove == ((To(best_move) << 6) | From(best_move))) goto set_jump;
				pos.do_move<me>(best_move);
				if (pos.ply() >= 100) {
					pos.undo_move<me>(best_move);
					goto set_jump;
				}
///				for (i = 4; i <= pos.ply(); i+=2) if (Stack[sp-i] == pos.key()) {
///					pos.undo_move<me>(best_move);
///					goto set_jump;
///				}
				if (pos.is_repeat()) {
					pos.undo_move<me>(best_move);
					goto set_jump;
				}
				pos.undo_move<me>(best_move);
				LastDepth = depth;
				LastTime = prev_time;
				LastValue = LastExactValue = hash_value;
				send_best_move(pos);
				Searching = 0;
///				if (MaxPrN > 1) ZERO_BIT_64(Smpi->searching, 0);
				return;
			} else goto set_jump;
		}
	}
	LastTime = 0;
set_jump:
///	memcpy(SaveBoard,Board,sizeof(GBoard));
///	memcpy(SaveData,Data,sizeof(GData));
///	save_sp = sp;
///	if (setjmp(Jump)) {
///		Current = Data;
///		Searching = 0;
///		if (MaxPrN > 1) {
///			halt_all(0, 127);
///			ZERO_BIT_64(Smpi->searching, 0);
///		}
///		memcpy(Board,SaveBoard,sizeof(GBoard));
///		memcpy(Data,SaveData,sizeof(GData));
///		sp = save_sp;
///		send_best_move(pos);
///		return;
///	}
	for (depth = start_depth; depth < DepthLimit; depth += 2) {
///		memset(Data + 1, 0, 127 * sizeof(GData));
		pos.clear_forward();
		CurrentSI->Early = 1;
		CurrentSI->Change = CurrentSI->FailHigh = CurrentSI->FailLow = CurrentSI->Singular = 0;
		if (PVN > 1) value = multipv<me>(pos, depth);
		else if ((depth/2) < 7 || F(Aspiration)) LastValue = LastExactValue = value = pv_search<me, 1>(pos, -MateValue, MateValue, depth, FlagNeatSearch);
		else {
			delta = 8;
			alpha = Previous - delta;
			beta = Previous + delta;
loop:
			if (delta >= 16 * 32) {
				LastValue = LastExactValue = value = pv_search<me, 1>(pos, -MateValue, MateValue, depth, FlagNeatSearch);
				goto finish;
			}
			value = pv_search<me, 1>(pos, alpha, beta, depth, FlagNeatSearch);
			if (value <= alpha) {
				CurrentSI->FailHigh = 0;
				CurrentSI->FailLow = 1;
				alpha -= delta;
				delta *= 2;
				LastValue = value;
				memcpy(BaseSI,CurrentSI,sizeof(GSearchInfo));
				goto loop;
			} else if (value >= beta) {
				CurrentSI->FailHigh = 1;
				CurrentSI->FailLow = 0;
				CurrentSI->Early = 1;
				CurrentSI->Change = 0;
				CurrentSI->Singular = Max(CurrentSI->Singular, 1);
				beta += delta;
				delta *= 2;
				LastDepth = depth;
				LastTime = MaxF(prev_time,get_time() - StartTime);
				LastSpeed = nodes/Max(LastTime, 1);
				if (depth + 2 < DepthLimit) depth += 2;
				InstCnt = 0;
#ifdef TIMING
				if (depth >= 6)
#endif
				check_time(LastTime,0);
///				memset(Data + 1, 0, 127 * sizeof(GData));
				pos.clear_forward();
				LastValue = value;
				memcpy(BaseSI,CurrentSI,sizeof(GSearchInfo));
				goto loop;
			} else LastValue = LastExactValue = value;
		}
finish:
		if (value < Previous - 50) CurrentSI->Bad = 1;
		else CurrentSI->Bad = 0;
		memcpy(BaseSI,CurrentSI,sizeof(GSearchInfo));
		LastDepth = depth;
		LastTime = MaxF(prev_time,get_time() - StartTime);
		LastSpeed = nodes/Max(LastTime, 1);
		Previous = value;
		InstCnt = 0;
#ifdef TIMING
		if (depth >= 6)
#endif
		check_time(LastTime,0);
	}
	Searching = 0;
///	if (MaxPrN > 1) ZERO_BIT_64(Smpi->searching, 0);
	if (F(Infinite) || DepthLimit < 128) send_best_move(pos);
}

void send_pv(Position& pos, int depth, int alpha, int beta, int score)
{
	int i, cur, move, mate = 0, mate_score, sel_depth;
	int64_t nps, snodes;
	if (F(Print)) return;
///	for (sel_depth = 1; sel_depth < 127 && T((Data + sel_depth)->att[0]); sel_depth++);
///	sel_depth--;
	sel_depth = pos.sel_depth();
	pv_length = 64;
	if (F(move = best_move)) move = RootList[0];
	if (F(move)) return;
	PV[0] = move;
	if (pos.cur_turn()) pos.do_move<1>(move);
	else pos.do_move<0>(move);
	pvp = 1;
	pick_pv(pos);
	if (pos.cur_turn() ^ 1) pos.undo_move<1>(move);
	else pos.undo_move<0>(move);
	cur = 0;
	for (i = 0; i < 64 && T(PV[i]); i++) {
		if (cur > 0) { 
			pv_string[cur] = ' '; 
			cur++; 
		}
        move = PV[i];
        pv_string[cur++] = ((move >> 6) & 7) + 'a';
        pv_string[cur++] = ((move >> 9) & 7) + '1';
        pv_string[cur++] = (move & 7) + 'a';
        pv_string[cur++] = ((move >> 3) & 7) + '1';
        if (IsPromotion(move)) {
            if ((move & 0xF000) == FlagPQueen)  pv_string[cur++] = 'q';
            else if ((move & 0xF000) == FlagPRook)   pv_string[cur++] = 'r';
            else if ((move & 0xF000) == FlagPLight || (move & 0xF000) == FlagPDark) pv_string[cur++] = 'b';
            else if ((move & 0xF000) == FlagPKnight) pv_string[cur++] = 'n';
		}
        pv_string[cur] = 0;
	}
	score_string[0] = 'c';
	score_string[1] = 'p';
    if (score > EvalValue) {
		mate = 1;
		strcpy(score_string,"mate ");
		mate_score = (MateValue - score + 1)/2;
	    score_string[6] = 0;
	} else if (score < -EvalValue) {
		mate = 1;
        strcpy(score_string,"mate ");
		mate_score = -(score + MateValue + 1)/2;
		score_string[6] = 0;
	} else {
        score_string[0] = 'c';
	    score_string[1] = 'p';
		score_string[2] = ' ';
		score_string[3] = 0;
	}
	nps = get_time() - StartTime;
#ifdef MP_NPS
///	snodes = Smpi->nodes;
	snodes = nodes;
#else
	snodes = nodes;
#endif
	if (nps) nps = (snodes * 1000)/nps; 
	if (score < beta) {
		if (score <= alpha) fprintf(stdout,"info depth %d seldepth %d score %s%d upperbound nodes %" PRId64 " nps %" PRId64 " pv %s\n",depth,sel_depth,score_string,(mate ? mate_score : score),snodes,nps,pv_string);
		else fprintf(stdout,"info depth %d seldepth %d score %s%d nodes %" PRId64 " nps %" PRId64 " pv %s\n",depth,sel_depth,score_string,(mate ? mate_score : score),snodes,nps,pv_string);
	} else fprintf(stdout,"info depth %d seldepth %d score %s%d lowerbound nodes %" PRId64 " nps %" PRId64 " pv %s\n",depth,sel_depth,score_string,(mate ? mate_score : score),snodes,nps,pv_string);
	fflush(stdout);
}

void send_multipv(Position& pos, int depth, int curr_number) {
	int i, j, cur, move, score;
	int64_t nps, snodes;
	if (F(Print)) return;
	for (j = 0; j < PVN && T(MultiPV[j]); j++) {
		pv_length = 63;
		pvp = 0;
		move = MultiPV[j] & 0xFFFF;
		score = MultiPV[j] >> 16;
		memset(PV,0,64 * sizeof(uint16_t));
		if (pos.cur_turn()) pos.do_move<1>(move);
	    else pos.do_move<0>(move);
		pick_pv(pos);
		if (pos.cur_turn() ^ 1) pos.undo_move<1>(move);
	    else pos.undo_move<0>(move);
		for (i = 63; i > 0; i--) PV[i] = PV[i - 1];
		PV[0] = move;
		cur = 0;
		for (i = 0; i < 64 && T(PV[i]); i++) {
			if (cur > 0) { 
				pv_string[cur] = ' '; 
				cur++; 
			}
        	move = PV[i];
        	pv_string[cur++] = ((move >> 6) & 7) + 'a';
        	pv_string[cur++] = ((move >> 9) & 7) + '1';
        	pv_string[cur++] = (move & 7) + 'a';
        	pv_string[cur++] = ((move >> 3) & 7) + '1';
        	if (IsPromotion(move)) {
            	if ((move & 0xF000) == FlagPQueen)  pv_string[cur++] = 'q';
            	else if ((move & 0xF000) == FlagPRook)   pv_string[cur++] = 'r';
            	else if ((move & 0xF000) == FlagPLight || (move & 0xF000) == FlagPDark) pv_string[cur++] = 'b';
            	else if ((move & 0xF000) == FlagPKnight) pv_string[cur++] = 'n';
			}
        	pv_string[cur] = 0;
		}
		score_string[0] = 'c';
		score_string[1] = 'p';
		if (score > EvalValue) {
			strcpy(score_string,"mate ");
			score = (MateValue - score + 1)/2;
	    	score_string[6] = 0;
		} else if (score < -EvalValue) {
        	strcpy(score_string,"mate ");
			score = -(score + MateValue + 1)/2;
			score_string[6] = 0;
		} else {
        	score_string[0] = 'c';
	    	score_string[1] = 'p';
			score_string[2] = ' ';
			score_string[3] = 0;
		}
		nps = get_time() - StartTime;
#ifdef MP_NPS
///		snodes = Smpi->nodes;
		snodes = nodes;
#else
		snodes = nodes;
#endif
	    if (nps) nps = (snodes * 1000)/nps; 
		fprintf(stdout,"info multipv %d depth %d score %s%d nodes %" PRId64 " nps %" PRId64 " pv %s\n",j + 1,(j <= curr_number ? depth : depth - 1),score_string,score,snodes,nps,pv_string);
		fflush(stdout);
	}
}

void send_best_move(Position& pos) {
	uint64_t snodes;
	int ponder;
	if (F(Print)) return;
#ifdef MP_NPS
///	snodes = Smpi->nodes;
#else
	snodes = nodes;
#endif
	fprintf(stdout,"info nodes %" PRIu64 " score cp %d\n",snodes,best_score);
	if (!best_move) return;
	pos.rewind();
	evaluate(pos);
	if (pos.cur_turn()) pos.do_move<1>(best_move);
	else pos.do_move<0>(best_move);
	pv_length = 1;
	pvp = 0;
	pick_pv(pos);
	ponder = PV[0];
	if (pos.cur_turn() ^ 1) pos.undo_move<1>(best_move);
	else pos.undo_move<0>(best_move);
	move_to_string(best_move,pv_string);
	if (ponder) {
		move_to_string(ponder,score_string);
		fprintf(stdout,"bestmove %s ponder %s\n",pv_string,score_string);
	} else fprintf(stdout,"bestmove %s\n",pv_string);
	fflush(stdout);
}
// 

//
//template int q_search<false, false>(int alpha, int beta, int depth, int flags);
//template int q_evasion<false, false>(int alpha, int beta, int depth, int flags);

//template int search<false, false>(int beta, int depth, int flags);

template void root<false>();
template void root<true>();

