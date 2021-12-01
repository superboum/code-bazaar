#!/bin/env -S swipl -q

:- use_module(library(dcg/basics)).

% parser
parse([V | L]) --> integer(V), "\n", parse(L).
parse([]) --> [], "\n".

% get a sliding window of size S
sliding(_, 0, []).
sliding([H|T], S, [H|P]) :- S > 0, D is S - 1, sliding(T, D, P).

% get the sum on a sliding window of size S
sum_sliding(L, S, R) :- sliding(L, S, LR), sumlist(LR, R).

% get two consecutive sliding windows, P and N, of size S
consecutive_sliding([H|T], S, P, N) :- sum_sliding([H|T], S, P), sum_sliding(T, S, N).

% generate all the possible combinations of consecutive entries
iter(L, S) :- consecutive_sliding(L, S, P, N), N > P.
iter([_|T], S) :- iter(T, S).

% parse the stream and count the number of true statement
aggr(F, S, R) :- phrase_from_stream(parse(L), F), aggregate_all(count, iter(L, S), R).

% open the file and display the result
disp(S) :- open('day1.txt', read, F), aggr(F, S, R), format('win: ~w, result: ~w~n', [S, R]).

:- disp(1), disp(3).
