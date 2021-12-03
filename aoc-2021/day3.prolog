#!/bin/env -S swipl -q

:- use_module(library(dcg/basics)).

% parser part 1
parse(L, C) --> line(X), parse(PL, PC), { lmerge(X, PL, L), C is PC + 1 }.
parse([], 0) --> "\n".
line([X|R]) --> digit(Y), line(R), { number_chars(X,[Y]) }.
line([]) --> "\n".

lmerge(L, [], L).
lmerge([H1|T1], [H2|T2], [HR|TR]) :- HR is H1 + H2, lmerge(T1, T2, TR).

cmp([H|T], C, [HR|TR]) :- H > C/2, HR = 1, cmp(T, C, TR).
cmp([H|T], C, [HR|TR]) :- H < C/2, HR = 0, cmp(T, C, TR).
cmp([], _, []).

neg([1|T], [0|TR]) :- neg(T, TR).
neg([0|T], [1|TR]) :- neg(T, TR).
neg([], []).

blist_str(L, X) :- blist_str(L, X, _).
blist_str([H|T], X, C) :- X = Y + H << D, C = D + 1, blist_str(T, Y, D).
blist_str([], 0, 0).

t(R) :- 
  open('day3.txt', read, F),
  phrase_from_stream(parse(L, C), F),
  cmp(L, C, G), neg(G, E),
  blist_str(G, GD), blist_str(E, ED),
  R is GD * ED.

