#!/bin/env -S swipl -q

:- use_module(library(dcg/basics)).

% parser part 1
parse(D, H) --> "forward ", integer(V), "\n", parse(D, HP), { H is V + HP }.
parse(D, H) --> "down ", integer(V), "\n", parse(DP, H), { D is V + DP }.
parse(D, H) --> "up ", integer(V), "\n", parse(DP, H), { D is DP - V }.
parse(0, 0) --> "\n".

% parser part 2
parse(A, D, H) --> "forward ", integer(V), "\n", parse(A, DP, HP), { H is V + HP, D = DP + A * V }.
parse(A, D, H) --> "down ", integer(V), "\n", parse(AN, D, H), { AN = A + V }.
parse(A, D, H) --> "up ", integer(V), "\n", parse(AN, D, H), { AN = A - V }.
parse(_, 0, 0) --> "\n".

:- 
  open('day2.txt', read, F),
  phrase_from_stream(parse(0, D, H), F),
  R is D * H,
  format('result: ~w~n', [R]).
