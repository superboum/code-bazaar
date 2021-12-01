:- use_module(library(dcg/basics)).

list([V | L]) --> integer(V), "\n", list(L).
list([]) --> [], "\n".

sliding(_, 0, []).
sliding([H|T], N, [H|P]) :-
  D is N - 1,
  sliding(T, D, P), !.

cmp([H|T]) :-
  sliding([H|T], 3, LS), sumlist(LS, LN),
  sliding(T, 3, TS), sumlist(TS, TN),
  TN > LN.

iter(L) :- cmp(L).
iter([_|T]) :- iter(T).

aggr(Stream, R) :-
  phrase_from_stream(list(L), Stream),
  aggregate_all(count, iter(L), R).

:-
  open('input.txt', read, Fd),
  aggr(Fd, C), 
  format('counter: ~w~n', [C]).
