:- use_module(library(dcg/basics)).

list([V | L]) --> integer(V), "\n", list(L).
list([]) --> [], "\n".

cmp(Previous, Current, Add) :- Previous < Current, Add = 1.
cmp(_, _, Add) :- Add = 0.

aggr(Stream, [ Previous | [ Current | Tail ]], Counter) :-
  aggr(Stream, [ Current | Tail ], Acc),
  cmp(Previous, Current, Add),
  Counter is Add + Acc.
aggr(_, _, 0).
aggr(Stream, Counter) :-
  phrase_from_stream(list(VList), Stream),
  aggr(Stream, VList, Counter).

:-
  open('input.txt', read, Fd),
  aggr(Fd, C), 
  format('counter: ~w~n', [C]).
