:- use_module(library(dcg/basics)).

list([V | L]) --> integer(V), "\n", list(L).
list([]) --> [], "\n".

cmp(Previous, Current, Add) :- Previous < Current, Add = 1.
cmp(_, _, Add) :- Add = 0.

iter([ Previous | [ Current | Tail ]], Counter) :-
  iter([ Current | Tail ], Acc),
  cmp(Previous, Current, Add),
  Counter is Add + Acc.
iter(_, 0).
aggr(Stream, Counter) :-
  phrase_from_stream(list(VList), Stream),
  iter(VList, Counter).

:-
  open('input.txt', read, Fd),
  aggr(Fd, C), 
  format('counter: ~w~n', [C]).
