:- module(lambda,[invoke/3, ff/1, mapper/3]).

invoke(F,A,R) :- F=..[P|_], call(P,F,A,R).

% factorial done as lambda

fact(fact(_),X,R) :- X = 0, !, R=1.
fact(fact(F),X,R) :- X>0, X1 is X-1, invoke(F,X1,R1), R is R1*X.

ff(F) :- F=fact(F).


% This is based on the definition:

% mapper(F) => let {
%  mp([]) => []
%  mp([E,..L]) => [F(E),..mp(L)]
%} in mp

mp(mp(_),[],[]).
mp(mp(F),[E|L],[R|M]) :- invoke(F,E,R), mp(mp(F),L,M).

mapper(mapper,F, mp(F)).

