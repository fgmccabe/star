:- module(sort,[sort/3]).
:- use_module(misc).


sort(L,C,P) :- mergeSort(L,C,P).

mergeSort([],_,[]).
mergeSort([E],_,[E]).
mergeSort(L,C,S) :-
  split(L,Lf,Rg),!,
  mergeSort(Lf,C,Ls),
  mergeSort(Rg,C,Rs),
  merge(Ls,Rs,C,S),!.

split([],[],[]).
split([E],[],[E]).
split([E1,E2|L],[E1|L1],[E2|L2]) :-
  split(L,L1,L2).

merge([],R,_,R).
merge(L,[],_,L).
merge([E1|L1],[E2|R2],C,[E1|S]) :- call(C,E1,E2),! merge(L1,[E2|R2],C,S).
merge([E1|L1],[E2|R2],C,[E2|S]) :- merge([E1|L1],R2,C,S).
