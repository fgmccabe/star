:- module(ocall,[ocall/3]).

ocall(Call,Lbl,ThVr) :- functor(Lbl,P,_), call(P,Call,Lbl,ThVr).
