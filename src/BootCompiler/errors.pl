:-module(errors,[reportError/3,reportError/2,reportMsg/2,reportMsg/3,errorCount/1,noErrors/0,startCount/0]).
:- use_module(display).
:- use_module(abstract).
:- use_module(lexer).
:- use_module(types).
:- use_module(canon).
:- use_module(location).

:- initialization(startCount).

reportError(Msg,A,Lc) :- incErrorCount(),
  errorCount(E),
  showLocation(Lc,OLc,[]),
  writef("Error %w: at %s\n",[E,OLc]),
  genDisplay(A,AA),
  writef(Msg,AA),nl(),!.
reportError(Msg,A) :- incErrorCount(),
  errorCount(E),
  writef("Error %w:\n",[E]),
  genDisplay(A,AA),
  writef(Msg,AA),nl(),!.

startCount :-
  nb_setval(errors,0).

incErrorCount() :-
  nb_getval(errors,C),
  C1 is C+1,
  nb_setval(errors,C1).

errorCount(C) :-
  nb_getval(errors,C).

noErrors :-
  errorCount(0).

reportMsg(Msg,A,Lc) :- 
  showLocation(Lc,OLc,[]),
  writef("Info: at %s ",[OLc]),
  genDisplay(A,AA),
  writef(Msg,AA),nl().

reportMsg(Msg,A) :- 
  writef("Info: "),
  genDisplay(A,AA),
  writef(Msg,AA),nl().

genDisplay([],[]).
genDisplay([A|L],[D|LL]) :- showTerm(A,D), !, genDisplay(L,LL).

showTerm(T,O) :- 
  string(T),!,
  string_chars(T,O).
showTerm(T,O) :- 
  isAst(T), !, dispAst(T,0,O,[]).
showTerm(L,O) :-
  isToken(L), !,
  dispToken(L,O).
showTerm(T,O) :-
  isType(T),!,
  showType(T,O,[]).
showTerm(C,O) :-
  isConstraint(C),!,
  showConstraint(C,O,[]).
showTerm(T,O) :-
  isCanon(T),!,
  showCanonTerm(T,O,[]).
showTerm(T,O) :-
  isLocation(T),
  showLocation(T,O,[]).
showTerm([],[]).
showTerm([E|_],O) :-
  showTerm(E,O).
showTerm(T,O) :- term_string(T,S), !,string_chars(S,O).
