:-module(errors,[reportError/3,reportError/2,reportMsg/2,reportMsg/3,genMsg/3,
          errorCount/1,noErrors/0,startCount/0]).
:- use_module(display).
:- use_module(abstract).
:- use_module(lexer).
:- use_module(types).
:- use_module(canon).
:- use_module(location).
:- use_module(uri).

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
  limitErrors(C),
  C1 is C+1,
  nb_setval(errors,C1).

errorCount(C) :-
  nb_getval(errors,C).

noErrors :-
  errorCount(0).

limitErrors(C) :-
  C>100,!,
  writef("Too many errors\n"),
  abort.
limitErrors(_).

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
genDisplay([A|L],[D|LL]) :- showTrm(A,D), !, genDisplay(L,LL).

genMsg(Msg,Args,Str) :-
  genDisplay(Args,AA),
  swritef(Str,Msg,AA).

showTrm(T,O) :-
  string(T),!,
  string_chars(T,O).
showTrm(T,O) :-
  isAst(T), !, dispAst(T,0,O,[]).
showTrm(L,O) :-
  isToken(L), !,
  dispToken(L,O).
showTrm(T,O) :-
  isType(T),!,
  showType(T,true,O,[]).
showTrm(C,O) :-
  isConstraint(C),!,
  showConstraint(C,O,[]).
showTrm(T,O) :-
  isCanon(T),!,
  showCanonTerm(T,0,O,[]).
showTrm(T,O) :-
  isLocation(T),
  showLocation(T,O,[]).
showTrm(P,O) :-
  isPkg(P),!,
  showPkg(P,O,[]).
showTrm(U,O) :-
  isUri(U),!,
  showUri(U,O,[]).
showTrm([],[]).
showTrm([E|_],O) :-
  showTrm(E,O).
showTrm(T,O) :- term_string(T,S), !,string_chars(S,O).
