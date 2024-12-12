:-module(errors,[reportError/3,reportFatal/2,reportFatal/3,
		 reportWarning/3,reportMsg/2,reportMsg/3,genMsg/3,
		 testOpt/2,
		 errorCount/1,warningCount/1,
		 noErrors/0,noNewErrors/1,startCount/0]).
:- use_module(astdisp).
:- use_module(abstract).
:- use_module(display).
:- use_module(lexer).
:- use_module(types).
:- use_module(canon).
:- use_module(location).
:- use_module(lterms).
:- use_module(uri).

:- initialization(startCount).

reportError(Msg,A,Lc) :- incErrorCount(),
  errorCount(E),
  showLocation(Lc,OLc,[]),
  writef("\nError %w - %s\n",[E,OLc]),
  genDisplay(A,AA),
  writef("<."),
  writef(Msg,AA),
  writef(".>"),nl(),!.

reportWarning(Msg,A,Lc) :- incWarningCount(),
  warningCount(E),
  showLocation(Lc,OLc,[]),
  writef("\nWarning %w - %s\n",[E,OLc]),
  genDisplay(A,AA),
  writef("<."),
  writef(Msg,AA),
  writef(".>"),nl(),!.

reportFatal(Msg,A,Lc) :-
  reportError(Msg,A,Lc),
  abort.
reportFatal(Msg,A) :-
  reportMsg(Msg,A),
  abort.

testOpt(none,_) :-!.
testOpt(some(Msg),Lc) :-
  reportFatal(Msg,[],Lc).

startCount :-
  nb_setval(errors,0),
  nb_setval(warnings,0).

incErrorCount() :-
  nb_getval(errors,C),
  limitErrors(C),
  C1 is C+1,
  nb_setval(errors,C1).

errorCount(C) :-
  nb_getval(errors,C).

incWarningCount() :-
  nb_getval(warnings,C),
  C1 is C+1,
  nb_setval(warnings,C1).

warningCount(C) :-
  nb_getval(warnings,C).

noErrors :-
  errorCount(0),!.
noErrors :-
  abort.

noNewErrors(Count) :-
  errorCount(Count).

limitErrors(C) :-
  C>30,!,
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
showTrm(ast(T),O) :-
  dispAst(T,0,O,[]).
showTrm(can(C),O) :-
  ss_to_chrs(canon:ssTerm(C,0),O,[]).
showTrm(cnact(C),O) :-
  ss_to_chrs(canon:ssAction(C,0),O,[]).
showTrm(rle(C),O) :-
  ss_to_chrs(canon:ssRule(C,0),O,[]).
showTrm(tpe(T),O) :-
  ss_to_chrs(types:ssType(T,true,0),O,[]).
showTrm(con(T),O) :-
  ss_to_chrs(types:ssConstraint(true,0,T),O,[]).
showTrm(dcl(D),O) :-
  ss_to_chrs(canon:ssDecl(D),O,[]).
showTrm(ldef(D),O) :-
  ss_to_chrs(lterms:ssRuleSet(D),O,[]).
showTrm(ltrm(L),O) :-
  ss_to_chrs(lterms:ssTrm(L,0),O,[]).
showTrm(lact(A),O) :-
  ss_to_chrs(lterms:ssAct(A,0),O,[]).
showTrm(ds(D),O) :-
  ss_to_chrs(D,O,[]).
showTrm(pk(P),O) :-
  ss_to_chrs(canon:ssPkg(P),O,[]).
showTrm(id(S),O) :-
  ss_to_chrs(id(S),O,[]).
showTrm(stk(some(N)),O) :-
  ss_to_chrs(sq([ss("Stk "),ix(N)]),O,[]).
showTrm(stk(none),O) :-
  ss_to_chrs(ss("Stk empty"),O,[]).
showTrm(tok(Tk),O) :-
  showToken(Tk,O).
showTrm(loc(Lc),O) :-
  showLocation(Lc,O,[]).
showTrm(canDef(T),O) :-
  ss_to_chrs(canon:ssDef(0,T),O,[]).
showTrm(ss(S),O) :-
  string_chars(S,O).
showTrm(T,O) :-
  isAst(T), !, dispAst(T,0,O,[]).
showTrm(L,O) :-
  isToken(L), !,
  showToken(L,O).
showTrm(T,O) :-
  isType(T),!,
  ss_to_chrs(types:ssType(T,true,0),O,[]).
showTrm(C,O) :-
  isConstraint(C),!,
  ss_to_chrs(types:ssConstraint(false,0,C),O,[]).
showTrm(T,O) :-
  isCanon(T),!,
  ss_to_chrs(canon:ssTerm(T,0),O,[]).
showTrm(T,O) :-
  isCanonDef(T),!,
  ss_to_chrs(canon:ssDef(0,T),O,[]).
showTrm(T,O) :-
  isLocation(T),
  showLocation(T,O,[]).
showTrm(P,O) :-
  isPkg(P),!,
  ss_to_chrs(canon:ssPkg(P),O,[]).
showTrm(U,O) :-
  isUri(U),!,
  showUri(U,O,[]).
showTrm([],[]).
showTrm([E|Es],O) :-
  showTrm(E,O0),
  showTrm(Es,O1),
  misc:concat(O0,[' '|O1],O).
showTrm(T,O) :- term_string(T,S), !,string_chars(S,O).
