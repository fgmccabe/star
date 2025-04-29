:-module(meta,[parseFlags/5,checkOpt/3,
	       showMsg/4,showAst/4,showConstraint/4]).

:- use_module(errors).
:- use_module(misc).

parseFlags([],CWD,CWD,[],[]).
parseFlags(['-g'|More],CWD,Cx,[debugging|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-w',W|M],CW,Cx,Opts,Files) :-!,
  atom_string(W,WN),
  parseURI(WN,WU),
  resolveURI(CW,WU,CWD),
  parseFlags(M,CWD,Cx,Opts,Files).
parseFlags(['-r', R|More],CWD,Cx,[repository(Repo)|Opts],Files) :-!,
  atom_string(R,RN),
  parseURI(RN,RU),
  resolveURI(CWD,RU,Ruri),
  openRepository(Ruri,Repo),
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-c'|More],CWD,Cx,[typeCheckOnly|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-tc'|More],CWD,Cx,[traceCheck|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-m'|More],CWD,Cx,[macroOnly|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-n'|More],CWD,Cx,[transformOnly|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-v', V|More],CWD,Cx,[ver(Vers)|Opts],Files) :-!,
  atom_string(V,Vers),
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-f'|More],CWD,Cx,[forceCompile|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-di'|More],CWD,Cx,[showGenCode|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-ti'|More],CWD,Cx,[traceGenCode|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-da'|More],CWD,Cx,[showAst|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-dm'|More],CWD,Cx,[showMacro|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-dd'|More],CWD,Cx,[showGroups|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-dc'|More],CWD,Cx,[showTCCode|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-dn'|More],CWD,Cx,[showTrCode|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-tn'|More],CWD,Cx,[traceNormalize|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-dA'|More],CWD,Cx,[showSetCode|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['--stdin'|More],CWD,Cx,[processStdin|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['--'|More], CWD,CWD, [], Files) :- !, stringify(More,Files).
parseFlags(More, CWD,CWD, [], Files) :- stringify(More,Files).

stringify([],[]).
stringify([Name|More],[Fn|Files]) :-
  atom_string(Name,Fn),
  stringify(More,Files).

isOpt(Opts,Flag) :-
  is_member(Flag,Opts).

checkOpt(Opts,Flag,Cmd) :-
  is_member(Flag,Opts),!,
  call(Cmd,Flag).
checkOpts(_,_,_) :-!.

showAst(Lc,M,A,_) :-
  reportMsg(M,[ast(A)],Lc).

showConstraint(Lc,M,A,_) :-
  reportMsg(M,[con(A)],Lc).

showMsg(none,M,A,_) :-
  reportMsg(M,A).
showMsg(Lc,M,A,_) :-
  reportMsg(M,A,Lc).
