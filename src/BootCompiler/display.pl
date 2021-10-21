:- module(display,[display/1,displayln/1,ss_to_chrs/3,ss_to_str/2,validSS/1]).

:- use_module(misc).

display(L) :-
  ss_to_str(L,Str),
  write(Str).

displayln(L) :-
  ss_to_str(L,Str),
  writeln(Str).

ss_to_chrs(ss(Str),C,Cx) :-!,
  appStr(Str,C,Cx).
ss_to_chrs(qt(Qt,Str),C,Cx) :-!,
  appQuoted(Str,Qt,C,Cx).
ss_to_chrs(ix(Ix),C,Cx) :-!,
  appInt(Ix,C,Cx).
ss_to_chrs(cp(Cp),C,Cx) :-!,
  appChr(Cp,C,Cx).
ss_to_chrs(fx(Dx),C,Cx) :-!,
  appFlt(Dx,C,Cx).
ss_to_chrs(id(Id),C,Cx) :-!,
  appIden(Id,C,Cx).
ss_to_chrs(sq(L),C,Cx) :-!,
  ss_seq(L,C,Cx).
ss_to_chrs(iv(S,L),C,Cx) :-!,
  ss_interleave(L,S,C,Cx).
ss_to_chrs(nl,C,Cx) :-!,
  appStr("\n",C,Cx).
ss_to_chrs(nl(Sp),C,Cx) :-!,
  appStr("\n",C,C0),
  appIndx(Sp,C0,Cx).
ss_to_chrs(lp,C,Cx) :-!,
  appStr("(",C,Cx).
ss_to_chrs(rp,C,Cx) :-!,
  appStr(")",C,Cx).
ss_to_chrs(lb,C,Cx) :-!,
  appStr("{",C,Cx).
ss_to_chrs(rb,C,Cx) :-!,
  appStr("}",C,Cx).
ss_to_chrs(X,C,Cx) :-
  call(X,Sq),!,
  ss_to_chrs(Sq,C,Cx).

ss_seq([],Cx,Cx).
ss_seq([S|Seq],C,Cx) :-
  ss_to_chrs(S,C,C0),
  ss_seq(Seq,C0,Cx).

ss_interleave([],_,Cx,Cx).
ss_interleave([S|Seq],Sp,C,Cx) :-
  ss_to_chrs(S,C,C0),
  ss_interleave_more(Seq,Sp,C0,Cx).

ss_interleave_more([],_,Cx,Cx).
ss_interleave_more([S|Seq],Sp,C,Cx) :-
  ss_to_chrs(Sp,C,C0),
  ss_to_chrs(S,C0,C1),
  ss_interleave_more(Seq,Sp,C1,Cx).

ss_to_str(L,Str) :-
  ss_to_chrs(L,Chrs,[]),
  string_chars(Str,Chrs).

validSS(ss(S)) :-!, string(S).
validSS(cp(S)) :-!, atom(S).
validSS(ix(I)) :-!, integer(I).
validSS(fx(Dx)) :-!, float(Dx).
validSS(id(Id)) :-!, string(Id).
validSS(sq(L)) :-!,
  validSS_seq(L),!.
validSS(iv(S,L)) :-!,
  validSS(S),
  validSS_seq(L).
validSS(nl) :-!.
validSS(nl(I)) :-!, integer(I).
validSS(lp) :-!.
validSS(rp) :-!.
validSS(lb) :-!.
validSS(rb) :-!.
validSS(X) :-
  call(X,Sq),!,
  validSS(Sq).

validSS_seq([]).
validSS_seq([D|L]) :-
  validSS(D),
  validSS_seq(L).
