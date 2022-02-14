:- module(encode,
	  [encode/2,encodeTerm/3,encType/2,encodeType/3,encLtp/2,
	   encodeLtipe/3,encodeConstraint/3]).

:- use_module(misc).
:- use_module(base64).
:- use_module(types).

encode(T,Str) :-
  encodeTerm(T,Chrs,[]),!,
  string_chars(Str,Chrs).

encodeTerm(voyd, ['v'|O],O).
encodeTerm(intgr(Ix),['x'|O],Ox) :- encodeInt(Ix,O,Ox).
encodeTerm(bigx(Ix),['b'|O],Ox) :- encodeText(Ix,O,Ox).
encodeTerm(float(Dx),['d'|O],Ox) :- encodeFloat(Dx,O,Ox).
encodeTerm(enum(Nm),['e'|O],Ox) :- encodeText(Nm,O,Ox).
encodeTerm(chr(Cp),['c'|O],Ox) :- appChr(Cp,O,Ox).
encodeTerm(strg(St),['s'|O],Ox) :- encodeText(St,O,Ox).
encodeTerm(lbl(Nm,Arity),['o'|O],Ox) :-
  encodeInt(Arity,O,O1),
  encodeText(Nm,O1,Ox).
encodeTerm(ctpl(Con,Els),['n'|O],Ox) :-
  length(Els,Ln),
  encodeInt(Ln,O,O1),
  encodeTerm(Con,O1,O2),
  encodeTerms(Els,O2,Ox).
encodeTerm(lst(Els),['l'|O],Ox) :-
  length(Els,Ln),
  encodeInt(Ln,O,O1),
  encodeTerms(Els,O1,Ox).

encodeTerms([],O,O).
encodeTerms([T|R],O,Ox) :-
  encodeTerm(T,O,O1),
  encodeTerms(R,O1,Ox).

encodeInt(N,['-'|O],Ox) :- N<0, N1 is -N, encodeInt(N1,O,Ox).
encodeInt(K,[D|O],O) :- K>=0 , K=<10, digit(K,D).
encodeInt(N,O,Ox) :- N1 is N div 10, encodeInt(N1,O,[D|Ox]), K is N mod 10, digit(K,D).

encodeFloat(F,C,Cx) :-
  swritef(S,"'%d'",[F]),
  appStr(S,C,Cx).

findDelim(Chrs,Delim) :-
  is_member(Delim,['''','"', '|', '/', '%']),
  \+ is_member(Delim,Chrs),!.
findDelim(_,'"').

encodeText(Txt,[Delim|O],Ox) :-
  string_chars(Txt,Chrs),
  findDelim(Chrs,Delim),
  encodeQuoted(Chrs,Delim,O,Ox).

encodeQuoted([],Delim,[Delim|Ox],Ox) :- !.
encodeQuoted(['\\'|More],Delim,['\\','\\'|O],Ox) :-
  encodeQuoted(More,Delim,O,Ox).
encodeQuoted([Delim|More],Delim,['\\',Delim|O],Ox) :-
  encodeQuoted(More,Delim,O,Ox).
encodeQuoted([Ch|More],Delim,[Ch|O],Ox) :-
  encodeQuoted(More,Delim,O,Ox).

digit(0,'0').
digit(1,'1').
digit(2,'2').
digit(3,'3').
digit(4,'4').
digit(5,'5').
digit(6,'6').
digit(7,'7').
digit(8,'8').
digit(9,'9').

encType(Tp,Sig) :-
  encodeType(Tp,O,[]),
  string_chars(Sig,O).

encodeType(T,C,Cx) :-
  deRef(T,Tp),
  encodeT(Tp,C,Cx),!.

encodeT(anonType,['_'|O],O).
encodeT(voidType,['v'|O],O).
encodeT(T,['_'|O],O) :- isUnbound(T).
encodeT(type("star.core*boolean"),['l'|O],O).
encodeT(type("star.core*integer"),['i'|O],O).
encodeT(type("star.core*bigint"),['b'|O],O).
encodeT(type("star.core*float"),['f'|O],O).
encodeT(type("star.core*char"),['c'|O],O).
encodeT(type("star.core*string"),['s'|O],O).
encodeT(kVar(Nm),['k'|O],Ox) :- encodeText(Nm,O,Ox).
encodeT(kFun(Nm,Ar),['K'|O],Ox) :- encodeInt(Ar,O,O1),encodeText(Nm,O1,Ox).
encodeT(type(Nm),['t'|O],Ox) :- encodeText(Nm,O,Ox).
encodeT(tpFun(Nm,Ar),['z'|O],Ox) :- encodeInt(Ar,O,O1),encodeText(Nm,O1,Ox).
encodeT(tpExp(T,Arg),['U'|O],Ox) :- deRef(T,Tp),encodeType(Tp,O,O1), encodeType(Arg,O1,Ox).
encodeT(refType(Tp),['r'|O],Ox) :- encodeType(Tp,O,Ox).
encodeT(funType(AT,Tp),['F'|O],Ox) :- encodeType(AT,O,O1), encodeType(Tp,O1,Ox).
encodeT(consType(Args,Tp),['C'|O],Ox) :- encodeType(Args,O,O1), encodeType(Tp,O1,Ox).
encodeT(contType(AT,Tp),['D'|O],Ox) :- encodeType(AT,O,O1), encodeType(Tp,O1,Ox).
encodeT(tplType(Args),['('|O],Ox) :- encodeTypes(Args,O,[')'|Ox]).
encodeT(faceType(Fields,Types),['I'|O],Ox) :- encodeFieldTypes(Fields,O,O1),encodeFieldTypes(Types,O1,Ox).
encodeT(allType(B,Tp),[':'|O],Ox) :- encodeType(B,O,O1),encodeType(Tp,O1,Ox).
encodeT(existType(B,Tp),['e'|O],Ox) :- encodeType(B,O,O1),encodeType(Tp,O1,Ox).
encodeT(constrained(Tp,Con),['|'|O],Ox) :- encodeType(Tp,O,O1),encodeConstraint(Con,O1,Ox).
encodeT(typeExists(L,R),['Y'|O],Ox) :- encodeType(L,O,O1), encodeType(R,O1,Ox).
encodeT(typeLambda(L,R),['y'|O],Ox) :- encodeType(L,O,O1), encodeType(R,O1,Ox).
encodeT(contractExists(L,R),['Z'|O],Ox) :- encodeConstraint(L,O,O1), encodeType(R,O1,Ox).

encodeTypes([],O,O).
encodeTypes([Tp|More],O,Ox) :- encodeType(Tp,O,O1), encodeTypes(More,O1,Ox).

encodeFieldTypes(Fields,['{'|O],Ox) :- encodeFieldTps(Fields,O,['}'|Ox]).

encodeFieldTps([],O,O).
encodeFieldTps([(Nm,Tp)|More],O,Ox) :- encodeText(Nm,O,O1),encodeType(Tp,O1,O2), encodeFieldTps(More,O2,Ox).

encodeConstraint(allType(V,C),[':'|O],Ox) :-
  encodeT(V,O,O1),
  encodeConstraint(C,O1,Ox).
encodeConstraint(constrained(Con,Extra),['|'|O],Ox) :-
  encodeConstraint(Con,O,O1),
  encodeConstraint(Extra,O1,Ox).
encodeConstraint(conTract(Nm,Args,Deps),['c'|O],Ox) :-
  encodeText(Nm,O,O1),
  encodeT(tplType(Args),O1,O2),
  encodeT(tplType(Deps),O2,Ox).
encodeConstraint(implementsFace(V,Face),['a'|O],Ox) :-
  encodeType(V,O,O1),
  encodeType(Face,O1,Ox).

encLtp(Tp,Sig) :-
  encodeLtipe(Tp,Chrs,[]),
  string_chars(Sig,Chrs).

encodeLtipe(i64Tipe,['i'|O],O).
encodeLtipe(f64Tipe,['f'|O],O).
encodeLtipe(blTipe,['l'|O],O).
encodeLtipe(ptrTipe,['p'|O],O).
encodeLtipe(fnTipe(As,R),['F'|O],Ox) :-
  encodeLtipe(tplTipe(As),O,O1),
  encodeLtipe(R,O1,Ox).
encodeLtipe(tplTipe(As),['('|O],Ox) :-
  rfold(As,encode:encodeLtipe,O,[')'|Ox]).


