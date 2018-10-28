:- module(encode,[encode/2,encodeTerm/3,encType/2,encodeType/3,encodeConstraint/3]).

:- use_module(types).
:- use_module(misc).
:- use_module(base64).

encode(T,Str) :-
  encodeTerm(T,Chrs,[]),
  string_chars(Str,Chrs).

encodeTerm(intgr(Ix),['x'|O],Ox) :- encodeInt(Ix,O,Ox).
encodeTerm(float(Dx),['d'|O],Ox) :- encodeFloat(Dx,O,Ox).
encodeTerm(enum(Nm),['e'|O],Ox) :- encodeText(Nm,O,Ox).
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

encType(Tp,Sig) :-
  encodeTp(Tp,O,[]),
  string_chars(Sig,O).

encodeTp(T,C,Cx) :-
  deRef(T,Tp),
  encodeType(Tp,C,Cx).

encodeType(anonType,['_'|O],O).
encodeType(voidType,['v'|O],O).
encodeType(thisType,['h'|O],O).
encodeType(T,['_'|O],O) :- isUnbound(T).
encodeType(tpExp(Tp,T),['L'|O],Ox) :- deRef(Tp,tpFun("star.core*list",1)),!,encodeTp(T,O,Ox).
encodeType(type("star.core*boolean"),['l'|O],O).
encodeType(type("star.core*integer"),['i'|O],O).
encodeType(type("star.core*float"),['f'|O],O).
encodeType(type("star.core*string"),['S'|O],O).
encodeType(kVar(Nm),['k'|O],Ox) :- encodeText(Nm,O,Ox).
encodeType(kFun(Nm,Ar),['K'|O],Ox) :- encodeInt(Ar,O,O1),encodeText(Nm,O1,Ox).
encodeType(type(Nm),['t'|O],Ox) :- encodeText(Nm,O,Ox).
encodeType(tpFun(Nm,Ar),['z'|O],Ox) :- encodeInt(Ar,O,O1),encodeText(Nm,O1,Ox).
encodeType(tpExp(T,Arg),['U'|O],Ox) :- deRef(T,Tp),encodeTp(Tp,O,O1), encodeTp(Arg,O1,Ox).
encodeType(funType(AT,Tp),['F'|O],Ox) :- encodeTp(AT,O,O1), encodeTp(Tp,O1,Ox).
encodeType(consType(Args,Tp),['C'|O],Ox) :- encodeTp(Args,O,O1), encodeTp(Tp,O1,Ox).
encodeType(tupleType(Args),O,Ox) :- encodeTypes(Args,O,Ox).
encodeType(faceType(Fields,Types),['I'|O],Ox) :- encodeFieldTypes(Fields,O,O1),encodeFieldTypes(Types,O1,Ox).
encodeType(allType(B,Tp),[':'|O],Ox) :- encodeTp(B,O,O1),encodeTp(Tp,O1,Ox).
encodeType(existType(B,Tp),['e'|O],Ox) :- encodeTp(B,O,O1),encodeTp(Tp,O1,Ox).
encodeType(constrained(Tp,Con),['|'|O],Ox) :- encodeTp(Tp,O,O1),encodeConstraint(Con,O1,Ox).
encodeType(typeExists(L,R),['Y'|O],Ox) :- encodeTp(L,O,O1), encodeTp(R,O1,Ox).
encodeType(typeLambda(L,R),['y'|O],Ox) :- encodeTp(L,O,O1), encodeTp(R,O1,Ox).
encodeType(contractExists(L,R),['Z'|O],Ox) :- encodeConstraint(L,O,O1), encodeTp(R,O1,Ox).

findDelim(Chrs,Delim) :-
  is_member(Delim,['''','"', '|', '/', '%']),
  \+ is_member(Delim,Chrs),!.
findDelim(_,'"').

encodeChars(Chars,Delim,[Delim|O],Ox) :-
 encodeQuoted(Chars,Delim,O,Ox).

encodeText(Txt,[Delim|O],Ox) :- string_chars(Txt,Chrs), findDelim(Chrs,Delim), encodeQuoted(Chrs,Delim,O,Ox).

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

encodeTypes(Tps,['('|O],Ox) :- encodeTps(Tps,O,[')'|Ox]).

encodeTps([],O,O).
encodeTps([Tp|More],O,Ox) :- encodeTp(Tp,O,O1), encodeTps(More,O1,Ox).

encodeFieldTypes(Fields,['{'|O],Ox) :- encodeFieldTps(Fields,O,['}'|Ox]).

encodeFieldTps([],O,O).
encodeFieldTps([(Nm,Tp)|More],O,Ox) :- encodeText(Nm,O,O1),encodeTp(Tp,O1,O2), encodeFieldTps(More,O2,Ox).

encodeConstraint(allType(V,C),[':'|O],Ox) :-
  encodeType(V,O,O1),
  encodeConstraint(C,O1,Ox).
encodeConstraint(constrained(Con,Extra),['|'|O],Ox) :-
  encodeConstraint(Con,O,O1),
  encodeConstraint(Extra,O1,Ox).
encodeConstraint(conTract(Nm,Args,Deps),['c'|O],Ox) :-
  encodeText(Nm,O,O1),
  encodeType(tupleType(Args),O1,O2),
  encodeType(tupleType(Deps),O2,Ox).
encodeConstraint(implementsFace(V,Face),['a'|O],Ox) :-
  encodeTp(V,O,O1),
  encodeTp(Face,O1,Ox).
