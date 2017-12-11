:- module(encode,[encodeTerm/3,encodeType/3,encodeConstraint/3]).

:- use_module(types).
:- use_module(misc).
:- use_module(base64).

encodeTerm(anon,['a'|O],O).
encodeTerm(intgr(Ix),['x'|O],Ox) :- encodeInt(Ix,O,Ox).
encodeTerm(float(Dx),['d'|O],Ox) :- encodeFloat(Dx,O,Ox).
encodeTerm(enum(Nm),['e'|O],Ox) :- encodeText(Nm,O,Ox).
encodeTerm(strg(St),['s'|O],Ox) :- encodeText(St,O,Ox).
encodeTerm(strct(Nm,Arity),['o'|O],Ox) :-
  encodeInt(Arity,O,O1),
  encodeText(Nm,O1,Ox).
encodeTerm(prg(Nm,Arity),['p'|O],Ox) :- encodeInt(Arity,O,O1), encodeText(Nm,O1,Ox).
encodeTerm(tpl(Els),O,Ox) :- tupleSig(Els,Con), encodeTerm(cons(Con,Els),O,Ox).
encodeTerm(cons(Con,Els),['n'|O],Ox) :-
  length(Els,Ln),
  encodeInt(Ln,O,O1),
  encodeTerm(Con,O1,O2),
  encodeTerms(Els,O2,Ox).
encodeTerm(code(Tp,Bytes,Lits),['#'|O],Ox) :-
  encodeType(Tp,O,O1),
  encodeTerm(Lits,O1,O2),
  encode64(Bytes,Chrs,[]),
  encodeChars(Chrs,'''',O2,Ox).

tupleSig(Els,strct(Con,Ar)) :- length(Els,Ar), number_string(Ar,A),string_concat("()",A,Con).

encodeTerms([],O,O).
encodeTerms([T|R],O,Ox) :-
  encodeTerm(T,O,O1),
  encodeTerms(R,O1,Ox).

encodeInt(N,['-'|O],Ox) :- N<0, N1 is -N, encodeInt(N1,O,Ox).
encodeInt(K,[D|O],O) :- K>=0 , K=<10, digit(K,D).
encodeInt(N,O,Ox) :- N1 is N div 10, encodeInt(N1,O,[D|Ox]), K is N mod 10, digit(K,D).

encType(Tp,Sig) :-
  encodeType(Tp,O,[]),
  string_chars(Sig,O).

encodeType(anonType,['_'|O],O).
encodeType(voidType,['v'|O],O).
encodeType(thisType,['h'|O],O).
encodeType(typeExp(Tp,[T]),['L'|O],Ox) :- deRef(Tp,tpFun("star.core*list",1)),!,encodeType(T,O,Ox).
encodeType(type("star.core*boolean"),['l'|O],O).
encodeType(type("star.core*integer"),['i'|O],O).
encodeType(type("star.core*float"),['f'|O],O).
encodeType(type("star.core*string"),['S'|O],O).
encodeType(kVar(Nm),['k'|O],Ox) :- encodeText(Nm,O,Ox).
encodeType(kFun(Nm,Ar),['K'|O],Ox) :- encodeInt(Ar,O,O1),encodeText(Nm,O1,Ox).
encodeType(type(Nm),['t'|O],Ox) :- encodeText(Nm,O,Ox).
encodeType(tpFun(Nm,Ar),['z'|O],Ox) :- encodeInt(Ar,O,O1),encodeText(Nm,O1,Ox).
encodeType(typeExp(T,Args),['U'|O],Ox) :- deRef(T,Tp),encodeType(Tp,O,O1), encodeTypes(Args,O1,Ox).
encodeType(funType(AT,Tp),['F'|O],Ox) :- encodeType(AT,O,O1), encodeType(Tp,O1,Ox).
encodeType(ptnType(AT,Tp),['p'|O],Ox) :- encodeType(AT,O,O1), encodeType(Tp,O1,Ox).
encodeType(consType(Args,Tp),['C'|O],Ox) :- encodeType(Args,O,O1), encodeType(Tp,O1,Ox).
encodeType(tupleType(Args),['T'|O],Ox) :- encodeTypes(Args,O,Ox).
encodeType(faceType(Fields,Types),['I'|O],Ox) :- encodeFieldTypes(Fields,O,O1),encodeFieldTypes(Types,O1,Ox).
encodeType(allType(B,Tp),[':'|O],Ox) :- encodeType(B,O,O1),encodeType(Tp,O1,Ox).
encodeType(existType(B,Tp),['e'|O],Ox) :- encodeType(B,O,O1),encodeType(Tp,O1,Ox).
encodeType(constrained(Tp,Con),['|'|O],Ox) :- encodeType(Tp,O,O1),encodeConstraint(Con,O1,Ox).
encodeType(typeExists(L,R),['Y'|O],Ox) :- encodeType(L,O,O1), encodeType(R,O1,Ox).
encodeType(contractExists(L,R),['Z'|O],Ox) :- encodeConstraint(L,O,O1), encodeType(R,O1,Ox).

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
encodeTps([T|More],O,Ox) :- deRef(T,Tp),encodeType(Tp,O,O1), encodeTps(More,O1,Ox).

encodeFieldTypes(Fields,['{'|O],Ox) :- encodeFieldTps(Fields,O,['}'|Ox]).

encodeFieldTps([],O,O).
encodeFieldTps([(Nm,T)|More],O,Ox) :- encodeText(Nm,O,O1),deRef(T,Tp),encodeType(Tp,O1,O2), encodeFieldTps(More,O2,Ox).

encodeConstraint(allType(V,C),[':'|O],Ox) :-
  deRef(V,VV),
  encodeType(VV,O,O1),
  encodeConstraint(C,O1,Ox).
encodeConstraint(constrained(Con,Extra),['|'|O],Ox) :-
  encodeConstraint(Con,O,O1),
  encodeConstraint(Extra,O1,Ox).
encodeConstraint(conTract(Nm,Args,Deps),['c'|O],Ox) :-
  encodeText(Nm,O,O1),
  encodeType(tupleType(Args),O1,O2),
  encodeType(tupleType(Deps),O2,Ox).
encodeConstraint(implementsFace(V,Face),['a'|O],Ox) :-
  deRef(V,VV),
  encodeType(VV,O,O1),
  encodeType(Face,O1,Ox).
