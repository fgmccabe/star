:- module(decode,[decodeTerm//1, decodeValue/2, decodeType//1, decodeConstraint//1, decodeConstraint/2,decodeSignature/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(base64).

% Decode a sequence of characters into various entites

/*
 Decided by a leading 'flag' byte which encodes a type indicator:
 'a': Unbound variable
 'x': Integer
 'd': Floating point
 'e': Identifier
 's': String
 'o': Constructor specifier
 'p': Program specifier
 'n': Term
 '#': Code
*/

decodeValue(Txt,Val) :-
  string_chars(Txt,Chrs),
  phrase(decodeTerm(Val),Chrs).

decodeTerm(anon) --> ['a'].
decodeTerm(intgr(Ix)) --> ['x'], decInt(Ix).
decodeTerm(float(Dx)) --> ['d'], decFloat(Dx).
decodeTerm(enum(Nm)) --> ['e'], decodeText(Nm).
decodeTerm(strg(Txt)) --> ['s'], decodeText(Txt).
decodeTerm(strct(Nm,Ar)) --> ['o'], decInt(Ar), decodeText(Nm).
decodeTerm(prg(Nm,Ar)) --> ['p'], decInt(Ar), decodeText(Nm).
decodeTerm(Term) --> ['n'], decInt(Len), decodeTerm(Con), decTerms(Len,Els),
   { isTupleSig(Con) -> Term = tpl(Els) | Term = cons(Con,Els)}.


isTupleSig(strct(Nm,Ar)) :- string_concat("()",A,Nm),number_string(Ar,A).

decTerms(0,[]) --> [].
decTerms(Count,[D|M]) --> { Count>0}, decodeTerm(D), { C is Count-1}, decTerms(C,M).

decInt(Ix) --> ['-'], digits(0,N), Ix is -N.
decInt(Ix) --> digits(0,Ix).

decodeText(Txt) --> [C], collectQuoted(C,Chrs),{ string_chars(Txt,Chrs)}.

collectQuoted(C,[]) --> [C].
collectQuoted(C,[Ch|M]) --> ['\\', Ch], collectQuoted(C,M).
collectQuoted(C,[Ch|M]) --> [Ch], collectQuoted(C,M).

decodeSignature(S,Tp) :-
  string_chars(S,Chrs),
  phrase(decodeType(Tp),Chrs).

decodeType(anonType) --> ['_'].
decodeType(voidType) --> ['v'].
decodeType(thisType) --> ['h'].
decodeType(type("star.core*integer")) --> ['i'].
decodeType(type("star.core*float")) --> ['f'].
decodeType(type("star.core*string")) --> ['S'].
decodeType(type("star.core*logical")) --> ['l'].
decodeType(kVar(Nm)) --> ['k'], decodeText(Nm).
decodeType(kFun(Nm,Ar)) --> ['K'], typeLen(Ar), decodeText(Nm).
decodeType(type(Nm)) --> ['t'], decodeText(Nm).
decodeType(tpFun(Nm,Ar)) --> ['z'], typeLen(Ar), decodeText(Nm).
decodeType(typeExp(tpFun("star.core*list",1),[ElTp])) --> ['L'], decodeType(ElTp).
decodeType(typeExp(Op,ArgTypes)) --> ['U'], decodeType(Op), decodeTypes(ArgTypes).
decodeType(allType(TV,Tp)) --> [':'], decodeType(TV), decodeType(Tp).
decodeType(constrained(Tp,Con)) --> ['|'], decodeType(Tp), decodeConstraint(Con).
decodeType(faceType(Fields,Tps)) --> ['I'], decodeFields(Fields), decodeFields(Tps).
decodeType(funType(A,T)) --> ['F'], decodeType(A), decodeType(T).
decodeType(ptnType(A,T)) --> ['p'], decodeArgTypes(A), decodeType(T).
decodeType(consType(A,T)) --> ['C'], decodeType(A), decodeType(T).
decodeType(tupleType(Tps)) --> ['T'], decodeTypes(Tps).
decodeType(typeExists(L,R)) --> ['Y'], decodeType(L), decodeType(R).


typeLen(Len) --> digits(0,Len).

decodeTypes(0,[]) --> [].
decodeTypes(Ln,[A|More]) --> { Ln > 0 }, decodeType(A), {L1 is Ln-1}, decodeTypes(L1,More).
decodeTypes(Types) --> typeLen(Len), decodeTypes(Len,Types).

decodeArgTypes(0,[]) --> [].
decodeArgTypes(Ln,[A|More]) --> { Ln > 0 }, decodeMode(_), decodeType(A), {L1 is Ln-1}, decodeArgTypes(L1,More).
decodeArgTypes(Types) --> typeLen(Len), decodeArgTypes(Len,Types).

decodeFields(0,[]) --> [].
decodeFields(Ln,[(Nm,Tp)|More]) --> { Ln > 0 }, decodeText(Nm), decodeType(Tp), {L1 is Ln-1}, decodeFields(L1,More).
decodeFields(Fields) --> typeLen(Len), decodeFields(Len,Fields).

decodeMode(inMode) --> ['+'].
decodeMode(outMode) --> ['-'].
decodeMode(biMode) --> ['?'].
decodeMode(biMode) --> [].

decodeConstraint(S,Con) :-
  string_chars(S,Chrs),
  phrase(decodeConstraint(Con),Chrs).

decodeConstraint(constrained(Con,Extra)) --> ['|'], decodeConstraint(Con), decodeConstraint(Extra).
decodeConstraint(conTract(Nm,Args,Deps)) --> ['c'], decodeText(Nm), decodeType(tupleType(Args)), decodeType(tupleType(Deps)).
decodeConstraint(implementsFace(Tp,Face)) --> ['a'], decodeType(Tp), decodeType(faceType(Face,[])).
decodeConstraint(allType(TV,Con)) --> [':'], decodeType(TV), decodeConstraint(Con).

collectUntil(C,[]) --> [C].
collectUntil(C,[B|More]) --> [B], collectUntil(C,More).

digits(SoFar,Ix) --> digit(D), { Nx is SoFar*10+D}, digits(Nx,Ix).
digits(Ix,Ix) --> \+ digit(_).

digit(0) --> ['0'].
digit(1) --> ['1'].
digit(2) --> ['2'].
digit(3) --> ['3'].
digit(4) --> ['4'].
digit(5) --> ['5'].
digit(6) --> ['6'].
digit(7) --> ['7'].
digit(8) --> ['8'].
digit(9) --> ['9'].
