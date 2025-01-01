:- module(decode,[decodeTerm//1, decodeValue/2, decodeType//1, decodeConstraint//1, decodeConstraint/2,decodeSignature/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(base64).

% Decode a sequence of characters into various entites

/*
 Decided by a leading 'flag' byte which encodes a type indicator:
 'x': Integer
 'b': Big integer
 'd': Floating point
 'e': Enum symbol
 's': String
 'o': Constructor label
 'n': Term
 'l': List
*/

decodeData(Txt,Val) :-
  string_chars(Txt,Chrs),
  phrase(decodeDta(Val),Chrs).

decodeDta(intgr(Nm)) --> ['x'], decInt(Nm).
decodeDta(bigx(Ix)) --> ['b'], tdigits(Ix).
decodeDta(float(Dx)) --> ['d'], decFloat(Dx).
decodeDta(ctpl(Nm,[])) --> ['e'], decodeText(Nm).
decodeDta(chr(Cp)) --> ['c'], decodeChar(Cp).
decodeDta(strg(Txt)) --> ['s'], decodeText(Txt).
decodeDta(lbl(Nm,Ar)) --> ['o'], decInt(Ar), decodeText(Nm).
decodeDta(ctpl(Con,Els)) --> ['n'], decInt(Len), decodeDta(Con), decTrms(Len,Els).
decodeDta(clos(Nm,Ar,Free,Tp)) --> ['p'], decodeDta(lbl(Nm,Ar)), decodeDta(Free), decodeSig(Tp).
decodeDta(lst(Els)) --> ['l'], decInt(Len), decTerms(Len,Els).

decTrms(0,[]) --> [].
decTrms(Count,[D|M]) --> { Count>0}, decodeDta(D), { C is Count-1}, decTrms(C,M).

decodeValue(Txt,Val) :-
  string_chars(Txt,Chrs),
  phrase(decodeTerm(Val),Chrs).

decodeTerm(voyd) --> ['v'].
decodeTerm(intgr(Nm)) --> ['x'], decInt(Nm).
decodeTerm(bigx(Ix)) --> ['b'], tdigits(Ix).
decodeTerm(float(Dx)) --> ['d'], decFloat(Dx).
decodeTerm(enum(Nm)) --> ['e'], decodeText(Nm).
decodeTerm(chr(Cp)) --> ['c'], decodeChar(Cp).
decodeTerm(strg(Txt)) --> ['s'], decodeText(Txt).
decodeTerm(lbl(Nm,Ar)) --> ['o'], decInt(Ar), decodeText(Nm).
decodeTerm(ctpl(Con,Els)) --> ['n'], decInt(Len), decodeTerm(Con), decTerms(Len,Els).
decodeTerm(clos(Nm,Ar,Free,Tp)) --> ['p'], decodeTerm(lbl(Nm,Ar)), decodeTerm(Free), decodeSig(Tp).
decodeTerm(lst(Els)) --> ['l'], decInt(Len), decTerms(Len,Els).

decTerms(0,[]) --> [].
decTerms(Count,[D|M]) --> { Count>0}, decodeTerm(D), { C is Count-1}, decTerms(C,M).

decInt(Ix) --> ['-'], digits(0,N), {Ix is (-N)}.
decInt(Ix) --> digits(0,Ix).

decFloat(Dx) --> decodeText(Txt), { number_string(Dx,Txt)}.

decodeText(Txt) --> [C], collectQuoted(C,Chrs),{ string_chars(Txt,Chrs)}.

collectQuoted(C,[]) --> [C].
collectQuoted(C,[Ch|M]) --> ['\\', Ch], collectQuoted(C,M).
collectQuoted(C,[Ch|M]) --> [Ch], collectQuoted(C,M).

decodeChar(Ch) --> ['\\', Ch],!.
decodeChar(Ch) --> [Ch].

decodeSig(Tp) --> decodeTerm(strg(Txt)), { decodeSignature(Txt,Tp)}.

decodeSignature(S,Tp) :-
  string_chars(S,Chrs),
  phrase(decodeType(Tp),Chrs).

decodeType(anonType) --> ['_'].
decodeType(voidType) --> ['v'].
decodeType(type("integer")) --> ['i'].
decodeType(type("bigint")) --> ['b'].
decodeType(type("float")) --> ['f'].
decodeType(type("char")) --> ['c'].
decodeType(type("string")) --> ['s'].
decodeType(type("boolean")) --> ['l'].
decodeType(kVar(Nm)) --> ['k'], decodeText(Nm).
decodeType(kFun(Nm,Ar)) --> ['K'], typeLen(Ar), decodeText(Nm).
decodeType(type(Nm)) --> ['t'], decodeText(Nm).
decodeType(tpFun(Nm,Ar)) --> ['z'], typeLen(Ar), decodeText(Nm).
decodeType(tpExp(tpFun("ref",1),ArgType)) --> ['r'],decodeType(ArgType).
decodeType(tpExp(Op,ArgType)) --> ['U'], decodeType(Op), decodeType(ArgType).
decodeType(allType(TV,Tp)) --> [':'], decodeType(TV), decodeType(Tp).
decodeType(existType(TV,Tp)) --> ['e'], decodeType(TV), decodeType(Tp).
decodeType(constrained(Tp,Con)) --> ['|'], decodeType(Tp), decodeConstraint(Con).
decodeType(faceType(Fields,Tps)) --> ['I'], decodeFields(Fields), decodeFields(Tps).
decodeType(funType(A,T)) --> ['F'], decodeType(A), decodeType(T).
decodeType(consType(A,T)) --> ['C'], decodeType(A), decodeType(T).
decodeType(tplType(Tps)) --> decodeTypes(Tps).
decodeType(typeExists(L,R)) --> ['Y'], decodeType(L), decodeType(R).
decodeType(typeLambda(L,R)) --> ['y'], decodeType(L), decodeType(R).
decodeType(contractExists(L,R)) --> ['Z'], decodeConstraint(L), decodeType(R).

typeLen(Len) --> digits(0,Len).

decodeTypes(Tps) --> ['('], decodeTps(Tps).

decodeTps([]) --> [')'].
decodeTps([A|More]) --> decodeType(A), decodeTps(More).

decodeFields(Flds) --> ['{'], decodeFlds(Flds).

decodeFlds([]) --> ['}'].
decodeFlds([(Nm,Tp)|More]) --> decodeText(Nm), decodeType(Tp),decodeFlds(More).

decodeConstraint(S,Con) :-
  string_chars(S,Chrs),
  phrase(decodeConstraint(Con),Chrs).

decodeConstraint(constrained(Con,Extra)) --> ['|'], decodeConstraint(Con), decodeConstraint(Extra).
decodeConstraint(conTract(Nm,Args,Deps)) --> ['c'], decodeText(Nm), decodeType(tplType(Args)), decodeType(tplType(Deps)).
decodeConstraint(implicit(Nm,Tp)) --> ['d'], decodeText(Nm), decodeType(Tp).
decodeConstraint(raises(Tp)) --> ['r'], decodeType(Tp).
decodeConstraint(implementsFace(Tp,Face)) --> ['a'], decodeType(Tp), decodeType(faceType(Face,[])).
decodeConstraint(allType(TV,Con)) --> [':'], decodeType(TV), decodeConstraint(Con).

digits(SoFar,Ix) --> digit(D), { Nx is SoFar*10+D}, digits(Nx,Ix).
digits(Ix,Ix) --> \+ digit(_).

tdigits([D|Nm]) --> digit(D), tdigits(Nm).
tdigits([]) --> \+ digit(_).

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
