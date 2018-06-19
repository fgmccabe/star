:- module(json, [parseJson//1, string2json/2, dispJson/3]).

:- use_module(misc).

string2json(S,J) :- string_chars(S,C), phrase(parseJson(J),C).

parseJson(J) --> skipBlanks(), jP(J), skipBlanks().

jP(jTrue) --> ['t','r','u','e'].
jP(jFalse) --> ['f','a','l','s','e'].
jP(jNull) --> ['n','u','l','l'].
jP(jNum(N)) --> ['-'], jNumber(NN), { N is -NN}.
jP(jNum(N)) --> jNumber(N).
jP(jTxt(S)) --> jString(S).
jP(jSeq(L)) --> ['['], skipBlanks(), parseSeq(L), skipBlanks(), [']'].
jP(jColl(M)) -->['{'], skipBlanks(), parseColl(M), skipBlanks(), ['}'].

jNumber(Flt) --> digit(D), readNatural(D,First), readMoreNumber(First,Flt).

readDecimal(D) --> ['-'], readNatural(0,N), { D is -N}.
readDecimal(D) --> readNatural(0,D).

readNatural(So,N) --> digit(D), { So1 is So*10+D }, readNatural(So1,N).
readNatural(So,So) --> [].

readMoreNumber(First,Flt) --> ['.'], [C], {digitVal(C,D), F is D/10},
    fraction(0.01,F,Fr),
    (( ['e'] | ['E'] ) -> readDecimal(Exp), { Flt is (First+Fr)*(10**Exp)} |
     { Flt is First+Fr }).
readMoreNumber(Flt,Flt) --> [].

fraction(Scal,Fr,Rslt) --> digit(D), { NScale is Scal/10, So is D*Scal+Fr},
    fraction(NScale,So,Rslt).
fraction(_,Fr,Fr) --> [].

jString(S) --> ['\"'], readStr([],S), ['\"'].

readStr(So,Txt) --> ['\\','u'], readHex(0,H,4), readStr([H|So],Txt).
readStr(So,Txt) --> ['\\','b'], readStr(['\b'|So],Txt).
readStr(So,Txt) --> ['\\','f'], readStr(['\f'|So],Txt).
readStr(So,Txt) --> ['\\','n'], readStr(['\n'|So],Txt).
readStr(So,Txt) --> ['\\','t'], readStr(['\t'|So],Txt).
readStr(So,Txt) --> ['\\','r'], readStr(['\r'|So],Txt).
readStr(So,Txt) --> ['\\'], [B], readStr([B|So],Txt).
readStr(So,Txt) --> [C], { C\='\"'}, readStr([C|So],Txt).
readStr(So,Txt) --> [], { reverse(So,T),string_chars(Txt,T)}.

readHex(So,H,0) --> [], { char_code(H,So) }.
readHex(So,H,Cn) --> {Cn>0}, hexDigit(D), {Nx is So*16+D, Cn1 is Cn-1}, readHex(Nx,H,Cn1).
readHex(So,H,_) --> [], { char_code(H,So) }.

parseSeq([J|L]) --> jP(J), skipBlanks(), parseMoreSeq(L).
parseSeq([]) --> [].

parseMoreSeq([J|L]) --> [','], skipBlanks(), jP(J), skipBlanks(), parseMoreSeq(L).
parseMoreSeq([]) --> [].

parseColl([(K,V)|L]) --> jString(K), skipBlanks(), [':'], skipBlanks(), jP(V), skipBlanks(),parseMoreColl(L).
parseColl([]) --> [].

parseMoreColl([(K,V)|L]) --> [','], skipBlanks(), jString(K), skipBlanks(), [':'], skipBlanks(), jP(V), skipBlanks(),parseMoreColl(L).
parseMoreColl([]) --> [].

skipBlanks() --> [' '], skipBlanks().
skipBlanks() --> ['\n'], skipBlanks().
skipBlanks() --> ['\t'], skipBlanks().
skipBlanks() --> ['\r'], skipBlanks().
skipBlanks() --> ['/','*'], blockComment(), skipBlanks().
skipBlanks() --> [].

blockComment() --> ['*','/'].
blockComment() --> [_], blockComment().

digit(D) --> [C], {digitVal(C,D)}.

digitVal('0',0).
digitVal('1',1).
digitVal('2',2).
digitVal('3',3).
digitVal('4',4).
digitVal('5',5).
digitVal('6',6).
digitVal('7',7).
digitVal('8',8).
digitVal('9',9).

hexDigit(H) --> [C], {digitVal(C,H)}.
hexDigit(10) --> ['a'].
hexDigit(11) --> ['b'].
hexDigit(12) --> ['c'].
hexDigit(13) --> ['d'].
hexDigit(14) --> ['e'].
hexDigit(15) --> ['f'].
hexDigit(10) --> ['A'].
hexDigit(11) --> ['B'].
hexDigit(12) --> ['C'].
hexDigit(13) --> ['D'].
hexDigit(14) --> ['E'].
hexDigit(15) --> ['F'].

dispJson(J,O,Ox) :-
  dspJsn(J,0,O,Ox).

dspJsn(jTrue,_,O,Ox) :- appStr("true",O,Ox).
dspJsn(jFalse,_,O,Ox) :- appStr("false",O,Ox).
dspJsn(jNull,_,O,Ox) :- appStr("null",O,Ox).
dspJsn(jNum(D),_,O,Ox) :- appFlt(D,O,Ox).
dspJsn(jTxt(S),_,O,Ox) :- appQuoted(S,'"',O,Ox).
dspJsn(jSeq(L),Sp,['['|O],Ox) :- dispJSeq(L,Sp,O,[']'|Ox]).
dspJsn(jColl(L),Sp,['{'|O],Ox) :- Sp2 is Sp+2,dispJColl(L,Sp2,O,['}'|Ox]).

dispJSeq([],_,O,O).
dispJSeq([E|L],Sp,O,Ox) :- dspJsn(E,Sp,O,O1), dispMoreSeq(L,Sp,O1,Ox).

dispMoreSeq([],_,O,O).
dispMoreSeq([E|L],Sp,[','|O],Ox) :- dspJsn(E,Sp,O,O1), dispMoreSeq(L,Sp,O1,Ox).

dispJColl([],_,O,O).
dispJColl([(K,V)|L],Sp,O,Ox) :- appQuoted(K,'"',O,[':'|O2]), dspJsn(V,Sp,O2,O3), dispMoreCol(L,Sp,O3,Ox).

dispMoreCol([],_,O,O).
dispMoreCol(L,Sp,[','|O],Ox) :- nl(Sp,O,O0),spaces(Sp,O0,O1), dispJColl(L,Sp,O1,Ox).

nl(0,O,O).
nl(K,['\n'|L],L) :- K>0.

spaces(0,O,O).
spaces(N,[' '|O],Ox) :- N>0, N1 is N-1, spaces(N1,O,Ox).
