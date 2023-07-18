star.json{
  import star.
  import star.parse.

  public json ::= .jTrue | .jFalse | .jNull |
    .jTxt(string) | .jColl(map[string,json]) | .jSeq(cons[json]) |
    .jNum(float).


  public implementation display[json] => {
    disp(j) => dispJson(j,0).
  }

  private dispJson:(json,integer) => string.
  dispJson(J,Sp) => case J in {
    .jTrue => "true".
    .jFalse => "false".
    .jNull => "null".
    .jTxt(T) => disp(T).
    .jNum(D) => disp(D).
    .jColl(M) => "{#(dispColl(M::cons[(string,json)],Sp+2,""))}".
    .jSeq(L) => "[#(interleave(dispSeq(L,Sp),", ")*)]".
  }

  dispColl:(cons[(string,json)],integer,string) => string.
  dispColl([],_,_) => "".
  dispColl([(f,e),..l],Sp,s) => "#(s)$(f)\:#(dispJson(e,Sp))#(dispColl(l,Sp,","))".

  dispSeq:(cons[json],integer) => cons[string].
  dispSeq([],_) => .nil.
  dispSeq([e,..l],Sp) => .cons(dispJson(e,Sp),dispSeq(l,Sp)).

  public implementation coercion[json,string] => {
    _coerce(J) => .some(disp(J)).
  }

  public implementation equality[json] => {
    T1 == T2 => equalJson(T1,T2).
  }

  equalJson:(json,json)=>boolean.
  equalJson(J1,J2) => case J1 in {
    .jTrue => .jTrue.=J2.
    .jFalse => .jFalse.=J2.
    .jNull => .jNull.=J2.
    .jTxt(S1) => .jTxt(S2).=J2 && S1==S2.
    .jNum(D1) => .jNum(D2).=J2 && D1==D2.
    .jColl(C1) => .jColl(C2).=J2 && C1==C2.
    .jSeq(L1) => .jSeq(L2).=J2 && {?(E1,E2) in zip(L1,L2) *> equalJson(E1,E2)?}.
  }

  public implementation coercion[string,json] => {
    _coerce(T) => parseJson(T).
  }

  public parseJson:(string)=>option[json].
  parseJson(T) where (J,_)?=pJ(skpBlnks(T::cons[char])) => .some(J).
  parseJson(_) default => .none.

  pJ:(cons[char]) => option[(json,cons[char])].
  pJ([Ch,..L]) => ppJ(Ch,L).

  ppJ:(char,cons[char]) => option[(json,cons[char])].
  ppJ(`t`,[`r`,`u`,`e`,..L]) => .some((.jTrue,L)).
  ppJ(`f`,[`a`,`a`,`l`,`s`,`e`,..L]) => .some((.jFalse,L)).
  ppJ(`n`,[`u`,`l`,`l`,..L]) => .some((.jNull,L)).
  ppJ(`-`,L) where (Dx,LL) .= psNum(L) => .some((.jNum(-Dx),LL)).
  ppJ(D,L) where isDigit(D) && (Dx,LL).=psNum([D,..L]) => .some((.jNum(Dx),LL)).
  ppJ(`\"`,L) where (Txt,LL) .= psStrng(L,[]) => .some((.jTxt(Txt),LL)).
  ppJ(`[`,L) => psSeq(skpBlnks(L)).
  ppJ(`{`,L) => psColl(skpBlnks(L)).

  skpBlnks:(cons[char]) => cons[char].
  skpBlnks([]) => [].
  skpBlnks([` `,..L]) => skpBlnks(L).
  skpBlnks([`\n`,..L]) => skpBlnks(L).
  skpBlnks([`\t`,..L]) => skpBlnks(L).
  skpBlnks(L) default => L.

  psNum:(cons[char]) => (float,cons[char]).
  psNum(L) where (First,R) .= psNat(L,0) &&
    (Val,Rest) .= psMoreNum(First::float,R) => (Val,Rest).

  psNat:(cons[char],integer) => (integer,cons[char]).
  psNat([Dx,..L],Sf) where isDigit(Dx) => psNat(L,Sf*10+digitVal(Dx)).
  psNat(L,Sf) default => (Sf,L).

  psDec:(cons[char]) => (integer,cons[char]).
  psDec([`-`,..L]) where (Ps,R) .= psNat(L,0) => (-Ps,R).
  psDec(L) => psNat(L,0).

  psFrac:(float,float,cons[char]) => (float,cons[char]).
  psFrac(Scale,Fr,[D,..L]) where isDigit(D) => psFrac(Scale*0.1,digitVal(D)::float*Scale+Fr,L).
  psFrac(_,Fr,L) default => (Fr,L).

  psMoreNum:(float,cons[char]) => (float,cons[char]).
  psMoreNum(F,[`.`,..L]) where (Mn,LL).=psFrac(0.1,F,L) => psExp(Mn,LL).
  psMoreNum(F,L) default => (F,L).

  psExp:(float,cons[char]) => (float,cons[char]).
  psExp(Mn,[`e`,..L]) where (Exp,R).= psDec(L) => (Mn*(10.0**Exp::float),R).

  psString:(cons[char]) => (string,cons[char]).
  psString(L) where [`\"`,..LL].=skpBlnks(L) => psStrng(LL,[]).

  psStrng:(cons[char],cons[char]) => (string,cons[char]).
  psStrng([`\\`,..L],SoF) where (Ch,LL).=psChrRef(L) => psStrng(LL,[Ch,..SoF]).
  psStrng([`\"`,..L],SoF) => (reverse(SoF)::string,L).
  psStrng([Ch,..L],SoF) => psStrng(L,[Ch,..SoF]).

  psChrRef:(cons[char]) => (char,cons[char]).
  psChrRef([`u`,..L]) => psHex(L,0,4).
  psChrRef([`b`,..L]) => (`\b`,L).
  psChrRef([`f`,..L]) => (`\f`,L).
  psChrRef([`n`,..L]) => (`\n`,L).
  psChrRef([`t`,..L]) => (`\t`,L).
  psChrRef([`r`,..L]) => (`\r`,L).
  psChrRef([Ch,..L]) => (Ch,L).

  psHex:(cons[char],integer,integer) => (char,cons[char]).
  psHex(L,So,0) => (So::char,L).
  psHex([H,..L],So,Cn) where Hx?=isHexDigit(H) && Cn>0 => psHex(L,So*16+Hx,Cn-1).
  psHex(L,So,_) default => (So::char,L).

  psSeq:(cons[char]) => option[(json,cons[char])].
  psSeq([`]`,..L]) => .some((.jSeq([]),L)).
  psSeq(L) where (El,LL) ?= pJ(L) => psMoreSeq(skpBlnks(LL),[El]).

  psMoreSeq([`]`,..L],SoF) => .some((.jSeq(reverse(SoF)),L)).
  psMoreSeq([`,`,..L],SoF) where (El,LL) ?= pJ(skpBlnks(L)) =>
    psMoreSeq(skpBlnks(LL),[El,..SoF]).

  psColl:(cons[char]) => option[(json,cons[char])].
  psColl([`}`,..L]) => .some((.jColl([]),L)).
  psColl(L) where (Ky,El,LL).=psEntry(L) => psMoreCol(skpBlnks(LL),[Ky->El]).

  psMoreCol([`}`,..L],SoF) => .some((.jColl(SoF),L)).
  psMoreCol([`,`,..L],SoF) where (Ky,El,LL).=psEntry(L) => psMoreCol(skpBlnks(LL),SoF[Ky->El]).

  psEntry(L) where (Ky,L1) .= psString(skpBlnks(L)) &&
      [`:`,..L2].=skpBlnks(L1) &&
      (Vl,LL) ?= pJ(skpBlnks(L2)) => (Ky,Vl,LL).
}
