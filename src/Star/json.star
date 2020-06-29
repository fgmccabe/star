star.json{
  import star.
  import star.parse.

  public json ::=
    .jTrue | .jFalse | .jNull |
    jTxt(string) | jColl(map[string,json]) | jSeq(cons[json]) |
    jNum(float).


  public implementation display[json] => {
    disp(j) => dispJson(j,0).
  }

  private dispJson:(json,integer) => ss.
  dispJson(.jTrue,_) => ss("true").
  dispJson(.jFalse,_) => ss("false").
  dispJson(.jNull,_) => ss("null").
  dispJson(jTxt(T),_) => disp(T).
  dispJson(jNum(D),_) => disp(D).
  dispJson(jColl(M),Sp) => ssSeq([ss("{"),ssSeq(dispColl(M::cons[keyval[string,json]],Sp+2,"")),ss("}")]).
  dispJson(jSeq(L),Sp) => ssSeq([ss("["),ssSeq(dispSeq(L,Sp,"")),ss("]")]).

  dispColl:(cons[keyval[string,json]],integer,string) => cons[ss].
  dispColl([],_,_) => [].
  dispColl([f->e,..l],Sp,s) => [ss(s),break(Sp),disp(f),ss(":"),dispJson(e,Sp),..dispColl(l,Sp,",")].

  dispSeq:(cons[json],integer,string) => cons[ss].
  dispSeq([],_,_) => [].
  dispSeq([e,..l],Sp,s) => [ss(s),dispJson(e,Sp),..dispSeq(l,Sp,",")].

  public implementation coercion[json,string] => {.
    _coerce(J) => disp(J):?string.
  .}

  break:(integer) => ss.
  break(0) => ssSeq([]).
  break(X) => let{
    spces:(integer) => cons[ss].
    spces(0) => [].
    spces(C) => [ss(" "),..spces(C-1)].
  } in ssSeq([ss("\n"),..spces(X)]).

  public implementation equality[json] => {.
    T1 == T2 => equalJson(T1,T2).
  .}

  equalJson:(json,json)=>boolean.
  equalJson(.jTrue,.jTrue) => .true.
  equalJson(.jFalse,.jFalse) => .true.
  equalJson(.jNull,.jNull) => .true.
  equalJson(jTxt(S1),jTxt(S2)) => S1==S2.
  equalJson(jNum(D1),jNum(D2)) => D1==D2.
  equalJson(jColl(C1),jColl(C2)) => C1==C2.
  equalJson(jSeq(L1),jSeq(L2)) => (E1,E2) in zip(L1,L2) *> equalJson(E1,E2).
  equalJson(_,_) => .false.

  public implementation coercion[string,json] => {.
    _coerce(T) => parseJson(T).
  .}

  public parseJson:(string)=>option[json].
  parseJson(T) where (J,_)^=pJ(skpBlnks(T::cons[integer])) => some(J).
  parseJson(_) default => .none.

  pJ:(cons[integer]) => option[(json,cons[integer])].
  pJ([Ch,..L]) => ppJ(Ch,L).

  ppJ:(integer,cons[integer]) => option[(json,cons[integer])].
  ppJ(0ct,[0cr,0cu,0ce,..L]) => some((.jTrue,L)).
  ppJ(0cf,[0ca,0ca,0cl,0cs,0ce,..L]) => some((.jFalse,L)).
  ppJ(0cn,[0cu,0cl,0cl,..L]) => some((.jNull,L)).
  ppJ(0c-,L) where (Dx,LL) .= psNum(L) => some((jNum(-Dx),LL)).
  ppJ(D,L) where isDigit(D) && (Dx,LL).=psNum([D,..L]) => some((jNum(Dx),LL)).
  ppJ(0c\",L) where (Txt,LL) .= psStrng(L,[]) => some((jTxt(Txt),LL)).
  ppJ(0c[,L) => psSeq(skpBlnks(L)).
  ppJ(0c{,L) => psColl(skpBlnks(L)).

  skpBlnks:(cons[integer]) => cons[integer].
  skpBlnks([]) => [].
  skpBlnks([0c ,..L]) => skpBlnks(L).
  skpBlnks([0c\n,..L]) => skpBlnks(L).
  skpBlnks([0c\t,..L]) => skpBlnks(L).
  skpBlnks(L) default => L.

  psNum:(cons[integer]) => (float,cons[integer]).
  psNum(L) where (First,R) .= psNat(L,0) &&
    (Val,Rest) .= psMoreNum(First::float,R) => (Val,Rest).

  psNat:(cons[integer],integer) => (integer,cons[integer]).
  psNat([Dx,..L],Sf) where isDigit(Dx) => psNat(L,Sf*10+digitVal(Dx)).
  psNat(L,Sf) default => (Sf,L).

  psDec:(cons[integer]) => (integer,cons[integer]).
  psDec([0c-,..L]) where (Ps,R) .= psNat(L,0) => (-Ps,R).
  psDec(L) => psNat(L,0).

  psFrac:(float,float,cons[integer]) => (float,cons[integer]).
  psFrac(Scale,Fr,[D,..L]) where isDigit(D) => psFrac(Scale*0.1,digitVal(D)::float*Scale+Fr,L).
  psFrac(_,Fr,L) default => (Fr,L).

  psMoreNum:(float,cons[integer]) => (float,cons[integer]).
  psMoreNum(F,[0c.,..L]) where (Mn,LL).=psFrac(0.1,F,L) => psExp(Mn,LL).
  psMoreNum(F,L) default => (F,L).

  psExp:(float,cons[integer]) => (float,cons[integer]).
  psExp(Mn,[0ce,..L]) where (Exp,R).= psDec(L) => (Mn*(10.0**Exp::float),R).

  psString:(cons[integer]) => (string,cons[integer]).
  psString(L) where [0c",..LL].=skpBlnks(L) => psStrng(LL,[]).

  psStrng:(cons[integer],cons[integer]) => (string,cons[integer]).
  psStrng([0c\\,..L],SoF) where (Ch,LL).=psChrRef(L) => psStrng(LL,[Ch,..SoF]).
  psStrng([0c",..L],SoF) => (reverse(SoF)::string,L).
  psStrng([Ch,..L],SoF) => psStrng(L,[Ch,..SoF]).

  psChrRef:(cons[integer]) => (integer,cons[integer]).
  psChrRef([0cu,..L]) => psHex(L,0,4).
  psChrRef([0cb,..L]) => (0c\b,L).
  psChrRef([0cf,..L]) => (0c\f,L).
  psChrRef([0cn,..L]) => (0c\n,L).
  psChrRef([0ct,..L]) => (0c\t,L).
  psChrRef([0cr,..L]) => (0c\r,L).
  psChrRef([Ch,..L]) => (Ch,L).

  psHex:(cons[integer],integer,integer) => (integer,cons[integer]).
  psHex(L,So,0) => (So,L).
  psHex([H,..L],So,Cn) where Hx^=isHexDigit(H) && Cn>0 => psHex(L,So*16+Hx,Cn-1).
  psHex(L,So,_) default => (So,L).

  psSeq:(cons[integer]) => option[(json,cons[integer])].
  psSeq([0c],..L]) => some((jSeq([]),L)).
  psSeq(L) where (El,LL)^=pJ(L) => psMoreSeq(skpBlnks(LL),[El]).

  psMoreSeq([0c],..L],SoF) => some((jSeq(reverse(SoF)),L)).
  psMoreSeq([0c,,..L],SoF) where (El,LL)^=pJ(L) =>
    psMoreSeq(skpBlnks(LL),[El,..SoF]).

  psColl:(cons[integer]) => option[(json,cons[integer])].
  psColl([0c},..L]) => some((jColl([]),L)).
  psColl(L) where (Ky,El,LL).=psEntry(L) => psMoreCol(skpBlnks(LL),[Ky->El]).

  psMoreCol([0c},..L],SoF) => some((jColl(SoF),L)).
  psMoreCol([0c,,..L],SoF) where (Ky,El,LL).=psEntry(L) => psMoreCol(skpBlnks(LL),SoF[Ky->El]).

  psEntry(L) where (Ky,L1) .= psString(skpBlnks(L)) &&
      [0c:,..L2].=skpBlnks(L1) &&
      (Vl,LL) ^= pJ(skpBlnks(L2)) => (Ky,Vl,LL).
}
