star.kdl{
  import star.
  -- In progress ..
  -- Inspired by the KDL language from kdl.dev

  public kdl ::= kTxt(string) |
    kNode(string,cons[kdl]) |
    kLbled(string,kdl).

  public implementation display[kdl] => {
    disp(j) => dispKdl(j,0).
  }

  private dispKdl:(kdl,integer) => string.
  dispKdl(kTxt(T),_) => T.
  dispKdl(kNode(L,A),Sp) => "#(L) {\n#((A//(E)=>dispKdl(A,Sp+2))*)}".
  dispKdl(kLbled(L,A),Sp) => "#(L) : #(dispKdl(A,Sp))".

  public implementation coercion[kdl,string] => {.
    _coerce(K) => some(dispKdl(K)).
  .}

  public implementation equality[kdl] => {.
    T1 == T2 => equalKdl(T1,T2).
  .}

  equalKdl:(kdl,kdl)=>boolean.
  equalKdl(kTxt(S1),kTxt(S2)) => S1==S2.
  equalKdl(kNode(L1,C1),kNode(L2,C2)) => L1==L2 &&
      {? (K1,K2) in zip(C1,C2) *> equalKdl(K1,K2) ?}.
  equalKdl(kLbled(L1,C1),kLbled(L2,C2)) => L1==L2 && equalKdl(C1,C2).
  equalKdl(_,_) => .false.

  public implementation coercion[string,kdl] => {.
    _coerce(T) => parseKdl(T).
  .}

  public parseKdl:(string)=>option[kdl].
  parseKdl(T) where (J,_)^=pK(skpBlnks(T::cons[integer])) => some(J).
  parseKdl(_) default => .none.

  pK:(cons[integer]) => option[(kdl,cons[integer])].
  pK([Ch,..L]) => ppK(Ch,L).

  ppK:(integer,cons[integer]) => option[(kdl,cons[integer])].
  ppK(0ct,[0cr,0cu,0ce,..L]) => some((.jTrue,L)).
  ppK(0cf,[0ca,0ca,0cl,0cs,0ce,..L]) => some((.jFalse,L)).
  ppK(0cn,[0cu,0cl,0cl,..L]) => some((.jNull,L)).
  ppK(0c-,L) where (Dx,LL) .= psNum(L) => some((jNum(-Dx),LL)).
  ppK(D,L) where isDigit(D) && (Dx,LL).=psNum([D,..L]) => some((jNum(Dx),LL)).
  ppK(0c\",L) where (Txt,LL) .= psStrng(L,[]) => some((kTxt(Txt),LL)).
  ppK(0c[,L) => psSeq(skpBlnks(L)).
  ppK(0c{,L) => psColl(skpBlnks(L)).

  skpBlnks:(cons[integer]) => cons[integer].
  skpBlnks([]) => [].
  skpBlnks([0c ,..L]) => skpBlnks(L).
  skpBlnks([0c\n,..L]) => skpBlnks(L).
  skpBlnks([0c\t,..L]) => skpBlnks(L).
  skpBlnks([0c-,0c-,0c ,..L]) => skpBlnks(skipToEol(L)).
  skpBlnks([0c/,0c*,..L]) => skpBlnks(skipComment(L)).
  skpBlnks(L) default => L.

  skipToEol([]) => [].
  skipToEol([0c\n,..L]) => L.
  skipToEol([_,..L]) => skipToEol(L).

  skipComment([]) => [].
  skipComment([0c*,0c/,..L]) => L.
  skipComment([_,..L]) => skipComment(L).

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

  psSeq:(cons[integer]) => option[(kdl,cons[integer])].
  psSeq([0c],..L]) => some((jSeq([]),L)).
  psSeq(L) where (El,LL)^=pK(L) => psMoreSeq(skpBlnks(LL),[El]).

  psMoreSeq([0c],..L],SoF) => some((jSeq(reverse(SoF)),L)).
  psMoreSeq([0c,,..L],SoF) where (El,LL)^=pK(L) =>
    psMoreSeq(skpBlnks(LL),[El,..SoF]).

  psColl:(cons[integer]) => option[(kdl,cons[integer])].
  psColl([0c},..L]) => some((jColl([]),L)).
  psColl(L) where (Ky,El,LL).=psEntry(L) => psMoreCol(skpBlnks(LL),[Ky->El]).

  psMoreCol([0c},..L],SoF) => some((jColl(SoF),L)).
  psMoreCol([0c,,..L],SoF) where (Ky,El,LL).=psEntry(L) => psMoreCol(skpBlnks(LL),SoF[Ky->El]).

  psEntry(L) where (Ky,L1) .= psString(skpBlnks(L)) &&
      [0c:,..L2].=skpBlnks(L1) &&
      (Vl,LL) ^= pK(skpBlnks(L2)) => (Ky,Vl,LL).
}
