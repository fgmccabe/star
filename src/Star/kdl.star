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
  parseKdl(T) where (J,_)^=pK(skpBlnks(T::cons[char])) => some(J).
  parseKdl(_) default => .none.

  pK:(cons[char]) => option[(kdl,cons[char])].
  pK([Ch,..L]) => ppK(Ch,L).

  ppK:(integer,cons[char]) => option[(kdl,cons[char])].
  ppK(`t`,[`r`,`u`,`e`,..L]) => some((.jTrue,L)).
  ppK(`f`,[`a`,`a`,`l`,`s`,`e`,..L]) => some((.jFalse,L)).
  ppK(`n`,[`u`,`l`,`l`,..L]) => some((.jNull,L)).
  ppK(`-`,L) where (Dx,LL) .= psNum(L) => some((jNum(-Dx),LL)).
  ppK(D,L) where isDigit(D) && (Dx,LL).=psNum([D,..L]) => some((jNum(Dx),LL)).
  ppK(`\"`,L) where (Txt,LL) .= psStrng(L,[]) => some((kTxt(Txt),LL)).
  ppK(`[`,L) => psSeq(skpBlnks(L)).
  ppK(`{`,L) => psColl(skpBlnks(L)).

  skpBlnks:(cons[char]) => cons[char].
  skpBlnks([]) => [].
  skpBlnks([` `,..L]) => skpBlnks(L).
  skpBlnks([`\n`,..L]) => skpBlnks(L).
  skpBlnks([`\t`,..L]) => skpBlnks(L).
  skpBlnks([`-`,`-`,` `,..L]) => skpBlnks(skipToEol(L)).
  skpBlnks([`/`,`*`,..L]) => skpBlnks(skipComment(L)).
  skpBlnks(L) default => L.

  skipToEol([]) => [].
  skipToEol([`\n`,..L]) => L.
  skipToEol([_,..L]) => skipToEol(L).

  skipComment([]) => [].
  skipComment([`*`,`/`,..L]) => L.
  skipComment([_,..L]) => skipComment(L).

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

  psChrRef:(cons[char]) => (integer,cons[char]).
  psChrRef([`u`,..L]) => psHex(L,0,4).
  psChrRef([`b`,..L]) => (`\b`,L).
  psChrRef([`f`,..L]) => (`\f`,L).
  psChrRef([`n`,..L]) => (`\n`,L).
  psChrRef([`t`,..L]) => (`\t`,L).
  psChrRef([`r`,..L]) => (`\r`,L).
  psChrRef([Ch,..L]) => (Ch,L).

  psHex:(cons[char],integer,integer) => (integer,cons[char]).
  psHex(L,So,0) => (So,L).
  psHex([H,..L],So,Cn) where Hx^=isHexDigit(H) && Cn>0 => psHex(L,So*16+Hx,Cn-1).
  psHex(L,So,_) default => (So,L).

  psSeq:(cons[char]) => option[(kdl,cons[char])].
  psSeq([`]`,..L]) => some((jSeq([]),L)).
  psSeq(L) where (El,LL)^=pK(L) => psMoreSeq(skpBlnks(LL),[El]).

  psMoreSeq([`]`,..L],SoF) => some((jSeq(reverse(SoF)),L)).
  psMoreSeq([`,`,..L],SoF) where (El,LL)^=pK(L) =>
    psMoreSeq(skpBlnks(LL),[El,..SoF]).

  psColl:(cons[char]) => option[(kdl,cons[char])].
  psColl([`}`,..L]) => some((jColl([]),L)).
  psColl(L) where (Ky,El,LL).=psEntry(L) => psMoreCol(skpBlnks(LL),[Ky->El]).

  psMoreCol([`}`,..L],SoF) => some((jColl(SoF),L)).
  psMoreCol([`,`,..L],SoF) where (Ky,El,LL).=psEntry(L) => psMoreCol(skpBlnks(LL),SoF[Ky->El]).

  psEntry(L) where (Ky,L1) .= psString(skpBlnks(L)) &&
      [`:`,..L2].=skpBlnks(L1) &&
      (Vl,LL) ^= pK(skpBlnks(L2)) => (Ky,Vl,LL).
}
