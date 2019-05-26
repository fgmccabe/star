star.compiler.terms{
  import star.

  import star.compiler.location.
  import star.compiler.types.

  term ::= voyd
    | idnt(string)
    | intgr(integer)
    | flot(float)
    | strg(string)
    | data(term,list[term])
    | lbl(string,integer)
    | enum(string).

  public implementation display[term] => let{
    dispTerm(voyd) => ss("␀").
    dispTerm(idnt(Nm)) => ss(Nm).
    dispTerm(intgr(Ix)) => disp(Ix).
    dispTerm(flot(Dx)) => disp(Dx).
    dispTerm(strg(Sx)) => disp(Sx).
    dispTerm(data(Op,Args)) => ssSeq([dispTerm(Op),ss("("),ssSeq(dispTerms(Args,"")),ss(")")]).
    dispTerm(lbl(Nm,Ar)) => ssSeq([ss(Nm),ss("/"),disp(Ar)]).
    dispTerm(enum(Sx)) => ssSeq([ss("'"),ss(Sx),ss("'")]).

    dispTerms([],_) => [].
    dispTerms([T,..Ts],Sp) => [dispTerm(T),ss(Sp),..dispTerms(Ts,", ")].
  } in {.
    disp(T) => dispTerm(T)
  .}

  decodeTerm:(list[integer])=>option[(term,list[integer])].
  decodeTerm([0cv,..Ls]) => some((voyd,Ls)).
  decodeTerm([0cx,..Ls]) where (Ix,L0)^=decodeInt(Ls) => some((intgr(Ix),L0)).
  decodeTerm([0cd,..Ls]) => do{
    (Txt,Lx) <- decodeText(Ls);
    valis (flot(Txt::float),Lx)
  }
  decodeTerm([0ce,..Ls]) => do{
    (Nm,Lx) <- decodeText(Ls);
    valis (enum(Nm),Lx)
  }
  decodeTerm([0cs,..Ls]) => do{
    (Nm,Lx) <- decodeText(Ls);
    valis (strg(Nm),Lx)
  }
  decodeTerm([0co,..Ls]) => do{
    (Ar,L0) <- decodeNat(Ls,0);
    (Nm,Lx) <- decodeText(L0);
    valis (lbl(Nm,Ar),Lx)
  }
  decodeTerm([0cn,..Ls]) => do{
    (Ax,L0) <- decodeNat(Ls,0);
    (Op,LL1) <- decodeTerm(L0);
    (Args,Lx) <- decodeTerms(LL1,Ax,[]);
    valis (data(Op,Args),Lx)
  }

  decodeTerms:(list[integer],integer,list[term]) => option[(list[term],list[integer])].
  decodeTerms(L,0,Args) => some((Args,L)).
  decodeTerms(L,Ix,Args) => do{
    (Arg,L0) <- decodeTerm(L);
    decodeTerms(L0,Ix-1,[Args..,Arg])
  }

  decodeInt:(list[integer])=>option[(integer,list[integer])].
  decodeInt([0c-,..L]) where (Px,Lx) ^= decodeNat(L,0) => some((-Px,Lx)).
  decodeInt(L) default => decodeNat(L,0).
  
  decodeNat:(list[integer],integer) => option[(integer,list[integer])].
  decodeNat([Cx,..Ls],Ix) where isDigit(Cx) => decodeNat(Ls,Ix*10+digitVal(Cx)).
  decodeNat(Ls,Ix) default => some((Ix,Ls)).

  decodeText:(list[integer]) => option[(string,list[integer])].
  decodeText([C,..L]) => collectQuoted(L,[],C).

  collectQuoted:(list[integer],list[integer],integer) => option[(string,list[integer])].
  collectQuoted([S,..Lx],SoF,S) => some((SoF::string,Lx)).
  collectQuoted([0c\\,X,..L],SoF,S) => collectQuoted(L,[SoF..,X],S).
  collectQuoted([X,..L],SoF,S) => collectQuoted(L,[SoF..,X],S).
  
}
