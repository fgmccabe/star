star.compiler.terms{
  import star.

  import star.compiler.location.
  import star.compiler.types.

  public term ::= voyd
    | intgr(integer)
    | flot(float)
    | strg(string)
    | data(term,list[term])
    | lbl(string,integer)
    | enum(string).

  public core ::= idnt(string) |
    lit(term) |
    cll(locn,core,list[core]) |
    ecll(locn,string,list[core]) |
    ocll(locn,core,list[core]) |
    dte(locn,core,core) |
    ltt(locn,core,tipe,core,core) |
    whr(locn,core,core) |
    vrn(locn,list[(string,core,tipe)],core) |
    case(locn,core,list[(locn,core,core)],core) |
    seqn(locn,core,core) |
    cnj(locn,core,core) |
    cnd(locn,core,core,core) |
    dsj(locn,core,core) |
    mtch(locn,core,core) |
    ng(locn,core).

  public implementation display[term] => let{
    dispT(voyd) => ss("␀").
    dispT(intgr(Ix)) => disp(Ix).
    dispT(flot(Dx)) => disp(Dx).
    dispT(strg(Sx)) => disp(Sx).
    dispT(data(lbl("[]",_),Args)) => ssSeq([ss("["),ssSeq(dispTs(Args,"")),ss("]")]).
    dispT(data(Op,Args)) => ssSeq([dispT(Op),ss("("),ssSeq(dispTs(Args,"")),ss(")")]).
    dispT(lbl(Nm,Ar)) => ssSeq([ss(Nm),ss("/"),disp(Ar)]).
    dispT(enum(Sx)) => ssSeq([ss("'"),ss(Sx),ss("'")]).

    dispTs([],_) => [].
    dispTs([T,..Ts],Sp) => [dispT(T),ss(Sp),..dispTs(Ts,", ")].
  } in {.
    disp(T) => dispT(T)
  .}

  public implementation display[core] => let{
    dispC(idnt(Nm)) => ss(Nm).
    dispC(lit(T)) => disp(T).
    dispC(cll(_,Op,Args)) => ssSeq([dispC(Op),ss("("),ssSeq(dispAs(Args,"")),ss(")")]).
    dispC(ecll(_,Op,Args)) => ssSeq([ss("esc "),ss(Op),
	ss("("),ssSeq(dispAs(Args,"")),ss(")")]).
    dispC(ocll(_,Ob,Args)) => ssSeq([dispC(Ob),ss(":("),ssSeq(dispAs(Args,"")),ss(")")]).
    dispC(dte(_,Ob,F)) => ssSeq([dispC(Ob),ss("."),dispC(F)]).
    dispC(ltt(_,Vr,T,Vl,B)) => ssSeq([ss("let {"),
	dispC(Vr),ss(":"),disp(T),ss("="),dispC(Vl),ss("} in "),dispC(B)]).
    dispC(whr(_,T,C)) => ssSeq([ss("("),dispC(T),ss(" where "),dispC(C),ss(")")]).
    dispC(vrn(_,Nms,Val)) => ssSeq([ss("vars: ["),
	ssSeq(dispVarNames(Nms)),ss("]->"),dispC(Val)]).
    dispC(case(_,Exp,Cases,Deflt)) =>
      ssSeq([ss("case "),dispC(Exp),ss(" in {"),
	  ssSeq(dispCases(Cases)),ss("} else "),dispC(Deflt)]).
    dispC(seqn(_,L,R)) => ssSeq([dispC(L),ss(";"),dispC(R)]).
    dispC(cnj(_,L,R))  => ssSeq([dispC(L),ss("&&"),dispC(R)]).
    dispC(cnd(_,T,L,R)) => ssSeq([ss("("),dispC(T),ss("?"),
	dispC(L),ss("||"),dispC(R),ss(")")]).
    dispC(dsj(_,L,R))  => ssSeq([dispC(L),ss("||"),dispC(R)]).
    dispC(mtch(_,L,R)) => ssSeq([dispC(L),ss(".="),dispC(R)]).
    dispC(ng(_,R)) => ssSeq([ss("\\+"),dispC(R)]).

    dispAs([],_) => [].
    dispAs([T,..Ts],Sp) => [dispC(T),ss(Sp),..dispAs(Ts,", ")].

    dispVarNames([]) => [].
    dispVarNames([(Nm,Vl,Tp),..Vs]) =>
      [ss(Nm),ss(":"),disp(Tp),ss("="),dispC(Vl),..dispVarNames(Vs)].

    dispCases([]) => [].
    dispCases([(_,Ptn,Vl),..Cs]) => [dispC(Ptn),ss("->"),dispC(Vl),..dispCases(Cs)].
  } in {.
    disp(C) => dispC(C)
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
  decodeTerm([0cl,..Ls]) => do{
    (Ax,L0) <- decodeNat(Ls,0);
    (Els,Lx) <- decodeTerms(L0,Ax,[]);
    valis (data(lbl("[]",Ax),Els),Lx)
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
