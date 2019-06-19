star.compiler.terms{
  import star.

  import star.compiler.location.
  import star.compiler.types.

  public term ::= voyd
    | intgr(integer)
    | flot(float)
    | strg(string)
    | term(term,list[term])
    | lbl(string,integer)
    | enum(string).

  public core ::= idnt(string) |
    lit(term) |
    tupl(locn,list[core]) | 
    cll(locn,core,core) |
    ecll(locn,string,core) |
    ocll(locn,core,core) |
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

  public ruleSet ::= fnDef(locn,term,tipe,list[core],core) |
    vrDef(locn,term,tipe,core) |
    rcDef(locn,term,tipe).

  public implementation display[term] => let{
    dispT(voyd) => ss("␀").
    dispT(intgr(Ix)) => disp(Ix).
    dispT(flot(Dx)) => disp(Dx).
    dispT(strg(Sx)) => disp(Sx).
    dispT(term(lbl("[]",_),Args)) => ssSeq([ss("["),ssSeq(dispTs(Args)),ss("]")]).
    dispT(term(lbl(T,_),Args)) where isTupleLbl(T) => ssSeq([ss("("),ssSeq(dispTs(Args)),ss(")")]).
    dispT(term(Op,Args)) => ssSeq([dispT(Op),ss("("),ssSeq(dispTs(Args)),ss(")")]).
    dispT(lbl(Nm,Ar)) => ssSeq([ss(Nm),ss("/"),disp(Ar)]).
    dispT(enum(Sx)) => ssSeq([ss("'"),ss(Sx),ss("'")]).

    dispTs(Els) => interleave(Els//dispT,ss(",")).

    isTupleLbl(T) where [0c(,0c),.._] .= T::list[integer] => true.
    isTupleLbl(_) default => false.
  } in {.
    disp(T) => dispT(T)
  .}

  public implementation display[core] => let{
    dispC(idnt(Nm)) => ss(Nm).
    dispC(lit(T)) => disp(T).
    dispC(tupl(_,Args)) => ssSeq([ss("("),ssSeq(dispAs(Args,"")),ss(")")]).
    dispC(cll(_,Op,Args)) => ssSeq([dispC(Op),dispC(Args)]).
    dispC(ecll(_,Op,Args)) => ssSeq([ss("esc "),ss(Op),dispC(Args)]).
    dispC(ocll(_,Ob,Args)) => ssSeq([dispC(Ob),ss(":"),dispC(Args)]).
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

  public implementation display[ruleSet] => let{
    dispRuleSet(fnDef(Lc,Nm,Tp,Args,Value)) =>
      ssSeq([ss("Function @ "),disp(Lc),ss("\n"),
	    disp(Nm),ss(":"),disp(Tp),ss("\n"),
	  disp(Nm),disp(tupl(Lc,Args)), ss(" => "),disp(Value),ss(".")]).
    dispRuleSet(vrDef(Lc,Nm,Tp,Value)) =>
      ssSeq([ss("Variable @ "),disp(Lc),ss("\n"),
	    disp(Nm),ss(":"),disp(Tp),ss("\n"),
	  disp(Nm),ss(" = "),disp(Value),ss(".")]).
    dispRuleSet(rcDef(Lc,Nm,Tp)) =>
      ssSeq([ss("Constructor @ "),disp(Lc),ss("\n"),
	  disp(Nm),ss(":"),disp(Tp),ss(".")])
  } in {.
    disp(R) => dispRuleSet(R)
  .}

  public implementation coercion[term,string] => {
    _coerce(T) => _implode(encodeTerm(T)).
  }

  public encodeTerm:(term)=>list[integer].
  encodeTerm(T) => encodeT(T,[]).

  encodeT:(term,list[integer])=>list[integer].
  encodeT(voyd,Cs) => [Cs..,0cv].
  encodeT(intgr(Ix),Cs) => encodeInt(Ix,[Cs..,0cx]).
  encodeT(flot(Dx),Cs) => encodeText(Dx::string,[Cs..,0cd]).
  encodeT(strg(Tx),Cs) => encodeText(Tx,[Cs..,0cs]).
  encodeT(lbl(Nm,Ar),Cs) => encodeText(Nm,encodeNat(Ar,[Cs..,0co])).
  encodeT(enum(Nm),Cs) => encodeText(Nm,[Cs..,0ce]).
  encodeT(term(Op,Args),Cs) =>
    encodeTerms(Args,encodeT(Op,encodeNat(size(Args),[Cs..,0cn]))).

  encodeTerms([],Cs) => Cs.
  encodeTerms([T,..Ts],Cs) => encodeTerms(Ts,encodeT(T,Cs)).
  
  public decodeTerm:(list[integer])=>option[(term,list[integer])].
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
    valis (term(Op,Args),Lx)
  }
  decodeTerm([0cl,..Ls]) => do{
    (Ax,L0) <- decodeNat(Ls,0);
    (Els,Lx) <- decodeTerms(L0,Ax,[]);
    valis (term(lbl("[]",Ax),Els),Lx)
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

  public decodeSignature:(string) => option[tipe].
  decodeSignature(St) => let{
    decodeType:(list[integer]) => option[(tipe,list[integer])].
    decodeType([0cv,..Ts]) => some((voidType,Ts)).
    decodeType([0ci,..Ts]) => some((tipe("star.core*integer"),Ts)).
    decodeType([0cf,..Ts]) => some((tipe("star.core*float"),Ts)).
    decodeType([0cS,..Ts]) => some((tipe("star.core*string"),Ts)).
    decodeType([0cl,..Ts]) => some((tipe("star.core*boolean"),Ts)).
    decodeType([0ck,..Ts]) => do {
      (Nm,T0) <- decodeText(Ts);
      valis (kVar(Nm),T0)
    }
    decodeType([0cK,..Ts]) => do {
      (Ar,T0) <- decodeNat(Ts,0);
      (Nm,T1) <- decodeText(T0);
      valis (kFun(Nm,Ar),T1)
    }
    decodeType([0ct,..Ts]) => do {
      (Nm,T1) <- decodeText(Ts);
      valis (tipe(Nm),T1)
    }
    decodeType([0cz,..Ts]) => do {
      (Ar,T0) <- decodeNat(Ts,0);
      (Nm,T1) <- decodeText(T0);
      valis (tpFun(Nm,Ar),T1)
    }
    decodeType([0cL,..Ts]) => do {
      (ElTp,T0) <- decodeType(Ts);
      valis (lstType(ElTp),T0)
    }
    decodeType([0cU,..Ts]) => do {
      (OpTp,T0) <- decodeType(Ts);
      (ElTp,T1) <- decodeType(T0);
      valis (tpExp(OpTp,ElTp),T1)
    }
    decodeType([0cd,..Ts]) => do{
      (OpTp,T0) <- decodeType(Ts);
      (tupleType(Els),T1) <- decodeType(T0);
      valis (funDeps(OpTp,Els),T1)
    }
    decodeType([0cr,..Ts]) => do {
      (ElTp,T0) <- decodeType(Ts);
      valis (refType(ElTp),T0)
    }
    decodeType([0x28,..Ts]) => do {  -- 0x28 == (
      (Tps,T0) <- decodeTypes(Ts,[]);
      valis (tupleType(Tps),T0)
    }
    decodeType([0c:,..Ts]) => do{
      (V,T0) <- decodeType(Ts);
      (B,T1) <- decodeType(T0);
      valis (allType(V,B),T1)
    }
    decodeType([0ce,..Ts]) => do{
      (V,T0) <- decodeType(Ts);
      (B,T1) <- decodeType(T0);
      valis (existType(V,B),T1)
    }
    decodeType([0c|,..Ts]) => do{
      (V,T0) <- decodeType(Ts);
      (B,T1) <- decodeConstraint(T0);
      valis (constrainedType(V,B),T1)
    }
    decodeType([0cI,..Ts]) => do{
      (F1,T0) <- decodeFields(Ts);
      (F2,T1) <- decodeFields(T0);
      valis (faceType(F1,F2),T1)
    }
    decodeType([0cF,..Ts]) => do{
      (A,T0) <- decodeType(Ts);
      (R,T1) <- decodeType(T0);
      valis (funType(A,R),T1)
    }
    decodeType([0cC,..Ts]) => do{
      (A,T0) <- decodeType(Ts);
      (R,T1) <- decodeType(T0);
      valis (consType(A,R),T1)
    }
    decodeType([0cY,..Ts]) => do{
      (A,T0) <- decodeType(Ts);
      (R,T1) <- decodeType(T0);
      valis (typeExists(A,R),T1)
    }
    decodeType([0cy,..Ts]) => do{
      (A,T0) <- decodeType(Ts);
      (R,T1) <- decodeType(T0);
      valis (typeLambda(A,R),T1)
    }

    decodeTypes([0x29,..Ts],Tps) => some((Tps,Ts)). -- 0x29 == )
    decodeTypes(Ts,Tps) => do{
      (ElTp,T0) <- decodeType(Ts);
      decodeTypes(T0,[Tps..,ElTp])
    }

    decodeFields([0x7b,..Ts]) => decodeFlds(Ts,[]). -- 0x7b == {

    decodeFlds([0x7d,..Ts],Flds) => some((Flds,Ts)). -- 0x7d == }
    decodeFlds(Ts,Flds) => do{
      (Nm,T0) <- decodeText(Ts);
      (Tp,T1) <- decodeType(T0);
      decodeFlds(T1,[Flds..,(Nm,Tp)])
    }
    decodeConstraint([0cc,..T]) => do{
      (Con,T1) <- decodeType(T);
      valis (typeConstraint(Con),T1)
    }
    decodeConstraint([0ca,..T]) => do{
      (BT,T0) <- decodeType(T);
      (FT,T1) <- decodeType(T0);
      valis (fieldConstraint(BT,FT),T1)
    }
  } in do{
    (Tp,_) <- decodeType(St::list[integer]);
    valis Tp
  }

  public encodeSignature:(tipe) => string.
  encodeSignature(Tp) => let{
    encodeType:(tipe,list[integer]) => list[integer].
    encodeType(voidType,Ts) => [Ts..,0cv].
    encodeType(tipe("star.core*integer"),Ts) => [Ts..,0ci].
    encodeType(tipe("star.core*float"),Ts) => [Ts..,0cf].
    encodeType(tipe("star.core*string"),Ts) => [Ts..,0cS].
    encodeType(tipe("star.core*boolean"),Ts) => [Ts..,0cl].
    encodeType(kVar(Nm),Ts) => encodeText(Nm,[Ts..,0ck]).
    encodeType(kFun(Nm,Ar),Ts) => encodeText(Nm,encodeNat(Ar,[Ts..,0cK])).
    encodeType(tipe(Nm),Ts) => encodeText(Nm,[Ts..,0ct]).
    encodeType(tpFun(Nm,Ar),Ts) => encodeText(Nm,encodeNat(Ar,[Ts..,0cz])).
    encodeType(tpExp(tpFun("star.core*list",1),El),Ts) =>
      encodeType(deRef(El),[Ts..,0cL]).
    encodeType(tpExp(tpFun("star.core*ref",1),El),Ts) =>
      encodeType(deRef(El),[Ts..,0cr]).
    encodeType(tpExp(tpExp(tpFun("=>",2),A),R),Ts) =>
      encodeType(deRef(R),encodeType(deRef(A),[Ts..,0cF])).
    encodeType(tpExp(tpExp(tpFun("<=>",2),A),R),Ts) =>
      encodeType(deRef(R),encodeType(deRef(A),[Ts..,0cC])).
    encodeType(tpExp(Op,A),Ts) =>
      encodeType(deRef(A),encodeType(deRef(Op),[Ts..,0cU])).
    encodeType(tupleType(Els),Ts) =>
      encodeTypes(Els,[Ts..,0x28]). -- 0x28==(
    encodeType(allType(V,T),Ts) =>
      encodeType(deRef(T),encodeType(deRef(V),[Ts..,0c:])).
    encodeType(existType(V,T),Ts) =>
      encodeType(deRef(T),encodeType(deRef(V),[Ts..,0ce])).
    encodeType(constrainedType(T,C),Ts) =>
      encodeConstraint(C,encodeType(deRef(T),[Ts..,0c|])).
    encodeType(funDeps(V,Ds),Ts) =>
      encodeType(tupleType(Ds),encodeType(deRef(V),[Ts..,0cd])).
    encodeType(faceType(Flds,Tps),Ts) =>
      encodeFields(Tps,encodeFields(Flds,[Ts..,0cI])).
    encodeType(typeExists(H,I),Ts) =>
      encodeType(deRef(I),encodeType(deRef(H),[Ts..,0cY])).
    encodeType(typeLambda(Hd,I),Ts) =>
      encodeType(deRef(I),encodeType(deRef(Hd),[Ts..,0cy])).

    encodeTypes:(list[tipe],list[integer])=>list[integer].
    encodeTypes([],Ts) => [Ts..,0x29].
    encodeTypes([T,..Tps],Ts) =>
      encodeTypes(Tps,encodeType(deRef(T),Ts)).
    
    encodeFields:(list[(string,tipe)],list[integer])=>list[integer].
    encodeFields(Flds,Ts) => encodeFlds(Flds,[Ts..,0x7b]). -- 0x7b == {
    
    encodeFlds:(list[(string,tipe)],list[integer])=>list[integer].
    encodeFlds([],Ts) => [Ts..,0x7d]. -- 0x7d == }
    encodeFlds([(Nm,T),..Tps],Ts) =>
      encodeFlds(Tps,encodeType(deRef(T),encodeText(Nm,Ts))).

    encodeConstraint(typeConstraint(T),Ts) =>
      encodeType(deRef(T),[Ts..,0cc]).
    encodeConstraint(fieldConstraint(V,T),Ts) =>
      encodeType(deRef(T),encodeType(deRef(V),[Ts..,0ca])).
  } in encodeType(deRef(Tp),[])::string.

  encodeText:(string,list[integer]) => list[integer].
  encodeText(Txt,Ts) where Chrs .= Txt::list[integer] &&
      D.=findDelim(Chrs,[0c|,0c/,0c%]) =>
    encodeQuoted(Chrs,D,[Ts..,D]).

  findDelim:(list[integer],list[integer])=>integer.
  findDelim(Chrs,[]) => 0x22. -- == "
  findDelim(Chrs,[D,..Ds]) where D in Chrs => findDelim(Chrs,Ds).
  findDelim(Chrs,[D,.._]) => D.

  encodeQuoted([],D,Ts) => [Ts..,D].
  encodeQuoted([D,..Cs],D,Ts) => encodeQuoted(Cs,D,[Ts..,0c\\,D]).
  encodeQuoted([C,..Cs],D,Ts) => encodeQuoted(Cs,D,[Ts..,C]).

  encodeNat:(integer,list[integer]) => list[integer].
  encodeNat(Dx,Ts) where Dx>=0 && Dx=<9 =>
    [Ts..,Dx+0c0].
  encodeNat(Dx,Ts) => [encodeNat(Dx/10,Ts)..,(Dx%10)+0c0].

  encodeInt:(integer,list[integer])=>list[integer].
  encodeInt(Ix,Ts) where Ix<0 => encodeNat(-Ix,[Ts..,0c-]).
  encodeInt(Ix,Ts) => encodeNat(Ix,Ts).
}
