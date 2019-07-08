star.compiler.canon{
  import star.
  import star.pkg.

  import star.compiler.meta.
  import star.compiler.location.
  import star.compiler.types.

  public pkgSpec::=pkgSpec(pkg,list[importSpec],tipe,list[canonDef],list[implSpec]).

  public implSpec ::= implSpec(option[locn],string,string,tipe).

  public canon ::= vr(locn,string,tipe) |
    mtd(locn,string,tipe) |
    over(locn,canon,tipe,list[constraint]) |
    intLit(locn,integer) |
    floatLit(locn,float) |
    stringLit(locn,string) |
    enm(locn,string,tipe) |
    whr(locn,canon,canon) |
    dot(locn,canon,string,tipe) |
    abstraction(locn,canon,canon,tipe) |
    act(locn,canonAction) | 
    serch(locn,canon,canon,canon) |
    match(locn,canon,canon) |
    conj(locn,canon,canon) |
    disj(locn,canon,canon) |
    neg(locn,canon) |
    cond(locn,canon,canon,canon) |
    apply(locn,canon,canon,tipe) |
    tple(locn,list[canon]) |
    lambda(list[equation],tipe) |
    letExp(locn,canon,canon) |
    theta(locn,string,boolean,list[list[canonDef]],list[canon],tipe) |
    record(locn,string,boolean,list[list[canonDef]],list[canon],tipe).

  public equation ::= eqn(locn,canon,canon).

  public canonAction ::= noDo(locn) |
    seqnDo(locn,canonAction,canonAction) |
    bindDo(locn,canon,canon,tipe,tipe,tipe) |
    varDo(locn,canon,canon) |
    delayDo(locn,canonAction,tipe,tipe) |
    assignDo(locn,canon,canon,tipe,tipe) |
    ifThenDo(locn,canon,canonAction,canonAction,tipe,tipe) |
    whileDo(locn,canon,canonAction,tipe,tipe) |
    forDo(locn,canon,canonAction,tipe,tipe) |
    tryCatchDo(locn,canonAction,canon,tipe,tipe,tipe) |
    throwDo(locn,canon,tipe,tipe) |
    returnDo(locn,canon,tipe,tipe) |
    simpleDo(locn,canon,tipe,tipe).
    
  public canonDef ::= varDef(locn,string,string,canon,list[constraint],tipe) |
    typeDef(locn,string,tipe,tipe) |
    conDef(locn,string,string,tipe) |
    cnsDef(locn,string,string,tipe) |
    implDef(locn,string,string,canon,tipe).

  public implementation hasType[canon] => {.
    typeOf(vr(_,_,T)) => T.
    typeOf(intLit(_,_)) => nomnal("star.core*integer").
    typeOf(floatLit(_,_)) => nomnal("star.core*float").
    typeOf(stringLit(_,_)) => nomnal("star.core*string").
    typeOf(enm(_,_,Tp)) => Tp.

    typeOf(match(_,_,_)) => nomnal("star.core*boolean").
    typeOf(conj(_,_,_)) => nomnal("star.core*boolean").
    typeOf(disj(_,_,_)) => nomnal("star.core*boolean").
    typeOf(serch(_,_,_,_)) => nomnal("star.core*boolean").
    typeOf(cond(_,_,L,_)) => typeOf(L).
  .}

  public implementation hasLoc[canon] => {.
    locOf(vr(Lc,_,_)) => Lc.
  .}

  public implementation hasLoc[equation] => {.
    locOf(eqn(Lc,_,_)) => Lc.
  .}

  public implementation display[pkgSpec] => {.
    disp(pkgSpec(Pkg,Imports,Face,Cons,Impls)) =>
      ss("Package: $(Pkg), imports=$(Imports), Signature=$(Face),Contracts=$(Cons),Implementations:$(Impls)").
  .}

  public implementation display[implSpec] => let{
    dispImpl(implSpec(_,Con,Full,Tp)) =>
      ss("implementation for $(Con), full name $(Full), type: $(Tp)")
  } in {
    disp(D) => dispImpl(D)
  }

  public implementation equality[canon] => let{
    eq(vr(_,N1,T1),vr(_,N2,T2)) => N1==N2 && T1==T2.
    eq(mtd(_,N1,T1),mtd(_,N2,T2)) => N1==N2 && T1==T2.
    eq(over(_,N1,T1,C1),over(_,N2,T2,C2)) => N1==N2 && T1==T2 && C1==C2.
    eq(intLit(_,I1),intLit(_,I2)) => I1==I2.
    eq(floatLit(_,D1),floatLit(_,D2)) => D1==D2.
    eq(stringLit(_,S1),stringLit(_,S2)) => S1==S2.
    eq(enm(_,N1,T1),enm(_,N2,T2)) => N1==N2 && T1==T2.
    eq(apply(_,O1,A1,T1),apply(_,O2,A2,T2)) => eq(O1,O2) && eq(A1,A2) && T1==T2.
    eq(whr(_,T1,C1),whr(_,T2,C2)) => eq(T1,T2) && eq(C1,C2).
    eq(dot(_,T1,F1,_),dot(_,T2,F2,_)) => eq(T1,T2) && F1==F2.
    eq(abstraction(_,P1,C1,T1),abstraction(_,P2,C2,T2)) => eq(P1,P2) && eq(C1,C2) && T1==T2
  } in {
    T1==T2 => eq(T1,T2)
  }

  public implementation hash[canon] => let{
    hsh(vr(_,N1,_)) => hash(N1).
    hsh(mtd(_,N1,_)) => hash(N1).
    hsh(over(_,N1,T1,C1)) => hash(N1)*37+hash(T1).
    hsh(intLit(_,I1)) => hash(I1).
    hsh(floatLit(_,D1)) => hash(D1).
    hsh(stringLit(_,S1)) => hash(S1).
    hsh(enm(_,N1,_)) => hash(N1).
    hsh(apply(_,O1,A1,T1)) => hsh(O1)*36+hsh(A1).
    hsh(whr(_,T1,C1)) => hsh(T1) *37+hsh(C1).
    hsh(dot(_,T1,F1,_)) => hsh(T1) *37+hash(F1).
    hsh(abstraction(_,P1,C1,T1)) => hsh(P1)*37+hsh(C1).
  } in {
    hash(T1) => hsh(T1)
  }

  public implementation display[canon] => let{
    showCanon(vr(_,Nm,_)) => ss(Nm).
    showCanon(mtd(_,Fld,_)) => ssSeq([ss("µ"),ss(Fld)]).
    showCanon(over(_,V,_,Cx)) => ssSeq([disp(Cx),ss("|:"),disp(V)]).
    showCanon(intLit(_,Ix)) => disp(Ix).
    showCanon(floatLit(_,Dx)) => disp(Dx).
    showCanon(stringLit(_,Sx)) => disp(Sx).
    showCanon(enm(_,Nm,_)) => ss(Nm).
    showCanon(whr(_,E,C)) => ssSeq([showCanon(E),ss(" where "),showCanon(C)]).
    showCanon(dot(_,R,F,_)) => ssSeq([showCanon(R),ss("."),ss(F)]).
    showCanon(abstraction(_,Exp,Gen,_)) =>
      ssSeq([ss("["),showCanon(Exp),ss(" | "),showCanon(Gen),ss("]")]).
    showCanon(serch(_,Ptn,Gen,_)) => ssSeq([showCanon(Ptn),ss(" in "),showCanon(Gen)]).
    showCanon(match(_,Ptn,Gen)) => ssSeq([showCanon(Ptn),ss(" .= "),showCanon(Gen)]).
    showCanon(conj(_,L,R)) => ssSeq([showCanon(L),ss(" && "),showCanon(R)]).
    showCanon(disj(_,L,R)) => ssSeq([showCanon(L),ss(" || "),showCanon(R)]).
    showCanon(neg(_,R)) => ssSeq([ss(" \\+ "),showCanon(R)]).
    showCanon(cond(_,T,L,R)) =>
      ssSeq([showCanon(T),ss("?"),showCanon(L),ss(" | "),showCanon(R)]).
    showCanon(apply(_,L,R,_)) => ssSeq([showCanon(L),showCanon(R)]).
    showCanon(tple(_,Els)) =>
      ssSeq([ss("("),ssSeq(interleave(Els//showCanon,ss(","))),ss(")")]).
    showCanon(lambda(Rls,Tp)) => ssSeq([ss("(λ"),showRls("λ",Rls),ss("λ)")]).
    showCanon(letExp(_,Th,Ep)) =>
      ssSeq([ss("let "),showCanon(Th),ss(" in "),showCanon(Ep)]).
    showCanon(theta(_,_,_,Groups,Others,_)) =>
      ssSeq([ss("{"),ssSeq(flatten(Groups)//showDef),ssSeq(Others//showOther),ss("}")]).
    showCanon(record(_,_,_,Groups,Others,_)) =>
      ssSeq([ss("{."),ssSeq(flatten(Groups)//showDef),ssSeq(Others//showOther),ss(".}")]).
    showOther(T) => ssSeq([showCanon(T),ss(".\n")]).
  } in {.
    disp(C) => showCanon(C)
  .}

  public implementation display[canonDef] => {
    disp(D) => showDef(D)
  }

  showDef(varDef(_,Nm,_,lambda(Rls,_),_,Tp)) =>
    ssSeq([ss(Nm),ss(":"),disp(Tp),ss("="),showRls(Nm,Rls)]).
  showDef(varDef(_,Nm,_,V,_,Tp)) => ssSeq([ss(Nm),ss(":"),disp(Tp),ss("="),disp(V)]).
  showDef(typeDef(_,Nm,T,_)) => ssSeq([ss("Type: "),ss(Nm),ss("~>"),disp(T)]).
  showDef(conDef(_,Nm,_,Tp)) => ssSeq([ss("Contract: "),ss(Nm),ss("::="),disp(Tp)]).
  showDef(cnsDef(_,Nm,_,Tp)) => ssSeq([ss("Constructor: "),ss(Nm),ss(":"),disp(Tp)]).
  showDef(implDef(_,Nm,FullNm,Exp,Tp)) =>
    ssSeq([ss("Implementation: "),ss(Nm),ss(" = "),ss(FullNm),ss(":"),disp(Tp),ss("="),disp(Exp)]).

  showRls:(string,list[equation]) => ss.
  showRls(Nm,Rls) => ssSeq(interleave(Rls//(Rl)=>showRl(Nm,Rl),ss(". "))).

  showRl:(string,equation) => ss.
  showRl(Nm,eqn(_,Ptn,Val)) => ssSeq([ss(Nm),disp(Ptn),ss(" => "),disp(Val)]).

  -- Useful constants
  public trueEnum:(locn)=>canon.
  trueEnum(Lc) => enm(Lc,"true",nomnal("star.core*boolean")).
}
