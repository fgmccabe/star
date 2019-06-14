star.compiler.canon{
  import star.
  import star.pkg.

  import star.compiler.meta.
  import star.compiler.location.
  import star.compiler.types.

  public pkgSpec::=pkgSpec(pkg,list[importSpec],tipe,list[canonDef],list[implDefn]).

  public implDefn ::= implDfn(option[locn],string,string,tipe).

  public canon ::= vr(locn,string,tipe) |
    mtd(locn,string,tipe) |
    intLit(locn,integer) |
    floatLit(locn,float) |
    stringLit(locn,string) |
    enm(locn,string,tipe) |
    whr(locn,canon,canon) |
    dot(locn,canon,string,tipe) |
    abstraction(locn,canon,canon,tipe) |
    ixabstraction(locn,canon,canon,canon,tipe) |
    act(locn,canonAction) | 
    search(locn,canon,canon,canon) |
    match(locn,canon,canon) |
    conj(locn,canon,canon) |
    disj(locn,canon,canon) |
    neg(locn,canon) |
    cond(locn,canon,canon,canon) |
    apply(locn,canon,canon,tipe) |
    tple(locn,list[canon]) |
    varRef(locn,canon,tipe) |
    lambda(locn,canon,canon,canon,tipe) |
    letExp(locn,canon,canon) |
    theta(locn,string,boolean,list[list[canonDef]],list[canon],tipe) |
    record(locn,string,boolean,list[list[canonDef]],list[canon],tipe) .

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
    implDef(locn,string,string,tipe) |
    funDef(locn,string,string,list[canon],tipe,list[constraint]).

  public implementation hasType[canon] => {.
    typeOf(vr(_,_,T)) => T.
    typeOf(intLit(_,_)) => tipe("star.core*integer").
    typeOf(floatLit(_,_)) => tipe("star.core*float").
    typeOf(stringLit(_,_)) => tipe("star.core*string").
    typeOf(enm(_,_,Tp)) => Tp.

    typeOf(match(_,_,_)) => tipe("star.core*boolean").
    typeOf(conj(_,_,_)) => tipe("star.core*boolean").
    typeOf(disj(_,_,_)) => tipe("star.core*boolean").
    typeOf(search(_,_,_,_)) => tipe("star.core*boolean").
    typeOf(cond(_,_,L,_)) => typeOf(L).
  .}

  public implementation hasLoc[canon] => {.
    locOf(vr(Lc,_,_)) => Lc.
  .}

  public implementation display[pkgSpec] => {.
    disp(pkgSpec(Pkg,Imports,Face,Cons,Impls)) =>
      ss("Package: $(Pkg), imports=$(Imports), Signature=$(Face),Contracts=$(Cons),Implementations:$(Impls)").
  .}

  public implementation display[implDefn] => let{
    dispImpl(implDfn(_,Con,Full,Tp)) =>
      ss("implementation for $(Con), full name $(Full), type: $(Tp)")
  } in {
    disp(D) => dispImpl(D)
  }

  public implementation display[canon] => let{
    showCanon(vr(_,Nm,_)) => ss(Nm).
    showCanon(intLit(_,Ix)) => disp(Ix).
    showCanon(floatLit(_,Dx)) => disp(Dx).
    showCanon(stringLit(_,Sx)) => disp(Sx).
    showCanon(enm(_,Nm,_)) => ss(Nm).
    showCanon(whr(_,E,C)) => ssSeq([showCanon(E),ss(" where "),showCanon(C)]).
    showCanon(dot(_,R,F,_)) => ssSeq([showCanon(R),ss("."),ss(F)]).
    showCanon(abstraction(_,Exp,Gen,_)) => ssSeq([ss("{"),showCanon(Exp),ss(" | "),showCanon(Gen),ss("}")]).
    showCanon(ixabstraction(_,Ky,Vl,Gen,_)) =>
      ssSeq([ss("{"),showCanon(Ky),ss(" -> "),showCanon(Vl),ss(" | "),showCanon(Gen),ss("}")]).
    showCanon(search(_,Ptn,Gen,_)) => ssSeq([showCanon(Ptn),ss(" in "),showCanon(Gen)]).
    showCanon(match(_,Ptn,Gen)) => ssSeq([showCanon(Ptn),ss(" .= "),showCanon(Gen)]).
    showCanon(conj(_,L,R)) => ssSeq([showCanon(L),ss(" && "),showCanon(R)]).
    showCanon(disj(_,L,R)) => ssSeq([showCanon(L),ss(" || "),showCanon(R)]).
    showCanon(neg(_,R)) => ssSeq([ss(" \\+ "),showCanon(R)]).
    showCanon(cond(_,T,L,R)) => ssSeq([showCanon(T),ss("?"),showCanon(L),ss(" | "),showCanon(R)]).
    showCanon(apply(_,L,R,_)) => ssSeq([showCanon(L),showCanon(R)]).
    showCanon(tple(_,Els)) => ssSeq([ss("("),ssSeq(interleave(Els//showCanon,ss(","))),ss(")")]).
    showCanon(varRef(_,R,_)) => ssSeq([showCanon(R),ss("!")]).
    showCanon(lambda(Lc,Args,Cond,Exp,Tp)) => showRl("",lambda(Lc,Args,Cond,Exp,Tp)).
    showCanon(letExp(_,Th,Ep)) => ssSeq([ss("let "),showCanon(Th),ss(" in "),showCanon(Ep)]).
    showCanon(theta(_,_,_,Groups,Others,_)) => ssSeq([ss("{"),ssSeq(flatten(Groups)//showDef),ssSeq(Others//showOther),ss("}")]).
    showCanon(record(_,_,_,Groups,Others,_)) => ssSeq([ss("{."),ssSeq(flatten(Groups)//showDef),ssSeq(Others//showOther),ss(".}")]).

    showOther(T) => ssSeq([showCanon(T),ss(".\n")]).
  } in {.
    disp(C) => showCanon(C)
  .}

  public implementation display[canonDef] => {
    disp(D) => showDef(D)
  }

  showDef(varDef(_,Nm,_,V,_,Tp)) => ssSeq([ss(Nm),ss(":"),disp(Tp),ss("="),disp(V)]).
  showDef(typeDef(_,Nm,T,_)) => ssSeq([ss("Type: "),ss(Nm),ss("~>"),disp(T)]).
  showDef(conDef(_,Nm,_,Tp)) => ssSeq([ss("Contract: "),ss(Nm),ss("::="),disp(Tp)]).
  showDef(cnsDef(_,Nm,_,Tp)) => ssSeq([ss("Constructor: "),ss(Nm),ss(":"),disp(Tp)]).
  showDef(funDef(_,Nm,_,Rls,Tp,_)) => ssSeq([ss(Nm),ss(":"),disp(Tp),ss("="),ssSeq(interleave(Rls//(Rl)=>showRl(Nm,Rl),ss(". ")))]).

  showRl:(string,canon) => ss.
  showRl(Nm,lambda(_,Ptn,vr(_,"true",tipe("star.core*boolean")),Val,_)) => ssSeq([ss(Nm),disp(Ptn),ss(" => "),disp(Val)]).
  showRl(Nm,lambda(_,Ptn,Cond,Val,_)) => ssSeq([ss(Nm),disp(Ptn),ss(" where "),disp(Cond),ss(" => "),disp(Val)]).
}
