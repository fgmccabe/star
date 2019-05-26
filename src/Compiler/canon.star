star.compiler.canon{
  import star.

  import star.compiler.location.
  import star.compiler.types.

  public canon ::= vr(locn,string,tipe) |
    intLit(locn,integer) |
    floatLit(locn,float) |
    stringLit(locn,string) |
    enm(locn,string,tipe) |
    whr(locn,canon,canon) |
    abstraction(locn,canon,canon,canon,tipe) |
    search(locn,canon,canon,canon) |
    match(locn,canon,canon) |
    conj(locn,canon,canon) |
    disj(locn,canon,canon) |
    neg(locn,canon) |
    cond(locn,canon,canon,canon) |
    apply(locn,canon,canon,tipe) |
    tple(locn,list[canon]) |
    varRef(locn,canon,tipe) |
    lambda(locn,list[(locn,canon,canon,canon)],tipe) |
    letExp(locn,canon,canon) |
    theta(locn,string,boolean,list[list[canonDef]],list[canon],tipe) |
    record(locn,string,boolean,list[list[canonDef]],list[canon],tipe).

  public canonDef ::= varDef(locn,string,string,canon,list[constraint],tipe) |
      typeDef(locn,string,tipe,tipe) |
      conDef(locn,string,string,tipe) |
      cnsDef(locn,string,string,tipe) |
      funDef(locn,string,string,list[(locn,canon,canon,canon)],tipe,list[constraint]).

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

  public implementation display[canon] => let{
    showCanon(vr(_,Nm,_)) => ss(Nm).
    showCanon(intLit(_,Ix)) => disp(Ix).
    showCanon(floatLit(_,Dx)) => disp(Dx).
    showCanon(stringLit(_,Sx)) => disp(Sx).
    showCanon(enm(_,Nm,_)) => ss(Nm).
    showCanon(whr(_,E,C)) => ssSeq([showCanon(E),ss(" where "),showCanon(C)]).
    showCanon(abstraction(_,Exp,Gen,_,_)) => ssSeq([ss("{"),showCanon(Exp),ss(" | "),showCanon(Gen),ss("}")]).
    showCanon(search(_,Ptn,Gen,_)) => ssSeq([showCanon(Ptn),ss(" in "),showCanon(Gen)]).
    showCanon(match(_,Ptn,Gen)) => ssSeq([showCanon(Ptn),ss(" .= "),showCanon(Gen)]).
    showCanon(conj(_,L,R)) => ssSeq([showCanon(L),ss(" && "),showCanon(R)]).
    showCanon(disj(_,L,R)) => ssSeq([showCanon(L),ss(" || "),showCanon(R)]).
    showCanon(neg(_,R)) => ssSeq([ss(" \\+ "),showCanon(R)]).
    showCanon(cond(_,T,L,R)) => ssSeq([showCanon(T),ss("?"),showCanon(L),ss(" | "),showCanon(R)]).
    showCanon(apply(_,L,R,_)) => ssSeq([showCanon(L),showCanon(R)]).
    showCanon(tple(_,Els)) => ssSeq([ss("("),ssSeq(interleave(Els//showCanon,ss(","))),ss(")")]).
    showCanon(varRef(_,R,_)) => ssSeq([showCanon(R),ss("!")]).
    showCanon(lambda(_,Rls,_)) => ssSeq(interleave(Rls//showRl,ss("|"))).
    showCanon(letExp(_,Th,Ep)) => ssSeq([ss("let "),showCanon(Th),ss(" in "),showCanon(Ep)]).
    showCanon(theta(_,_,_,Groups,Others,_)) => ssSeq([ss("{"),ssSeq(flatten(Groups)//showDef),ssSeq(Others//showOther),ss("}")]).
    showCanon(record(_,_,_,Groups,Others,_)) => ssSeq([ss("{."),ssSeq(flatten(Groups)//showDef),ssSeq(Others//showOther),ss(".}")]).

    showOther(T) => ssSeq([showCanon(T),ss(".\n")]).
  } in {.
    disp(C) => showCanon(C)
  .}

  showDef(varDef(_,Nm,_,V,_,_)) => ssSeq([ss(Nm),ss("="),disp(V)]).
  showDef(typeDef(_,Nm,T,_)) => ssSeq([ss(Nm),ss("~>"),disp(T)]).
  showDef(conDef(_,Nm,_,Tp)) => ssSeq([ss(Nm),ss("::="),disp(Tp)]).
  showDef(cnsDef(_,Nm,_,Tp)) => ssSeq([ss(Nm),ss("<=>"),disp(Tp)]).
  showDef(funDef(_,Nm,_,Rls,_,_)) => ssSeq([ss(Nm),ss("="),ssSeq(Rls//showRl)]).

  showRl:((locn,canon,canon,canon)) => ss.
  showRl((_,Ptn,Cond,Val)) => ssSeq([disp(Ptn),ss(" where "),disp(Cond),ss(" => "),disp(Val)]).
}
