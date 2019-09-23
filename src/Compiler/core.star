star.compiler.core{
  import star.

  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.
  
  public crExp ::= crVar(locn,crVar)
    | crLit(locn,data,tipe)
    | crApply(locn,crExp,list[crExp],tipe)
    | crLet(locn,crVar,crExp,crExp)
    | crLam(locn,list[crVar],crExp)
    | crCase(locn,crExp,list[crCase],tipe)
    | crType(locn,tipe)
    | crAbort(locn,string).
  
  public crVar ::= crId(string,tipe)
    | crTpVr(string,crKind).

  crKind ~> tipe.

  public crCase ~> (locn,crExp,crExp).

  public mkCrTpl:(list[crExp],locn) => crExp.
  mkCrTpl(Args,Lc) where ATp .= tupleType(Args//typeOf) =>
    crApply(Lc,crLit(Lc,tplLbl(size(Args)), consType(ATp,ATp)),
      Args,ATp).

  public implementation equality[crVar] => {.
    crId(N1,T1) == crId(N2,T2) => N1==N2 && T1==T2.
    crTpVr(N1,T1) == crTpVr(N2,T2) => N1==N2 && T1==T2
  .}

  public implementation hash[crVar] => {.
    hash(crId(N,T)) => hash(N)*37+hash(T).
    hash(crTpVr(N,T)) => hash(N)*37+hash(T).
  .}

  public implementation hasLoc[crExp] => {
    locOf(crVar(Lc,_)) => Lc.
    locOf(crLit(Lc,_,_)) => Lc.
    locOf(crApply(Lc,_,_,_)) => Lc.
    locOf(crLet(Lc,_,_,_)) => Lc.
    locOf(crLam(Lc,_,_)) => Lc.
    locOf(crCase(Lc,_,_,_)) => Lc.
    locOf(crType(Lc,_)) => Lc.
  }

  public implementation hasType[crExp] => let{
    tpOf(crVar(_,V)) => typeOf(V).
    tpOf(crLit(_,_,Tp)) => Tp.
    tpOf(crApply(_,_,_,Tp)) => Tp.
    tpOf(crLet(_,_,_,E)) => tpOf(E).
    tpOf(crLam(_,Vs,E)) => funType(tupleType(Vs//typeOf),tpOf(E)).
    tpOf(crCase(_,_,_,Tp)) => Tp.
    tpOf(crType(_,Tp)) => Tp.
  } in {
    typeOf = tpOf
  }

  public implementation hasType[crVar] => {.
    typeOf(crId(_,Tp)) => Tp.
    typeOf(crTpVr(_,Tp)) => Tp.
  .}

  public implementation display[crExp] => let{
    dE(crVar(_,V)) => disp(V).
    dE(crLit(_,D,_)) => disp(D).
    dE(crApply(_,Op,As,_)) => ssSeq([dE(Op),ss("("),ssSeq(interleave(As//dE,ss(","))),ss(")")]).
    dE(crLet(_,V,E,I)) => ssSeq([ss("let "),disp(V),ss(" = "),dE(E),ss(" in "),dE(I)]).
    dE(crLam(_,Ps,R)) => ssSeq([ss("lambda"),ss("("),ssSeq(interleave(Ps//disp,ss(","))),ss(") => "),dE(R)]).
    dE(crCase(_,E,Cs,_)) => ssSeq([ss("case "),dE(E),ss(" in {"),ssSeq(interleave(Cs//dCase,ss("; "))),ss("}")]).
    dE(crType(_,T)) => ssSeq([ss("@"),disp(T)]).

    dCase((_,P,E)) => ssSeq([dE(P),ss(" -> "),dE(E)]).
  } in {
    disp = dE
  }

  public implementation display[crVar] => {.
    disp(crId(Nm,_)) => disp(Nm).
    disp(crTpVr(Nm,_)) => ssSeq([ss("%"),disp(Nm)]).
  .}

  public rewriteTerm:(crExp,map[string,crExp])=>crExp.
  rewriteTerm(crVar(Lc,V),M) => rewriteVar(Lc,V,M).
  rewriteTerm(crLit(Lc,D,T),_) => crLit(Lc,D,T).
  rewriteTerm(crApply(Lc,Op,Args,Tp),M) =>
    crApply(Lc,rewriteTerm(Op,M),Args//(A)=>rewriteTerm(A,M),Tp).
  rewriteTerm(crLet(Lc,V,B,E),M) where M1 .= dropVar(M,V) =>
    crLet(Lc,V,rewriteTerm(B,M1),rewriteTerm(E,M1)).
  rewriteTerm(crLam(Lc,Args,R),M) where M1 .= foldLeft(dropVar,M,Args) =>
    crLam(Lc,Args,rewriteTerm(R,M1)).
  rewriteTerm(crCase(Lc,Sel,Cases,Tp),M) =>
    crCase(Lc,rewriteTerm(Sel,M),Cases//(C)=>rewriteCase(C,M),Tp).
  rewriteTerm(crType(Lc,Tp),_)=>crType(Lc,Tp).
  rewriteTerm(crAbort(Lc,Tp),_)=>crAbort(Lc,Tp).

  dropVar:(map[string,crExp],crVar)=>map[string,crExp].
  dropVar(M,crId(Nm,_)) => M[\+Nm].

  rewriteVar:(locn,crVar,map[string,crExp])=>crExp.
  rewriteVar(_,crId(Nm,_),M) where T^=M[Nm] => T.
  rewriteVar(Lc,V,_) => crVar(Lc,V).

  rewriteCase:(crCase,map[string,crExp]) => crCase.
  rewriteCase((Lc,Ptn,Rp),M) => (Lc,rewriteTerm(Ptn,M),rewriteTerm(Rp,M)).
  
}
