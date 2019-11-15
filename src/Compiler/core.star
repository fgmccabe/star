star.compiler.core{
  import star.

  import star.compiler.location.
  import star.compiler.terms.
  import star.compiler.types.
  
  public crExp ::= crVar(locn,crVar)
    | crInt(locn,integer)
    | crFlot(locn,float)
    | crStrg(locn,string)
    | crLbl(locn,string,integer,tipe)
    | crApply(locn,crExp,list[crExp],tipe)
    | crCall(locn,crExp,list[crExp],tipe)
    | crECall(locn,string,list[crExp],tipe)
    | crOCall(locn,crExp,list[crExp],tipe)
    | crRecord(locn,string,list[(string,crExp)],tipe)
    | crDte(locn,crExp,string,tipe)
    | crTplDte(locn,crExp,integer,tipe)
    | crMtch(locn,crExp,crExp)
    | crCnj(locn,crExp,crExp)
    | crDsj(locn,crExp,crExp)
    | crNeg(locn,crExp)
    | crCnd(locn,crExp,crExp,crExp)
    | crLet(locn,crVar,crExp,crExp)
    | crLam(locn,list[crVar],crExp)
    | crCase(locn,crExp,list[crCase],crExp,tipe)
    | crAbort(locn,string,tipe)
    | crWhere(locn,crExp,crExp)
    | crMatch(locn,crExp,crExp).
  
  public crVar ::= crId(string,tipe).

  public crCase ~> (locn,crExp,crExp).

  public crDefn ::= fnDef(locn,string,tipe,list[crVar],crExp).

  public implementation display[crDefn] => {.
    disp(fnDef(Lc,Nm,Tp,Args,Rep)) =>
      ssSeq([ss("fun: "),ss(Nm),ss(":"),disp(Tp),ss("\n"),
	  ss(Nm),ss("("),ssSeq(interleave(Args//disp,ss(","))),ss(") => "),
	  disp(Rep)]).
  .}

  dspExp:(crExp,integer) => ss.
  dspExp(crVar(_,V),_) => disp(V).
  dspExp(crInt(_,Ix),_) => disp(Ix).
  dspExp(crFlot(_,Dx),_) => disp(Dx).
  dspExp(crStrg(_,Sx),_) => disp(Sx).
  dspExp(crLbl(_,Lb,Ar,_),_) => ssSeq([ss(Lb),ss("/"),disp(Ar)]).
  dspExp(crECall(_,Op,As,_),Off) => ssSeq([ss(Op),ss("("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crCall(_,Op,As,_),Off) => ssSeq([dspExp(Op,Off),ss("("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crApply(_,Op,As,_),Off) where isTplOp(Op) => ssSeq([ss("("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crApply(_,Op,As,_),Off) => ssSeq([dspExp(Op,Off),ss("("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crDte(_,O,Ix,_),Off) => ssSeq([dspExp(O,Off),ss("."),disp(Ix)]).
  dspExp(crTplDte(_,O,Ix,_),Off) => ssSeq([dspExp(O,Off),ss("."),disp(Ix)]).
  dspExp(crRecord(_,Path,Fs,_),Off) => ssSeq([ss(Path),ss("{"),ssSeq(dsplyFlds(Fs,Off+2)),ss("}")]).
  dspExp(crLet(_,V,E,I),Off) => ssSeq([ss("let "),disp(V),ss(" = "),dspExp(E,Off),ss(" in "),dspExp(I,Off)]).
  dspExp(crLam(_,Ps,R),Off) => ssSeq([ss("lambda"),ss("("),ssSeq(interleave(Ps//disp,ss(","))),ss(") => "),dspExp(R,Off)]).
  dspExp(crCase(_,E,Cs,Dflt,_),Off) => ssSeq([ss("case "),
      dspExp(E,Off),ss(" in {"),ssSeq(dspCases(Cs,Off+2)),ss("} else "),dspExp(Dflt,Off)]).
  dspExp(crMatch(_,P,E),Off) => ssSeq([dspExp(P,Off),ss(".="),dspExp(E,Off)]).
  dspExp(crWhere(_,T,C),Off) => ssSeq([dspExp(T,Off),ss(" where "), dspExp(C,Off+2)]).

  dspCases(Cs,Off) => let{
    Gap = ss(";\n").
  } in interleave(Cs//((_,P,V))=>ssSeq([dspExp(P,Off),ss("->"),dspExp(V,Off)]),Gap).

  dsplyExps(Es,Off) => let{
    Gap = ss(", ").
  } in interleave(Es//(E)=>dspExp(E,Off),Gap).

  dsplyFlds(Fs,Off) => let{
    Gap = ss(", ").
  } in interleave(Fs//((Nm,Vl))=>ssSeq([ss(Nm),ss("="),dspExp(Vl,Off)]),Gap).

  isTplOp(crLbl(_,Nm,_,_)) => isTplLbl(Nm).
  isTplOp(_) default => false.

  public mkCrTpl:(list[crExp],locn) => crExp.
  mkCrTpl(Args,Lc) => let{
    TpTp = tupleType(Args//typeOf).
    Ar = size(Args)
  } in crApply(Lc,crLbl(Lc,tplLbl(Ar),Ar,TpTp), Args, TpTp).

  public implementation equality[crVar] => {.
    crId(N1,T1) == crId(N2,T2) => N1==N2 && T1==T2.
  .}

  public implementation hash[crVar] => {.
    hash(crId(N,T)) => hash(N)*37+hash(T).
  .}

  public implementation hasLoc[crExp] => {
    locOf(crVar(Lc,_)) => Lc.
    locOf(crInt(Lc,_)) => Lc.
    locOf(crFlot(Lc,_)) => Lc.
    locOf(crStrg(Lc,_)) => Lc.
    locOf(crLbl(Lc,_,_,_)) => Lc.
    locOf(crDte(Lc,_,_,_)) => Lc.
    locOf(crTplDte(Lc,_,_,_)) => Lc.
    locOf(crApply(Lc,_,_,_)) => Lc.
    locOf(crWhere(Lc,_,_)) => Lc.
    locOf(crMatch(Lc,_,_)) => Lc.
    locOf(crLet(Lc,_,_,_)) => Lc.
    locOf(crLam(Lc,_,_)) => Lc.
    locOf(crCase(Lc,_,_,_,_)) => Lc.
  }

  public implementation hasType[crExp] => let{
    tpOf(crVar(_,V)) => typeOf(V).
    tpOf(crInt(_,_)) => intType.
    tpOf(crFlot(_,_)) => fltType.
    tpOf(crStrg(_,_)) => strType.
    tpOf(crLbl(_,_,_,Tp)) => Tp.
    tpOf(crApply(_,_,_,Tp)) => Tp.
    tpOf(crECall(_,_,_,Tp)) => Tp.
    tpOf(crCall(_,_,_,Tp)) => Tp.
    tpOf(crDte(_,_,_,Tp)) => Tp.
    tpOf(crTplDte(_,_,_,Tp)) => Tp.
    tpOf(crLet(_,_,_,E)) => tpOf(E).
    tpOf(crLam(_,Vs,E)) => funType(tupleType(Vs//typeOf),tpOf(E)).
    tpOf(crCase(_,_,_,_,Tp)) => Tp.
    tpOf(crWhere(_,T,_)) => tpOf(T).
    tpOf(crMatch(_,_,_)) => boolType.
    tpOf(crAbort(_,_,Tp)) => Tp.
  } in {
    typeOf = tpOf
  }

  public implementation hasType[crVar] => {.
    typeOf(crId(_,Tp)) => Tp.
  .}

  public implementation display[crExp] => let{
    dE(crVar(_,V)) => disp(V).
    dE(crInt(_,Ix)) => disp(Ix).
    dE(crFlot(_,Dx)) => disp(Dx).
    dE(crStrg(_,Sx)) => disp(Sx).
    dE(crLbl(_,Lb,Ar,_)) => ssSeq([ss(Lb),ss("/"),disp(Ar)]).
    dE(crECall(_,Op,As,_)) => ssSeq([ss(Op),ss("("),ssSeq(interleave(As//dE,ss(","))),ss(")")]).
    dE(crCall(_,Op,As,_)) => ssSeq([dE(Op),ss("("),ssSeq(interleave(As//dE,ss(","))),ss(")")]).
    dE(crApply(_,Op,As,_)) where isTplOp(Op) => ssSeq([ss("("),ssSeq(interleave(As//dE,ss(","))),ss(")")]).
    dE(crApply(_,Op,As,_)) => ssSeq([dE(Op),ss("("),ssSeq(interleave(As//dE,ss(","))),ss(")")]).
    dE(crDte(_,O,Ix,_)) => ssSeq([dE(O),ss("."),disp(Ix)]).
    dE(crTplDte(_,O,Ix,_)) => ssSeq([dE(O),ss("."),disp(Ix)]).
    dE(crRecord(_,Path,Fs,_)) => ssSeq([ss(Path),ss("{"),ssSeq(interleave(Fs//((Nm,Vl))=>ssSeq([ss(Nm),ss("="),dE(Vl)]),ss(". "))),ss("}")]).
    dE(crLet(_,V,E,I)) => ssSeq([ss("let "),disp(V),ss(" = "),dE(E),ss(" in "),dE(I)]).
    dE(crLam(_,Ps,R)) => ssSeq([ss("lambda"),ss("("),ssSeq(interleave(Ps//disp,ss(","))),ss(") => "),dE(R)]).
    dE(crCase(_,E,Cs,Dflt,_)) => ssSeq([ss("case "),dE(E),ss(" in {"),ssSeq(interleave(Cs//dCase,ss("; "))),ss("} else "),disp(Dflt)]).
    dE(crMatch(_,P,E)) => ssSeq([dE(P),ss(".="),dE(E)]).
    dE(crWhere(_,T,C)) => ssSeq([dE(T),ss(" where "), dE(C)]).

    dCase((_,P,E)) => ssSeq([dE(P),ss(" -> "),dE(E)]).

    isTplOp(crLbl(_,Nm,_,_)) => isTplLbl(Nm).
    isTplOp(_) default => false.
  } in {
    disp = dE
  }

  public implementation display[crVar] => {.
    disp(crId(Nm,_)) => ssSeq([ss("%"),ss(Nm)]).
  .}

  public rewriteTerm:(crExp,map[string,crExp])=>crExp.
  rewriteTerm(crVar(Lc,V),M) => rewriteVar(Lc,V,M).
  rewriteTerm(crInt(Lc,Ix),_) => crInt(Lc,Ix).
  rewriteTerm(crFlot(Lc,Dx),_) => crFlot(Lc,Dx).
  rewriteTerm(crStrg(Lc,Sx),_) => crStrg(Lc,Sx).
  rewriteTerm(crLbl(Lc,Sx,Ar,Tp),_) => crLbl(Lc,Sx,Ar,Tp).
  rewriteTerm(crDte(Lc,R,Ix,Tp),M) => crDte(Lc,R,Ix,Tp).
  rewriteTerm(crTplDte(Lc,R,Ix,Tp),M) => crTplDte(Lc,R,Ix,Tp).
  rewriteTerm(crApply(Lc,Op,Args,Tp),M) =>
    crApply(Lc,rewriteTerm(Op,M),Args//(A)=>rewriteTerm(A,M),Tp).
  rewriteTerm(crCall(Lc,Op,Args,Tp),M) =>
    crCall(Lc,rewriteTerm(Op,M),Args//(A)=>rewriteTerm(A,M),Tp).
  rewriteTerm(crECall(Lc,Op,Args,Tp),M) =>
    crECall(Lc,Op,Args//(A)=>rewriteTerm(A,M),Tp).
  rewriteTerm(crLet(Lc,V,B,E),M) where M1 .= dropVar(M,V) =>
    crLet(Lc,V,rewriteTerm(B,M1),rewriteTerm(E,M1)).
  rewriteTerm(crLam(Lc,Args,R),M) where M1 .= foldLeft(dropVar,M,Args) =>
    crLam(Lc,Args,rewriteTerm(R,M1)).
  rewriteTerm(crCase(Lc,Sel,Cases,Deflt,Tp),M) =>
    crCase(Lc,rewriteTerm(Sel,M),Cases//(C)=>rewriteCase(C,M),rewriteTerm(Deflt,M),Tp).
  rewriteTerm(crAbort(Lc,Nm,Tp),_)=>crAbort(Lc,Nm,Tp).
  rewriteTerm(crWhere(Lc,T,C),M) =>
    crWhere(Lc,rewriteTerm(T,M),rewriteTerm(C,M)).
  rewriteTerm(crMatch(Lc,P,E),M) =>
    crMatch(Lc,rewriteTerm(P,M),rewriteTerm(E,M)).
  rewriteTerm(crLet(Lc,V,B,E),M) where M1 .= dropVar(M,V) =>
    crLet(Lc,V,rewriteTerm(B,M1),rewriteTerm(E,M1)).

  dropVar:(map[string,crExp],crVar)=>map[string,crExp].
  dropVar(M,crId(Nm,_)) => M[\+Nm].

  rewriteVar:(locn,crVar,map[string,crExp])=>crExp.
  rewriteVar(_,crId(Nm,_),M) where T^=M[Nm] => T.
  rewriteVar(Lc,V,_) => crVar(Lc,V).

  rewriteCase:(crCase,map[string,crExp]) => crCase.
  rewriteCase((Lc,Ptn,Rp),M) => (Lc,rewriteTerm(Ptn,M),rewriteTerm(Rp,M)).
}
