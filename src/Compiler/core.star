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
    | crTerm(locn,crExp,list[crExp],tipe)
    | crCall(locn,crExp,list[crExp],tipe)
    | crECall(locn,string,list[crExp],tipe)
    | crOCall(locn,crExp,list[crExp],tipe)
    | crRecord(locn,string,list[(string,crExp)],tipe)
    | crDte(locn,crExp,string,tipe)
    | crTplDte(locn,crExp,integer,tipe)
    | crCnj(locn,crExp,crExp)
    | crDsj(locn,crExp,crExp)
    | crNeg(locn,crExp)
    | crCnd(locn,crExp,crExp,crExp)
    | crLtt(locn,crDefn,crExp)
    | crCase(locn,crExp,list[crCase],crExp,tipe)
    | crAbort(locn,string,tipe)
    | crWhere(locn,crExp,crExp)
    | crMatch(locn,crExp,crExp).
  
  public crVar ::= crId(string,tipe).

  public crCase ~> (locn,crExp,crExp).

  public crDefn ::= fnDef(locn,string,tipe,list[crVar],crExp) | vrDef(locn,crVar,crExp).

  public defVar:(crDefn)=>crVar.
  defVar(fnDef(_,Nm,Tp,_,_))=>crId(Nm,Tp).
  defVar(vrDef(_,V,_)) => V.

  public implementation display[crDefn] => {.
    disp(Df) => dspDef(Df,"  ").
  .}

  dspDef:(crDefn,string) => ss.
  dspDef(fnDef(Lc,Nm,Tp,Args,Rep),Off) =>
    ssSeq([ss("fun: "),ss(Nm),ss("("),
	ssSeq(interleave(Args//disp,ss(","))),ss(") => "),
	dspExp(Rep,Off)]).
  dspDef(vrDef(Lc,Vr,Vl),Off) =>
      ssSeq([ss("var: "), disp(Vr),ss("="),dspExp(Vl,Off)]).

  dspExp:(crExp,string) => ss.
  dspExp(crVar(_,V),_) => disp(V).
  dspExp(crInt(_,Ix),_) => disp(Ix).
  dspExp(crFlot(_,Dx),_) => disp(Dx).
  dspExp(crStrg(_,Sx),_) => disp(Sx).
  dspExp(crLbl(_,Lb,Ar,_),_) => ssSeq([ss(Lb),ss("/"),disp(Ar)]).
  dspExp(crECall(_,Op,As,_),Off) => ssSeq([ss(Op),ss("("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crOCall(_,Op,As,_),Off) => ssSeq([dspExp(Op,Off),ss(".("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crCall(_,Op,As,_),Off) => ssSeq([dspExp(Op,Off),ss("("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crTerm(_,Op,As,_),Off) where isTplOp(Op) => ssSeq([ss("("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crTerm(_,Op,As,_),Off) => ssSeq([dspExp(Op,Off),ss("("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crDte(_,O,Ix,_),Off) => ssSeq([dspExp(O,Off),ss("."),disp(Ix)]).
  dspExp(crTplDte(_,O,Ix,_),Off) => ssSeq([dspExp(O,Off),ss("."),disp(Ix)]).
  dspExp(crRecord(_,Path,Fs,_),Off) => ssSeq([ss(Path),ss("{"),ssSeq(dsplyFlds(Fs,Off++"  ")),ss("}")]).
  dspExp(crLtt(_,D,I),Off) where Off2.=Off++"  " => ssSeq([ss("let ("),dspDef(D,Off2),ss(") in\n"),ss(Off2),dspExp(I,Off2)]).
  dspExp(crCase(_,E,Cs,Dflt,_),Off) where Off2.=Off++"  "=> ssSeq([ss("case "),
      dspExp(E,Off),ss(" in { "),ssSeq(dspCases(Cs,Off2)),ss("} else "),dspExp(Dflt,Off2)]).
  dspExp(crMatch(_,P,E),Off) => ssSeq([dspExp(P,Off),ss(".="),dspExp(E,Off)]).
  dspExp(crWhere(_,T,C),Off) => ssSeq([dspExp(T,Off),ss(" where "), dspExp(C,Off++"  ")]).
  dspExp(crCnj(_,L,R),Off) => ssSeq([dspExp(L,Off),ss("&&"),dspExp(R,Off)]).
  dspExp(crDsj(_,L,R),Off) => ssSeq([ss("("),dspExp(L,Off),ss("||"),dspExp(R,Off),ss(")")]).
  dspExp(crCnd(_,T,L,R),Off) where Off2 .= Off++"  " =>
    ssSeq([ss("("),dspExp(T,Off),ss("? "),dspExp(L,Off2),ss(" ||\n"),ss(Off2),dspExp(R,Off2),ss(")")]).
  dspExp(crNeg(_,R),Off) => ssSeq([ss("\\+"),dspExp(R,Off)]).

  dspCases(Cs,Off) => let{
    Gap = ss(";\n"++Off).
  } in interleave(Cs//((_,P,V))=>ssSeq([dspExp(P,Off),ss("->"),dspExp(V,Off)]),Gap).

  dsplyExps(Es,Off) => let{
    Gap = ss(", ").
  } in interleave(Es//(E)=>dspExp(E,Off),Gap).

  dsplyFlds(Fs,Off) => let{
    Gap = ss(",\n"++Off).
  } in interleave(Fs//((Nm,Vl))=>ssSeq([ss(Nm),ss(" = "),dspExp(Vl,Off)]),Gap).

  isTplOp(crLbl(_,Nm,_,_)) => isTplLbl(Nm).
  isTplOp(_) default => false.

  public mkCrTpl:(list[crExp],locn) => crExp.
  mkCrTpl(Args,Lc) => let{
    TpTp = tupleType(Args//typeOf).
    Ar = size(Args)
  } in crTerm(Lc,crLbl(Lc,tplLbl(Ar),Ar,TpTp), Args, TpTp).

  public implementation equality[crVar] => {.
    crId(N1,T1) == crId(N2,T2) => N1==N2 && T1==T2.
  .}

  public implementation hash[crVar] => {.
    hash(crId(N,T)) => hash(N).
  .}

  public implementation hasLoc[crExp] => {
    locOf(crVar(Lc,_)) => Lc.
    locOf(crInt(Lc,_)) => Lc.
    locOf(crFlot(Lc,_)) => Lc.
    locOf(crStrg(Lc,_)) => Lc.
    locOf(crLbl(Lc,_,_,_)) => Lc.
    locOf(crDte(Lc,_,_,_)) => Lc.
    locOf(crTplDte(Lc,_,_,_)) => Lc.
    locOf(crTerm(Lc,_,_,_)) => Lc.
    locOf(crWhere(Lc,_,_)) => Lc.
    locOf(crMatch(Lc,_,_)) => Lc.
    locOf(crLtt(Lc,_,_)) => Lc.
    locOf(crCase(Lc,_,_,_,_)) => Lc.
  }

  public implementation hasType[crExp] => let{
    tpOf(crVar(_,V)) => typeOf(V).
    tpOf(crInt(_,_)) => intType.
    tpOf(crFlot(_,_)) => fltType.
    tpOf(crStrg(_,_)) => strType.
    tpOf(crLbl(_,_,_,Tp)) => Tp.
    tpOf(crTerm(_,_,_,Tp)) => Tp.
    tpOf(crECall(_,_,_,Tp)) => Tp.
    tpOf(crOCall(_,_,_,Tp)) => Tp.
    tpOf(crCall(_,_,_,Tp)) => Tp.
    tpOf(crDte(_,_,_,Tp)) => Tp.
    tpOf(crTplDte(_,_,_,Tp)) => Tp.
    tpOf(crLtt(_,_,E)) => tpOf(E).
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

  public implementation display[crExp] => {
    disp(T) => dspExp(T,"")
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
  rewriteTerm(crTerm(Lc,Op,Args,Tp),M) =>
    crTerm(Lc,rewriteTerm(Op,M),Args//(A)=>rewriteTerm(A,M),Tp).
  rewriteTerm(crCall(Lc,Op,Args,Tp),M) =>
    crCall(Lc,rewriteTerm(Op,M),Args//(A)=>rewriteTerm(A,M),Tp).
  rewriteTerm(crECall(Lc,Op,Args,Tp),M) =>
    crECall(Lc,Op,Args//(A)=>rewriteTerm(A,M),Tp).
  rewriteTerm(crLtt(Lc,D,E),M) where M1 .= dropDefVar(M,D) =>
    crLtt(Lc,rewriteDef(D,M1),rewriteTerm(E,M1)).
  rewriteTerm(crCase(Lc,Sel,Cases,Deflt,Tp),M) =>
    crCase(Lc,rewriteTerm(Sel,M),Cases//(C)=>rewriteCase(C,M),rewriteTerm(Deflt,M),Tp).
  rewriteTerm(crAbort(Lc,Nm,Tp),_)=>crAbort(Lc,Nm,Tp).
  rewriteTerm(crWhere(Lc,T,C),M) =>
    crWhere(Lc,rewriteTerm(T,M),rewriteTerm(C,M)).
  rewriteTerm(crMatch(Lc,P,E),M) =>
    crMatch(Lc,rewriteTerm(P,M),rewriteTerm(E,M)).

  rewriteDef(fnDef(Lc,Nm,Tp,Args,Val),M) =>
    fnDef(Lc,Nm,Tp,Args,rewriteTerm(Val,M)).

  dropDefVar(M,fnDef(_,Nm,_,_,_)) => M[\+Nm].
  dropDefVar(M,vrDef(_,crId(Nm,_),_)) => M[\+Nm].

  dropVar:(map[string,crExp],crVar)=>map[string,crExp].
  dropVar(M,crId(Nm,_)) => M[\+Nm].

  rewriteVar:(locn,crVar,map[string,crExp])=>crExp.
  rewriteVar(_,crId(Nm,_),M) where T^=M[Nm] => T.
  rewriteVar(Lc,V,_) => crVar(Lc,V).

  rewriteCase:(crCase,map[string,crExp]) => crCase.
  rewriteCase((Lc,Ptn,Rp),M) => (Lc,rewriteTerm(Ptn,M),rewriteTerm(Rp,M)).

  public implementation hasLoc[crDefn] => {
    locOf(fnDef(Lc,_,_,_,_)) => Lc.
    locOf(vrDef(Lc,_,_)) => Lc.
  }

  public crName:(crVar) => string.
  crName(crId(Nm,_))=>Nm.

  public isCrCond:(crExp)=>boolean.
  isCrCond(crCnj(_,_,_))=>true.
  isCrCond(crDsj(_,_,_))=>true.
  isCrCond(crNeg(_,_))=>true.
  isCrCond(crCnd(_,_,L,R))=>isCrCond(L)||isCrCond(R).
  isCrCond(crWhere(_,L,_)) => isCrCond(L).
  isCrCond(crMatch(_,_,_))=>true.
  isCrCond(_) default => false.

}
