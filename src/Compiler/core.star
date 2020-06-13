star.compiler.core{
  import star.

  import star.compiler.assem.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.terms.
  import star.compiler.types.
  import star.pkg.
  
  public crExp ::= crVar(locn,crVar)
    | crInt(locn,integer)
    | crFlot(locn,float)
    | crStrg(locn,string)
    | crLbl(locn,string,tipe)
    | crTerm(locn,string,cons[crExp],tipe)
    | crMemo(locn,crExp,tipe)
    | crMemoGet(locn,crExp,tipe)
    | crMemoSet(locn,crExp,crExp)
    | crCall(locn,string,cons[crExp],tipe)
    | crECall(locn,string,cons[crExp],tipe)
    | crIntrinsic(locn,assemOp,cons[crExp],tipe)
    | crOCall(locn,crExp,cons[crExp],tipe)
    | crRecord(locn,string,cons[(string,crExp)],tipe)
    | crDot(locn,crExp,string,tipe)
    | crTplOff(locn,crExp,integer,tipe)
    | crCnj(locn,crExp,crExp)
    | crDsj(locn,crExp,crExp)
    | crNeg(locn,crExp)
    | crCnd(locn,crExp,crExp,crExp)
    | crLtt(locn,crVar,crExp,crExp)
    | crLtRec(locn,crVar,cons[crExp],crExp)
    | crCase(locn,crExp,cons[crCase],crExp,tipe)
    | crAbort(locn,string,tipe)
    | crWhere(locn,crExp,crExp)
    | crMatch(locn,crExp,crExp).
  
  public crVar ::= crId(string,tipe).

  public crCase ~> (locn,crExp,crExp).

  public crDefn ::= fnDef(locn,string,tipe,cons[crVar],crExp) |
    glbDef(locn,crVar,crExp) |
    rcDef(locn,string,tipe,cons[(string,tipe,integer)]).

  public dispCrProg:(cons[crDefn])=>ss.
  dispCrProg(Defs) => ssSeq(interleave(Defs//disp,ss(".\n"))).

  public implementation display[crDefn] => {.
    disp(Df) => dspDef(Df,"  ").
  .}

  dspDef:(crDefn,string) => ss.
  dspDef(fnDef(Lc,Nm,Tp,Args,Rep),Off) =>
    ssSeq([ss("fun: "),disp(Lc),ss("\n"), 
	ss(Nm),ss("("),
	ssSeq(interleave(Args//disp,ss(","))),ss(") => "),
	dspExp(Rep,Off)]).
  dspDef(glbDef(Lc,V,Rep),Off) =>
    ssSeq([ss("glb: "),disp(Lc),ss("\n"),
	disp(V),/*ss(":"),disp(typeOf(V)),*/ss("="),
	dspExp(Rep,Off)]).
  dspDef(rcDef(Lc,Nm,Tp,Fields),Off) where Off2 .= Off++"  " =>
    ssSeq([ss("rec: "),disp(Lc),ss("\n"),
	ss(Nm),ss(":"),disp(Tp),ss("\n"),ss(Off),
	ss(Nm),ss("{\n"),ss(Off2),
	ssSeq(interleave(Fields//((FNm,FTp,FIx))=>ssSeq([ss(FNm),ss(":"),disp(FTp),ss("@"),disp(FIx)]),ss(";\n"++Off2))),
	ss("}")]).

  dspExp:(crExp,string) => ss.
  dspExp(crVar(_,V),_) => disp(V).
  dspExp(crInt(_,Ix),_) => disp(Ix).
  dspExp(crFlot(_,Dx),_) => disp(Dx).
  dspExp(crStrg(_,Sx),_) => disp(Sx).
  dspExp(crLbl(_,Lb,_),_) => ssSeq([ss("."),ss(Lb)]).
  dspExp(crECall(_,Op,As,_),Off) => ssSeq([ss(Op),ss("("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crIntrinsic(_,Op,As,_),Off) => ssSeq([ss("intrinsic{"),disp(Op),ss("("),ssSeq(dsplyExps(As,Off)),ss(")}")]).
  dspExp(crOCall(_,Op,As,_),Off) => ssSeq([dspExp(Op,Off),ss("·("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crCall(_,Op,As,_),Off) => ssSeq([ss(Op),ss("("),ssSeq(dsplyExps(As,Off)),ss(")")]).
  dspExp(crTerm(_,Op,As,_),Off) where isTplLbl(Op) => ssSeq([ss("‹"),ssSeq(dsplyExps(As,Off)),ss("›")]).
  dspExp(crTerm(_,Op,As,_),Off) => ssSeq([ss(Op),ss("‹"),ssSeq(dsplyExps(As,Off)),ss("›")]).
  dspExp(crMemo(_,Gen,_),Off) => ssSeq([ss("memo‹"),dspExp(Gen,Off),ss("›")]).
  dspExp(crMemoGet(_,Memo,_),Off) => ssSeq([ss("getmemo‹"),dspExp(Memo,Off),ss("›")]).
  dspExp(crMemoSet(_,Memo,Vl),Off) => ssSeq([ss("setmemo‹"),dspExp(Memo,Off),ss(":="),
      dspExp(Vl,Off),ss("›")]).
  dspExp(crDot(_,O,Ix,_),Off) => ssSeq([dspExp(O,Off),ss("."),disp(Ix)]).
  dspExp(crTplOff(_,O,Ix,_),Off) => ssSeq([dspExp(O,Off),ss("."),disp(Ix)]).
  dspExp(crRecord(_,Path,Fs,_),Off) => ssSeq([ss(Path),ss("{"),ssSeq(dsplyFlds(Fs,Off++"  ")),ss("}")]).
  dspExp(crLtt(_,V,D,I),Off) where Off2.=Off++"  " =>
    ssSeq([ss("let "),disp(V),ss(" = "),dspExp(D,Off2),ss(" in\n"),ss(Off2),dspExp(I,Off2)]).
  dspExp(crLtRec(_,V,D,I),Off) where Off2.=Off++"  " =>
    ssSeq([ss("let rec "),disp(V),ss(" = ["),ssSeq(dsplyExps(D,Off2)),ss("] in\n"),ss(Off2),dspExp(I,Off2)]).
  dspExp(crCase(_,E,Cs,Dflt,_),Off) where Off2.=Off++"  "=> ssSeq([ss("case "),
      dspExp(E,Off),ss(" in { "),ssSeq(dspCases(Cs,Off2)),ss("} else "),dspExp(Dflt,Off2)]).
  dspExp(crMatch(_,P,E),Off) => ssSeq([dspExp(P,Off),ss(".="),dspExp(E,Off)]).
  dspExp(crWhere(_,T,C),Off) => ssSeq([dspExp(T,Off),ss(" where "), dspExp(C,Off++"  ")]).
  dspExp(crCnj(_,L,R),Off) => ssSeq([dspExp(L,Off),ss("&&"),dspExp(R,Off)]).
  dspExp(crDsj(_,L,R),Off) => ssSeq([ss("("),dspExp(L,Off),ss("||"),dspExp(R,Off),ss(")")]).
  dspExp(crCnd(_,T,L,R),Off) where Off2 .= Off++"  " =>
    ssSeq([ss("("),dspExp(T,Off),ss("? "),dspExp(L,Off2),ss(" ||\n"),ss(Off2),dspExp(R,Off2),ss(")")]).
  dspExp(crNeg(_,R),Off) => ssSeq([ss("~"),dspExp(R,Off)]).
  dspExp(crAbort(_,Msg,_),Off) => ssSeq([ss("abort "),disp(Msg)]).

  dspCases(Cs,Off) => let{
    Gap = ss(";\n"++Off).
  } in interleave(Cs//((_,P,V))=>ssSeq([dspExp(P,Off),ss("->"),dspExp(V,Off)]),Gap).

  dsplyExps(Es,Off) => let{
    Gap = ss(", ").
  } in interleave(Es//(E)=>dspExp(E,Off),Gap).

  dsplyFlds(Fs,Off) => let{
    Gap = ss(",\n"++Off).
  } in interleave(Fs//((Nm,Vl))=>ssSeq([ss(Nm),ss(" = "),dspExp(Vl,Off)]),Gap).

  isTplOp(crLbl(_,Nm,_)) => isTplLbl(Nm).
  isTplOp(_) default => .false.

  public mkCrTpl:(locn,cons[crExp]) => crExp.
  mkCrTpl(Lc,Args) => let{
    TpTp = tupleType(Args//typeOf).
    Ar = size(Args)
  } in crTerm(Lc,tplLbl(Ar), Args, TpTp).

  public implementation equality[crVar] => {.
    crId(N1,T1) == crId(N2,T2) => N1==N2.
  .}

  public implementation hash[crVar] => {.
    hash(crId(N,T)) => hash(N).
  .}

  public implementation equality[crExp] => let{
    eqTerm(crVar(_,V1),crVar(_,V2)) => V1==V2.
    eqTerm(crInt(_,N1),crInt(_,N2)) => N1==N2.
    eqTerm(crFlot(_,N1),crFlot(_,N2)) => N1==N2.
    eqTerm(crStrg(_,S1),crStrg(_,S2)) => S1==S2.
    eqTerm(crLbl(_,S1,_),crLbl(_,S2,_)) => S1==S2.
    eqTerm(crTerm(_,S1,A1,_),crTerm(_,S2,A2,_)) => S1==S2 && eqs(A1,A2).
    eqTerm(crMemo(_,G1,_),crMemo(_,G2,_)) => eqTerm(G1,G2).
    eqTerm(crCall(_,S1,A1,_),crCall(_,S2,A2,_)) => S1==S2 && eqs(A1,A2).
    eqTerm(crECall(_,S1,A1,_),crECall(_,S2,A2,_)) => S1==S2 && eqs(A1,A2).
--    eqTerm(crIntrinsic(_,S1,A1,_),crIntrinsic(_,S2,A2,_)) => S1==S2 && eqs(A1,A2).
    eqTerm(crOCall(_,S1,A1,_),crOCall(_,S2,A2,_)) => eqTerm(S1,S2) && eqs(A1,A2).
    eqTerm(crRecord(_,S1,F1,_),crRecord(_,S2,F2,_)) => S1==S2 && eqFs(F1,F2).
    eqTerm(crDot(_,R1,F1,_),crDot(_,R2,F2,_)) => eqTerm(R1,R2) && F1==F2.
    eqTerm(crTplOff(_,R1,F1,_),crTplOff(_,R2,F2,_)) => eqTerm(R1,R2) && F1==F2.
    eqTerm(crCnj(_,L1,R1),crCnj(_,L2,R2)) => eqTerm(L1,L2) && eqTerm(R1,R2).
    eqTerm(crDsj(_,L1,R1),crDsj(_,L2,R2)) => eqTerm(L1,L2) && eqTerm(R1,R2).
    eqTerm(crNeg(_,R1),crNeg(_,R2)) => eqTerm(R1,R2).
    eqTerm(crCnd(_,T1,L1,R1),crCnd(_,T2,L2,R2)) =>
      eqTerm(T1,T2) && eqTerm(L1,L2) && eqTerm(R1,R2).
    eqTerm(crLtt(_,T1,L1,R1),crLtt(_,T2,L2,R2)) =>
      T1==T2 && eqTerm(L1,L2) && eqTerm(R1,R2).
    eqTerm(crLtRec(_,T1,L1,R1),crLtRec(_,T2,L2,R2)) =>
      T1==T2 && eqs(L1,L2) && eqTerm(R1,R2).
    eqTerm(crCase(_,S1,C1,D1,_),crCase(_,S2,C2,D2,_)) =>
      eqTerm(S1,S2) && eqCs(C1,C2) && eqTerm(D1,D2).
    eqTerm(crAbort(_,V1,_),crAbort(_,V2,_)) => V1==V2.
    eqTerm(crWhere(_,E1,C1),crWhere(_,E2,C2)) => eqTerm(E1,E2) && eqTerm(C1,C2).
    eqTerm(crMatch(_,P1,E1),crMatch(_,P2,E2)) => eqTerm(E1,E2) && eqTerm(P1,P2).
    eqTerm(_,_) default => .false.

    eqs([],[]) => .true.
    eqs([E1,..S1],[E2,..S2]) => eqTerm(E1,E2) && eqs(S1,S2).
    eqs(_,_) default => .false.

    eqFs([],[]) => .true.
    eqFs([(N1,E1),..S1],[(N2,E2),..S2]) => N1==N2 && eqTerm(E1,E2) && eqFs(S1,S2).
    eqFs(_,_) default => .false.

    eqCs([],[]) => .true.
    eqCs([(_,N1,E1),..S1],[(_,N2,E2),..S2]) => eqTerm(N1,N2) && eqTerm(E1,E2) && eqCs(S1,S2).
    eqCs(_,_) default => .false.
  } in {.
    X == Y => eqTerm(X,Y)
  .}

  public implementation hasLoc[crExp] => {
    locOf(crVar(Lc,_)) => Lc.
    locOf(crInt(Lc,_)) => Lc.
    locOf(crFlot(Lc,_)) => Lc.
    locOf(crStrg(Lc,_)) => Lc.
    locOf(crLbl(Lc,_,_)) => Lc.
    locOf(crDot(Lc,_,_,_)) => Lc.
    locOf(crTplOff(Lc,_,_,_)) => Lc.
    locOf(crTerm(Lc,_,_,_)) => Lc.
    locOf(crMemo(Lc,_,_)) => Lc.
    locOf(crWhere(Lc,_,_)) => Lc.
    locOf(crMatch(Lc,_,_)) => Lc.
    locOf(crLtt(Lc,_,_,_)) => Lc.
    locOf(crLtRec(Lc,_,_,_)) => Lc.
    locOf(crCase(Lc,_,_,_,_)) => Lc.
    locOf(crCall(Lc,_,_,_))=>Lc.
    locOf(crIntrinsic(Lc,_,_,_))=>Lc.
    locOf(crECall(Lc,_,_,_))=>Lc.
    locOf(crOCall(Lc,_,_,_))=>Lc.
    locOf(crRecord(Lc,_,_,_)) => Lc.
    locOf(crCnj(Lc,_,_)) => Lc.
    locOf(crDsj(Lc,_,_)) => Lc.
    locOf(crNeg(Lc,_)) => Lc.
    locOf(crCnd(Lc,_,_,_)) => Lc.
  }

  public implementation hasType[crExp] => let{
    tpOf(crVar(_,V)) => typeOf(V).
    tpOf(crInt(_,_)) => intType.
    tpOf(crFlot(_,_)) => fltType.
    tpOf(crStrg(_,_)) => strType.
    tpOf(crLbl(_,_,Tp)) => Tp.
    tpOf(crTerm(_,_,_,Tp)) => Tp.
    tpOf(crMemo(_,_,Tp)) => Tp.
    tpOf(crMemoGet(_,_,Tp)) => Tp.
    tpOf(crMemoSet(_,_,Vl)) => tpOf(Vl).
    tpOf(crRecord(_,_,_,Tp)) => Tp.
    tpOf(crIntrinsic(_,_,_,Tp)) => Tp.
    tpOf(crECall(_,_,_,Tp)) => Tp.
    tpOf(crOCall(_,_,_,Tp)) => Tp.
    tpOf(crCall(_,_,_,Tp)) => Tp.
    tpOf(crDot(_,_,_,Tp)) => Tp.
    tpOf(crTplOff(_,_,_,Tp)) => Tp.
    tpOf(crLtt(_,_,_,E)) => tpOf(E).
    tpOf(crLtRec(_,_,_,E)) => tpOf(E).
    tpOf(crCase(_,_,_,_,Tp)) => Tp.
    tpOf(crCnd(_,_,L,_)) => tpOf(L).
    tpOf(crWhere(_,T,_)) => tpOf(T).
    tpOf(crMatch(_,_,_)) => boolType.
    tpOf(crCnj(_,_,_)) => boolType.
    tpOf(crDsj(_,_,_)) => boolType.
    tpOf(crNeg(_,_)) => boolType.
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

  public implementation coercion[crExp,option[term]] => {
    _coerce(crInt(_,Ix)) => some(intgr(Ix)).
    _coerce(crFlot(_,Dx)) => some(flot(Dx)).
    _coerce(crStrg(_,Sx)) => some(strg(Sx)).
    _coerce(crInt(_,Ix)) => some(intgr(Ix)).
    _coerce(crLbl(_,Nm,_)) => some(enum(tLbl(Nm,0))).
    _coerce(crTerm(_,Nm,[],_)) => some(term(tLbl(Nm,0),[])).
    _coerce(crTerm(_,Nm,Args,_)) where NArgs ^= mapArgs(Args,[]) =>
      some(term(tLbl(Nm,size(Args)),NArgs)).
    _coerce(_) default => .none.

    private mapArgs([],So) => some(reverse(So)).
    mapArgs([A,..As],So) where NA^=_coerce(A) => mapArgs(As,[NA,..So]).
    mapArgs(_,_) default => .none.
  }

  public implementation coercion[locn,crExp] => {.
    _coerce(Lc) where locn(Nm,Line,Col,Off,Len).=Lc =>
      mkCrTpl(Lc,[crStrg(Lc,Nm),crInt(Lc,Line),crInt(Lc,Col),crInt(Lc,Off),crInt(Lc,Len)])
  .}

  public rewriteTerm:(crExp,map[string,crExp])=>crExp.
  rewriteTerm(crVar(Lc,V),M) => rewriteVar(Lc,V,M).
  rewriteTerm(crInt(Lc,Ix),_) => crInt(Lc,Ix).
  rewriteTerm(crFlot(Lc,Dx),_) => crFlot(Lc,Dx).
  rewriteTerm(crStrg(Lc,Sx),_) => crStrg(Lc,Sx).
  rewriteTerm(crLbl(Lc,Sx,Tp),_) => crLbl(Lc,Sx,Tp).
  rewriteTerm(crDot(Lc,R,Ix,Tp),M) => crDot(Lc,rewriteTerm(R,M),Ix,Tp).
  rewriteTerm(crTplOff(Lc,R,Ix,Tp),M) => crTplOff(Lc,rewriteTerm(R,M),Ix,Tp).
  rewriteTerm(crTerm(Lc,Op,Args,Tp),M) =>
    crTerm(Lc,Op,rewriteTerms(Args,M),Tp).
  rewriteTerm(crMemo(Lc,Gen,Tp),M) =>
    crMemo(Lc,rewriteTerm(Gen,M),Tp).
  rewriteTerm(crMemoGet(Lc,Mem,Tp),M) =>
    crMemoGet(Lc,rewriteTerm(Mem,M),Tp).
  rewriteTerm(crMemoSet(Lc,Mem,Val),M) =>
    crMemoSet(Lc,rewriteTerm(Mem,M),rewriteTerm(Val,M)).
  rewriteTerm(crRecord(Lc,Op,Flds,Tp),M) =>
    crRecord(Lc,Op,Flds//((F,T))=>(F,rewriteTerm(T,M)),Tp).
  rewriteTerm(crCall(Lc,Op,Args,Tp),M) =>
    crCall(Lc,Op,Args//(A)=>rewriteTerm(A,M),Tp).
  rewriteTerm(crOCall(Lc,Op,Args,Tp),M) =>
    crOCall(Lc,rewriteTerm(Op,M),Args//(A)=>rewriteTerm(A,M),Tp).
  rewriteTerm(crIntrinsic(Lc,Op,Args,Tp),M) =>
    crIntrinsic(Lc,Op,rewriteTerms(Args,M),Tp).
  rewriteTerm(crECall(Lc,Op,Args,Tp),M) =>
    crECall(Lc,Op,Args//(A)=>rewriteTerm(A,M),Tp).
  rewriteTerm(crCnj(Lc,L,R),M) =>
    crCnj(Lc,rewriteTerm(L,M),rewriteTerm(R,M)).
  rewriteTerm(crDsj(Lc,L,R),M) =>
    crDsj(Lc,rewriteTerm(L,M),rewriteTerm(R,M)).
  rewriteTerm(crNeg(Lc,R),M) =>
    crNeg(Lc,rewriteTerm(R,M)).
  rewriteTerm(crCnd(Lc,T,L,R),M) =>
    crCnd(Lc,rewriteTerm(T,M),rewriteTerm(L,M),rewriteTerm(R,M)).
  rewriteTerm(crLtt(Lc,V,D,E),M) where M1 .= dropVar(M,V) =>
    crLtt(Lc,V,rewriteTerm(D,M),rewriteTerm(E,M1)).
  rewriteTerm(crLtRec(Lc,V,D,E),M) where M1 .= dropVar(M,V) =>
    crLtRec(Lc,V,rewriteTerms(D,M),rewriteTerm(E,M)).
  rewriteTerm(crCase(Lc,Sel,Cases,Deflt,Tp),M) =>
    crCase(Lc,rewriteTerm(Sel,M),Cases//(C)=>rewriteCase(C,M),rewriteTerm(Deflt,M),Tp).
  rewriteTerm(crAbort(Lc,Nm,Tp),_)=>crAbort(Lc,Nm,Tp).
  rewriteTerm(crWhere(Lc,T,C),M) =>
    crWhere(Lc,rewriteTerm(T,M),rewriteTerm(C,M)).
  rewriteTerm(crMatch(Lc,P,E),M) =>
    crMatch(Lc,rewriteTerm(P,M),rewriteTerm(E,M)).

  public rewriteTerms:(cons[crExp],map[string,crExp])=>cons[crExp].
  rewriteTerms(Els,Mp) => (Els//(E)=>rewriteTerm(E,Mp)).

  rewriteDef(fnDef(Lc,Nm,Tp,Args,Val),M) =>
    fnDef(Lc,Nm,Tp,Args,rewriteTerm(Val,M)).
  rewriteDef(glbDef(Lc,V,Val),M) =>
    glbDef(Lc,V,rewriteTerm(Val,M)).

  dropVar:(map[string,crExp],crVar)=>map[string,crExp].
  dropVar(M,crId(Nm,_)) => M[~Nm].

  rewriteVar:(locn,crVar,map[string,crExp])=>crExp.
  rewriteVar(_,crId(Nm,_),M) where T^=M[Nm] => T.
  rewriteVar(Lc,V,_) => crVar(Lc,V).

  rewriteCase:(crCase,map[string,crExp]) => crCase.
  rewriteCase((Lc,Ptn,Rp),M) => (Lc,rewriteTerm(Ptn,M),rewriteTerm(Rp,M)).

  public implementation hasLoc[crDefn] => {
    locOf(fnDef(Lc,_,_,_,_)) => Lc.
    locOf(glbDef(Lc,_,_)) => Lc.
  }

  public crName:(crVar) => string.
  crName(crId(Nm,_))=>Nm.

  public isCrCond:(crExp)=>boolean.
  isCrCond(crCnj(_,_,_))=>.true.
  isCrCond(crDsj(_,_,_))=>.true.
  isCrCond(crNeg(_,_))=>.true.
  isCrCond(crCnd(_,_,L,R))=>isCrCond(L)||isCrCond(R).
  isCrCond(crWhere(_,L,_)) => isCrCond(L).
  isCrCond(crMatch(_,_,_))=>.true.
  isCrCond(_) default => .false.

  public isGround:(crExp) => boolean.
  isGround(crInt(_,_)) => .true.
  isGround(crFlot(_,_)) => .true.
  isGround(crStrg(_,_)) => .true.
  isGround(crLbl(_,_,_)) => .true.
  isGround(crTerm(_,_,Els,_)) => E in Els *> isGround(E).
  isGround(_) default => .false.

}
