star.compiler.core{
  import star.

  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.terms.
  import star.compiler.types.
  import star.pkg.
  
  public crExp ::= crVar(locn,crVar)
    | crInt(locn,integer)
    | crBig(locn,bigint)
    | crFlot(locn,float)
    | crStrg(locn,string)
    | crVoid(locn,tipe)
    | crLbl(locn,string,tipe)
    | crTerm(locn,string,cons[crExp],tipe)
    | crCall(locn,string,cons[crExp],tipe)
    | crECall(locn,string,cons[crExp],tipe)
    | crIntrinsic(locn,string,cons[crExp],tipe)
    | crOCall(locn,crExp,cons[crExp],tipe)
    | crTplOff(locn,crExp,integer,tipe)
    | crTplUpdate(locn,crExp,integer,crExp)
    | crCnj(locn,crExp,crExp)
    | crDsj(locn,crExp,crExp)
    | crNeg(locn,crExp)
    | crCnd(locn,crExp,crExp,crExp)
    | crLtt(locn,crVar,crExp,crExp)
    | crUnpack(locn,crExp,cons[crCase],tipe)
    | crCase(locn,crExp,cons[crCase],crExp,tipe)
    | crWhere(locn,crExp,crExp)
    | crMatch(locn,crExp,crExp).
  
  public crVar ::= crId(string,tipe).

  public crCase ~> (locn,crExp,crExp).

  public crDefn ::= fnDef(locn,string,tipe,cons[crVar],crExp) |
    glbDef(locn,string,tipe,crExp)|
    tpDef(locn,tipe,tipe).

  public dispCrProg:(cons[crDefn])=>string.
  dispCrProg(Defs) => interleave(Defs//disp,".\n")*.

  public implementation display[crDefn] => {
    disp(Df) => dspDef(Df,"  ").
  }

  dspDef:(crDefn,string) => string.
  dspDef(fnDef(Lc,Nm,Tp,Args,Rep),Off) =>
    "fun: $(Lc)\n#(Nm)(#(interleave(Args//disp,",")*)) => #(dspExp(Rep,Off))".
  dspDef(glbDef(Lc,Nm,Tp,Rep),Off) =>
    "glb: $(Lc)\n#(Nm):$(Tp)=#(dspExp(Rep,Off))".

  dspExp:(crExp,string) => string.
  dspExp(crVar(_,V),_) => disp(V).
  dspExp(crInt(_,Ix),_) => disp(Ix).
  dspExp(crBig(_,Ix),_) => disp(Ix).
  dspExp(crFlot(_,Dx),_) => disp(Dx).
  dspExp(crStrg(_,Sx),_) => disp(Sx).
  dspExp(crVoid(_,_),_) => "void".
  dspExp(crLbl(_,Lb,_),_) => ".#(Lb)".
  dspExp(crECall(_,Op,As,_),Off) => "#(Op)(#(dsplyExps(As,Off)*))".
  dspExp(crOCall(_,Op,As,_),Off) => "#(dspExp(Op,Off))·(#(dsplyExps(As,Off)*))".
  dspExp(crCall(_,Op,As,_),Off) => "#(Op)(#(dsplyExps(As,Off)*))".
  dspExp(crTerm(_,Op,As,_),Off) where isTplLbl(Op) => "‹#(dsplyExps(As,Off)*)›".
  dspExp(crTerm(_,Op,As,_),Off) => "#(Op)‹#(dsplyExps(As,Off)*)›".
  dspExp(crTplOff(_,O,Ix,_),Off) => "#(dspExp(O,Off)).$(Ix)".
  dspExp(crTplUpdate(_,O,Ix,E),Off) => "#(dspExp(O,Off)).$(Ix) := #(dspExp(E,Off))".
  dspExp(crLtt(_,V,D,I),Off) where Off2.=Off++"  " =>
    "let $(V) = #(dspExp(D,Off2)) in\n#(Off2)#(dspExp(I,Off2))".
  dspExp(crCase(_,E,Cs,Dflt,_),Off) where Off2.=Off++"  "=>
    "case #(dspExp(E,Off)) in { #(dspCases(Cs,Off2)*) } else #(dspExp(Dflt,Off2))".
  dspExp(crUnpack(_,E,Cs,_),Off) where Off2.=Off++"  "=>
    "unpack #(dspExp(E,Off)) in { #(dspCases(Cs,Off2)*) }".
  dspExp(crMatch(_,P,E),Off) => "#(dspExp(P,Off)).=#(dspExp(E,Off))".
  dspExp(crWhere(_,T,C),Off) => "#(dspExp(T,Off)) where #(dspExp(C,Off++"  "))".
  dspExp(crCnj(_,L,R),Off) => "#(dspExp(L,Off))&&#(dspExp(R,Off))".
  dspExp(crDsj(_,L,R),Off) => "(#(dspExp(L,Off))||#(dspExp(R,Off)))".
  dspExp(crCnd(_,T,L,R),Off) where Off2 .= Off++"  " =>
    "(#(dspExp(T,Off)) ? #(dspExp(L,Off2)) ||\n #(Off2)#(dspExp(R,Off2)))".
  dspExp(crNeg(_,R),Off) => "~#(dspExp(R,Off))".

  dspCases(Cs,Off) => let{
    Gap = ";\n"++Off.
  } in interleave(Cs//((_,P,V))=>"#(dspExp(P,Off))->#(dspExp(V,Off))",Gap).

  dsplyExps(Es,Off) => interleave(Es//(E)=>dspExp(E,Off),", ").

  isTplOp(crLbl(_,Nm,_)) => isTplLbl(Nm).
  isTplOp(_) default => .false.

  public mkCrTpl:(locn,cons[crExp]) => crExp.
  mkCrTpl(Lc,Args) => let{
    TpTp = tupleType(Args//typeOf).
    Ar = size(Args)
  } in crTerm(Lc,tplLbl(Ar), Args, TpTp).

  public implementation equality[crVar] => {
    crId(N1,T1) == crId(N2,T2) => N1==N2.
  }

  public implementation hashable[crVar] => {
    hash(crId(N,T)) => hash(N).
  }

  public implementation equality[crExp] => let{.
    eqTerm(crVar(_,V1),crVar(_,V2)) => V1==V2.
    eqTerm(crInt(_,N1),crInt(_,N2)) => N1==N2.
    eqTerm(crFlot(_,N1),crFlot(_,N2)) => N1==N2.
    eqTerm(crStrg(_,S1),crStrg(_,S2)) => S1==S2.
    eqTerm(crVoid(_,T1),crVoid(_,T2)) => T1==T2.
    eqTerm(crLbl(_,S1,_),crLbl(_,S2,_)) => S1==S2.
    eqTerm(crTerm(_,S1,A1,_),crTerm(_,S2,A2,_)) => S1==S2 && eqs(A1,A2).
    eqTerm(crCall(_,S1,A1,_),crCall(_,S2,A2,_)) => S1==S2 && eqs(A1,A2).
    eqTerm(crECall(_,S1,A1,_),crECall(_,S2,A2,_)) => S1==S2 && eqs(A1,A2).
    eqTerm(crOCall(_,S1,A1,_),crOCall(_,S2,A2,_)) => eqTerm(S1,S2) && eqs(A1,A2).
    eqTerm(crTplOff(_,R1,F1,_),crTplOff(_,R2,F2,_)) => eqTerm(R1,R2) && F1==F2.
    eqTerm(crTplUpdate(_,R1,Ix,E1),crTplUpdate(_,R2,Ix,E2)) => eqTerm(R1,R2) && eqTerm(E1,E2).
    eqTerm(crCnj(_,L1,R1),crCnj(_,L2,R2)) => eqTerm(L1,L2) && eqTerm(R1,R2).
    eqTerm(crDsj(_,L1,R1),crDsj(_,L2,R2)) => eqTerm(L1,L2) && eqTerm(R1,R2).
    eqTerm(crNeg(_,R1),crNeg(_,R2)) => eqTerm(R1,R2).
    eqTerm(crCnd(_,T1,L1,R1),crCnd(_,T2,L2,R2)) =>
      eqTerm(T1,T2) && eqTerm(L1,L2) && eqTerm(R1,R2).
    eqTerm(crLtt(_,T1,L1,R1),crLtt(_,T2,L2,R2)) =>
      T1==T2 && eqTerm(L1,L2) && eqTerm(R1,R2).
    eqTerm(crUnpack(_,S1,C1,_),crUnpack(_,S2,C2,_)) =>
      eqTerm(S1,S2) && eqCs(C1,C2).
    eqTerm(crCase(_,S1,C1,D1,_),crCase(_,S2,C2,D2,_)) =>
      eqTerm(S1,S2) && eqCs(C1,C2) && eqTerm(D1,D2).
    eqTerm(crWhere(_,E1,C1),crWhere(_,E2,C2)) => eqTerm(E1,E2) && eqTerm(C1,C2).
    eqTerm(crMatch(_,P1,E1),crMatch(_,P2,E2)) => eqTerm(E1,E2) && eqTerm(P1,P2).
    eqTerm(_,_) default => .false.

    eqs([],[]) => .true.
    eqs([E1,..S1],[E2,..S2]) => eqTerm(E1,E2) && eqs(S1,S2).
    eqs(_,_) default => .false.

    eqCs([],[]) => .true.
    eqCs([(_,N1,E1),..S1],[(_,N2,E2),..S2]) => eqTerm(N1,N2) && eqTerm(E1,E2) && eqCs(S1,S2).
    eqCs(_,_) default => .false.
  .} in {
    X == Y => eqTerm(X,Y)
  }

  public implementation hasLoc[crExp] => {
    locOf(crVar(Lc,_)) => Lc.
    locOf(crInt(Lc,_)) => Lc.
    locOf(crFlot(Lc,_)) => Lc.
    locOf(crStrg(Lc,_)) => Lc.
    locOf(crVoid(Lc,_)) => Lc.
    locOf(crLbl(Lc,_,_)) => Lc.
    locOf(crTplOff(Lc,_,_,_)) => Lc.
    locOf(crTplUpdate(Lc,_,_,_)) => Lc.
    locOf(crTerm(Lc,_,_,_)) => Lc.
    locOf(crWhere(Lc,_,_)) => Lc.
    locOf(crMatch(Lc,_,_)) => Lc.
    locOf(crLtt(Lc,_,_,_)) => Lc.
    locOf(crUnpack(Lc,_,_,_)) => Lc.
    locOf(crCase(Lc,_,_,_,_)) => Lc.
    locOf(crCall(Lc,_,_,_))=>Lc.
    locOf(crECall(Lc,_,_,_))=>Lc.
    locOf(crOCall(Lc,_,_,_))=>Lc.
    locOf(crCnj(Lc,_,_)) => Lc.
    locOf(crDsj(Lc,_,_)) => Lc.
    locOf(crNeg(Lc,_)) => Lc.
    locOf(crCnd(Lc,_,_,_)) => Lc.
  }

  public implementation hasType[crExp] => let{.
    tpOf(crVar(_,V)) => typeOf(V).
    tpOf(crInt(_,_)) => intType.
    tpOf(crFlot(_,_)) => fltType.
    tpOf(crStrg(_,_)) => strType.
    tpOf(crVoid(_,Tp)) => Tp.
    tpOf(crLbl(_,_,Tp)) => Tp.
    tpOf(crTerm(_,_,_,Tp)) => Tp.
    tpOf(crECall(_,_,_,Tp)) => Tp.
    tpOf(crOCall(_,_,_,Tp)) => Tp.
    tpOf(crCall(_,_,_,Tp)) => Tp.
    tpOf(crTplOff(_,_,_,Tp)) => Tp.
    tpOf(crTplUpdate(_,T,_,_)) => tpOf(T).
    tpOf(crLtt(_,_,_,E)) => tpOf(E).
    tpOf(crUnpack(_,_,_,Tp)) => Tp.
    tpOf(crCase(_,_,_,_,Tp)) => Tp.
    tpOf(crCnd(_,_,L,_)) => tpOf(L).
    tpOf(crWhere(_,T,_)) => tpOf(T).
    tpOf(crMatch(_,_,_)) => boolType.
    tpOf(crCnj(_,_,_)) => boolType.
    tpOf(crDsj(_,_,_)) => boolType.
    tpOf(crNeg(_,_)) => boolType.
  .} in {
    typeOf = tpOf
  }

  public implementation hasType[crVar] => {
    typeOf(crId(_,Tp)) => Tp.
  }

  public implementation display[crExp] => {
    disp(T) => dspExp(T,"")
  }

  public implementation display[crVar] => {
    disp(crId(Nm,_)) => "%#(Nm)".
  }

  public implementation coercion[crExp,term] => {.
    _coerce(crInt(_,Ix)) => some(intgr(Ix)).
    _coerce(crFlot(_,Dx)) => some(flot(Dx)).
    _coerce(crStrg(_,Sx)) => some(strg(Sx)).
    _coerce(crVoid(_,_)) => some(symb(tLbl("void",0))).
    _coerce(crInt(_,Ix)) => some(intgr(Ix)).
    _coerce(crLbl(_,Nm,_)) => some(symb(tLbl(Nm,0))).
    _coerce(crTerm(_,Nm,Args,_)) where NArgs ^= mapArgs(Args,[]) =>
      some(term(tLbl(Nm,size(Args)),NArgs)).
    _coerce(_) default => .none.

    private mapArgs([],So) => some(reverse(So)).
    mapArgs([A,..As],So) where NA^=_coerce(A) => mapArgs(As,[NA,..So]).
    mapArgs(_,_) default => .none.
  .}

  public implementation coercion[locn,crExp] => {
    _coerce(Lc) where locn(Nm,Line,Col,Off,Len).=Lc =>
      some(mkCrTpl(Lc,[crStrg(Lc,Nm),crInt(Lc,Line),crInt(Lc,Col),crInt(Lc,Off),crInt(Lc,Len)]))
  }

  public rwTerm:(crExp,(crExp)=>option[crExp])=>crExp.
  rwTerm(T,Tst) where Rep^=Tst(T) => Rep.
  rwTerm(crVar(Lc,V),_) => crVar(Lc,V).
  rwTerm(crInt(Lc,Ix),_) => crInt(Lc,Ix).
  rwTerm(crFlot(Lc,Dx),_) => crFlot(Lc,Dx).
  rwTerm(crStrg(Lc,Sx),_) => crStrg(Lc,Sx).
  rwTerm(crVoid(Lc,Tp),_) => crVoid(Lc,Tp).
  rwTerm(crLbl(Lc,Sx,Tp),_) => crLbl(Lc,Sx,Tp).
  rwTerm(crTplOff(Lc,R,Ix,Tp),Tst) => crTplOff(Lc,rwTerm(R,Tst),Ix,Tp).
  rwTerm(crTplUpdate(Lc,R,Ix,E),Tst) => crTplUpdate(Lc,rwTerm(R,Tst),Ix,rwTerm(E,Tst)).
  rwTerm(crTerm(Lc,Op,Args,Tp),Tst) =>
    crTerm(Lc,Op,rwTerms(Args,Tst),Tp).
  rwTerm(crCall(Lc,Op,Args,Tp),Tst) =>
    crCall(Lc,Op,Args//(A)=>rwTerm(A,Tst),Tp).
  rwTerm(crOCall(Lc,Op,Args,Tp),Tst) =>
    crOCall(Lc,rwTerm(Op,Tst),Args//(A)=>rwTerm(A,Tst),Tp).
  rwTerm(crECall(Lc,Op,Args,Tp),Tst) =>
    crECall(Lc,Op,Args//(A)=>rwTerm(A,Tst),Tp).
  rwTerm(crCnj(Lc,L,R),Tst) =>
    crCnj(Lc,rwTerm(L,Tst),rwTerm(R,Tst)).
  rwTerm(crDsj(Lc,L,R),Tst) =>
    crDsj(Lc,rwTerm(L,Tst),rwTerm(R,Tst)).
  rwTerm(crNeg(Lc,R),Tst) =>
    crNeg(Lc,rwTerm(R,Tst)).
  rwTerm(crCnd(Lc,T,L,R),Tst) =>
    crCnd(Lc,rwTerm(T,Tst),rwTerm(L,Tst),rwTerm(R,Tst)).
  rwTerm(crLtt(Lc,V,D,E),Tst) =>
    crLtt(Lc,V,rwTerm(D,Tst),rwTerm(E,dropVar(crName(V),Tst))).
  rwTerm(crUnpack(Lc,Sel,Cases,Tp),M) =>
    crUnpack(Lc,rwTerm(Sel,M),Cases//(C)=>rwCase(C,M),Tp).
  rwTerm(crCase(Lc,Sel,Cases,Deflt,Tp),M) =>
    crCase(Lc,rwTerm(Sel,M),Cases//(C)=>rwCase(C,M),rwTerm(Deflt,M),Tp).
  rwTerm(crWhere(Lc,T,C),M) =>
    crWhere(Lc,rwTerm(T,M),rwTerm(C,M)).
  rwTerm(crMatch(Lc,P,E),M) =>
    crMatch(Lc,rwTerm(P,M),rwTerm(E,M)).

  dropVar:(string,(crExp)=>option[crExp])=>(crExp)=>option[crExp].
  dropVar(Nm,Tst) => let{
    test(crVar(_,crId(Nm,_))) => .none.
    test(T) default => Tst(T)
  } in test.

  public rwTerms:(cons[crExp],(crExp)=>option[crExp])=>cons[crExp].
  rwTerms(Els,Tst) => (Els//(E)=>rwTerm(E,Tst)).

  rwDef(fnDef(Lc,Nm,Tp,Args,Val),M) =>
    fnDef(Lc,Nm,Tp,Args,rwTerm(Val,M)).
  rwDef(glbDef(Lc,Nm,Tp,Val),M) =>
    glbDef(Lc,Nm,Tp,rwTerm(Val,M)).

  rwCase:(crCase,(crExp)=>option[crExp]) => crCase.
  rwCase((Lc,Ptn,Rp),T) => (Lc,rwTerm(Ptn,T),rwTerm(Rp,T)).

  public rewriteTerm:(crExp,map[string,crExp])=>crExp.
  rewriteTerm(T,Map) => rwTerm(T,rwVar(Map)).

  public rewriteTerms:(cons[crExp],map[string,crExp])=>cons[crExp].
  rewriteTerms(T,Map) => rwTerms(T,rwVar(Map)).

  rwVar(M) => let{
    test(crVar(Lc,crId(Nm,Tp))) => M[Nm].
    test(_) => .none.
  } in test.

  public implementation hasLoc[crDefn] => {
    locOf(fnDef(Lc,_,_,_,_)) => Lc.
    locOf(glbDef(Lc,_,_,_)) => Lc.
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
  isGround(crTerm(_,_,Els,_)) => {? E in Els *> isGround(E) ?}.
  isGround(_) default => .false.
}
