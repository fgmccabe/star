star.compiler.term{
  import star.

  import star.compiler.data.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.data.
  import star.compiler.types.
  import star.pkg.
  
  public cExp ::= .cVoid(option[locn],tipe)
    | .cAnon(option[locn],tipe)
    | .cVar(option[locn],cId)
    | .cInt(option[locn],integer)
    | .cChar(option[locn],char)
    | .cBig(option[locn],bigint)
    | .cFloat(option[locn],float)
    | .cString(option[locn],string)
    | .cTerm(option[locn],string,cons[cExp],tipe)
    | .cNth(option[locn],cExp,integer,tipe)
    | .cSetNth(option[locn],cExp,integer,cExp)
    | .cCall(option[locn],string,cons[cExp],tipe)
    | .cECall(option[locn],string,cons[cExp],tipe)
    | .cOCall(option[locn],cExp,cons[cExp],tipe)
    | .cThrow(option[locn],cExp,tipe)
    | .cSeq(option[locn],cExp,cExp)
    | .cCnj(option[locn],cExp,cExp)
    | .cDsj(option[locn],cExp,cExp)
    | .cNeg(option[locn],cExp)
    | .cCnd(option[locn],cExp,cExp,cExp)
    | .cLtt(option[locn],cId,cExp,cExp)
    | .cUnpack(option[locn],cExp,cons[cCase[cExp]],tipe)
    | .cCase(option[locn],cExp,cons[cCase[cExp]],cExp,tipe)
    | .cWhere(option[locn],cExp,cExp)
    | .cMatch(option[locn],cExp,cExp)
    | .cVarNmes(option[locn],cons[(string,cId)],cExp)
    | .cAbort(option[locn],string,tipe)
    | .cSusp(option[locn],cExp,cExp,tipe)
    | .cResume(option[locn],cExp,cExp,tipe)
    | .cTry(option[locn],cExp,cExp,tipe)
    | .cValof(option[locn],aAction,tipe).
  
  public cId ::= cId(string,tipe).

  public all e ~~ cCase[e] ~> (option[locn],cExp,e).

  public aAction ::= .aNop(option[locn])
    | .aSeq(option[locn],aAction,aAction)
    | .aLbld(option[locn],string,aAction)
    | .aBreak(option[locn],string)
    | .aValis(option[locn],cExp)
    | .aThrow(option[locn],cExp)
    | .aPerf(option[locn],cExp)
    | .aDefn(option[locn],cExp,cExp)
    | .aAsgn(option[locn],cExp,cExp)
    | .aCase(option[locn],cExp,cons[cCase[aAction]],aAction)
    | .aUnpack(option[locn],cExp,cons[cCase[aAction]])
    | .aIftte(option[locn],cExp,aAction,aAction)
    | .aWhile(option[locn],cExp,aAction)
    | .aRetire(option[locn],cExp,cExp)
    | .aTry(option[locn],aAction,aAction)
    | .aLtt(option[locn],cId,cExp,aAction)
    | .aVarNmes(option[locn],cons[(string,cId)],aAction)
    | .aAbort(option[locn],string).

  public cDefn ::= fnDef(option[locn],string,tipe,cons[cId],cExp) |
    vrDef(option[locn],string,tipe,cExp)|
    tpDef(option[locn],tipe,typeRule,cons[(termLbl,tipe,integer)]) |
    lblDef(option[locn],termLbl,tipe,integer).

  public dispCrProg:(cons[cDefn])=>string.
  dispCrProg(Defs) => interleave(Defs//disp,".\n")*.

  public implementation display[cDefn] => {
    disp(Df) => dspDef(Df,"  ").
  }

  dspDef:(cDefn,string) => string.
  dspDef(Df,Off) => case Df in {
    .fnDef(Lc,Nm,Tp,Args,Rep) =>
      "fun: $(Lc) #(Nm)(#(interleave(Args//disp,",")*)) => #(dspExp(Rep,Off))".
    .vrDef(Lc,Nm,Tp,Rep) =>
      "var: $(Lc) #(Nm)=#(dspExp(Rep,Off))".
    .tpDef(Lc,Tp,TpRl,Map) =>
      "tpe: $(Lc) $(TpRl) with $(Map)".
    .lblDef(Lc,Lbl,Tp,Ix) =>
      "lbl: $(Lc) $(Lbl)\:$(Tp)@$(Ix)".
  }

  dspExp:(cExp,string) => string.
  dspExp(Exp,Off) => case Exp in {
    .cAnon(_,_) => "_".
    .cVar(_,.cId(V,VTp)) => "%#(V)/$(arity(VTp))".
    .cInt(_,Ix) => disp(Ix).
    .cChar(_,Ix) => disp(Ix).
    .cBig(_,Ix) => disp(Ix).
    .cFloat(_,Dx) => disp(Dx).
    .cString(_,Sx) => disp(Sx).
    .cVoid(_,_) => "void".
    .cECall(_,Op,As,_) => "#(Op)ε(#(dsplyExps(As,Off)*))".
    .cOCall(_,Op,As,_) => "#(dspExp(Op,Off))·(#(dsplyExps(As,Off)*))".
    .cCall(_,Op,As,_) => "#(Op)(#(dsplyExps(As,Off)*))".
    .cTerm(_,Op,As,_) where isTplLbl(Op) => "‹#(dsplyExps(As,Off)*)›".
    .cTerm(_,Op,As,_) => "#(Op)‹#(dsplyExps(As,Off)*)›".
    .cNth(_,O,Ix,_) => "#(dspExp(O,Off)).$(Ix)".
    .cSetNth(_,O,Ix,E) => "(#(dspExp(O,Off)).$(Ix) := #(dspExp(E,Off)))".
    .cThrow(_,E,_) => "throw #(dspExp(E,Off))".
    .cLtt(_,V,D,I) => valof{
      Off2=Off++"  ";
      valis "let $(V) = #(dspExp(D,Off2)) in\n#(Off2)#(dspExp(I,Off2))"
    }.
    .cCase(_,E,Cs,D,_)  => valof{
      Off2=Off++"  ";
      valis "case #(dspExp(E,Off)) in {\n#(Off2)#(dspCases(Cs,dspExp,Off2)*)\n#(Off)} else #(dspExp(D,Off))"
    }.
    .cUnpack(_,E,Cs,_) => valof{
      Off2=Off++"  ";
      valis "unpack #(dspExp(E,Off)) in {\n#(Off2)#(dspCases(Cs,dspExp,Off2)*)\n#(Off)}"
    }.
    .cMatch(_,P,E) => "#(dspExp(P,Off)).=#(dspExp(E,Off))".
    .cWhere(_,T,C) => "#(dspExp(T,Off)) where #(dspExp(C,Off++"  "))".
    .cCnj(_,L,R) => "#(dspExp(L,Off))&&#(dspExp(R,Off))".
    .cDsj(_,L,R) => "(#(dspExp(L,Off))||#(dspExp(R,Off)))".
    .cCnd(_,T,L,R) => valof{
      Off2=Off++"  ";
      valis "(#(dspExp(T,Off)) ? #(dspExp(L,Off2)) ||\n #(Off2)#(dspExp(R,Off2)))"
    }.
    .cNeg(_,R) => "~#(dspExp(R,Off))".
    .cSeq(Lc,L,R) => "{#(dspSeq(cSeq(Lc,L,R),Off++"  "))}".
    .cVarNmes(_,V,E) => "<vars #(dspVrs(V)) in #(dspExp(E,Off))>".
    .cAbort(_,M,_) => "abort #(M)".
    .cSusp(_,T,E,_) => "#(dspExp(T,Off)) suspend #(dspExp(E,Off))".
    .cResume(_,T,E,_) => "#(dspExp(T,Off)) resume #(dspExp(E,Off))".
    .cTry(_,E,H,_)=> "try #(dspExp(E,Off)) catch #(dspExp(H,Off++"  "))".
    .cValof(_,A,_) => "valof #(dspAct(A,Off))".
  }

  dspAct:(aAction,string)=>string.
  dspAct(Act,Off) => case Act in {
    .aNop(_) => "{}".
    .aSeq(_,L,R) => valof{
      Off2=Off++"  ";
      valis "{ #(dspAct(L,Off2)); #(dspActSeq(R,Off2)) }"
    }.
    .aLbld(_,Lb,A) => "#(Lb) : #(dspAct(A,Off))".
    .aBreak(_,Lb) => "break #(Lb)".
    .aValis(_,E) => "valis #(dspExp(E,Off))".
    .aThrow(_,E) => "throw #(dspExp(E,Off))".
    .aPerf(_,E) => "perform #(dspExp(E,Off))".
    .aDefn(_,P,E) => "#(dspExp(P,Off)) = #(dspExp(E,Off))".
    .aAsgn(_,P,E) => "#(dspExp(P,Off)) := #(dspExp(E,Off))".
    .aCase(_,E,Cs,Df) => valof{
      Off2=Off++"  ";
      valis "case #(dspExp(E,Off)) in {\n#(Off2)#(dspCases(Cs,dspAct,Off2)*)\n#(Off)} else #(dspAct(Df,Off))"
    }.
    .aUnpack(_,E,Cs) => valof{
      Off2=Off++"  ";
      valis "unpack #(dspExp(E,Off)) in {\n#(Off2)#(dspCases(Cs,dspAct,Off2)*)\n#(Off)}"
    }.
    .aIftte(_,C,T,E) => valof{
      Off2=Off++"  ";
      valis "if #(dspExp(C,Off)) then\n#(Off2)#(dspAct(T,Off2))else\n#(Off2)#(dspAct(E,Off2))"
    }.
    .aWhile(_,C,A) => valof{
      Off2=Off++"  ";
      valis "while #(dspExp(C,Off)) do#(dspAct(A,Off2))"
    }.
    .aRetire(_,T,E) => "#(dspExp(T,Off)) retire #(dspExp(E,Off))".
    .aTry(_,E,H) => valof{
      Off2=Off++"  ";
      valis "try #(dspAct(E,Off)) catch #(dspAct(H,Off))"
    }.
    .aLtt(_,V,D,I) => valof{
      Off2=Off++"  ";
      valis "let $(V) = #(dspExp(D,Off2)) in\n#(Off2)#(dspAct(I,Off2))"
    }.
    .aVarNmes(_,V,A) => "<vars #(dspVrs(V)) in #(dspAct(A,Off))>".
    .aAbort(_,M) => "abort #(M)".
  }

  dspActSeq(.aSeq(_,L,R),Off) => "\n#(Off)#(dspAct(L,Off));#(dspActSeq(R,Off))".
  dspActSeq(A,Off) => dspAct(A,Off).

  dspCases:all e ~~ (cons[cCase[e]],(e,string)=>string,string)=>cons[string].
  dspCases(Cs,F,Off) => let{
    Gap = ";\n"++Off.
  } in interleave(Cs//((_,P,V))=>"#(dspExp(P,Off))=>#(F(V,Off))",Gap).

  dsplyExps(Es,Off) => interleave(Es//(E)=>dspExp(E,Off),", ").

  dspSeq(.cSeq(_,L,R),Off) => "#(dspSeq(L,Off));#(dspSeq(R,Off))".
  dspSeq(T,Off) => dspExp(T,Off).

  dspVrs(V) => interleave(V//(((N,T))=>"$(N)=$(T)"),", ")*.

  public mcTpl:(option[locn],cons[cExp]) => cExp.
  mcTpl(Lc,Args) => let{
    TpTp = tupleType(Args//typeOf).
    Ar = size(Args)
  } in cTerm(Lc,tplLbl(Ar), Args, TpTp).

  public contract all e ~~ rewrite[e] ::= {
    rewrite:(e,map[termLbl,cDefn])=>e
  }

  public implementation equality[cId] => {
    .cId(N1,T1) == .cId(N2,T2) => N1==N2.
  }

  public implementation hashable[cId] => {
    hash(.cId(N,T)) => hash(N).
  }

  eqTerm(E1,E2) => case E1 in {
    .cAnon(_,T1) => .cAnon(_,T2).=E2 && T1==T2.
    .cVoid(_,T1) => .cVoid(_,T2).=E2 && T1==T2.
    .cVar(_,V1) => .cVar(_,V2).=E2 && V1==V2.
    .cInt(_,N1) => .cInt(_,N2).=E2 && N1==N2.
    .cChar(_,N1) => .cChar(_,N2).=E2 && N1==N2.
    .cFloat(_,N1) => .cFloat(_,N2).=E2 && N1==N2.
    .cString(_,S1) => .cString(_,S2).=E2 && S1==S2.
    .cTerm(_,S1,A1,_) => .cTerm(_,S2,A2,_).=E2 && S1==S2 && eqs(A1,A2).
    .cCall(_,S1,A1,_) => .cCall(_,S2,A2,_).=E2 && S1==S2 && eqs(A1,A2).
    .cECall(_,S1,A1,_) => .cECall(_,S2,A2,_).=E2 && S1==S2 && eqs(A1,A2).
    .cOCall(_,S1,A1,_) => .cOCall(_,S2,A2,_).=E2 && eqTerm(S1,S2) && eqs(A1,A2).
    .cThrow(_,S1,_) => .cThrow(_,S2,_).=E2 && S1==S2.
    .cNth(_,R1,F1,_) => .cNth(_,R2,F2,_).=E2 && eqTerm(R1,R2) && F1==F2.
    .cSetNth(_,R1,Ix,E1) => .cSetNth(_,R2,Ix,E2).=E2 && eqTerm(R1,R2) && eqTerm(E1,E2).
    .cSeq(_,L1,R1) => .cSeq(_,L2,R2).=E2 && eqTerm(L1,L2) && eqTerm(R1,R2).
    .cCnj(_,L1,R1) => .cCnj(_,L2,R2).=E2 && eqTerm(L1,L2) && eqTerm(R1,R2).
    .cDsj(_,L1,R1) => .cDsj(_,L2,R2).=E2 && eqTerm(L1,L2) && eqTerm(R1,R2).
    .cNeg(_,R1) => .cNeg(_,R2).=E2 && eqTerm(R1,R2).
    .cCnd(_,T1,L1,R1) => .cCnd(_,T2,L2,R2).=E2 &&
	eqTerm(T1,T2) && eqTerm(L1,L2) && eqTerm(R1,R2).
    .cLtt(_,T1,L1,R1) => .cLtt(_,T2,L2,R2).=E2 &&
	T1==T2 && eqTerm(L1,L2) && eqTerm(R1,R2).
    .cUnpack(_,S1,C1,_) => .cUnpack(_,S2,C2,_).=E2 &&
	eqTerm(S1,S2) && eqCs(C1,eqTerm,C2).
    .cCase(_,S1,C1,D1,_) => .cCase(_,S2,C2,D2,_).=E2 &&
	eqTerm(S1,S2) && eqCs(C1,eqTerm,C2) && eqTerm(D1,D2).
    .cWhere(_,E1,C1) => .cWhere(_,E2,C2).=E2 && eqTerm(E1,E2) && eqTerm(C1,C2).
    .cMatch(_,P1,E1) => .cMatch(_,P2,E2).=E2 && eqTerm(E1,E2) && eqTerm(P1,P2).
    .cAbort(_,M1,T1) => .cAbort(_,M2,T2).=E2 && M1==M2 && T1==T2.
    .cSusp(_,T1,E1,_) => .cSusp(_,T2,E2,_).=E2 && eqTerm(T1,T2) && eqTerm(E1,E2).
    .cResume(_,T1,E1,_) => .cResume(_,T2,E2,_).=E2 && eqTerm(T1,T2) && eqTerm(E1,E2).
    .cTry(_,M1,H1,_) => .cTry(_,M2,H2,_).=E2 && eqTerm(M1,M2) && eqTerm(H1,H2).
    .cValof(_,A1,_) => .cValof(_,A2,_).=E2 && eqAct(A1,A2).
    .cVarNmes(_,V1,E1) => .cVarNmes(_,V2,E2).=E2 && eqVs(V1,V2) && eqTerm(E1,E2).
    _ default => .false
  }

  eqs(L1,L2) => case L1 in {
    [] => L2==[].
    [E1,..S1] => [E2,..S2].=L2 ? eqTerm(E1,E2) && eqs(S1,S2) || .false.
    _ default => .false
  }

  eqCs:all e ~~ (cons[cCase[e]],(e,e)=>boolean,cons[cCase[e]])=>boolean.
  eqCs(Cs1,P,Cs2) => case Cs1 in {
    [] => isEmpty(Cs2).
    [(_,N1,E1),..S1] => [(_,N2,E2),..S2].=Cs2 && eqTerm(N1,N2) && P(E1,E2) && eqCs(S1,P,S2).
    _ default => .false.
  }

  eqVs(Vs1,Vs2) => case Vs1 in {
    [] => Vs2==[].
    [(N1,E1),..S1] => [(N2,E2),..S2].=Vs2 && N1==N2 && E1==E2 && eqVs(S1,S2).
    _ default => .false
  }

  eqAct(A1,A2) => case A1 in {
    .aNop(_) => .aNop(_).=A2.
    .aSeq(_,L1,R1) => .aSeq(_,L2,R2) .=A2 && eqAct(L1,L2) && eqAct(R1,R2).
    .aLbld(_,L1,Ac1) => .aLbld(_,L2,Ac2).=A2 && L1==L2 && eqAct(Ac1,Ac2).
    .aBreak(_,L1) => .aBreak(_,L2).=A2 && L1==L2.
    .aValis(_,E1) => .aValis(_,E2).=A2 && eqTerm(E1,E2).
    .aThrow(_,E1) => .aThrow(_,E2).=A2 && eqTerm(E1,E2).
    .aPerf(_,E1) => .aPerf(_,E2).=A2 && eqTerm(E1,E2).
    .aDefn(_,E1,V1) => .aDefn(_,E2,V2).=A2 && eqTerm(E1,E2) && eqTerm(V1,V2).
    .aAsgn(_,E1,V1) => .aAsgn(_,E2,V2).=A2 && eqTerm(E1,E2) && eqTerm(V1,V2).
    .aCase(_,S1,C1,D1) => .aCase(_,S2,C2,D2).=A2 &&
	eqTerm(S1,S2) && eqCs(C1,eqAct,C2) && eqAct(D1,D2).
    .aUnpack(_,S1,C1) => .aUnpack(_,S2,C2).=A2 &&
	eqTerm(S1,S2) && eqCs(C1,eqAct,C2).
    .aIftte(_,C1,L1,R1) => .aIftte(_,C2,L2,R2).=A2 &&
	eqTerm(C1,C2) && eqAct(L1,L2) && eqAct(R1,R2).
    .aWhile(_,C1,L1) => .aWhile(_,C2,L2).=A2 &&
	eqTerm(C1,C2) && eqAct(L1,L2).
    .aRetire(_,E1,V1) => .aRetire(_,E2,V2).=A2 && eqTerm(E1,E2) && eqTerm(V1,V2).
    .aTry(_,M1,H1) => .aTry(_,M2,H2).=A2 && eqAct(M1,M2) && eqAct(H1,H2).
    .aLtt(_,V1,D1,Ac1) => .aLtt(_,V2,D2,Ac2).=A2 &&
	V1==V2 && eqTerm(D1,D2) && eqAct(Ac1,Ac2).
    .aVarNmes(_,V1,Ac1) => .aVarNmes(_,V2,Ac2).=A2 && eqVs(V1,V2) && eqAct(Ac1,Ac2).
    .aAbort(_,M1) => .aAbort(_,M2).=A1 && M1==M2.
    _ default => .false.
  }

  public implementation equality[cExp] => {
    X == Y => eqTerm(X,Y)
  }

  public implementation equality[aAction] => {
    X == Y => eqAct(X,Y)
  }

  public implementation hasLoc[cExp] => {
    locOf(Tr) => case Tr in {
      .cVar(Lc,_) => Lc.
      .cVoid(Lc,_) => Lc.
      .cInt(Lc,_) => Lc.
      .cBig(Lc,_) => Lc.
      .cChar(Lc,_) => Lc.
      .cFloat(Lc,_) => Lc.
      .cString(Lc,_) => Lc.
      .cNth(Lc,_,_,_) => Lc.
      .cSetNth(Lc,_,_,_) => Lc.
      .cTerm(Lc,_,_,_) => Lc.
      .cWhere(Lc,_,_) => Lc.
      .cMatch(Lc,_,_) => Lc.
      .cLtt(Lc,_,_,_) => Lc.
      .cUnpack(Lc,_,_,_) => Lc.
      .cCase(Lc,_,_,_,_) => Lc.
      .cCall(Lc,_,_,_)=>Lc.
      .cECall(Lc,_,_,_)=>Lc.
      .cOCall(Lc,_,_,_)=>Lc.
      .cSeq(Lc,_,_) => Lc.
      .cCnj(Lc,_,_) => Lc.
      .cDsj(Lc,_,_) => Lc.
      .cNeg(Lc,_) => Lc.
      .cCnd(Lc,_,_,_) => Lc.
      .cAbort(Lc,_,_) => Lc.
      .cVarNmes(Lc,_,_) => Lc.
      .cSusp(Lc,_,_,_) => Lc.
      .cResume(Lc,_,_,_) => Lc.
      .cTry(Lc,_,_,_) => Lc.
      .cValof(Lc,_,_) => Lc.
    }
  }

  public implementation hasType[cExp] => let{.
    tpOf(Tr) => case Tr in {
      .cVoid(_,Tp) => Tp.
      .cAnon(_,Tp) => Tp.
      .cVar(_,V) => typeOf(V).
      .cInt(_,_) => intType.
      .cBig(_,_) => bigintType.
      .cChar(_,_) => chrType.
      .cFloat(_,_) => fltType.
      .cString(_,_) => strType.
      .cTerm(_,_,_,Tp) => Tp.
      .cECall(_,_,_,Tp) => Tp.
      .cOCall(_,_,_,Tp) => Tp.
      .cCall(_,_,_,Tp) => Tp.
      .cThrow(_,_,Tp) => Tp.
      .cNth(_,_,_,Tp) => Tp.
      .cSetNth(_,T,_,_) => tpOf(T).
      .cSeq(_,_,R) => tpOf(R).
      .cCnj(_,_,_) => boolType.
      .cDsj(_,_,_) => boolType.
      .cNeg(_,_) => boolType.
      .cLtt(_,_,_,E) => tpOf(E).
      .cUnpack(_,_,_,Tp) => Tp.
      .cCase(_,_,_,_,Tp) => Tp.
      .cCnd(_,_,L,_) => tpOf(L).
      .cWhere(_,T,_) => tpOf(T).
      .cMatch(_,_,_) => boolType.
      .cSusp(_,_,_,T) => T.
      .cResume(_,_,_,T) => T.
      .cTry(_,_,_,T) => T.
      .cValof(_,_,T) => T.
      .cAbort(_,_,T) => T.
      .cVarNmes(_,_,E) => tpOf(E).
    }
  .} in {
    typeOf = tpOf
  }

  public implementation hasType[cId] => {
    typeOf(.cId(_,Tp)) => Tp.
  }

  public implementation display[cExp] => {
    disp(T) => dspExp(T,"")
  }

  public implementation display[cId] => {
    disp(.cId(Nm,_)) => "%#(Nm)".
  }

  public implementation hasLoc[aAction] => {
    locOf(Ac) => case Ac in {
      .aNop(Lc) => Lc.
      .aSeq(Lc,_,_) => Lc.
      .aLbld(Lc,_,_) => Lc.
      .aBreak(Lc,_) => Lc.
      .aValis(Lc,_) => Lc.
      .aThrow(Lc,_) => Lc.
      .aPerf(Lc,_) => Lc.
      .aDefn(Lc,_,_) => Lc.
      .aAsgn(Lc,_,_) => Lc.
      .aCase(Lc,_,_,_) => Lc.
      .aUnpack(Lc,_,_) => Lc.
      .aIftte(Lc,_,_,_) => Lc.
      .aWhile(Lc,_,_) => Lc.
      .aRetire(Lc,_,_) => Lc.
      .aTry(Lc,_,_) => Lc.
      .aLtt(Lc,_,_,_) => Lc.
      .aVarNmes(Lc,_,_) => Lc.
      .aAbort(Lc,_) => Lc.
    }
  }

  public implementation display[aAction] => {
    disp(A) => dspAct(A,"")
  }

  public implementation coercion[cExp,data] => {.
    _coerce(Tr) => case Tr in {
      .cInt(_,Ix) => .some(.intgr(Ix)).
      .cBig(_,Ix) => .some(.bigi(Ix)).
      .cChar(_,Cx) => .some(.chr(Cx)).
      .cFloat(_,Dx) => .some(.flot(Dx)).
      .cString(_,Sx) => .some(.strg(Sx)).
      .cVoid(_,_) => .some(.symb(tLbl("void",0))).
      .cInt(_,Ix) => .some(.intgr(Ix)).
      .cTerm(_,Nm,[],_) => .some(.symb(tLbl(Nm,0))).
      .cTerm(_,Nm,Args,_) where NArgs ?= mapArgs(Args,[]) =>
	.some(.term(Nm,NArgs)).
      _ default => .none.
    }.

    private mapArgs([],So) => .some(reverse(So)).
    mapArgs([A,..As],So) where NA?=_coerce(A) => mapArgs(As,[NA,..So]).
    mapArgs(_,_) default => .none.
  .}

  public implementation coercion[locn,cExp] => {
    _coerce(Lc) where .locn(Nm,Line,Col,Off,Len).=Lc &&
	OLc .= .some(Lc) =>
      .some(mcTpl(OLc,[.cString(OLc,Nm),
	    .cInt(OLc,Line),.cInt(OLc,Col),.cInt(OLc,Off),.cInt(OLc,Len)]))
  }

  public rwTerm:(cExp,(cExp)=>option[cDefn])=>cExp.
  rwTerm(T,Tst) => .vrDef(_,_,_,Vl) ?= Tst(T) ?
    Vl ||
    case T in {
      .cVoid(Lc,Tp) => .cVoid(Lc,Tp).
      .cAnon(Lc,Tp) => .cAnon(Lc,Tp).
      .cVar(Lc,V) => .cVar(Lc,V).
      .cInt(Lc,Ix) => .cInt(Lc,Ix).
      .cBig(Lc,Ix) => .cBig(Lc,Ix).
      .cFloat(Lc,Dx) => .cFloat(Lc,Dx).
      .cChar(Lc,Cx) => .cChar(Lc,Cx).
      .cString(Lc,Sx) => .cString(Lc,Sx).
      .cTerm(Lc,Op,Args,Tp) => .cTerm(Lc,Op,rwTerms(Args,Tst),Tp).
      .cNth(Lc,R,Ix,Tp) =>.cNth(Lc,rwTerm(R,Tst),Ix,Tp).
      .cSetNth(Lc,R,Ix,E) =>.cSetNth(Lc,rwTerm(R,Tst),Ix,rwTerm(E,Tst)).
      .cCall(Lc,Op,Args,Tp) => .cCall(Lc,Op,Args//(A)=>rwTerm(A,Tst),Tp).
      .cOCall(Lc,Op,Args,Tp) => .cOCall(Lc,rwTerm(Op,Tst),Args//(A)=>rwTerm(A,Tst),Tp).
      .cECall(Lc,Op,Args,Tp) => .cECall(Lc,Op,Args//(A)=>rwTerm(A,Tst),Tp).
      .cThrow(Lc,E,Tp) =>.cThrow(Lc,rwTerm(E,Tst),Tp).
      .cSeq(Lc,L,R) =>.cSeq(Lc,rwTerm(L,Tst),rwTerm(R,Tst)).
      .cCnj(Lc,L,R) =>.cCnj(Lc,rwTerm(L,Tst),rwTerm(R,Tst)).
      .cDsj(Lc,L,R) =>.cDsj(Lc,rwTerm(L,Tst),rwTerm(R,Tst)).
      .cNeg(Lc,R) =>.cNeg(Lc,rwTerm(R,Tst)).
      .cCnd(Lc,G,L,R) =>.cCnd(Lc,rwTerm(G,Tst),rwTerm(L,Tst),rwTerm(R,Tst)).
      .cLtt(Lc,V,D,E) =>.cLtt(Lc,V,rwTerm(D,Tst),rwTerm(E,dropVar(cName(V),Tst))).
      .cUnpack(Lc,Sel,Cases,Tp) => .cUnpack(Lc,
	rwTerm(Sel,Tst),Cases//(C)=>rwCase(C,Tst,rwTerm),Tp).
      .cCase(Lc,Sel,Cases,Dflt,Tp) => .cCase(Lc,rwTerm(Sel,Tst),
	Cases//(C)=>rwCase(C,Tst,rwTerm),rwTerm(Dflt,Tst),Tp).
      .cWhere(Lc,Tr,C) => .cWhere(Lc,rwTerm(Tr,Tst),rwTerm(C,Tst)).
      .cMatch(Lc,P,E) => .cMatch(Lc,rwTerm(P,Tst),rwTerm(E,Tst)).
      .cSusp(Lc,F,E,Tp) => .cSusp(Lc,rwTerm(F,Tst),rwTerm(E,Tst),Tp).
      .cResume(Lc,F,E,Tp) => .cResume(Lc,rwTerm(F,Tst),rwTerm(E,Tst),Tp).
      .cTry(Lc,E,H,Tp) => .cTry(Lc,rwTerm(E,Tst),rwTerm(H,Tst),Tp).
      .cVarNmes(Lc,Vs,E) => .cVarNmes(Lc,Vs,rwTerm(E,Tst)).
      .cValof(Lc,A,Tp) => .cValof(Lc,rwAct(A,Tst),Tp).
      .cAbort(Lc,Ms,Tp) => .cAbort(Lc,Ms,Tp).
    }.

  public rwAct:(aAction,(cExp)=>option[cDefn])=>aAction.
  rwAct(Ac,Tst) => case Ac in {
    .aNop(Lc) => .aNop(Lc).
    .aSeq(Lc,L,R) => .aSeq(Lc,rwAct(L,Tst),rwAct(R,Tst)).
    .aLbld(Lc,L,A) => .aLbld(Lc,L,rwAct(A,Tst)).
    .aBreak(Lc,L) => .aBreak(Lc,L).
    .aValis(Lc,E) => .aValis(Lc,rwTerm(E,Tst)).
    .aThrow(Lc,E) => .aThrow(Lc,rwTerm(E,Tst)).
    .aPerf(Lc,E) => .aPerf(Lc,rwTerm(E,Tst)).
    .aDefn(Lc,V,E) => .aDefn(Lc,rwTerm(V,Tst),rwTerm(E,Tst)).
    .aAsgn(Lc,V,E) => .aAsgn(Lc,rwTerm(V,Tst),rwTerm(E,Tst)).
    .aCase(Lc,G,Cs,D) => .aCase(Lc,rwTerm(G,Tst),Cs//(C)=>rwCase(C,Tst,rwAct),rwAct(D,Tst)).
    .aUnpack(Lc,G,Cs) => .aUnpack(Lc,rwTerm(G,Tst),Cs//(C)=>rwCase(C,Tst,rwAct)).
    .aIftte(Lc,C,L,R) => .aIftte(Lc,rwTerm(C,Tst),rwAct(L,Tst),rwAct(R,Tst)).
    .aWhile(Lc,C,B) => .aWhile(Lc,rwTerm(C,Tst),rwAct(B,Tst)).
    .aRetire(Lc,T,E) => .aRetire(Lc,rwTerm(T,Tst),rwTerm(E,Tst)).
    .aTry(Lc,B,H) => .aTry(Lc,rwAct(B,Tst),rwAct(H,Tst)).
    .aLtt(Lc,V,D,A) =>.aLtt(Lc,V,rwTerm(D,Tst),rwAct(A,dropVar(cName(V),Tst))).
    .aVarNmes(Lc,Vs,E) => .aVarNmes(Lc,Vs,rwAct(E,Tst)).
    .aAbort(Lc,Ms) => .aAbort(Lc,Ms).
  }

  dropVar:(string,(cExp)=>option[cDefn])=>(cExp)=>option[cDefn].
  dropVar(Nm,Tst) => let{
    test(.cVar(_,.cId(Nm,_))) => .none.
    test(T) default => Tst(T)
  } in test.
  
  public rwTerms:(cons[cExp],(cExp)=>option[cDefn])=>cons[cExp].
  rwTerms(Els,Tst) => (Els//(E)=>rwTerm(E,Tst)).

  rwDef(D,M) => case D in {
    .fnDef(Lc,Nm,Tp,Args,Val) => .fnDef(Lc,Nm,Tp,Args,rwTerm(Val,M)).
    .vrDef(Lc,Nm,Tp,Val) => .vrDef(Lc,Nm,Tp,rwTerm(Val,M)).
    _ default => D
  }

  rwCase:all e ~~ (cCase[e],(cExp)=>option[cDefn],(e,(cExp)=>option[cDefn])=>e) => cCase[e].
  rwCase((Lc,Ptn,Rep),T,F) => (Lc,rwTerm(Ptn,T),F(Rep,T)).

  public implementation rewrite[cExp] => {
    rewrite(E,M) => rwTerm(E,rwVar(M)).
  }

  public implementation rewrite[aAction] => {
    rewrite(E,M) => rwAct(E,rwVar(M)).
  }

  public rewriteTerm:(cExp,map[termLbl,cDefn])=>cExp.
  rewriteTerm(T,Map) => rwTerm(T,rwVar(Map)).

  public rewriteTerms:all e ~~ rewrite[e] |: (cons[e],map[termLbl,cDefn])=>cons[e].
  rewriteTerms(Els,Map) => (Els//(E)=>rewrite(E,Map)).

  rwVar(M) => let{
    test(.cVar(Lc,.cId(Nm,Tp))) => M[.tLbl(Nm,arity(Tp))].
    test(_) => .none.
  } in test.

  public implementation hasLoc[cDefn] => {
    locOf(Df) => case Df in {
      .fnDef(Lc,_,_,_,_) => Lc.
      .vrDef(Lc,_,_,_) => Lc.
      .tpDef(Lc,_,_,_) => Lc.
      .lblDef(Lc,_,_,_) => Lc.
    }
  }

  public dfLbl:(cDefn)=>option[termLbl].
  dfLbl(.fnDef(_,Nm,_,Args,_)) => ?.tLbl(Nm,[|Args|]).
  dfLbl(.vrDef(_,Nm,Tp,_)) => ?.tLbl(Nm,arity(Tp)).
  dfLbl(_) default => .none.

  public cName:(cId) => string.
  cName(.cId(Nm,_))=>Nm.

  public cType:(cId) => tipe.
  cType(.cId(_,Tp)) => Tp.

  public isCond:(cExp)=>boolean.
  isCond(C) => case C in {
    .cCnj(_,_,_)=>.true.
    .cDsj(_,_,_)=>.true.
    .cNeg(_,_)=>.true.
    .cCnd(_,_,L,R)=>isCond(L)&&isCond(R).
    .cWhere(_,L,_) => isCond(L).
    .cMatch(_,_,_)=>.true.
    _ default => .false.
  }

  public isGround:(cExp) => boolean.
  isGround(T) => case T in {
    .cInt(_,_) => .true.
    .cBig(_,_) => .true.
    .cFloat(_,_) => .true.
    .cChar(_,_) => .true.
    .cString(_,_) => .true.
    .cTerm(_,_,Els,_) => {? E in Els *> isGround(E) ?}.
    _ default => .false.
  }

  public mergeGoal:(option[locn],option[cExp],option[cExp])=>option[cExp].
  mergeGoal(Lc,G1,G2) => case (G1,G2) in {
    (G,.none) => G.
    (.none,G) => G.
    (.some(G),.some(H)) => .some(.cCnj(Lc,G,H)).
  }
  
  public contract all e ~~ reform[e] ::= {
    mkCond:(option[locn],cExp,e,e)=>e.
    mkCase:(option[locn],cExp,cons[cCase[e]],e) => e.
    mkUnpack:(option[locn],cExp,cons[cCase[e]]) => e.
    varNames:(option[locn],cons[(string,cId)],e)=>e.
    pullWhere:(e,option[cExp]) => (e,option[cExp]).
    mkLtt:(option[locn],cId,cExp,e) => e.
  }

  public implementation reform[cExp] => {.
    mkCond(Lc,Tst,Th,El) => valof{
      if .cCnd(_,T1,Th1,El1).=Th && El1==El then
	valis .cCnd(Lc,cCnj(Lc,Tst,T1),Th1,El1) else
      valis .cCnd(Lc,Tst,Th,El).
    }

    varNames(Lc,Bnds,Val) => cVarNmes(Lc,Bnds,Val).

    pullWhere(.cWhere(Lc,V,C),G) where (Val,G1) .= pullWhere(V,G) =>
      (Val,mergeGoal(Lc,some(C),G1)).
    pullWhere(.cTerm(Lc,Lbl,Args,Tp),G) where (NArgs,Gx) .= pullWheres(Args,G) =>
      (.cTerm(Lc,Lbl,NArgs,Tp),Gx).
    pullWhere(Exp,G) default => (Exp,G).

    pullWheres([],G) => ([],G).
    pullWheres([A,..As],G) where (NA,NG).=pullWhere(A,G) && (NAs,Gx) .= pullWheres(As,NG) =>
      ([NA,..NAs],Gx).

    mkCase(Lc,Tst,[(PLc,Ptn,Val)],Deflt) => mkCond(Lc,cMatch(PLc,Ptn,Tst),Val,Deflt).
    mkCase(Lc,V,Cases,Deflt) => cCase(Lc,V,Cases,Deflt,typeOf(Deflt)).

    mkUnpack(Lc,V,Arms) => cUnpack(Lc,V,Arms,typeOf(V)).

    mkLtt(Lc,V,E,X) => cLtt(Lc,V,E,X).
  .}

  public implementation reform[aAction] => {
    mkCond(Lc,Tst,Th,El) where
	.aIftte(Lc0,T1,Th1,El1).=Th && El1==El => .aIftte(Lc0,cCnj(Lc,Tst,T1),Th1,El1).
    mkCond(Lc,Tst,Th,El) => aIftte(Lc,Tst,Th,El).

    varNames(Lc,Bnds,Val) => aVarNmes(Lc,Bnds,Val).

    pullWhere(A,Cond) => (A,Cond).

    mkCase(Lc,V,Cases,Deflt) => aCase(Lc,V,Cases,Deflt).
    mkUnpack(Lc,V,Arms) => aUnpack(Lc,V,Arms).

    mkLtt(Lc,V,E,X) => aLtt(Lc,V,E,X).
  }

  dfVars:(cons[cDefn],set[cId])=>set[cId].
  dfVars([.fnDef(_,Nm,Tp,_,_),..Ds],D) =>
    dfVars(Ds,D\+cId(Nm,Tp)).
  dfVars([.vrDef(_,Nm,Tp,_),..Ds],D) =>
    dfVars(Ds,D\+cId(Nm,Tp)).
  dfVars([_,..Ds],D) => dfVars(Ds,D).
  dfVars([],D) => D.

  dclVrs:(cons[decl],set[cId])=>set[cId].
  dclVrs(Decs,Vrs) => foldLeft(dclVr,Vrs,Decs).

  dclVr(Df,Vrs) => case Df in {
    .funDec(_,_,Nm,Tp) => Vrs\+.cId(Nm,Tp).
    .varDec(_,_,Nm,Tp) => Vrs\+.cId(Nm,Tp).
    .cnsDec(_,_,Nm,Tp) => Vrs\+.cId(Nm,Tp).
    .accDec(_,_,_,Nm,Tp) => Vrs\+.cId(Nm,Tp).
    .updDec(_,_,_,Nm,Tp) => Vrs\+.cId(Nm,Tp).
    .implDec(_,_,Nm,Tp) => Vrs\+.cId(Nm,Tp).
    _ default => Vrs
  }

  public validProg:(cons[cDefn],cons[decl]) => ().
  validProg(Defs,Decls) => valof{
    
    D = dfVars(Defs,dclVrs(Decls,[]));

    for Df in Defs do{
      case Df in {
	.fnDef(Lc,Nm,Tp,Args,Val) => {
	  D1 = foldLeft((V,D1)=>D1\+V,D,Args);
	  if ~validE(Val,D1) then{
	    reportError("$(fnDef(Lc,Nm,Tp,Args,Val)) not valid",Lc)
	  }
	}.
	.vrDef(Lc,Nm,Tp,Val) => {
	  if ~validE(Val,D) then{
	    reportError("$(vrDef(Lc,Nm,Tp,Val)) not valid",Lc)
	  }
	}
	_ default => {}
      }
    };
    valis ()
  }

  validE:(cExp,set[cId]) => boolean.
  validE(Exp,Vrs) => case Exp in {
    .cVoid(Lc,Tp) => .true.
    .cAnon(_,_) => .true.
    .cVar(Lc,V) =>  V .<. Vrs ? .true || valof{
      reportError("$(V)/$(arity(typeOf(V)))",Lc);
      valis .false
    }.
    .cInt(_,_) => .true.
    .cBig(_,_) => .true.
    .cChar(_,_) => .true.
    .cString(_,_) => .true.
    .cFloat(_,_) => .true.
    .cTerm(_,_,Args,_) => {? E in Args *> validE(E,Vrs) ?}.
    .cNth(_,R,_,_) => validE(R,Vrs).
    .cSetNth(_,R,_,V) => validE(R,Vrs) && validE(V,Vrs).
    .cCall(_,_,Args,_) => {? E in Args *> validE(E,Vrs) ?}.
    .cECall(_,_,Args,_) => {? E in Args *> validE(E,Vrs) ?}.
    .cOCall(_,Op,Args,_) => validE(Op,Vrs) && {? E in Args *> validE(E,Vrs) ?}.
    .cThrow(_,E,_) => validE(E,Vrs).
    .cSeq(_,L,R) => validE(L,Vrs) && validE(R,Vrs).
    .cCnj(_,L,R) => validE(L,Vrs) && validE(R,Vrs).
    .cDsj(_,L,R) => validE(L,Vrs) && validE(R,Vrs).
    .cNeg(_,R) => validE(R,Vrs).
    .cCnd(_,Ts,L,R) => valof{
      V1 = glVars(Ts,Vrs);
      valis validE(Ts,V1) && validE(L,V1) && validE(R,Vrs)
    }.
    .cLtt(_,B,V,E) => validE(V,Vrs) && validE(E,Vrs\+B).
    .cCase(_,G,Cs,Df,_) => validE(G,Vrs) && validCases(Cs,validE,Vrs) && validE(Df,Vrs).
    .cUnpack(_,G,Cs,_) => validE(G,Vrs) && validCases(Cs,validE,Vrs).
    .cWhere(_,V,E) => validE(V,Vrs) && validE(E,Vrs).
    .cMatch(_,V,E) => validE(V,Vrs) && validE(E,Vrs).
    .cVarNmes(_,_,E) => validE(E,Vrs).
    .cAbort(_,_,_) => .true.
    .cSusp(_,Ts,E,_) => validE(Ts,Vrs) && validE(E,Vrs).
    .cResume(_,Ts,E,_) => validE(Ts,Vrs) && validE(E,Vrs).
    .cTry(_,Ts,E,_) => validE(Ts,Vrs) && validE(E,Vrs).
    .cValof(_,A,_) => validA(A,Vrs).
  }

  validCases:all e ~~ (cons[cCase[e]],(e,set[cId])=>boolean,set[cId]) => boolean.
  validCases([],_,_) => .true.
  validCases([(_,A,E),..Cs],P,Vrs) => valof{
    D1 = ptnVrs(A,Vrs);
    valis validE(A,D1) && P(E,D1) && validCases(Cs,P,Vrs)
  }

  validA:(aAction,set[cId])=>boolean.
  validA(Ac,Vrs) => case Ac in {
    .aNop(_) => .true.
    .aSeq(_,A1,A2) => valof{
      if .aDefn(_,P,V) .= A1 then{
	V1 = ptnVrs(P,Vrs);
	valis validE(P,V1) && validE(V,Vrs) && validA(A2,V1);
      } else {
	valis validA(A1,Vrs) && validA(A2,Vrs)
      }
    }.
    .aLbld(_,_,A) => validA(A,Vrs).
    .aBreak(_,L) => .true.
    .aValis(_,E) => validE(E,Vrs).
    .aThrow(_,E) => validE(E,Vrs).
    .aPerf(_,E) => validE(E,Vrs).
    .aDefn(_,P,E) => validE(P,ptnVrs(P,Vrs)) && validE(E,Vrs).
    .aAsgn(_,L,V) => validE(L,Vrs) && validE(V,Vrs).
    .aCase(_,G,Cs,Df) =>
      validE(G,Vrs) && validCases(Cs,validA,Vrs) && validA(Df,Vrs).
    .aUnpack(_,G,Cs) =>
      validE(G,Vrs) && validCases(Cs,validA,Vrs).
    .aIftte(_,G,Th,E) => valof{
      D1 = glVars(G,Vrs);
      valis validE(G,D1) && validA(Th,D1) && validA(E,Vrs)
    }.
    .aWhile(_,G,A) => valof{
      D1 = glVars(G,Vrs);
      valis validE(G,D1) && validA(A,D1)
    }.
    .aRetire(_,Ts,E) => validE(Ts,Vrs) && validE(E,Vrs).
    .aTry(_,A,H) => validA(A,Vrs) && validA(H,Vrs).
    .aLtt(_,B,V,A) => validE(V,Vrs) && validA(A,Vrs\+B).
    .aVarNmes(_,_,A) => validA(A,Vrs).
    .aAbort(_,_) => .true.
  }

  public ptnVrs:(cExp,set[cId]) => set[cId].
  ptnVrs(E,Vrs) => case E in {
    .cVoid(_,_) => Vrs.
    .cAnon(_,_) => Vrs.
    .cVar(_,V) => Vrs\+V.
    .cInt(_,_) => Vrs.
    .cBig(_,_) => Vrs.
    .cChar(_,_) => Vrs.
    .cString(_,_) => Vrs.
    .cFloat(_,_) => Vrs.
    .cTerm(_,_,Args,_) => foldLeft(ptnVrs,Vrs,Args).
    .cNth(_,R,_,_) => ptnVrs(R,Vrs).
    .cWhere(_,V,E) => ptnVrs(V,glVars(E,Vrs)).
  }

  glVars:(cExp,set[cId])=>set[cId].
  glVars(G,Vrs) => case G in {
    .cCnj(_,L,R) => glVars(R,glVars(L,Vrs)).
    .cDsj(_,L,R) => valof{
      D1 = glVars(L,[]);
      D2 = glVars(R,[]);
      valis Vrs\/(D1/\D2)
    }.
    .cNeg(_,R) => Vrs.
    .cCnd(_,Ts,L,R) => valof{
      D1 = glVars(Ts,glVars(L,[]));
      D2 = glVars(R,[]);
      valis Vrs\/(D1/\D2)
    }.
    .cWhere(_,V,C) => glVars(C,glVars(V,Vrs)).
    .cMatch(_,P,_) => ptnVrs(P,Vrs).
    _ default => Vrs
  }

  public contract all e ~~ present[e] ::= {
    present:(e,(cExp)=>boolean)=>boolean
  }

  public implementation present[cExp] => {
    present(E,F) => presentInE(E,(_)=>.false,F)
  }

  public implementation present[aAction] => {
    present(A,F) => presentInA(A,(_)=>.false,F)
  }

  public lblUsed:(aAction,string) => boolean.
  lblUsed(A,Lb) => presentInA(A,(T)=>isBreak(T,Lb),(_)=>.false).

  isBreak(.aBreak(_,Lb),Lb) => .true.
  isBreak(_,_) default => .false.

  presentInA(A,C,T) => C(A) ?
    .true ||
    case A in {
      .aNop(_) => .false.
      .aSeq(_,A1,A2) => presentInA(A1,C,T) || presentInA(A2,C,T).
      .aLbld(_,_,A) => presentInA(A,C,T).
      .aBreak(_,L) => .false.
      .aValis(_,E) => presentInE(E,C,T).
      .aThrow(_,E) => presentInE(E,C,T).
      .aPerf(_,E) => presentInE(E,C,T).
      .aDefn(_,_,E) => presentInE(E,C,T).
      .aAsgn(_,L,V) => presentInE(L,C,T) || presentInE(V,C,T).
      .aCase(_,G,Cs,D) =>
	presentInE(G,C,T) || presentInCases(Cs,presentInA,C,T) || presentInA(D,C,T).
      .aUnpack(_,G,Cs) =>
	presentInE(G,C,T) || presentInCases(Cs,presentInA,C,T).
      .aIftte(_,G,Th,E) =>
	presentInE(G,C,T) || presentInA(Th,C,T) || presentInA(E,C,T).
      .aWhile(_,G,A) =>
	presentInE(G,C,T) || presentInA(A,C,T).
      .aRetire(_,Ts,E) => presentInE(Ts,C,T) || presentInE(E,C,T).
      .aTry(_,A,H) => presentInA(A,C,T) || presentInA(H,C,T).
      .aLtt(_,_,V,A) => presentInE(V,C,T) || presentInA(A,C,T).
      .aVarNmes(_,_,A) => presentInA(A,C,T).
      .aAbort(_,_) => .false.
    }.

  public varPresent:all e ~~ present[e] |: (e,string)=>boolean.
  varPresent(E,Nm) => present(E,(V)=>sameVar(V,Nm)).

  sameVar(.cVar(_,.cId(Nm,_)),Nm) => .true.
  sameVar(_,_) default => .false.

  presentInE:(cExp,(aAction)=>boolean,(cExp)=>boolean) => boolean.
  presentInE(T,A,C) => C(T) ?
    .true ||
    case T in {
      .cVoid(_,_) => .false.
      .cAnon(_,_) => .false.
      .cVar(_,_) => .false.
      .cInt(_,_) => .false.
      .cBig(_,_) => .false.
      .cChar(_,_) => .false.
      .cString(_,_) => .false.
      .cFloat(_,_) => .false.
      .cTerm(_,_,Args,_) => {? E in Args && presentInE(E,A,C) ?}.
      .cNth(_,R,_,_) => presentInE(R,A,C).
      .cSetNth(_,R,_,V) => presentInE(R,A,C) || presentInE(V,A,C).
      .cCall(_,_,Args,_) => {? E in Args && presentInE(E,A,C) ?}.
      .cECall(_,_,Args,_) => {? E in Args && presentInE(E,A,C) ?}.
      .cOCall(_,Op,Args,_) =>
	presentInE(Op,A,C) || {? E in Args && presentInE(E,A,C) ?}.
      .cThrow(_,E,_) => presentInE(E,A,C).
      .cSeq(_,L,R) => presentInE(L,A,C) || presentInE(R,A,C).
      .cCnj(_,L,R) => presentInE(L,A,C) || presentInE(R,A,C).
      .cDsj(_,L,R) => presentInE(L,A,C) || presentInE(R,A,C).
      .cNeg(_,R) => presentInE(R,A,C).
      .cCnd(_,Ts,L,R) =>
	presentInE(Ts,A,C) || presentInE(L,A,C) || presentInE(R,A,C).
      .cLtt(_,_,V,E) =>
	presentInE(V,A,C) || presentInE(E,A,C).
      .cCase(_,G,Cs,D,_) =>
	presentInE(G,A,C) || presentInCases(Cs,presentInE,A,C) || presentInE(D,A,C).
      .cUnpack(_,G,Cs,_) =>
	presentInE(G,A,C) || presentInCases(Cs,presentInE,A,C).
      .cWhere(_,V,E) =>
	presentInE(V,A,C) || presentInE(E,A,C).
      .cMatch(_,V,E) =>
	presentInE(V,A,C) || presentInE(E,A,C).
      .cVarNmes(_,_,E) =>
	presentInE(E,A,C).
      .cAbort(_,_,_) => .false.
      .cSusp(_,Ts,E,_) => presentInE(Ts,A,C) || presentInE(E,A,C).
      .cResume(_,Ts,E,_) => presentInE(Ts,A,C) || presentInE(E,A,C).
      .cTry(_,Ts,E,_) => presentInE(Ts,A,C) || presentInE(E,A,C).
      .cValof(_,Act,_) => presentInA(Act,A,C).
    }

  presentInCases:all e ~~ (cons[cCase[e]],(e,(aAction)=>boolean,(cExp)=>boolean)=>boolean,
    (aAction)=>boolean,(cExp)=>boolean)=>boolean.
  presentInCases([],_,_,_) => .false.
  presentInCases([(_,A,E),..Cs],P,C,T) =>
    presentInE(A,C,T) || P(E,C,T) || presentInCases(Cs,P,C,T).

  public visitE:all a ~~ (cExp,(cExp,a)=>a,(aAction,a)=>a,a) => a.
  visitE(Tr,V,VA,X) => case Tr in {
    .cTerm(Lc,Nm,Args,Tp) =>
      visitEls(Args,V,VA,V(.cTerm(Lc,Nm,Args,Tp),X)).
    .cNth(Lc,R,Ix,Tp) =>
      visitE(R,V,VA,V(.cNth(Lc,R,Ix,Tp),X)).
    .cSetNth(Lc,R,F,N) =>
      visitE(R,V,VA,visitE(N,V,VA,V(.cSetNth(Lc,R,F,N),X))).
    .cCall(Lc,Nm,Args,Tp) =>
      visitEls(Args,V,VA,V(.cCall(Lc,Nm,Args,Tp),X)).
    .cECall(Lc,Nm,Args,Tp) =>
      visitEls(Args,V,VA,V(.cECall(Lc,Nm,Args,Tp),X)).
    .cOCall(Lc,Op,Args,Tp) =>
      visitEls(Args,V,VA,visitE(Op,V,VA,V(.cOCall(Lc,Op,Args,Tp),X))).
    .cThrow(Lc,E,Tp) =>
      visitE(E,V,VA,V(.cThrow(Lc,E,Tp),X)).
    .cSeq(Lc,L,R) =>
      visitE(R,V,VA,visitE(L,V,VA,V(.cSeq(Lc,L,R),X))).
    .cCnj(Lc,L,R) =>
      visitE(R,V,VA,visitE(L,V,VA,V(.cCnj(Lc,L,R),X))).
    .cDsj(Lc,L,R) =>
      visitE(R,V,VA,visitE(L,V,VA,V(.cDsj(Lc,L,R),X))).
    .cNeg(Lc,R) =>
      visitE(R,V,VA,V(.cNeg(Lc,R),X)).
    .cCnd(Lc,T,L,R) =>
      visitE(R,V,VA,visitE(L,V,VA,visitE(T,V,VA,V(.cCnd(Lc,T,L,R),X)))).
    .cWhere(Lc,L,R) =>
      visitE(R,V,VA,visitE(L,V,VA,V(.cWhere(Lc,L,R),X))).
    .cMatch(Lc,L,R) =>
      visitE(R,V,VA,visitE(L,V,VA,V(.cMatch(Lc,L,R),X))).
    .cLtt(Lc,Vr,B,E) =>
      visitE(E,V,VA,visitE(B,V,VA,V(.cLtt(Lc,Vr,B,E),X))).
    .cCase(Lc,G,Cs,D,Tp) =>
      visitE(D,V,VA,
	visitCases(Cs,V,VA,visitE,
	  visitE(G,V,VA,
	    V(.cCase(Lc,G,Cs,D,Tp),X)))).
    .cUnpack(Lc,G,Cs,Tp) =>
      visitCases(Cs,V,VA,visitE,
	visitE(G,V,VA,
	  V(.cUnpack(Lc,G,Cs,Tp),X))).
    .cVarNmes(Lc,Vs,E) =>
      visitE(E,V,VA,V(.cVarNmes(Lc,Vs,E),X)).
    .cSusp(Lc,T,E,Tp) =>
      visitE(T,V,VA,visitE(E,V,VA,V(.cSusp(Lc,T,E,Tp),X))).
    .cResume(Lc,T,E,Tp) =>
      visitE(T,V,VA,visitE(E,V,VA,V(.cResume(Lc,T,E,Tp),X))).
    .cTry(Lc,E,H,Tp) =>
      visitE(H,V,VA,visitE(E,V,VA,V(.cTry(Lc,E,H,Tp),X))).
    .cValof(Lc,A,Tp) =>
      visitA(A,V,VA,V(.cValof(Lc,A,Tp),X)).
  }

  visitEls:all a ~~ (cons[cExp],(cExp,a)=>a,(aAction,a)=>a,a) => a.
  visitEls([],_,_,X) => X.
  visitEls([E,..Els],V,VA,X) =>
    visitEls(Els,V,VA,visitE(E,V,VA,X)).

  public visitA:all a ~~ (aAction,(cExp,a)=>a,(aAction,a)=>a,a) => a.
  visitA(Ac,V,VA,X) => case Ac in {
    .aSeq(Lc,A1,A2) => visitA(A2,V,VA,visitA(A1,V,VA,VA(.aSeq(Lc,A1,A2),X))).
    .aLbld(Lc,Lb,A) => visitA(A,V,VA,VA(.aLbld(Lc,Lb,A),X)).
    .aValis(Lc,E) => visitE(E,V,VA,X).
    .aThrow(Lc,E) => visitE(E,V,VA,X).
    .aPerf(Lc,E) => visitE(E,V,VA,X).
    .aDefn(Lc,Vr,E) => visitE(E,V,VA,VA(.aDefn(Lc,Vr,E),X)).
    .aAsgn(Lc,L,E) => visitE(L,V,VA,visitE(E,V,VA,VA(.aAsgn(Lc,L,E),X))).
    .aCase(Lc,G,Cs,D) =>
      visitA(D,V,VA,
	visitCases(Cs,V,VA,visitA,
	  visitE(G,V,VA,VA(.aCase(Lc,G,Cs,D),X)))).
    .aUnpack(Lc,G,Cs) =>
      visitCases(Cs,V,VA,visitA,
	visitE(G,V,VA,VA(.aUnpack(Lc,G,Cs),X))).
    .aIftte(Lc,G,T,E) =>
      visitA(E,V,VA,visitA(T,V,VA,visitE(G,V,VA,VA(.aIftte(Lc,G,T,E),X)))).
    .aWhile(Lc,G,A) =>
      visitA(A,V,VA,visitE(G,V,VA,VA(.aWhile(Lc,G,A),X))).
    .aRetire(Lc,T,E) =>
      visitE(E,V,VA,visitE(T,V,VA,VA(.aRetire(Lc,T,E),X))).
    .aTry(Lc,A,H) =>
      visitA(H,V,VA,visitA(A,V,VA,VA(.aTry(Lc,A,H),X))).
    .aLtt(Lc,Vr,B,A) =>
      visitA(A,V,VA,visitE(B,V,VA,VA(.aLtt(Lc,Vr,B,A),X))).
    .aVarNmes(Lc,Vs,A) => visitA(A,V,VA,VA(.aVarNmes(Lc,Vs,A),X)).
  }

  visitCases:all e,a ~~
  (cons[cCase[e]],(cExp,a)=>a,(aAction,a)=>a,(e,(cExp,a)=>a,(aAction,a)=>a,a)=>a,a)=>a.
  visitCases([],_,_,_,X) => X.
  visitCases([(Lc,A,E),..Cs],V,VA,VC,X) =>
    visitCases(Cs,V,VA,VC,V(A,VC(E,V,VA,X))).

  public freezeDefn:(cDefn) => data.
  freezeDefn(D) => case D in {
    .fnDef(Lc,Nm,Tp,Vrs,Vl) => mkCons("fun",[Lc::data,.strg(Nm),encodeSig(Tp),
	mkTpl(Vrs//(.cId(Vn,VTp))=>mkTpl([.strg(Vn),encodeSig(VTp)])),
	freezeTerm(Vl)]).
    .vrDef(Lc,Nm,Tp,Vl) => mkCons("glb",[Lc::data,.strg(Nm),encodeSig(Tp),
	freezeTerm(Vl)]).
    .tpDef(Lc,Tp,TpRl,Map) => mkCons("tpe",[Lc::data,encodeSig(Tp),
	.strg(encodeTpRlSignature(TpRl)),
	mkTpl(Map//((Lbl,CTp,Ix))=>mkTpl([.symb(Lbl),encodeSig(CTp),.intgr(Ix)]))]).
    .lblDef(Lc,Lbl,Tp,Ix) => mkCons("cns",[Lc::data,.symb(Lbl),encodeSig(Tp),.intgr(Ix)]).
  }

  freezeTerm:(cExp)=>data.
  freezeTerm(E) => case E in {
    .cVoid(Lc,Tp) => mkCons("void",[Lc::data,encodeSig(Tp)]).
    .cAnon(Lc,Tp) => mkCons("anon",[Lc::data,encodeSig(Tp)]).
    .cVar(Lc,.cId(V,Tp)) => mkCons("var",[Lc::data,.strg(V),encodeSig(Tp)]).
    .cInt(Lc,Ix) => mkCons("int",[Lc::data,.intgr(Ix)]).
    .cChar(Lc,Cx) => mkCons("chr",[Lc::data,.chr(Cx)]).
    .cFloat(Lc,Dx) => mkCons("flt",[Lc::data,.flot(Dx)]).
    .cBig(Lc,Bx) => mkCons("big",[Lc::data,.strg(Bx::string)]).
    .cString(Lc,Sx) => mkCons("str",[Lc::data,.strg(Sx)]).
    .cTerm(Lc,Nm,Args,Tp) => mkCons("term",[Lc::data,.strg(Nm),mkTpl(Args//freezeTerm),
	.strg(encodeSignature(Tp))]).
    .cNth(Lc,T,Ix,Tp) => mkCons("nth",[Lc::data,freezeTerm(T),.intgr(Ix),
	.strg(encodeSignature(Tp))]).
    .cSetNth(Lc,T,Ix,R) => mkCons("setnth",[Lc::data,freezeTerm(T),.intgr(Ix),
	freezeTerm(R)]).
    .cCall(Lc,Nm,Args,Tp) => mkCons("call",[Lc::data,.strg(Nm),mkTpl(Args//freezeTerm),
	.strg(encodeSignature(Tp))]).
    .cECall(Lc,Nm,Args,Tp) => mkCons("ecll",[Lc::data,.strg(Nm),mkTpl(Args//freezeTerm),
	.strg(encodeSignature(Tp))]).
    .cOCall(Lc,Op,Args,Tp) => mkCons("ocll",[Lc::data,freezeTerm(Op),
	mkTpl(Args//freezeTerm),.strg(encodeSignature(Tp))]).
    .cThrow(Lc,X,Tp) => mkCons("thrw",[Lc::data,freezeTerm(X),.strg(encodeSignature(Tp))]).
    .cSeq(Lc,L,R) => mkCons("seq",[Lc::data,freezeTerm(L),freezeTerm(R)]).
    .cCnj(Lc,L,R) => mkCons("cnj",[Lc::data,freezeTerm(L),freezeTerm(R)]).
    .cDsj(Lc,L,R) => mkCons("dsj",[Lc::data,freezeTerm(L),freezeTerm(R)]).
    .cNeg(Lc,R) => mkCons("neg",[Lc::data,freezeTerm(R)]).
    .cCnd(Lc,T,L,R) => mkCons("cnd",[Lc::data,freezeTerm(T),freezeTerm(L),freezeTerm(R)]).
    .cWhere(Lc,L,R) => mkCons("whr",[Lc::data,freezeTerm(L),freezeTerm(R)]).
    .cMatch(Lc,L,R) => mkCons("mtch",[Lc::data,freezeTerm(L),freezeTerm(R)]).
    .cLtt(Lc,.cId(V,Tp),B,X) => mkCons("ltt",[Lc::data,.strg(V),encodeSig(Tp),
	freezeTerm(B),freezeTerm(X)]).
    .cUnpack(Lc,G,Cs,Tp) => mkCons("unpck",[Lc::data,freezeTerm(G),
	freezeCases(Cs,freezeTerm),encodeSig(Tp)]).
    .cCase(Lc,G,Cs,Df,Tp) => mkCons("case",[Lc::data,freezeTerm(G),
	freezeCases(Cs,freezeTerm),freezeTerm(Df),encodeSig(Tp)]).
    .cAbort(Lc,Msg,Tp) => mkCons("abrt",[Lc::data,.strg(Msg),encodeSig(Tp)]).
    .cSusp(Lc,F,V,Tp) => mkCons("susp",[Lc::data,freezeTerm(F),freezeTerm(V),encodeSig(Tp)]).
    .cResume(Lc,F,V,Tp) => mkCons("resme",[Lc::data,freezeTerm(F),freezeTerm(V),encodeSig(Tp)]).
    .cTry(Lc,G,H,Tp) => mkCons("try",[Lc::data,freezeTerm(G),freezeTerm(H),encodeSig(Tp)]).
    .cVarNmes(Lc,Vs,B) => mkCons("vrs",[Lc::data,freezeNames(Vs),freezeTerm(B)]).
    .cValof(Lc,A,Tp) => mkCons("valf",[Lc::data,freezeAct(A),encodeSig(Tp)]).
  }

  freezeCases:all e ~~ (cons[cCase[e]],(e)=>data) => data.
  freezeCases(Cs,F) => mkTpl(Cs//((Lc,Pt,E))=>mkTpl([Lc::data,freezeTerm(Pt),F(E)])).

  freezeNames(Vs) => mkTpl(Vs//((Nm,.cId(Vn,VTp)))=>
      mkTpl([.strg(Nm),.strg(Vn),encodeSig(VTp)])).

  freezeAct:(aAction)=>data.
  freezeAct(A) => case A in {
    .aNop(Lc) => mkCons("nop",[Lc::data]).
    .aSeq(Lc,L,R) => mkCons("seq",[Lc::data,freezeAct(L),freezeAct(R)]).
    .aLbld(Lc,L,I) => mkCons("lbld",[Lc::data,.strg(L),freezeAct(I)]).
    .aBreak(Lc,L) => mkCons("brek",[Lc::data,.strg(L)]).
    .aValis(Lc,V) => mkCons("vls",[Lc::data,freezeTerm(V)]).
    .aThrow(Lc,V) => mkCons("thrw",[Lc::data,freezeTerm(V)]).
    .aPerf(Lc,V) => mkCons("perf",[Lc::data,freezeTerm(V)]).
    .aDefn(Lc,P,V) => mkCons("defn",[Lc::data,freezeTerm(P),freezeTerm(V)]).
    .aAsgn(Lc,P,V) => mkCons("asgn",[Lc::data,freezeTerm(P),freezeTerm(V)]).
    .aCase(Lc,G,C,D) => mkCons("case",[Lc::data,freezeTerm(G),
	freezeCases(C,freezeAct),freezeAct(D)]).
    .aUnpack(Lc,G,C) => mkCons("unpk",[Lc::data,freezeTerm(G),freezeCases(C,freezeAct)]).
    .aIftte(Lc,T,L,R) => mkCons("iftt",[Lc::data,freezeTerm(T),freezeAct(L),freezeAct(R)]).
    .aWhile(Lc,T,I) => mkCons("whle",[Lc::data,freezeTerm(T),freezeAct(I)]).
    .aRetire(Lc,F,V) => mkCons("rtre",[Lc::data,freezeTerm(F),freezeTerm(V)]).
    .aTry(Lc,B,H) => mkCons("try",[Lc::data,freezeAct(B),freezeAct(H)]).
    .aLtt(Lc,.cId(V,Tp),B,X) => mkCons("ltt",[Lc::data,.strg(V),encodeSig(Tp),
	freezeTerm(B),freezeAct(X)]).
    .aVarNmes(Lc,Vs,B) => mkCons("vrs",[Lc::data,freezeNames(Vs),freezeAct(B)]).
    .aAbort(Lc,Msg) => mkCons("abrt",[Lc::data,.strg(Msg)]).
  }

  public thawDefn:(data) => cDefn.
  thawDefn(D) => case D in {
    .term("fun",[Lc,.strg(Nm),Sig,.term(_,Vrs),Vl]) =>
      .fnDef(thawLoc(Lc),Nm,decodeSig(Sig),
	Vrs//(.term(_,[.strg(Vn),VSig]))=>.cId(Vn,decodeSig(VSig)),thawTerm(Vl)).
    .term("glb",[Lc,.strg(V),Sig,Vl]) =>
      .vrDef(thawLoc(Lc),V,decodeSig(Sig),thawTerm(Vl)).
    .term("tpe",[Lc,Sig,.strg(RlSig),.term(_,Map)]) =>
      .tpDef(thawLoc(Lc),decodeSig(Sig),decodeTypeRuleSignature(RlSig),
	Map//(.term(_,[.symb(Lbl),LSig,.intgr(Ix)]))=>(Lbl,decodeSig(LSig),Ix)).
    .term("cns",[Lc,.symb(Lbl),Sig,.intgr(Ix)]) =>
      .lblDef(thawLoc(Lc),Lbl,decodeSig(Sig),Ix).
  }

  thawTerm:(data) => cExp.
  thawTerm(D) => case D in {
    .term("void",[Lc,Sig]) =>
      .cVoid(thawLoc(Lc),decodeSig(Sig)).
    .term("anon",[Lc,Sig]) =>
      .cAnon(thawLoc(Lc),decodeSig(Sig)).
    .term("var",[Lc,.strg(V),Sig]) =>
      .cVar(thawLoc(Lc),.cId(V,decodeSig(Sig))).
    .term("int",[Lc,.intgr(Ix)]) => .cInt(thawLoc(Lc),Ix).
    .term("chr",[Lc,.chr(Ix)]) => .cChar(thawLoc(Lc),Ix).
    .term("flt",[Lc,.flot(Dx)]) => .cFloat(thawLoc(Lc),Dx).
    .term("big",[Lc,.strg(Bx)]) => .cBig(thawLoc(Lc),Bx::bigint).
    .term("str",[Lc,.strg(Sx)]) => .cString(thawLoc(Lc),Sx).
    .term("term",[Lc,.strg(Nm),.term(_,Args),Sig]) =>
      .cTerm(thawLoc(Lc),Nm,Args//thawTerm,decodeSig(Sig)).
    .term("nth",[Lc,E,.intgr(Ix),Sig]) =>
      .cNth(thawLoc(Lc),thawTerm(E),Ix,decodeSig(Sig)).
    .term("setnth",[Lc,E,.intgr(Ix),R]) =>
      .cSetNth(thawLoc(Lc),thawTerm(E),Ix,thawTerm(R)).
    .term("call",[Lc,.strg(Nm),.term(_,Args),Sig]) =>
      .cCall(thawLoc(Lc),Nm,Args//thawTerm,decodeSig(Sig)).
    .term("ecll",[Lc,.strg(Nm),.term(_,Args),Sig]) =>
      .cECall(thawLoc(Lc),Nm,Args//thawTerm,decodeSig(Sig)).
    .term("ocll",[Lc,Op,.term(_,Args),Sig]) =>
      .cOCall(thawLoc(Lc),thawTerm(Op),Args//thawTerm,decodeSig(Sig)).
    .term("thrw",[Lc,Op,Sig]) =>
      .cThrow(thawLoc(Lc),thawTerm(Op),decodeSig(Sig)).
    .term("seq",[Lc,L,R]) =>
      .cSeq(thawLoc(Lc),thawTerm(L),thawTerm(R)).
    .term("cnj",[Lc,L,R]) =>
      .cCnj(thawLoc(Lc),thawTerm(L),thawTerm(R)).
    .term("dsj",[Lc,L,R]) =>
      .cDsj(thawLoc(Lc),thawTerm(L),thawTerm(R)).
    .term("neg",[Lc,R]) =>
      .cNeg(thawLoc(Lc),thawTerm(R)).
    .term("cnd",[Lc,T,L,R]) =>
      .cCnd(thawLoc(Lc),thawTerm(T),thawTerm(L),thawTerm(R)).
    .term("whr",[Lc,L,R]) =>
      .cWhere(thawLoc(Lc),thawTerm(L),thawTerm(R)).
    .term("mtch",[Lc,L,R]) =>
      .cMatch(thawLoc(Lc),thawTerm(L),thawTerm(R)).
    .term("ltt",[Lc,.strg(V),Sig,B,X]) =>
      .cLtt(thawLoc(Lc),.cId(V,decodeSig(Sig)),thawTerm(B),thawTerm(X)).
    .term("unpck",[Lc,G,Cs,Sig]) => .cUnpack(thawLoc(Lc),thawTerm(G),
      thawCases(Cs,thawTerm),decodeSig(Sig)).
    .term("case",[Lc,G,Cs,D,Sig]) => .cCase(thawLoc(Lc),thawTerm(G),
      thawCases(Cs,thawTerm),thawTerm(D),decodeSig(Sig)).
    .term("abrt",[Lc,.strg(M),Sig]) => .cAbort(thawLoc(Lc),M,decodeSig(Sig)).
    .term("susp",[Lc,F,E,Sig]) => .cSusp(thawLoc(Lc),thawTerm(F),thawTerm(E),decodeSig(Sig)).
    .term("resme",[Lc,F,E,Sig]) => .cResume(thawLoc(Lc),thawTerm(F),thawTerm(E),decodeSig(Sig)).
    .term("try",[Lc,E,H,Sig]) => .cTry(thawLoc(Lc),thawTerm(E),thawTerm(H),decodeSig(Sig)).
    .term("vrs",[Lc,Vs,B]) => .cVarNmes(thawLoc(Lc),thawVars(Vs),thawTerm(B)).
  }

  thawLoc(L:data) => L::option[locn].

  thawCases:all e ~~ (data,(data)=>e) => cons[cCase[e]].
  thawCases(.term(_,Args),T) => (Args//(.term(_,[Lc,P,E]))=>
      (thawLoc(Lc),thawTerm(P),T(E))).

  thawVars(.term(_,Vs)) => (Vs//(.term(_,[.strg(Nm),.strg(Vn),Sig]))=>
      (Nm,.cId(Vn,decodeSig(Sig)))).

  thawAct:(data) => aAction.
  thawAct(A) => case A in {
    .term("nop",[Lc]) => .aNop(thawLoc(Lc)).
    .term("seq",[Lc,L,R]) => .aSeq(thawLoc(Lc),thawAct(L),thawAct(R)).
    .term("lbld",[Lc,.strg(L),I]) => .aLbld(thawLoc(Lc),L,thawAct(I)).
    .term("brek",[Lc,.strg(L)]) => .aBreak(thawLoc(Lc),L).
    .term("vls",[Lc,V]) => .aValis(thawLoc(Lc),thawTerm(V)).
    .term("thrw",[Lc,V]) => .aThrow(thawLoc(Lc),thawTerm(V)).
    .term("perf",[Lc,V]) => .aPerf(thawLoc(Lc),thawTerm(V)).
    .term("defn",[Lc,P,V]) => .aDefn(thawLoc(Lc),thawTerm(P),thawTerm(V)).
    .term("asgn",[Lc,P,V]) => .aAsgn(thawLoc(Lc),thawTerm(P),thawTerm(V)).
    .term("case",[Lc,G,C,D]) => .aCase(thawLoc(Lc),thawTerm(G),thawCases(C,thawAct),
      thawAct(D)).
    .term("unpk",[Lc,G,C]) => .aUnpack(thawLoc(Lc),thawTerm(G),thawCases(C,thawAct)).
    .term("iftt",[Lc,T,L,R]) => .aIftte(thawLoc(Lc),thawTerm(T),thawAct(L),thawAct(R)).
    .term("whle",[Lc,T,I]) => .aWhile(thawLoc(Lc),thawTerm(T),thawAct(I)).
    .term("rtre",[Lc,F,V]) => .aRetire(thawLoc(Lc),thawTerm(F),thawTerm(V)).
    .term("try",[Lc,B,H]) => .aTry(thawLoc(Lc),thawAct(B),thawAct(H)).
    .term("vrs",[Lc,Vs,B]) => .aVarNmes(thawLoc(Lc),thawVars(Vs),thawAct(B)).
    .term("ltt",[Lc,.strg(V),Sig,B,X]) =>
      .aLtt(thawLoc(Lc),.cId(V,decodeSig(Sig)),thawTerm(B),thawAct(X)).
    .term("abrt",[Lc,.strg(M)]) => .aAbort(thawLoc(Lc),M).
  }
  



  
}
