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
  | .cClos(option[locn],string,integer,cExp,tipe)
  | .cCall(option[locn],string,cons[cExp],tipe)
  | .cECall(option[locn],string,cons[cExp],tipe)
  | .cOCall(option[locn],cExp,cons[cExp],tipe)
  | .cRaise(option[locn],cExp,cExp,tipe)
  | .cSeq(option[locn],cExp,cExp)
  | .cCnj(option[locn],cExp,cExp)
  | .cDsj(option[locn],cExp,cExp)
  | .cNeg(option[locn],cExp)
  | .cCnd(option[locn],cExp,cExp,cExp)
  | .cLtt(option[locn],cId,cExp,cExp)
  | .cCont(option[locn],cId,cExp,cExp)
  | .cCase(option[locn],cExp,cons[cCase[cExp]],cExp,tipe)
  | .cMatch(option[locn],cExp,cExp)
  | .cVarNmes(option[locn],cons[(string,cId)],cExp)
  | .cAbort(option[locn],string,tipe)
  | .cTry(option[locn],cExp,cExp,cExp,cExp,tipe)
  | .cValof(option[locn],aAction,tipe).
  
  public cId ::= .cId(string,tipe).

  public all e ~~ cCase[e] ~> (option[locn],cExp,e).

  public aAction ::= .aNop(option[locn])
    | .aSeq(option[locn],aAction,aAction)
    | .aLbld(option[locn],string,aAction)
    | .aBreak(option[locn],string)
    | .aValis(option[locn],cExp)
    | .aRaise(option[locn],cExp,cExp)
    | .aPerf(option[locn],cExp)
    | .aSetNth(option[locn],cExp,integer,cExp)
    | .aDefn(option[locn],cExp,cExp)
    | .aAsgn(option[locn],cExp,cExp)
    | .aCase(option[locn],cExp,cons[cCase[aAction]],aAction)
    | .aIftte(option[locn],cExp,aAction,aAction)
    | .aWhile(option[locn],cExp,aAction)
    | .aTry(option[locn],aAction,cExp,cExp,aAction)
    | .aWith(option[locn],aAction,cExp,cExp,aAction)
    | .aLtt(option[locn],cId,cExp,aAction)
    | .aCont(option[locn],cId,cExp,aAction)
    | .aVarNmes(option[locn],cons[(string,cId)],aAction)
    | .aAbort(option[locn],string).

  public cDefn ::= .fnDef(option[locn],string,tipe,cons[cId],cExp) |
    .vrDef(option[locn],string,tipe,cExp)|
    .tpDef(option[locn],tipe,typeRule,cons[(termLbl,tipe,integer)]) |
    .lblDef(option[locn],termLbl,tipe,integer).

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
    .cVoid(_,_) => "void".
    .cAnon(_,_) => "_".
    .cVar(_,.cId(V,VTp)) => "%#(V)/$(arity(VTp))".
    .cInt(_,Ix) => disp(Ix).
    .cChar(_,Ix) => disp(Ix).
    .cBig(_,Ix) => disp(Ix).
    .cFloat(_,Dx) => disp(Dx).
    .cString(_,Sx) => disp(Sx).
    .cECall(_,Op,As,_) => "#(Op)ε(#(dsplyExps(As,Off)*))".
    .cOCall(_,Op,As,_) => "#(dspExp(Op,Off))·(#(dsplyExps(As,Off)*))".
    .cCall(_,Op,As,_) => "#(Op)(#(dsplyExps(As,Off)*))".
    .cTerm(_,Op,As,_) where isTplLbl(Op) => "(#(dsplyExps(As,Off)*))".
    .cTerm(_,Op,As,_) => ".#(Op)(#(dsplyExps(As,Off)*))".
    .cNth(_,O,Ix,_) => "#(dspExp(O,Off)).$(Ix)".
    .cSetNth(_,O,Ix,E) => "(#(dspExp(O,Off)).$(Ix) <- #(dspExp(E,Off)))".
    .cClos(_,Nm,Ar,Fr,_) => "<#(Nm)/$(Ar)\:#(dspExp(Fr,Off))>".
    .cRaise(_,T,E,_) => "#(dspExp(T,Off)) raise #(dspExp(E,Off))".
    .cLtt(_,V,D,I) => valof{
      Off2=Off++"  ";
      valis "let $(V) = #(dspExp(D,Off2)) in\n#(Off2)#(dspExp(I,Off2))"
    }.
    .cCont(_,V,D,I) => valof{
      Off2=Off++"  ";
      valis "cont $(V) = #(dspExp(D,Off2)) in\n#(Off2)#(dspExp(I,Off2))"
    }.
    .cCase(_,E,Cs,D,_)  => valof{
      Off2=Off++"  ";
      valis "case #(dspExp(E,Off)) in {\n#(Off2)#(dspCases(Cs,dspExp,Off2)*)\n#(Off)} else #(dspExp(D,Off))"
    }.
    .cMatch(_,P,E) => "#(dspExp(P,Off)).=#(dspExp(E,Off))".
    .cCnj(_,L,R) => "#(dspExp(L,Off)) && #(dspExp(R,Off))".
    .cDsj(_,L,R) => "(#(dspExp(L,Off)) || #(dspExp(R,Off)))".
    .cCnd(_,T,L,R) => valof{
      Off2=Off++"  ";
      valis "(#(dspExp(T,Off)) ?? #(dspExp(L,Off2)) ||\n #(Off2)#(dspExp(R,Off2)))"
    }.
    .cNeg(_,R) => "~#(dspExp(R,Off))".
    .cSeq(Lc,L,R) => "{#(dspSeq(.cSeq(Lc,L,R),Off++"  "))}".
    .cVarNmes(_,V,E) => "<vars #(dspVrs(V)) in #(dspExp(E,Off))>".
    .cAbort(_,M,_) => "abort #(M)".
    .cTry(_,B,T,E,H,_)=> 
      "(try #(dspExp(T,Off)) in #(dspExp(B,Off)) catch $(E) in #(dspExp(H,Off)))".
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
    .aRaise(_,T,E) => "#(dspExp(T,Off)) raise #(dspExp(E,Off))".
    .aPerf(_,E) => "perform #(dspExp(E,Off))".
    .aSetNth(_,T,Ix,V) => "update #(dspExp(T,Off))[$(Ix)] <- #(dspExp(V,Off))".
    .aDefn(_,P,E) => "#(dspExp(P,Off)) = #(dspExp(E,Off))".
    .aAsgn(_,P,E) => "#(dspExp(P,Off)) := #(dspExp(E,Off))".
    .aCase(_,E,Cs,Df) => valof{
      Off2=Off++"  ";
      valis "case (#(dspExp(E,Off))) in {\n#(Off2)#(dspCases(Cs,dspAct,Off2)*)\n#(Off)} else #(dspAct(Df,Off))"
    }.
    .aIftte(_,C,T,E) => valof{
      Off2=Off++"  ";
      valis "if #(dspExp(C,Off)) then\n#(Off2)#(dspAct(T,Off2)) else\n#(Off2)#(dspAct(E,Off2))"
    }.
    .aWhile(_,C,A) => valof{
      Off2=Off++"  ";
      valis "while #(dspExp(C,Off)) do#(dspAct(A,Off2))"
    }.
    .aTry(_,B,T,V,H) => 
      "{ try #(dspExp(T,Off)) in #(dspAct(B,Off)) catch $(V) in #(dspAct(H,Off))}".
    .aWith(_,B,T,V,H) => 
      "{ try #(dspExp(T,Off)) in #(dspAct(B,Off)) with $(V) in #(dspAct(H,Off))}".
    .aLtt(_,V,D,I) => valof{
      Off2=Off++"  ";
      valis "let $(V) = #(dspExp(D,Off2)) in\n#(Off2)#(dspAct(I,Off2))"
    }.
    .aCont(_,V,D,I) => valof{
      Off2=Off++"  ";
      valis "cont $(V) = #(dspExp(D,Off2)) in\n#(Off2)#(dspAct(I,Off2))"
    }.
    .aVarNmes(_,V,A) => "<vars #(dspVrs(V)) in #(dspAct(A,Off))>".
    .aAbort(_,M) => "abort #(M)".
  }

  dspActSeq(.aSeq(_,L,R),Off) => "\n#(Off)#(dspAct(L,Off));#(dspActSeq(R,Off))".
  dspActSeq(A,Off) => dspAct(A,Off).

  dspCases:all e ~~ (cons[cCase[e]],(e,string)=>string,string)=>cons[string].
  dspCases(Cs,F,Off) => let{
    Gap = "\n"++Off++"| ".
  } in interleave(Cs//((_,P,V))=>"#(dspExp(P,Off))=>#(F(V,Off))",Gap).

  dsplyExps(Es,Off) => interleave(Es//(E)=>dspExp(E,Off),", ").

  dspSeq(.cSeq(_,L,R),Off) => "#(dspSeq(L,Off));#(dspSeq(R,Off))".
  dspSeq(T,Off) => dspExp(T,Off).

  dspVrs(V) => interleave(V//(((N,T))=>"$(N)=$(T)"),", ")*.

  public mcTpl:(option[locn],cons[cExp]) => cExp.
  mcTpl(Lc,Args) => let{
    TpTp = .tupleType(Args//typeOf).
    Ar = size(Args)
  } in .cTerm(Lc,tplLbl(Ar), Args, TpTp).

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
    .cClos(_,L1,A1,F1,_) => .cClos(_,L2,A2,F2,_).=E2 && L1==L2 && A1==A2 && eqTerm(F1,F2).
    .cCall(_,S1,A1,_) => .cCall(_,S2,A2,_).=E2 && S1==S2 && eqs(A1,A2).
    .cECall(_,S1,A1,_) => .cECall(_,S2,A2,_).=E2 && S1==S2 && eqs(A1,A2).
    .cOCall(_,S1,A1,_) => .cOCall(_,S2,A2,_).=E2 && eqTerm(S1,S2) && eqs(A1,A2).
    .cRaise(_,T1,S1,_) => .cRaise(_,T2,S2,_).=E2 && T1==T2 && S1==S2.
    .cNth(_,R1,F1,_) => .cNth(_,R2,F2,_).=E2 && eqTerm(R1,R2) && F1==F2.
    .cSetNth(_,R1,Ix,V1) => .cSetNth(_,R2,Ix,V2).=E2 && eqTerm(R1,R2) && eqTerm(V1,V2).
    .cSeq(_,L1,R1) => .cSeq(_,L2,R2).=E2 && eqTerm(L1,L2) && eqTerm(R1,R2).
    .cCnj(_,L1,R1) => .cCnj(_,L2,R2).=E2 && eqTerm(L1,L2) && eqTerm(R1,R2).
    .cDsj(_,L1,R1) => .cDsj(_,L2,R2).=E2 && eqTerm(L1,L2) && eqTerm(R1,R2).
    .cNeg(_,R1) => .cNeg(_,R2).=E2 && eqTerm(R1,R2).
    .cCnd(_,T1,L1,R1) => .cCnd(_,T2,L2,R2).=E2 &&
	eqTerm(T1,T2) && eqTerm(L1,L2) && eqTerm(R1,R2).
    .cLtt(_,T1,L1,R1) => .cLtt(_,T2,L2,R2).=E2 &&
	T1==T2 && eqTerm(L1,L2) && eqTerm(R1,R2).
    .cCont(_,T1,L1,R1) => .cCont(_,T2,L2,R2).=E2 &&
	T1==T2 && eqTerm(L1,L2) && eqTerm(R1,R2).
    .cCase(_,S1,C1,D1,_) => .cCase(_,S2,C2,D2,_).=E2 &&
	eqTerm(S1,S2) && eqCs(C1,eqTerm,C2) && eqTerm(D1,D2).
    .cMatch(_,P1,V1) => .cMatch(_,P2,V2).=E2 && eqTerm(V1,V2) && eqTerm(P1,P2).
    .cAbort(_,M1,T1) => .cAbort(_,M2,T2).=E2 && M1==M2 && T1==T2.
    .cTry(_,M1,T1,E1,H1,_) => .cTry(_,M2,T2,E2,H2,_).=E2 &&
	eqTerm(T1,T2) && eqTerm(M1,M2) && eqTerm(E1,E2) && eqTerm(H1,H2).
    .cValof(_,A1,_) => .cValof(_,A2,_).=E2 && eqAct(A1,A2).
    .cVarNmes(_,N1,V1) => .cVarNmes(_,N2,V2).=E2 && eqVs(N1,N2) && eqTerm(V1,V2).
    _ default => .false
  }

  eqs(L1,L2) => case L1 in {
    [] => L2==[].
    [E1,..S1] => [E2,..S2].=L2 ?? eqTerm(E1,E2) && eqs(S1,S2) || .false.
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
    .aRaise(_,T1,E1) => .aRaise(_,T2,E2).=A2 && eqTerm(T1,T2) && eqTerm(E1,E2).
    .aPerf(_,E1) => .aPerf(_,E2).=A2 && eqTerm(E1,E2).
    .aSetNth(_,V1,Ix1,T1) => .aSetNth(_,V2,Ix2,T2).=A2 && eqTerm(V1,V2) && Ix1==Ix2 && eqTerm(T1,T2).
    .aDefn(_,E1,V1) => .aDefn(_,E2,V2).=A2 && eqTerm(E1,E2) && eqTerm(V1,V2).
    .aAsgn(_,E1,V1) => .aAsgn(_,E2,V2).=A2 && eqTerm(E1,E2) && eqTerm(V1,V2).
    .aCase(_,S1,C1,D1) => .aCase(_,S2,C2,D2).=A2 &&
	eqTerm(S1,S2) && eqCs(C1,eqAct,C2) && eqAct(D1,D2).
    .aIftte(_,C1,L1,R1) => .aIftte(_,C2,L2,R2).=A2 &&
	eqTerm(C1,C2) && eqAct(L1,L2) && eqAct(R1,R2).
    .aWhile(_,C1,L1) => .aWhile(_,C2,L2).=A2 &&
	eqTerm(C1,C2) && eqAct(L1,L2).
    .aTry(_,M1,T1,E1,H1) => .aTry(_,M2,T2,E2,H2).=A2 && eqAct(M1,M2) &&
	eqTerm(T1,T2) && eqTerm(E1,E2) && eqAct(H1,H2).
    .aWith(_,M1,T1,E1,H1) => .aWith(_,M2,T2,E2,H2).=A2 && eqAct(M1,M2) &&
	eqTerm(T1,T2) && eqTerm(E1,E2) && eqAct(H1,H2).
    .aLtt(_,V1,D1,Ac1) => .aLtt(_,V2,D2,Ac2).=A2 &&
	V1==V2 && eqTerm(D1,D2) && eqAct(Ac1,Ac2).
    .aCont(_,V1,D1,Ac1) => .aCont(_,V2,D2,Ac2).=A2 &&
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
      .cVoid(Lc,_) => Lc.
      .cAnon(Lc,_) => Lc.
      .cVar(Lc,_) => Lc.
      .cInt(Lc,_) => Lc.
      .cBig(Lc,_) => Lc.
      .cChar(Lc,_) => Lc.
      .cFloat(Lc,_) => Lc.
      .cString(Lc,_) => Lc.
      .cNth(Lc,_,_,_) => Lc.
      .cSetNth(Lc,_,_,_) => Lc.
      .cTerm(Lc,_,_,_) => Lc.
      .cClos(Lc,_,_,_,_) => Lc.
      .cMatch(Lc,_,_) => Lc.
      .cLtt(Lc,_,_,_) => Lc.
      .cCont(Lc,_,_,_) => Lc.
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
      .cTry(Lc,_,_,_,_,_) => Lc.
      .cRaise(Lc,_,_,_) => Lc.
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
      .cClos(_,_,_,_,Tp) => Tp.
      .cECall(_,_,_,Tp) => Tp.
      .cOCall(_,_,_,Tp) => Tp.
      .cCall(_,_,_,Tp) => Tp.
      .cRaise(_,_,_,Tp) => Tp.
      .cNth(_,_,_,Tp) => Tp.
      .cSetNth(_,T,_,_) => tpOf(T).
      .cSeq(_,_,R) => tpOf(R).
      .cCnj(_,_,_) => boolType.
      .cDsj(_,_,_) => boolType.
      .cNeg(_,_) => boolType.
      .cLtt(_,_,_,E) => tpOf(E).
      .cCont(_,_,_,E) => tpOf(E).
      .cCase(_,_,_,_,Tp) => Tp.
      .cCnd(_,_,L,_) => tpOf(L).
      .cMatch(_,_,_) => boolType.
      .cTry(_,_,_,_,_,T) => T.
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
    disp(.cId(Nm,Tp)) => "%#(Nm)\:$(Tp)".
  }

  public implementation hasLoc[aAction] => {
    locOf(Ac) => case Ac in {
      .aNop(Lc) => Lc.
      .aSeq(Lc,_,_) => Lc.
      .aLbld(Lc,_,_) => Lc.
      .aBreak(Lc,_) => Lc.
      .aValis(Lc,_) => Lc.
      .aRaise(Lc,_,_) => Lc.
      .aPerf(Lc,_) => Lc.
      .aSetNth(Lc,_,_,_) => Lc.
      .aDefn(Lc,_,_) => Lc.
      .aAsgn(Lc,_,_) => Lc.
      .aCase(Lc,_,_,_) => Lc.
      .aIftte(Lc,_,_,_) => Lc.
      .aWhile(Lc,_,_) => Lc.
      .aTry(Lc,_,_,_,_) => Lc.
      .aWith(Lc,_,_,_,_) => Lc.
      .aLtt(Lc,_,_,_) => Lc.
      .aCont(Lc,_,_,_) => Lc.
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
      .cVoid(_,_) => .some(.symb(.tLbl("void",0))).
      .cInt(_,Ix) => .some(.intgr(Ix)).
      .cTerm(_,Nm,Args,_) where NArgs ?= mapArgs(Args,[]) =>
	.some(.term(Nm,NArgs)).
      .cClos(_,L,A,F,_) where NF ?= _coerce(F) => .some(.clos(.tLbl(L,A),NF)).
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

  rwTerm:(cExp,(cExp)=>option[cDefn])=>cExp.
  rwTerm(Trm,Tst) =>
    .vrDef(_,_,_,Vl) ?= Tst(Trm) ?? Vl || case Trm in {
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
      .cClos(Lc,L,A,F,Tp) => .cClos(Lc,L,A,rwTerm(F,Tst),Tp).
      .cCall(Lc,Op,Args,Tp) => .cCall(Lc,Op,Args//(A)=>rwTerm(A,Tst),Tp).
      .cOCall(Lc,Op,Args,Tp) => .cOCall(Lc,rwTerm(Op,Tst),Args//(A)=>rwTerm(A,Tst),Tp).
      .cECall(Lc,Op,Args,Tp) => .cECall(Lc,Op,Args//(A)=>rwTerm(A,Tst),Tp).
      .cRaise(Lc,Th,E,Tp) =>.cRaise(Lc,rwTerm(Th,Tst),rwTerm(E,Tst),Tp).
      .cSeq(Lc,L,R) =>.cSeq(Lc,rwTerm(L,Tst),rwTerm(R,Tst)).
      .cCnj(Lc,L,R) =>.cCnj(Lc,rwTerm(L,Tst),rwTerm(R,Tst)).
      .cDsj(Lc,L,R) =>.cDsj(Lc,rwTerm(L,Tst),rwTerm(R,Tst)).
      .cNeg(Lc,R) =>.cNeg(Lc,rwTerm(R,Tst)).
      .cCnd(Lc,G,L,R) =>.cCnd(Lc,rwTerm(G,Tst),rwTerm(L,Tst),rwTerm(R,Tst)).
      .cLtt(Lc,V,D,E) =>.cLtt(Lc,V,rwTerm(D,Tst),rwTerm(E,dropVar(cName(V),Tst))).
      .cCont(Lc,V,D,E) =>.cCont(Lc,V,rwTerm(D,Tst),rwTerm(E,dropVar(cName(V),Tst))).
      .cCase(Lc,Sel,Cases,Dflt,Tp) => .cCase(Lc,rwTerm(Sel,Tst),
	Cases//(C)=>rwCase(C,Tst,rwTerm),rwTerm(Dflt,Tst),Tp).
      .cMatch(Lc,P,E) => .cMatch(Lc,rwTerm(P,Tst),rwTerm(E,Tst)).
      .cTry(Lc,B,T,E,H,Tp) => .cTry(Lc,rwTerm(B,Tst),rwTerm(T,Tst),rwTerm(E,Tst),rwTerm(H,Tst),Tp).
      .cVarNmes(Lc,Vs,E) => .cVarNmes(Lc,Vs,rwTerm(E,Tst)).
      .cValof(Lc,A,Tp) => .cValof(Lc,rwAct(A,Tst),Tp).
      .cAbort(Lc,Ms,Tp) => .cAbort(Lc,Ms,Tp).
    }.

  rwAct:(aAction,(cExp)=>option[cDefn])=>aAction.
  rwAct(Ac,Tst) => case Ac in {
    .aNop(Lc) => .aNop(Lc).
    .aSeq(Lc,L,R) => .aSeq(Lc,rwAct(L,Tst),rwAct(R,Tst)).
    .aLbld(Lc,L,A) => .aLbld(Lc,L,rwAct(A,Tst)).
    .aBreak(Lc,L) => .aBreak(Lc,L).
    .aValis(Lc,E) => .aValis(Lc,rwTerm(E,Tst)).
    .aRaise(Lc,T,E) => .aRaise(Lc,rwTerm(T,Tst),rwTerm(E,Tst)).
    .aPerf(Lc,E) => .aPerf(Lc,rwTerm(E,Tst)).
    .aSetNth(Lc,V,Ix,E) => .aSetNth(Lc,rwTerm(V,Tst),Ix,rwTerm(E,Tst)).
    .aDefn(Lc,V,E) => .aDefn(Lc,rwTerm(V,Tst),rwTerm(E,Tst)).
    .aAsgn(Lc,V,E) => .aAsgn(Lc,rwTerm(V,Tst),rwTerm(E,Tst)).
    .aCase(Lc,G,Cs,D) => .aCase(Lc,rwTerm(G,Tst),Cs//(C)=>rwCase(C,Tst,rwAct),rwAct(D,Tst)).
    .aIftte(Lc,C,L,R) => .aIftte(Lc,rwTerm(C,Tst),rwAct(L,Tst),rwAct(R,Tst)).
    .aWhile(Lc,C,B) => .aWhile(Lc,rwTerm(C,Tst),rwAct(B,Tst)).
    .aTry(Lc,B,T,E,Hs) => .aTry(Lc,rwAct(B,Tst),rwTerm(T,Tst),rwTerm(E,Tst),rwAct(Hs,Tst)).
    .aWith(Lc,B,T,E,Hs) => .aWith(Lc,rwAct(B,Tst),rwTerm(T,Tst),rwTerm(E,Tst),rwAct(Hs,Tst)).
    .aLtt(Lc,V,D,A) =>.aLtt(Lc,V,rwTerm(D,Tst),rwAct(A,dropVar(cName(V),Tst))).
    .aCont(Lc,V,D,A) =>.aCont(Lc,V,rwTerm(D,Tst),rwAct(A,dropVar(cName(V),Tst))).
    .aVarNmes(Lc,Vs,E) => .aVarNmes(Lc,Vs,rwAct(E,Tst)).
    .aAbort(Lc,Ms) => .aAbort(Lc,Ms).
  }

  dropVar:(string,(cExp)=>option[cDefn])=>(cExp)=>option[cDefn].
  dropVar(Nm,Tst) => let{
    test(.cVar(_,.cId(Nm,_))) => .none.
    test(T) default => Tst(T)
  } in test.
  
  rwTerms:(cons[cExp],(cExp)=>option[cDefn])=>cons[cExp].
  rwTerms(Els,Tst) => (Els//(E)=>rwTerm(E,Tst)).

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

  public freshenE:(cExp,map[termLbl,cExp])=>cExp.
  freshenE(E,Mp) => frshnE(E,[Mp]).

  scope ~> cons[map[termLbl,cExp]].

  frshnE:(cExp,scope)=>cExp.
  frshnE(Trm,Sc) => case Trm in {
    .cVoid(Lc,Tp) => .cVoid(Lc,Tp)
    | .cAnon(Lc,Tp) => .cAnon(Lc,Tp)
    | .cVar(Lc,V) => (Rp ?= hasBinding(lName(V),Sc) ?? Rp || Trm)
    | .cInt(Lc,Ix) => .cInt(Lc,Ix)
    | .cBig(Lc,Ix) => .cBig(Lc,Ix)
    | .cFloat(Lc,Dx) => .cFloat(Lc,Dx)
    | .cChar(Lc,Cx) => .cChar(Lc,Cx)
    | .cString(Lc,Sx) => .cString(Lc,Sx)
    | .cTerm(Lc,Op,Args,Tp) => .cTerm(Lc,Op,frshnEs(Args,Sc),Tp)
    | .cNth(Lc,R,Ix,Tp) =>.cNth(Lc,frshnE(R,Sc),Ix,Tp)
    | .cSetNth(Lc,R,Ix,E) =>.cSetNth(Lc,frshnE(R,Sc),Ix,frshnE(E,Sc))
    | .cClos(Lc,L,A,F,Tp) => .cClos(Lc,L,A,frshnE(F,Sc),Tp)
    | .cCall(Lc,Op,Args,Tp) => .cCall(Lc,Op,frshnEs(Args,Sc),Tp)
    | .cOCall(Lc,Op,Args,Tp) => .cOCall(Lc,frshnE(Op,Sc),frshnEs(Args,Sc),Tp)
    | .cECall(Lc,Op,Args,Tp) => .cECall(Lc,Op,frshnEs(Args,Sc),Tp)
    | .cRaise(Lc,Th,E,Tp) =>.cRaise(Lc,frshnE(Th,Sc),frshnE(E,Sc),Tp)
    | .cSeq(Lc,L,R) =>.cSeq(Lc,frshnE(L,Sc),frshnE(R,Sc))
    | .cCnd(Lc,G,L,R) => valof{
      Sc1 = newVars(glVars(G,[]),Sc);
      valis .cCnd(Lc,frshnE(G,Sc1),frshnE(L,Sc1),frshnE(R,Sc))
    }
    | .cCnj(Lc,L,R) => valof{
      Sc1 = newVars(glVars(L,[]),Sc);
      valis .cCnj(Lc,frshnE(L,Sc1),frshnE(R,Sc1))
    }
    | .cDsj(Lc,L,R) =>valof{
      Sc1 = newVars(glVars(Trm,[]),Sc);
      valis .cDsj(Lc,frshnE(L,Sc1),frshnE(R,Sc1))
    }
    | .cNeg(Lc,R) =>.cNeg(Lc,frshnE(R,Sc))
    | .cMatch(Lc,P,E) => .cMatch(Lc,frshnE(P,Sc),frshnE(E,Sc))
    | .cLtt(Lc,V,D,E) =>.cLtt(Lc,V,frshnE(D,Sc),frshnE(E,pushScope(Sc)))
    | .cCont(Lc,V,D,E) =>.cCont(Lc,V,frshnE(D,Sc),frshnE(E,pushScope(Sc)))
    | .cCase(Lc,Sel,Cs,Dflt,Tp) => .cCase(Lc,frshnE(Sel,Sc),frCases(Cs,Sc,frshnE),frshnE(Dflt,Sc),Tp)
    | .cTry(Lc,B,T,E,H,Tp) => valof{
      Sc0 = pushScope(Sc);
      Sc1 = newVars(ptnVrs(E,[]),Sc0);
      Sc2 = newVars(ptnVrs(T,[]),Sc0);
      valis .cTry(Lc,frshnE(B,Sc2),frshnE(T,Sc2),frshnE(E,Sc1),frshnE(H,Sc1),Tp)
    }
    | .cVarNmes(Lc,Vs,E) => .cVarNmes(Lc,Vs,frshnE(E,Sc))
    | .cValof(Lc,A,Tp) => .cValof(Lc,frshnA(A,pushScope(Sc)),Tp)
    | .cAbort(Lc,Ms,Tp) => .cAbort(Lc,Ms,Tp)
  }.

  hasBinding:(termLbl,scope) => option[cExp].
  hasBinding(_,[]) => .none.
  hasBinding(V,[M,.._]) where Vl ?= M[V] => .some(Vl).
  hasBinding(V,[_,..Ms]) => hasBinding(V,Ms).

  newVars:(set[cId],scope) => scope.
  newVars(Vrs,[Mp,..Ms]) => let{
    def:(cId,map[termLbl,cExp]) => map[termLbl,cExp].
    def(V,M) where Nm .= lName(V) => (_ ?= M[Nm] ?? M || M[Nm->newVar(V)]).
  } in [foldLeft(def,Mp,Vrs),..Ms].

  newVar(.cId(Nm,Tp)) => .cVar(.none,.cId(genId(Nm),Tp)).

  pushScope:(scope) => scope.
  pushScope(Sc) => [[],..Sc].

  public lName:(cId) => termLbl.
  lName(.cId(Nm,Tp)) => .tLbl(Nm,arity(Tp)).

  frshnA:(aAction,scope)=>aAction.
  frshnA(Ac,Sc) => case Ac in {
    .aNop(Lc) => .aNop(Lc)
    | .aSeq(Lc,.aDefn(LL,P,E),R) => valof{
      Sc1 = newVars(ptnVrs(P,[]),Sc);
      valis .aSeq(Lc,.aDefn(LL,frshnE(P,Sc1),frshnE(E,Sc)),frshnA(R,Sc1))
    }
    | .aSeq(Lc,L,R) => .aSeq(Lc,frshnA(L,Sc),frshnA(R,Sc))
    | .aLbld(Lc,L,A) => .aLbld(Lc,L,frshnA(A,Sc))
    | .aBreak(Lc,L) => .aBreak(Lc,L)
    | .aValis(Lc,E) => .aValis(Lc,frshnE(E,Sc))
    | .aRaise(Lc,T,E) => .aRaise(Lc,frshnE(T,Sc),frshnE(E,Sc))
    | .aPerf(Lc,E) => .aPerf(Lc,frshnE(E,Sc))
    | .aSetNth(Lc,V,Ix,E) => .aSetNth(Lc,frshnE(V,Sc),Ix,frshnE(E,Sc))
    | .aDefn(Lc,V,E) => .aDefn(Lc,frshnE(V,Sc),frshnE(E,Sc))
    | .aAsgn(Lc,V,E) => .aAsgn(Lc,frshnE(V,Sc),frshnE(E,Sc))
    | .aCase(Lc,G,Cs,D) => .aCase(Lc,frshnE(G,Sc),frCases(Cs,Sc,frshnA),frshnA(D,Sc))
    | .aIftte(Lc,C,L,R) => valof{
      Sc0 = pushScope(Sc);
      Sc1 = newVars(glVars(C,[]),Sc0);
      valis .aIftte(Lc,frshnE(C,Sc1),frshnA(L,Sc1),frshnA(R,Sc))
    }
    | .aWhile(Lc,C,B) => valof{
      Sc0 = pushScope(Sc);
      Sc1 = newVars(glVars(C,[]),Sc0);
      valis .aWhile(Lc,frshnE(C,Sc1),frshnA(B,Sc1))
    }
    | .aWith(Lc,B,T,E,Hs) => valof{
      Sc0 = pushScope(Sc);
      Sc1 = newVars(ptnVrs(E,[]),Sc0);
      Sc2 = newVars(ptnVrs(T,[]),Sc0);
      valis .aWith(Lc,frshnA(B,Sc2),frshnE(T,Sc2), frshnE(E,Sc1),frshnA(Hs,Sc1))
    }
    | .aLtt(Lc,V,D,A) =>.aLtt(Lc,V,frshnE(D,Sc),frshnA(A,pushScope(Sc)))
    | .aCont(Lc,V,D,A) =>.aCont(Lc,V,frshnE(D,Sc),frshnA(A,pushScope(Sc)))
    | .aVarNmes(Lc,Vs,E) => .aVarNmes(Lc,Vs,frshnA(E,Sc))
    | .aAbort(Lc,Ms) => .aAbort(Lc,Ms)
  }

  frshnEs:(cons[cExp],scope)=>cons[cExp].
  frshnEs(Els,Sc) => (Els//(E)=>frshnE(E,Sc)).

  frCases:all e ~~ (cons[cCase[e]],scope,(e,scope)=>e) => cons[cCase[e]].
  frCases(Cs,Sc,F) => (Cs//((Lc,Ptn,Rep)) => valof{
      Sc1=newVars(ptnVrs(Ptn,[]),pushScope(Sc));
      valis (Lc,frshnE(Ptn,Sc1),F(Rep,Sc1))
    }).

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
    .cClos(_,_,_,F,_) => isGround(F).
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
    varNames:(option[locn],cons[(string,cId)],e)=>e.
    pullWhere:(e) => (e,option[cExp]).
    mkLtt:(option[locn],cId,cExp,e) => e.
    mkCont:(option[locn],cId,cExp,e) => e.
  }

  public implementation reform[cExp] => {.
    mkCond(Lc,Tst,Th,El) => valof{
      if .cCnd(_,T1,Th1,El1).=Th && El1==El then
	valis .cCnd(Lc,.cCnj(Lc,Tst,T1),Th1,El1) else
      valis .cCnd(Lc,Tst,Th,El).
    }

    varNames(Lc,Bnds,Val) => .cVarNmes(Lc,Bnds,Val).

    pullWhere(.cTerm(Lc,Lbl,Args,Tp)) where (NArgs,Gx) .= pullWheres(Args) =>
      (.cTerm(Lc,Lbl,NArgs,Tp),Gx).
    pullWhere(Exp) default => (Exp,.none).

    pullWheres([]) => ([],.none).
    pullWheres([A,..As]) where (NA,NG).=pullWhere(A) && (NAs,Gx) .= pullWheres(As) =>
      ([NA,..NAs],mergeGoal(locOf(A),NG,Gx)).

    mkCase(Lc,Tst,[(PLc,Ptn,Val)],Deflt) => mkCond(Lc,.cMatch(PLc,Ptn,Tst),Val,Deflt).
    mkCase(Lc,V,Cases,Deflt) => .cCase(Lc,V,Cases,Deflt,typeOf(Deflt)).

    mkLtt(Lc,V,E,X) => .cLtt(Lc,V,E,X).

    mkCont(Lc,V,E,X) => .cCont(Lc,V,E,X).
  .}

  public implementation reform[aAction] => {
    mkCond(Lc,Tst,Th,El) where
	.aIftte(Lc0,T1,Th1,El1).=Th && El1==El => .aIftte(Lc0,.cCnj(Lc,Tst,T1),Th1,El1).
    mkCond(Lc,.cMatch(_,.cAnon(_,_),_),Th,_) => Th.
    mkCond(Lc,.cMatch(_,.cVar(_,Vr),Vl),Th,_) => .aLtt(Lc,Vr,Vl,Th).
    mkCond(Lc,Tst,Th,El) => .aIftte(Lc,Tst,Th,El).

    varNames(Lc,Bnds,Val) => .aVarNmes(Lc,Bnds,Val).

    pullWhere(A) => (A,.none).

    mkCase(Lc,V,Cases,Deflt) => .aCase(Lc,V,Cases,Deflt).

    mkLtt(Lc,V,E,X) => .aLtt(Lc,V,E,X).

    mkCont(Lc,V,E,X) => .aCont(Lc,V,E,X).
  }

  dfVars:(cons[cDefn],set[cId])=>set[cId].
  dfVars([.fnDef(_,Nm,Tp,_,_),..Ds],D) =>
    dfVars(Ds,D\+.cId(Nm,Tp)).
  dfVars([.vrDef(_,Nm,Tp,_),..Ds],D) =>
    dfVars(Ds,D\+.cId(Nm,Tp)).
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
	    reportError("$(.fnDef(Lc,Nm,Tp,Args,Val)) not valid",Lc)
	  }
	}.
	.vrDef(Lc,Nm,Tp,Val) => {
	  if ~validE(Val,D) then{
	    reportError("$(.vrDef(Lc,Nm,Tp,Val)) not valid",Lc)
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
    .cAnon(Lc,_) => valof{
      reportError("anons not allowed in expressions",Lc);
      valis .false
    }.
    .cVar(Lc,V) =>  V .<. Vrs ?? .true || valof{
      reportError("variable $(V)\:$(typeOf(V)) not in scope",Lc);
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
    .cClos(_,_,_,F,_) => validE(F,Vrs).
    .cCall(_,_,Args,_) => {? E in Args *> validE(E,Vrs) ?}.
    .cECall(_,_,Args,_) => {? E in Args *> validE(E,Vrs) ?}.
    .cOCall(_,Op,Args,_) => validE(Op,Vrs) && {? E in Args *> validE(E,Vrs) ?}.
    .cRaise(_,T,E,_) => validE(T,Vrs) && validE(E,Vrs).
    .cSeq(_,L,R) => validE(L,Vrs) && validE(R,Vrs).
    .cCnj(_,L,R) => valof{
      V1 = glVars(L,Vrs);
      valis validE(L,V1) && validE(R,V1)
    }.
    .cDsj(_,L,R) => validE(L,Vrs) && validE(R,Vrs).
    .cNeg(_,R) => validE(R,Vrs).
    .cCnd(_,Ts,L,R) => valof{
      V1 = glVars(Ts,Vrs);
      valis validE(Ts,V1) && validE(L,V1) && validE(R,Vrs)
    }.
    .cLtt(_,B,V,E) => validE(V,Vrs) && validE(E,Vrs\+B).
    .cCont(_,B,V,E) => validE(V,Vrs) && validE(E,Vrs\+B).
    .cCase(_,G,Cs,Df,_) => validE(G,Vrs) && validCases(Cs,validE,Vrs) && validE(Df,Vrs).
    .cMatch(_,V,E) => valof{
      V1 = glVars(E,Vrs);
      valis validP(V,V1) && validE(E,V1)
    }.
    .cVarNmes(_,_,E) => validE(E,Vrs).
    .cAbort(_,_,_) => .true.
    .cTry(_,B,T,E,H,_) => valof{
      V1 = ptnVrs(T,Vrs);
      V2 = ptnVrs(E,Vrs);
      valis validE(B,V1) && validE(T,V1) && validE(E,V2) && validE(H,V2)
    }.
    .cValof(_,A,_) => validA(A,Vrs).
  }

  validP:(cExp,set[cId]) => boolean.
  validP(Exp,Vrs) => case Exp in {
    .cVoid(Lc,Tp) => .true.
    .cAnon(_,_) => .true.
    .cVar(Lc,V) =>  .true.
    .cInt(_,_) => .true.
    .cBig(_,_) => .true.
    .cChar(_,_) => .true.
    .cString(_,_) => .true.
    .cFloat(_,_) => .true.
    .cTerm(_,_,Args,_) => {? E in Args *> validP(E,Vrs) ?}.
    _ default => valof{
      reportError("invalid pattern: $(Exp)",locOf(Exp));
      valis .false.
    }
  }
  
  validCases:all e ~~ (cons[cCase[e]],(e,set[cId])=>boolean,set[cId]) => boolean.
  validCases([],_,_) => .true.
  validCases([(_,A,E),..Cs],P,Vrs) => valof{
    D1 = ptnVrs(A,Vrs);
    valis validP(A,D1) && P(E,D1) && validCases(Cs,P,Vrs)
  }

  validA:(aAction,set[cId])=>boolean.
  validA(Ac,Vrs) => case Ac in {
    .aNop(_) => .true.
    .aSeq(_,A1,A2) => valof{
      if .aDefn(_,P,V) .= A1 then{
	V1 = ptnVrs(P,Vrs);
	valis validP(P,V1) && validE(V,Vrs) && validA(A2,V1);
      } else {
	valis validA(A1,Vrs) && validA(A2,Vrs)
      }
    }.
    .aLbld(_,_,A) => validA(A,Vrs).
    .aBreak(_,L) => .true.
    .aValis(_,E) => validE(E,Vrs).
    .aRaise(_,T,E) => validE(T,Vrs) && validE(E,Vrs).
    .aPerf(_,E) => validE(E,Vrs).
    .aSetNth(_,V,_,E) => validE(V,Vrs) && validE(E,Vrs).
    .aDefn(_,P,E) => 
      validP(P,ptnVrs(P,Vrs)) && validE(E,Vrs).
    .aAsgn(_,L,V) => validE(L,Vrs) && validE(V,Vrs).
    .aCase(_,G,Cs,Df) =>
      validE(G,Vrs) && validCases(Cs,validA,Vrs) && validA(Df,Vrs).
    .aIftte(_,G,Th,E) => valof{
      D1 = glVars(G,Vrs);
      valis validE(G,D1) && validA(Th,D1) && validA(E,Vrs)
    }.
    .aWhile(_,G,A) => valof{
      D1 = glVars(G,Vrs);
      valis validE(G,D1) && validA(A,D1)
    }.
    .aTry(_,B,Th,E,Hs) => valof{
      V1 = ptnVrs(Th,Vrs);
      V2 = ptnVrs(E,Vrs);
      valis validA(B,V1) && validE(Th,V1) && validE(E,V2) && validA(Hs,V2)
    }.
    .aWith(_,B,Th,E,Hs) => valof{
      V1 = ptnVrs(Th,Vrs);
      V2 = ptnVrs(E,Vrs);
      valis validA(B,V1) && validE(Th,V1) && validE(E,V2) && validA(Hs,V2)
    }.
    .aLtt(_,B,V,A) => validE(V,Vrs) && validA(A,Vrs\+B).
    .aCont(_,B,V,A) => validE(V,Vrs) && validA(A,Vrs\+B).
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
  }

  public glVars:(cExp,set[cId])=>set[cId].
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

  presentInA(Ac,C,T) => C(Ac) ??
    .true ||
    case Ac in {
    .aNop(_) => .false.
    .aSeq(_,A1,A2) => presentInA(A1,C,T) || presentInA(A2,C,T).
    .aLbld(_,_,A) => presentInA(A,C,T).
    .aBreak(_,L) => .false.
    .aValis(_,E) => presentInE(E,C,T).
    .aRaise(_,Th,E) => presentInE(Th,C,T) || presentInE(E,C,T).
    .aPerf(_,E) => presentInE(E,C,T).
    .aSetNth(_,V,_,E) => presentInE(V,C,T) || presentInE(E,C,T).
    .aDefn(_,_,E) => presentInE(E,C,T).
    .aAsgn(_,L,V) => presentInE(L,C,T) || presentInE(V,C,T).
    .aCase(_,G,Cs,D) =>
      presentInE(G,C,T) || presentInCases(Cs,presentInA,C,T) || presentInA(D,C,T).
    .aIftte(_,G,Th,E) =>
      presentInE(G,C,T) || presentInA(Th,C,T) || presentInA(E,C,T).
    .aWhile(_,G,B) =>
      presentInE(G,C,T) || presentInA(B,C,T).
    .aTry(_,B,Th,E,H) => presentInA(B,C,T) || presentInE(Th,C,T) || presentInE(E,C,T) || presentInA(H,C,T).
    .aWith(_,B,Th,E,H) =>
      presentInA(B,C,T) || presentInE(Th,C,T) || presentInE(E,C,T) || presentInA(H,C,T).
    .aLtt(_,_,V,B) => presentInE(V,C,T) || presentInA(B,C,T).
    .aCont(_,_,V,B) => presentInE(V,C,T) || presentInA(B,C,T).
    .aVarNmes(_,_,B) => presentInA(B,C,T).
    .aAbort(_,_) => .false.
  }.

  presentInE:(cExp,(aAction)=>boolean,(cExp)=>boolean) => boolean.
  presentInE(T,A,C) => C(T) ?? .true || case T in {
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
    .cClos(_,_,_,F,_) => presentInE(F,A,C).
    .cCall(_,_,Args,_) => {? E in Args && presentInE(E,A,C) ?}.
    .cECall(_,_,Args,_) => {? E in Args && presentInE(E,A,C) ?}.
    .cOCall(_,Op,Args,_) =>
      presentInE(Op,A,C) || {? E in Args && presentInE(E,A,C) ?}.
    .cRaise(_,Th,E,_) => presentInE(Th,A,C) || presentInE(E,A,C).
    .cSeq(_,L,R) => presentInE(L,A,C) || presentInE(R,A,C).
    .cCnj(_,L,R) => presentInE(L,A,C) || presentInE(R,A,C).
    .cDsj(_,L,R) => presentInE(L,A,C) || presentInE(R,A,C).
    .cNeg(_,R) => presentInE(R,A,C).
    .cCnd(_,Ts,L,R) =>
      presentInE(Ts,A,C) || presentInE(L,A,C) || presentInE(R,A,C).
    .cLtt(_,_,V,E) =>
      presentInE(V,A,C) || presentInE(E,A,C).
    .cCont(_,_,V,E) =>
      presentInE(V,A,C) || presentInE(E,A,C).
    .cCase(_,G,Cs,D,_) =>
      presentInE(G,A,C) || presentInCases(Cs,presentInE,A,C) || presentInE(D,A,C).
    .cMatch(_,V,E) =>
      presentInE(V,A,C) || presentInE(E,A,C).
    .cVarNmes(_,_,E) =>
      presentInE(E,A,C).
    .cAbort(_,_,_) => .false.
    .cTry(_,B,Th,E,H,_) =>
      presentInE(B,A,C) || presentInE(Th,A,C) || presentInE(E,A,C) || presentInE(H,A,C).
    .cValof(_,Act,_) => presentInA(Act,A,C).
  }

  presentInCases:all e ~~ (cons[cCase[e]],(e,(aAction)=>boolean,(cExp)=>boolean)=>boolean,
    (aAction)=>boolean,(cExp)=>boolean)=>boolean.
  presentInCases([],_,_,_) => .false.
  presentInCases([(_,A,E),..Cs],P,C,T) =>
    presentInE(A,C,T) || P(E,C,T) || presentInCases(Cs,P,C,T).

  public freezeDefn:(cDefn) => data.
  freezeDefn(D) => case D in {
    .fnDef(Lc,Nm,Tp,Vrs,Vl) => mkCons("fun",[Lc::data,.strg(Nm),encodeSig(Tp),
	mkTpl(Vrs//(.cId(Vn,VTp))=>mkTpl([.strg(Vn),encodeSig(VTp)])),
	frzeExp(Vl)]).
    .vrDef(Lc,Nm,Tp,Vl) => mkCons("glb",[Lc::data,.strg(Nm),encodeSig(Tp),
	frzeExp(Vl)]).
    .tpDef(Lc,Tp,TpRl,Map) => mkCons("tpe",[Lc::data,encodeSig(Tp),
	.strg(encodeTpRlSignature(TpRl)),
	mkTpl(Map//((Lbl,CTp,Ix))=>mkTpl([.symb(Lbl),encodeSig(CTp),.intgr(Ix)]))]).
    .lblDef(Lc,Lbl,Tp,Ix) => mkCons("cns",[Lc::data,.symb(Lbl),encodeSig(Tp),.intgr(Ix)]).
  }

  frzeExp:(cExp)=>data.
  frzeExp(Ex) => case Ex in {
    .cVoid(Lc,Tp) => mkCons("void",[Lc::data,encodeSig(Tp)]).
    .cAnon(Lc,Tp) => mkCons("anon",[Lc::data,encodeSig(Tp)]).
    .cVar(Lc,.cId(V,Tp)) => mkCons("var",[Lc::data,.strg(V),encodeSig(Tp)]).
    .cInt(Lc,Ix) => mkCons("int",[Lc::data,.intgr(Ix)]).
    .cChar(Lc,Cx) => mkCons("chr",[Lc::data,.chr(Cx)]).
    .cFloat(Lc,Dx) => mkCons("flt",[Lc::data,.flot(Dx)]).
    .cBig(Lc,Bx) => mkCons("big",[Lc::data,.strg(Bx::string)]).
    .cString(Lc,Sx) => mkCons("str",[Lc::data,.strg(Sx)]).
    .cTerm(Lc,Nm,Args,Tp) => mkCons("term",[Lc::data,.strg(Nm),mkTpl(Args//frzeExp),
	.strg(encodeSignature(Tp))]).
    .cNth(Lc,T,Ix,Tp) => mkCons("nth",[Lc::data,frzeExp(T),.intgr(Ix),
	.strg(encodeSignature(Tp))]).
    .cSetNth(Lc,T,Ix,R) => mkCons("setnth",[Lc::data,frzeExp(T),.intgr(Ix),
	frzeExp(R)]).
    .cClos(Lc,N,A,F,Tp) => mkCons("clos",[Lc::data,.strg(N),.intgr(A),frzeExp(F),
	.strg(encodeSignature(Tp))]).
    .cCall(Lc,Nm,Args,Tp) => mkCons("call",[Lc::data,.strg(Nm),mkTpl(Args//frzeExp),
	.strg(encodeSignature(Tp))]).
    .cECall(Lc,Nm,Args,Tp) => mkCons("ecll",[Lc::data,.strg(Nm),mkTpl(Args//frzeExp),
	.strg(encodeSignature(Tp))]).
    .cOCall(Lc,Op,Args,Tp) => mkCons("ocll",[Lc::data,frzeExp(Op),
	mkTpl(Args//frzeExp),.strg(encodeSignature(Tp))]).
    .cRaise(Lc,Th,X,Tp) => mkCons("rais",[Lc::data,frzeExp(Th),
	frzeExp(X),.strg(encodeSignature(Tp))]).
    .cSeq(Lc,L,R) => mkCons("seq",[Lc::data,frzeExp(L),frzeExp(R)]).
    .cCnj(Lc,L,R) => mkCons("cnj",[Lc::data,frzeExp(L),frzeExp(R)]).
    .cDsj(Lc,L,R) => mkCons("dsj",[Lc::data,frzeExp(L),frzeExp(R)]).
    .cNeg(Lc,R) => mkCons("neg",[Lc::data,frzeExp(R)]).
    .cCnd(Lc,T,L,R) => mkCons("cnd",[Lc::data,frzeExp(T),frzeExp(L),frzeExp(R)]).
    .cMatch(Lc,L,R) => mkCons("mtch",[Lc::data,frzeExp(L),frzeExp(R)]).
    .cLtt(Lc,.cId(V,Tp),B,X) => mkCons("ltt",[Lc::data,.strg(V),encodeSig(Tp),
	frzeExp(B),frzeExp(X)]).
    .cCont(Lc,.cId(V,Tp),B,X) => mkCons("cont",[Lc::data,.strg(V),encodeSig(Tp),
	frzeExp(B),frzeExp(X)]).
    .cCase(Lc,G,Cs,Df,Tp) => mkCons("case",[Lc::data,frzeExp(G),
	freezeCases(Cs,frzeExp),frzeExp(Df),encodeSig(Tp)]).
    .cAbort(Lc,Msg,Tp) => mkCons("abrt",[Lc::data,.strg(Msg),encodeSig(Tp)]).
    .cTry(Lc,B,T,E,H,Tp) => mkCons("try",[Lc::data,frzeExp(B),frzeExp(T),frzeExp(E),frzeExp(H),encodeSig(Tp)]).
    .cVarNmes(Lc,Vs,B) => mkCons("vrs",[Lc::data,freezeNames(Vs),frzeExp(B)]).
    .cValof(Lc,A,Tp) => mkCons("valof",[Lc::data,frzeAct(A),encodeSig(Tp)]).
  }

  freezeCases:all e ~~ (cons[cCase[e]],(e)=>data) => data.
  freezeCases(Cs,F) => mkTpl(Cs//((Lc,Pt,E))=>mkTpl([Lc::data,frzeExp(Pt),F(E)])).

  freezeNames(Vs) => mkTpl(Vs//((Nm,.cId(Vn,VTp)))=>
      mkTpl([.strg(Nm),.strg(Vn),encodeSig(VTp)])).

  frzeAct:(aAction)=>data.
  frzeAct(Ac) => case Ac in {
    .aNop(Lc) => mkCons("nop",[Lc::data]).
    .aSeq(Lc,L,R) => mkCons("seq",[Lc::data,frzeAct(L),frzeAct(R)]).
    .aLbld(Lc,L,I) => mkCons("lbld",[Lc::data,.strg(L),frzeAct(I)]).
    .aBreak(Lc,L) => mkCons("brek",[Lc::data,.strg(L)]).
    .aValis(Lc,V) => mkCons("vls",[Lc::data,frzeExp(V)]).
    .aRaise(Lc,T,V) => mkCons("rais",[Lc::data,frzeExp(T),frzeExp(V)]).
    .aPerf(Lc,V) => mkCons("perf",[Lc::data,frzeExp(V)]).
    .aSetNth(Lc,V,Ix,E) => mkCons("setix",[Lc::data,frzeExp(V),.intgr(Ix),frzeExp(E)]).
    .aDefn(Lc,P,V) => mkCons("defn",[Lc::data,frzeExp(P),frzeExp(V)]).
    .aAsgn(Lc,P,V) => mkCons("asgn",[Lc::data,frzeExp(P),frzeExp(V)]).
    .aCase(Lc,G,C,D) => mkCons("case",[Lc::data,frzeExp(G),
	freezeCases(C,frzeAct),frzeAct(D)]).
    .aIftte(Lc,T,L,R) => mkCons("iftt",[Lc::data,frzeExp(T),frzeAct(L),frzeAct(R)]).
    .aWhile(Lc,T,I) => mkCons("whle",[Lc::data,frzeExp(T),frzeAct(I)]).
    .aTry(Lc,B,T,E,H) => mkCons("try",[Lc::data,frzeAct(B),frzeExp(T),frzeExp(E),frzeAct(H)]).
    .aWith(Lc,B,T,E,H) => mkCons("hndl",[Lc::data,frzeAct(B),frzeExp(T),frzeExp(E),frzeAct(H)]).
    .aLtt(Lc,.cId(V,Tp),B,X) => mkCons("ltt",[Lc::data,.strg(V),encodeSig(Tp),
	frzeExp(B),frzeAct(X)]).
    .aCont(Lc,.cId(V,Tp),B,X) => mkCons("cont",[Lc::data,.strg(V),encodeSig(Tp),
	frzeExp(B),frzeAct(X)]).
    .aVarNmes(Lc,Vs,B) => mkCons("vrs",[Lc::data,freezeNames(Vs),frzeAct(B)]).
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
    .term("clos",[Lc,.strg(N),.intgr(A),F,Sig]) =>
      .cClos(thawLoc(Lc),N,A,thawTerm(F),decodeSig(Sig)).
    .term("call",[Lc,.strg(Nm),.term(_,Args),Sig]) =>
      .cCall(thawLoc(Lc),Nm,Args//thawTerm,decodeSig(Sig)).
    .term("ecll",[Lc,.strg(Nm),.term(_,Args),Sig]) =>
      .cECall(thawLoc(Lc),Nm,Args//thawTerm,decodeSig(Sig)).
    .term("ocll",[Lc,Op,.term(_,Args),Sig]) =>
      .cOCall(thawLoc(Lc),thawTerm(Op),Args//thawTerm,decodeSig(Sig)).
    .term("rais",[Lc,Th,Op,Sig]) =>
      .cRaise(thawLoc(Lc),thawTerm(Th),thawTerm(Op),decodeSig(Sig)).
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
    .term("mtch",[Lc,L,R]) =>
      .cMatch(thawLoc(Lc),thawTerm(L),thawTerm(R)).
    .term("ltt",[Lc,.strg(V),Sig,B,X]) =>
      .cLtt(thawLoc(Lc),.cId(V,decodeSig(Sig)),thawTerm(B),thawTerm(X)).
    .term("cont",[Lc,.strg(V),Sig,B,X]) =>
      .cCont(thawLoc(Lc),.cId(V,decodeSig(Sig)),thawTerm(B),thawTerm(X)).
    .term("case",[Lc,G,Cs,Df,Sig]) => .cCase(thawLoc(Lc),thawTerm(G),
      thawCases(Cs,thawTerm),thawTerm(Df),decodeSig(Sig)).
    .term("abrt",[Lc,.strg(M),Sig]) => .cAbort(thawLoc(Lc),M,decodeSig(Sig)).
    .term("try",[Lc,B,Th,E,H,Sig]) => .cTry(thawLoc(Lc),thawTerm(B),thawTerm(Th),thawTerm(E),thawTerm(H),decodeSig(Sig)).
    .term("vrs",[Lc,Vs,B]) => .cVarNmes(thawLoc(Lc),thawVars(Vs),thawTerm(B)).
    .term("valof",[Lc,A,T]) => .cValof(thawLoc(Lc),thawAct(A),decodeSig(T)).
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
    .term("rais",[Lc,T,V]) => .aRaise(thawLoc(Lc),thawTerm(T),thawTerm(V)).
    .term("perf",[Lc,V]) => .aPerf(thawLoc(Lc),thawTerm(V)).
    .term("setix",[Lc,V,.intgr(Ix),E]) => .aSetNth(thawLoc(Lc),thawTerm(V),Ix,thawTerm(E)).
    .term("defn",[Lc,P,V]) => .aDefn(thawLoc(Lc),thawTerm(P),thawTerm(V)).
    .term("asgn",[Lc,P,V]) => .aAsgn(thawLoc(Lc),thawTerm(P),thawTerm(V)).
    .term("case",[Lc,G,C,D]) => .aCase(thawLoc(Lc),thawTerm(G),thawCases(C,thawAct),
      thawAct(D)).
    .term("iftt",[Lc,T,L,R]) => .aIftte(thawLoc(Lc),thawTerm(T),thawAct(L),thawAct(R)).
    .term("whle",[Lc,T,I]) => .aWhile(thawLoc(Lc),thawTerm(T),thawAct(I)).
    .term("try",[Lc,B,T,E,H]) => .aTry(thawLoc(Lc),thawAct(B),thawTerm(T),thawTerm(E),thawAct(H)).
    .term("hndl",[Lc,B,T,E,H]) => .aWith(thawLoc(Lc),thawAct(B),thawTerm(T),thawTerm(E),thawAct(H)).
    .term("vrs",[Lc,Vs,B]) => .aVarNmes(thawLoc(Lc),thawVars(Vs),thawAct(B)).
    .term("ltt",[Lc,.strg(V),Sig,B,X]) =>
      .aLtt(thawLoc(Lc),.cId(V,decodeSig(Sig)),thawTerm(B),thawAct(X)).
    .term("cont",[Lc,.strg(V),Sig,B,X]) =>
      .aCont(thawLoc(Lc),.cId(V,decodeSig(Sig)),thawTerm(B),thawAct(X)).
    .term("abrt",[Lc,.strg(M)]) => .aAbort(thawLoc(Lc),M).
  }
}
