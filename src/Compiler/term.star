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
  
  public cExp ::= cVoid(option[locn],tipe)
    | cAnon(option[locn],tipe)
    | cVar(option[locn],cId)
    | cInt(option[locn],integer)
    | cChar(option[locn],char)
    | cBig(option[locn],bigint)
    | cFloat(option[locn],float)
    | cString(option[locn],string)
    | cTerm(option[locn],string,cons[cExp],tipe)
    | cNth(option[locn],cExp,integer,tipe)
    | cSetNth(option[locn],cExp,integer,cExp)
    | cCall(option[locn],string,cons[cExp],tipe)
    | cECall(option[locn],string,cons[cExp],tipe)
    | cOCall(option[locn],cExp,cons[cExp],tipe)
    | cThrow(option[locn],cExp,tipe)
    | cSeq(option[locn],cExp,cExp)
    | cCnj(option[locn],cExp,cExp)
    | cDsj(option[locn],cExp,cExp)
    | cNeg(option[locn],cExp)
    | cCnd(option[locn],cExp,cExp,cExp)
    | cLtt(option[locn],cId,cExp,cExp)
    | cUnpack(option[locn],cExp,cons[cCase[cExp]],tipe)
    | cCase(option[locn],cExp,cons[cCase[cExp]],cExp,tipe)
    | cWhere(option[locn],cExp,cExp)
    | cMatch(option[locn],cExp,cExp)
    | cVarNmes(option[locn],cons[(string,cId)],cExp)
    | cAbort(option[locn],string,tipe)
    | cTask(option[locn],cExp,tipe)
    | cSusp(option[locn],cExp,cExp,tipe)
    | cResume(option[locn],cExp,cExp,tipe)
    | cTry(option[locn],cExp,cExp,tipe)
    | cValof(option[locn],aAction,tipe).
  
  public cId ::= cId(string,tipe).

  public all e ~~ cCase[e] ~> (option[locn],cExp,e).

  public aAction ::= aNop(option[locn])
    | aSeq(option[locn],aAction,aAction)
    | aLbld(option[locn],string,aAction)
    | aBreak(option[locn],string)
    | aValis(option[locn],cExp)
    | aThrow(option[locn],cExp)
    | aPerf(option[locn],cExp)
    | aDefn(option[locn],cExp,cExp)
    | aAsgn(option[locn],cExp,cExp)
    | aCase(option[locn],cExp,cons[cCase[aAction]],aAction)
    | aUnpack(option[locn],cExp,cons[cCase[aAction]])
    | aIftte(option[locn],cExp,aAction,aAction)
    | aWhile(option[locn],cExp,aAction)
    | aRetire(option[locn],cExp,cExp)
    | aTry(option[locn],aAction,aAction)
    | aLtt(option[locn],cId,cExp,aAction)
    | aVarNmes(option[locn],cons[(string,cId)],aAction)
    | aAbort(option[locn],string).

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
  dspDef(fnDef(Lc,Nm,Tp,Args,Rep),Off) =>
    "fun: $(Lc) #(Nm)(#(interleave(Args//disp,",")*)) => #(dspExp(Rep,Off))".
  dspDef(vrDef(Lc,Nm,Tp,Rep),Off) =>
    "var: $(Lc) #(Nm)=#(dspExp(Rep,Off))".
  dspDef(tpDef(Lc,Tp,TpRl,Map),Off) =>
    "tpe: $(Lc) $(TpRl) with $(Map)".
  dspDef(lblDef(Lc,Lbl,Tp,Ix),Off) =>
    "lbl: $(Lc) $(Lbl)\:$(Tp)@$(Ix)".

  dspExp:(cExp,string) => string.
  dspExp(cVar(_,cId(V,VTp)),_) => "%#(V)/$(arity(VTp))".
  dspExp(cInt(_,Ix),_) => disp(Ix).
  dspExp(cChar(_,Ix),_) => disp(Ix).
  dspExp(cBig(_,Ix),_) => disp(Ix).
  dspExp(cFloat(_,Dx),_) => disp(Dx).
  dspExp(cString(_,Sx),_) => disp(Sx).
  dspExp(cVoid(_,_),_) => "void".
  dspExp(cECall(_,Op,As,_),Off) => "#(Op)ε(#(dsplyExps(As,Off)*))".
  dspExp(cOCall(_,Op,As,_),Off) => "#(dspExp(Op,Off))·(#(dsplyExps(As,Off)*))".
  dspExp(cCall(_,Op,As,_),Off) => "#(Op)(#(dsplyExps(As,Off)*))".
  dspExp(cTerm(_,Op,As,_),Off) where isTplLbl(Op) => "‹#(dsplyExps(As,Off)*)›".
  dspExp(cTerm(_,Op,As,_),Off) => "#(Op)‹#(dsplyExps(As,Off)*)›".
  dspExp(cNth(_,O,Ix,_),Off) => "#(dspExp(O,Off)).$(Ix)".
  dspExp(cSetNth(_,O,Ix,E),Off) => "(#(dspExp(O,Off)).$(Ix) := #(dspExp(E,Off)))".
  dspExp(cThrow(_,E,_),Off) => "throw #(dspExp(E,Off))".
  dspExp(cLtt(_,V,D,I),Off) where Off2.=Off++"  " =>
    "let $(V) = #(dspExp(D,Off2)) in\n#(Off2)#(dspExp(I,Off2))".
  dspExp(cCase(_,E,Cs,D,_),Off) where Off2.=Off++"  "=>
    "case #(dspExp(E,Off)) in {\n#(Off2)#(dspCases(Cs,dspExp,Off2)*)\n#(Off)} else #(dspExp(D,Off))".
  dspExp(cUnpack(_,E,Cs,_),Off) where Off2.=Off++"  "=>
    "unpack #(dspExp(E,Off)) in {\n#(Off2)#(dspCases(Cs,dspExp,Off2)*)\n#(Off)}".
  dspExp(cMatch(_,P,E),Off) => "#(dspExp(P,Off)).=#(dspExp(E,Off))".
  dspExp(cWhere(_,T,C),Off) => "#(dspExp(T,Off)) where #(dspExp(C,Off++"  "))".
  dspExp(cCnj(_,L,R),Off) => "#(dspExp(L,Off))&&#(dspExp(R,Off))".
  dspExp(cDsj(_,L,R),Off) => "(#(dspExp(L,Off))||#(dspExp(R,Off)))".
  dspExp(cCnd(_,T,L,R),Off) where Off2 .= Off++"  " =>
    "(#(dspExp(T,Off)) ? #(dspExp(L,Off2)) ||\n #(Off2)#(dspExp(R,Off2)))".
  dspExp(cNeg(_,R),Off) => "~#(dspExp(R,Off))".
  dspExp(cSeq(Lc,L,R),Off) => "{#(dspSeq(cSeq(Lc,L,R),Off++"  "))}".
  dspExp(cVarNmes(_,V,E),Off) => "<vars #(dspVrs(V)) in #(dspExp(E,Off))>".
  dspExp(cAbort(_,M,_),Off) => "abort #(M)".
  dspExp(cTask(_,A,_),Off) => "task #(dspExp(A,Off))".
  dspExp(cSusp(_,T,E,_),Off) => "#(dspExp(T,Off)) suspend #(dspExp(E,Off))".
  dspExp(cResume(_,T,E,_),Off) => "#(dspExp(T,Off)) resume #(dspExp(E,Off))".
  dspExp(cTry(_,E,H,_),Off) where Off2.=Off++"  " =>
    "try #(dspExp(E,Off)) catch #(dspExp(H,Off2))".
  dspExp(cValof(_,A,_),Off) => "valof #(dspAct(A,Off))".

  dspAct:(aAction,string)=>string.
  dspAct(aNop(_),_) => "{}".
  dspAct(aSeq(_,L,R),Off)  where Off2.=Off++"  " =>
    "{ #(dspAct(L,Off2)); #(dspActSeq(R,Off2)) }".
  dspAct(aLbld(_,Lb,A),Off) => "#(Lb) : #(dspAct(A,Off))".
  dspAct(aBreak(_,Lb),_Off) => "break #(Lb)".
  dspAct(aValis(_,E),Off) => "valis #(dspExp(E,Off))".
  dspAct(aThrow(_,E),Off) => "throw #(dspExp(E,Off))".
  dspAct(aPerf(_,E),Off) => "perform #(dspExp(E,Off))".
  dspAct(aDefn(_,P,E),Off) => "#(dspExp(P,Off)) = #(dspExp(E,Off))".
  dspAct(aAsgn(_,P,E),Off) => "#(dspExp(P,Off)) := #(dspExp(E,Off))".
  dspAct(aCase(_,E,Cs,Df),Off) where Off2.=Off++"  "=>
    "case #(dspExp(E,Off)) in {\n#(Off2)#(dspCases(Cs,dspAct,Off2)*)\n#(Off)} else #(dspAct(Df,Off))".
  dspAct(aUnpack(_,E,Cs),Off) where Off2.=Off++"  " =>
    "unpack #(dspExp(E,Off)) in {\n#(Off2)#(dspCases(Cs,dspAct,Off2)*)\n#(Off)}".
  dspAct(aIftte(_,C,T,E),Off)  where Off2.=Off++"  " =>
    "if #(dspExp(C,Off)) then\n#(Off2)#(dspAct(T,Off2))else\n#(Off2)#(dspAct(E,Off2))".
  dspAct(aWhile(_,C,A),Off)  where Off2.=Off++"  " =>
    "while #(dspExp(C,Off)) do#(dspAct(A,Off2))".
  dspAct(aRetire(_,T,E),Off) =>
    "#(dspExp(T,Off)) retire #(dspExp(E,Off))".
  dspAct(aTry(_,E,H),Off) where Off2.=Off++"  " =>
    "try #(dspAct(E,Off)) catch #(dspAct(H,Off))".
  dspAct(aLtt(_,V,D,I),Off) where Off2.=Off++"  " =>
    "let $(V) = #(dspExp(D,Off2)) in\n#(Off2)#(dspAct(I,Off2))".
  dspAct(aVarNmes(_,V,A),Off) => "<vars #(dspVrs(V)) in #(dspAct(A,Off))>".
  dspAct(aAbort(_,M),Off) => "abort #(M)".

  dspActSeq(aSeq(_,L,R),Off) => "\n#(Off)#(dspAct(L,Off));#(dspActSeq(R,Off))".
  dspActSeq(A,Off) => dspAct(A,Off).

  dspCases:all e ~~ (cons[cCase[e]],(e,string)=>string,string)=>cons[string].
  dspCases(Cs,F,Off) => let{
    Gap = ";\n"++Off.
  } in interleave(Cs//((_,P,V))=>"#(dspExp(P,Off))=>#(F(V,Off))",Gap).

  dsplyExps(Es,Off) => interleave(Es//(E)=>dspExp(E,Off),", ").

  dspSeq(cSeq(_,L,R),Off) => "#(dspSeq(L,Off));#(dspSeq(R,Off))".
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
    cId(N1,T1) == cId(N2,T2) => N1==N2.
  }

  public implementation hashable[cId] => {
    hash(cId(N,T)) => hash(N).
  }

  eqTerm(cAnon(_,T1),cAnon(_,T2)) => T1==T2.
  eqTerm(cVoid(_,T1),cVoid(_,T2)) => T1==T2.
  eqTerm(cVar(_,V1),cVar(_,V2)) => V1==V2.
  eqTerm(cInt(_,N1),cInt(_,N2)) => N1==N2.
  eqTerm(cChar(_,N1),cChar(_,N2)) => N1==N2.
  eqTerm(cFloat(_,N1),cFloat(_,N2)) => N1==N2.
  eqTerm(cString(_,S1),cString(_,S2)) => S1==S2.
  eqTerm(cTerm(_,S1,A1,_),cTerm(_,S2,A2,_)) => S1==S2 && eqs(A1,A2).
  eqTerm(cCall(_,S1,A1,_),cCall(_,S2,A2,_)) => S1==S2 && eqs(A1,A2).
  eqTerm(cECall(_,S1,A1,_),cECall(_,S2,A2,_)) => S1==S2 && eqs(A1,A2).
  eqTerm(cOCall(_,S1,A1,_),cOCall(_,S2,A2,_)) => eqTerm(S1,S2) && eqs(A1,A2).
  eqTerm(cThrow(_,S1,_),cThrow(_,S2,_)) => S1==S2.
  eqTerm(cNth(_,R1,F1,_),cNth(_,R2,F2,_)) => eqTerm(R1,R2) && F1==F2.
  eqTerm(cSetNth(_,R1,Ix,E1),cSetNth(_,R2,Ix,E2)) => eqTerm(R1,R2) && eqTerm(E1,E2).
  eqTerm(cSeq(_,L1,R1),cSeq(_,L2,R2)) => eqTerm(L1,L2) && eqTerm(R1,R2).
  eqTerm(cCnj(_,L1,R1),cCnj(_,L2,R2)) => eqTerm(L1,L2) && eqTerm(R1,R2).
  eqTerm(cDsj(_,L1,R1),cDsj(_,L2,R2)) => eqTerm(L1,L2) && eqTerm(R1,R2).
  eqTerm(cNeg(_,R1),cNeg(_,R2)) => eqTerm(R1,R2).
  eqTerm(cCnd(_,T1,L1,R1),cCnd(_,T2,L2,R2)) =>
    eqTerm(T1,T2) && eqTerm(L1,L2) && eqTerm(R1,R2).
  eqTerm(cLtt(_,T1,L1,R1),cLtt(_,T2,L2,R2)) =>
    T1==T2 && eqTerm(L1,L2) && eqTerm(R1,R2).
  eqTerm(cUnpack(_,S1,C1,_),cUnpack(_,S2,C2,_)) =>
    eqTerm(S1,S2) && eqCs(C1,eqTerm,C2).
  eqTerm(cCase(_,S1,C1,D1,_),cCase(_,S2,C2,D2,_)) =>
    eqTerm(S1,S2) && eqCs(C1,eqTerm,C2) && eqTerm(D1,D2).
  eqTerm(cWhere(_,E1,C1),cWhere(_,E2,C2)) => eqTerm(E1,E2) && eqTerm(C1,C2).
  eqTerm(cMatch(_,P1,E1),cMatch(_,P2,E2)) => eqTerm(E1,E2) && eqTerm(P1,P2).
  eqTerm(cAbort(_,M1,T1),cAbort(_,M2,T2)) => M1==M2 && T1==T2.
  eqTerm(cTask(_,M1,_),cTask(_,M2,_)) => eqTerm(M1,M2).
  eqTerm(cSusp(_,T1,E1,_),cSusp(_,T2,E2,_)) => eqTerm(T1,T2) && eqTerm(E1,E2).
  eqTerm(cResume(_,T1,E1,_),cResume(_,T2,E2,_)) => eqTerm(T1,T2) && eqTerm(E1,E2).
  eqTerm(cTry(_,M1,H1,_),cTry(_,M2,H2,_)) => eqTerm(M1,M2) && eqTerm(H1,H2).
  eqTerm(cValof(_,A1,_),cValof(_,A2,_)) => eqAct(A1,A2).
  eqTerm(cVarNmes(_,V1,E1),cVarNmes(_,V2,E2)) => eqVs(V1,V2) && eqTerm(E1,E2).
  eqTerm(_,_) default => .false.
  
  eqs([],[]) => .true.
  eqs([E1,..S1],[E2,..S2]) => eqTerm(E1,E2) && eqs(S1,S2).
  eqs(_,_) default => .false.

  eqCs:all e ~~ (cons[cCase[e]],(e,e)=>boolean,cons[cCase[e]])=>boolean.
  eqCs([],_,[]) => .true.
  eqCs([(_,N1,E1),..S1],P,[(_,N2,E2),..S2]) => eqTerm(N1,N2) && P(E1,E2) && eqCs(S1,P,S2).
  eqCs(_,_,_) default => .false.

  eqVs([],[]) => .true.
  eqVs([(N1,E1),..S1],[(N2,E2),..S2]) => N1==N2 && E1==E2 && eqVs(S1,S2).
  eqVs(_,_) default => .false.

  eqAct(aNop(_),aNop(_)) => .true.
  eqAct(aSeq(_,L1,R1),aSeq(_,L2,R2)) => eqAct(L1,L2) && eqAct(R1,R2).
  eqAct(aLbld(_,L1,A1),aLbld(_,L2,A2)) => L1==L2 && eqAct(A1,A2).
  eqAct(aBreak(_,L1),aBreak(_,L2)) => L1==L2.
  eqAct(aValis(_,E1),aValis(_,E2)) => eqTerm(E1,E2).
  eqAct(aThrow(_,E1),aThrow(_,E2)) => eqTerm(E1,E2).
  eqAct(aPerf(_,E1),aPerf(_,E2)) => eqTerm(E1,E2).
  eqAct(aDefn(_,E1,V1),aDefn(_,E2,V2)) => eqTerm(E1,E2) && eqTerm(V1,V2).
  eqAct(aAsgn(_,E1,V1),aAsgn(_,E2,V2)) => eqTerm(E1,E2) && eqTerm(V1,V2).
  eqAct(aCase(_,S1,C1,D1),aCase(_,S2,C2,D2)) =>
    eqTerm(S1,S2) && eqCs(C1,eqAct,C2) && eqAct(D1,D2).
  eqAct(aUnpack(_,S1,C1),aUnpack(_,S2,C2)) =>
    eqTerm(S1,S2) && eqCs(C1,eqAct,C2).
  eqAct(aIftte(_,C1,L1,R1),aIftte(_,C2,L2,R2)) =>
    eqTerm(C1,C2) && eqAct(L1,L2) && eqAct(R1,R2).
  eqAct(aWhile(_,C1,L1),aWhile(_,C2,L2)) =>
    eqTerm(C1,C2) && eqAct(L1,L2).
  eqAct(aRetire(_,E1,V1),aRetire(_,E2,V2)) => eqTerm(E1,E2) && eqTerm(V1,V2).
  eqAct(aTry(_,M1,H1),aTry(_,M2,H2)) => eqAct(M1,M2) && eqAct(H1,H2).
  eqAct(aLtt(_,V1,D1,A1),aLtt(_,V2,D2,A2)) => V1==V2 && eqTerm(D1,D2) && eqAct(A1,A2).
  eqAct(aVarNmes(_,V1,A1),aVarNmes(_,V2,A2)) => eqVs(V1,V2) && eqAct(A1,A2).
  eqAct(aAbort(_,M1),aAbort(_,M2)) => M1==M2.

  public implementation equality[cExp] => {
    X == Y => eqTerm(X,Y)
  }

  public implementation equality[aAction] => {
    X == Y => eqAct(X,Y)
  }

  public implementation hasLoc[cExp] => {
    locOf(cVar(Lc,_)) => Lc.
    locOf(cVoid(Lc,_)) => Lc.
    locOf(cInt(Lc,_)) => Lc.
    locOf(cBig(Lc,_)) => Lc.
    locOf(cChar(Lc,_)) => Lc.
    locOf(cFloat(Lc,_)) => Lc.
    locOf(cString(Lc,_)) => Lc.
    locOf(cNth(Lc,_,_,_)) => Lc.
    locOf(cSetNth(Lc,_,_,_)) => Lc.
    locOf(cTerm(Lc,_,_,_)) => Lc.
    locOf(cWhere(Lc,_,_)) => Lc.
    locOf(cMatch(Lc,_,_)) => Lc.
    locOf(cLtt(Lc,_,_,_)) => Lc.
    locOf(cUnpack(Lc,_,_,_)) => Lc.
    locOf(cCase(Lc,_,_,_,_)) => Lc.
    locOf(cCall(Lc,_,_,_))=>Lc.
    locOf(cECall(Lc,_,_,_))=>Lc.
    locOf(cOCall(Lc,_,_,_))=>Lc.
    locOf(cSeq(Lc,_,_)) => Lc.
    locOf(cCnj(Lc,_,_)) => Lc.
    locOf(cDsj(Lc,_,_)) => Lc.
    locOf(cNeg(Lc,_)) => Lc.
    locOf(cCnd(Lc,_,_,_)) => Lc.
    locOf(cAbort(Lc,_,_)) => Lc.
    locOf(cVarNmes(Lc,_,_)) => Lc.
    locOf(cTask(Lc,_,_)) => Lc.
    locOf(cSusp(Lc,_,_,_)) => Lc.
    locOf(cResume(Lc,_,_,_)) => Lc.
    locOf(cTry(Lc,_,_,_)) => Lc.
    locOf(cValof(Lc,_,_)) => Lc.
  }

  public implementation hasType[cExp] => let{.
    tpOf(cVoid(_,Tp)) => Tp.
    tpOf(cAnon(_,Tp)) => Tp.
    tpOf(cVar(_,V)) => typeOf(V).
    tpOf(cInt(_,_)) => intType.
    tpOf(cBig(_,_)) => bigintType.
    tpOf(cChar(_,_)) => chrType.
    tpOf(cFloat(_,_)) => fltType.
    tpOf(cString(_,_)) => strType.
    tpOf(cTerm(_,_,_,Tp)) => Tp.
    tpOf(cECall(_,_,_,Tp)) => Tp.
    tpOf(cOCall(_,_,_,Tp)) => Tp.
    tpOf(cCall(_,_,_,Tp)) => Tp.
    tpOf(cThrow(_,_,Tp)) => Tp.
    tpOf(cNth(_,_,_,Tp)) => Tp.
    tpOf(cSetNth(_,T,_,_)) => tpOf(T).
    tpOf(cSeq(_,_,R)) => tpOf(R).
    tpOf(cCnj(_,_,_)) => boolType.
    tpOf(cDsj(_,_,_)) => boolType.
    tpOf(cNeg(_,_)) => boolType.
    tpOf(cLtt(_,_,_,E)) => tpOf(E).
    tpOf(cUnpack(_,_,_,Tp)) => Tp.
    tpOf(cCase(_,_,_,_,Tp)) => Tp.
    tpOf(cCnd(_,_,L,_)) => tpOf(L).
    tpOf(cWhere(_,T,_)) => tpOf(T).
    tpOf(cMatch(_,_,_)) => boolType.
    tpOf(cSusp(_,_,_,T)) => T.
    tpOf(cResume(_,_,_,T)) => T.
    tpOf(cTry(_,_,_,T)) => T.
    tpOf(cValof(_,_,T)) => T.
    tpOf(cAbort(_,_,T)) => T.
    tpOf(cVarNmes(_,_,E)) => tpOf(E).
  .} in {
    typeOf = tpOf
  }

  public implementation hasType[cId] => {
    typeOf(cId(_,Tp)) => Tp.
  }

  public implementation display[cExp] => {
    disp(T) => dspExp(T,"")
  }

  public implementation display[cId] => {
    disp(cId(Nm,_)) => "%#(Nm)".
  }

  public implementation hasLoc[aAction] => {
    locOf(aNop(Lc)) => Lc.
    locOf(aSeq(Lc,_,_)) => Lc.
    locOf(aLbld(Lc,_,_)) => Lc.
    locOf(aBreak(Lc,_)) => Lc.
    locOf(aValis(Lc,_)) => Lc.
    locOf(aThrow(Lc,_)) => Lc.
    locOf(aPerf(Lc,_)) => Lc.
    locOf(aDefn(Lc,_,_)) => Lc.
    locOf(aAsgn(Lc,_,_)) => Lc.
    locOf(aCase(Lc,_,_,_)) => Lc.
    locOf(aUnpack(Lc,_,_)) => Lc.
    locOf(aIftte(Lc,_,_,_)) => Lc.
    locOf(aWhile(Lc,_,_)) => Lc.
    locOf(aRetire(Lc,_,_)) => Lc.
    locOf(aTry(Lc,_,_)) => Lc.
    locOf(aLtt(Lc,_,_,_)) => Lc.
    locOf(aVarNmes(Lc,_,_)) => Lc.
    locOf(aAbort(Lc,_)) => Lc.
  }

  public implementation display[aAction] => {
    disp(A) => dspAct(A,"")
  }

  public implementation coercion[cExp,data] => {.
    _coerce(cInt(_,Ix)) => some(intgr(Ix)).
    _coerce(cBig(_,Ix)) => some(bigi(Ix)).
    _coerce(cChar(_,Cx)) => some(chr(Cx)).
    _coerce(cFloat(_,Dx)) => some(flot(Dx)).
    _coerce(cString(_,Sx)) => some(strg(Sx)).
    _coerce(cVoid(_,_)) => some(symb(tLbl("void",0))).
    _coerce(cInt(_,Ix)) => some(intgr(Ix)).
    _coerce(cTerm(_,Nm,[],_)) => some(symb(tLbl(Nm,0))).
    _coerce(cTerm(_,Nm,Args,_)) where NArgs ^= mapArgs(Args,[]) =>
      some(term(tLbl(Nm,size(Args)),NArgs)).
    _coerce(_) default => .none.

    private mapArgs([],So) => some(reverse(So)).
    mapArgs([A,..As],So) where NA^=_coerce(A) => mapArgs(As,[NA,..So]).
    mapArgs(_,_) default => .none.
  .}

  public implementation coercion[locn,cExp] => {
    _coerce(Lc) where locn(Nm,Line,Col,Off,Len).=Lc &&
	OLc .= some(Lc) =>
      some(mcTpl(OLc,[cString(OLc,Nm),
	    cInt(OLc,Line),cInt(OLc,Col),cInt(OLc,Off),cInt(OLc,Len)]))
  }

  public rwTerm:(cExp,(cExp)=>option[cDefn])=>cExp.
  rwTerm(T,Tst) where vrDef(_,_,_,Vl) ^= Tst(T) => Vl.
  rwTerm(cVoid(Lc,T),_) => cVoid(Lc,T).
  rwTerm(cAnon(Lc,T),_) => cAnon(Lc,T).
  rwTerm(cVar(Lc,V),_) => cVar(Lc,V).
  rwTerm(cInt(Lc,Ix),_) => cInt(Lc,Ix).
  rwTerm(cBig(Lc,Ix),_) => cBig(Lc,Ix).
  rwTerm(cFloat(Lc,Dx),_) => cFloat(Lc,Dx).
  rwTerm(cChar(Lc,Cx),_) => cChar(Lc,Cx).
  rwTerm(cString(Lc,Sx),_) => cString(Lc,Sx).
  rwTerm(cTerm(Lc,Op,Args,Tp),Tst) =>
    cTerm(Lc,Op,rwTerms(Args,Tst),Tp).
  rwTerm(cNth(Lc,R,Ix,Tp),Tst) => cNth(Lc,rwTerm(R,Tst),Ix,Tp).
  rwTerm(cSetNth(Lc,R,Ix,E),Tst) => cSetNth(Lc,rwTerm(R,Tst),Ix,rwTerm(E,Tst)).
  rwTerm(cCall(Lc,Op,Args,Tp),Tst) =>
    cCall(Lc,Op,Args//(A)=>rwTerm(A,Tst),Tp).
  rwTerm(cOCall(Lc,Op,Args,Tp),Tst) =>
    cOCall(Lc,rwTerm(Op,Tst),Args//(A)=>rwTerm(A,Tst),Tp).
  rwTerm(cECall(Lc,Op,Args,Tp),Tst) =>
    cECall(Lc,Op,Args//(A)=>rwTerm(A,Tst),Tp).
  rwTerm(cThrow(Lc,E,Tp),Tst) => cThrow(Lc,rwTerm(E,Tst),Tp).
  rwTerm(cSeq(Lc,L,R),Tst) => cSeq(Lc,rwTerm(L,Tst),rwTerm(R,Tst)).
  rwTerm(cCnj(Lc,L,R),Tst) => cCnj(Lc,rwTerm(L,Tst),rwTerm(R,Tst)).
  rwTerm(cDsj(Lc,L,R),Tst) => cDsj(Lc,rwTerm(L,Tst),rwTerm(R,Tst)).
  rwTerm(cNeg(Lc,R),Tst) => cNeg(Lc,rwTerm(R,Tst)).
  rwTerm(cCnd(Lc,T,L,R),Tst) => cCnd(Lc,rwTerm(T,Tst),rwTerm(L,Tst),rwTerm(R,Tst)).
  rwTerm(cLtt(Lc,V,D,E),Tst) => cLtt(Lc,V,rwTerm(D,Tst),rwTerm(E,dropVar(cName(V),Tst))).
  rwTerm(cUnpack(Lc,Sel,Cases,Tp),M) =>
    cUnpack(Lc,rwTerm(Sel,M),Cases//(C)=>rwCase(C,M,rwTerm),Tp).
  rwTerm(cCase(Lc,Sel,Cases,Dflt,Tp),M) =>
    cCase(Lc,rwTerm(Sel,M),Cases//(C)=>rwCase(C,M,rwTerm),rwTerm(Dflt,M),Tp).
  rwTerm(cWhere(Lc,T,C),M) =>
    cWhere(Lc,rwTerm(T,M),rwTerm(C,M)).
  rwTerm(cMatch(Lc,P,E),M) =>
    cMatch(Lc,rwTerm(P,M),rwTerm(E,M)).
  rwTerm(cTask(Lc,A,T),M) => cTask(Lc,rwTerm(A,M),T).
  rwTerm(cSusp(Lc,T,E,Tp),M) => cSusp(Lc,rwTerm(T,M),rwTerm(E,M),Tp).
  rwTerm(cResume(Lc,T,E,Tp),M) => cResume(Lc,rwTerm(T,M),rwTerm(E,M),Tp).
  rwTerm(cTry(Lc,E,H,Tp),M) => cTry(Lc,rwTerm(E,M),rwTerm(H,M),Tp).
  rwTerm(cVarNmes(Lc,Vs,E),M) => cVarNmes(Lc,Vs,rwTerm(E,M)).
  rwTerm(cValof(Lc,A,Tp),M) => cValof(Lc,rwAct(A,M),Tp).
  rwTerm(cAbort(Lc,Ms,T),M) => cAbort(Lc,Ms,T).

  public rwAct:(aAction,(cExp)=>option[cDefn])=>aAction.
  rwAct(aNop(Lc),_) => aNop(Lc).
  rwAct(aSeq(Lc,L,R),M) => aSeq(Lc,rwAct(L,M),rwAct(R,M)).
  rwAct(aLbld(Lc,L,A),M) => aLbld(Lc,L,rwAct(A,M)).
  rwAct(aBreak(Lc,L),M) => aBreak(Lc,L).
  rwAct(aValis(Lc,E),M) => aValis(Lc,rwTerm(E,M)).
  rwAct(aThrow(Lc,E),M) => aThrow(Lc,rwTerm(E,M)).
  rwAct(aPerf(Lc,E),M) => aPerf(Lc,rwTerm(E,M)).
  rwAct(aDefn(Lc,V,E),M) => aDefn(Lc,rwTerm(V,M),rwTerm(E,M)).
  rwAct(aAsgn(Lc,V,E),M) => aAsgn(Lc,rwTerm(V,M),rwTerm(E,M)).
  rwAct(aCase(Lc,G,Cs,D),M) => aCase(Lc,rwTerm(G,M),Cs//(C)=>rwCase(C,M,rwAct),rwAct(D,M)).
  rwAct(aUnpack(Lc,G,Cs),M) => aUnpack(Lc,rwTerm(G,M),Cs//(C)=>rwCase(C,M,rwAct)).
  rwAct(aIftte(Lc,C,L,R),M) => aIftte(Lc,rwTerm(C,M),rwAct(L,M),rwAct(R,M)).
  rwAct(aWhile(Lc,C,B),M) => aWhile(Lc,rwTerm(C,M),rwAct(B,M)).
  rwAct(aRetire(Lc,T,E),M) => aRetire(Lc,rwTerm(T,M),rwTerm(E,M)).
  rwAct(aTry(Lc,B,H),M) => aTry(Lc,rwAct(B,M),rwAct(H,M)).
  rwAct(aLtt(Lc,V,D,A),Tst) => aLtt(Lc,V,rwTerm(D,Tst),rwAct(A,dropVar(cName(V),Tst))).
  rwAct(aVarNmes(Lc,Vs,E),M) => aVarNmes(Lc,Vs,rwAct(E,M)).
  rwAct(aAbort(Lc,Ms),M) => aAbort(Lc,Ms).

  dropVar:(string,(cExp)=>option[cDefn])=>(cExp)=>option[cDefn].
  dropVar(Nm,Tst) => let{
    test(cVar(_,cId(Nm,_))) => .none.
    test(T) default => Tst(T)
  } in test.

  public rwTerms:(cons[cExp],(cExp)=>option[cDefn])=>cons[cExp].
  rwTerms(Els,Tst) => (Els//(E)=>rwTerm(E,Tst)).

  rwDef(fnDef(Lc,Nm,Tp,Args,Val),M) =>
    fnDef(Lc,Nm,Tp,Args,rwTerm(Val,M)).
  rwDef(vrDef(Lc,Nm,Tp,Val),M) =>
    vrDef(Lc,Nm,Tp,rwTerm(Val,M)).
  rwDef(D,_) default => D.

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
    test(cVar(Lc,cId(Nm,Tp))) => M[tLbl(Nm,arity(Tp))].
    test(_) => .none.
  } in test.

  public implementation hasLoc[cDefn] => {
    locOf(fnDef(Lc,_,_,_,_)) => Lc.
    locOf(vrDef(Lc,_,_,_)) => Lc.
    locOf(tpDef(Lc,_,_,_)) => Lc.
    locOf(lblDef(Lc,_,_,_)) => Lc.
  }

  public cName:(cId) => string.
  cName(cId(Nm,_))=>Nm.

  public cType:(cId) => tipe.
  cType(cId(_,Tp)) => Tp.

  public isCrCond:(cExp)=>boolean.
  isCrCond(cCnj(_,_,_))=>.true.
  isCrCond(cDsj(_,_,_))=>.true.
  isCrCond(cNeg(_,_))=>.true.
  isCrCond(cCnd(_,_,L,R))=>isCrCond(L)||isCrCond(R).
  isCrCond(cWhere(_,L,_)) => isCrCond(L).
  isCrCond(cMatch(_,_,_))=>.true.
  isCrCond(_) default => .false.

  public isGround:(cExp) => boolean.
  isGround(cInt(_,_)) => .true.
  isGround(cBig(_,_)) => .true.
  isGround(cFloat(_,_)) => .true.
  isGround(cChar(_,_)) => .true.
  isGround(cString(_,_)) => .true.
  isGround(cTerm(_,_,Els,_)) => {? E in Els *> isGround(E) ?}.
  isGround(_) default => .false.

  public mergeGoal:(option[locn],option[cExp],option[cExp])=>option[cExp].
  mergeGoal(_,G,.none) => G.
  mergeGoal(_,.none,G) => G.
  mergeGoal(Lc,some(G),some(H)) => some(cCnj(Lc,G,H)).
  
  public contract all e ~~ reform[e] ::= {
    mkCond:(option[locn],cExp,e,e)=>e.
    mkCase:(option[locn],cExp,cons[cCase[e]],e) => e.
    mkUnpack:(option[locn],cExp,cons[cCase[e]]) => e.
    varNames:(option[locn],cons[(string,cId)],e)=>e.
    pullWhere:(e,option[cExp]) => (e,option[cExp]).
    mkLtt:(option[locn],cId,cExp,e) => e.
  }

  public implementation reform[cExp] => {.
    mkCond(Lc,Tst,Th,El) where 
	cCnd(_,T1,Th1,El1).=Th && El1==El => cCnd(Lc,cCnj(Lc,Tst,T1),Th1,El1).
    mkCond(Lc,Tst,Th,El) => cCnd(Lc,Tst,Th,El).

    varNames(Lc,Bnds,Val) => cVarNmes(Lc,Bnds,Val).

    pullWhere(cWhere(Lc,V,C),G) where (Val,G1) .= pullWhere(V,G) =>
      (Val,mergeGoal(Lc,some(C),G1)).
    pullWhere(cTerm(Lc,Lbl,Args,Tp),G) where (NArgs,Gx) .= pullWheres(Args,G) =>
      (cTerm(Lc,Lbl,NArgs,Tp),Gx).
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
	aIftte(Lc0,T1,Th1,El1).=Th && El1==El => aIftte(Lc0,cCnj(Lc,Tst,T1),Th1,El1).
    mkCond(Lc,Tst,Th,El) => aIftte(Lc,Tst,Th,El).

    varNames(Lc,Bnds,Val) => aVarNmes(Lc,Bnds,Val).

    pullWhere(A,Cond) => (A,Cond).

    mkCase(Lc,V,Cases,Deflt) => aCase(Lc,V,Cases,Deflt).
    mkUnpack(Lc,V,Arms) => aUnpack(Lc,V,Arms).

    mkLtt(Lc,V,E,X) => aLtt(Lc,V,E,X).
  }

  dfVars:(cons[cDefn],set[cId])=>set[cId].
  dfVars([fnDef(_,Nm,Tp,_,_),..Ds],D) =>
    dfVars(Ds,D\+cId(Nm,Tp)).
  dfVars([vrDef(_,Nm,Tp,_),..Ds],D) =>
    dfVars(Ds,D\+cId(Nm,Tp)).
  dfVars([_,..Ds],D) => dfVars(Ds,D).
  dfVars([],D) => D.

  dclVrs:(cons[decl],set[cId])=>set[cId].
  dclVrs([funDec(_,_,Nm,Tp),..Ds],D) =>
    dclVrs(Ds,D\+cId(Nm,Tp)).
  dclVrs([varDec(_,_,Nm,Tp),..Ds],D) =>
    dclVrs(Ds,D\+cId(Nm,Tp)).
  dclVrs([cnsDec(_,_,Nm,Tp),..Ds],D) =>
    dclVrs(Ds,D\+cId(Nm,Tp)).
  dclVrs([accDec(_,_,_,Nm,Tp),..Ds],D) => 
    dclVrs(Ds,D\+cId(Nm,Tp)).
  dclVrs([updDec(_,_,_,Nm,Tp),..Ds],D) =>
    dclVrs(Ds,D\+cId(Nm,Tp)).
  dclVrs([implDec(_,_,Nm,Tp),..Ds],D) =>
    dclVrs(Ds,D\+cId(Nm,Tp)).
  dclVrs([_,..Ds],D) => dclVrs(Ds,D).
  dclVrs([],D) => D.

  public validProg:(cons[cDefn],cons[decl]) => ().
  validProg(Defs,Decls) => valof{
    
    D = dfVars(Defs,dclVrs(Decls,[]));

    for Df in Defs do{
      case Df in {
	fnDef(Lc,Nm,Tp,Args,Val) => {
	  D1 = foldLeft((V,D1)=>D1\+V,D,Args);
	  if ~validE(Val,D1) then{
	    reportError("$(fnDef(Lc,Nm,Tp,Args,Val)) not valid",Lc)
	  }
	}
	vrDef(Lc,Nm,Tp,Val) => {
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
  validE(cVoid(Lc,Tp),_) => .true.
  validE(cAnon(_,_),_) => .true.
  validE(cVar(Lc,V),D) => ( V .<. D ? .true || valof{
      reportError("$(V)/$(arity(typeOf(V)))",Lc);
      valis .false
    }).
  validE(cInt(_,_),_) => .true.
  validE(cBig(_,_),_) => .true.
  validE(cChar(_,_),_) => .true.
  validE(cString(_,_),_) => .true.
  validE(cFloat(_,_),_) => .true.
  validE(cTerm(_,_,Args,_),D) => {? E in Args *> validE(E,D) ?}.
  validE(cNth(_,R,_,_),D) => validE(R,D).
  validE(cSetNth(_,R,_,V),D) => validE(R,D) && validE(V,D).
  validE(cCall(_,_,Args,_),D) => {? E in Args *> validE(E,D) ?}.
  validE(cECall(_,_,Args,_),D) => {? E in Args *> validE(E,D) ?}.
  validE(cOCall(_,Op,Args,_),D) =>
    validE(Op,D) && {? E in Args *> validE(E,D) ?}.
  validE(cThrow(_,E,_),D) => validE(E,D).
  validE(cSeq(_,L,R),D) => validE(L,D) && validE(R,D).
  validE(cCnj(_,L,R),D) => validE(L,D) && validE(R,D).
  validE(cDsj(_,L,R),D) => validE(L,D) && validE(R,D).
  validE(cNeg(_,R),D) => validE(R,D).
  validE(cCnd(_,Ts,L,R),D) => valof{
    D1 = glVars(Ts,D);
    valis validE(Ts,D1) && validE(L,D1) && validE(R,D)
  }
  validE(cLtt(_,B,V,E),D) => validE(V,D) && validE(E,D\+B).
  validE(cCase(_,G,Cs,Df,_),D) => validE(G,D) && validCases(Cs,validE,D) && validE(Df,D).
  validE(cUnpack(_,G,Cs,_),D) => validE(G,D) && validCases(Cs,validE,D).
  validE(cWhere(_,V,E),D) =>
    validE(V,D) && validE(E,D).
  validE(cMatch(_,V,E),D) =>
    validE(V,D) && validE(E,D).
  validE(cVarNmes(_,_,E),D) => validE(E,D).
  validE(cAbort(_,_,_),_) => .true.
  validE(cTask(_,Ts,_),D) => validE(Ts,D).
  validE(cSusp(_,Ts,E,_),D) => validE(Ts,D) && validE(E,D).
  validE(cResume(_,Ts,E,_),D) => validE(Ts,D) && validE(E,D).
  validE(cTry(_,Ts,E,_),D) => validE(Ts,D) && validE(E,D).
  validE(cValof(_,A,_),D) => validA(A,D).

  validCases:all e ~~ (cons[cCase[e]],(e,set[cId])=>boolean,set[cId]) => boolean.
  validCases([],_,_) => .true.
  validCases([(_,A,E),..Cs],P,D) => valof{
    D1 = ptnVars(A,D);
    valis validE(A,D1) && P(E,D1) && validCases(Cs,P,D)
  }

  validA:(aAction,set[cId])=>boolean.
  validA(aNop(_),_) => .true.
  validA(aSeq(_,A1,A2),D) => valof{
    if aDefn(_,P,V) .= A1 then{
      D1 = ptnVars(P,D);
      valis validE(P,D1) && validE(V,D) && validA(A2,D1);
    } else {
      valis validA(A1,D) && validA(A2,D)
    }
  }
  validA(aLbld(_,_,A),D) => validA(A,D).
  validA(aBreak(_,L),D) => .true.
  validA(aValis(_,E),D) => validE(E,D).
  validA(aThrow(_,E),D) => validE(E,D).
  validA(aPerf(_,E),D) => validE(E,D).
  validA(aDefn(_,P,E),D) => validE(P,ptnVars(P,D)) && validE(E,D).
  validA(aAsgn(_,L,V),D) => validE(L,D) && validE(V,D).
  validA(aCase(_,G,Cs,Df),D) =>
    validE(G,D) && validCases(Cs,validA,D) && validA(Df,D).
  validA(aUnpack(_,G,Cs),D) =>
    validE(G,D) && validCases(Cs,validA,D).
  validA(aIftte(_,G,Th,E),D) => valof{
    D1 = glVars(G,D);
    valis validE(G,D1) && validA(Th,D1) && validA(E,D)
  }
  validA(aWhile(_,G,A),D) => valof{
    D1 = glVars(G,D);
    valis validE(G,D1) && validA(A,D1)
  }
  validA(aRetire(_,Ts,E),D) => validE(Ts,D) && validE(E,D).
  validA(aTry(_,A,H),D) => validA(A,D) && validA(H,D).
  validA(aLtt(_,B,V,A),D) => validE(V,D) && validA(A,D\+B).
  validA(aVarNmes(_,_,A),D) => validA(A,D).
  validA(aAbort(_,_),_) => .true.

  ptnVars:(cExp,set[cId]) => set[cId].
  ptnVars(cVoid(_,_),D) => D.
  ptnVars(cAnon(_,_),D) => D.
  ptnVars(cVar(_,V),D) => D\+V.
  ptnVars(cInt(_,_),D) => D.
  ptnVars(cBig(_,_),D) => D.
  ptnVars(cChar(_,_),D) => D.
  ptnVars(cString(_,_),D) => D.
  ptnVars(cFloat(_,_),D) => D.
  ptnVars(cTerm(_,_,Args,_),D) => foldLeft(ptnVars,D,Args).
  ptnVars(cNth(_,R,_,_),D) => ptnVars(R,D).
  ptnVars(cWhere(_,V,E),D) => ptnVars(V,glVars(E,D)).

  glVars:(cExp,set[cId])=>set[cId].
  glVars(cCnj(_,L,R),D) => glVars(R,glVars(L,D)).
  glVars(cDsj(_,L,R),D) => valof{
    D1 = glVars(L,[]);
    D2 = glVars(R,[]);
    valis D\/(D1/\D2)
  }
  glVars(cNeg(_,R),D) => D.
  glVars(cCnd(_,Ts,L,R),D) => valof{
    D1 = glVars(Ts,glVars(L,[]));
    D2 = glVars(R,[]);
    valis D\/(D1/\D2)
  }
  glVars(cWhere(_,V,C),D) => glVars(C,glVars(V,D)).
  glVars(cMatch(_,P,_),D) => ptnVars(P,D).
  glVars(_,D) default => D.

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

  isBreak(aBreak(_,Lb),Lb) => .true.
  isBreak(_,_) default => .false.

  presentInA(A,C,_) where C(A) => .true.
  presentInA(aNop(_),_,_) => .false.
  presentInA(aSeq(_,A1,A2),C,T) => presentInA(A1,C,T) || presentInA(A2,C,T).
  presentInA(aLbld(_,_,A),C,T) => presentInA(A,C,T).
  presentInA(aBreak(_,L),C,T) => .false.
  presentInA(aValis(_,E),C,T) => presentInE(E,C,T).
  presentInA(aThrow(_,E),C,T) => presentInE(E,C,T).
  presentInA(aPerf(_,E),C,T) => presentInE(E,C,T).
  presentInA(aDefn(_,_,E),C,T) => presentInE(E,C,T).
  presentInA(aAsgn(_,L,V),C,T) => presentInE(L,C,T) || presentInE(V,C,T).
  presentInA(aCase(_,G,Cs,D),C,T) =>
    presentInE(G,C,T) || presentInCases(Cs,presentInA,C,T) || presentInA(D,C,T).
  presentInA(aUnpack(_,G,Cs),C,T) =>
    presentInE(G,C,T) || presentInCases(Cs,presentInA,C,T).
  presentInA(aIftte(_,G,Th,E),C,T) =>
    presentInE(G,C,T) || presentInA(Th,C,T) || presentInA(E,C,T).
  presentInA(aWhile(_,G,A),C,T) =>
    presentInE(G,C,T) || presentInA(A,C,T).
  presentInA(aRetire(_,Ts,E),C,T) => presentInE(Ts,C,T) || presentInE(E,C,T).
  presentInA(aTry(_,A,H),C,T) => presentInA(A,C,T) || presentInA(H,C,T).
  presentInA(aLtt(_,_,V,A),C,T) => presentInE(V,C,T) || presentInA(A,C,T).
  presentInA(aVarNmes(_,_,A),C,T) => presentInA(A,C,T).
  presentInA(aAbort(_,_),_,_) => .false.

  public varPresent:all e ~~ present[e] |: (e,string)=>boolean.
  varPresent(E,Nm) => present(E,(V)=>sameVar(V,Nm)).

  sameVar(cVar(_,cId(Nm,_)),Nm) => .true.
  sameVar(_,_) default => .false.

  presentInE:(cExp,(aAction)=>boolean,(cExp)=>boolean) => boolean.
  presentInE(T,_,C) where C(T) => .true.
  presentInE(cVoid(_,_),_,_) => .false.
  presentInE(cAnon(_,_),_,_) => .false.
  presentInE(cVar(_,_),_,_) => .false.
  presentInE(cInt(_,_),_,_) => .false.
  presentInE(cBig(_,_),_,_) => .false.
  presentInE(cChar(_,_),_,_) => .false.
  presentInE(cString(_,_),_,_) => .false.
  presentInE(cFloat(_,_),_,_) => .false.
  presentInE(cTerm(_,_,Args,_),C,T) => {? E in Args && presentInE(E,C,T) ?}.
  presentInE(cNth(_,R,_,_),C,T) => presentInE(R,C,T).
  presentInE(cSetNth(_,R,_,V),C,T) => presentInE(R,C,T) || presentInE(V,C,T).
  presentInE(cCall(_,_,Args,_),C,T) => {? E in Args && presentInE(E,C,T) ?}.
  presentInE(cECall(_,_,Args,_),C,T) => {? E in Args && presentInE(E,C,T) ?}.
  presentInE(cOCall(_,Op,Args,_),C,T) =>
    presentInE(Op,C,T) || {? E in Args && presentInE(E,C,T) ?}.
  presentInE(cThrow(_,E,_),C,T) => presentInE(E,C,T).
  presentInE(cSeq(_,L,R),C,T) => presentInE(L,C,T) || presentInE(R,C,T).
  presentInE(cCnj(_,L,R),C,T) => presentInE(L,C,T) || presentInE(R,C,T).
  presentInE(cDsj(_,L,R),C,T) => presentInE(L,C,T) || presentInE(R,C,T).
  presentInE(cNeg(_,R),C,T) => presentInE(R,C,T).
  presentInE(cCnd(_,Ts,L,R),C,T) =>
    presentInE(Ts,C,T) || presentInE(L,C,T) || presentInE(R,C,T).
  presentInE(cLtt(_,_,V,E),C,T) =>
    presentInE(V,C,T) || presentInE(E,C,T).
  presentInE(cCase(_,G,Cs,D,_),C,T) =>
    presentInE(G,C,T) || presentInCases(Cs,presentInE,C,T) || presentInE(D,C,T).
  presentInE(cUnpack(_,G,Cs,_),C,T) =>
    presentInE(G,C,T) || presentInCases(Cs,presentInE,C,T).
  presentInE(cWhere(_,V,E),C,T) =>
    presentInE(V,C,T) || presentInE(E,C,T).
  presentInE(cMatch(_,V,E),C,T) =>
    presentInE(V,C,T) || presentInE(E,C,T).
  presentInE(cVarNmes(_,_,E),C,T) =>
    presentInE(E,C,T).
  presentInE(cAbort(_,_,_),_,_) => .false.
  presentInE(cTask(_,Ts,_),C,T) => presentInE(Ts,C,T).
  presentInE(cSusp(_,Ts,E,_),C,T) => presentInE(Ts,C,T) || presentInE(E,C,T).
  presentInE(cResume(_,Ts,E,_),C,T) => presentInE(Ts,C,T) || presentInE(E,C,T).
  presentInE(cTry(_,Ts,E,_),C,T) => presentInE(Ts,C,T) || presentInE(E,C,T).
  presentInE(cValof(_,A,_),C,T) => presentInA(A,C,T).

  presentInCases:all e ~~ (cons[cCase[e]],(e,(aAction)=>boolean,(cExp)=>boolean)=>boolean,
    (aAction)=>boolean,(cExp)=>boolean)=>boolean.
  presentInCases([],_,_,_) => .false.
  presentInCases([(_,A,E),..Cs],P,C,T) =>
    presentInE(A,C,T) || P(E,C,T) || presentInCases(Cs,P,C,T).
}
