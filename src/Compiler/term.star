star.compiler.term{
  import star.
  import star.topsort.
  import star.multi.

  import star.compiler.data.
  import star.compiler.decode.
  import star.compiler.encode.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.
  import star.pkg.
  
  public cExp ::= .cVoid(option[locn])
  | .cAnon(option[locn],tipe)
  | .cUnrch(option[locn],tipe)
  | .cVar(option[locn],cV)
  | .cCel(option[locn],cExp,tipe)
  | .cGet(option[locn],cExp,tipe)
  | .cInt(option[locn],integer)
  | .cChar(option[locn],char)
  | .cBig(option[locn],bigint)
  | .cFlt(option[locn],float)
  | .cString(option[locn],string)
  | .cTerm(option[locn],string,cons[cExp],tipe)
  | .cNth(option[locn],cExp,integer,tipe)
  | .cSetNth(option[locn],cExp,integer,cExp)
  | .cClos(option[locn],string,integer,cExp,tipe)
  | .cSv(option[locn],tipe)
  | .cSvDrf(option[locn],cExp,tipe)
  | .cSvSet(option[locn],cExp,cExp)
  | .cCall(option[locn],string,cons[cExp],tipe)
  | .cOCall(option[locn],cExp,cons[cExp],tipe)
  | .cXCall(option[locn],string,cons[cExp],tipe,tipe)
  | .cXOCall(option[locn],cExp,cons[cExp],tipe,tipe)
  | .cSeq(option[locn],cExp,cExp)
  | .cCnj(option[locn],cExp,cExp)
  | .cDsj(option[locn],cExp,cExp)
  | .cNeg(option[locn],cExp)
  | .cCnd(option[locn],cExp,cExp,cExp)
  | .cLtt(option[locn],cV,cExp,cExp)
  | .cCase(option[locn],cExp,cons[cCase[cExp]],cExp,tipe)
  | .cIxCase(option[locn],cExp,cons[cCase[cExp]],cExp,tipe)
  | .cMatch(option[locn],cExp,cExp)
  | .cResum(option[locn],cExp,cExp,tipe)
  | .cSusp(option[locn],cExp,cExp,tipe)
  | .cRetyr(option[locn],cExp,cExp,tipe)
  | .cVarNme(option[locn],string,cExp,cExp)
  | .cAbort(option[locn],string,tipe)
  | .cTry(option[locn],cExp,cExp,cExp,tipe)
  | .cThrw(option[locn],cExp,tipe)
  | .cValof(option[locn],aAction,tipe).
  
  public cV ::= .cV(string,tipe).

  public all e ~~ cCase[e] ~> (option[locn],cExp,e).

  public aAction ::= .aNop(option[locn])
  | .aSeq(option[locn],aAction,aAction)
  | .aLbld(option[locn],string,aAction)
  | .aBreak(option[locn],string)
  | .aValis(option[locn],cExp)
  | .aDo(option[locn],cExp)
  | .aSetNth(option[locn],cExp,integer,cExp)
  | .aDefn(option[locn],cExp,cExp)
  | .aMatch(option[locn],cExp,cExp)
  | .aAsgn(option[locn],cExp,cExp)
  | .aCase(option[locn],cExp,cons[cCase[aAction]],aAction)
  | .aIxCase(option[locn],cExp,cons[cCase[aAction]],aAction)
  | .aIftte(option[locn],cExp,aAction,aAction)
  | .aWhile(option[locn],cExp,aAction)
  | .aTry(option[locn],aAction,cExp,aAction)
  | .aThrw(option[locn],cExp)
  | .aLtt(option[locn],cV,cExp,aAction)
  | .aVarNme(option[locn],string,cExp,aAction)
  | .aAbort(option[locn],string).

  public cDefn ::= .fnDef(option[locn],string,tipe,cons[cV],cExp)
  | .prDef(option[locn],string,tipe,cons[cV],aAction)
  | .glDef(option[locn],string,tipe,cExp)
  | .tpDef(option[locn],tipe,typeRule,indexMap)
  | .lblDef(option[locn],termLbl,tipe,integer).

  public dispCrProg:(cons[cDefn])=>string.
  dispCrProg(Defs) => interleave(Defs//disp,".\n")*.

  public implementation display[cDefn] => {
    disp(Df) => dspDef(Df,"  ").
  }

  dspDef:(cDefn,string) => string.
  dspDef(Df,Off) => case Df in {
    | .fnDef(_Lc,Nm,Tp,Args,Rep) =>
      "fn: #(Nm)(#(interleave(Args//disp,",")*)) => #(dspExp(Rep,Off))"
    | .prDef(_Lc,Nm,Tp,Args,Act) =>
      "pr: #(Nm)(#(interleave(Args//disp,",")*)) => #(dspAct(Act,Off))"
    | .glDef(_Lc,Nm,Tp,Rep) => "vr: #(Nm)=#(dspExp(Rep,Off))"
    | .tpDef(_Lc,Tp,TpRl,Map) => "tp: $(TpRl) with $(Map)"
    | .lblDef(_Lc,Lbl,Tp,Ix) => "lb: $(Lbl)\:$(Tp)@$(Ix)"
  }

  public defName:(cDefn)=>string.
  defName(.fnDef(_Lc,Nm,_Tp,_Args,_Rep)) => "Fn: #(Nm)".
  defName(.prDef(_Lc,Nm,_Tp,_Args,_Act)) => "Pr: #(Nm)".
  defName(.glDef(_Lc,Nm,_Tp,_Rep)) => "Gb: #(Nm)".
  defName(.tpDef(_Lc,Tp,_Rl,_Map)) => "Tp: #(tpName(Tp))".
  defName(.lblDef(_Lc,Nm,_Tp,_Ix)) => "Lb: $(Nm)".

  dspExp:(cExp,string) => string.
  dspExp(Exp,Off) => case Exp in {
    | .cVoid(_) => "void"
    | .cAnon(_,_) => "_"
    | .cUnrch(_,_) => "unreachable"
    | .cVar(_,.cV(V,VTp)) => "%#(V)"
    | .cInt(_,Ix) => disp(Ix)
    | .cChar(_,Ix) => disp(Ix)
    | .cBig(_,Ix) => disp(Ix)
    | .cFlt(_,Dx) => disp(Dx)
    | .cString(_,Sx) => disp(Sx)
    | .cCall(_,Op,As,_) => "#(Op)(#(dsplyExps(As,Off)*))"
    | .cOCall(_,Op,As,_) => "#(pDspExp(Op,Off))°(#(dsplyExps(As,Off)*))"
    | .cXCall(_,Op,As,_,ETp) => "#(Op)(#(dsplyExps(As,Off)*)) throws $(ETp)"
    | .cXOCall(_,Op,As,_,ETp) => "#(pDspExp(Op,Off))°°(#(dsplyExps(As,Off)*)) throws $(ETp)"
    | .cTerm(_,Op,As,_) where isTplLbl(Op) => "(#(dsplyExps(As,Off)*))"
    | .cTerm(_,Op,As,_) => ".#(Op)(#(dsplyExps(As,Off)*))"
    | .cNth(_,O,Ix,_) => "#(dspExp(O,Off)).$(Ix)"
    | .cSetNth(_,O,Ix,E) => "(#(dspExp(O,Off)).$(Ix) <- #(dspExp(E,Off)))"
    | .cClos(_,Nm,Ar,Fr,_) => "<#(Nm)/$(Ar)\:#(dspExp(Fr,Off))>"
    | .cCel(_,E,_) => "ref #(dspExp(E,Off))"
    | .cGet(_,E,_) => "#(dspExp(E,Off))!"
    | .cSv(_,Tp) => "^$(Tp)"
    | .cSvDrf(_,E,_) => "#(dspExp(E,Off))^"
    | .cSvSet(_,E,V) => "#(dspExp(E,Off))<-#(dspExp(V,Off))"
    | .cLtt(_,V,D,I) => valof{
      Off2=Off++"  ";
      valis "let $(V) = #(dspExp(D,Off2)) in\n#(Off2)#(dspExp(I,Off2))"
    }
    | .cCase(_,E,Cs,D,_)  => 
      "case #(dspExp(E,Off)) in {#(dspCases(Cs,dspExp,Off++"  ")*)\n#(Off)} else #(dspExp(D,Off))"
    | .cIxCase(_,E,Cs,D,_)  => 
      "index #(dspExp(E,Off)) in {#(dspCases(Cs,dspExp,Off++"  ")*)\n#(Off)} else #(dspExp(D,Off))"
    | .cMatch(_,P,E) => "#(dspExp(P,Off)).=#(dspExp(E,Off))"
    | .cCnj(_,L,R) => "#(dspExp(L,Off)) && #(dspExp(R,Off))"
    | .cDsj(_,L,R) => "(#(dspExp(L,Off)) || #(dspExp(R,Off)))"
    | .cCnd(_,T,L,R) => valof{
      Off2=Off++"  ";
      valis "(#(dspExp(T,Off)) ?? #(dspExp(L,Off2)) ||\n #(Off2)#(dspExp(R,Off2)))"
    }
    | .cNeg(_,R) => "~#(dspExp(R,Off))"
    | .cSeq(Lc,L,R) => "{#(dspSeq(.cSeq(Lc,L,R),Off++"  "))}"
    | .cResum(_,P,E,_) => "#(dspExp(P,Off)) resume #(dspExp(E,Off))"
    | .cSusp(_,P,E,_) => "#(dspExp(P,Off)) suspend #(dspExp(E,Off))"
    | .cRetyr(_,P,E,_) => "#(dspExp(P,Off)) retire #(dspExp(E,Off))"
    | .cVarNme(_,Nm,V,E) => "<var #(Nm) = #(dspExp(V,Off)) in #(dspExp(E,Off))>"
    | .cAbort(_,M,_) => "abort #(M)"
    | .cTry(_,B,E,H,_)=> 
      "(try #(dspExp(B,Off)) catch $(E) in #(dspExp(H,Off)))"
    | .cThrw(_,E,_) => "throw #(dspExp(E,Off))"
    | .cValof(_,A,_) => "valof #(dspAct(A,Off))"
  }

  pDspExp:(cExp,string) => string.
  pDspExp(E,Off) where needParens(E) => "(#(dspExp(E,Off)))".
  pDspExp(E,Off) default => "#(dspExp(E,Off))".

  needParens(.cLtt(_,_,_,_)) => .true.
  needParens(.cOCall(_,_,_,_)) => .true.
  needParens(.cCnj(_,_,_)) => .true.
  needParens(.cCnj(_,_,_)) => .true.
  needParens(.cDsj(_,_,_)) => .true.
  needParens(.cNeg(_,_)) => .true.
  needParens(.cCnd(_,_,_,_)) => .true.
  needParens(.cVarNme(_,_,_,_)) => .true.
  needParens(.cCase(_,_,_,_,_)) => .true.
  needParens(.cIxCase(_,_,_,_,_)) => .true.
  needParens(.cTry(_,_,_,_,_)) => .true.
  needParens(_) default => .false.

  dspAct:(aAction,string)=>string.
  dspAct(Act,Off) => case Act in {
    | .aNop(_) => "{}"
    | .aSeq(_,L,R) => valof{
      Off2=Off++"  ";
      valis "{ #(dspAct(L,Off2)); #(dspActSeq(R,Off2)) }"
    }
    | .aLbld(_,Lb,A) => "#(Lb) : #(dspAct(A,Off))"
    | .aBreak(_,Lb) => "break #(Lb)"
    | .aValis(_,E) => "valis #(dspExp(E,Off))"
    | .aDo(_,E) => "call #(dspExp(E,Off))"
    | .aSetNth(_,T,Ix,V) => "update #(dspExp(T,Off))[$(Ix)] <- #(dspExp(V,Off))"
    | .aDefn(_,P,E) => "#(dspExp(P,Off)) = #(dspExp(E,Off))"
    | .aMatch(_,P,E) => "#(dspExp(P,Off)) = #(dspExp(E,Off))"
    | .aAsgn(_,P,E) => "#(dspExp(P,Off)) := #(dspExp(E,Off))"
    | .aCase(_,E,Cs,Df) =>
      "case (#(dspExp(E,Off))) in {#(dspCases(Cs,dspAct,Off++"  ")*)\n#(Off)} else #(dspAct(Df,Off))"
    | .aIxCase(_,E,Cs,Df) =>
      "index (#(dspExp(E,Off))) in {#(dspCases(Cs,dspAct,Off++"  ")*)\n#(Off)} else #(dspAct(Df,Off))"
    | .aIftte(_,C,T,E) => valof{
      Off2=Off++"  ";
      valis "if #(dspExp(C,Off)) then\n#(Off2)#(dspAct(T,Off2)) else\n#(Off2)#(dspAct(E,Off2))"
    }
    | .aWhile(_,C,A) => valof{
      Off2=Off++"  ";
      valis "while #(dspExp(C,Off)) do#(dspAct(A,Off2))"
    }
    | .aTry(_,B,V,H) => "{ try #(dspAct(B,Off)) catch $(V) in #(dspAct(H,Off))}"
    | .aThrw(_,E) => "throw #(dspExp(E,Off))"
    | .aLtt(_,V,D,I) => valof{
      Off2=Off++"  ";
      valis "let $(V) = #(dspExp(D,Off2)) in\n#(Off2)#(dspAct(I,Off2))"
    }
    | .aVarNme(_,N,V,A) => "<vars #(N) = #(dspExp(V,Off)) in #(dspAct(A,Off))>"
    | .aAbort(_,M) => "abort #(M)"
  }

  dspActSeq(.aSeq(_,L,R),Off) => "\n#(Off)#(dspAct(L,Off));#(dspActSeq(R,Off))".
  dspActSeq(A,Off) => dspAct(A,Off).

  dspCases:all e ~~ (cons[cCase[e]],(e,string)=>string,string)=>cons[string].
  dspCases(Cs,F,Off) =>
    (Cs//((_,P,V))=>"\n#(Off)| #(dspExp(P,Off))=>#(F(V,Off))").

  dsplyExps(Es,Off) => interleave(Es//(E)=>dspExp(E,Off),", ").

  dspSeq(.cSeq(_,L,R),Off) => "#(dspSeq(L,Off));#(dspSeq(R,Off))".
  dspSeq(T,Off) => dspExp(T,Off).

  public mcTpl:(option[locn],cons[cExp]) => cExp.
  mcTpl(Lc,Args) => let{
    TpTp = .tupleType(Args//typeOf).
    Ar = size(Args)
  } in .cTerm(Lc,tplLbl(Ar), Args, TpTp).

  public mcEnum:(option[locn],string,tipe) => cExp.
  mcEnum(Lc,Nm,Tp) => .cTerm(Lc,Nm,[],Tp).

  public mcSome:(option[locn],cExp) => cExp.
  mcSome(Lc,Arg) => .cTerm(Lc,"some",[Arg],optType(typeOf(Arg))).

  public mcNone:(option[locn],tipe) => cExp.
  mcNone(Lc,Tp) => mcEnum(Lc,"none",optType(Tp)).

  public contract all e ~~ rewrite[e] ::= {
    rewrite:(e,(cExp)=>option[cExp])=>e
  }

  public rwVar:(map[string,cExp])=>(cExp)=>option[cExp].
  rwVar(M) => let{
    test(.cVar(_,.cV(Nm,_))) => M[Nm].
    test(_) => .none.
  } in test.

  public implementation equality[cV] => {
    .cV(N1,T1) == .cV(N2,T2) => N1==N2.
  }

  public implementation hashable[cV] => {
    hash(.cV(N,T)) => hash(N).
  }

  /*
    eqTerm/eqAct: fold E1 into a comparator (cExp/aAction)=>boolean closed over
    E1, then apply it to E2, turns binary tree-equality into a unary fold.
  */

  eqAlgebra:treeAlgebra[(),(cExp)=>boolean,(aAction)=>boolean].
  eqAlgebra = treeAlgebra{
    onVoid(_,_)=>(X2)=>(.cVoid(_).=X2).
    onAnon(_,_,T1)=>(X2)=>(.cAnon(_,T2).=X2 && T1==T2).
    onUnrch(_,_,T1)=>(X2)=>(.cUnrch(_,T2).=X2 && T1==T2).
    onVar(_,_,V1)=>(X2)=>(.cVar(_,V2).=X2 && V1==V2).
    onCel(_,_,ECmp,_)=>(X2)=>(.cCel(_,T2,_).=X2 && ECmp(T2)).
    onGet(_,_,ECmp,_)=>(X2)=>(.cGet(_,T2,_).=X2 && ECmp(T2)).
    onInt(_,_,N1)=>(X2)=>(.cInt(_,N2).=X2 && N1==N2).
    onChar(_,_,C1)=>(X2)=>(.cChar(_,C2).=X2 && C1==C2).
    onBig(_,_,N1)=>(X2)=>(.cBig(_,N2).=X2 && N1==N2).
    onFlt(_,_,D1)=>(X2)=>(.cFlt(_,D2).=X2 && D1==D2).
    onString(_,_,S1)=>(X2)=>(.cString(_,S2).=X2 && S1==S2).
    onSv(_,_,_)=>(X2)=>(.cSv(_,_).=X2).
    onTerm(_,_,S1,ArgCmps,_)=>(X2)=>(.cTerm(_,S2,A2,_).=X2 && S1==S2 && eqsWith(ArgCmps,A2)).
    onNth(_,_,RCmp,F1,_)=>(X2)=>(.cNth(_,R2,F2,_).=X2 && RCmp(R2) && F1==F2).
    onSetNth(_,_,RCmp,Ix1,VCmp)=>(X2)=>(.cSetNth(_,R2,Ix2,V2).=X2 && RCmp(R2) && Ix1==Ix2 && VCmp(V2)).
    onClos(_,_,L1,A1,FCmp,_)=>(X2)=>(.cClos(_,L2,A2,F2,_).=X2 && L1==L2 && A1==A2 && FCmp(F2)).
    onSvDrf(_,_,TCmp,_)=>(X2)=>(.cSvDrf(_,T2,_).=X2 && TCmp(T2)).
    onSvSet(_,_,TCmp,VCmp)=>(X2)=>(.cSvSet(_,T2,V2).=X2 && TCmp(T2) && VCmp(V2)).
    onCall(_,_,S1,ArgCmps,_)=>(X2)=>(.cCall(_,S2,A2,_).=X2 && S1==S2 && eqsWith(ArgCmps,A2)).
    onOCall(_,_,OpCmp,ArgCmps,_)=>(X2)=>(.cOCall(_,S2,A2,_).=X2 && OpCmp(S2) && eqsWith(ArgCmps,A2)).
    onXCall(_,_,S1,ArgCmps,_,_)=>(X2)=>(.cXCall(_,S2,A2,_,_).=X2 && S1==S2 && eqsWith(ArgCmps,A2)).
    onXOCall(_,_,OpCmp,ArgCmps,_,_)=>(X2)=>(.cXOCall(_,S2,A2,_,_).=X2 && OpCmp(S2) && eqsWith(ArgCmps,A2)).
    onSeq(_,_,LCmp,RCmp)=>(X2)=>(.cSeq(_,L2,R2).=X2 && LCmp(L2) && RCmp(R2)).
    onCnj(_,_,LCmp,RCmp)=>(X2)=>(.cCnj(_,L2,R2).=X2 && LCmp(L2) && RCmp(R2)).
    onDsj(_,_,LCmp,RCmp)=>(X2)=>(.cDsj(_,L2,R2).=X2 && LCmp(L2) && RCmp(R2)).
    onNeg(_,_,RCmp)=>(X2)=>(.cNeg(_,R2).=X2 && RCmp(R2)).
    onCnd(_,_,TCmp,LCmp,RCmp)=>(X2)=>(.cCnd(_,T2,L2,R2).=X2 && TCmp(T2) && LCmp(L2) && RCmp(R2)).
    onLtt(_,_,V1,DCmp,ECmp)=>(X2)=>(.cLtt(_,V2,D2,E2).=X2 && V1==V2 && DCmp(D2) && ECmp(E2)).
    onCase(_,_,SelCmp,Cases,DfltCmp,_)=>(X2)=>(.cCase(_,S2,C2,D2,_).=X2 && SelCmp(S2) && eqCasesE(Cases,C2) && DfltCmp(D2)).
    onIxCase(_,_,SelCmp,Cases,DfltCmp,_)=>(X2)=>(.cIxCase(_,S2,C2,D2,_).=X2 && SelCmp(S2) && eqCasesE(Cases,C2) && DfltCmp(D2)).
    onMatch(_,_,PCmp,VCmp)=>(X2)=>(.cMatch(_,P2,V2).=X2 && VCmp(V2) && PCmp(P2)).
    onResum(_,_,PCmp,VCmp,_)=>(X2)=>(.cResum(_,P2,V2,_).=X2 && VCmp(V2) && PCmp(P2)).
    onSusp(_,_,PCmp,VCmp,_)=>(X2)=>(.cSusp(_,P2,V2,_).=X2 && VCmp(V2) && PCmp(P2)).
    onRetyr(_,_,PCmp,VCmp,_)=>(X2)=>(.cRetyr(_,P2,V2,_).=X2 && VCmp(V2) && PCmp(P2)).
    onVarNme(_,_,N1,VCmp,BCmp)=>(X2)=>(.cVarNme(_,N2,V2,B2).=X2 && N1==N2 && VCmp(V2) && BCmp(B2)).
    onAbort(_,_,M1,T1)=>(X2)=>(.cAbort(_,M2,T2).=X2 && M1==M2 && T1==T2).
    onTry(_,_,MCmp,ECmp,HCmp,_)=>(X2)=>(.cTry(_,M2,E2,H2,_).=X2 && MCmp(M2) && ECmp(E2) && HCmp(H2)).
    onThrw(_,_,SCmp,_)=>(X2)=>(.cThrw(_,S2,_).=X2 && SCmp(S2)).
    onValof(_,_,ACmp,_)=>(X2)=>(.cValof(_,A2,_).=X2 && ACmp(A2)).

    onANop(_,_)=>(X2)=>(.aNop(_).=X2).
    onASeq(_,_,LCmp,RCmp)=>(X2)=>(.aSeq(_,L2,R2).=X2 && LCmp(L2) && RCmp(R2)).
    onALbld(_,_,L1,ACmp)=>(X2)=>(.aLbld(_,L2,Ac2).=X2 && L1==L2 && ACmp(Ac2)).
    onABreak(_,_,L1)=>(X2)=>(.aBreak(_,L2).=X2 && L1==L2).
    onAValis(_,_,ECmp)=>(X2)=>(.aValis(_,E2).=X2 && ECmp(E2)).
    onADo(_,_,ECmp)=>(X2)=>(.aDo(_,E2).=X2 && ECmp(E2)).
    onASetNth(_,_,VCmp,Ix1,TCmp)=>(X2)=>(.aSetNth(_,V2,Ix2,T2).=X2 && VCmp(V2) && Ix1==Ix2 && TCmp(T2)).
    onADefn(_,_,C1,C2)=>(X2)=>(.aDefn(_,E2,V2).=X2 && C1(E2) && C2(V2)).
    onAMatch(_,_,C1,C2)=>(X2)=>(.aMatch(_,E2,V2).=X2 && C1(E2) && C2(V2)).
    onAAsgn(_,_,C1,C2)=>(X2)=>(.aAsgn(_,E2,V2).=X2 && C1(E2) && C2(V2)).
    onACase(_,_,SCmp,Cs,DCmp)=>(X2)=>(.aCase(_,S2,C2,D2).=X2 && SCmp(S2) && eqCasesA(Cs,C2) && DCmp(D2)).
    onAIxCase(_,_,SCmp,Cs,DCmp)=>(X2)=>(.aIxCase(_,S2,C2,D2).=X2 && SCmp(S2) && eqCasesA(Cs,C2) && DCmp(D2)).
    onAIftte(_,_,CCmp,LCmp,RCmp)=>(X2)=>(.aIftte(_,C2,L2,R2).=X2 && CCmp(C2) && LCmp(L2) && RCmp(R2)).
    onAWhile(_,_,CCmp,LCmp)=>(X2)=>(.aWhile(_,C2,L2).=X2 && CCmp(C2) && LCmp(L2)).
    onATry(_,_,MCmp,ECmp,HCmp)=>(X2)=>(.aTry(_,M2,E2,H2).=X2 && MCmp(M2) && ECmp(E2) && HCmp(H2)).
    onAThrw(_,_,ECmp)=>(X2)=>(.aThrw(_,E2).=X2 && ECmp(E2)).
    onALtt(_,_,V1,DCmp,ACmp)=>(X2)=>(.aLtt(_,V2,D2,Ac2).=X2 && V1==V2 && DCmp(D2) && ACmp(Ac2)).
    onAVarNme(_,_,N1,VCmp,ACmp)=>(X2)=>(.aVarNme(_,N2,V2,Ac2).=X2 && N1==N2 && VCmp(V2) && ACmp(Ac2)).
    onAAbort(_,_,M1)=>(X2)=>(.aAbort(_,M2).=X2 && M1==M2).

    extendLtt(Nv,_)=>Nv.
    onRaw(_,_)=>.none.
    onARaw(_,_)=>.none.
    }.

  eqsWith:(cons[(cExp)=>boolean],cons[cExp]) => boolean.
  eqsWith(Cmps,Es2) => case Cmps in {
    | [] => isEmpty(Es2)
    | [Cmp,..Rest] => case Es2 in {
        | [E2,..Rest2] => Cmp(E2) && eqsWith(Rest,Rest2)
        | _ default => .false
      }
  }.

  eqCasesE:(cons[cCase[cExp]],cons[cCase[cExp]]) => boolean.
  eqCasesE(Cs1,Cs2) => case Cs1 in {
    | [] => isEmpty(Cs2)
    | [(_,Ptn1,Rep1),..Rest1] => case Cs2 in {
        | [(_,Ptn2,Rep2),..Rest2] =>
          foldExp(Ptn1,(),eqAlgebra)(Ptn2) && foldExp(Rep1,(),eqAlgebra)(Rep2) && eqCasesE(Rest1,Rest2)
        | _ default => .false
      }
    | _ default => .false
  }.

  eqCasesA:(cons[cCase[aAction]],cons[cCase[aAction]]) => boolean.
  eqCasesA(Cs1,Cs2) => case Cs1 in {
    | [] => isEmpty(Cs2)
    | [(_,Ptn1,Rep1),..Rest1] => case Cs2 in {
        | [(_,Ptn2,Rep2),..Rest2] =>
          foldExp(Ptn1,(),eqAlgebra)(Ptn2) && foldAct(Rep1,(),eqAlgebra)(Rep2) && eqCasesA(Rest1,Rest2)
        | _ default => .false
      }
    | _ default => .false
  }.

  eqTerm:(cExp,cExp) => boolean.
  eqTerm(E1,E2) => foldExp(E1,(),eqAlgebra)(E2).

  eqAct:(aAction,aAction) => boolean.
  eqAct(A1,A2) => foldAct(A1,(),eqAlgebra)(A2).

  public implementation equality[cExp] => {
    X == Y => eqTerm(X,Y)
  }

  public implementation equality[aAction] => {
    X == Y => eqAct(X,Y)
  }

  public implementation hasLoc[cExp] => {
    locOf(Tr) => case Tr in {
      | .cVoid(Lc) => Lc
      | .cAnon(Lc,_) => Lc
      | .cUnrch(Lc,_) => Lc
      | .cVar(Lc,_) => Lc
      | .cInt(Lc,_) => Lc
      | .cBig(Lc,_) => Lc
      | .cChar(Lc,_) => Lc
      | .cFlt(Lc,_) => Lc
      | .cString(Lc,_) => Lc
      | .cNth(Lc,_,_,_) => Lc
      | .cSetNth(Lc,_,_,_) => Lc
      | .cTerm(Lc,_,_,_) => Lc
      | .cClos(Lc,_,_,_,_) => Lc
      | .cSv(Lc,_) => Lc
      | .cSvDrf(Lc,_,_) => Lc
      | .cSvSet(Lc,_,_) => Lc
      | .cCel(Lc,_,_) => Lc
      | .cGet(Lc,_,_) => Lc
      | .cMatch(Lc,_,_) => Lc
      | .cLtt(Lc,_,_,_) => Lc
      | .cCase(Lc,_,_,_,_) => Lc
      | .cIxCase(Lc,_,_,_,_) => Lc
      | .cCall(Lc,_,_,_)=>Lc
      | .cOCall(Lc,_,_,_)=>Lc
      | .cXCall(Lc,_,_,_,_)=>Lc
      | .cXOCall(Lc,_,_,_,_)=>Lc
      | .cSeq(Lc,_,_) => Lc
      | .cCnj(Lc,_,_) => Lc
      | .cDsj(Lc,_,_) => Lc
      | .cNeg(Lc,_) => Lc
      | .cCnd(Lc,_,_,_) => Lc
      | .cAbort(Lc,_,_) => Lc
      | .cResum(Lc,_,_,_) => Lc
      | .cSusp(Lc,_,_,_) => Lc
      | .cRetyr(Lc,_,_,_) => Lc
      | .cVarNme(Lc,_,_,_) => Lc
      | .cTry(Lc,_,_,_,_) => Lc
      | .cThrw(Lc,_,_) => Lc
      | .cValof(Lc,_,_) => Lc
    }
  }

  public implementation hasType[cExp] => let{.
    tpOf(Tr) => case Tr in {
      | .cVoid(_) => .voidType
      | .cAnon(_,Tp) => Tp
      | .cUnrch(_,Tp) => Tp
      | .cVar(_,V) => typeOf(V)
      | .cInt(_,_) => intType
      | .cBig(_,_) => bigintType
      | .cChar(_,_) => chrType
      | .cFlt(_,_) => fltType
      | .cString(_,_) => strType
      | .cTerm(_,_,_,Tp) => Tp
      | .cClos(_,_,_,_,Tp) => Tp
      | .cSv(_,Tp) => Tp
      | .cSvDrf(_,_,Tp) => Tp
      | .cSvSet(_,_,Vl) => tpOf(Vl)
      | .cCel(_,_,Tp) => Tp
      | .cGet(_,_,Tp) => Tp
      | .cCall(_,_,_,Tp) => Tp
      | .cOCall(_,_,_,Tp) => Tp
      | .cXCall(_,_,_,Tp,_) => Tp
      | .cXOCall(_,_,_,Tp,_) => Tp
      | .cThrw(_,_,Tp) => Tp
      | .cNth(_,_,_,Tp) => Tp
      | .cSetNth(_,T,_,_) => tpOf(T)
      | .cSeq(_,_,R) => tpOf(R)
      | .cCnj(_,_,_) => boolType
      | .cDsj(_,_,_) => boolType
      | .cNeg(_,_) => boolType
      | .cLtt(_,_,_,E) => tpOf(E)
      | .cCase(_,_,_,_,Tp) => Tp
      | .cIxCase(_,_,_,_,Tp) => Tp
      | .cCnd(_,_,L,_) => tpOf(L)
      | .cMatch(_,_,_) => boolType
      | .cResum(_,_,_,T) => T
      | .cSusp(_,_,_,T) => T
      | .cRetyr(_,_,_,T) => T
      | .cTry(_,_,_,_,T) => T
      | .cValof(_,_,T) => T
      | .cAbort(_,_,T) => T
      | .cVarNme(_,_,_,E) => tpOf(E)
    }
  .} in {
    typeOf = tpOf
  }

  public implementation hasType[cV] => {
    typeOf(.cV(_,Tp)) => Tp.
  }

  public implementation display[cExp] => {
    disp(T) => dspExp(T,"")
  }

  public implementation display[cV] => {
    disp(.cV(Nm,Tp)) => "%#(Nm)\:$(Tp)".
  }

  public implementation hasLoc[aAction] => {
    locOf(Ac) => case Ac in {
      | .aNop(Lc) => Lc
      | .aSeq(Lc,_,_) => Lc
      | .aLbld(Lc,_,_) => Lc
      | .aBreak(Lc,_) => Lc
      | .aValis(Lc,_) => Lc
      | .aDo(Lc,_) => Lc
      | .aSetNth(Lc,_,_,_) => Lc
      | .aDefn(Lc,_,_) => Lc
      | .aMatch(Lc,_,_) => Lc
      | .aAsgn(Lc,_,_) => Lc
      | .aCase(Lc,_,_,_) => Lc
      | .aIxCase(Lc,_,_,_) => Lc
      | .aIftte(Lc,_,_,_) => Lc
      | .aWhile(Lc,_,_) => Lc
      | .aTry(Lc,_,_,_) => Lc
      | .aThrw(Lc,_) => Lc
      | .aLtt(Lc,_,_,_) => Lc
      | .aVarNme(Lc,_,_,_) => Lc
      | .aAbort(Lc,_) => Lc
    }
  }

  public implementation display[aAction] => {
    disp(A) => dspAct(A,"")
  }

  public implementation coercion[cExp,data->>exception] => {.
    _coerce(Tr) => case Tr in {
      | .cInt(_,Ix) => .intgr(Ix)
      | .cBig(_,Ix) => .bigi(Ix)
      | .cChar(_,Cx) => .chr(Cx)
      | .cFlt(_,Dx) => .flot(Dx)
      | .cString(_,Sx) => .strg(Sx)
      | .cVoid(_) => .symb(.tLbl("void",0))
      | .cInt(_,Ix) => .intgr(Ix)
      | .cTerm(_,Nm,Args,_) where NArgs .= mapArgs(Args,[]) =>
	.term(Nm,NArgs)
      | .cClos(_,L,A,F,Tp) where NF .= _coerce(F) => .clos(.tLbl(L,A),NF,Tp)
      | _ default => throw .exception("Cannot coerce $(Tr) to data")
    }.

    private mapArgs:(cons[cExp],cons[data]) => cons[data] throws exception.
    mapArgs([],So) => reverse(So).
    mapArgs([A,..As],So) where NA.=_coerce(A) => mapArgs(As,[NA,..So]).
  .}

  public implementation coercion[locn,cExp->>_] => {
    _coerce(Lc) where .locn(Nm,Line,Col,Off,Len).=Lc &&
	OLc .= .some(Lc) =>
      mcTpl(OLc,[.cString(OLc,Nm),
	    .cInt(OLc,Line),.cInt(OLc,Col),.cInt(OLc,Off),.cInt(OLc,Len)])
  }

  dropVar:(string,(cExp)=>option[cExp])=>(cExp)=>option[cExp].
  dropVar(Nm,Tst) => let{
    test(.cVar(_,.cV(Nm,_))) => .none.
    test(T) default => Tst(T)
  } in test.

  public implementation rewrite[cExp] => {
    rewrite(E,F) => rwTerm(E,F).
  }

  public implementation rewrite[aAction] => {
    rewrite(E,F) => rwAct(E,F).
  }

  public rewriteTerm:(cExp,(cExp)=>option[cExp])=>cExp.
  rewriteTerm(T,F) => rwTerm(T,F).

  public rewriteTerms:all e ~~ rewrite[e] |= (cons[e],(cExp)=>option[cExp])=>cons[e].
  rewriteTerms(Els,F) => (Els//(E)=>rewrite(E,F)).

  -- Catamorphism over cExp/aAction: one field per constructor,
  --   written once here instead of once per walker.

  public all env,r,ra ~~ treeAlgebra[env,r,ra] ::= treeAlgebra{
    onVoid   : (env,option[locn]) => r.
    onAnon   : (env,option[locn],tipe) => r.
    onUnrch  : (env,option[locn],tipe) => r.
    onVar    : (env,option[locn],cV) => r.
    onCel    : (env,option[locn],r,tipe) => r.
    onGet    : (env,option[locn],r,tipe) => r.
    onInt    : (env,option[locn],integer) => r.
    onChar   : (env,option[locn],char) => r.
    onBig    : (env,option[locn],bigint) => r.
    onFlt    : (env,option[locn],float) => r.
    onString : (env,option[locn],string) => r.
    onTerm   : (env,option[locn],string,cons[r],tipe) => r.
    onNth    : (env,option[locn],r,integer,tipe) => r.
    onSetNth : (env,option[locn],r,integer,r) => r.
    onClos   : (env,option[locn],string,integer,r,tipe) => r.
    onSv     : (env,option[locn],tipe) => r.
    onSvDrf  : (env,option[locn],r,tipe) => r.
    onSvSet  : (env,option[locn],r,r) => r.
    onCall   : (env,option[locn],string,cons[r],tipe) => r.
    onOCall  : (env,option[locn],r,cons[r],tipe) => r.
    onXCall  : (env,option[locn],string,cons[r],tipe,tipe) => r.
    onXOCall : (env,option[locn],r,cons[r],tipe,tipe) => r.
    onSeq    : (env,option[locn],r,r) => r.
    onCnj    : (env,option[locn],r,r) => r.
    onDsj    : (env,option[locn],r,r) => r.
    onNeg    : (env,option[locn],r) => r.
    onCnd    : (env,option[locn],r,r,r) => r.
    onLtt    : (env,option[locn],cV,r,r) => r.
    onCase   : (env,option[locn],r,cons[cCase[cExp]],r,tipe) => r.
    onIxCase : (env,option[locn],r,cons[cCase[cExp]],r,tipe) => r.
    onMatch  : (env,option[locn],r,r) => r.
    onResum  : (env,option[locn],r,r,tipe) => r.
    onSusp   : (env,option[locn],r,r,tipe) => r.
    onRetyr  : (env,option[locn],r,r,tipe) => r.
    onVarNme : (env,option[locn],string,r,r) => r.
    onAbort  : (env,option[locn],string,tipe) => r.
    onTry    : (env,option[locn],r,r,r,tipe) => r.
    onThrw   : (env,option[locn],r,tipe) => r.
    onValof  : (env,option[locn],ra,tipe) => r.

    onANop   : (env,option[locn]) => ra.
    onASeq   : (env,option[locn],ra,ra) => ra.
    onALbld  : (env,option[locn],string,ra) => ra.
    onABreak : (env,option[locn],string) => ra.
    onAValis : (env,option[locn],r) => ra.
    onADo    : (env,option[locn],r) => ra.
    onASetNth: (env,option[locn],r,integer,r) => ra.
    onADefn  : (env,option[locn],r,r) => ra.
    onAMatch : (env,option[locn],r,r) => ra.
    onAAsgn  : (env,option[locn],r,r) => ra.
    onACase  : (env,option[locn],r,cons[cCase[aAction]],ra) => ra.
    onAIxCase: (env,option[locn],r,cons[cCase[aAction]],ra) => ra.
    onAIftte : (env,option[locn],r,ra,ra) => ra.
    onAWhile : (env,option[locn],r,ra) => ra.
    onATry   : (env,option[locn],ra,r,ra) => ra.
    onAThrw  : (env,option[locn],r) => ra.
    onALtt   : (env,option[locn],cV,r,ra) => ra.
    onAVarNme: (env,option[locn],string,r,ra) => ra.
    onAAbort : (env,option[locn],string) => ra.

    extendLtt: (env,cV) => env.

    /* Escape hatch: a .some(Rp) return short-circuits foldExp/foldAct straight to Rp
    */

    onRaw : (env,cExp) => option[r].
    onARaw: (env,aAction) => option[ra].
    }.

  public foldExp:all env,r,ra ~~ (cExp,env,treeAlgebra[env,r,ra]) => r.
  foldExp(Trm,Nv,Alg) => Ov ?= Alg.onRaw(Nv,Trm) ?? Ov || case Trm in {
    | .cVoid(Lc) => Alg.onVoid(Nv,Lc)
    | .cAnon(Lc,Tp) => Alg.onAnon(Nv,Lc,Tp)
    | .cUnrch(Lc,Tp) => Alg.onUnrch(Nv,Lc,Tp)
    | .cVar(Lc,V) => Alg.onVar(Nv,Lc,V)
    | .cCel(Lc,E,Tp) => Alg.onCel(Nv,Lc,foldExp(E,Nv,Alg),Tp)
    | .cGet(Lc,E,Tp) => Alg.onGet(Nv,Lc,foldExp(E,Nv,Alg),Tp)
    | .cInt(Lc,Ix) => Alg.onInt(Nv,Lc,Ix)
    | .cChar(Lc,Cx) => Alg.onChar(Nv,Lc,Cx)
    | .cBig(Lc,Ix) => Alg.onBig(Nv,Lc,Ix)
    | .cFlt(Lc,Dx) => Alg.onFlt(Nv,Lc,Dx)
    | .cString(Lc,Sx) => Alg.onString(Nv,Lc,Sx)
    | .cTerm(Lc,Op,Args,Tp) => Alg.onTerm(Nv,Lc,Op,foldExps(Args,Nv,Alg),Tp)
    | .cNth(Lc,R,Ix,Tp) => Alg.onNth(Nv,Lc,foldExp(R,Nv,Alg),Ix,Tp)
    | .cSetNth(Lc,R,Ix,E) => Alg.onSetNth(Nv,Lc,foldExp(R,Nv,Alg),Ix,foldExp(E,Nv,Alg))
    | .cClos(Lc,L,A,F,Tp) => Alg.onClos(Nv,Lc,L,A,foldExp(F,Nv,Alg),Tp)
    | .cSv(Lc,Tp) => Alg.onSv(Nv,Lc,Tp)
    | .cSvDrf(Lc,E,Tp) => Alg.onSvDrf(Nv,Lc,foldExp(E,Nv,Alg),Tp)
    | .cSvSet(Lc,E,V) => Alg.onSvSet(Nv,Lc,foldExp(E,Nv,Alg),foldExp(V,Nv,Alg))
    | .cCall(Lc,Op,Args,Tp) => Alg.onCall(Nv,Lc,Op,foldExps(Args,Nv,Alg),Tp)
    | .cOCall(Lc,Op,Args,Tp) => Alg.onOCall(Nv,Lc,foldExp(Op,Nv,Alg),foldExps(Args,Nv,Alg),Tp)
    | .cXCall(Lc,Op,Args,Tp,ErTp) => Alg.onXCall(Nv,Lc,Op,foldExps(Args,Nv,Alg),Tp,ErTp)
    | .cXOCall(Lc,Op,Args,Tp,ErTp) => Alg.onXOCall(Nv,Lc,foldExp(Op,Nv,Alg),foldExps(Args,Nv,Alg),Tp,ErTp)
    | .cSeq(Lc,L,R) => Alg.onSeq(Nv,Lc,foldExp(L,Nv,Alg),foldExp(R,Nv,Alg))
    | .cCnj(Lc,L,R) => Alg.onCnj(Nv,Lc,foldExp(L,Nv,Alg),foldExp(R,Nv,Alg))
    | .cDsj(Lc,L,R) => Alg.onDsj(Nv,Lc,foldExp(L,Nv,Alg),foldExp(R,Nv,Alg))
    | .cNeg(Lc,R) => Alg.onNeg(Nv,Lc,foldExp(R,Nv,Alg))
    | .cCnd(Lc,G,L,R) => Alg.onCnd(Nv,Lc,foldExp(G,Nv,Alg),foldExp(L,Nv,Alg),foldExp(R,Nv,Alg))
    | .cLtt(Lc,V,D,E) => Alg.onLtt(Nv,Lc,V,foldExp(D,Nv,Alg),foldExp(E,Alg.extendLtt(Nv,V),Alg))
    | .cCase(Lc,Sel,Cases,Dflt,Tp) =>
      Alg.onCase(Nv,Lc,foldExp(Sel,Nv,Alg),Cases,foldExp(Dflt,Nv,Alg),Tp)
    | .cIxCase(Lc,Sel,Cases,Dflt,Tp) =>
      Alg.onIxCase(Nv,Lc,foldExp(Sel,Nv,Alg),Cases,foldExp(Dflt,Nv,Alg),Tp)
    | .cMatch(Lc,P,E) => Alg.onMatch(Nv,Lc,foldExp(P,Nv,Alg),foldExp(E,Nv,Alg))
    | .cResum(Lc,T,M,Tp) => Alg.onResum(Nv,Lc,foldExp(T,Nv,Alg),foldExp(M,Nv,Alg),Tp)
    | .cSusp(Lc,T,M,Tp) => Alg.onSusp(Nv,Lc,foldExp(T,Nv,Alg),foldExp(M,Nv,Alg),Tp)
    | .cRetyr(Lc,T,M,Tp) => Alg.onRetyr(Nv,Lc,foldExp(T,Nv,Alg),foldExp(M,Nv,Alg),Tp)
    | .cVarNme(Lc,N,V,E) => Alg.onVarNme(Nv,Lc,N,foldExp(V,Nv,Alg),foldExp(E,Nv,Alg))
    | .cAbort(Lc,Ms,Tp) => Alg.onAbort(Nv,Lc,Ms,Tp)
    | .cTry(Lc,B,E,H,Tp) => Alg.onTry(Nv,Lc,foldExp(B,Nv,Alg),foldExp(E,Nv,Alg),foldExp(H,Nv,Alg),Tp)
    | .cThrw(Lc,E,Tp) => Alg.onThrw(Nv,Lc,foldExp(E,Nv,Alg),Tp)
    | .cValof(Lc,A,Tp) => Alg.onValof(Nv,Lc,foldAct(A,Nv,Alg),Tp)
  }.

  foldExps:all env,r,ra ~~ (cons[cExp],env,treeAlgebra[env,r,ra]) => cons[r].
  foldExps(Es,Nv,Alg) => (Es//(E)=>foldExp(E,Nv,Alg)).

  foldCasesE:all env,r,ra ~~ (cons[cCase[cExp]],env,treeAlgebra[env,r,ra]) => cons[cCase[r]].
  foldCasesE(Cs,Nv,Alg) => (Cs//((Lc,Ptn,Rep))=>(Lc,Ptn,foldExp(Rep,Nv,Alg))).

  public foldAct:all env,r,ra ~~ (aAction,env,treeAlgebra[env,r,ra]) => ra.
  foldAct(Ac,Nv,Alg) => Ov ?= Alg.onARaw(Nv,Ac) ?? Ov || case Ac in {
    | .aNop(Lc) => Alg.onANop(Nv,Lc)
    | .aSeq(Lc,L,R) => Alg.onASeq(Nv,Lc,foldAct(L,Nv,Alg),foldAct(R,Nv,Alg))
    | .aLbld(Lc,L,A) => Alg.onALbld(Nv,Lc,L,foldAct(A,Nv,Alg))
    | .aBreak(Lc,L) => Alg.onABreak(Nv,Lc,L)
    | .aValis(Lc,E) => Alg.onAValis(Nv,Lc,foldExp(E,Nv,Alg))
    | .aDo(Lc,E) => Alg.onADo(Nv,Lc,foldExp(E,Nv,Alg))
    | .aSetNth(Lc,V,Ix,E) => Alg.onASetNth(Nv,Lc,foldExp(V,Nv,Alg),Ix,foldExp(E,Nv,Alg))
    | .aDefn(Lc,V,E) => Alg.onADefn(Nv,Lc,foldExp(V,Nv,Alg),foldExp(E,Nv,Alg))
    | .aMatch(Lc,V,E) => Alg.onAMatch(Nv,Lc,foldExp(V,Nv,Alg),foldExp(E,Nv,Alg))
    | .aAsgn(Lc,V,E) => Alg.onAAsgn(Nv,Lc,foldExp(V,Nv,Alg),foldExp(E,Nv,Alg))
    | .aCase(Lc,G,Cs,D) => Alg.onACase(Nv,Lc,foldExp(G,Nv,Alg),Cs,foldAct(D,Nv,Alg))
    | .aIxCase(Lc,G,Cs,D) => Alg.onAIxCase(Nv,Lc,foldExp(G,Nv,Alg),Cs,foldAct(D,Nv,Alg))
    | .aIftte(Lc,C,L,R) => Alg.onAIftte(Nv,Lc,foldExp(C,Nv,Alg),foldAct(L,Nv,Alg),foldAct(R,Nv,Alg))
    | .aWhile(Lc,C,B) => Alg.onAWhile(Nv,Lc,foldExp(C,Nv,Alg),foldAct(B,Nv,Alg))
    | .aTry(Lc,B,E,Hs) => Alg.onATry(Nv,Lc,foldAct(B,Nv,Alg),foldExp(E,Nv,Alg),foldAct(Hs,Nv,Alg))
    | .aThrw(Lc,E) => Alg.onAThrw(Nv,Lc,foldExp(E,Nv,Alg))
    | .aLtt(Lc,V,D,A) => Alg.onALtt(Nv,Lc,V,foldExp(D,Nv,Alg),foldAct(A,Alg.extendLtt(Nv,V),Alg))
    | .aVarNme(Lc,N,V,E) => Alg.onAVarNme(Nv,Lc,N,foldExp(V,Nv,Alg),foldAct(E,Nv,Alg))
    | .aAbort(Lc,Ms) => Alg.onAAbort(Nv,Lc,Ms)
  }.

  foldCasesA:all env,r,ra ~~ (cons[cCase[aAction]],env,treeAlgebra[env,r,ra]) => cons[cCase[ra]].
  foldCasesA(Cs,Nv,Alg) => (Cs//((Lc,Ptn,Rep))=>(Lc,Ptn,foldAct(Rep,Nv,Alg))).

  /* Rebuild-only baseline: foldExp(E,Nv,identityAlgebra) == E for any Nv, same as rwTerm
     with an always-.none test. Every other walker is either this with a handful of fields
     overridden, or a genuinely different result type (r=integer for a counter, r=boolean
     for a search, r=cExp with substitution for a rewriter). */
  public identityAlgebra:all env ~~ treeAlgebra[env,cExp,aAction].
  identityAlgebra = treeAlgebra{
    onVoid(_,Lc)=>.cVoid(Lc).
    onAnon(_,Lc,Tp)=>.cAnon(Lc,Tp).
    onUnrch(_,Lc,Tp)=>.cUnrch(Lc,Tp).
    onVar(_,Lc,V)=>.cVar(Lc,V).
    onCel(_,Lc,E,Tp)=>.cCel(Lc,E,Tp).
    onGet(_,Lc,E,Tp)=>.cGet(Lc,E,Tp).
    onInt(_,Lc,Ix)=>.cInt(Lc,Ix).
    onChar(_,Lc,Cx)=>.cChar(Lc,Cx).
    onBig(_,Lc,Ix)=>.cBig(Lc,Ix).
    onFlt(_,Lc,Dx)=>.cFlt(Lc,Dx).
    onString(_,Lc,Sx)=>.cString(Lc,Sx).
    onTerm(_,Lc,Op,Args,Tp)=>.cTerm(Lc,Op,Args,Tp).
    onNth(_,Lc,R,Ix,Tp)=>.cNth(Lc,R,Ix,Tp).
    onSetNth(_,Lc,R,Ix,E)=>.cSetNth(Lc,R,Ix,E).
    onClos(_,Lc,L,A,F,Tp)=>.cClos(Lc,L,A,F,Tp).
    onSv(_,Lc,Tp)=>.cSv(Lc,Tp).
    onSvDrf(_,Lc,E,Tp)=>.cSvDrf(Lc,E,Tp).
    onSvSet(_,Lc,E,V)=>.cSvSet(Lc,E,V).
    onCall(_,Lc,Op,Args,Tp)=>.cCall(Lc,Op,Args,Tp).
    onOCall(_,Lc,Op,Args,Tp)=>.cOCall(Lc,Op,Args,Tp).
    onXCall(_,Lc,Op,Args,Tp,ErTp)=>.cXCall(Lc,Op,Args,Tp,ErTp).
    onXOCall(_,Lc,Op,Args,Tp,ErTp)=>.cXOCall(Lc,Op,Args,Tp,ErTp).
    onSeq(_,Lc,L,R)=>.cSeq(Lc,L,R).
    onCnj(_,Lc,L,R)=>.cCnj(Lc,L,R).
    onDsj(_,Lc,L,R)=>.cDsj(Lc,L,R).
    onNeg(_,Lc,R)=>.cNeg(Lc,R).
    onCnd(_,Lc,G,L,R)=>.cCnd(Lc,G,L,R).
    onLtt(_,Lc,V,D,E)=>.cLtt(Lc,V,D,E).
    onCase(Nv,Lc,Sel,Cases,Dflt,Tp)=>.cCase(Lc,Sel,foldCasesE(Cases,Nv,identityAlgebra),Dflt,Tp).
    onIxCase(Nv,Lc,Sel,Cases,Dflt,Tp)=>.cIxCase(Lc,Sel,foldCasesE(Cases,Nv,identityAlgebra),Dflt,Tp).
    onMatch(_,Lc,P,E)=>.cMatch(Lc,P,E).
    onResum(_,Lc,T,M,Tp)=>.cResum(Lc,T,M,Tp).
    onSusp(_,Lc,T,M,Tp)=>.cSusp(Lc,T,M,Tp).
    onRetyr(_,Lc,T,M,Tp)=>.cRetyr(Lc,T,M,Tp).
    onVarNme(_,Lc,N,V,E)=>.cVarNme(Lc,N,V,E).
    onAbort(_,Lc,Ms,Tp)=>.cAbort(Lc,Ms,Tp).
    onTry(_,Lc,B,E,H,Tp)=>.cTry(Lc,B,E,H,Tp).
    onThrw(_,Lc,E,Tp)=>.cThrw(Lc,E,Tp).
    onValof(_,Lc,A,Tp)=>.cValof(Lc,A,Tp).

    onANop(_,Lc)=>.aNop(Lc).
    onASeq(_,Lc,L,R)=>.aSeq(Lc,L,R).
    onALbld(_,Lc,L,A)=>.aLbld(Lc,L,A).
    onABreak(_,Lc,L)=>.aBreak(Lc,L).
    onAValis(_,Lc,E)=>.aValis(Lc,E).
    onADo(_,Lc,E)=>.aDo(Lc,E).
    onASetNth(_,Lc,V,Ix,E)=>.aSetNth(Lc,V,Ix,E).
    onADefn(_,Lc,V,E)=>.aDefn(Lc,V,E).
    onAMatch(_,Lc,V,E)=>.aMatch(Lc,V,E).
    onAAsgn(_,Lc,V,E)=>.aAsgn(Lc,V,E).
    onACase(Nv,Lc,G,Cs,D)=>.aCase(Lc,G,foldCasesA(Cs,Nv,identityAlgebra),D).
    onAIxCase(Nv,Lc,G,Cs,D)=>.aIxCase(Lc,G,foldCasesA(Cs,Nv,identityAlgebra),D).
    onAIftte(_,Lc,C,L,R)=>.aIftte(Lc,C,L,R).
    onAWhile(_,Lc,C,B)=>.aWhile(Lc,C,B).
    onATry(_,Lc,B,E,Hs)=>.aTry(Lc,B,E,Hs).
    onAThrw(_,Lc,E)=>.aThrw(Lc,E).
    onALtt(_,Lc,V,D,A)=>.aLtt(Lc,V,D,A).
    onAVarNme(_,Lc,N,V,E)=>.aVarNme(Lc,N,V,E).
    onAAbort(_,Lc,Ms)=>.aAbort(Lc,Ms).

    extendLtt(Nv,_)=>Nv.
    onRaw(_,_)=>.none.
    onARaw(_,_)=>.none.
    }.

  /* rwTerm/rwAct: replace matching variables with replacement expression */
  public rewriteAlgebra:treeAlgebra[(cExp)=>option[cExp],cExp,aAction].
  rewriteAlgebra = treeAlgebra{
    onVoid(_,Lc)=>.cVoid(Lc).
    onAnon(_,Lc,Tp)=>.cAnon(Lc,Tp).
    onUnrch(_,Lc,Tp)=>.cUnrch(Lc,Tp).
    onVar(Tst,Lc,V)=>Vl?=Tst(.cVar(Lc,V)) ?? Vl || .cVar(Lc,V).
    onCel(_,Lc,E,Tp)=>.cCel(Lc,E,Tp).
    onGet(_,Lc,E,Tp)=>.cGet(Lc,E,Tp).
    onInt(_,Lc,Ix)=>.cInt(Lc,Ix).
    onChar(_,Lc,Cx)=>.cChar(Lc,Cx).
    onBig(_,Lc,Ix)=>.cBig(Lc,Ix).
    onFlt(_,Lc,Dx)=>.cFlt(Lc,Dx).
    onString(_,Lc,Sx)=>.cString(Lc,Sx).
    onTerm(_,Lc,Op,Args,Tp)=>.cTerm(Lc,Op,Args,Tp).
    onNth(_,Lc,R,Ix,Tp)=>.cNth(Lc,R,Ix,Tp).
    onSetNth(_,Lc,R,Ix,E)=>.cSetNth(Lc,R,Ix,E).
    onClos(_,Lc,L,A,F,Tp)=>.cClos(Lc,L,A,F,Tp).
    onSv(_,Lc,Tp)=>.cSv(Lc,Tp).
    onSvDrf(_,Lc,E,Tp)=>.cSvDrf(Lc,E,Tp).
    onSvSet(_,Lc,E,V)=>.cSvSet(Lc,E,V).
    onCall(_,Lc,Op,Args,Tp)=>.cCall(Lc,Op,Args,Tp).
    onOCall(_,Lc,Op,Args,Tp)=>.cOCall(Lc,Op,Args,Tp).
    onXCall(_,Lc,Op,Args,Tp,ErTp)=>.cXCall(Lc,Op,Args,Tp,ErTp).
    onXOCall(_,Lc,Op,Args,Tp,ErTp)=>.cXOCall(Lc,Op,Args,Tp,ErTp).
    onSeq(_,Lc,L,R)=>.cSeq(Lc,L,R).
    onCnj(_,Lc,L,R)=>.cCnj(Lc,L,R).
    onDsj(_,Lc,L,R)=>.cDsj(Lc,L,R).
    onNeg(_,Lc,R)=>.cNeg(Lc,R).
    onCnd(_,Lc,G,L,R)=>.cCnd(Lc,G,L,R).
    onLtt(_,Lc,V,D,E)=>.cLtt(Lc,V,D,E).
    onCase(Tst,Lc,Sel,Cases,Dflt,Tp)=>.cCase(Lc,Sel,rwCasesE(Cases,Tst),Dflt,Tp).
    onIxCase(Tst,Lc,Sel,Cases,Dflt,Tp)=>.cIxCase(Lc,Sel,rwCasesE(Cases,Tst),Dflt,Tp).
    onMatch(_,Lc,P,E)=>.cMatch(Lc,P,E).
    onResum(_,Lc,T,M,Tp)=>.cResum(Lc,T,M,Tp).
    onSusp(_,Lc,T,M,Tp)=>.cSusp(Lc,T,M,Tp).
    onRetyr(_,Lc,T,M,Tp)=>.cRetyr(Lc,T,M,Tp).
    onVarNme(_,Lc,N,V,E)=>.cVarNme(Lc,N,V,E).
    onAbort(_,Lc,Ms,Tp)=>.cAbort(Lc,Ms,Tp).
    onTry(_,Lc,B,E,H,Tp)=>.cTry(Lc,B,E,H,Tp).
    onThrw(_,Lc,E,Tp)=>.cThrw(Lc,E,Tp).
    onValof(_,Lc,A,Tp)=>.cValof(Lc,A,Tp).

    onANop(_,Lc)=>.aNop(Lc).
    onASeq(_,Lc,L,R)=>.aSeq(Lc,L,R).
    onALbld(_,Lc,L,A)=>.aLbld(Lc,L,A).
    onABreak(_,Lc,L)=>.aBreak(Lc,L).
    onAValis(_,Lc,E)=>.aValis(Lc,E).
    onADo(_,Lc,E)=>.aDo(Lc,E).
    onASetNth(_,Lc,V,Ix,E)=>.aSetNth(Lc,V,Ix,E).
    onADefn(_,Lc,V,E)=>.aDefn(Lc,V,E).
    onAMatch(_,Lc,V,E)=>.aMatch(Lc,V,E).
    onAAsgn(_,Lc,V,E)=>.aAsgn(Lc,V,E).
    onACase(Tst,Lc,G,Cs,D)=>.aCase(Lc,G,rwCasesA(Cs,Tst),D).
    onAIxCase(Tst,Lc,G,Cs,D)=>.aIxCase(Lc,G,rwCasesA(Cs,Tst),D).
    onAIftte(_,Lc,C,L,R)=>.aIftte(Lc,C,L,R).
    onAWhile(_,Lc,C,B)=>.aWhile(Lc,C,B).
    onATry(_,Lc,B,E,Hs)=>.aTry(Lc,B,E,Hs).
    onAThrw(_,Lc,E)=>.aThrw(Lc,E).
    onALtt(_,Lc,V,D,A)=>.aLtt(Lc,V,D,A).
    onAVarNme(_,Lc,N,V,E)=>.aVarNme(Lc,N,V,E).
    onAAbort(_,Lc,Ms)=>.aAbort(Lc,Ms).

    extendLtt(Tst,V)=>dropVar(cName(V),Tst).
    onRaw(_,_)=>.none.
    onARaw(_,_)=>.none.
    }.

  rwCasesE:(cons[cCase[cExp]],(cExp)=>option[cExp]) => cons[cCase[cExp]].
  rwCasesE(Cs,Tst) => (Cs//((Lc,Ptn,Rep))=>(Lc,foldExp(Ptn,Tst,rewriteAlgebra),foldExp(Rep,Tst,rewriteAlgebra))).

  rwCasesA:(cons[cCase[aAction]],(cExp)=>option[cExp]) => cons[cCase[aAction]].
  rwCasesA(Cs,Tst) => (Cs//((Lc,Ptn,Rep))=>(Lc,foldExp(Ptn,Tst,rewriteAlgebra),foldAct(Rep,Tst,rewriteAlgebra))).

  rwTerm:(cExp,(cExp)=>option[cExp]) => cExp.
  rwTerm(E,Tst) => foldExp(E,Tst,rewriteAlgebra).

  rwAct:(aAction,(cExp)=>option[cExp]) => aAction.
  rwAct(A,Tst) => foldAct(A,Tst,rewriteAlgebra).

  public implementation measured[cDefn->>integer] => {
    [|.fnDef(_,_,_,_,Vl)|] => termCnt(Vl).
    [|.prDef(_,_,_,_,Act)|] => actCnt(Act).
    [|.glDef(_,_,_,Vl)|] => termCnt(Vl).
    [|.tpDef(_,_,_,_)|] => 0.
    [|.lblDef(_,_,_,_)|] => 0.
  }

  /* termCnt/actCnt: one treeAlgebra[(),integer,integer] instance (countAlgebra, below)
     instead of two hand-written case-matches over cExp/aAction. sum1/sum0 are the only
     two recursion shapes actually in play: "sum the folded children, +1 for this node"
     and "sum the folded children, no +1" (cSeq, cValof, cLbld, the two Ix-case forms). */
  termCnt:(cExp) => integer.
  termCnt(E) => foldExp(E,(),countAlgebra).

  actCnt:(aAction) => integer.
  actCnt(A) => foldAct(A,(),countAlgebra).

  sum1:(cons[integer]) => integer.
  sum1(Ns) => foldLeft((N,Cx)=>N+Cx,1,Ns).

  sum0:(cons[integer]) => integer.
  sum0(Ns) => foldLeft((N,Cx)=>N+Cx,0,Ns).

  caseSumE:(cons[cCase[cExp]],(),integer) => integer.
  caseSumE(Cs,Nv,Base) => foldLeft(((_,Ptn,Rep),Cx)=>termCnt(Ptn)+foldExp(Rep,Nv,countAlgebra)+1+Cx,Base,Cs).

  caseSumA:(cons[cCase[aAction]],(),integer) => integer.
  caseSumA(Cs,Nv,Base) => foldLeft(((_,Ptn,Rep),Cx)=>termCnt(Ptn)+foldAct(Rep,Nv,countAlgebra)+1+Cx,Base,Cs).

  countAlgebra:treeAlgebra[(),integer,integer].
  countAlgebra = treeAlgebra{
    onVoid(_,_)=>1. onAnon(_,_,_)=>1. onUnrch(_,_,_)=>1. onVar(_,_,_)=>1.
    onInt(_,_,_)=>1. onChar(_,_,_)=>1. onBig(_,_,_)=>1. onFlt(_,_,_)=>1.
    onString(_,_,_)=>1. onSv(_,_,_)=>1. onAbort(_,_,_,_)=>1.

    onCel(_,_,E,_)=>sum1([E]).
    onGet(_,_,E,_)=>sum1([E]).
    onTerm(_,_,_,As,_)=>sum1(As).
    onNth(_,_,R,_,_)=>sum1([R]).
    onSetNth(_,_,R,_,E)=>sum1([R,E]).
    onClos(_,_,_,_,F,_)=>sum1([F]).
    onSvDrf(_,_,E,_)=>sum1([E]).
    onSvSet(_,_,E,V)=>sum1([E,V]).
    onCall(_,_,_,As,_)=>sum1(As).
    onOCall(_,_,Op,As,_)=>sum1([Op,..As]).
    onXCall(_,_,_,As,_,_)=>sum1(As).
    onXOCall(_,_,Op,As,_,_)=>sum1([Op,..As]).
    onThrw(_,_,E,_)=>sum1([E]).
    onSeq(_,_,L,R)=>sum0([L,R]).
    onCnj(_,_,L,R)=>sum1([L,R]).
    onDsj(_,_,L,R)=>sum1([L,R]).
    onNeg(_,_,R)=>sum1([R]).
    onCnd(_,_,G,L,R)=>sum1([G,L,R]).
    onLtt(_,_,_,D,E)=>sum1([D,E]).
    onCase(Nv,_,Sel,Cases,Dflt,_)=>caseSumE(Cases,Nv,Sel)+Dflt+1.
    onIxCase(Nv,_,Sel,Cases,Dflt,_)=>caseSumE(Cases,Nv,Sel)+Dflt+1.
    onMatch(_,_,P,E)=>sum1([P,E]).
    onResum(_,_,T,M,_)=>sum1([T,M]).
    onSusp(_,_,T,M,_)=>sum1([T,M]).
    onRetyr(_,_,T,M,_)=>sum1([T,M]).
    onVarNme(_,_,_,_,E)=>sum1([E]).
    onTry(_,_,B,_,H,_)=>sum1([B,H]).
    onValof(_,_,A,_)=>A.

    onANop(_,_)=>1. onABreak(_,_,_)=>1. onAAbort(_,_,_)=>1.
    onALbld(_,_,_,A)=>A.
    onASeq(_,_,L,R)=>sum0([L,R]).
    onAValis(_,_,E)=>sum1([E]).
    onADo(_,_,E)=>E.
    onASetNth(_,_,V,_,E)=>sum1([V,E]).
    onADefn(_,_,_,E)=>sum1([E]).
    onAMatch(_,_,V,E)=>sum1([V,E]).
    onAAsgn(_,_,V,E)=>sum1([V,E]).
    onACase(Nv,_,G,Cs,D)=>caseSumA(Cs,Nv,G)+D+1.
    onAIxCase(Nv,_,G,Cs,D)=>caseSumA(Cs,Nv,G)+D+1.
    onAIftte(_,_,C,L,R)=>sum1([C,L,R]).
    onAWhile(_,_,C,B)=>sum1([C,B]).
    onATry(_,_,B,_,Hs)=>sum1([B,Hs]).
    onAThrw(_,_,E)=>sum1([E]).
    onALtt(_,_,_,D,A)=>sum1([D,A]).
    onAVarNme(_,_,_,_,E)=>E.

    extendLtt(Nv,_)=>Nv.
    onRaw(_,_)=>.none.
    onARaw(_,_)=>.none.
    }.

  public freshenE:(cExp,map[termLbl,cExp])=>cExp.
  freshenE(E,Mp) => foldExp(E,[Mp],frshnAlgebra).

  scope ~> cons[map[termLbl,cExp]].

  hasBinding:(termLbl,scope) => option[cExp].
  hasBinding(_,[]) => .none.
  hasBinding(V,[M,.._]) where Vl ?= M[V] => .some(Vl).
  hasBinding(V,[_,..Ms]) => hasBinding(V,Ms).

  newVars:(set[cV],scope) => scope.
  newVars(Vrs,[Mp,..Ms]) => let{
    def:(cV,map[termLbl,cExp]) => map[termLbl,cExp].
    def(V,M) where Nm .= lName(V) => (_ ?= M[Nm] ?? M || M[Nm->newVar(V)]).
  } in [foldLeft(def,Mp,Vrs),..Ms].

  public newVar(.cV(Nm,Tp)) => .cVar(.none,.cV(genId(Nm),Tp)).

  pushScope:(scope) => scope.
  pushScope(Sc) => [[],..Sc].

  public lName:(cV) => termLbl.
  lName(.cV(Nm,Tp)) => .tLbl(Nm,arity(Tp)).

  public vName:(cV) => string.
  vName(.cV(Nm,_)) => Nm.

  public freshenA:(aAction,map[termLbl,cExp])=>aAction.
  freshenA(A,Mp) => foldAct(A,[Mp],frshnAlgebra).

  /* frshnE/frshnA: freshen an expression with new variables */

  frshnAlgebra:treeAlgebra[scope,cExp,aAction].
  frshnAlgebra = treeAlgebra{
    onVoid(_,Lc)=>.cVoid(Lc).
    onAnon(_,Lc,Tp)=>.cAnon(Lc,Tp).
    onUnrch(_,Lc,Tp)=>.cUnrch(Lc,Tp).
    onVar(Sc,Lc,V)=>(Rp?=hasBinding(lName(V),Sc) ?? Rp || .cVar(Lc,V)).
    onCel(_,Lc,E,Tp)=>.cCel(Lc,E,Tp).
    onGet(_,Lc,E,Tp)=>.cGet(Lc,E,Tp).
    onInt(_,Lc,Ix)=>.cInt(Lc,Ix).
    onChar(_,Lc,Cx)=>.cChar(Lc,Cx).
    onBig(_,Lc,Ix)=>.cBig(Lc,Ix).
    onFlt(_,Lc,Dx)=>.cFlt(Lc,Dx).
    onString(_,Lc,Sx)=>.cString(Lc,Sx).
    onTerm(_,Lc,Op,Args,Tp)=>.cTerm(Lc,Op,Args,Tp).
    onNth(_,Lc,R,Ix,Tp)=>.cNth(Lc,R,Ix,Tp).
    onSetNth(_,Lc,R,Ix,E)=>.cSetNth(Lc,R,Ix,E).
    onClos(_,Lc,L,A,F,Tp)=>.cClos(Lc,L,A,F,Tp).
    onSv(_,Lc,Tp)=>.cSv(Lc,Tp).
    onSvDrf(_,Lc,E,Tp)=>.cSvDrf(Lc,E,Tp).
    onSvSet(_,Lc,E,V)=>.cSvSet(Lc,E,V).
    onCall(_,Lc,Op,Args,Tp)=>.cCall(Lc,Op,Args,Tp).
    onOCall(_,Lc,Op,Args,Tp)=>.cOCall(Lc,Op,Args,Tp).
    onXCall(_,Lc,Op,Args,Tp,ErTp)=>.cXCall(Lc,Op,Args,Tp,ErTp).
    onXOCall(_,Lc,Op,Args,Tp,ErTp)=>.cXOCall(Lc,Op,Args,Tp,ErTp).
    onSeq(_,Lc,L,R)=>.cSeq(Lc,L,R).
    onCnj(_,Lc,L,R)=>.cCnj(Lc,L,R).
    onDsj(_,Lc,L,R)=>.cDsj(Lc,L,R).
    onNeg(_,Lc,R)=>.cNeg(Lc,R).
    onCnd(_,Lc,G,L,R)=>.cCnd(Lc,G,L,R).
    onLtt(_,Lc,V,D,E)=>.cLtt(Lc,V,D,E).
    onCase(Sc,Lc,Sel,Cases,Dflt,Tp)=>.cCase(Lc,Sel,frshnCasesE(Cases,Sc),Dflt,Tp).
    onIxCase(Sc,Lc,Sel,Cases,Dflt,Tp)=>.cIxCase(Lc,Sel,frshnCasesE(Cases,Sc),Dflt,Tp).
    onMatch(_,Lc,P,E)=>.cMatch(Lc,P,E).
    onResum(_,Lc,T,M,Tp)=>.cResum(Lc,T,M,Tp).
    onSusp(_,Lc,T,M,Tp)=>.cSusp(Lc,T,M,Tp).
    onRetyr(_,Lc,T,M,Tp)=>.cRetyr(Lc,T,M,Tp).
    onVarNme(_,Lc,N,V,E)=>.cVarNme(Lc,N,V,E).
    onAbort(_,Lc,Ms,Tp)=>.cAbort(Lc,Ms,Tp).
    onTry(_,Lc,B,E,H,Tp)=>.cTry(Lc,B,E,H,Tp).
    onThrw(_,Lc,E,Tp)=>.cThrw(Lc,E,Tp).
    onValof(_,Lc,A,Tp)=>.cValof(Lc,A,Tp).

    onANop(_,Lc)=>.aNop(Lc).
    onASeq(_,Lc,L,R)=>.aSeq(Lc,L,R).
    onALbld(_,Lc,L,A)=>.aLbld(Lc,L,A).
    onABreak(_,Lc,L)=>.aBreak(Lc,L).
    onAValis(_,Lc,E)=>.aValis(Lc,E).
    onADo(_,Lc,E)=>.aDo(Lc,E).
    onASetNth(_,Lc,V,Ix,E)=>.aSetNth(Lc,V,Ix,E).
    onADefn(_,Lc,V,E)=>.aDefn(Lc,V,E).
    onAMatch(_,Lc,V,E)=>.aMatch(Lc,V,E).
    onAAsgn(_,Lc,V,E)=>.aAsgn(Lc,V,E).
    onACase(Sc,Lc,G,Cs,D)=>.aCase(Lc,G,frshnCasesA(Cs,Sc),D).
    onAIxCase(Sc,Lc,G,Cs,D)=>.aIxCase(Lc,G,frshnCasesA(Cs,Sc),D).
    onAIftte(_,Lc,C,L,R)=>.aIftte(Lc,C,L,R).
    onAWhile(_,Lc,C,B)=>.aWhile(Lc,C,B).
    onATry(_,Lc,B,E,Hs)=>.aTry(Lc,B,E,Hs).
    onAThrw(_,Lc,E)=>.aThrw(Lc,E).
    onALtt(_,Lc,V,D,A)=>.aLtt(Lc,V,D,A).
    onAVarNme(_,Lc,N,V,E)=>.aVarNme(Lc,N,V,E).
    onAAbort(_,Lc,Ms)=>.aAbort(Lc,Ms).

    extendLtt(Sc,_)=>pushScope(Sc).
    onRaw=frshnOnRaw.
    onARaw=frshnOnARaw.
    }.

  frshnOnRaw:(scope,cExp) => option[cExp].
  frshnOnRaw(Sc,.cCnd(Lc,G,L,R)) => .some(valof{
      Sc1 = newVars(glVars(G,[]),Sc);
      valis .cCnd(Lc,foldExp(G,Sc1,frshnAlgebra),foldExp(L,Sc1,frshnAlgebra),foldExp(R,Sc,frshnAlgebra))
    }).
  frshnOnRaw(Sc,.cCnj(Lc,L,R)) => .some(valof{
      Sc1 = newVars(glVars(L,[]),Sc);
      valis .cCnj(Lc,foldExp(L,Sc1,frshnAlgebra),foldExp(R,Sc1,frshnAlgebra))
    }).
  frshnOnRaw(Sc,Trm) where .cDsj(Lc,L,R).=Trm => .some(valof{
      Sc1 = newVars(glVars(Trm,[]),Sc);
      valis .cDsj(Lc,foldExp(L,Sc1,frshnAlgebra),foldExp(R,Sc1,frshnAlgebra))
    }).
  frshnOnRaw(Sc,.cTry(Lc,B,E,H,Tp)) => .some(valof{
      Sc0 = pushScope(Sc);
      Sc1 = newVars(ptnVrs(E,[]),Sc0);
      valis .cTry(Lc,foldExp(B,Sc0,frshnAlgebra),foldExp(E,Sc1,frshnAlgebra),foldExp(H,Sc1,frshnAlgebra),Tp)
    }).
  frshnOnRaw(Sc,.cValof(Lc,Act,Tp)) =>
    .some(.cValof(Lc,foldAct(Act,pushScope(Sc),frshnAlgebra),Tp)).
  frshnOnRaw(_,_) default => .none.

  frshnOnARaw:(scope,aAction) => option[aAction].
  frshnOnARaw(Sc,.aSeq(Lc,.aDefn(LL,P,E),R)) => .some(valof{
      Sc1 = newVars(ptnVrs(P,[]),Sc);
      valis .aSeq(Lc,.aDefn(LL,foldExp(P,Sc1,frshnAlgebra),foldExp(E,Sc,frshnAlgebra)),foldAct(R,Sc1,frshnAlgebra))
    }).
  frshnOnARaw(Sc,.aSeq(Lc,.aMatch(LL,P,E),R)) => .some(valof{
      Sc1 = newVars(ptnVrs(P,[]),Sc);
      valis .aSeq(Lc,.aMatch(LL,foldExp(P,Sc1,frshnAlgebra),foldExp(E,Sc,frshnAlgebra)),foldAct(R,Sc1,frshnAlgebra))
    }).
  frshnOnARaw(Sc,.aIftte(Lc,C,L,R)) => .some(valof{
      Sc0 = pushScope(Sc);
      Sc1 = newVars(glVars(C,[]),Sc0);
      valis .aIftte(Lc,foldExp(C,Sc1,frshnAlgebra),foldAct(L,Sc1,frshnAlgebra),foldAct(R,Sc,frshnAlgebra))
    }).
  frshnOnARaw(Sc,.aWhile(Lc,C,B)) => .some(valof{
      Sc0 = pushScope(Sc);
      Sc1 = newVars(glVars(C,[]),Sc0);
      valis .aWhile(Lc,foldExp(C,Sc1,frshnAlgebra),foldAct(B,Sc1,frshnAlgebra))
    }).
  frshnOnARaw(_,_) default => .none.

  frshnCasesE:(cons[cCase[cExp]],scope) => cons[cCase[cExp]].
  frshnCasesE(Cs,Sc) => (Cs//((Lc,Ptn,Rep)) => valof{
      Sc1 = newVars(ptnVrs(Ptn,[]),pushScope(Sc));
      valis (Lc,foldExp(Ptn,Sc1,frshnAlgebra),foldExp(Rep,Sc1,frshnAlgebra))
    }).

  frshnCasesA:(cons[cCase[aAction]],scope) => cons[cCase[aAction]].
  frshnCasesA(Cs,Sc) => (Cs//((Lc,Ptn,Rep)) => valof{
      Sc1 = newVars(ptnVrs(Ptn,[]),pushScope(Sc));
      valis (Lc,foldExp(Ptn,Sc1,frshnAlgebra),foldAct(Rep,Sc1,frshnAlgebra))
    }).

  public implementation hasLoc[cDefn] => {
    locOf(Df) => case Df in {
      | .fnDef(Lc,_,_,_,_) => Lc
      | .prDef(Lc,_,_,_,_) => Lc
      | .glDef(Lc,_,_,_) => Lc
      | .tpDef(Lc,_,_,_) => Lc
      | .lblDef(Lc,_,_,_) => Lc
    }
  }

  public isTypeDef:(cDefn)=>boolean.
  isTypeDef(.tpDef(_,_,_,_)) => .true.
  isTypeDef(_) default => .false.

  public cName:(cV) => string.
  cName(.cV(Nm,_))=>Nm.

  public cType:(cV) => tipe.
  cType(.cV(_,Tp)) => Tp.

  public isCond:(cExp)=>boolean.
  isCond(C) => case C in {
    | .cCnj(_,_,_)=>.true
    | .cDsj(_,_,_)=>.true
    | .cNeg(_,_)=>.true
    | .cCnd(_,_,L,R)=>(isCond(L)||isCond(R))
    | .cMatch(_,_,_)=>.true
    | _ default => .false
  }

  public isGround:(cExp) => boolean.
  isGround(T) => case T in {
    | .cInt(_,_) => .true
    | .cBig(_,_) => .true
    | .cFlt(_,_) => .true
    | .cChar(_,_) => .true
    | .cString(_,_) => .true
    | .cTerm(_,_,Els,_) => {? E in Els *> isGround(E) ?}
    | .cClos(_,_,_,F,_) => isGround(F)
    | _ default => .false
  }

  public mergeGoal:(option[locn],option[cExp],option[cExp])=>option[cExp].
  mergeGoal(Lc,G1,G2) => case (G1,G2) in {
    | (G,.none) => G
    | (.none,G) => G
    | (.some(G),.some(H)) => .some(.cCnj(Lc,G,H))
  }

  public contract all e ~~ reform[e] ::= {
    mkCond:(option[locn],cExp,e,e)=>e.
    mkCase:(option[locn],cExp,cons[cCase[e]],e) => e.
    mkIndex:(option[locn],cExp,cons[cCase[e]],e) => e.
    decorateVar:(option[locn],string,cExp,e)=>e.
    pullWhere:(e) => (e,option[cExp]).
    mkLtt:(option[locn],cV,cExp,e) => e.
  }

  isTrue(.cTerm(_,"true",[],_)) => .true.
  isTrue(_) default => .false.

  isFalse(.cTerm(_,"false",[],_)) => .true.
  isFalse(_) default => .false.

  public implementation reform[cExp] => {.
    mkCond(Lc,Tst,Th,El) => valof{
      if .cCnd(_,T1,Th1,El1).=Th && El1==El then
	valis .cCnd(Lc,.cCnj(Lc,Tst,T1),Th1,El1)
      else if isTrue(Th) && isFalse(El) then
	valis Tst
      else if isFalse(Th) && isTrue(El) then
	valis .cNeg(Lc,Tst)
      else
      valis .cCnd(Lc,Tst,Th,El).
    }

    decorateVar(Lc,Nm,Vr,Val) => .cVarNme(Lc,Nm,Vr,Val).

    pullWhere(.cTerm(Lc,Lbl,Args,Tp)) where (NArgs,Gx) .= pullWheres(Args) =>
      (.cTerm(Lc,Lbl,NArgs,Tp),Gx).
    pullWhere(Exp) default => (Exp,.none).

    mkCase(Lc,Tst,[(PLc,Ptn,Val)],Deflt) => mkCond(Lc,.cMatch(PLc,Ptn,Tst),Val,Deflt).
    mkCase(Lc,V,Cases,Deflt) => .cCase(Lc,V,Cases,Deflt,typeOf(Deflt)).

    mkIndex(Lc,Tst,[(PLc,Ptn,Val)],Deflt) => mkCond(Lc,.cMatch(PLc,Ptn,Tst),Val,Deflt).
    mkIndex(Lc,V,Cases,Deflt) => .cIxCase(Lc,V,Cases,Deflt,typeOf(Deflt)).

    mkLtt(Lc,V,E,X) => .cLtt(Lc,V,E,X).
  .}

  public implementation reform[aAction] => {
    mkCond(Lc,Tst,Th,El) where
	.aIftte(Lc0,T1,Th1,El1).=Th && El1==El => .aIftte(Lc0,.cCnj(Lc,Tst,T1),Th1,El1).
    mkCond(Lc,.cMatch(_,.cAnon(_,_),_),Th,_) => Th.
    mkCond(Lc,.cMatch(_,.cVar(_,Vr),Vl),Th,_) => .aLtt(Lc,Vr,Vl,Th).
    mkCond(Lc,Tst,Th,El) => .aIftte(Lc,Tst,Th,El).

    decorateVar(Lc,Nm,Vr,Val) => .aVarNme(Lc,Nm,Vr,Val).

    pullWhere(A) => (A,.none).

    mkCase(Lc,Tst,[(PLc,Ptn,Val)],Deflt) => mkCond(Lc,.cMatch(PLc,Ptn,Tst),Val,Deflt).
    mkCase(Lc,V,Cases,Deflt) => .aCase(Lc,V,Cases,Deflt).
    
    mkIndex(Lc,Tst,[(PLc,Ptn,Val)],Deflt) => mkCond(Lc,.cMatch(PLc,Ptn,Tst),Val,Deflt).
    mkIndex(Lc,V,Cases,Deflt) => .aIxCase(Lc,V,Cases,Deflt).

    mkLtt(Lc,V,E,X) => .aLtt(Lc,V,E,X).
  }

  public pullWheres: all e ~~ reform[e],hasLoc[e] |= (cons[e])=>(cons[e],option[cExp]).
  pullWheres([]) => ([],.none).
  pullWheres([A,..As]) where (NA,NG).=pullWhere(A) && (NAs,Gx) .= pullWheres(As) =>
      ([NA,..NAs],mergeGoal(locOf(A),NG,Gx)).

  dfVars:(cons[cDefn],set[cV])=>set[cV].
  dfVars([.fnDef(_,Nm,Tp,_,_),..Ds],D) => dfVars(Ds,D\+.cV(Nm,Tp)).
  dfVars([.prDef(_,Nm,Tp,_,_),..Ds],D) => dfVars(Ds,D\+.cV(Nm,Tp)).
  dfVars([.glDef(_,Nm,Tp,_),..Ds],D) => dfVars(Ds,D\+.cV(Nm,Tp)).
  dfVars([_,..Ds],D) => dfVars(Ds,D).
  dfVars([],D) => D.

  dclVrs:(cons[decl],set[cV])=>set[cV].
  dclVrs(Decs,Vrs) => foldLeft(dclVr,Vrs,Decs).

  dclVr(Df,Vrs) => case Df in {
    | .funDec(_,_,Nm,Tp) => Vrs\+.cV(Nm,Tp)
    | .varDec(_,_,Nm,Tp) => Vrs\+.cV(Nm,Tp)
    | .cnsDec(_,_,Nm,Tp) => Vrs\+.cV(Nm,Tp)
    | .implDec(_,_,Nm,Tp) => Vrs\+.cV(Nm,Tp)
    | _ default => Vrs
  }

  public validProg:(cons[cDefn],cons[decl]) => ().
  validProg(Defs,Decls) => valof{
    
    D = dfVars(Defs,dclVrs(Decls,[]));

    for Df in Defs do{
      case Df in {
	| .fnDef(Lc,Nm,Tp,Args,Val) do {
	  D1 = foldLeft(((V,DD)=>DD\+V),D,Args);
	  if ~validE(Val,D1) then{
	    reportError("$(Df) not valid",Lc)
	  }
	}
	| .prDef(Lc,Nm,Tp,Args,Act) do {
	  D1 = foldLeft(((V,DD)=>DD\+V),D,Args);
	  if ~validA(Act,D1) then{
	    reportError("$(Df) not valid",Lc)
	  }
	}
	| .glDef(Lc,Nm,Tp,Val) do {
	  if ~validE(Val,D) then{
	    reportError("$(Df) not valid",Lc)
	  }
	}
	| _ do {}
      }
    };
    valis ()
  }

  validE:(cExp,set[cV]) => boolean.
  validE(Exp,Vrs) => case Exp in {
    | .cVoid(Lc) => .true
    | .cUnrch(_,_) => .true
    | .cAnon(Lc,_) => valof{
      reportError("anons not allowed in expressions",Lc);
      valis .false
    }
    | .cVar(Lc,V) =>  V .<. Vrs ?? .true || valof{
      reportError("variable $(V) not in scope",Lc);
      valis .false
    }
    | .cInt(_,_) => .true
    | .cBig(_,_) => .true
    | .cChar(_,_) => .true
    | .cString(_,_) => .true
    | .cFlt(_,_) => .true
    | .cTerm(_,_,Args,_) => {? E in Args *> validE(E,Vrs) ?}
    | .cNth(_,R,_,_) => validE(R,Vrs)
    | .cSetNth(_,R,_,V) => validE(R,Vrs) && validE(V,Vrs)
    | .cClos(_,_,_,F,_) => validE(F,Vrs)
    | .cSv(_,_) => .true
    | .cSvSet(_,E,V) => validE(E,Vrs) && validE(V,Vrs)
    | .cCel(_,E,_) => validE(E,Vrs)
    | .cGet(_,E,_) => validE(E,Vrs)
    | .cCall(_,_,Args,_) => {? E in Args *> validE(E,Vrs) ?}
    | .cOCall(_,Op,Args,_) => validE(Op,Vrs) && {? E in Args *> validE(E,Vrs) ?}
    | .cXCall(_,_,Args,_,_) => {? E in Args *> validE(E,Vrs) ?}
    | .cXOCall(_,Op,Args,_,_) => validE(Op,Vrs) && {? E in Args *> validE(E,Vrs) ?}
    | .cThrw(_,E,_) => validE(E,Vrs)
    | .cSeq(_,L,R) => validE(L,Vrs) && validE(R,Vrs)
    | .cCnj(_,L,R) => valof{
      V1 = glVars(L,Vrs);
      valis validE(L,V1) && validE(R,V1)
    }
    | .cDsj(_,L,R) => validE(L,Vrs) && validE(R,Vrs)
    | .cNeg(_,R) => validE(R,Vrs)
    | .cCnd(_,Ts,L,R) => valof{
      V1 = glVars(Ts,Vrs);
      valis validE(Ts,V1) && validE(L,V1) && validE(R,Vrs)
    }
    | .cLtt(_,B,V,E) => validE(V,Vrs) && validE(E,Vrs\+B)
    | .cCase(_,G,Cs,Df,_) => validE(G,Vrs) && validCases(Cs,validE,Vrs) && validE(Df,Vrs)
    | .cIxCase(_,G,Cs,Df,_) => validE(G,Vrs) && validCases(Cs,validE,Vrs) && validE(Df,Vrs)
    | .cMatch(_,V,E) => valof{
      V1 = glVars(E,Vrs);
      valis validPtn(V,V1) && validE(E,V1)
    }
    | .cResum(_,L,R,_) => validE(L,Vrs) && validE(R,Vrs)
    | .cSusp(_,L,R,_) => validE(L,Vrs) && validE(R,Vrs)
    | .cRetyr(_,L,R,_) => validE(L,Vrs) && validE(R,Vrs)
    | .cVarNme(_,_,_,E) => validE(E,Vrs)
    | .cAbort(_,_,_) => .true
    | .cTry(_,B,E,H,_) => valof{
      V1 = ptnVrs(E,Vrs);
      valis validE(B,Vrs) && validE(E,V1) && validE(H,V1)
    }
    | .cValof(_,A,_) => validA(A,Vrs)
  }

  validPtn:(cExp,set[cV]) => boolean.
  validPtn(Exp,Vrs) => case Exp in {
    | .cVoid(Lc) => .true
    | .cAnon(_,_) => .true
    | .cVar(_,_) => .true
    | .cInt(_,_) => .true
    | .cBig(_,_) => .true
    | .cChar(_,_) => .true
    | .cString(_,_) => .true
    | .cFlt(_,_) => .true
    | .cTerm(_,_,Args,_) => {? E in Args *> validPtn(E,Vrs) ?}
    | .cSvDrf(_,P,_) => validPtn(P,Vrs)
    | _ default => valof{
      reportError("invalid pattern: $(Exp)",locOf(Exp));
      valis .false
    }
  }
  
  validCases:all e ~~ (cons[cCase[e]],(e,set[cV])=>boolean,set[cV]) => boolean.
  validCases([],_,_) => .true.
  validCases([(_,A,E),..Cs],P,Vrs) => valof{
    D1 = ptnVrs(A,Vrs);
    valis validPtn(A,D1) && P(E,D1) && validCases(Cs,P,Vrs)
  }

  validA:(aAction,set[cV])=>boolean.
  validA(Ac,Vrs) => case Ac in {
    | .aNop(_) => .true
    | .aSeq(_,A1,A2) => valof{
      if .aDefn(_,P,V) .= A1 then{
	V1 = ptnVrs(P,Vrs);
	valis validPtn(P,V1) && validE(V,Vrs) && validA(A2,V1);
      } else if .aMatch(_,P,V) .= A1 then{
	V1 = ptnVrs(P,Vrs);
	valis validPtn(P,V1) && validE(V,Vrs) && validA(A2,V1);
      } else {
	valis validA(A1,Vrs) && validA(A2,Vrs)
      }
    }
    | .aLbld(_,_,A) => validA(A,Vrs)
    | .aBreak(_,L) => .true
    | .aValis(_,E) => validE(E,Vrs)
    | .aDo(_,E) => validE(E,Vrs)
    | .aSetNth(_,V,_,E) => validE(V,Vrs) && validE(E,Vrs)
    | .aDefn(_,P,E) => validPtn(P,ptnVrs(P,Vrs)) && validE(E,Vrs)
    | .aMatch(_,P,E) => validPtn(P,ptnVrs(P,Vrs)) && validE(E,Vrs)
    | .aAsgn(_,L,V) => validE(L,Vrs) && validE(V,Vrs)
    | .aCase(_,G,Cs,Df) => validE(G,Vrs) && validCases(Cs,validA,Vrs) && validA(Df,Vrs)
    | .aIxCase(_,G,Cs,Df) => validE(G,Vrs) && validCases(Cs,validA,Vrs) && validA(Df,Vrs)
    | .aIftte(_,G,Th,E) => valof{
      D1 = glVars(G,Vrs);
      valis validE(G,D1) && validA(Th,D1) && validA(E,Vrs)
    }
    | .aWhile(_,G,A) => valof{
      D1 = glVars(G,Vrs);
      valis validE(G,D1) && validA(A,D1)
    }
    | .aTry(_,B,E,Hs) => valof{
      V2 = ptnVrs(E,Vrs);
      valis validA(B,Vrs) && validE(E,V2) && validA(Hs,V2)
    }
    | .aThrw(_,E) => validE(E,Vrs)
    | .aLtt(_,B,V,A) => validE(V,Vrs) && validA(A,Vrs\+B)
    | .aVarNme(_,_,_,A) => validA(A,Vrs)
    | .aAbort(_,_) => .true
  }

  public ptnVrs:(cExp,set[cV]) => set[cV].
  ptnVrs(E,Vrs) => case E in {
    | .cVoid(_) => Vrs
    | .cAnon(_,_) => Vrs
    | .cVar(_,V) => Vrs\+V
    | .cInt(_,_) => Vrs
    | .cBig(_,_) => Vrs
    | .cChar(_,_) => Vrs
    | .cString(_,_) => Vrs
    | .cFlt(_,_) => Vrs
    | .cTerm(_,_,Args,_) => foldLeft(ptnVrs,Vrs,Args)
    | .cNth(_,R,_,_) => ptnVrs(R,Vrs)
    | .cSvDrf(_,S,_) => ptnVrs(S,Vrs)
  }

  public glVars:(cExp,set[cV])=>set[cV].
  glVars(G,Vrs) => case G in {
    | .cCnj(_,L,R) => glVars(R,glVars(L,Vrs))
    | .cDsj(_,L,R) => valof{
      D1 = glVars(L,[]);
      D2 = glVars(R,[]);
      valis Vrs\/(D1/\D2)
    }
    | .cNeg(_,R) => Vrs
    | .cCnd(_,Ts,L,R) => valof{
      D1 = glVars(Ts,glVars(L,[]));
      D2 = glVars(R,[]);
      valis Vrs\/(D1/\D2)
    }
    | .cMatch(_,P,_) => ptnVrs(P,Vrs)
    | _ default => Vrs
  }

  public contract all e ~~ present[e] ::= {
    present:(e,(cExp)=>boolean)=>boolean
  }

  public implementation present[cExp] => {
    present(E,F) => presentInExp(E,F)
  }

  public implementation present[aAction] => {
    present(A,F) => presentInAct(A,F)
  }

  public implementation present[cDefn] => {
    present(.fnDef(_,_,_,_,E),F) => presentInExp(E,F).
    present(.prDef(_,_,_,_,A),F) => presentInAct(A,F).
    present(.glDef(_,_,_,E),F) => presentInExp(E,F).
    present(_,_) default => .false
  }

  public lblUsed:(aAction,string) => boolean.
  lblUsed(A,Lb) => foldAct(A,((T)=>isBreak(T,Lb),(_)=>.false),presentAlgebra).

  public varUsed:all T ~~ present[T] |= (T,cV) => boolean.
  varUsed(T,V) => present(T,(Ex)=>(.cVar(_,VV).=Ex ?? VV==V || .false)).

  isBreak(.aBreak(_,Lb),Lb) => .true.
  isBreak(_,_) default => .false.

  /* present: check something being present */
  presentAlgebra:treeAlgebra[((aAction)=>boolean,(cExp)=>boolean),boolean,boolean].
  presentAlgebra = treeAlgebra{
    onVoid((_,ET),Lc)=>ET(.cVoid(Lc)).
    onAnon((_,ET),Lc,Tp)=>ET(.cAnon(Lc,Tp)).
    onUnrch((_,ET),Lc,Tp)=>ET(.cUnrch(Lc,Tp)).
    onVar((_,ET),Lc,V)=>ET(.cVar(Lc,V)).
    onCel(_,_,E,_)=>E.
    onGet(_,_,E,_)=>E.
    onInt((_,ET),Lc,Ix)=>ET(.cInt(Lc,Ix)).
    onChar((_,ET),Lc,Cx)=>ET(.cChar(Lc,Cx)).
    onBig((_,ET),Lc,Ix)=>ET(.cBig(Lc,Ix)).
    onFlt((_,ET),Lc,Dx)=>ET(.cFlt(Lc,Dx)).
    onString((_,ET),Lc,Sx)=>ET(.cString(Lc,Sx)).
    onTerm(_,_,_,Args,_)=>orAll(Args).
    onNth(_,_,R,_,_)=>R.
    onSetNth(_,_,R,_,E)=>R||E.
    onClos(_,_,_,_,F,_)=>F.
    onSv((_,ET),Lc,Tp)=>ET(.cSv(Lc,Tp)).
    onSvDrf(_,_,E,_)=>E.
    onSvSet(_,_,E,V)=>E||V.
    onCall(_,_,_,Args,_)=>orAll(Args).
    onOCall(_,_,Op,Args,_)=>Op||orAll(Args).
    onXCall(_,_,_,Args,_,_)=>orAll(Args).
    onXOCall(_,_,Op,Args,_,_)=>Op||orAll(Args).
    onSeq(_,_,L,R)=>L||R.
    onCnj(_,_,L,R)=>L||R.
    onDsj(_,_,L,R)=>L||R.
    onNeg(_,_,R)=>R.
    onCnd(_,_,T,L,R)=>T||L||R.
    onLtt(_,_,_,D,E)=>D||E.
    onCase(Env,_,Sel,Cases,Dflt,_)=>Sel||presentCasesE(Cases,Env)||Dflt.
    onIxCase(Env,_,Sel,Cases,Dflt,_)=>Sel||presentCasesE(Cases,Env)||Dflt.
    onMatch(_,_,P,E)=>P||E.
    onResum(_,_,T,M,_)=>T||M.
    onSusp(_,_,T,M,_)=>T||M.
    onRetyr(_,_,T,M,_)=>T||M.
    onVarNme(_,_,_,_,E)=>E.
    onAbort((_,ET),Lc,Ms,Tp)=>ET(.cAbort(Lc,Ms,Tp)).
    onTry(_,_,B,E,H,_)=>B||E||H.
    onThrw(_,_,E,_)=>E.
    onValof(_,_,A,_)=>A.

    onANop((AT,_),Lc)=>AT(.aNop(Lc)).
    onASeq(_,_,L,R)=>L||R.
    onALbld(_,_,_,A)=>A.
    onABreak((AT,_),Lc,L)=>AT(.aBreak(Lc,L)).
    onAValis(_,_,E)=>E.
    onADo(_,_,E)=>E.
    onASetNth(_,_,V,_,E)=>V||E.
    onADefn(_,_,_,E)=>E.
    onAMatch(_,_,P,E)=>P||E.
    onAAsgn(_,_,L,V)=>L||V.
    onACase(Env,_,G,Cs,D)=>G||presentCasesA(Cs,Env)||D.
    onAIxCase(Env,_,G,Cs,D)=>G||presentCasesA(Cs,Env)||D.
    onAIftte(_,_,G,Th,El)=>G||Th||El.
    onAWhile(_,_,G,B)=>G||B.
    onATry(_,_,B,E,H)=>B||E||H.
    onAThrw(_,_,E)=>E.
    onALtt(_,_,_,D,B)=>D||B.
    onAVarNme(_,_,_,_,B)=>B.
    onAAbort((AT,_),Lc,Ms)=>AT(.aAbort(Lc,Ms)).

    extendLtt(Nv,_)=>Nv.
    onRaw(_,_)=>.none.
    onARaw(_,_)=>.none.
    }.

  orAll:(cons[boolean]) => boolean.
  orAll(Bs) => foldLeft((B,Sf)=>B||Sf,.false,Bs).

  presentCasesE:(cons[cCase[cExp]],((aAction)=>boolean,(cExp)=>boolean)) => boolean.
  presentCasesE(Cs,Env) => case Cs in {
    | [] => .false
    | [(_,Ptn,Rep),..Rest] =>
      foldExp(Ptn,Env,presentAlgebra) || foldExp(Rep,Env,presentAlgebra) || presentCasesE(Rest,Env)
  }.

  presentCasesA:(cons[cCase[aAction]],((aAction)=>boolean,(cExp)=>boolean)) => boolean.
  presentCasesA(Cs,Env) => case Cs in {
    | [] => .false
    | [(_,Ptn,Rep),..Rest] =>
      foldExp(Ptn,Env,presentAlgebra) || foldAct(Rep,Env,presentAlgebra) || presentCasesA(Rest,Env)
  }.

  presentInExp:(cExp,(cExp)=>boolean) => boolean.
  presentInExp(E,F) => foldExp(E,((_)=>.false,F),presentAlgebra).

  presentInAct:(aAction,(cExp)=>boolean) => boolean.
  presentInAct(A,F) => foldAct(A,((_)=>.false,F),presentAlgebra).

  public freezeDefn:(cDefn) => data.
  freezeDefn(D) => case D in {
    | .fnDef(Lc,Nm,Tp,Vrs,Vl) => mkCons("fun",[Lc::data,.strg(Nm),encodeSig(Tp),
	mkTpl(Vrs//frzeVar),
	frzeExp(Vl)])
    | .prDef(Lc,Nm,Tp,Vrs,Act) => mkCons("prc",[Lc::data,.strg(Nm),encodeSig(Tp),
	mkTpl(Vrs//frzeVar),
	frzeAct(Act)])
    | .glDef(Lc,Nm,Tp,Vl) => mkCons("glb",[Lc::data,.strg(Nm),encodeSig(Tp),
	frzeExp(Vl)])
    | .tpDef(Lc,Tp,TpRl,Map) => mkCons("tpe",[Lc::data,encodeSig(Tp),
	.strg(encodeTpRlSignature(TpRl)),
	mkTpl(ixLeft((Lbl,Ix,Lst)=>[mkTpl([.symb(Lbl),.intgr(Ix)]),..Lst],[],Map))])
    | .lblDef(Lc,Lbl,Tp,Ix) => mkCons("cns",[Lc::data,.symb(Lbl),encodeSig(Tp),.intgr(Ix)])
  }

  frzeVar(.cV(Nm,Tp)) => mkTpl([.strg(Nm),encodeSig(Tp)]).

  frzeExp:(cExp)=>data.
  frzeExp(Ex) => case Ex in {
    | .cVoid(Lc) => mkCons("void",[Lc::data])
    | .cAnon(Lc,Tp) => mkCons("anon",[Lc::data,encodeSig(Tp)])
    | .cUnrch(Lc,Tp) => mkCons("unreachable",[Lc::data,encodeSig(Tp)])
    | .cVar(Lc,.cV(V,Tp)) => mkCons("var",[Lc::data,.strg(V),encodeSig(Tp)])
    | .cInt(Lc,Ix) => mkCons("int",[Lc::data,.intgr(Ix)])
    | .cChar(Lc,Cx) => mkCons("chr",[Lc::data,.chr(Cx)])
    | .cFlt(Lc,Dx) => mkCons("flt",[Lc::data,.flot(Dx)])
    | .cBig(Lc,Bx) => mkCons("big",[Lc::data,.strg(Bx::string)])
    | .cString(Lc,Sx) => mkCons("str",[Lc::data,.strg(Sx)])
    | .cTerm(Lc,Nm,Args,Tp) => mkCons("term",[Lc::data,.strg(Nm),mkTpl(Args//frzeExp),
	.strg(encodeSignature(Tp))])
    | .cNth(Lc,T,Ix,Tp) => mkCons("nth",[Lc::data,frzeExp(T),.intgr(Ix),
	.strg(encodeSignature(Tp))])
    | .cSetNth(Lc,T,Ix,R) => mkCons("setnth",[Lc::data,frzeExp(T),.intgr(Ix),
	frzeExp(R)])
    | .cClos(Lc,N,A,F,Tp) => mkCons("clos",[Lc::data,.strg(N),.intgr(A),frzeExp(F),
	.strg(encodeSignature(Tp))])
    | .cSv(Lc,Tp) => mkCons("sav",[Lc::data,.strg(encodeSignature(Tp))])
    | .cSvDrf(Lc,E,Tp) => mkCons("svget",[Lc::data,frzeExp(E),
	.strg(encodeSignature(Tp))])
    | .cSvSet(Lc,E,V) => mkCons("svset",[Lc::data,frzeExp(E),frzeExp(V)])
    | .cCall(Lc,Nm,Args,Tp) => mkCons("call",[Lc::data,.strg(Nm),mkTpl(Args//frzeExp),
	.strg(encodeSignature(Tp))])
    | .cOCall(Lc,Op,Args,Tp) => mkCons("ocll",[Lc::data,frzeExp(Op),
	mkTpl(Args//frzeExp),.strg(encodeSignature(Tp))])
    | .cXCall(Lc,Nm,Args,Tp,ETp) => mkCons("xcall",[Lc::data,.strg(Nm),mkTpl(Args//frzeExp),
	.strg(encodeSignature(Tp)),.strg(encodeSignature(ETp))])
    | .cXOCall(Lc,Op,Args,Tp,ETp) => mkCons("xocll",[Lc::data,frzeExp(Op),
	mkTpl(Args//frzeExp),.strg(encodeSignature(Tp)),.strg(encodeSignature(ETp))])
    | .cCel(Lc,E,Tp) => mkCons("cel",[Lc::data,frzeExp(E),
	.strg(encodeSignature(Tp))])
    | .cGet(Lc,E,Tp) => mkCons("get",[Lc::data,frzeExp(E),
	.strg(encodeSignature(Tp))])
    | .cThrw(Lc,X,Tp) => mkCons("throw",[Lc::data,
	frzeExp(X),.strg(encodeSignature(Tp))])
    | .cSeq(Lc,L,R) => mkCons("seq",[Lc::data,frzeExp(L),frzeExp(R)])
    | .cCnj(Lc,L,R) => mkCons("cnj",[Lc::data,frzeExp(L),frzeExp(R)])
    | .cDsj(Lc,L,R) => mkCons("dsj",[Lc::data,frzeExp(L),frzeExp(R)])
    | .cNeg(Lc,R) => mkCons("neg",[Lc::data,frzeExp(R)])
    | .cCnd(Lc,T,L,R) => mkCons("cnd",[Lc::data,frzeExp(T),frzeExp(L),frzeExp(R)])
    | .cMatch(Lc,L,R) => mkCons("mtch",[Lc::data,frzeExp(L),frzeExp(R)])
    | .cLtt(Lc,.cV(V,Tp),B,X) => mkCons("ltt",[Lc::data,.strg(V),encodeSig(Tp),
	frzeExp(B),frzeExp(X)])
    | .cCase(Lc,G,Cs,Df,Tp) => mkCons("case",[Lc::data,frzeExp(G),
	freezeCases(Cs,frzeExp),frzeExp(Df),encodeSig(Tp)])
    | .cIxCase(Lc,G,Cs,Df,Tp) => mkCons("index",[Lc::data,frzeExp(G),
	freezeCases(Cs,frzeExp),frzeExp(Df),encodeSig(Tp)])
    | .cAbort(Lc,Msg,Tp) => mkCons("abrt",[Lc::data,.strg(Msg),encodeSig(Tp)])
    | .cTry(Lc,B,E,H,Tp) => mkCons("try",[Lc::data,frzeExp(B),frzeExp(E),frzeExp(H),encodeSig(Tp)])
    | .cResum(Lc,L,R,Tp) => mkCons("rsme",[Lc::data,frzeExp(L),frzeExp(R),encodeSig(Tp)])
    | .cSusp(Lc,L,R,Tp) => mkCons("susp",[Lc::data,frzeExp(L),frzeExp(R),encodeSig(Tp)])
    | .cRetyr(Lc,L,R,Tp) => mkCons("retyr",[Lc::data,frzeExp(L),frzeExp(R),encodeSig(Tp)])
    | .cVarNme(Lc,N,V,B) => mkCons("vrs",[Lc::data,.strg(N),frzeExp(V),frzeExp(B)])
    | .cValof(Lc,A,Tp) => mkCons("valof",[Lc::data,frzeAct(A),encodeSig(Tp)])
  }

  freezeCases:all e ~~ (cons[cCase[e]],(e)=>data) => data.
  freezeCases(Cs,F) => mkTpl(Cs//((Lc,Pt,E))=>mkTpl([Lc::data,frzeExp(Pt),F(E)])).

  frzeAct:(aAction)=>data.
  frzeAct(Ac) => case Ac in {
    | .aNop(Lc) => mkCons("nop",[Lc::data])
    | .aSeq(Lc,L,R) => mkCons("seq",[Lc::data,frzeAct(L),frzeAct(R)])
    | .aLbld(Lc,L,I) => mkCons("lbld",[Lc::data,.strg(L),frzeAct(I)])
    | .aBreak(Lc,L) => mkCons("brek",[Lc::data,.strg(L)])
    | .aValis(Lc,V) => mkCons("vls",[Lc::data,frzeExp(V)])
    | .aDo(Lc,V) => mkCons("do",[Lc::data,frzeExp(V)])
    | .aSetNth(Lc,V,Ix,E) => mkCons("setix",[Lc::data,frzeExp(V),.intgr(Ix),frzeExp(E)])
    | .aDefn(Lc,P,V) => mkCons("defn",[Lc::data,frzeExp(P),frzeExp(V)])
    | .aMatch(Lc,P,V) => mkCons("match",[Lc::data,frzeExp(P),frzeExp(V)])
    | .aAsgn(Lc,P,V) => mkCons("asgn",[Lc::data,frzeExp(P),frzeExp(V)])
    | .aCase(Lc,G,C,D) => mkCons("case",[Lc::data,frzeExp(G),
	freezeCases(C,frzeAct),frzeAct(D)])
    | .aIxCase(Lc,G,C,D) => mkCons("index",[Lc::data,frzeExp(G),
	freezeCases(C,frzeAct),frzeAct(D)])
    | .aIftte(Lc,T,L,R) => mkCons("iftt",[Lc::data,frzeExp(T),frzeAct(L),frzeAct(R)])
    | .aWhile(Lc,T,I) => mkCons("whle",[Lc::data,frzeExp(T),frzeAct(I)])
    | .aTry(Lc,B,E,H) => mkCons("try",[Lc::data,frzeAct(B),frzeExp(E),frzeAct(H)])
    | .aThrw(Lc,E) => mkCons("throw",[Lc::data,frzeExp(E)])
    | .aLtt(Lc,.cV(V,Tp),B,X) => mkCons("ltt",[Lc::data,.strg(V),encodeSig(Tp),
	frzeExp(B),frzeAct(X)])
    | .aVarNme(Lc,N,V,B) => mkCons("vrs",[Lc::data,.strg(N),frzeExp(V),frzeAct(B)])
    | .aAbort(Lc,Msg) => mkCons("abrt",[Lc::data,.strg(Msg)])
  }

  public thawDefn:(data) => cDefn throws exception.
  thawDefn(D) => case D in {
    | .term("fun",[Lc,.strg(Nm),Sig,.term(_,Vrs),Vl]) =>
      .fnDef(thawLoc(Lc),Nm,decodeSig(Sig),Vrs//thawVr,thwTrm(Vl))
    | .term("prc",[Lc,.strg(Nm),Sig,.term(_,Vrs),Vl]) =>
      .prDef(thawLoc(Lc),Nm,decodeSig(Sig),Vrs//thawVr,thawAct(Vl))
    | .term("glb",[Lc,.strg(V),Sig,Vl]) =>
      .glDef(thawLoc(Lc),V,decodeSig(Sig),thwTrm(Vl))
    | .term("tpe",[Lc,Sig,.strg(RlSig),.term(_,Map)]) =>
      .tpDef(thawLoc(Lc),decodeSig(Sig),decodeTypeRuleSignature(RlSig),
      foldLeft((.term(_,[.symb(Lbl),.intgr(Ix)]),Mp)=>Mp[Lbl->Ix],[],Map))
    | .term("cns",[Lc,.symb(Lbl),Sig,.intgr(Ix)]) =>
      .lblDef(thawLoc(Lc),Lbl,decodeSig(Sig),Ix)
  }

  thawVr(.term(_,[.strg(Vn),VSig]))=>.cV(Vn,decodeSig(VSig)).

  thwTrm:(data) => cExp throws exception.
  thwTrm(D) => case D in {
    | .term("void",[Lc]) => .cVoid(thawLoc(Lc))
    | .term("anon",[Lc,Sig]) => .cAnon(thawLoc(Lc),decodeSig(Sig))
    | .term("unreachable",[Lc,Sig]) => .cUnrch(thawLoc(Lc),decodeSig(Sig))
    | .term("var",[Lc,.strg(V),Sig]) => .cVar(thawLoc(Lc),.cV(V,decodeSig(Sig)))
    | .term("int",[Lc,.intgr(Ix)]) => .cInt(thawLoc(Lc),Ix)
    | .term("chr",[Lc,.chr(Ix)]) => .cChar(thawLoc(Lc),Ix)
    | .term("flt",[Lc,.flot(Dx)]) => .cFlt(thawLoc(Lc),Dx)
    | .term("big",[Lc,.strg(Bx)]) => .cBig(thawLoc(Lc),Bx::bigint)
    | .term("str",[Lc,.strg(Sx)]) => .cString(thawLoc(Lc),Sx)
    | .term("term",[Lc,.strg(Nm),.term(_,Args),Sig]) =>
      .cTerm(thawLoc(Lc),Nm,thwTerms(Args),decodeSig(Sig))
    | .term("nth",[Lc,E,.intgr(Ix),Sig]) =>
      .cNth(thawLoc(Lc),thwTrm(E),Ix,decodeSig(Sig))
    | .term("setnth",[Lc,E,.intgr(Ix),R]) =>
      .cSetNth(thawLoc(Lc),thwTrm(E),Ix,thwTrm(R))
    | .term("clos",[Lc,.strg(N),.intgr(A),F,Sig]) =>
      .cClos(thawLoc(Lc),N,A,thwTrm(F),decodeSig(Sig))
    | .term("sav",[Lc,Sig]) => .cSv(thawLoc(Lc),decodeSig(Sig))
    | .term("svget",[Lc,E,Sig]) => .cSvDrf(thawLoc(Lc),thwTrm(E),decodeSig(Sig))
    | .term("svset",[Lc,E,V]) => .cSvSet(thawLoc(Lc),thwTrm(E),thwTrm(V))
    | .term("cel",[Lc,E,Sig]) => .cCel(thawLoc(Lc),thwTrm(E),decodeSig(Sig))
    | .term("get",[Lc,E,Sig]) => .cGet(thawLoc(Lc),thwTrm(E),decodeSig(Sig))
    | .term("call",[Lc,.strg(Nm),.term(_,Args),Sig]) =>
      .cCall(thawLoc(Lc),Nm,thwTerms(Args),decodeSig(Sig))
    | .term("ocll",[Lc,Op,.term(_,Args),Sig]) =>
      .cOCall(thawLoc(Lc),thwTrm(Op),thwTerms(Args),decodeSig(Sig))
    | .term("xcall",[Lc,.strg(Nm),.term(_,Args),Sig,ESig]) =>
      .cXCall(thawLoc(Lc),Nm,thwTerms(Args),decodeSig(Sig),decodeSig(ESig))
    | .term("xocll",[Lc,Op,.term(_,Args),Sig,ESig]) =>
      .cXOCall(thawLoc(Lc),thwTrm(Op),thwTerms(Args),decodeSig(Sig),decodeSig(ESig))
    | .term("throw",[Lc,Op,Sig]) =>
      .cThrw(thawLoc(Lc),thwTrm(Op),decodeSig(Sig))
    | .term("seq",[Lc,L,R]) =>
      .cSeq(thawLoc(Lc),thwTrm(L),thwTrm(R))
    | .term("cnj",[Lc,L,R]) =>
      .cCnj(thawLoc(Lc),thwTrm(L),thwTrm(R))
    | .term("dsj",[Lc,L,R]) =>
      .cDsj(thawLoc(Lc),thwTrm(L),thwTrm(R))
    | .term("neg",[Lc,R]) =>
      .cNeg(thawLoc(Lc),thwTrm(R))
    | .term("cnd",[Lc,T,L,R]) =>
      .cCnd(thawLoc(Lc),thwTrm(T),thwTrm(L),thwTrm(R))
    | .term("mtch",[Lc,L,R]) => .cMatch(thawLoc(Lc),thwTrm(L),thwTrm(R))
    | .term("ltt",[Lc,.strg(V),Sig,B,X]) =>
      .cLtt(thawLoc(Lc),.cV(V,decodeSig(Sig)),thwTrm(B),thwTrm(X))
    | .term("case",[Lc,G,Cs,Df,Sig]) => .cCase(thawLoc(Lc),thwTrm(G),
      thawCases(Cs,thwTrm),thwTrm(Df),decodeSig(Sig))
    | .term("index",[Lc,G,Cs,Df,Sig]) => .cIxCase(thawLoc(Lc),thwTrm(G),
      thawCases(Cs,thwTrm),thwTrm(Df),decodeSig(Sig))
    | .term("abrt",[Lc,.strg(M),Sig]) => .cAbort(thawLoc(Lc),M,decodeSig(Sig))
    | .term("try",[Lc,B,E,H,Sig]) =>
      .cTry(thawLoc(Lc),thwTrm(B),thwTrm(E),thwTrm(H),decodeSig(Sig))
    | .term("rsme",[Lc,L,R,Sig]) => .cResum(thawLoc(Lc),thwTrm(L),thwTrm(R),decodeSig(Sig))
    | .term("susp",[Lc,L,R,Sig]) => .cSusp(thawLoc(Lc),thwTrm(L),thwTrm(R),decodeSig(Sig))
    | .term("retyr",[Lc,L,R,Sig]) => .cRetyr(thawLoc(Lc),thwTrm(L),thwTrm(R),decodeSig(Sig))
    | .term("vrs",[Lc,.strg(N),V,B]) => .cVarNme(thawLoc(Lc),N,thwTrm(V),thwTrm(B))
    | .term("valof",[Lc,A,T]) => .cValof(thawLoc(Lc),thawAct(A),decodeSig(T))
  }

  thwTerms:(cons[data]) => cons[cExp] throws exception.
  thwTerms([]) => [].
  thwTerms([A,..As]) => [thwTrm(A),..thwTerms(As)].
  

  thawLoc(L:data) => L::option[locn].

  thawCases:all e ~~ (data,(data)=>e throws exception) => cons[cCase[e]].
  thawCases(.term(_,Args),T) => (Args//(.term(_,[Lc,P,E]))=>
      (thawLoc(Lc),(try thwTrm(P) catch { _ => unreachable}),(try T(E) catch { _ => unreachable}))).

  thawAct:(data) => aAction throws exception.
  thawAct(A) => case A in {
    | .term("nop",[Lc]) => .aNop(thawLoc(Lc))
    | .term("seq",[Lc,L,R]) => .aSeq(thawLoc(Lc),thawAct(L),thawAct(R))
    | .term("lbld",[Lc,.strg(L),I]) => .aLbld(thawLoc(Lc),L,thawAct(I))
    | .term("brek",[Lc,.strg(L)]) => .aBreak(thawLoc(Lc),L)
    | .term("vls",[Lc,V]) => .aValis(thawLoc(Lc),thwTrm(V))
    | .term("do",[Lc,V]) => .aDo(thawLoc(Lc),thwTrm(V))
    | .term("setix",[Lc,V,.intgr(Ix),E]) => .aSetNth(thawLoc(Lc),thwTrm(V),Ix,thwTrm(E))
    | .term("defn",[Lc,P,V]) => .aDefn(thawLoc(Lc),thwTrm(P),thwTrm(V))
    | .term("match",[Lc,P,V]) => .aMatch(thawLoc(Lc),thwTrm(P),thwTrm(V))
    | .term("asgn",[Lc,P,V]) => .aAsgn(thawLoc(Lc),thwTrm(P),thwTrm(V))
    | .term("case",[Lc,G,C,D]) => .aCase(thawLoc(Lc),thwTrm(G),thawCases(C,thawAct),
      thawAct(D))
    | .term("index",[Lc,G,C,D]) => .aIxCase(thawLoc(Lc),thwTrm(G),thawCases(C,thawAct),
      thawAct(D))
    | .term("iftt",[Lc,T,L,R]) => .aIftte(thawLoc(Lc),thwTrm(T),thawAct(L),thawAct(R))
    | .term("whle",[Lc,T,I]) => .aWhile(thawLoc(Lc),thwTrm(T),thawAct(I))
    | .term("try",[Lc,B,E,H]) => .aTry(thawLoc(Lc),thawAct(B),thwTrm(E),thawAct(H))
    | .term("throw",[Lc,E]) => .aThrw(thawLoc(Lc),thwTrm(E))
    | .term("vrs",[Lc,.strg(N),V,B]) => .aVarNme(thawLoc(Lc),N,thwTrm(V),thawAct(B))
    | .term("ltt",[Lc,.strg(V),Sig,B,X]) =>
      .aLtt(thawLoc(Lc),.cV(V,decodeSig(Sig)),thwTrm(B),thawAct(X))
    | .term("abrt",[Lc,.strg(M)]) => .aAbort(thawLoc(Lc),M)
  }

  glSpec ~> (string,cDefn,cons[string]).

  nameOf(.fnDef(_,Nm,_,_,_)) => Nm.
  nameOf(.prDef(_,Nm,_,_,_)) => Nm.
  nameOf(.glDef(_,Nm,_,_)) => Nm.
  nameOf(.tpDef(_,Tp,_,_)) => tpName(Tp).
  nameOf(.lblDef(_,.tLbl(Nm,_),_,_)) => Nm.

  public sortDefs:(cons[cDefn]) => cons[cons[cDefn]].
  sortDefs(Defs) => valof{
    Globals : map[string,cDefn];
    Globals = { nameOf(Df)->Df | Df in Defs };
    AllRefs = foldRight((Df,A)=>[(nameOf(Df),Df,findRefs(Df,Globals)),..A],([]:cons[glSpec]),Defs);
    valis (topsort(AllRefs) // ((G) => (G//(((_,Df,_))=>Df))));
  }

  implementation depends[glSpec->>string] => {
    references((_,_,Refs)) => Refs.
    defined((Nm,_,_),Rf) => Nm==Rf.
  }

  findRefs:(cDefn,map[string,cDefn])=> cons[string].
  findRefs(Df,Gls) => let{
    findVRef(.cVar(_,.cV(V,_)),.inExp,SoF) =>
      ({? _ ?= Gls[V] && ~ V in SoF ?} ?? [V,..SoF] || SoF).
    findVRef(_,_,SoF) default => SoF.
  } in let{
    findD:(cDefn)=> cons[string].
    findD(.fnDef(_,_,_,_,Vl)) => foldV(Vl,.inExp,findVRef,[]).
    findD(.prDef(_,_,_,_,A)) => foldA(A,findVRef,[]).
    findD(.glDef(_,_,_,Vl)) => foldV(Vl,.inExp,findVRef,[]).
    findD(_) default => [].
  } in findD(Df).

  public vMode ::= .inExp | .inPtn.

  public foldV:all a ~~ (cExp,vMode,(cExp,vMode,a)=>a,a) => a.
  foldV(Ex,Mode,Fn,SoF) => case Ex in {
    | .cVoid(_) => SoF
    | .cAnon(_,_) => SoF
    | .cUnrch(_,_) => SoF
    | .cVar(_,_) => Fn(Ex,Mode,SoF)
    | .cInt(_,Ix) => SoF
    | .cChar(_,Cx) => SoF
    | .cFlt(_,Dx) => SoF
    | .cBig(_,Bx) => SoF
    | .cString(_,Sx) => SoF
    | .cTerm(Lc,Lb,Args,Tp) => foldRight((Arg,SF)=>foldV(Arg,Mode,Fn,SF),Fn(.cVar(Lc,.cV(Lb,Tp)),Mode,SoF),Args)
    | .cNth(_,T,_,_) => foldV(T,Mode,Fn,SoF)
    | .cSetNth(_,T,_,R) => foldV(T,Mode,Fn,foldV(R,Mode,Fn,SoF))
    | .cClos(Lc,Nm,_,Fr,Tp) => foldV(Fr,Mode,Fn,Fn(.cVar(Lc,.cV(Nm,Tp)),Mode,SoF))
    | .cSv(_,_) => SoF
    | .cSvDrf(_,E,_) => foldV(E,Mode,Fn,SoF)
    | .cSvSet(_,E,V) => foldV(V,Mode,Fn,foldV(E,Mode,Fn,SoF))
    | .cCel(Lc,E,Tp) => foldV(E,Mode,Fn,SoF)
    | .cGet(Lc,E,Tp) => foldV(E,Mode,Fn,SoF)
    | .cCall(Lc,F,Args,Tp) => foldRight((Arg,SF)=>foldV(Arg,Mode,Fn,SF),Fn(.cVar(Lc,.cV(F,Tp)),Mode,SoF),Args)
    | .cOCall(_,Op,Args,_) => foldRight((Arg,SF)=>foldV(Arg,Mode,Fn,SF),foldV(Op,Mode,Fn,SoF),Args)
    | .cXCall(Lc,F,Args,Tp,_) => foldRight((Arg,SF)=>foldV(Arg,Mode,Fn,SF),Fn(.cVar(Lc,.cV(F,Tp)),Mode,SoF),Args)
    | .cXOCall(_,Op,Args,_,_) => foldRight((Arg,SF)=>foldV(Arg,Mode,Fn,SF),foldV(Op,Mode,Fn,SoF),Args)
    | .cThrw(_,X,_) => foldV(X,Mode,Fn,SoF)
    | .cSeq(_,L,R) => foldV(R,Mode,Fn,foldV(L,Mode,Fn,SoF))
    | .cCnj(_,L,R) => foldV(R,Mode,Fn,foldV(L,Mode,Fn,SoF))
    | .cDsj(_,L,R) => foldV(R,Mode,Fn,foldV(L,Mode,Fn,SoF))
    | .cNeg(_,R) => foldV(R,Mode,Fn,SoF)
    | .cCnd(_,T,L,R) => foldV(R,Mode,Fn,foldV(L,Mode,Fn,foldV(T,Mode,Fn,SoF)))
    | .cMatch(_,L,R) => foldV(R,.inExp,Fn,foldV(L,.inPtn,Fn,SoF))
    | .cLtt(_,_,B,X) => foldV(X,Mode,Fn,foldV(B,.inExp,Fn,SoF))
    | .cCase(_,G,Cs,Df,_) =>
      foldV(Df,.inExp,Fn,foldECases(Cs,Mode,Fn,foldV(G,.inExp,Fn,SoF)))
    | .cIxCase(_,G,Cs,Df,_) =>
      foldV(Df,.inExp,Fn,foldECases(Cs,Mode,Fn,foldV(G,.inExp,Fn,SoF)))
    | .cAbort(_,_,_) => SoF
    | .cTry(_,B,E,H,_) => foldV(H,.inExp,Fn,foldV(B,.inExp,Fn,foldV(E,.inExp,Fn,SoF)))
    | .cResum(_,L,R,_) => foldV(R,.inExp,Fn,foldV(L,.inPtn,Fn,SoF))
    | .cSusp(_,L,R,_) => foldV(R,.inExp,Fn,foldV(L,.inPtn,Fn,SoF))
    | .cRetyr(_,L,R,_) => foldV(R,.inExp,Fn,foldV(L,.inPtn,Fn,SoF))
    | .cVarNme(_,_,_,B) => foldV(B,Mode,Fn,SoF)
    | .cValof(_,A,_) => foldA(A,Fn,SoF)
  }

  foldECases:all a ~~ (cons[cCase[cExp]],vMode,(cExp,vMode,a)=>a,a)=>a.
  foldECases(Cs,Mode,Fn,SoF) =>
    foldRight(((_,Pt,E),SF)=>foldV(E,Mode,Fn,foldV(Pt,.inPtn,Fn,SF)),SoF,Cs).


  foldA:all a ~~ (aAction,(cExp,vMode,a)=>a,a) => a.
  foldA(Ac,Fn,SoF) => case Ac in {
    | .aNop(_) => SoF
    | .aSeq(_,L,R) => foldA(R,Fn,foldA(L,Fn,SoF))
    | .aLbld(_,L,I) => foldA(I,Fn,SoF)
    | .aBreak(_,L) => SoF
    | .aValis(_,V) => foldV(V,.inExp,Fn,SoF)
    | .aDo(_,V) => foldV(V,.inExp,Fn,SoF)
    | .aSetNth(_,V,Ix,E) => foldV(E,.inExp,Fn,foldV(V,.inExp,Fn,SoF))
    | .aDefn(_,P,V) => foldV(V,.inExp,Fn,foldV(P,.inPtn,Fn,SoF))
    | .aMatch(_,P,V) => foldV(V,.inExp,Fn,foldV(P,.inPtn,Fn,SoF))
    | .aAsgn(_,P,V) => foldV(V,.inExp,Fn,foldV(P,.inExp,Fn,SoF))
    | .aCase(_,G,C,D) => foldA(D,Fn,foldACases(C,Fn,foldV(G,.inExp,Fn,SoF)))
    | .aIxCase(_,G,C,D) => foldA(D,Fn,foldACases(C,Fn,foldV(G,.inExp,Fn,SoF)))
    | .aIftte(_,T,L,R) => foldA(R,Fn,foldA(L,Fn,foldV(T,.inExp,Fn,SoF)))
    | .aWhile(_,T,I) => foldA(I,Fn,foldV(T,.inExp,Fn,SoF))
    | .aTry(_,B,E,H) => foldA(B,Fn,foldA(H,Fn,SoF))
    | .aThrw(_,E) => foldV(E,.inExp,Fn,SoF)
    | .aVarNme(_,_,_,B) => foldA(B,Fn,SoF)
    | .aLtt(_,_,B,A) => foldA(A,Fn,foldV(B,.inExp,Fn,SoF))
    | .aAbort(_,_) => SoF
  }

  foldACases:all a ~~ (cons[cCase[aAction]],(cExp,vMode,a)=>a,a)=>a.
  foldACases(Cs,Fn,SoF) =>
    foldRight(((_,Pt,A),SF)=>foldA(A,Fn,foldV(Pt,.inPtn,Fn,SF)),SoF,Cs).

  public genVar:(string,tipe)=>cV.
  genVar(Pr,Tp) => .cV(genId(Pr),Tp).
}
