star.compiler.rewrite{
  import star.
  
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.term.
  import star.compiler.types.

  public uniqify:(cDefn) => cDefn.
  uniqify(.fnDef(Lc,Nm,Tp,Args,Val)) => valof{
    D = foldRight((Vr,M)=>M[vName(Vr)->.cVar(Lc,Vr)],[],Args);
    valis .fnDef(Lc,Nm,Tp,Args,rwTerm(Val,D,extendU))
  }
  uniqify(.prDef(Lc,Nm,Tp,Args,Act)) => valof{
    D = foldRight((Vr,M)=>M[vName(Vr)->.cVar(Lc,Vr)],[],Args);
    valis .prDef(Lc,Nm,Tp,Args,rwAct(Act,D,extendU))
  }
  uniqify(.glDef(Lc,Nm,Tp,Val)) => 
    .glDef(Lc,Nm,Tp,rwTerm(Val,[],extendU)).
  uniqify(Df) default => Df.

  extendU:(extendTgt,rwMap)=>rwMap.
  extendU(.inLet(V),Map) => Map[vName(V)->newVar(V)].
  extendU(.inPttrn(P),Map) =>
    foldRight((V,M)=>M[vName(V)->newVar(V)],Map,ptnVrs(P,[])).
  extendU(.inGoal(G),Map) =>
    foldRight((V,M)=>M[vName(V)->newVar(V)],Map,glVars(G,[])).
  extendU(.inSeq(A),Map) =>
    foldRight((V,M)=>M[vName(V)->newVar(V)],Map,extractVars(A)).

  extractVars(.aDefn(_,P,_)) => ptnVrs(P,[]).
  extractVars(.aMatch(_,P,_)) => ptnVrs(P,[]).
  extractVars(_) default => [].

/*  public freshenE:(cExp,rwMap)=>cExp.
  freshenE(Term,Mp) =>
    rwTerm(Term,Mp,extendE).

  extendE(.inLet(V),Map) => Map[vName(V)->newVar(V)].
  extendE(.inPttrn(P),Map) =>
    foldRight((V,M)=>M[vName(V)->newVar(V)],Map,ptnVrs(P,[])).
  extendE(.inGoal(G),Map) =>
    foldRight((V,M)=>M[vName(V)->newVar(V)],Map,glVars(G,[])).
  extendE(.inSeq(A),Map) =>
  foldRight((V,M)=>M[vName(V)->newVar(V)],Map,extractVars(A)).
  */

  rwTerm:(cExp,rwMap,extMap)=>cExp.
  rwTerm(Trm,Mp,ExtFn) => case Trm in {
    | .cVoid(_) => Trm
    | .cAnon(_,_) => Trm
    | .cUnrch(_,_) => Trm
    | .cVar(Lc,.cV(VNm,Tp)) => (E ?= Mp[VNm] ?? E || Trm)
    | .cInt(Lc,Ix) => Trm
    | .cBig(Lc,Ix) => Trm
    | .cFlt(Lc,Dx) => Trm
    | .cChar(Lc,Cx) => Trm
    | .cString(Lc,Sx) => Trm
    | .cTerm(Lc,Op,Args,Tp) => .cTerm(Lc,Op,rwTerms(Args,Mp,ExtFn),Tp)
    | .cNth(Lc,R,Ix,Tp) =>.cNth(Lc,rwTerm(R,Mp,ExtFn),Ix,Tp)
    | .cSetNth(Lc,R,Ix,E) =>.cSetNth(Lc,rwTerm(R,Mp,ExtFn),Ix,rwTerm(E,Mp,ExtFn))
    | .cClos(Lc,L,A,F,Tp) => .cClos(Lc,L,A,rwTerm(F,Mp,ExtFn),Tp)
    | .cSv(_,_) => Trm
    | .cSvDrf(Lc,E,Tp) => .cSvDrf(Lc,rwTerm(E,Mp,ExtFn),Tp)
    | .cSvSet(Lc,E,V) => .cSvSet(Lc,rwTerm(E,Mp,ExtFn),rwTerm(V,Mp,ExtFn))
    | .cCel(Lc,E,Tp) => .cCel(Lc,rwTerm(E,Mp,ExtFn),Tp)
    | .cGet(Lc,E,Tp) => .cGet(Lc,rwTerm(E,Mp,ExtFn),Tp)
    | .cCall(Lc,Op,Args,Tp) => .cCall(Lc,Op,Args//(A)=>rwTerm(A,Mp,ExtFn),Tp)
    | .cOCall(Lc,Op,Args,Tp) => .cOCall(Lc,rwTerm(Op,Mp,ExtFn),Args//(A)=>rwTerm(A,Mp,ExtFn),Tp)
    | .cXCall(Lc,Op,Args,Tp,ErTp) => .cXCall(Lc,Op,Args//(A)=>rwTerm(A,Mp,ExtFn),Tp,ErTp)
    | .cXOCall(Lc,Op,Args,Tp,ErTp) => .cXOCall(Lc,rwTerm(Op,Mp,ExtFn),Args//(A)=>rwTerm(A,Mp,ExtFn),Tp,ErTp)
    | .cThrw(Lc,E,Tp) =>.cThrw(Lc,rwTerm(E,Mp,ExtFn),Tp)
    | .cSeq(Lc,L,R) =>.cSeq(Lc,rwTerm(L,Mp,ExtFn),rwTerm(R,Mp,ExtFn))
    | .cMatch(_,_,_) => rwGoal(Trm,Mp,ExtFn)
    | .cCnj(_,_,_) => rwGoal(Trm,Mp,ExtFn)
    | .cDsj(_,_,_) => rwGoal(Trm,Mp,ExtFn)
    | .cNeg(_,_) =>rwGoal(Trm,Mp,ExtFn)
    | .cCnd(Lc,G,L,R) => valof{
      if isCond(L) || isCond(R) then
	valis rwGoal(Trm,Mp,ExtFn)
      else{
	NM = ExtFn(.inGoal(G),Mp);

	valis .cCnd(Lc,rwGl(G,NM,ExtFn),rwTerm(L,NM,ExtFn),rwTerm(R,Mp,ExtFn))
      }
    }
    | .cLtt(Lc,LV,D,E) => valof{
      NMp = ExtFn(.inLet(LV),Mp);
      if .cVar(_,NV) .= rwTerm(.cVar(Lc,LV),NMp,ExtFn) then
	valis .cLtt(Lc,NV,rwTerm(D,NMp,ExtFn),rwTerm(E,NMp,ExtFn))
      else{
	reportError("Cannot rewrite $(Trm)",Lc);
	valis Trm
      }
    }
    | .cCase(Lc,Sel,Cases,Dflt,Tp) => .cCase(Lc,rwTerm(Sel,Mp,ExtFn),
      Cases//(C)=>rwCase(C,Mp,ExtFn,rwTerm),rwTerm(Dflt,Mp,ExtFn),Tp)
    | .cIxCase(Lc,Sel,Cases,Dflt,Tp) => .cIxCase(Lc,rwTerm(Sel,Mp,ExtFn),
      Cases//(C)=>rwCase(C,Mp,ExtFn,rwTerm),rwTerm(Dflt,Mp,ExtFn),Tp)
    | .cTry(Lc,B,E,H,Tp) =>  valof{
      NMp = ExtFn(.inPttrn(E),Mp);
      valis .cTry(Lc,rwTerm(B,NMp,ExtFn),rwTerm(E,NMp,ExtFn),rwTerm(H,NMp,ExtFn),Tp)
    }
    | .cResum(Lc,T,M,Tp) => .cResum(Lc,rwTerm(T,Mp,ExtFn),rwTerm(M,Mp,ExtFn),Tp)
    | .cSusp(Lc,T,M,Tp) => .cSusp(Lc,rwTerm(T,Mp,ExtFn),rwTerm(M,Mp,ExtFn),Tp)
    | .cRetyr(Lc,T,M,Tp) => .cRetyr(Lc,rwTerm(T,Mp,ExtFn),rwTerm(M,Mp,ExtFn),Tp)
    | .cVarNme(Lc,N,V,E) => .cVarNme(Lc,N,rwTerm(V,Mp,ExtFn),rwTerm(E,Mp,ExtFn))
    | .cValof(Lc,A,Tp) => .cValof(Lc,rwAct(A,Mp,ExtFn),Tp)
    | .cAbort(Lc,Ms,Tp) => .cAbort(Lc,Ms,Tp)
  }.

  rwTerms:(cons[cExp],rwMap,extMap)=>cons[cExp].
  rwTerms(Ts,Mp,Ex) => foldRight((A,Ti)=>[rwTerm(A,Mp,Ex),..Ti],[],Ts).

  rwCase:all e ~~ (cCase[e],rwMap,extMap,(e,rwMap,extMap)=>e) => cCase[e].
  rwCase((Lc,Ptn,Rep),M,ExtFn,F) => valof{
    NMp = ExtFn(.inPttrn(Ptn),M);
    valis (Lc,rwTerm(Ptn,NMp,ExtFn),F(Rep,NMp,ExtFn))
  }

  rwPtn:(cExp,rwMap,extMap)=>cExp.
  rwPtn(Ptn,Mp,ExtFn) => case Ptn in {
    | .cVoid(_) => Ptn
    | .cAnon(_,_) => Ptn
    | .cUnrch(_,_) => Ptn
    | .cVar(Lc,.cV(VNm,Tp)) => (E ?= Mp[VNm] ?? E || Ptn)
    | .cInt(Lc,Ix) => Ptn
    | .cBig(Lc,Ix) => Ptn
    | .cFlt(Lc,Dx) => Ptn
    | .cChar(Lc,Cx) => Ptn
    | .cString(Lc,Sx) => Ptn
    | .cTerm(Lc,Op,Args,Tp) => .cTerm(Lc,Op,rwPtns(Args,Mp,ExtFn),Tp)
    | .cSetNth(Lc,R,Ix,E) =>.cSetNth(Lc,rwPtn(R,Mp,ExtFn),Ix,rwPtn(E,Mp,ExtFn))
    | .cSvDrf(Lc,E,Tp) => .cSvDrf(Lc,rwPtn(E,Mp,ExtFn),Tp)
  }

  rwPtns:(cons[cExp],rwMap,extMap)=>cons[cExp].
  rwPtns(Ts,Mp,Ex) => foldRight((A,Ti)=>[rwPtn(A,Mp,Ex),..Ti],[],Ts).

  rwGoal:(cExp,rwMap,extMap)=>cExp.
  rwGoal(Gl,Mp,ExtFn) => valof{
    NMp = ExtFn(.inGoal(Gl),Mp);
    valis rwGl(Gl,NMp,ExtFn)
  }

  rwGl:(cExp,rwMap,extMap)=>cExp.
  rwGl(Gl,Mp,ExtFn) => case Gl in {
    | .cSeq(Lc,L,R) =>.cSeq(Lc,rwGl(L,Mp,ExtFn),rwGl(R,Mp,ExtFn))
    | .cCnj(Lc,L,R) =>.cCnj(Lc,rwGl(L,Mp,ExtFn),rwGl(R,Mp,ExtFn))
    | .cDsj(Lc,L,R) => valof{
      DVrs = glVars(Gl,[]);
      if isEmpty(DVrs) then
	valis .cDsj(Lc,rwGl(L,Mp,ExtFn),rwGl(R,Mp,ExtFn))
      else
      valis rwDisjunction(Lc,L,R,Mp,ExtFn,DVrs)
    }
    | .cNeg(Lc,R) =>.cNeg(Lc,rwGl(R,Mp,ExtFn))
    | .cCnd(Lc,G,L,R) => valof{
      NMp = ExtFn(.inGoal(G),Mp);
      valis .cCnd(Lc,rwGl(G,NMp,ExtFn),rwGl(L,NMp,ExtFn),rwGl(R,Mp,ExtFn))
    }
    | .cMatch(Lc,P,E) => .cMatch(Lc,rwPtn(P,Mp,ExtFn),rwGl(E,Mp,ExtFn))
    | _ => rwTerm(Gl,Mp,ExtFn)
  }

  rwDisjunction:(option[locn],cExp,cExp,rwMap,extMap,set[cV])=>cExp.
  rwDisjunction(Lc,L,R,Mp,ExtFn,Dvrs) => valof{
    VTpl = mcTpl(Lc,foldLeft((Vr,Ls)=>[.cVar(Lc,Vr),..Ls],[],Dvrs));
    (L0,LPtn) = pullOutDis(L,VTpl,ExtFn);
    (R0,RPtn) = pullOutDis(R,VTpl,ExtFn);
    Lx = rwGl(L0,Mp,ExtFn);
    Rx = rwGl(R0,Mp,ExtFn);
    MTpl = rwTerm(VTpl,Mp,ExtFn);
    valis .cMatch(Lc,mcSome(Lc,MTpl),
      .cCnd(Lc,Lx,mcSome(Lc,LPtn),
	.cCnd(Lc,Rx,mcSome(Lc,RPtn),mcNone(Lc,typeOf(MTpl)))))
  }

  pullOutDis(G,Tpl,ExtFn) => valof{
    GVrs = glVars(G,[]);
    GD = foldRight((Vr,M)=>M[vName(Vr)->newVar(Vr)],[],GVrs);
    Ptn = rwPtn(Tpl,GD,ExtFn);
    Gl = rwGl(G,GD,ExtFn);
    valis (Gl,Ptn)
  }

  rwAct:(aAction,rwMap,extMap)=>aAction.
  rwAct(Ac,Mp,ExtFn) => case Ac in {
    | .aNop(Lc) => .aNop(Lc)
    | .aSeq(Lc,L,R) => valof{
	NMp = ExtFn(.inSeq(L),Mp);
	valis .aSeq(Lc,rwAct(L,NMp,ExtFn),rwAct(R,NMp,ExtFn))
      }
    | .aLbld(Lc,L,A) => .aLbld(Lc,L,rwAct(A,Mp,ExtFn))
    | .aBreak(Lc,L) => .aBreak(Lc,L)
    | .aValis(Lc,E) => .aValis(Lc,rwTerm(E,Mp,ExtFn))
    | .aDo(Lc,E) => .aDo(Lc,rwTerm(E,Mp,ExtFn))
    | .aSetNth(Lc,V,Ix,E) => .aSetNth(Lc,rwTerm(V,Mp,ExtFn),Ix,rwTerm(E,Mp,ExtFn))
    | .aDefn(Lc,V,E) => .aDefn(Lc,rwTerm(V,Mp,ExtFn),rwTerm(E,Mp,ExtFn))
    | .aMatch(Lc,V,E) => .aMatch(Lc,rwTerm(V,Mp,ExtFn),rwTerm(E,Mp,ExtFn))
    | .aAsgn(Lc,V,E) => .aAsgn(Lc,rwTerm(V,Mp,ExtFn),rwTerm(E,Mp,ExtFn))
    | .aCase(Lc,G,Cs,D) => .aCase(Lc,rwTerm(G,Mp,ExtFn),Cs//(C)=>rwCase(C,Mp,ExtFn,rwAct),rwAct(D,Mp,ExtFn))
    | .aIxCase(Lc,G,Cs,D) => .aIxCase(Lc,rwTerm(G,Mp,ExtFn),Cs//(C)=>rwCase(C,Mp,ExtFn,rwAct),rwAct(D,Mp,ExtFn))
    | .aIftte(Lc,C,L,R) => valof{
      NMp = ExtFn(.inGoal(C),Mp);
      valis .aIftte(Lc,rwGl(C,NMp,ExtFn),rwAct(L,NMp,ExtFn),rwAct(R,Mp,ExtFn))
    }
    | .aWhile(Lc,C,B) => valof{
      NMp = ExtFn(.inGoal(C),Mp);
      valis .aWhile(Lc,rwGl(C,NMp,ExtFn),rwAct(B,NMp,ExtFn))
    }
    | .aTry(Lc,B,E,Hs) => .aTry(Lc,rwAct(B,Mp,ExtFn),rwTerm(E,Mp,ExtFn),rwAct(Hs,Mp,ExtFn))
    | .aThrw(Lc,E) => .aThrw(Lc,rwTerm(E,Mp,ExtFn))
    | .aLtt(Lc,V,D,A) => valof{
      NMp = ExtFn(.inLet(V),Mp);
      if .cVar(_,NV) .= rwTerm(.cVar(Lc,V),NMp,ExtFn) then
	valis .aLtt(Lc,NV,rwTerm(D,NMp,ExtFn),rwAct(A,NMp,ExtFn))
      else{
	reportError("Cannot rewrite $(Ac)",Lc);
	valis Ac
      }
    }
    | .aVarNme(Lc,N,V,E) => .aVarNme(Lc,N,rwTerm(V,Mp,ExtFn),rwAct(E,Mp,ExtFn))
    | .aAbort(Lc,Ms) => .aAbort(Lc,Ms)
  }

  dropVar:(string,map[string,cExp]) => map[string,cExp].
  dropVar(Nm,Map) => Map[~Nm].

  extendTgt ::= .inLet(cV) | .inGoal(cExp) | .inPttrn(cExp) | .inSeq(aAction).
  rwMap ~> map[string,cExp].
  extMap ~> (extendTgt,rwMap)=>rwMap.
}
