star.compiler.gencode{
  import star.
  import star.multi.
  import star.pkg.
  import star.sort.

  import star.compiler.assem.
  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.peephole.
  import star.compiler.ltipe.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.terms.

  srcLoc ::= lclVar(integer,ltipe) |
    argVar(integer,ltipe) |
    glbVar(string,ltipe) |
    glbFun(termLbl,ltipe).

  codeCtx ::= codeCtx(map[string,srcLoc],locn,integer,integer).

  emptyCtx:(locn,map[string,srcLoc])=>codeCtx.
  emptyCtx(Lc,Glbs) => codeCtx(Glbs,Lc,0,0).

  ctxLbls:(codeCtx,codeCtx)=>codeCtx.
  ctxLbls(codeCtx(Vrs,Lc,Mx1,Lb1),codeCtx(_,_,Mx2,Lb2))=>
    codeCtx(Vrs,Lc,max(Mx1,Mx2),max(Lb1,Lb2)).

  implementation display[codeCtx] => {.
    disp(codeCtx(Vrs,_,Depth,Stk)) => ssSeq([ss("depth "),disp(Depth),ss(" stack "),disp(Stk)]).
  .}

  implementation sizeable[codeCtx] => {.
    isEmpty(codeCtx(Vars,_,_,_))=>isEmpty(Vars).
    size(codeCtx(Vars,_,_,_))=>size(Vars).
  .}

  implementation display[srcLoc] => {.
    disp(lclVar(Off,Tpe)) =>ssSeq([ss("lcl "),disp(Off),ss(":"),disp(Tpe)]).
    disp(argVar(Off,Tpe)) =>ssSeq([ss("arg "),disp(Off),ss(":"),disp(Tpe)]).
    disp(glbVar(Off,Tpe)) =>ssSeq([ss("glb "),disp(Off),ss(":"),disp(Tpe)]).
    disp(glbFun(Off,Tpe)) =>ssSeq([ss("fun "),disp(Off),ss(":"),disp(Tpe)]).
  .}

  public compCrProg:(pkg,cons[crDefn],cons[(string,tipe)],compilerOptions,reports)=>
    either[reports,cons[codeSegment]].
  compCrProg(Pkg,Defs,Globals,Opts,Rp) => do{
    compDefs(Defs,localFuns(Defs,foldRight(((Pk,Tp),G)=>G[Pk->glbVar(Pk,Tp)],[],Globals)),
      Opts,genBoot(Pkg,Defs),Rp)
  }.

  localFuns:(cons[crDefn],map[string,srcLoc])=>map[string,srcLoc].
  localFuns(Defs,Vars) => foldRight(defFun,Vars,Defs).

  defFun(fnDef(Lc,Nm,Tp,_,_),Vrs) => Vrs[Nm->glbFun(tLbl(Nm,arity(Tp)),Tp::ltipe)].
  defFun(glbDef(Lc,Nm,Tp,_),Vrs) => Vrs[Nm->glbVar(Nm,Tp::ltipe)].
  defFun(rcDef(_,_,_,_),Vrs) => Vrs.
  
  compDefs:(cons[crDefn],map[string,srcLoc],compilerOptions,cons[codeSegment],reports)=>
    either[reports,cons[codeSegment]].
  compDefs([],_,_,Cs,_)=>either(Cs).
  compDefs([D,..Dfs],Glbs,Opts,Cs,Rp) => do{
    Code<-compDefn(D,Glbs,Opts,Rp);
    compDefs(Dfs,Glbs,Opts,[Code,..Cs],Rp)
  }

  compDefn:(crDefn,map[string,srcLoc],compilerOptions,reports) => either[reports,codeSegment].
  compDefn(fnDef(Lc,Nm,Tp,Args,Val),Glbs,Opts,Rp) => do{
    if Opts.showCode then
      logMsg("compile $(fnDef(Lc,Nm,Tp,Args,Val))");
    Ctx .= emptyCtx(Lc,Glbs);
    Ctxa .= argVars(Args,Ctx,0);
    (Ctxx,Code,Stk) <- compExp(Val,Opts,Ctxa,[],Rp);
    valis method(tLbl(Nm,size(Args)),Tp,peepOptimize((Code++[.iRet])::cons[assemOp]))
  }
  compDefn(glbDef(Lc,Nm,Tp,Val),Glbs,Opts,Rp) => do{
    if Opts.showCode then
      logMsg("compile global $(Nm)\:$(Tp) = $(Val))");
    Ctx .= emptyCtx(Lc,Glbs);
    (Ctxx,Code,Stk) <- compExp(Val,Opts,Ctx,[],Rp);
    valis global(tLbl(Nm,0),Tp,peepOptimize((Code++[iTG(Nm),.iRet])::cons[assemOp]))
  }

  compExp:(crExp,compilerOptions,codeCtx,cons[ltipe],reports) =>
    either[reports,(codeCtx,multi[assemOp],cons[ltipe])].
  compExp(Exp,_,Ctx,Stk,Rp) where (Const,Tp)^=isLiteral(Exp) =>
    either((Ctx,[iLdC(Const)],[Tp::ltipe,..Stk])).
  compExp(crVar(Lc,crId(Vr,Tp)),Opts,Ctx,Stk,Rp) => do{
    if Loc^=locateVar(Vr,Ctx) then {
      compVar(Lc,Vr,Loc,Opts,Ctx,Stk,Rp)
    } else
    raise reportError(Rp,"cannot locate variable $(Vr)\:$(Tp)",Lc)
  }
  compExp(crTerm(Lc,Nm,Args,Tp),Opts,Ctx,Stk,Rp) => do{
    (Ctx1,CdeA,Stka) <- compExps(Args,Opts,Ctx,[],Rp);
    valis (Ctx1,CdeA++[iAlloc(tLbl(Nm,size(Args)))],[Tp::ltipe,..Stk])
  }
  compExp(crECall(Lc,Op,Args,Tp),Opts,Ctx,Stk,Rp) where (_,Ins)^=intrinsic(Op) => do{
    (Ctx1,CdeA,Stka) <- compExps(Args,Opts,Ctx,[],Rp);
    valis (Ctx1,CdeA++[Ins],[Tp::ltipe,..Stk])
  }
  compExp(crECall(Lc,Nm,Args,Tp),Opts,Ctx,Stk,Rp) => do{
    (Ctx1,CdeA,Stka) <- compExps(Args,Opts,Ctx,Stk,Rp);
    valis (Ctx1,CdeA++[iEscape(Nm),iFrame(tplTipe(Stka))],[Tp::ltipe,..Stk])
  }
  compExp(crCall(Lc,Nm,Args,Tp),Opts,Ctx,Stk,Rp) => do{
    (Ctx1,CdeA,Stka) <- compExps(Args,Opts,Ctx,Stk,Rp);
    valis (Ctx1,CdeA++[iCall(tLbl(Nm,size(Args))),iFrame(tplTipe(Stka))],[Tp::ltipe,..Stk])
  }
  compExp(crOCall(Lc,Op,Args,Tp),Opts,Ctx,Stk,Rp) => do{
    (Ctx1,CdeA,Stka) <- compExps(Args,Opts,Ctx,Stk,Rp);
    valis (Ctx1,CdeA++[iOCall(size(Args)+1),iFrame(tplTipe(Stka))],[Tp::ltipe,..Stk])
  }
  compExp(crRecord(Lc,Nm,Fields,Tp),Opts,Ctx,Stk,Rp) => do{
    Sorted .= sort(Fields,((F1,_),(F2,_))=>F1<F2);
    Args .= (Sorted//((_,V))=>V);
    (Ctx1,CdeA,Stka) <- compExps(Args,Opts,Ctx,[],Rp);
    Lbl .= tRec(Nm,Sorted//((F,V))=>(F,typeOf(V)));
    valis (Ctx1,CdeA++[iAlloc(Lbl)],[Tp::ltipe,..Stk])
  }
  compExp(crDot(Lc,Rc,Fld,Tp),Opts,Ctx,Stk,Rp) => do{
    (Ctx1,CdeR,Stka) <- compExp(Rc,Opts,Ctx,Stk,Rp);
    valis (Ctx1,CdeR++[iGet(tLbl(Fld,0))],[Tp::ltipe,..Stk])
  }
  compExp(crTplOff(Lc,Rc,Ix,Tp),Opts,Ctx,Stk,Rp) => do{
    (Ctx1,CdeR,Stka) <- compExp(Rc,Opts,Ctx,Stk,Rp);
    valis (Ctx1,CdeR++[iNth(Ix)],[Tp::ltipe,..Stk])
  }
  compExp(crTplUpdate(Lc,Rc,Ix,E),Opts,Ctx,Stk,Rp) => do{
    (Ctx1,CdeR,Stka) <- compExp(Rc,Opts,Ctx,Stk,Rp);
    (Ctx2,CdeE,Stke) <- compExp(E,Opts,Ctx1,Stka,Rp);
    valis (Ctx2,CdeE++CdeR++[iStNth(Ix)],Stk)
  }
  compExp(crCnd(Lc,T,L,R),Opts,Ctx,Stk,Rp) => do{
    CtxC .= ptnVars(T,Ctx);
    logMsg("conditional exp $(crCnd(Lc,T,L,R)) @ $(Lc), Stk=$(Stk), LTp=$(typeOf(L)), RTp=$(typeOf(R))");
    StkTp .= tplTipe([typeOf(L)::ltipe,..Stk]);
    (Ctx1,Cde1,Stk1) <- compCond(T,Opts,1,Ctx,[],Rp);
    (Ctx2,Cde2,Stk2) <- compExp(L,Opts,Ctx,Stk1,Rp);
    (Ctx3,Cde3,Stk3) <- compExp(R,Opts,Ctx,Stk,Rp);
    Ctxx .= mergeCtx(Ctx3,Ctx2,Ctx);
    valis (Ctxx,[iBlock(.ptr,([iBlock(.ptr,(Cde1++Cde2++[iBreak(1)])::cons[assemOp])]++Cde3)::cons[assemOp])],
      [typeOf(L)::ltipe,..Stk])
  }
  compExp(crLtt(Lc,crId(V,VTp),Val,Exp),Opts,Ctx,Stk,Rp) => do{
    (Ctx1,CdeVl,Stka) <- compExp(Val,Opts,Ctx,Stk,Rp);
    (Off,Ctx2) .= defineLclVar(V,VTp::ltipe,Ctx1);
    (Ctxx,CdeE,Stke) <- compExp(Exp,Opts,Ctx2,Stk,Rp);
    valis (Ctxx,CdeVl++[iStL(Off)]++CdeE,Stke)
  }
  compExp(crLtRec(Lc,crId(V,VTp),Vl,Exp),Opts,Ctx,Stk,Rp) => do{
    (Off,Ctxb) .= defineLclVar(V,VTp::ltipe,Ctx);
    (Ctx1,Cde1,FCode,Stk1) <- compFrTerm(Vl,[crId(V,VTp)],crId(V,VTp),[],Opts,Ctxb,Stk,Rp);
    Cdel .= [iStV(Off)]++Cde1++[iStL(Off)]++FCode;
    (Ctxx,CdeVl,_) <- compExp(Exp,Opts,Ctx1,Stk,Rp);
    valis (Ctxx,Cdel++CdeVl,[typeOf(Exp)::ltipe,..Stk])
  }
  compExp(crCase(Lc,Exp,Cases,Deflt,Tp),Opts,Ctx,Stk,Rp) =>
    compCase(Lc,Exp,Cases,Deflt,Tp,Opts,Cont,Ctx,Cde,Stk,Rp).
  compExp(crAbort(Lc,Msg,Tp),Opts,Ctx,Stk,Rp) => do{
    (Ctxx,Acde,AStk) <- compExps([Lc::crExp,crStrg(Lc,Msg)],Opts,Ctx,Stk,Rp);
    valis (Ctxx,Acde++[iEscape("_abort"),iFrame(tplTipe(Stk))],Stk)
  }

  compExp(C,Opts,Ctx,Stk,Rp) where isCrCond(C) => do{
    (Ctx1,Cde,_) <- compCond(C,Opts,0,Ctx,[],Rp);
    valis (Ctx1,[iBlock(.ptr,[iBlock(.ptr,(Cde++[iLdC(term(tLbl("star.core#true",0),[])),iBreak(1)])::cons[assemOp]),
	    iLdC(term(tLbl("star.core#false",0),[])),iBreak(0)])],[.bool,..Stk])
  }

  -- compute elements to go in free tuple
  compFrTerm:(crExp,set[crVar],crVar,cons[either[integer,string]],compilerOptions,codeCtx,
    cons[ltipe],reports) =>
    either[reports,(codeCtx,multi[assemOp],multi[assemOp],cons[ltipe])].
  compFrTerm(crVar(Lc,Vr),Roots,Base,Pth,Opts,Ctx,Stk,Rp) => do{
    if Vr.<.Roots then {
      FCode .= fixupCode(Base,Pth,Vr,Ctx,[]);
      valis (Ctx,[.iLdV],FCode,[typeOf(Vr)::ltipe,..Stk])
    }
    else{
      (Ctxx,Cdex,Stkx) <- compExp(crVar(Lc,Vr),Opts,Ctx,Stk,Rp);
      valis (Ctxx,Cdex,[],Stkx)
    }
  }
  compFrTerm(crInt(Lc,Ix),_,_,_,_,Ctx,Stk,Rp) =>
    either((Ctx,[iLdC(intgr(Ix))],[],[.int64,..Stk])).
  compFrTerm(crFlot(Lc,Dx),_,_,_,_,Ctx,Stk,Rp) =>
    either((Ctx,[iLdC(flot(Dx))],[],[.flt64,..Stk])).
  compFrTerm(crStrg(Lc,Sx),_,_,_,_,Ctx,Stk,Rp) => 
    either((Ctx,[iLdC(strg(Sx))],[],[.ptr,..Stk])).
  compFrTerm(crLbl(Lc,Nm,Tp),_,_,_,_,Ctx,Stk,Rp) =>
    either((Ctx,[iLdC(symb(tLbl(Nm,0)))],[],[.ptr,..Stk])).
  compFrTerm(crVoid(Lc,Tp),_,_,_,_,Ctx,Stk,Rp) => 
    either((Ctx,[.iLdV],[],[.ptr,..Stk])).
  compFrTerm(crTerm(Lc,Nm,Args,Tp),Roots,Base,Pth,Opts,Ctx,Stk,Rp) => do{
    (Ctx1,CdeA,FxA,_) <- compFrArgs(Args,Roots,Base,0,Pth,Opts,Ctx,Stk,Rp);
    valis (Ctx1,CdeA++[iAlloc(tLbl(Nm,size(Args)))],FxA,[.ptr,..Stk])
  }
  compFrTerm(crRecord(Lc,Nm,Fields,Tp),Roots,Base,Pth,Opts,Ctx,Stk,Rp) => do{
    Sorted .= sort(Fields,((F1,_),(F2,_))=>F1<F2);
    Args .= (Sorted//((_,V))=>V);
    (Ctx1,CdeA,FxR,_) <- compFrArgs(Args,Roots,Base,0,Pth,Opts,Ctx,Stk,Rp);
    valis (Ctx1,CdeA++[iAlloc(tRec(Nm,Sorted//((F,V))=>(F,typeOf(V))))],FxR,[.ptr,..Stk])
  }
  /*
  compFrTerm(crTplOff(Lc,Tpl,Ix,Tp),Roots,Base,Pth,Opts,Ctx,Stk,Rp) => do{
    (Rc,OffPth) .= offPath(Tpl,[other(Ix)]);
    if crVar(RLc,V).=Rc && V .<. Roots then{
      FCode .= fixupCode(Base,Pth,V,Ctx,OffPth);
      valis (Ctx,[.iLdV],FCode,[Tp::ltipe,..Stk])
    }
    else{
      (Ctx1,Cde,FxCde,Stkx) <- compFrTerm(Rc,Roots,Base,Pth,Opts,
	bothCont(followPathCont(OffPth,Tp),Cont),Ctx,Cde,some(Stk),Rp)
    }
  }
  compFrTerm(crDot(Lc,Rc,Fld,Tp),Roots,Base,Pth,Opts,Ctx,Stk,Rp) => do{
    (Rc,OffPth) .= offPath(Rc,[either(Fld)]);
    if crVar(RLc,V).=Rc && V .<. Roots then{
      FCont .= fixupCode(Base,Pth,V,OffPth);
      (Ctx1,Cde1,Stk1) <- Cont.C(Ctx,Cde++[.iLdV],some([Tp,..Stk]),Rp);
      valis (Ctx1,Cde1,Stk1,FCont)
    }
    else{
      compFrTerm(Rc,Roots,Base,Pth,Opts,bothCont(followPathCont([either(Fld),..OffPth],Tp),Cont),Ctx,Cde,some(Stk),Rp)
    }
  }
  
  compFrTerm(crLtt(Lc,V,Val,Exp),Roots,Base,Path,Opts,Ctx,Stk,Rp) => do{
    (Nxt,Ctx0) .= defineLbl("F",Ctx);
    (Off,Ctxa) .= defineLclVar(V,Ctx0);
    (Ctx1,Cde1,Stk1,FC1) <- compFrTerm(Val,Roots\+V,V,[],Opts,bothCont(stoLcl(V,Off),jmpCont(Nxt)),Ctxa,[iStV(Off)],Stk,Rp);
    (Ctx2,Cde2,Stk2,FC2) <- compFrTerm(Exp,Roots,Base,Path,Opts,Cont,Ctx1,Cde1++[iLbl(Nxt)],Stk1,Rp);
    valis (Ctx2,Cde++Cde2,Stk2,bothCont(FC1,FC2))
  }
  compFrTerm(crLtRec(Lc,V,Val,Exp),Roots,Base,Path,Opts,Ctx,Cde,Rp) => do{
    (Nxt,Ctx0) .= defineLbl("G",Ctx);
    (Off,Ctxa) .= defineLclVar(V,Ctx0);
    (Ctx1,Cde1,Stk1,FC1) <- compFrTerm(Val,Roots\+V,V,[],Opts,bothCont(stoLcl(V,Off),jmpCont(Nxt)),Ctxa,[iStV(Off)],Stk,Rp);

    (Ctx2,Cde2,Stk2) <- FC1.C(Ctx1,Cde1++[iLbl(Nxt)],Stk1,Rp);
    (Ctx3,Cde3,Stk3,FC2) <- compFrTerm(Exp,Roots,Base,Path,Opts,Cont,Ctx2,Cde2,Stk2,Rp);
    valis (Ctx3,Cde++Cde3,Stk3,FC2)
  }
*/
  compFrTerm(Exp,_,_,_,Opts,Ctx,Stk,Rp) => 
    other(reportError(Rp,"cannot generate code for free term $(Exp)",locOf(Exp))).


  offPath:(crExp,cons[either[integer,string]])=>(crExp,cons[either[integer,string]]).
  offPath(crTplOff(_,Rc,Ix,_),Pth) => offPath(Rc,[other(Ix),..Pth]).
  offPath(crDot(_,Rc,Fld,_),Pth) => offPath(Rc,[either(Fld),..Pth]).
  offPath(Exp,Pth) default => (Exp,Pth).

  compFrArgs:(cons[crExp],set[crVar],crVar,integer,cons[either[integer,string]],compilerOptions,
    codeCtx,cons[ltipe],reports) =>
    either[reports,(codeCtx,multi[assemOp],multi[assemOp],cons[ltipe])].

  compFrArgs([],_,_,_,_,Opts,Ctx,Stk,Rp) => do{
    valis (Ctx,[],[],Stk)
  }
  compFrArgs([El,..Es],Roots,Base,Ix,Pth,Opts,Ctx,Stk,Rp)=> do{
    (Ctx1,Cde1,CF1,Stk1) <- compFrArgs(Es,Roots,Base,Ix+1,Pth,Opts,Ctx,Stk,Rp);
    (Ctx2,Cde2,CF2,Stk2) <- compFrTerm(El,Roots,Base,[other(Ix),..Pth],Opts,Ctx1,Stk1,Rp);
    valis (Ctx2,Cde2,CF1++CF2,Stk2)
  }.

  -- Expressions are evaluated in reverse order
  compExps:(cons[crExp],compilerOptions,codeCtx,cons[ltipe],reports) =>
    either[reports,(codeCtx,multi[assemOp],cons[ltipe])].
  compExps([],Opts,Ctx,Stk,Rp)=>either((Ctx,[],Stk)).
  compExps([El,..Es],Opts,Ctx,Stk,Rp)=> do{
    (Ctx1,Cde,Stk1) <- compExp(El,Opts,Ctx,Stk,Rp);
    (Ctxx,CdeS,Stkx) <- compExps(Es,Opts,Ctx1,Stk1,Rp);
    valis (Ctxx,Cde++CdeS,Stkx)
  }.

  compCase:(locn,crExp,cons[crCase],crExp,tipe,compilerOptions,codeCtx,cons[ltipe],reports) => either[reports,(codeCtx,multi[assemOp],cons[ltipe])].
  compCase(Lc,E,Cases,Deflt,Tp,Opts,Cont,Ctx,Cde,some(Stk),Rp) => do{
    (Nxt,Ctx1) .= defineLbl("CN",Ctx);
    (DLbl,Ctx2) .= defineLbl("CD",Ctx1);
    (Ctx3,ECode,EStk) <- compExp(E,Opts,jmpCont(Nxt),Ctx2,Cde,some(Stk),Rp);
    (Table,Max) .= genCaseTable(Cases);
    OC .= onceCont(Lc,Cont);
    (Ctx4,CCode,CStk) <- compCases(Table,0,Max,OC,jmpCont(DLbl),DLbl,Opts,Ctx3,ECode++[iLbl(Nxt),iCase(Max)],EStk,Rp);
    (Ctx5,DCode,DStk) <- compExp(Deflt,Opts,OC,Ctx4,CCode++[iLbl(DLbl),iRst(size(Stk))],some(Stk),Rp);
    Stkx <- mergeStack(Lc,CStk,DStk,Rp);
    logMsg("stack after case @ $(Lc) is $(Stkx)");
    valis (Ctx5,DCode,Stkx)
  }

  genCaseTable(Cases) where Mx.=nextPrime(size(Cases)) =>
    (sortCases(caseHashes(Cases,Mx)),Mx).

  caseHashes:(cons[crCase],integer)=>cons[(locn,crExp,integer,crExp)].
  caseHashes(Cases,Mx) => (Cases//((Lc,Pt,Ex))=>(Lc,Pt,caseHash(Pt)%Mx,Ex)).

  caseHash:(crExp)=>integer.
  caseHash(crVar(_,_)) => 0.
  caseHash(crInt(_,Ix)) => Ix.
  caseHash(crFlot(_,Dx)) => hash(Dx).
  caseHash(crStrg(_,Sx)) => hash(Sx).
  caseHash(crLbl(_,Nm,Tp)) => arity(Tp)*37+hash(Nm).
  caseHash(crTerm(_,Nm,Args,_)) => size(Args)*37+hash(Nm).

  sortCases(Cases) => mergeDuplicates(sort(Cases,((_,_,H1,_),(_,_,H2,_))=>H1<H2)).

  mergeDuplicates:(cons[(locn,crExp,integer,crExp)])=>cons[(integer,cons[(locn,crExp,crExp)])].
  mergeDuplicates([])=>[].
  mergeDuplicates([(Lc,Pt,Hx,Ex),..M]) where (D,Rs).=mergeDuplicate(M,Hx,[]) =>
    [(Hx,[(Lc,Pt,Ex),..D]),..mergeDuplicates(Rs)].

  mergeDuplicate([(Lc,Pt,Hx,Ex),..M],Hx,SoFar) =>
    mergeDuplicate(M,Hx,SoFar++[(Lc,Pt,Ex)]).
  mergeDuplicate(M,_,SoFar) default => (SoFar,M).

  compCases:(cons[(integer,cons[(locn,crExp,crExp)])],integer,integer,Cont,Cont,assemLbl,compilerOptions,
    codeCtx,multi[assemOp],option[cons[tipe]],reports) => either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  compCases([],Mx,Mx,_,_,_,_,Ctx,Cde,Stk,_) => either((Ctx,Cde,Stk)).
  compCases([],Ix,Mx,Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) where Ix<Mx =>
    compCases([],Ix+1,Mx,Succ,Fail,Deflt,Opts,Ctx,Cde++[iJmp(Deflt)],Stk,Rp).
  compCases([(Ix,Case),..Cases],Ix,Mx,Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) => do{
    (Lb,Ctx1) .= defineLbl("CC",Ctx);
    (Ctx2,Cde2,_) <- compCases(Cases,Ix+1,Mx,Succ,Fail,Deflt,Opts,Ctx1,Cde++[iJmp(Lb)],Stk,Rp);
    compCaseBranch(Case,Succ,Fail,Deflt,Opts,Ctx2,Cde2++[iLbl(Lb)],Stk,Rp)
  }
  compCases([(Iy,Case),..Cases],Ix,Mx,Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) =>
    compCases([(Iy,Case),..Cases],Ix+1,Mx,Succ,Fail,Deflt,Opts,Ctx,Cde++[iJmp(Deflt)],Stk,Rp).

  compCaseBranch([(Lc,Ptn,Exp)],Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) => do{
    (Nxt,Ctx1) .= defineLbl("CB",Ctx);
--    logMsg("case branch $(Ptn)->$(Exp), Stk=$(Stk)");
    (Ctx2,Cde2,Stk1)<-compPtn(Ptn,Opts,jmpCont(Nxt),Fail,Ctx1,Cde,Stk,Rp);
    compExp(Exp,Opts,Succ,Ctx2,Cde2++[iLbl(Nxt)],Stk1,Rp)
  }
  compCaseBranch([(Lc,Ptn,Exp),..More],Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) => do{
    (Fl,Ctx2) .= defineLbl("CF",Ctx);
    (VLb,Ctx3) .= defineLbl("CN",Ctx2);
    Vr .= crId(genSym("__"),typeOf(Ptn));
    (Off,Ctx4) .= defineLclVar(Vr,Ctx3);
    (Ctx5,Cde2,Stk1)<-compPtn(Ptn,Opts,expCont(Exp,Opts,Succ),jmpCont(Fl),Ctx4,Cde++[iTL(Off)],Stk,Rp);
    compMoreCase(More,Off,Succ,Fail,Opts,Ctx5,Cde2++[iLbl(Fl)],Stk,Rp)
  }

  compMoreCase:(cons[(locn,crExp,crExp)],integer,Cont,Cont,compilerOptions,codeCtx,multi[assemOp],option[cons[tipe]],reports) => either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  compMoreCase([],_,_,Fail,Opts,Ctx,Cde,Stk,Rp) => Fail.C(Ctx,Cde,Stk,Rp).
  compMoreCase([(Lc,Ptn,Exp),..More],Off,Succ,Fail,Opts,Ctx,Cde,Stk,Rp) => do{
    (Fl,Ctx1) .= defineLbl("CM",Ctx);
    (Ctx5,Cde2,Stk1)<-compPtn(Ptn,Opts,expCont(Exp,Opts,Succ),jmpCont(Fl),Ctx1,Cde++[iLdL(Off)],Stk,Rp);
    compMoreCase(More,Off,Succ,Fail,Opts,Ctx5,Cde2++[iLbl(Fl)],Stk,Rp)
  }
  
  compVar:(locn,string,srcLoc,compilerOptions,codeCtx,cons[ltipe],reports) =>
    either[reports,(codeCtx,multi[assemOp],cons[ltipe])].
  compVar(Lc,Nm,argVar(Off,Tp),Opts,Ctx,Stk,Rp) =>
    either((Ctx,[iLdA(Off)],[Tp,..Stk])).
  compVar(Lc,Nm,lclVar(Off,Tp),Opts,Ctx,Stk,Rp) =>
    either((Ctx,[iLdL(Off)],[Tp,..Stk])).
  compVar(Lc,_,glbVar(Nm,Tp),Opts,Ctx,Stk,Rp) =>
    either((Ctx,[iLdG(Nm)],[Tp,..Stk])).
  compVar(Lc,_,glbFun(Nm,Tp),Opts,Ctx,Stk,Rp) =>
    either((Ctx,[iLdC(symb(tLbl(Nm,arity(Tp))))],[Tp,..Stk])).
  compVar(Lc,Nm,Loc,Opts,_,_,Rp) =>
    other(reportError(Rp,"cannot compile variable $(Nm)",Lc)).

  compCond:(crExp,compilerOptions,integer,codeCtx,cons[ltipe],reports) =>
    either[reports,(codeCtx,multi[assemOp],cons[ltipe])].
  compCond(crCnj(Lc,L,R),Opts,Fail,Ctx,Stk,Rp) => do{
    (Ctx1,CdeL,_) <- compCond(L,Opts,Fail,Ctx,Stk,Rp);
    (Ctx2,CdeR,_) <- compCond(R,Opts,Fail,Ctx1,Stk,Rp);
    valis (Ctx2,CdeL++CdeR,Stk)
  }
  compCond(crDsj(Lc,L,R),Opts,Fail,Ctx,Stk,Rp) => do{
    (Ctx1,CdeL,_) <- compCond(L,Opts,0,Ctx,Stk,Rp);
    (Ctx2,CdeR,_) <- compCond(R,Opts,Fail,Ctx,Stk,Rp);
    Ctxx .= mergeCtx(Ctx1,Ctx2,Ctx);
    valis (Ctxx,[iBlock(tplTipe([]),(CdeL++[iBreak(1)])::cons[assemOp])]++CdeR,Stk)
  }
  compCond(crNeg(Lc,R),Opts,Fail,Ctx,Stk,Rp) where isCrCond(R) => do{
    (Ctxx,CdeR,_) <- compCond(R,Opts,0,Ctx,Stk,Rp);
    valis (Ctxx,[iBlock(tplTipe([]),(CdeR++[iBreak(Fail)])::cons[assemOp])]++[iBreak(0)],Stk)
  }.
  compCond(crNeg(Lc,Exp),Opts,Fail,Ctx,Stk,Rp) => do{
    (Ctx1,CdE,StkE) <- compExp(Exp,Opts,Ctx,Stk,Rp);
    valis (Ctx1,CdE++[iIf(Fail)],Stk)
  }
  compCond(crCnd(Lc,T,L,R),Opts,Fail,Ctx,Stk,Rp) => do{
    (Ctx1,CdeT,_) <- compCond(T,Opts,0,Ctx,Stk,Rp);
    (Ctx2,CdeL,_) <- compCond(L,Opts,Fail+2,Ctx1,Stk,Rp);
    (Ctx3,CdeR,_) <- compCond(R,Opts,Fail+1,Ctx,Stk,Rp);
    Ctxx .= mergeCtx(Ctx2,Ctx3,Ctx);
    valis (Ctxx,[iBlock(tplTipe([]),
	  [iBlock(tplTipe([]),(CdeT++CdeL++[iBreak(1)])::cons[assemOp])]++CdeR::cons[assemOp])],Stk)
  }
  compCond(crMatch(Lc,Ptn,Exp),Opts,Fail,Ctx,Stk,Rp) => do{
    (Ctx1,CdeE,Stk1) <- compExp(Exp,Opts,Ctx,Stk,Rp);
    (Ctx2,CdeP,_) <- compPttrn(Ptn,Opts,Fail,Ctx1,Stk1,Rp);
    valis (Ctx2,CdeE++CdeP,Stk)
  }
  compCond(Exp,Opts,Fail,Ctx,Stk,Rp) => do{
    (Ctx1,CdE,StkE) <- compExp(Exp,Opts,Ctx,Stk,Rp);
    valis (Ctx1,CdE++[iIfNot(Fail)],Stk)
  }

  compPttrn:(crExp,compilerOptions,integer,codeCtx,cons[ltipe],reports) =>
    either[reports,(codeCtx,multi[assemOp],cons[ltipe])].
  compPttrn(crVar(Lc,crId(Vr,Tp)),Opts,Fail,Ctx,[LTp,..Stk],Rp) => do{
    if Loc ^= locateVar(Vr,Ctx) then 
      compPttrnVar(Lc,Vr,Loc,Opts,Ctx,Stk,Rp)
    else{
      (Off,Ctx1) .= defineLclVar(Vr,LTp,Ctx);
      compPttrnVar(Lc,Vr,lclVar(Off,LTp),Opts,Ctx1,Stk,Rp)
    }
  }
  compPttrn(crVoid(Lc,Tp),Opts,Fail,Ctx,[_,..Stk],Rp) =>
    either((Ctx,[iCmpVd(Fail)],Stk)).
  compPttrn(L,Opts,Fail,Ctx,Stk,Rp) where (T,Tp)^=isLiteral(L) =>
    pttrnTest(locOf(L),Tp,Fail,Ctx,[iLdC(T)],[Tp::ltipe,..Stk],Rp).
  compPttrn(crTerm(Lc,Nm,Args,Tp),Opts,Fail,Ctx,[_,..Stk],Rp) => do{
    (Ctx1,ArgCde,Stk1) <- compPttrnArgs(Args,Opts,Fail,Ctx,(Args//(A)=>typeOf(A)::ltipe)++Stk,Rp);
    valis (Ctx1,[iUnpack(tLbl(Nm,size(Args)),Fail)]++ArgCde,Stk)
  }
  compPttrn(crWhere(Lc,Ptn,Cond),Opts,Fail,Ctx,Stk,Rp) => do{
    (Ctx1,Cde1,Stk1) <- compPttrn(Ptn,Opts,Fail,Ctx,Stk,Rp);
    (Ctx2,Cde2,_) <- compCond(Cond,Opts,Fail,Ctx1,Stk1,Rp);
    valis (Ctx2,Cde1++Cde2,Stk1)
  }
  compPttrn(Ptn,Opts,Fail,Ctx,_,Rp) =>
    other(reportError(Rp,"unreachable pattern $(Ptn)",locOf(Ptn))).  


  compPttrnVar:(locn,string,srcLoc,compilerOptions,codeCtx,cons[ltipe],reports) =>
    either[reports,(codeCtx,multi[assemOp],cons[ltipe])].
  compPttrnVar(Lc,Nm,lclVar(Off,Tp),Opts,Ctx,Stk,Rp) => either((Ctx,[iStL(Off)],Stk)).
  compPttrnVar(Lc,Nm,Loc,_,_,_,Rp) => other(reportError(Rp,"cannot target var at $(Loc) in pattern",Lc)).

  compPttrnArgs:(cons[crExp],compilerOptions,integer,codeCtx,cons[ltipe],reports) =>
    either[reports,(codeCtx,multi[assemOp],cons[ltipe])].
  compPttrnArgs([],_,_,Ctx,Stk,Rp) => either((Ctx,[],Stk)).
  compPttrnArgs([A,..As],Opts,Fail,Ctx,Stk,Rp) => do{
    (Ctx1,Cde1,Stk1) <- compPttrn(A,Opts,Fail,Ctx,Stk,Rp);
    (Ctx2,Cde2,Stk2) <- compPttrnArgs(As,Opts,Fail,Ctx1,Stk1,Rp);
    valis (Ctx2,Cde1++Cde2,Stk2)
  }
  
  pttrnTest:(locn,tipe,integer,codeCtx,multi[assemOp],cons[ltipe],reports) =>
    either[reports,(codeCtx,multi[assemOp],cons[ltipe])].
  pttrnTest(_,intType,Fail,Ctx,Cde,[_,_,..Stk],Rp) =>
    either((Ctx,Cde++[iICmp(Fail)],Stk)).
  pttrnTest(_,Tp,Fail,Ctx,Cde,[_,_,..Stk],Rp) =>
    either((Ctx,Cde++[iComp(Fail)],Stk)).

  isLiteral:(crExp)=>option[(term,tipe)].
  isLiteral(crInt(_,Ix))=>some((intgr(Ix),intType)).
  isLiteral(crFlot(_,Dx))=>some((flot(Dx),fltType)).
  isLiteral(crStrg(_,Sx))=>some((strg(Sx),strType)).
  isLiteral(crLbl(_,Nm,Tp))=>some((symb(tLbl(Nm,0)),Tp)).
  isLiteral(crTerm(_,Nm,[],Tp)) => some((term(tLbl(Nm,0),[]),Tp)).
  isLiteral(crVoid(Lc,Tp)) => some((symb(tLbl("star.core#void",0)),Tp)).
  isLiteral(_) default => .none.

  -- continuations

  expCont:(crExp,compilerOptions,Cont)=>Cont.
  expCont(Exp,Opts,Cont) =>
    ccont(.false,.false,(Ctx,Cde,Stk,Rp) => do{
	compExp(Exp,Opts,Cont,Ctx,Cde,Stk,Rp)
      }).

  fixupCode:(crVar,cons[either[integer,string]],crVar,codeCtx,cons[either[integer,string]]) => multi[assemOp].
  fixupCode(Base,[Off,..Pth],Src,Ctx,SrcPth) => valof action {
    BNm.=crName(Base);
    lclVar(BLoc,_)^=locateVar(BNm,Ctx);
    lclVar(SLoc,_)^=locateVar(crName(Src),Ctx);
    valis [iLdL(BLoc),..followPath(Pth)]++[iLdL(SLoc),..followPath(SrcPth)]++storeField(Off)
  }.

  followPathCont:(cons[either[integer,string]],tipe) => Cont.
  followPathCont(Path,Tp) => ccont(.false,.false,
    (Ctx,Cde,some([_,..Stk]),Rp) => do{
      valis (Ctx,Cde++followPath(Path),some([Tp,..Stk]))
    }).

  storeField:(either[integer,string])=>multi[assemOp].
  storeField(other(Ix)) => [iStNth(Ix)].
  storeField(either(Fld)) => [iSet(tLbl(Fld,0))].

  -- path is first in first out
  followPath:(cons[either[integer,string]])=>multi[assemOp].
  followPath([])=>[].
  followPath([other(Off),..Pth]) => [iNth(Off)]++followPath(Pth).
  followPath([either(Fld),..Pth]) => [iGet(tLbl(Fld,0))]++followPath(Pth).

  stoLcl:(crVar,integer)=>Cont.
  stoLcl(Vr,Off)=>
    ccont(.true,.false,(Ctx,Cde,some([_,..Stk]),Rp) => do{
	valis (Ctx,Cde++[iStL(Off)],some(Stk))
    }).
  
  bothCont:(Cont,Cont)=>Cont.
  bothCont(F,G) => ccont(F.Simple&&G.Simple,G.IsJump,(Ctx,Cde,Stk,Rp)=> do{
      (Ct1,Cd1,Stk1) <- F.C(Ctx,Cde,Stk,Rp);
      G.C(Ct1,Cd1,Stk1,Rp)
    }).

  onceCont:(locn,Cont)=>Cont.
  onceCont(_,C) where C.Simple => C.
  onceCont(Lc,C)=> let{.
    d := .none.
  .} in ccont(.false,.true,(Ctx,Cde,Stk,Rp) => do{
      if (Lbl,EStk,LStk)^=d! then{
	XStk <- mergeStack(Lc,EStk,Stk,Rp);
	valis (Ctx,Cde++[iJmp(Lbl)],LStk)
      }
      else{
	(Lbl,Cx) .= defineLbl("O",Ctx);
	(Ctx1,Cde1,Stk1) <- C.C(Cx,Cde++[iLbl(Lbl)],Stk,Rp);
	d := some((Lbl,Stk,Stk1));
	valis (Ctx1,Cde1,Stk1)
      }
  }).


  mergeStack:(locn,option[cons[tipe]],option[cons[tipe]],reports)=>either[reports,option[cons[tipe]]].
  mergeStack(Lc,.none,S,_) => either(S).
  mergeStack(Lc,S,.none,_) => either(S).
  mergeStack(Lc,S1,S2,_) where S1==S2 => either(S1).
  mergeStack(Lc,S1,S2,Rp) => other(reportError(Rp,"inconsistent stacks: $(S1) vs $(S2)",Lc)).
  
  locateVar:(string,codeCtx)=>option[srcLoc].
  locateVar(Nm,codeCtx(Vars,_,_,_)) => Vars[Nm].

  defineLclVar:(string,ltipe,codeCtx) => (integer,codeCtx).
  defineLclVar(Nm,Tp,codeCtx(Vrs,Lc,Count,Lbl)) where NxtCnt .= Count+1 =>
    (NxtCnt,codeCtx(Vrs[Nm->lclVar(NxtCnt,Tp)],Lc,NxtCnt,Lbl)).

  defineLbl:(string,codeCtx)=>(assemLbl,codeCtx).
  defineLbl(Pr,codeCtx(Vrs,Lc,Count,Lb))=>(al(Pr++"$(Lb)"),codeCtx(Vrs,Lc,Count,Lb+1)).

  changeLoc:(locn,compilerOptions,codeCtx)=>(multi[assemOp],codeCtx).
  changeLoc(Lc,_,codeCtx(Vars,Lc0,Dp,Lb)) where Lc~=Lc0 =>
    ([iLine(Lc::term)],codeCtx(Vars,Lc,Dp,Lb)).
    changeLoc(_,_,Ctx)=>([],Ctx).

  implementation hasLoc[codeCtx] => {
    locOf(codeCtx(_,Lc,_,_))=>Lc.
  }

  ptnVars:(crExp,codeCtx) => codeCtx.
  ptnVars(crVar(_,crId(Nm,Tp)),codeCtx(Vars,CLc,Count,Lb)) where _ ^= Vars[Nm] => codeCtx(Vars,CLc,Count,Lb).
  ptnVars(crVar(_,crId(Nm,Tp)),codeCtx(Vars,CLc,Count,Lb)) => codeCtx(Vars[Nm->lclVar(Count+1,Tp::ltipe)],CLc,Count+1,Lb).
  ptnVars(crInt(_,_),Ctx) => Ctx.
  ptnVars(crFlot(_,_),Ctx) => Ctx.
  ptnVars(crStrg(_,_),Ctx) => Ctx.
  ptnVars(crVoid(_,_),Ctx) => Ctx.
  ptnVars(crLbl(_,_,_),Ctx) => Ctx.
  ptnVars(crTerm(_,Op,Els,_),Ctx) => foldRight(ptnVars,Ctx,Els).
  ptnVars(crCall(_,_,_,_),Ctx) => Ctx.
  ptnVars(crECall(_,_,_,_),Ctx) => Ctx.
  ptnVars(crOCall(_,_,_,_),Ctx) => Ctx.
  ptnVars(crRecord(_,_,Els,_),Ctx) => foldRight(((_,El),X)=>ptnVars(El,X),Ctx,Els).
  ptnVars(crDot(_,_,_,_),Ctx) => Ctx.
  ptnVars(crTplOff(_,_,_,_),Ctx) => Ctx.
  ptnVars(crTplUpdate(_,_,_,_),Ctx) => Ctx.
  ptnVars(crCnj(_,L,R),Ctx) => ptnVars(L,ptnVars(R,Ctx)).
  ptnVars(crDsj(_,L,R),Ctx) => mergeCtx(ptnVars(L,Ctx),ptnVars(R,Ctx),Ctx).
  ptnVars(crNeg(_,R),Ctx) => Ctx.
  ptnVars(crCnd(Lc,T,L,R),Ctx) => mergeCtx(ptnVars(crCnj(Lc,T,L),Ctx),ptnVars(R,Ctx),Ctx).
  ptnVars(crLtt(_,_,_,_),Ctx) => Ctx.
  ptnVars(crCase(_,_,_,_,_),Ctx) => Ctx.
  ptnVars(crAbort(_,_,_),Ctx) => Ctx.
  ptnVars(crWhere(_,P,C),Ctx) => ptnVars(C,ptnVars(P,Ctx)).
  ptnVars(crMatch(_,P,_),Ctx) => ptnVars(P,Ctx).

  argVars:(cons[crVar],codeCtx,integer) => codeCtx.
  argVars([],Ctx,_)=>Ctx.
  argVars([crId(Nm,Tp),..As],codeCtx(Vars,CLc,Count,Lb),Ix) =>
    argVars(As,codeCtx(Vars[Nm->argVar(Ix,Tp::ltipe)],CLc,Count+1,Lb),Ix+1).
  argVars([_,..As],Ctx,Arg) => argVars(As,Ctx,Arg+1).

  mergeCtx:(codeCtx,codeCtx,codeCtx)=>codeCtx.
  mergeCtx(codeCtx(LV,_,_,Lb1),codeCtx(RV,_,_,Lb2),Base) => let{
    mergeVar:(string,srcLoc,codeCtx) => codeCtx.
    mergeVar(Nm,_,Vrs) where _ ^= locateVar(Nm,Base) => Vrs.
    mergeVar(Nm,_,codeCtx(Vs,Lc,Count,_)) where lclVar(_,Tp) ^= RV[Nm] =>
      codeCtx(Vs[Nm->lclVar(Count+1,Tp)],Lc,Count+1,max(Lb1,Lb2)).
    mergeVar(Nm,lclVar(_,Tp),codeCtx(Vs,Lc,Count,_)) =>
      codeCtx(Vs[Nm->lclVar(Count+1,Tp)],Lc,Count+1,max(Lb1,Lb2)).
  } in ixRight(mergeVar,Base,LV).

  drop:all x,e ~~ stream[x->>e] |: (x,integer)=>x.
  drop(S,0)=>S.
  drop([_,..S],N)=>drop(S,N-1).

  trimStack:all x, e ~~ stream[x->>e],sizeable[x] |: (option[x],integer)=>option[x].
  trimStack(some(L),N) =>some(drop(L,size(L)-N)).
  trimStack(.none,_) => .none.

  genBoot:(pkg,cons[crDefn])=>cons[codeSegment].
  genBoot(P,Defs) where Mn .= qualifiedName(pkgName(P),.valMark,"_main") && fnDef(_,Mn,_,_,_) in Defs => 
    [method(tLbl(qualifiedName(pkgName(P),.pkgMark,"_boot"),0),funType([],unitTp),[
          iEscape("_command_line"),
	  iLdG(packageVar(P)),
	  iGet(tLbl("_main",0)),
	  iOCall(2),
	  iFrame(tplTipe([])),
	  .iHalt])].

  genBoot(_,_) default => [].

  frameSig:(cons[tipe])=>term.
  frameSig(Tps) => strg((tupleType(Tps)::ltipe)::string).

  enum:(string)=>term.
  enum(Nm)=>term(tLbl(Nm,0),[]).
}
