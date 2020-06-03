star.compiler.gencode{
  import star.
  import star.pkg.
  import star.sort.

  import star.compiler.assem.
  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.multi.
  import star.compiler.peephole.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.terms.

  srcLoc ::= lclVar(integer,tipe) |
    argVar(integer,tipe) |
    glbVar(string,tipe) |
    glbFun(string,tipe) |
    onStack(tipe).

  Cont ::= cont{
    C:(codeCtx,multi[assemOp],option[cons[tipe]],reports)=>either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
    L:option[assemLbl].
    Simple:boolean
  }.

  codeCtx ::= codeCtx(map[string,srcLoc],locn,integer,integer).

  emptyCtx:(locn,map[string,srcLoc])=>codeCtx.
  emptyCtx(Lc,Glbs) => codeCtx(Glbs,Lc,0,0).

  ctxLbls:(codeCtx,codeCtx)=>codeCtx.
  ctxLbls(codeCtx(Vrs,Lc,Mx1,Lb1),codeCtx(_,_,Mx2,Lb2))=>codeCtx(Vrs,Lc,max(Mx1,Mx2),max(Lb1,Lb2)).

  implementation display[codeCtx] => {.
    disp(codeCtx(Vrs,_,Depth,_)) => ssSeq([ss("depth "),disp(Depth),ss(" vars "),disp(Vrs)]).
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
--    logMsg("globals $(Globals)");
--    logMsg("Defs $(Defs)");
    compDefs(Defs,localFuns(Defs,foldRight(((Pk,Tp),G)=>G[Pk->glbVar(Pk,Tp)],[],Globals)),
      Opts,genBoot(Pkg,Defs),Rp)
  }.

  localFuns:(cons[crDefn],map[string,srcLoc])=>map[string,srcLoc].
  localFuns(Defs,Vars) => foldRight(defFun,Vars,Defs).

  defFun(fnDef(Lc,Nm,Tp,_,_),Vrs) => Vrs[Nm->glbFun(Nm,Tp)].
  defFun(vrDef(Lc,crId(Nm,Tp),_),Vrs) => Vrs[Nm->glbVar(Nm,Tp)].
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
--    logMsg("initial context: $(Ctx)");
    Ctxa .= argVars(Args,Ctx,0);
    (Ctxx,Code,Stk) <- compExp(Val,Opts,retCont(Lc),Ctxa,[],some([]),Rp);
--    logMsg("final context: $(Ctxx)");
    valis method(tLbl(Nm,size(Args)),Tp,peep(Code::cons[assemOp]))
  }
  compDefn(vrDef(Lc,crId(Nm,Tp),Val),Glbs,Opts,Rp) => do{
    if Opts.showCode then
      logMsg("compile global $(Nm)\:$(Tp) = $(Val))");
    Ctx .= emptyCtx(Lc,Glbs);
--    logMsg("initial context: $(Ctx)");
    (Ctxx,Code,Stk) <- compExp(Val,Opts,bothCont(stoGlb(Nm),retCont(Lc)),Ctx,[],some([]),Rp);
    valis global(tLbl(Nm,0),Tp,peep(Code::cons[assemOp]))
  }

  compExp:(crExp,compilerOptions,Cont,codeCtx,multi[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  compExp(Exp,_,_,_,_,.none,Rp) => other(reportError(Rp,"dead code",locOf(Exp))).
  compExp(Exp,_,Cont,Ctx,Cde,some(Stk),Rp) where Const^=Exp::option[term] =>
    Cont.C(Ctx,Cde++[iLdC(Const)],some([typeOf(Exp),..Stk]),Rp).
  compExp(crInt(Lc,Ix),_,Cont,Ctx,Cde,some(Stk),Rp) =>
    Cont.C(Ctx,Cde++[iLdC(intgr(Ix))],some([intType,..Stk]),Rp).
  compExp(crFlot(Lc,Dx),_,Cont,Ctx,Cde,some(Stk),Rp) =>
    Cont.C(Ctx,Cde++[iLdC(flot(Dx))],some([fltType,..Stk]),Rp).
  compExp(crStrg(Lc,Sx),_,Cont,Ctx,Cde,some(Stk),Rp) =>
    Cont.C(Ctx,Cde++[iLdC(strg(Sx))],some([strType,..Stk]),Rp).
  compExp(crLbl(Lc,Nm,Tp),_,Cont,Ctx,Cde,some(Stk),Rp) =>
    Cont.C(Ctx,Cde++[iLdC(enum(tLbl(Nm,0)))],some([Tp,..Stk]),Rp).
  compExp(crVar(Lc,crId(Vr,Tp)),Opts,Cont,Ctx,Cde,Stk,Rp) => do{
    if Loc^=locateVar(Vr,Ctx) then {
      compVar(Lc,Vr,Loc,Opts,Cont,Ctx,Cde,Stk,Rp)
    } else
    throw reportError(Rp,"cannot locate variable $(Vr)\:$(Tp)",Lc)
  }
  compExp(crTerm(Lc,Nm,Args,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) =>
    compExps(Args,Opts,bothCont(allocCont(tLbl(Nm,size(Args)),Tp,Stk),Cont),Ctx,Cde,Stk,Rp).
  compExp(crIntrinsic(Lc,Op,Args,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) =>
    compExps(Args,Opts,bothCont(asmCont(Op,size(Args),Tp,Stk),Cont),Ctx,Cde,Stk,Rp).
  compExp(crECall(Lc,Nm,Args,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) =>
    compExps(Args,Opts,bothCont(escCont(Nm,size(Args),Tp,Stk),Cont),Ctx,Cde,Stk,Rp).
  compExp(crCall(Lc,Nm,Args,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) =>
    compExps(Args,Opts,bothCont(callCont(Nm,size(Args),Tp,Stk),Cont),Ctx,Cde,Stk,Rp).
  compExp(crCall(Lc,Nm,Args,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) =>
    compExps(Args,Opts,bothCont(callCont(Nm,size(Args),Tp,Stk),Cont),Ctx,Cde,Stk,Rp).
  compExp(crOCall(Lc,Op,Args,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) =>
    compExps(Args,Opts,bothCont(expCont(Op,Opts,oclCont(size(Args)+1,Tp,Stk)),Cont),Ctx,Cde,Stk,Rp).
  compExp(crRecord(Lc,Nm,Fields,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) => do{
    Sorted .= sort(Fields,((F1,_),(F2,_))=>F1<F2);
    Args .= (Sorted//((_,V))=>V);
    compExps(Args,Opts,bothCont(allocCont(tRec(Nm,Sorted//((F,V))=>(F,typeOf(V))),Tp,Stk),Cont),Ctx,Cde,Stk,Rp).    
  }
  compExp(crDot(Lc,Rc,Field,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) =>
    compExp(Rc,Opts,bothCont(fldCont(Field,Tp),Cont),Ctx,Cde,Stk,Rp).
  compExp(crTplOff(Lc,Rc,Ix,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) => do{
    compExp(Rc,Opts,bothCont(tplOffCont(Ix,Tp),Cont),Ctx,Cde,Stk,Rp)}
  compExp(crLtt(Lc,V,Val,Exp),Opts,Cont,Ctx,Cde,Stk,Rp) => do{
--    logMsg("$(crLtt(Lc,V,Val,Exp)) at $(Lc)");
    (Off,Ctx0) .= defineLclVar(V,Ctx);
    compExp(Val,Opts,
      bothCont(stoLcl(V,Off),expCont(Exp,Opts,Cont)),Ctx0,Cde,Stk,Rp)
  }
  compExp(crLtRec(Lc,V,Val,Exp),Opts,Cont,Ctx,Cde,Stk,Rp) => do{
    (Off,Ctx0) .= defineLclVar(V,Ctx);
    compExp(Val,Opts,
      bothCont(stoLcl(V,Off),expCont(Exp,Opts,Cont)),Ctx0,Cde,Stk,Rp)
  }
  compExp(crCase(Lc,Exp,Cases,Deflt,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) =>
    compCase(Lc,Exp,Cases,Deflt,Tp,Opts,Cont,Ctx,Cde,Stk,Rp).
  compExp(crAbort(Lc,Msg,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) =>
    compExps([Lc::crExp,crStrg(Lc,Msg)],Opts,bothCont(escCont("_abort",2,Tp,.none),Cont),Ctx,Cde,Stk,Rp).

  compExp(crCnd(Lc,T,L,R),Opts,Cont,Ctx,Cde,Stk,Rp) => do{
    CtxC .= ptnVars(T,Ctx);
    OS .= onceCont(Lc,Cont);
--    logMsg("cond cont is $(Cont.Simple) simple");
    compCond(T,Opts,
      resetCont(Stk,expCont(L,Opts,OS)),
      resetCont(Stk,expCont(R,Opts,OS)),CtxC,Cde,Stk,Rp)
  }

  
  compExp(C,Opts,Cont,Ctx,Cde,Stk,Rp) where isCrCond(C) => do{
    OS .= onceCont(locOf(C),Cont);
--    logMsg("created once for $(locOf(C))");
    compCond(C,Opts,bothCont(resetCont(Stk,
	  litCont(enum(tLbl("star.core#true",0)),boolType)),OS),
      bothCont(resetCont(Stk,
	  litCont(enum(tLbl("star.core#false",0)),boolType)),OS),
      Ctx,Cde,Stk,Rp).
  }

  -- Expressions are evaluated in reverse order
  compExps:(cons[crExp],compilerOptions,Cont,codeCtx,multi[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  compExps([],Opts,Cont,Ctx,Cde,Stk,Rp)=>Cont.C(Ctx,Cde,Stk,Rp).
  compExps([El,..Es],Opts,Cont,Ctx,Cde,Stk,Rp)=> do{
    (Nxt,Ctx1) .= defineLbl(Ctx);
    (Ctx2,Cde1,CCstk) <- compExps(Es,Opts,jmpCont(Nxt),Ctx1,Cde,Stk,Rp);
    compExp(El,Opts,Cont,Ctx2,Cde1++[iLbl(Nxt)],CCstk,Rp)
  }.

  compCase:(locn,crExp,cons[crCase],crExp,tipe,compilerOptions,Cont,codeCtx,multi[assemOp],option[cons[tipe]],reports) => either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  compCase(Lc,E,Cases,Deflt,Tp,Opts,Cont,Ctx,Cde,some(Stk),Rp) => do{
    (Nxt,Ctx1) .= defineLbl(Ctx);
    (DLbl,Ctx2) .= defineLbl(Ctx1);
    OC .= onceCont(Lc,Cont);
    (Ctx3,ECode,EStk) <- compExp(E,Opts,jmpCont(Nxt),Ctx2,Cde,some(Stk),Rp);
    (Table,Max) .= genCaseTable(Cases);
--    logMsg("case max: $(Max), table $(Table)");
    (Ctx4,CCode,_) <- compCases(Table,0,Max,Cont,jmpCont(DLbl),DLbl,Opts,Ctx3,ECode++[iLbl(Nxt),iCase(Max)],EStk,Rp);
    compExp(Deflt,Opts,Cont,Ctx4,CCode++[iLbl(DLbl),iRst(size(Stk))],some(Stk),Rp)
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
    (Lb,Ctx1) .= defineLbl(Ctx);
    (Ctx2,Cde2,_) <- compCases(Cases,Ix+1,Mx,Succ,Fail,Deflt,Opts,Ctx1,Cde++[iJmp(Lb)],Stk,Rp);
    compCaseBranch(Case,Succ,Fail,Deflt,Opts,Ctx2,Cde2++[iLbl(Lb)],Stk,Rp)
  }
  compCases([(Iy,Case),..Cases],Ix,Mx,Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) =>
    compCases([(Iy,Case),..Cases],Ix+1,Mx,Succ,Fail,Deflt,Opts,Ctx,Cde++[iJmp(Deflt)],Stk,Rp).

  compCaseBranch([(Lc,Ptn,Exp)],Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) => do{
    (Nxt,Ctx1) .= defineLbl(Ctx);
    (Ctx2,Cde2,Stk1)<-compPtn(Ptn,Opts,jmpCont(Nxt),Fail,Ctx1,Cde,Stk,Rp);
    compExp(Exp,Opts,Succ,Ctx2,Cde2++[iLbl(Nxt)],Stk1,Rp)
  }
  compCaseBranch([(Lc,Ptn,Exp),..More],Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) => do{
    (Fl,Ctx2) .= defineLbl(Ctx);
    (VLb,Ctx3) .= defineLbl(Ctx2);
    Vr .= crId(genSym("__"),typeOf(Ptn));
    (Off,Ctx4) .= defineLclVar(Vr,Ctx3);
    (Ctx5,Cde2,Stk1)<-compPtn(Ptn,Opts,expCont(Exp,Opts,Succ),jmpCont(Fl),Ctx4,Cde++[iTL(Off)],Stk,Rp);
    compMoreCase(More,Off,Succ,Fail,Opts,Ctx5,Cde2++[iLbl(Fl)],Stk,Rp)
  }

  compMoreCase:(cons[(locn,crExp,crExp)],integer,Cont,Cont,compilerOptions,codeCtx,multi[assemOp],option[cons[tipe]],reports) => either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  compMoreCase([],_,_,Fail,Opts,Ctx,Cde,Stk,Rp) => Fail.C(Ctx,Cde,Stk,Rp).
  compMoreCase([(Lc,Ptn,Exp),..More],Off,Succ,Fail,Opts,Ctx,Cde,Stk,Rp) => do{
    (Fl,Ctx1) .= defineLbl(Ctx);
    (Ctx5,Cde2,Stk1)<-compPtn(Ptn,Opts,expCont(Exp,Opts,Succ),jmpCont(Fl),Ctx1,Cde++[iLdL(Off)],Stk,Rp);
    compMoreCase(More,Off,Succ,Fail,Opts,Ctx5,Cde2++[iLbl(Fl)],Stk,Rp)
  }
    
  compVar:(locn,string,srcLoc,compilerOptions,Cont,codeCtx,multi[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  compVar(Lc,Nm,argVar(Off,Tp),Opts,Cont,Ctx,Cde,some(Stk),Rp) =>
    Cont.C(Ctx,Cde++[iLdA(Off)],some([Tp,..Stk]),Rp).
  compVar(Lc,Nm,lclVar(Off,Tp),Opts,Cont,Ctx,Cde,some(Stk),Rp) =>
    Cont.C(Ctx,Cde++[iLdL(Off)],some([Tp,..Stk]),Rp).
  compVar(Lc,_,glbVar(Nm,Tp),Opts,Cont,Ctx,Cde,some(Stk),Rp) =>
    Cont.C(Ctx,Cde++[iLdG(Nm)],some([Tp,..Stk]),Rp).
  compVar(Lc,_,glbFun(Nm,Tp),Opts,Cont,Ctx,Cde,some(Stk),Rp) =>
    Cont.C(Ctx,Cde++[iLdC(enum(tLbl(Nm,arity(Tp))))],some([Tp,..Stk]),Rp).
  compVar(Lc,Nm,_,_,_,_,_,.none,Rp) =>
    other(reportError(Rp,"unreachable variable $(Nm)",Lc)).
  compVar(Lc,Nm,Loc,Opts,_,_,_,_,Rp) =>
    other(reportError(Rp,"cannot compile variable $(Nm)",Lc)).

  compCond:(crExp,compilerOptions,Cont,Cont,codeCtx,multi[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  compCond(crCnj(Lc,L,R),Opts,Succ,Fail,Ctx,Cde,Stk,Rp) => do{
    OC .= onceCont(Lc,Fail);
    compCond(L,Opts,condCont(R,Opts,Succ,OC),OC,Ctx,Cde,Stk,Rp)
  }
  compCond(crDsj(Lc,L,R),Opts,Succ,Fail,Ctx,Cde,Stk,Rp) => do{
    OC .= onceCont(Lc,Succ);
    compCond(L,Opts,OC,condCont(R,Opts,OC,Fail),Ctx,Cde,Stk,Rp)
  }
  compCond(crNeg(Lc,R),Opts,Succ,Fail,Ctx,Cde,Stk,Rp) => do{
--    logMsg("compile negated $(R), stack is $(Stk)");
    compCond(R,Opts,Fail,Succ,Ctx,Cde,Stk,Rp)
  }.
  compCond(crCnd(Lc,T,L,R),Opts,Succ,Fail,Ctx,Cde,Stk,Rp) => do{
--    logMsg("compile conditional $(crCnd(Lc,T,L,R)), stack is $(Stk)");
    OS .= onceCont(Lc,resetCont(Stk,Succ));
    OF .= onceCont(Lc,resetCont(Stk,Fail));
    CtxC .= ptnVars(T,Ctx);
    compCond(T,Opts,condCont(L,Opts,OS,OF),condCont(R,Opts,OS,OF),CtxC,Cde,Stk,Rp)
  }
  compCond(crMatch(Lc,Ptn,Exp),Opts,Succ,Fail,Ctx,Cde,Stk,Rp) => do{
--    logMsg("compile match $(crMatch(Lc,Ptn,Exp)), stack = $(Stk)");
    compExp(Exp,Opts,ptnCont(Ptn,Opts,Succ,Fail),Ctx,Cde,Stk,Rp)
  }
  compCond(Exp,Opts,Succ,Fail,Ctx,Cde,Stk,Rp) => do{
--    logMsg("compile other condition $(Exp), stack = $(Stk), Succ=$(Succ), Fail=$(Fail)");
    compExp(Exp,Opts,testCont(locOf(Exp),Succ,Fail),Ctx,Cde,Stk,Rp).
  }
  compPtn:(crExp,compilerOptions,Cont,Cont,codeCtx,multi[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  compPtn(crVar(Lc,crId(Vr,Tp)),Opts,Succ,Fail,Ctx,Cde,Stk,Rp) => do{
    if Loc ^= locateVar(Vr,Ctx) then 
      compPtnVar(Lc,Vr,Loc,Opts,Succ,Ctx,Cde,Stk,Rp)
    else{
      (Off,Ctx1) .= defineLclVar(crId(Vr,Tp),Ctx);
      compPtnVar(Lc,Vr,lclVar(Off,Tp),Opts,Succ,Ctx1,Cde,Stk,Rp)
    }
  }
  compPtn(L,Opts,Succ,Fail,Ctx,Cde,some(Stk),Rp) where (T,Tp)^=isLiteral(L) =>
    ptnTest(locOf(L),Tp,Succ,Fail,Ctx,Cde++[iLdC(T)],some([Tp,..Stk]),Rp).
  compPtn(crTerm(Lc,Nm,Args,Tp),Opts,Succ,Fail,Ctx,Cde,some([_,..Stk]),Rp) => do{
--    logMsg("compiling term $(crTerm(Lc,Nm,Args,Tp))");
    (FLb,Ctx1) .= defineLbl(Ctx);
    (NLb,Ctx2) .= defineLbl(Ctx1);
    (Ctx3,FlCode,_Stx3) <- Fail.C(Ctx2,[iLbl(FLb),iRst(size(Stk))],some(Stk),Rp);
    compPtns(Args,0,Opts,
      resetCont(some(Stk),Succ),jmpCont(FLb),ctxLbls(Ctx,Ctx3),
      Cde++[.iDup,iLdC(enum(tLbl(Nm,size(Args)))),iCLbl(NLb)]++FlCode++[iLbl(NLb)],some([Tp,..Stk]),Rp)
  }
  compPtn(crWhere(Lc,Ptn,Cond),Opts,Succ,Fail,Ctx,Cde,Stk,Rp) =>
    compPtn(Ptn,Opts,condCont(Cond,Opts,Succ,Fail),Fail,Ctx,Cde,Stk,Rp).

  compPtns:(cons[crExp],integer,compilerOptions,Cont,Cont,
    codeCtx,multi[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  compPtns([],_,_,Succ,_,Ctx,Cde,Stk,Rp) => do{
    Succ.C(Ctx,Cde,Stk,Rp)
  }
  compPtns([A,..As],Ix,Opts,Succ,Fail,Ctx,Cde,some(Stk),Rp) => do{
    (NLb,Ctx1) .= defineLbl(Ctx);
    NCont .= resetCont(some(Stk),jmpCont(NLb));
    (Ctx2,Cde2,_) <- compPtn(A,Opts,NCont,Fail,Ctx1,Cde++[.iDup,iNth(Ix)],some([typeOf(A),..Stk]),Rp);
    compPtns(As,Ix+1,Opts,Succ,Fail,Ctx2,Cde2++[iLbl(NLb)],some(Stk),Rp)
  }
  
  compPtnVar:(locn,string,srcLoc,compilerOptions,Cont,codeCtx,multi[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  compPtnVar(Lc,Nm,lclVar(Off,Tp),Opts,Cont,Ctx,Cde,some([_,..Stk]),Rp) =>
    Cont.C(Ctx,Cde++[iStL(Off)],some(Stk),Rp).
  compPtnVar(Lc,Nm,Loc,_,_,_,_,_,Rp) => other(reportError(Rp,"cannot target var at $(Loc) in pattern",Lc)).

  ptnTest:(locn,tipe,Cont,Cont,codeCtx,multi[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])].
  ptnTest(_,intType,Succ,Fail,Ctx,Cde,some([_,_,..Stk]),Rp) where Fl^=Fail.L =>
    Succ.C(Ctx,Cde++[iICmp(Fl)],some(Stk),Rp).
  ptnTest(_,Tp,Succ,Fail,Ctx,Cde,some([_,_,..Stk]),Rp) where Fl^=Fail.L =>
    Succ.C(Ctx,Cde++[iCmp(Fl)],some(Stk),Rp).
  ptnTest(Lc,Tp,Succ,Fail,Ctx,Cde,some([_,_,..Stk]),Rp) => do{
--    logMsg("implement test at $(Lc), remaining stack is $(Stk)");
    (Fl,Ctx1) .= defineLbl(Ctx);
    (Ctx2,Cde1,Stk1) <- Succ.C(Ctx1,Cde++[iCmp(Fl)],some(Stk),Rp);
--    logMsg("stack after $(Lc) succ $(Stk1)");
    (Ctx3,Cde2,Stk2) <- Fail.C(Ctx2,Cde1++[iLbl(Fl)],some(Stk),Rp);
--    logMsg("stack after $(Lc) fail $(Stk2)");
    Stkx <- mergeStack(Lc,Stk1,Stk2,Rp);
    valis (Ctx3,Cde2,Stkx)
  }

  isLiteral:(crExp)=>option[(term,tipe)].
  isLiteral(crInt(_,Ix))=>some((intgr(Ix),intType)).
  isLiteral(crFlot(_,Dx))=>some((flot(Dx),fltType)).
  isLiteral(crStrg(_,Sx))=>some((strg(Sx),strType)).
  isLiteral(crLbl(_,Nm,Tp))=>some((enum(tLbl(Nm,0)),Tp)).
  isLiteral(_) default => .none.

  -- continuations

  ccont:(boolean,(codeCtx,multi[assemOp],option[cons[tipe]],reports)=>either[reports,(codeCtx,multi[assemOp],option[cons[tipe]])])=>Cont.
  ccont(S,C)=>cont{.
    C=C.
    L=.none.
    Simple = S.
  .}

  retCont:(locn)=>Cont.
  retCont(Lc) => ccont(.true,(Ctx,Cde,Stk,Rp) => do{
--      logMsg("ret at $(Lc), Stack is $(Stk)");
      if [Top]^=Stk then
	valis (Ctx,Cde++[.iRet],.none)
      else if Stk==.none then
	valis (Ctx,Cde,.none)
      else
      throw reportError(Rp,"top of stack should have exactly one value, not $(Stk)",Lc)
    }).

  jmpCont:(assemLbl)=>Cont.
  jmpCont(Lb) => cont{
    C(Ctx,Cde,Stk,Rp)=>do{
      valis (Ctx,Cde++[iJmp(Lb)],Stk)
    }.
    L=some(Lb).
    Simple=.true.
  }.

  resetCont:(option[cons[tipe]],Cont)=>Cont.
  resetCont(.none,Cont) => Cont.
  resetCont(some(SStk),C) where Dpth.=size(SStk) =>
    ccont(C.Simple,(Ctx,Cde,Stk,Rp)=>
	C.C(Ctx,Cde++genRst(Dpth,Stk),trimStack(Stk,Dpth),Rp)).

  genRst:(integer,option[cons[tipe]]) => multi[assemOp].
  genRst(_,.none) => [].
  genRst(Dpth,some(S)) where size(S)==Dpth => [].
  genRst(Dpth,_) => [iRst(Dpth)].

  allocCont:(termLbl,tipe,option[cons[tipe]])=>Cont.
  allocCont(Lbl,Tp,some(OStk)) =>
    ccont(.true,(Ctx,Cde,Stk,Rp)=>either((Ctx,Cde++[iAlloc(Lbl)],some([Tp,..OStk])))).

  asmCont:(assemOp,integer,tipe,option[cons[tipe]])=>Cont.
  asmCont(Op,Ar,Tp,some(Stk)) =>
    ccont(.true,(Ctx,Cde,OStk,Rp) => either((Ctx,Cde++[Op],some([Tp,..Stk])))).

  escCont:(string,integer,tipe,option[cons[tipe]])=>Cont.
  escCont(Nm,Ar,Tp,some(Stk)) =>
    ccont(.false,(Ctx,Cde,OStk,Rp) => do{
      valis (Ctx,Cde++[iEscape(Nm),iFrame(intgr(size(Stk)+1))],some([Tp,..Stk]))
    }).
  escCont(Nm,Ar,Tp,.none) => ccont(.true,(Ctx,Cde,OStk,Rp) => do{
      valis (Ctx,Cde++[iEscape(Nm),iFrame(intgr(0))],.none)
    }).

  callCont:(string,integer,tipe,option[cons[tipe]])=>Cont.
  callCont(Nm,Ar,Tp,some(Stk)) =>
    ccont(.false,(Ctx,Cde,OStk,Rp) =>
	either((Ctx,Cde++[iCall(tLbl(Nm,Ar)),iFrame(intgr(size(Stk)+1))],some([Tp,..Stk])))).

  oclCont:(integer,tipe,option[cons[tipe]])=>Cont.
  oclCont(Ar,Tp,some(Stk)) =>
    ccont(.false,(Ctx,Cde,SStk,Rp) => do{
      valis (Ctx,Cde++[iOCall(Ar),iFrame(intgr(size(Stk)+1))],some([Tp,..Stk]))
    }).

  expCont:(crExp,compilerOptions,Cont)=>Cont.
  expCont(Exp,Opts,Cont) =>
    ccont(.false,(Ctx,Cde,Stk,Rp) => do{
	compExp(Exp,Opts,Cont,Ctx,Cde,Stk,Rp)
    }).

  condCont:(crExp,compilerOptions,Cont,Cont)=>Cont.
  condCont(C,Opts,Succ,Fail)=>
    ccont(Succ.Simple && Fail.Simple,(Ctx,Cde,Stk,Rp) => do{
	compCond(C,Opts,Succ,Fail,Ctx,Cde,Stk,Rp)
      }).
  
  ptnCont:(crExp,compilerOptions,Cont,Cont)=>Cont.
  ptnCont(Ptn,Opts,Succ,Fail)=>
    ccont(.false,(Ctx,Cde,Stk,Rp)=> do{
--      logMsg("pattern cont $(Ptn), stack = $(Stk)");
      compPtn(Ptn,Opts,Succ,Fail,Ctx,Cde,Stk,Rp)}).

  fldCont:(string,tipe)=>Cont.
  fldCont(Fld,Tp)=>
    ccont(.true,(Ctx,Cde,some([T,..Stk]),Rp) => do{
--      logMsg("fldCont, current stack: $([T,..Stk]), new stack: $([Tp,..Stk])");
      valis (Ctx,Cde++[iGet(tLbl(Fld,0))],some([Tp,..Stk]))
    }).

--      either((Ctx,Cde++[iGet(tLbl(Fld,0))],[Tp,..Stk]))).

  tplOffCont:(integer,tipe)=>Cont.
  tplOffCont(Ix,Tp)=>
    ccont(.true,(Ctx,Cde,some([T,..Stk]),Rp) =>  do{
--      logMsg("offCont, current stack: $([T,..Stk]), new stack: $([Tp,..Stk])");
      valis (Ctx,Cde++[iNth(Ix)],some([Tp,..Stk]))
    })
  -- either((Ctx,Cde++[iNth(Ix)],[Tp,..Stk]))).

  litCont:(term,tipe)=>Cont.
  litCont(T,Tp) =>
    ccont(.true,(Ctx,Cde,some(Stk),Rp) => do{
--      logMsg("push literal $(T), stack now $([Tp,..Stk])");
      valis (Ctx,Cde++[iLdC(T)],some([Tp,..Stk]))
    }).
--      either((Ctx,Cde++[iLdC(T)],[Tp,..Stk]))).

  stoLcl:(crVar,integer)=>Cont.
  stoLcl(Vr,Off)=>
    ccont(.true,(Ctx,Cde,some([_,..Stk]),Rp) => do{
--	logMsg("storing $(Vr) at $(Off), stack currently $(Stk)");
	valis (Ctx,Cde++[iStL(Off)],some(Stk))
    }).
  
  stoCont:(crVar)=>Cont.
  stoCont(V)=>
    ccont(.true,(Ctx,Cde,some([_,..Stk]),Rp) => do{
	(Off,Ctx1) .= defineLclVar(V,Ctx);
--	logMsg("storing $(V) at $(Off), stack currently $(Stk)");
	valis (Ctx1,Cde++[iStL(Off)],some(Stk))
    }).

  stoGlb:(string)=>Cont.
  stoGlb(V) =>
    ccont(.true,(Ctx,Cde,Stk,Rp) => either((Ctx,Cde++[iTG(V)],Stk))).

  updateCont:(srcLoc)=>Cont.
  updateCont(lclVar(Off,_))=>
    ccont(.true,(Ctx,Cde,some([_,..Stk]),Rp) =>
      either((Ctx,Cde++[iStL(Off)],some(Stk)))).
  updateCont(argVar(Off,_))=>
    ccont(.true,(Ctx,Cde,some([_,..Stk]),Rp) =>
      either((Ctx,Cde++[iStA(Off)],some(Stk)))).
  updateCont(glbVar(Nm,_))=>
    ccont(.true,(Ctx,Cde,some([_,..Stk]),Rp) =>
      either((Ctx,Cde++[iStG(Nm)],some(Stk)))).
  updateCont(onStack(_))=>
    ccont(.true,(Ctx,Cde,Stk,Rp) => either((Ctx,Cde,Stk))).

  bothCont:(Cont,Cont)=>Cont.
  bothCont(F,G) => ccont(F.Simple&&G.Simple,(Ctx,Cde,Stk,Rp)=> do{
--      logMsg("start both conts, stack is $(Stk)");
      (Ct1,Cd1,Stk1) <- F.C(Ctx,Cde,Stk,Rp);
--      logMsg("between conts, stack is $(Stk1)");
      G.C(Ct1,Cd1,Stk1,Rp)
    }).

  onceCont:(locn,Cont)=>Cont.
  onceCont(_,C) where C.Simple => C.
  onceCont(Lc,C)=> let{.
    d := .none.
  .} in ccont(.false,(Ctx,Cde,Stk,Rp) => do{
--	logMsg("enter once, $(d!!)");
      if (Lbl,EStk,LStk)^=d!! then{
--	  logMsg("reuse $(Lc)\:$(Lbl), merge stacks $(EStk) and $(Stk), return $(LStk)");
	XStk <- mergeStack(Lc,EStk,Stk,Rp);
	valis (Ctx,Cde++[iJmp(Lbl)],LStk)
      }
      else{
	(Lbl,Cx) .= defineLbl(Ctx);
	(Ctx1,Cde1,Stk1) <- C.C(Cx,Cde++[iLbl(Lbl)],Stk,Rp);
	d := some((Lbl,Stk,Stk1));
--	  logMsg("triggering $(Lc)\:$(Lbl), record stack $(Stk) -> $(Stk1)");
	valis (Ctx1,Cde1,Stk1)
      }
  }).

  testCont:(locn,Cont,Cont)=>Cont.
  testCont(Lc,Succ,Fail)=>
    ccont(.false,(Ctx,Cde,some([_,..Stk]),Rp)=> do{
      (Lb,Ctx0) .= defineLbl(Ctx);
--      logMsg("run succ of test $(Lc), stack going in $(Stk)");
      (Ctx1,C1,Stk1) <- Succ.C(Ctx0,Cde++[iBf(Lb)],some(Stk),Rp);
--      logMsg("$(Lc)\: Stk1 = $(Stk1)");
--      logMsg("run fail of test, stack going in $(Stk)");
      (Ctx2,C2,Stk2) <- Fail.C(ctxLbls(Ctx,Ctx1),C1++[iLbl(Lb)],some(Stk),Rp);
--      logMsg("$(Lc)\: Stk2 = $(Stk2)");
      Stkx <- mergeStack(Lc,Stk1,Stk2,Rp);
--      logMsg(" $(Lc) merged stack $(Stkx)");
      valis (Ctx2,C2,Stkx)
    }
  ).

  mergeStack:(locn,option[cons[tipe]],option[cons[tipe]],reports)=>either[reports,option[cons[tipe]]].
  mergeStack(Lc,.none,S,_) => either(S).
  mergeStack(Lc,S,.none,_) => either(S).
  mergeStack(Lc,S1,S2,_) where S1==S2 => either(S1).
  mergeStack(Lc,S1,S2,Rp) => other(reportError(Rp,"inconsistent stacks: $(S1) vs $(S2)",Lc)).
  
  locateVar:(string,codeCtx)=>option[srcLoc].
  locateVar(Nm,codeCtx(Vars,_,_,_)) => Vars[Nm].

  defineLclVar:(crVar,codeCtx) => (integer,codeCtx).
  defineLclVar(crId(Nm,Tp),codeCtx(Vrs,Lc,Count,Lbl)) where NxtCnt .= Count+1 =>
    (NxtCnt,codeCtx(Vrs[Nm->lclVar(NxtCnt,Tp)],Lc,NxtCnt,Lbl)).

  defineLbl:(codeCtx)=>(assemLbl,codeCtx).
  defineLbl(codeCtx(Vrs,Lc,Count,Lb))=>(al("L$(Lb)"),codeCtx(Vrs,Lc,Count,Lb+1)).

  changeLoc:(locn,compilerOptions,codeCtx)=>(multi[assemOp],codeCtx).
  changeLoc(Lc,_,codeCtx(Vars,Lc0,Dp,Lb)) where Lc=!=Lc0 => ([iLine(Lc::term)],codeCtx(Vars,Lc,Dp,Lb)).
  changeLoc(_,_,Ctx)=>([],Ctx).

  implementation hasLoc[codeCtx] => {
    locOf(codeCtx(_,Lc,_,_))=>Lc.
  }

  ptnVars:(crExp,codeCtx) => codeCtx.
  ptnVars(crVar(_,crId(Nm,Tp)),codeCtx(Vars,CLc,Count,Lb)) where _ ^= Vars[Nm] => codeCtx(Vars,CLc,Count,Lb).
  ptnVars(crVar(_,crId(Nm,Tp)),codeCtx(Vars,CLc,Count,Lb)) => codeCtx(Vars[Nm->lclVar(Count+1,Tp)],CLc,Count+1,Lb).
  ptnVars(crInt(_,_),Ctx) => Ctx.
  ptnVars(crFlot(_,_),Ctx) => Ctx.
  ptnVars(crStrg(_,_),Ctx) => Ctx.
  ptnVars(crLbl(_,_,_),Ctx) => Ctx.
  ptnVars(crTerm(_,Op,Els,_),Ctx) => foldRight(ptnVars,Ctx,Els).
  ptnVars(crCall(_,_,_,_),Ctx) => Ctx.
  ptnVars(crIntrinsic(_,_,_,_),Ctx) => Ctx.
  ptnVars(crECall(_,_,_,_),Ctx) => Ctx.
  ptnVars(crOCall(_,_,_,_),Ctx) => Ctx.
  ptnVars(crRecord(_,_,Els,_),Ctx) => foldRight(((_,El),X)=>ptnVars(El,X),Ctx,Els).
  ptnVars(crDot(_,_,_,_),Ctx) => Ctx.
  ptnVars(crTplOff(_,_,_,_),Ctx) => Ctx.
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
    argVars(As,codeCtx(Vars[Nm->argVar(Ix,Tp)],CLc,Count+1,Lb),Ix+1).
  argVars([_,..As],Ctx,Arg) => argVars(As,Ctx,Arg+1).

  mergeCtx:(codeCtx,codeCtx,codeCtx)=>codeCtx.
  mergeCtx(codeCtx(LV,_,_,Lb1),codeCtx(RV,_,_,Lb2),Base) => let{
    mergeVar:(string,srcLoc,codeCtx) => codeCtx.
    mergeVar(Nm,_,Vrs) where _ ^= locateVar(Nm,Base) => Vrs.
    mergeVar(Nm,_,codeCtx(Vs,Lc,Count,_)) where lclVar(_,Tp) ^= RV[Nm] => codeCtx(Vs[Nm->lclVar(Count+1,Tp)],Lc,Count+1,max(Lb1,Lb2)).
    mergeVar(Nm,lclVar(_,Tp),codeCtx(Vs,Lc,Count,_)) => codeCtx(Vs[Nm->lclVar(Count+1,Tp)],Lc,Count+1,max(Lb1,Lb2)).
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
	  iFrame(intgr(0)),
	  .iHalt])].

  genBoot(_,_) default => [].
}

