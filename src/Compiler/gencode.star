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
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.terms.

  srcLoc ::= lclVar(integer,tipe) |
    argVar(integer,tipe) |
    glbVar(string,tipe) |
    onStack(tipe).

  Cont ::= cont{
    C:(codeCtx,list[assemOp],option[cons[tipe]],reports)=>either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
    L:option[assemLbl]
  }.

  codeCtx ::= codeCtx(map[string,srcLoc],locn,integer,integer).

  emptyCtx:(locn,list[(string,tipe)])=>codeCtx.
  emptyCtx(Lc,Glbs) => codeCtx(foldRight(((Pkg,Tp),G)=>G[Pkg->glbVar(Pkg,Tp)],[],Glbs),Lc,0,0).

  ctxLbls:(codeCtx,codeCtx)=>codeCtx.
  ctxLbls(codeCtx(Vrs,Lc,Mx1,Lb1),codeCtx(_,_,Mx2,Lb2))=>codeCtx(Vrs,Lc,max(Mx1,Mx2),max(Lb1,Lb2)).

  implementation display[codeCtx] => {.
    disp(codeCtx(Vrs,_,Depth,_)) => ssSeq([ss("dict"),disp(Vrs)]).
  .}

  implementation sizeable[codeCtx] => {.
    isEmpty(codeCtx(Vars,_,_,_))=>isEmpty(Vars).
    size(codeCtx(Vars,_,_,_))=>size(Vars).
  .}

  implementation display[srcLoc] => {.
    disp(lclVar(Off,Tpe)) =>ssSeq([ss("lcl "),disp(Off),ss(":"),disp(Tpe)]).
    disp(argVar(Off,Tpe)) =>ssSeq([ss("arg "),disp(Off),ss(":"),disp(Tpe)]).
    disp(glbVar(Off,Tpe)) =>ssSeq([ss("glb "),disp(Off),ss(":"),disp(Tpe)]).
  .}

  public compCrProg:(list[crDefn],list[(string,tipe)],compilerOptions,reports)=>
    either[reports,list[codeSegment]].
  compCrProg(Defs,Globals,Opts,Rp) =>
    compDefs(Defs,localFuns(Defs,Globals),Opts,[],Rp).

  localFuns:(list[crDefn],list[(string,tipe)])=>list[(string,tipe)].
  localFuns(Defs,Vars) => foldRight(defFun,Vars,Defs).

  defFun(fnDef(Lc,Nm,Tp,_,_),Vrs) => [(Nm,Tp),..Vrs].
  defFun(vrDef(Lc,crId(Nm,Tp),_),Vrs) => [(Nm,Tp),..Vrs].
  defFun(rcDef(_,_,_,_),Vrs) => Vrs.
  
  compDefs:(list[crDefn],list[(string,tipe)],compilerOptions,list[codeSegment],reports)=>
    either[reports,list[codeSegment]].
  compDefs([],_,_,Cs,_)=>either(Cs).
  compDefs([D,..Dfs],Glbs,Opts,Cs,Rp) => do{
    Code<-compDefn(D,Glbs,Opts,Rp);
    compDefs(Dfs,Glbs,Opts,[Cs..,Code],Rp)
  }

  compDefn:(crDefn,list[(string,tipe)],compilerOptions,reports) => either[reports,codeSegment].
  compDefn(fnDef(Lc,Nm,Tp,Args,Val),Glbs,Opts,Rp) => do{
    logMsg("compile $(fnDef(Lc,Nm,Tp,Args,Val))");
    Ctx .= emptyCtx(Lc,Glbs);
    Ctxa .= argVars(Args,Ctx,0);
    (Ctxx,Code,Stk) <- compExp(Val,Opts,retCont(Lc),Ctxa,[],some([]),Rp);
    valis method(tLbl(Nm,size(Args)),Tp,Code)
  }
  compDefn(vrDef(Lc,crId(Nm,Tp),Val),Glbs,Opts,Rp) => do{
    logMsg("compile global $(Nm)\:$(Tp) = $(Val))");
    Ctx .= emptyCtx(Lc,Glbs);
    (Ctxx,Code,Stk) <- compExp(Val,Opts,bothCont(stoGlb(Nm),retCont(Lc)),Ctx,[],some([]),Rp);
    valis global(tLbl(Nm,0),Tp,Code)
  }

  compExp:(crExp,compilerOptions,Cont,codeCtx,list[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
  compExp(Exp,_,_,_,_,.none,Rp) => other(reportError(Rp,"dead code",locOf(Exp))).
  compExp(crInt(Lc,Ix),_,Cont,Ctx,Cde,some(Stk),Rp) => Cont.C(Ctx,[Cde..,iLdC(intgr(Ix))],some([Stk..,intType]),Rp).
  compExp(crFlot(Lc,Dx),_,Cont,Ctx,Cde,some(Stk),Rp) => Cont.C(Ctx,[Cde..,iLdC(flot(Dx))],some([Stk..,fltType]),Rp).
  compExp(crStrg(Lc,Sx),_,Cont,Ctx,Cde,some(Stk),Rp) => Cont.C(Ctx,[Cde..,iLdC(strg(Sx))],some([Stk..,strType]),Rp).
  compExp(crLbl(Lc,Nm,Tp),_,Cont,Ctx,Cde,some(Stk),Rp) => Cont.C(Ctx,[Cde..,iLdC(enum(tLbl(Nm,0)))],some([Stk..,Tp]),Rp).
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
--    logMsg("compile $(crTplOff(Lc,Rc,Ix,Tp)), stack is $(Stk)");
    compExp(Rc,Opts,bothCont(tplOffCont(Ix,Tp),Cont),Ctx,Cde,Stk,Rp)}
  compExp(crLtt(Lc,V,Val,Exp),Opts,Cont,Ctx,Cde,Stk,Rp) => do{
--    logMsg("compile $(crLtt(Lc,V,Val,Exp)), stack = $(Stk)");
    (Ctx1,Cd1,_)<-compExp(Val,Opts,stoCont(V),Ctx,Cde,Stk,Rp);
    compExp(Exp,Opts,Cont,Ctx1,Cd1,Stk,Rp)
  }
  compExp(crCase(Lc,Exp,Cases,Deflt,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) =>
    compCase(Lc,Exp,Cases,Deflt,Tp,Opts,Cont,Ctx,Cde,Stk,Rp).
  compExp(crAbort(Lc,Msg,Tp),Opts,Cont,Ctx,Cde,Stk,Rp) =>
    compExps([Lc::crExp,crStrg(Lc,Msg)],Opts,bothCont(escCont("_abort",2,Tp,.none),Cont),Ctx,Cde,Stk,Rp).

  compExp(crCnd(Lc,T,L,R),Opts,Cont,Ctx,Cde,Stk,Rp) => do{
--    logMsg("compile conditional $(crCnd(Lc,T,L,R)), stack = $(Stk)");
    OS .= onceCont(Lc,Cont);
    compCond(T,Opts,expCont(L,Opts,OS),expCont(R,Opts,OS),Ctx,Cde,Stk,Rp)
  }
  compExp(C,Opts,Cont,Ctx,Cde,Stk,Rp) where isCrCond(C) => do{
    OS .= onceCont(locOf(C),Cont);
--    logMsg("created once for $(locOf(C))");
    compCond(C,Opts,bothCont(litCont(enum(tLbl("star.core$true",0)),boolType),OS),
      bothCont(litCont(enum(tLbl("star.core$false",0)),boolType),OS),Ctx,Cde,Stk,Rp).
  }

  compExps:(list[crExp],compilerOptions,Cont,codeCtx,list[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
  compExps([],Opts,Cont,Ctx,Cde,Stk,Rp)=>Cont.C(Ctx,Cde,Stk,Rp).
  compExps([El,..Es],Opts,Cont,Ctx,Cde,Stk,Rp)=>
    compExps(Es,Opts,expCont(El,Opts,Cont),Ctx,Cde,Stk,Rp).

  compCase:(locn,crExp,list[crCase],crExp,tipe,compilerOptions,Cont,codeCtx,list[assemOp],option[cons[tipe]],reports) => either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
  compCase(Lc,E,Cases,Deflt,Tp,Opts,Cont,Ctx,Cde,some(Stk),Rp) => do{
    (Nxt,Ctx1) .= defineLbl(Ctx);
    (DLbl,Ctx2) .= defineLbl(Ctx1);
    OC .= onceCont(Lc,Cont);
    (Ctx3,ECode,EStk) <- compExp(E,Opts,jmpCont(Nxt),Ctx2,Cde,some(Stk),Rp);
    (Table,Max) .= genCaseTable(Cases);
--    logMsg("case max: $(Max), table $(Table)");
    (Ctx4,CCode,_) <- compCases(Table,0,Max,Cont,jmpCont(DLbl),DLbl,Opts,Ctx3,[ECode..,iLbl(Nxt),iCase(Max)],EStk,Rp);
    compExp(Deflt,Opts,Cont,Ctx4,[CCode..,iLbl(DLbl),iRst(size(Stk))],some(Stk),Rp)
  }

  genCaseTable(Cases) where Mx.=nextPrime(size(Cases)) =>
    (sortCases(caseHashes(Cases,Mx)),Mx).

  caseHashes:(list[crCase],integer)=>list[(locn,crExp,integer,crExp)].
  caseHashes(Cases,Mx) => (Cases//((Lc,Pt,Ex))=>(Lc,Pt,caseHash(Pt,Mx),Ex)).

  caseHash:(crExp,integer)=>integer.
  caseHash(crVar(_,_),_) => 0.
  caseHash(crInt(_,Ix),Mx) => Ix%Mx.
  caseHash(crFlot(_,Dx),Mx) => hash(Dx)%Mx.
  caseHash(crStrg(_,Sx),Mx) => hash(Sx)%Mx.
  caseHash(crLbl(_,Nm,Tp),Mx) => (arity(Tp)*37+hash(Nm))%Mx.
  caseHash(crTerm(_,Nm,Args,_),Mx) => (size(Args)*37+hash(Nm))%Mx.

  sortCases(Cases) => mergeDuplicates(sort(Cases,((_,_,H1,_),(_,_,H2,_))=>H1<H2)).

  mergeDuplicates:(list[(locn,crExp,integer,crExp)])=>list[(integer,list[(locn,crExp,crExp)])].
  mergeDuplicates([])=>[].
  mergeDuplicates([(Lc,Pt,Hx,Ex),..M]) where (D,Rs).=mergeDuplicate(M,Hx,[]) =>
    [(Hx,[(Lc,Pt,Ex),..D]),..mergeDuplicates(Rs)].

  mergeDuplicate([(Lc,Pt,Hx,Ex),..M],Hx,SoFar) =>
    mergeDuplicate(M,Hx,[SoFar..,(Lc,Pt,Ex)]).
  mergeDuplicate(M,_,SoFar) default => (SoFar,M).

  compCases:(list[(integer,list[(locn,crExp,crExp)])],integer,integer,Cont,Cont,assemLbl,compilerOptions,
    codeCtx,list[assemOp],option[cons[tipe]],reports) => either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
  compCases([],Mx,Mx,_,_,_,_,Ctx,Cde,Stk,_) => either((Ctx,Cde,Stk)).
  compCases([],Ix,Mx,Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) where Ix<Mx =>
    compCases([],Ix+1,Mx,Succ,Fail,Deflt,Opts,Ctx,[Cde..,iJmp(Deflt)],Stk,Rp).
  compCases([(Ix,Case),..Cases],Ix,Mx,Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) => do{
    (Lb,Ctx1) .= defineLbl(Ctx);
    (Ctx2,Cde2,_) <- compCases(Cases,Ix+1,Mx,Succ,Fail,Deflt,Opts,Ctx1,[Cde..,iJmp(Lb)],Stk,Rp);
    compCaseBranch(Case,Succ,Fail,Deflt,Opts,Ctx2,[Cde2..,iLbl(Lb)],Stk,Rp)
  }
  compCases([(Iy,Case),..Cases],Ix,Mx,Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) =>
    compCases([(Iy,Case),..Cases],Ix+1,Mx,Succ,Fail,Deflt,Opts,Ctx,[Cde..,iJmp(Deflt)],Stk,Rp).

  compCaseBranch([(Lc,Ptn,Exp)],Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) => do{
    (Nxt,Ctx1) .= defineLbl(Ctx);
    (Ctx2,Cde2,Stk1)<-compPtn(Ptn,Opts,jmpCont(Nxt),Fail,Ctx1,Cde,Stk,Rp);
    compExp(Exp,Opts,Succ,Ctx2,[Cde2..,iLbl(Nxt)],Stk1,Rp)
  }
  compCaseBranch([(Lc,Ptn,Exp),..More],Succ,Fail,Deflt,Opts,Ctx,Cde,Stk,Rp) => do{
    (Fl,Ctx2) .= defineLbl(Ctx);
    (VLb,Ctx3) .= defineLbl(Ctx2);
    Vr .= crId(genSym("__"),typeOf(Ptn));
    (Off,Ctx4) .= defineLclVar(Vr,Ctx3);
    (Ctx5,Cde2,Stk1)<-compPtn(Ptn,Opts,expCont(Exp,Opts,Succ),jmpCont(Fl),Ctx4,[Cde..,iTL(Off)],Stk,Rp);
    compMoreCase(More,Off,Succ,Fail,Opts,Ctx5,[Cde2..,iLbl(Fl)],Stk,Rp)
  }

  compMoreCase:(list[(locn,crExp,crExp)],integer,Cont,Cont,compilerOptions,codeCtx,list[assemOp],option[cons[tipe]],reports) => either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
  compMoreCase([],_,_,Fail,Opts,Ctx,Cde,Stk,Rp) => Fail.C(Ctx,Cde,Stk,Rp).
  compMoreCase([(Lc,Ptn,Exp),..More],Off,Succ,Fail,Opts,Ctx,Cde,Stk,Rp) => do{
    (Fl,Ctx1) .= defineLbl(Ctx);
    (Ctx5,Cde2,Stk1)<-compPtn(Ptn,Opts,expCont(Exp,Opts,Succ),jmpCont(Fl),Ctx1,[Cde..,iLdL(Off)],Stk,Rp);
    compMoreCase(More,Off,Succ,Fail,Opts,Ctx5,[Cde2..,iLbl(Fl)],Stk,Rp)
  }
    
  compVar:(locn,string,srcLoc,compilerOptions,Cont,codeCtx,list[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
  compVar(Lc,Nm,argVar(Off,Tp),Opts,Cont,Ctx,Cde,some(Stk),Rp) =>
    Cont.C(Ctx,[Cde..,iLdA(Off)],some([Stk..,Tp]),Rp).
  compVar(Lc,Nm,lclVar(Off,Tp),Opts,Cont,Ctx,Cde,some(Stk),Rp) =>
    Cont.C(Ctx,[Cde..,iLdL(Off)],some([Stk..,Tp]),Rp).
  compVar(Lc,_,glbVar(Nm,Tp),Opts,Cont,Ctx,Cde,some(Stk),Rp) =>
    Cont.C(Ctx,[Cde..,iLdG(Nm)],some([Stk..,Tp]),Rp).
  compVar(Lc,Nm,_,_,_,_,_,.none,Rp) =>
    other(reportError(Rp,"unreachable variable $(Nm)",Lc)).
  compVar(Lc,Nm,Loc,Opts,_,_,_,_,Rp) =>
    other(reportError(Rp,"cannot compile variable $(Nm)",Lc)).

  compCond:(crExp,compilerOptions,Cont,Cont,codeCtx,list[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
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
    OS .= onceCont(Lc,Succ);
    OF .= onceCont(Lc,Fail);
    compCond(T,Opts,condCont(L,Opts,OS,OF),condCont(R,Opts,OS,OF),Ctx,Cde,Stk,Rp)
  }
  compCond(crMatch(Lc,Ptn,Exp),Opts,Succ,Fail,Ctx,Cde,Stk,Rp) => do{
--    logMsg("compile match $(crMatch(Lc,Ptn,Exp)), stack = $(Stk)");
    compExp(Exp,Opts,ptnCont(Ptn,Opts,Succ,Fail),Ctx,Cde,Stk,Rp)
  }
  compCond(Exp,Opts,Succ,Fail,Ctx,Cde,Stk,Rp) =>
    compExp(Exp,Opts,testCont(locOf(Exp),Succ,Fail),Ctx,Cde,Stk,Rp).

  compPtn:(crExp,compilerOptions,Cont,Cont,codeCtx,list[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
  compPtn(crVar(Lc,crId(Vr,Tp)),Opts,Succ,Fail,Ctx,Cde,Stk,Rp) => do{
    if Loc ^= locateVar(Vr,Ctx) then 
      compPtnVar(Lc,Vr,Loc,Opts,Succ,Ctx,Cde,Stk,Rp)
    else{
      (Off,Ctx1) .= defineLclVar(crId(Vr,Tp),Ctx);
      compPtnVar(Lc,Vr,lclVar(Off,Tp),Opts,Succ,Ctx1,Cde,Stk,Rp)
    }
  }
  compPtn(L,Opts,Succ,Fail,Ctx,Cde,some(Stk),Rp) where (T,Tp)^=isLiteral(L) =>
    ptnTest(locOf(L),Tp,Succ,Fail,Ctx,[Cde..,iLdC(T)],some([Stk..,Tp]),Rp).
  compPtn(crTerm(Lc,Nm,Args,Tp),Opts,Succ,Fail,Ctx,Cde,some([Stk..,_]),Rp) => do{
--    logMsg("compiling term $(crTerm(Lc,Nm,Args,Tp))");
    (FLb,Ctx1) .= defineLbl(Ctx);
    (NLb,Ctx2) .= defineLbl(Ctx1);
    JCont .= jmpCont(FLb);
    (Ctx3,FlCode,_Stx3) <- Fail.C(Ctx2,[iLbl(FLb)],some(Stk),Rp);
    RCont .= lblCont(FLb,resetCont(size(Stk),JCont));

    compPtns(Args,0,Opts,
      resetCont(size(Stk),Succ),RCont,ctxLbls(Ctx,Ctx3),
      [Cde..,.iDup,iLdC(enum(tLbl(Nm,size(Args)))),iCLbl(NLb)]++FlCode++[iLbl(NLb)],some([Stk..,Tp]),Rp)
  }
  compPtn(crWhere(Lc,Ptn,Cond),Opts,Succ,Fail,Ctx,Cde,Stk,Rp) =>
    compPtn(Ptn,Opts,condCont(Cond,Opts,Succ,Fail),Fail,Ctx,Cde,Stk,Rp).

  compPtns:(list[crExp],integer,compilerOptions,Cont,Cont,codeCtx,list[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
  compPtns([],_,_,Succ,_,Ctx,Cde,Stk,Rp) => do{
    Succ.C(Ctx,Cde,Stk,Rp)
  }
  compPtns([A,..As],Ix,Opts,Succ,Fail,Ctx,Cde,some(Stk),Rp) => do{
    (NLb,Ctx1) .= defineLbl(Ctx);
    NCont .= jmpCont(NLb);
    (Ctx2,Cde2,_) <- compPtn(A,Opts,NCont,Fail,Ctx1,[Cde..,.iDup,iNth(Ix)],some([Stk..,typeOf(A)]),Rp);
    compPtns(As,Ix+1,Opts,Succ,Fail,Ctx2,[Cde2..,iLbl(NLb)],some(Stk),Rp)
  }
  
  compPtnVar:(locn,string,srcLoc,compilerOptions,Cont,codeCtx,list[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
  compPtnVar(Lc,Nm,lclVar(Off,Tp),Opts,Cont,Ctx,Cde,some([Stk..,_]),Rp) =>
    Cont.C(Ctx,[Cde..,iStL(Off)],some(Stk),Rp).
  compPtnVar(Lc,Nm,Loc,_,_,_,_,_,Rp) => other(reportError(Rp,"cannot target var at $(Loc) in pattern",Lc)).

  ptnTest:(locn,tipe,Cont,Cont,codeCtx,list[assemOp],option[cons[tipe]],reports) =>
    either[reports,(codeCtx,list[assemOp],option[cons[tipe]])].
  ptnTest(_,intType,Succ,Fail,Ctx,Cde,some([Stk..,_,_]),Rp) where Fl^=Fail.L =>
    Succ.C(Ctx,[Cde..,iICmp(Fl)],some(Stk),Rp).
  ptnTest(_,Tp,Succ,Fail,Ctx,Cde,some([Stk..,_,_]),Rp) where Fl^=Fail.L =>
    Succ.C(Ctx,[Cde..,iCmp(Fl)],some(Stk),Rp).
  ptnTest(Lc,Tp,Succ,Fail,Ctx,Cde,some([Stk..,_,_]),Rp) => do{
--    logMsg("implement test at $(Lc), remaining stack is $(Stk)");
    (Fl,Ctx1) .= defineLbl(Ctx);
    (Ctx2,Cde1,Stk1) <- Succ.C(Ctx1,[Cde..,iCmp(Fl)],some(Stk),Rp);
--    logMsg("stack after $(Lc) succ $(Stk1)");
    (Ctx3,Cde2,Stk2) <- Fail.C(Ctx2,[Cde1..,iLbl(Fl)],some(Stk),Rp);
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

  ccont:((codeCtx,list[assemOp],option[cons[tipe]],reports)=>either[reports,(codeCtx,list[assemOp],option[cons[tipe]])])=>Cont.
  ccont(C)=>cont{
    C=C.
    L=.none.
  }

  retCont:(locn)=>Cont.
  retCont(Lc) => ccont((Ctx,Cde,Stk,Rp) => do{
--      logMsg("ret at $(Lc), Stack is $(Stk)");
      if [Top]^=Stk then
	valis (Ctx,[Cde..,.iRet],.none)
      else if Stk==.none then
	valis (Ctx,Cde,.none)
      else
      throw reportError(Rp,"top of stack should have exactly one valis, not $(Stk)",Lc)
    }).

  jmpCont:(assemLbl)=>Cont.
  jmpCont(Lb) => cont{
    C(Ctx,Cde,Stk,Rp)=>do{
--      logMsg("jump to $(Lb), Stack is $(Stk)");
      valis (Ctx,[Cde..,iJmp(Lb)],Stk)
    }.
    L=some(Lb).
  }.

  lblCont:(assemLbl,Cont)=>Cont.
  lblCont(Lb,CC) => cont{
    C(Ctx,Cde,Stk,Rp)=>
      CC.C(Ctx,[Cde..,iLbl(Lb)],Stk,Rp).
    L=.none.
  }.

  resetCont:(integer,Cont)=>Cont.
  resetCont(Dpth,C)=>ccont((Ctx,Cde,Stk,Rp)=>
      C.C(Ctx,[Cde..,iRst(Dpth)],trimStack(Stk,Dpth),Rp)).
  
  allocCont:(termLbl,tipe,option[cons[tipe]])=>Cont.
  allocCont(Lbl,Tp,some(OStk)) =>
    ccont((Ctx,Cde,Stk,Rp)=>either((Ctx,[Cde..,iAlloc(Lbl)],some([OStk..,Tp])))).

  asmCont:(assemOp,integer,tipe,option[cons[tipe]])=>Cont.
  asmCont(Op,Ar,Tp,some(Stk)) =>
    ccont((Ctx,Cde,OStk,Rp) => either((Ctx,[Cde..,Op],some([Stk..,Tp])))).

  escCont:(string,integer,tipe,option[cons[tipe]])=>Cont.
  escCont(Nm,Ar,Tp,some(Stk)) default => ccont((Ctx,Cde,OStk,Rp) => do{
--      logMsg("escape $(Nm) - stack left at $([Stk..,Tp])");
      valis (Ctx,[Cde..,iEscape(Nm),iFrame(intgr(size(Stk)+1))],some([Stk..,Tp]))
    }).
  escCont(Nm,Ar,Tp,.none) default => ccont((Ctx,Cde,OStk,Rp) => do{
--      logMsg("escape $(Nm) - stack left at $(OStk)");
      valis (Ctx,[Cde..,iEscape(Nm),iFrame(intgr(0))],.none)
    }).

  callCont:(string,integer,tipe,option[cons[tipe]])=>Cont.
  callCont(Nm,Ar,Tp,some(Stk)) =>
    ccont((Ctx,Cde,OStk,Rp) =>
	either((Ctx,[Cde..,iCall(tLbl(Nm,Ar)),iFrame(intgr(size(Stk)+1))],some([Stk..,Tp])))).

  oclCont:(integer,tipe,option[cons[tipe]])=>Cont.
  oclCont(Ar,Tp,some(Stk)) => ccont((Ctx,Cde,SStk,Rp) => do{
--      logMsg("oclCont, current stack: $(SStk), new stack: $([Stk..,Tp])");
      valis (Ctx,[Cde..,iOCall(Ar),iFrame(intgr(size(Stk)+1))],some([Stk..,Tp]))
    }).

--      either((Ctx,[Cde..,iOCall(Ar),iFrame(intgr(size(Stk)+1))],[Stk..,Tp]))).

  expCont:(crExp,compilerOptions,Cont)=>Cont.
  expCont(Exp,Opts,Cont) => ccont((Ctx,Cde,Stk,Rp) => do{
--      logMsg("expression cont $(Exp)\:$(typeOf(Exp)), stack is $(Stk)");
      compExp(Exp,Opts,Cont,Ctx,Cde,Stk,Rp)
    }).

  expsCont:(list[crExp],compilerOptions,Cont)=>Cont.
  expsCont(Exps,Opts,Cont) => ccont((Ctx,Cde,Stk,Rp)=>
      compExps(Exps,Opts,Cont,Ctx,Cde,Stk,Rp)).

  condCont:(crExp,compilerOptions,Cont,Cont)=>Cont.
  condCont(C,Opts,Succ,Fail)=>
    ccont((Ctx,Cde,Stk,Rp) => do{
--	logMsg("cond cont with $(C), stack currently $(Stk)");
	compCond(C,Opts,Succ,Fail,Ctx,Cde,Stk,Rp)
      }).
  
  ptnCont:(crExp,compilerOptions,Cont,Cont)=>Cont.
  ptnCont(Ptn,Opts,Succ,Fail)=>ccont((Ctx,Cde,Stk,Rp)=> do{
--      logMsg("pattern cont $(Ptn), stack = $(Stk)");
      compPtn(Ptn,Opts,Succ,Fail,Ctx,Cde,Stk,Rp)}).

  fldCont:(string,tipe)=>Cont.
  fldCont(Fld,Tp)=>ccont((Ctx,Cde,some([Stk..,T]),Rp) => do{
--      logMsg("fldCont, current stack: $([Stk..,T]), new stack: $([Stk..,Tp])");
      valis (Ctx,[Cde..,iGet(tLbl(Fld,0))],some([Stk..,Tp]))
    }).

--      either((Ctx,[Cde..,iGet(tLbl(Fld,0))],[Stk..,Tp]))).

  tplOffCont:(integer,tipe)=>Cont.
  tplOffCont(Ix,Tp)=>ccont((Ctx,Cde,some([Stk..,T]),Rp) =>  do{
--      logMsg("offCont, current stack: $([Stk..,T]), new stack: $([Stk..,Tp])");
      valis (Ctx,[Cde..,iNth(Ix)],some([Stk..,Tp]))
    })
  -- either((Ctx,[Cde..,iNth(Ix)],[Stk..,Tp]))).

  litCont:(term,tipe)=>Cont.
  litCont(T,Tp) => ccont((Ctx,Cde,some(Stk),Rp) => do{
--      logMsg("push literal $(T), stack now $([Stk..,Tp])");
      valis (Ctx,[Cde..,iLdC(T)],some([Stk..,Tp]))
    }).
--      either((Ctx,[Cde..,iLdC(T)],[Stk..,Tp]))).

  stoCont:(crVar)=>Cont.
  stoCont(V)=>ccont((Ctx,Cde,some([Stk..,_]),Rp) => do{
      (Off,Ctx1) .= defineLclVar(V,Ctx);
--      logMsg("sto cont, stack currently $(Stk)");
      valis (Ctx1,[Cde..,iStL(Off)],some(Stk))
    }).

  stoGlb:(string)=>Cont.
  stoGlb(V) => ccont((Ctx,Cde,Stk,Rp) => either((Ctx,[Cde..,iTG(V)],Stk))).

  updateCont:(srcLoc)=>Cont.
  updateCont(lclVar(Off,_))=>ccont((Ctx,Cde,some([Stk..,_]),Rp) =>
      either((Ctx,[Cde..,iStL(Off)],some(Stk)))).
  updateCont(argVar(Off,_))=>ccont((Ctx,Cde,some([Stk..,_]),Rp) =>
      either((Ctx,[Cde..,iStA(Off)],some(Stk)))).
  updateCont(glbVar(Nm,_))=>ccont((Ctx,Cde,some([Stk..,_]),Rp) =>
      either((Ctx,[Cde..,iStG(Nm)],some(Stk)))).
  updateCont(onStack(_))=>ccont((Ctx,Cde,Stk,Rp) => either((Ctx,Cde,Stk))).

  bothCont:(Cont,Cont)=>Cont.
  bothCont(F,G) => ccont((Ctx,Cde,Stk,Rp)=> do{
--      logMsg("start both conts, stack is $(Stk)");
      (Ct1,Cd1,Stk1) <- F.C(Ctx,Cde,Stk,Rp);
--      logMsg("between conts, stack is $(Stk1)");
      G.C(Ct1,Cd1,Stk1,Rp)
    }).

  onceCont:(locn,Cont)=>Cont.
  onceCont(_,C) where Lbl^=C.L => C.
  onceCont(Lc,C)=> let{
    d := .none.
    cc:Cont.
    cc=ccont((Ctx,Cde,Stk,Rp) => do{
--	logMsg("enter once, $(d!)");
	if (Lbl,EStk,LStk)^=d! then{
--	  logMsg("reuse $(Lc)\:$(Lbl), merge stacks $(EStk) and $(Stk), return $(LStk)");
	  XStk <- mergeStack(Lc,EStk,Stk,Rp);
	  valis (Ctx,[Cde..,iJmp(Lbl)],LStk)
	}
	else{
	  (Lbl,Cx) .= defineLbl(Ctx);
	  (Ctx1,Cde1,Stk1) <- C.C(Cx,[Cde..,iLbl(Lbl)],Stk,Rp);
	  d := some((Lbl,Stk,Stk1));
--	  logMsg("triggering $(Lc)\:$(Lbl), record stack $(Stk) -> $(Stk1)");
	  valis (Ctx1,Cde1,Stk1)
	}
      })
  } in cc.

  testCont:(locn,Cont,Cont)=>Cont.
  testCont(Lc,Succ,Fail)=>ccont((Ctx,Cde,some([Stk..,_]),Rp)=> do{
      (Lb,Ctx0) .= defineLbl(Ctx);
--      logMsg("run succ of test $(Lc), stack going in $(Stk)");
      (Ctx1,C1,Stk1) <- Succ.C(Ctx0,[Cde..,iBf(Lb)],some(Stk),Rp);
--      logMsg("$(Lc)\: Stk1 = $(Stk1)");
--      logMsg("run fail of test, stack going in $(Stk)");
      (Ctx2,C2,Stk2) <- Fail.C(ctxLbls(Ctx,Ctx1),[C1..,iLbl(Lb)],some(Stk),Rp);
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

  changeLoc:(locn,compilerOptions,codeCtx)=>(list[assemOp],codeCtx).
  changeLoc(Lc,_,codeCtx(Vars,Lc0,Dp,Lb)) where Lc=!=Lc0 => ([iLine(Lc::term)],codeCtx(Vars,Lc,Dp,Lb)).
  changeLoc(_,_,Ctx)=>([],Ctx).

  implementation hasLoc[codeCtx] => {
    locOf(codeCtx(_,Lc,_,_))=>Lc.
  }

  ptnVars:(crExp,codeCtx) => codeCtx.
  ptnVars(crVar(_,crId(Nm,Tp)),codeCtx(Vars,CLc,Count,Lb)) where _ ^= Vars[Nm] => codeCtx(Vars,CLc,Count,Lb).
  ptnVars(crVar(_,crId(Nm,Tp)),codeCtx(Vars,CLc,Count,Lb)) => codeCtx(Vars[Nm->lclVar(Count,Tp)],CLc,Count+1,Lb).
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

  argVars:(list[crVar],codeCtx,integer) => codeCtx.
  argVars([],Ctx,_)=>Ctx.
  argVars([crId(Nm,Tp),..As],codeCtx(Vars,CLc,Count,Lb),Ix) =>
    argVars(As,codeCtx(Vars[Nm->argVar(Ix,Tp)],CLc,Count+1,Lb),Ix+1).
  argVars([_,..As],Ctx,Arg) => argVars(As,Ctx,Arg+1).

  mergeCtx:(codeCtx,codeCtx,codeCtx)=>codeCtx.
  mergeCtx(codeCtx(LV,_,_,Lb1),codeCtx(RV,_,_,Lb2),Base) => let{
    mergeVar:(string,srcLoc,codeCtx) => codeCtx.
    mergeVar(Nm,_,Vrs) where _ ^= locateVar(Nm,Base) => Vrs.
    mergeVar(Nm,_,codeCtx(Vs,Lc,Count,_)) where lclVar(_,Tp) ^= RV[Nm] => codeCtx(Vs[Nm->lclVar(Count,Tp)],Lc,Count+1,max(Lb1,Lb2)).
  } in ixRight(mergeVar,Base,LV).

  drop:all x,e ~~ stream[x->>e] |: (x,integer)=>x.
  drop(S,0)=>S.
  drop([S..,_],N)=>drop(S,N-1).

  trimStack:all x, e ~~ stream[x->>e],sizeable[x] |: (option[x],integer)=>option[x].
  trimStack(some(L),N) =>some(drop(L,size(L)-N)).
  trimStack(.none,_) => .none.
}

