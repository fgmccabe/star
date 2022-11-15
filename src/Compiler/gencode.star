star.compiler.gencode{
  import star.
  import star.multi.
  import star.pkg.
  import star.sort.

  import star.compiler.assem.
  import star.compiler.term.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.peephole.
  import star.compiler.ltipe.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.data.

  public compProg:(pkg,cons[cDefn],cons[decl])=>cons[codeSegment].
  compProg(Pkg,Defs,Globals) => valof{
    Vars = foldLeft(declGlobal,[],Globals);
    valis compDefs(Defs,localFuns(Defs,Vars))
  }

  declGlobal(.varDec(_,_,Nm,Tp), Vrs) => Vrs[Nm->glbVar(Nm,Tp::ltipe)].
  declGlobal(.funDec(_,_,Nm,Tp), Vrs) => Vrs[Nm->glbVar(Nm,Tp::ltipe)].
  declGlobal(.accDec(_,_,_,Nm,Tp), Vrs) => Vrs[Nm->glbVar(Nm,Tp::ltipe)].
  declGlobal(.updDec(_,_,_,Nm,Tp), Vrs) => Vrs[Nm->glbVar(Nm,Tp::ltipe)].
  declGlobal(_,Vrs) => Vrs.

  localFuns:(cons[cDefn],map[string,srcLoc])=>map[string,srcLoc].
  localFuns(Defs,Vars) => foldRight(defFun,Vars,Defs).

  defFun(Def,Vrs) => case Def in {
    .fnDef(Lc,Nm,Tp,_,_) => Vrs[Nm->glbFun(tLbl(Nm,arity(Tp)),Tp::ltipe)].
    .vrDef(Lc,Nm,Tp,_) => Vrs[Nm->glbVar(Nm,Tp::ltipe)].
    _ default => Vrs
  }
  
  compDefs:(cons[cDefn],map[string,srcLoc])=> cons[codeSegment].
  compDefs(Dfs,Glbs) => (Dfs//(D)=>genDef(D,Glbs)).

  genDef:(cDefn,map[string,srcLoc]) => codeSegment.
  genDef(Defn,Glbs) => case Defn in {
    .fnDef(Lc,Nm,Tp,Args,Val) => valof{
      if traceCodegen! then
	logMsg("compile $(fnDef(Lc,Nm,Tp,Args,Val))");
      Ctx = emptyCtx(argVars(Args,Glbs,0));
      (_,AbortCde) = abortCont(Lc,"function: $(Nm)").C(Ctx,.none,[]);
      (_Stk,Code) = compExp(Val,retCont,jmpCont(Ctx.escape,.none),Ctx,.some([]));
      if traceCodegen! then
	logMsg("non-peep code is $((Code++[.iLbl(Ctx.escape),..AbortCde])::cons[assemOp])");
      Peeped = peepOptimize(([.iLocals(Ctx.hwm!),..Code]++[.iLbl(Ctx.escape),..AbortCde])::cons[assemOp]);
      if traceCodegen! then
	logMsg("code is $(.func(tLbl(Nm,size(Args)),.hardDefinition,Tp,Ctx.hwm!,Peeped))");
      valis .func(tLbl(Nm,size(Args)),.hardDefinition,Tp,Ctx.hwm!,Peeped)
    }.
    .vrDef(Lc,Nm,Tp,Val) => valof{
      if traceCodegen! then
	logMsg("compile global $(Nm)\:$(Tp) = $(Val))");
      Ctx = emptyCtx(Glbs);
      (_,AbortCde) = abortCont(Lc,"global: $(Nm)").C(Ctx,.none,[]);
      (_Stk,Code) = compExp(Val,glbRetCont(Nm),jmpCont(Ctx.escape,.none),Ctx,.some([]));
      Peeped = peepOptimize(([.iLocals(Ctx.hwm!),..Code]++[.iLbl(Ctx.escape),..AbortCde])::cons[assemOp]);
      if traceCodegen! then
	logMsg("code is $(.global(tLbl(Nm,0),Tp,Ctx.hwm!,Peeped))");

      valis .global(tLbl(Nm,0),Tp,Ctx.hwm!,Peeped)
    }.
    .tpDef(Lc,Tp,TpRl,Index) => .tipe(Tp,TpRl,Index).
    .lblDef(_Lc,Lbl,Tp,Ix) => .struct(Lbl,Tp,Ix).
  }

  compExp:(cExp,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compExp(Exp,Cont,ECont,Ctx,Stk) => case Exp in {
    E where isGround(E) =>
      Cont.C(Ctx,pushStack(typeOf(Exp)::ltipe,Stk),[.iLdC(Exp::data)]).
    .cVar(Lc,.cId(Vr,Tp)) => valof{
      if Loc?=locateVar(Vr,Ctx) then {
	valis compVar(Lc,Vr,Loc,Cont,Ctx,Stk)
      } else {
	reportError("cannot locate variable $(Vr)\:$(Tp)",Lc);
	valis Cont.C(Ctx,pushStack(Tp::ltipe,Stk),[.iLdV])
      }
    }.
    .cTerm(_,Nm,Args,Tp) =>
      compExps(Args,allocCont(tLbl(Nm,size(Args)),pushStack(Tp::ltipe,Stk),Cont),ECont,Ctx,Stk).
    .cECall(Lc,Op,Args,Tp) where (_,Ins,Frm)?=intrinsic(Op) =>
      compExps(Args,intrinsicCont(Ins,Frm,pushStack(Tp::ltipe,Stk),Cont),ECont,Ctx,Stk).
    .cECall(Lc,Es,Args,Tp) =>
      compExps(Args,escapeCont(Es,pushStack(Tp::ltipe,Stk),Cont),ECont,Ctx,Stk).
    .cCall(Lc,Nm,Args,Tp) =>
      compExps(Args,callCont(tLbl(Nm,[|Args|]),pushStack(Tp::ltipe,Stk),Cont),ECont,Ctx,Stk).
    .cOCall(Lc,Op,Args,Tp) => 
      compExps(Args,expCont(Op,oclCont([|Args|]+1,pushStack(Tp::ltipe,Stk),Cont),ECont),
	ECont,Ctx,Stk).
    .cNth(Lc,E,Ix,Tp) =>
      compExp(E,nthCont(Ix,Cont,pushStack(Tp::ltipe,Stk)),ECont,Ctx,Stk).
    .cSetNth(Lc,R,Ix,V) =>
      compExp(R,expCont(V,setNthCont(Ix,Cont,Stk),ECont),ECont,Ctx,Stk).
    .cThrow(Lc,Exp,_) =>
      compExp(Exp,ECont,abortCont(Lc,"throw"),Ctx,Stk).
    .cSeq(_,L,R) =>
      compExp(L,resetCont(Stk,expCont(R,Cont,ECont)),ECont,Ctx,Stk).
    .cCnd(Lc,G,L,R) => valof{
      LC = splitCont(Lc,Ctx,expCont(L,Cont,ECont));
      RC = splitCont(Lc,Ctx,expCont(R,Cont,ECont));
      valis compCond(G,LC,RC,ECont,Ctx,Stk)
    }.
    .cCase(Lc,Gov,Cases,Deflt,_Tp) =>
      compCase(Lc,Gov,Cases,Deflt,expCont,Cont,ECont,Ctx,Stk).
    .cUnpack(Lc,Gov,Cases,_Tp) => valof{
      valis compCnsCase(Lc,Gov,Cases,expCont,Cont,ECont,Ctx,Stk)
    }.
    .cLtt(Lc,.cId(Vr,VTp),Val,Bnd) => valof{
      (Off,Ctx1) = defineLclVar(Vr,VTp::ltipe,Ctx);
      valis compExp(Val,stoCont(Off,Stk,expCont(Bnd,Cont,ECont)),ECont,Ctx1,Stk)
    }.
    .cAbort(Lc,Msg,Tp) =>
      abortCont(Lc,Msg).C(Ctx,Stk,[]).
    .cWhere(Lc,E,C) =>
      compCond(C,expCont(E,Cont,ECont),ECont,ECont,Ctx,Stk).
    .cSusp(Lc,Fb,Ev,Tp) => 
      compExp(Fb,expCont(Ev,suspendCont(pushStack(Tp::ltipe,Stk),Cont),ECont),ECont,Ctx,Stk).
    .cResume(Lc,Fb,Ev,Tp) => 
      compExp(Fb,expCont(Ev,resumeCont(pushStack(Tp::ltipe,Stk),Cont),ECont),ECont,Ctx,Stk).
    .cTry(Lc,B,H,Tp) => valof{
      (CLb,CtxB) = defineExitLbl("Tr",Ctx);
      valis compExp(B,Cont,catchCont(CLb,()=>compExp(H,Cont,ECont,Ctx,Stk)),CtxB,Stk)
    }.
    .cValof(Lc,A,Tp) => 
      compAction(A,errorCont(Lc,"missing valis action"),Cont,ECont,Ctx,Stk).
    C where isCond(C) => valof{
      Nx = defineLbl("E",Ctx);
      Stk0 = pushStack(boolType::ltipe,Stk);
      (Stk1,Cde) = compCond(C,trueCont(jmpCont(Nx,Stk0)),
	falseCont(jmpCont(Nx,Stk0)),ECont,Ctx,Stk);
      valis Cont.C(Ctx,Stk1,Cde++[.iLbl(Nx)]) -- fix me
    }.
    C => valof{
      reportError("cannot compile expression $(C)",locOf(C));
      valis Cont.C(Ctx,Stk,[])
    }
  }
    
  -- Expressions are evaluated in reverse order
  compExps:(cons[cExp],Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compExps([],Cont,_,Ctx,Stk)=>Cont.C(Ctx,Stk,[]).
  compExps([Exp,..Es],Cont,ECont,Ctx,Stk)=>
    compExps(Es,expCont(Exp,Cont,ECont),ECont,Ctx,Stk).

  compVar:(option[locn],string,srcLoc,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compVar(Lc,_Nm,Loc,Cont,Ctx,Stk) => case Loc in {
    .argVar(Off,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdA(Off)]).
    .lclVar(Off,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdL(Off)]).
    .glbVar(Nm,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdG(Nm,Ctx.escape)]).
    .glbFun(Nm,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdC(symb(Nm))]).
  }
    
  compCond:(cExp,Cont,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compCond(C,Succ,Fail,ECont,Ctx,Stk) => case C in {
    .cTerm(_,"star.core#true",[],_) => Succ.C(Ctx,Stk,[]).
    .cTerm(_,"star.core#false",[],_) => Fail.C(Ctx,Stk,[]).
    .cCnj(Lc,L,R) => valof{
      valis compCond(L,condCont(R,Succ,Fail,ECont,Stk),Fail,ECont,Ctx,Stk)
    }.
    .cDsj(Lc,L,R) => valof{
      valis compCond(L,Succ,ctxCont(Ctx,condCont(R,Succ,Fail,ECont,Stk)),ECont,Ctx,Stk)
    }.
    .cNeg(Lc,R) =>
      compCond(R,ctxCont(Ctx,Fail),ctxCont(Ctx,Succ),ECont,Ctx,Stk).
    .cCnd(Lc,T,L,R) => valof{
      if traceCodegen! then
	logMsg("compiling conditional cond $(C)");
--      FC = splitCont(Lc,Ctx,Fail);
--      SC = splitCont(Lc,Ctx,Succ);
      valis compCond(T,condCont(L,Succ,Fail,ECont,Stk),condCont(R,Succ,Fail,ECont,Stk),
	ECont,Ctx,Stk)
    }.
    .cMatch(Lc,Ptn,Exp) => 
      compExp(Exp,ptnCont(Ptn,Succ,Fail,ECont),ECont,Ctx,Stk).
    Exp default => 
      compExp(Exp,ifCont(locOf(Exp),Stk,Succ,Fail),ECont,Ctx,Stk).
  }

  compAction:(aAction,Cont,Cont,Cont,codeCtx,stack) =>(stack,multi[assemOp]).
  compAction(A,ACont,Cont,ECont,Ctx,Stk) => case A in {
    .aNop(Lc) => ACont.C(Ctx,Stk,[]).
    .aSeq(Lc,L,R) => compAction(L,resetCont(Stk,actionCont(R,ACont,Cont,ECont)),Cont,ECont,Ctx,Stk).
    .aLbld(Lc,Lb,LbldA) => valof{
      Ctxl = (Ctx.brks<<-Ctx.brks[Lb->ctxCont(Ctx,resetCont(Stk,ACont))]);
      valis compAction(LbldA,ACont,Cont,ECont,Ctxl,Stk)
    }.
    .aBreak(Lc,Lb) => valof{
      if XCont?=Ctx.brks[Lb] then
	valis XCont.C(Ctx,Stk,[])
      else{
	reportError("unknown break label $(Lb)",Lc);
	valis ACont.C(Ctx,Stk,[])
      }
    }.
    .aValis(Lc,E) =>
      compExp(E,Cont,ECont,Ctx,Stk).
    .aThrow(Lc,E) =>
      compExp(E,ECont,abortCont(Lc,"double throw"),Ctx,Stk).
    .aPerf(Lc,E) => 
      compExp(E,resetCont(Stk,ACont),ECont,Ctx,Stk).
    .aDefn(Lc,P,E) =>
      compExp(E,ptnCont(P,ACont,abortCont(Lc,"define error"),ECont),ECont,Ctx,Stk).
    .aAsgn(Lc,P,E) =>
      compExp(E,expCont(P,asgnCont(ACont,Ctx,Stk),ECont),ECont,Ctx,Stk).
    .aCase(Lc,G,Cs,D) =>
      compCase(Lc,G,Cs,D,(Ac,C1,C2)=>actionCont(Ac,ACont,C1,C2),Cont,ECont,Ctx,Stk).
    .aUnpack(Lc,G,Cs) =>
      compCnsCase(Lc,G,Cs,(Ac,C1,C2)=>actionCont(Ac,ACont,C1,C2),Cont,ECont,Ctx,Stk).
    .aIftte(Lc,G,L,R) => valof{
      AC = splitCont(Lc,Ctx,ACont);
      CC = splitCont(Lc,Ctx,Cont);
      EE = splitCont(Lc,Ctx,ECont);
      valis compCond(G,actionCont(L,AC,CC,EE),
	resetCont(Stk,actionCont(R,AC,CC,EE)),ECont,Ctx,Stk)
    }.
    .aWhile(Lc,G,B) => valof{
      EE = splitCont(Lc,Ctx,ECont);
      Lp = defineLbl("Lp",Ctx);

      (Stk1,WCde) = compCond(G,actionCont(B,jmpCont(Lp,Stk),Cont,EE),ACont,EE,Ctx,Stk);
      valis (reconcileStack(Stk,Stk1),[.iLbl(Lp)]++WCde)
    }.
    .aLtt(Lc,.cId(Vr,VTp),Val,Bnd) => valof{
      (Off,Ctx1) = defineLclVar(Vr,VTp::ltipe,Ctx);
--      (EX,Ctx2) = defineExitLbl("_",Ctx1);
      valis compExp(Val,stoCont(Off,Stk,actionCont(Bnd,ACont,Cont,ECont)),ECont,Ctx1,Stk)
    }.
    .aTry(Lc,B,H) => valof{
      (CLb,CtxB) = defineExitLbl("aTr",Ctx);
      valis compAction(B,ACont,Cont,
	catchCont(CLb,()=>compAction(H,ACont,Cont,ECont,Ctx,Stk)),Ctx,Stk)
    }.
    .aRetire(Lc,F,E) => compExp(F,expCont(E,retireCont(ACont),
	abortCont(Lc,"retire")),ECont,Ctx,Stk).
    .aAbort(Lc,Msg) =>
      abortCont(Lc,Msg).C(Ctx,Stk,[]).
    _ => valof{
      reportError("cannot compile action $(A)",locOf(A));
      valis ACont.C(Ctx,Stk,[])
    }
  }.

  compCase:all e ~~ display[e] |: (option[locn],cExp,cons[cCase[e]],e,
    (e,Cont,Cont)=>Cont,
    Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compCase(Lc,Gv,Cases,Deflt,Comp,Cont,ECont,Ctx,Stk) => valof{
    if traceCodegen! then
      logMsg("compiling case, Stk $(Stk)");
    Nxt = defineLbl("CN",Ctx);
    DLbl = defineLbl("CD",Ctx);
    (Stk1,GCode) = compExp(Gv,jmpCont(Nxt,pushStack(typeOf(Gv)::ltipe,Stk)),ECont,Ctx,Stk);
    (Table,Max) = genCaseTable(Cases);
    OC = splitCont(Lc,Ctx,Cont);
    EC = splitCont(Lc,Ctx,ECont);
    (Stkc,DCode) = Comp(Deflt,OC,EC).C(Ctx,Stk,[]);

    (Stkb,TCode,CCode) = compCases(Table,0,Max,Comp,OC,jmpCont(DLbl,Stkc),EC,DLbl,Ctx,Stk1);
    Hgt = ^[|Stk|];

    valis (reconcileStack(Stkb,Stkc),
      GCode++[.iLbl(Nxt),.iCase(Max)]++TCode++CCode++[.iLbl(DLbl),.iRst(Hgt)]++DCode)
  }

  compCases:all e ~~ display[e] |: (cons[csEntry[e]],integer,integer,
    (e,Cont,Cont)=>Cont,Cont,Cont,Cont,assemLbl,codeCtx,stack) =>
    (stack,multi[assemOp],multi[assemOp]).
  compCases(Cs,Ix,Mx,Comp,Succ,Fail,ECont,Deflt,Ctx,Stk) => case Cs in {
    [] => valof{
      if Ix==Mx then
	valis (.none,[],[])
      else{
	(Stk1,TCde,Cde) = compCases([],Ix+1,Mx,Comp,Succ,Fail,ECont,Deflt,Ctx,Stk);
	valis (Stk1,TCde++[.iJmp(Deflt)],Cde)
      }
    }.
    [(Ix,Case),..Cases] => valof{
      Lb = defineLbl("CC",Ctx);
      (Stkb,TCde2,Cde2) = compCases(Cases,Ix+1,Mx,Comp,Succ,Fail,ECont,Deflt,Ctx,Stk);
      (Stkc,CCde) = compCaseBranch(Case,Comp,Succ,Fail,ECont,Ctx,Stk);
      valis (reconcileStack(Stkb,Stkc),TCde2++[iJmp(Lb)],Cde2++[.iLbl(Lb),..CCde])
    }.
    [(Iy,Case),..Cases] => valof{
      (Stk1,TCde,CCde) = compCases([(Iy,Case),..Cases],Ix+1,Mx,Comp,Succ,Fail,ECont,Deflt,Ctx,Stk);
      valis (Stk1,TCde++[iJmp(Deflt)],CCde)
    }
  }

  compCaseBranch:all e ~~ display[e] |: (cons[cCase[e]],
    (e,Cont,Cont)=>Cont,Cont,Cont,Cont,codeCtx,stack) =>
    (stack,multi[assemOp]).

  compCaseBranch(Cs,Comp,Succ,Fail,ECont,Ctx,Stk) => case Cs in {
    [(Lc,Ptn,Exp)] => compPttrn(Ptn,Comp(Exp,Succ,ECont),Fail,ECont,Ctx,Stk).
    [(Lc,Ptn,Exp),..More] => valof{
      Fl = defineLbl("CF",Ctx);
      VLb = defineLbl("CN",Ctx);
      Vr = genSym("__");
      (Off,Ctx1) = defineLclVar(Vr,typeOf(Ptn)::ltipe,Ctx);
      (Stkb,RlCde) = compPttrn(Ptn,Comp(Exp,Succ,ECont),jmpCont(Fl,Stk),ECont,Ctx1,Stk);
      (Stkc,AltCde) = compMoreCase(More,Off,Comp,Succ,Fail,ECont,Ctx,Stk);
      valis (reconcileStack(Stkb,Stkc),[.iTL(Off)]++RlCde++[.iLbl(Fl)]++AltCde)
    }
  }

  compMoreCase:all e ~~ (cons[(option[locn],cExp,e)],integer,(e,Cont,Cont)=>Cont,
    Cont,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compMoreCase(Cs,Off,Comp,Succ,Fail,ECont,Ctx,Stk) => case Cs in {
    [] => Fail.C(Ctx,Stk,[]).
    [(Lc,Ptn,Exp),..More] => valof{
      Fl = defineLbl("CM",Ctx);
      (Stk2,RlCde) = compPttrn(Ptn,Comp(Exp,Succ,ECont),jmpCont(Fl,Stk),ECont,Ctx,Stk);
      (Stk3,RstCde) = compMoreCase(More,Off,Comp,Succ,Fail,ECont,Ctx,Stk);
      valis (reconcileStack(Stk2,Stk3),[.iLdL(Off)]++RlCde++[.iLbl(Fl)]++RstCde)
    }
  }
  
  compCnsCase:all e ~~ (option[locn],cExp,cons[cCase[e]],(e,Cont,Cont)=>Cont,
    Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compCnsCase(_,Gv,Cs,Comp,Cont,ECont,Ctx,Stk) => case Cs in {
    [(Lc,Ptn,Exp)] => 
      compExp(Gv,
	ptnCont(Ptn,Comp(Exp,Cont,ECont),
	  abortCont(Lc,"match error"),ECont),ECont,Ctx,Stk).
    Cases default =>
      compExp(Gv,cnsCaseCont(Cases,Comp,Cont,ECont),ECont,Ctx,Stk).
  }

  cnsCaseCont:all e ~~ (cons[cCase[e]],(e,Cont,Cont)=>Cont,Cont,Cont) => Cont.
  cnsCaseCont(Cases,Comp,Cont,ECont) => cont{
    C(Ctx,AStk,GCde) => valof{
      (Stk2,JCde,CCde) = compCnsCases(Cases,Comp,Cont,ECont,Ctx,AStk);
    
      valis (Stk2,GCde++[.iIndxJmp([|Cases|])]++JCde++CCde)
    }
  }

  compCnsCases:all e ~~ (cons[cCase[e]],(e,Cont,Cont)=>Cont,Cont,Cont,codeCtx,stack) =>
    (stack,multi[assemOp],multi[assemOp]).
  compCnsCases(Cs,Comp,Succ,ECont,Ctx,Stk) => case Cs in {
    [] => (Stk,[],[]).
    [(Lc,Ptn,Exp),..Cases] => valof{
      Lb = cseLbl(Ptn,Ctx);
      (Stk2a,TCde2,Cde2) = compCnsCases(Cases,Comp,Succ,ECont,Ctx,Stk);
      (Stk3a,CCde) = compPtn(Ptn,Comp(Exp,Succ,ECont),
	abortCont(Lc,"match error"),ECont,Ctx,Stk);
      valis (reconcileStack(Stk2a,Stk3a),[iJmp(Lb),..TCde2],Cde2++[.iLbl(Lb),..CCde])
    }
  }

  cseLbl(.cTerm(_,Nm,_,_),Ctx) => defineLbl(Nm,Ctx).
  cseLbl(_,Ctx) => defineLbl("L",Ctx).

  all e ~~ csEntry[e] ~> (integer,cons[cCase[e]]).

  genCaseTable(Cases) where Mx.=nextPrime(size(Cases)) =>
    (sortCases(caseHashes(Cases,Mx)),Mx).

  caseHashes:all e ~~ (cons[cCase[e]],integer)=>cons[(option[locn],cExp,integer,e)].
  caseHashes(Cases,Mx) => (Cases//((Lc,Pt,Ex))=>(Lc,Pt,caseHash(Pt)%Mx,Ex)).

  caseHash:(cExp)=>integer.
  caseHash(E) => case E in {
    .cVar(_,_) => 0.
    .cInt(_,Ix) => Ix.
    .cBig(_,Bx) => hash(Bx).
    .cFloat(_,Dx) => hash(Dx).
    .cChar(_,Cx) => hash(Cx).
    .cString(_,Sx) => hash(Sx).
    .cTerm(_,Nm,Args,_) => size(Args)*37+hash(Nm).
    .cWhere(_,P,_) => caseHash(P).
  }.

  sortCases(Cases) => mergeDuplicates(sort(Cases,((_,_,H1,_),(_,_,H2,_))=>H1<H2)).

  mergeDuplicates:all e ~~ (cons[(option[locn],cExp,integer,e)])=>cons[csEntry[e]].
  mergeDuplicates([])=>[].
  mergeDuplicates([(Lc,Pt,Hx,Ex),..M]) where (D,Rs).=mergeDuplicate(M,Hx,[]) =>
    [(Hx,[(Lc,Pt,Ex),..D]),..mergeDuplicates(Rs)].

  mergeDuplicate:all e ~~ (cons[(option[locn],cExp,integer,e)],integer,cons[(option[locn],cExp,e)]) => (cons[(option[locn],cExp,e)],cons[(option[locn],cExp,integer,e)]).
  mergeDuplicate([(Lc,Pt,Hx,Ex),..M],Hx,SoFar) =>
    mergeDuplicate(M,Hx,SoFar++[(Lc,Pt,Ex)]).
  mergeDuplicate(M,_,SoFar) default => (SoFar,M).

  compPttrn:(cExp,Cont,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPttrn(Ptn,Succ,Fail,ECont,Ctx,Stk) => compPtn(Ptn,Succ,Fail,ECont,Ctx,Stk).
  
  compPtn:(cExp,Cont,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtn(Ptn,Succ,Fail,ECont,Ctx,Stk) => case Ptn in {
    .cVar(Lc,.cId(Vr,Tp)) => valof{
      if Loc ?= locateVar(Vr,Ctx) then 
	valis compPtnVar(Lc,Vr,Loc,Succ,Ctx,dropStack(Stk))
      else{
	LTp = Tp::ltipe;
	(Off,Ctx1) = defineLclVar(Vr,LTp,Ctx);
	valis compPtnVar(Lc,Vr,lclVar(Off,LTp),Succ,Ctx1,dropStack(Stk))
      }
    }.
    .cVoid(Lc,_) =>
      Succ.C(Ctx,dropStack(Stk),[.iDrop]).
    .cAnon(Lc,_) =>
      Succ.C(Ctx,dropStack(Stk),[.iDrop]).
    .cTerm(Lc,Nm,Args,Tp) => valof{
      Stk0 = dropStack(Stk);
      
      valis compPtnArgs(Args,unpackCont(Lc,tLbl(Nm,size(Args)),Succ,Fail,Stk0),
	Fail,ECont,Ctx,loadStack(Args//(A)=>(typeOf(A)::ltipe),Stk0))
    }.
    .cWhere(Lc,Ptrn,Cond) =>
      compPtn(Ptrn,condCont(Cond,Succ,Fail,ECont,dropStack(Stk)),Fail,ECont,Ctx,Stk).
    _ default => valof{
      if isGround(Ptn) then{
	Flb = defineLbl("Tst",Ctx);
	Stk0 = dropStack(Stk); 
	(Stk1,FCde) = Fail.C(Ctx,Stk0,[]);
	(Stk2,SCde) = Succ.C(Ctx,Stk0,[]);
	valis (reconcileStack(Stk1,Stk2),
	  [.iLdC(Ptn::data),ptnCmp(Ptn,Flb)]++SCde++[.iLbl(Flb),..FCde])
      } else{
	reportError("uncompilable pattern $(Ptn)",locOf(Ptn));
	valis Succ.C(Ctx,Stk,[])
      }
    }.
  }

  ptnCmp(Ptn,Lb) => case Ptn in {
    .cInt(_,Ix) => .iICmp(Lb).
    .cChar(_,Cx) => .iICmp(Lb).
    .cFloat(_,Dx) => .iFCmp(Lb).
    _ => .iCmp(Lb)
  }.

  compPtnVar:(option[locn],string,srcLoc,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtnVar(Lc,Nm,.lclVar(Off,Tp),Cont,Ctx,Stk) => Cont.C(Ctx,Stk,[.iStL(Off)]).

  compPtnArgs:(cons[cExp],Cont,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtnArgs(Es,Succ,Fail,ECont,Ctx,Stk) => case Es in {
    [] => Succ.C(Ctx,Stk,[]).
    [A,..As] =>
      compPtn(A,argsPtnCont(As,Succ,Fail,ECont),Fail,ECont,Ctx,Stk).
  }

  argsPtnCont(As,Succ,Fail,ECont) => cont{
    C(Ctx,Stk,Cde) => compPtnArgs(As,propCode(Cde,Succ),Fail,ECont,Ctx,Stk).
  }

  -- continuations

  Cont ::= cont{
    C:(codeCtx,stack,multi[assemOp])=>(stack,multi[assemOp]).
  }.

  propCode:(multi[assemOp],Cont)=>Cont.
  propCode(Pre,Cont) => cont{
    C(Ctx,Stk,Cde) => Cont.C(Ctx,Stk,Pre++Cde)
  }

  allocCont:(termLbl,stack,Cont) => Cont.
  allocCont(Lbl,Stk,Cont) => cont{
    C(Ctx,_AStk,Cde) => Cont.C(Ctx,Stk,Cde++[.iAlloc(Lbl),frameIns(Stk)])
  }.

  escapeCont:(string,stack,Cont) => Cont.
  escapeCont(Es,Stk,Cont) => cont{
    C(Ctx,_Stk,Cde) => Cont.C(Ctx,Stk,Cde++[.iEscape(Es,Ctx.escape),frameIns(Stk)]).
  }

  intrinsicCont:(assemOp,boolean,stack,Cont) => Cont.
  intrinsicCont(I,Frm,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,Stk,Cde++[I]++(Frm??[frameIns(Stk)]||[])).
  }

  callCont:(termLbl,stack,Cont) => Cont.
  callCont(Lbl,Stk,Cont) => cont{
    C(Ctx,_AStk,Cde) => Cont.C(Ctx,Stk,Cde++[.iCall(Lbl,Ctx.escape),frameIns(Stk)]).
  }

  oclCont:(integer,stack,Cont) => Cont.
  oclCont(Ar,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,Stk,Cde++[.iOCall(Ar,Ctx.escape),frameIns(Stk)]).
  }

  retCont:Cont.
  retCont = cont{
    C(_,_,Cde) => (.none,Cde++[.iRet])
  }

  glbRetCont:(string)=>Cont.
  glbRetCont(Nm) => cont{
    C(_,_,Cde) => (.none,Cde++[.iTG(Nm),.iRtG])
  }

  jmpCont:(assemLbl,stack)=>Cont.
  jmpCont(Lbl,Stk) => cont{
    C(Ctx,_Stk1,Cde) => (Stk,Cde++[.iJmp(Lbl)]).
  }

  stoCont:(integer,stack,Cont) => Cont.
  stoCont(Off,Stk,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.iStL(Off)])
  }

  ifCont:(option[locn],stack,Cont,Cont) => Cont.
  ifCont(Lc,Stk,Succ,Fail) => cont{
    C(Ctx,_,Cde) => valof{
      Flb = defineLbl("F",Ctx);
      (SStk,SCde) = Succ.C(Ctx,Stk,[.iIfNot(Flb)]);
      (FStk,FCde) = Fail.C(Ctx,Stk,[]);
      
      valis (reconcileStack(SStk,FStk),Cde++SCde++[.iLbl(Flb),..FCde])
    }
  }

  unpackCont:(option[locn],termLbl,Cont,Cont,stack) => Cont.
  unpackCont(Lc,Lbl,Succ,Fail,Stk0) => cont{
    C(Ctx,Stk,Cde) => valof{
      Flb = defineLbl("U",Ctx);
      (Stk1,FCde) = Fail.C(Ctx,Stk0,[]);
      (Stk2,SCde) = Succ.C(Ctx,Stk,[]);
      valis (reconcileStack(Stk1,Stk2),
	[.iUnpack(Lbl,Flb),..Cde]++SCde++[.iLbl(Flb),..FCde])
    }
  }

  suspendCont:(stack,Cont) => Cont.
  suspendCont(Stk,Cont) => cont{
    C(Ctx,_AStk,Cde) => Cont.C(Ctx,Stk,Cde++[.iSuspend,frameIns(Stk)]).
  }

  resumeCont:(stack,Cont) => Cont.
  resumeCont(Stk,Cont) => cont{
    C(Ctx,_AStk,Cde) => Cont.C(Ctx,Stk,Cde++[.iResume,frameIns(Stk)]).
  }

  retireCont:(Cont) => Cont.
  retireCont(Cont) => cont{
    C(Ctx,_AStk,Cde) => Cont.C(Ctx,.none,Cde++[.iRetire]).
  }

  nullCont = cont{
    C(_,Stk,Cde) => (Stk,Cde).
  }

  ctxCont:(codeCtx,Cont) => Cont.
  ctxCont(Ctx,Cont) => cont{
    C(_,Stk,Cde) => Cont.C(Ctx,Stk,Cde)
  }

  bothCont:(Cont,Cont) => Cont.
  bothCont(L,R) => cont{
    C(Ctx,Stk,Cde) => valof{
      (Stk1,C1) = L.C(Ctx,Stk,Cde);
      valis R.C(Ctx,Stk1,C1)
    }.
  }

  falseCont:(Cont) => Cont.
  falseCont(Cont) => cont{
    C(Cxt,Stk,Cde) => Cont.C(Cxt,pushStack(.bool,Stk),Cde++[.iLdC(falseEnum)]).
  }

  trueCont:(Cont) => Cont.
  trueCont(Cont) => cont{
    C(Cxt,Stk,Cde) => Cont.C(Cxt,pushStack(.bool,Stk),Cde++[.iLdC(trueEnum)]).
  }

  expCont:(cExp,Cont,Cont) => Cont.
  expCont(Exp,Cont,ECont) => cont{
    C(Ctx,Stk,Cde) => valof{
      (Stk1,OCde) = compExp(Exp,Cont,ECont,Ctx,Stk);
      valis (Stk1,Cde++OCde)
    }
  }

  condCont:(cExp,Cont,Cont,Cont,stack) => Cont.
  condCont(Cond,Succ,Fail,ECont,Stk) => cont{
    C(Ctx,XStk,Cde) => valof{
      (Stk1,SSCde) = resetStack([|Stk|],XStk);
      (Stk2,CCde) = compCond(Cond,Succ,Fail,ECont,Ctx,Stk1);
      valis (Stk2,Cde++SSCde++CCde)
    }
  }

  ptnCont:(cExp,Cont,Cont,Cont) => Cont.
  ptnCont(Ptn,Succ,Fail,ECont) => cont{
    C(Ctx,Stk,Cde) => valof{
      (Stk1,Cde1) = compPttrn(Ptn,Succ,Fail,ECont,Ctx,Stk);
      valis (Stk1,Cde++Cde1)
    }
  }

  nthCont:(integer,Cont,stack)=>Cont.
  nthCont(Ix,Cont,Stk) => cont{
    C(Ctx,_,Cde) => 
      Cont.C(Ctx,Stk,Cde++[.iNth(Ix)])
  }
      
  setNthCont:(integer,Cont,stack)=>Cont.
  setNthCont(Ix,Cont,Stk) => cont{
    C(Ctx,_SS,Cde) => Cont.C(Ctx,Stk,Cde++[.iStNth(Ix)]).
  }

  catchCont:all e ~~ (assemLbl,()=>(stack,multi[assemOp])) => Cont.
  catchCont(CLb,HComp) => cont{
    C(_,_,Cde) => valof{
      (SStk,HCde) = HComp();
      valis (SStk,Cde++[.iLbl(CLb)]++HCde)
    }
  }

  asgnCont:(Cont,codeCtx,stack) => Cont.
  asgnCont(ACont,Ctx,Stk) => cont{
    C(_,_,Cde) => 
      ACont.C(Ctx,Stk,Cde++[.iAssign])
  }

  abortCont:(option[locn],string) => Cont.
  abortCont(.some(Lc),Msg) => cont{
    C(_,_,Cde) => (.none,Cde++[.iLdC(Lc::data),.iLdC(strg(Msg)),.iAbort]).
  }

  errorCont:(option[locn],string) => Cont.
  errorCont(Lc,Msg) => cont{
    C(_,Stk,Cde) => valof{
      if _?=Stk then
	reportError(Msg,Lc);
      valis (Stk,Cde)
    }
  }

  actionCont:(aAction,Cont,Cont,Cont) => Cont.
  actionCont(A,ACont,Cont,ECont) => cont{
    C(Ctx,Stk,Cde) => valof{
      (SStk,SCde) = compAction(A,ACont,Cont,ECont,Ctx,Stk);
      valis (SStk,Cde++SCde)
    }
  }

  splitCont:(option[locn],codeCtx,Cont) => Cont.
  splitCont(Lc,Ctx0,Cnt) => valof{
    Lb = defineLbl("Splt",Ctx0);      -- Create a new label
    tStk = ref .none;
    triggered = ref .false;
    
    valis cont{
      C(Ctx,Stk,Cde) => valof{
	if triggered! then{
	  triggered := .true;
	  valis (tStk!,Cde++[.iJmp(Lb)])
	} else{
	  triggered := .true;
	  (rStk,rCde) = Cnt.C(Ctx,Stk,Cde++[.iLbl(Lb)]);
	  tStk := rStk;
	  valis (rStk,rCde);
	}
      }
    }
  }

  reconcileStack:(stack,stack)=>stack.
  reconcileStack(S1,S2) => case S1 in {
    .none => S2.
    .some(Sl) => case S2 in {
      .none => S1.
      .some(Sr) => valof{
	if Sl==Sr then
	  valis .some(Sl)
	else{
	  reportTrap("misaligned stacks $(S1) vs $(S2)");
	  valis .some(Sl)
	}
      }
    }
  }

  resetStack:(option[integer],stack) => (stack,multi[assemOp]).
  resetStack(.some(Dp),.some(Stk)) =>
    case [|Stk|] in {
      Dp => (.some(Stk),[]).
      Dp1 where Dp == Dp1-1 => (tail(Stk),[.iDrop]).
      Dp1 where Dp1>Dp => (.some(popTo(Stk,Dp)),[.iRst(Dp)]).
      Dp1 default => valof{	
	reportTrap("invalid stack height in $(Stk)\:$(Dp1)~$(Dp)");
	valis (.some(Stk),[])
      }
    }.
  resetStack(_,.none) => (.none,[]).

  resetCont:(stack,Cont) => Cont.
  resetCont(Stk,Cont) => cont{
    C(Ctx,XStk,Cde) => valof{
      (NStk,SCde) = resetStack([|Stk|],XStk);
      valis Cont.C(Ctx,NStk,Cde++SCde)
    }
  }

  frameIns:(stack)=>assemOp.
  frameIns(.some(Stk)) => .iFrame(tplTipe(Stk)).

  locateVar:(string,codeCtx)=>option[srcLoc].
  locateVar(Nm,Ctx) => Ctx.vars[Nm].

  defineLclVar:(string,ltipe,codeCtx) => (integer,codeCtx).
  defineLclVar(Nm,Tp,Ctx) => valof{
    hwm = Ctx.hwm;
    
    Off = hwm!+1;
    hwm := Off;

    valis (Off,Ctx.vars<<-Ctx.vars[Nm->lclVar(Off,Tp)])
  }

  argVars:(cons[cId],map[string,srcLoc],integer) => map[string,srcLoc].
  argVars([],Mp,_)=>Mp.
  argVars([.cId(Nm,Tp),..As],Vars,Ix) =>
    argVars(As,Vars[Nm->argVar(Ix,Tp::ltipe)],Ix+1).
  argVars([_,..As],Map,Ix) => argVars(As,Map,Ix+1).

  drop:all x,e ~~ stream[x->>e] |: (x,integer)=>x.
  drop(S,0)=>S.
  drop([_,..S],N)=>drop(S,N-1).

  trimStack:all x, e ~~ stream[x->>e],sizeable[x] |: (option[x],integer)=>option[x].
  trimStack(.some(L),N) =>.some(drop(L,size(L)-N)).
  trimStack(.none,_) => .none.

  dropStack(.none) => .none.
  dropStack(.some([_,..Stk])) => .some(Stk).

  srcLoc ::= lclVar(integer,ltipe) |
    argVar(integer,ltipe) |
    glbVar(string,ltipe) |
    glbFun(termLbl,ltipe).

  codeCtx ::= codeCtx{
    vars : map[string,srcLoc].
    end : assemLbl.
    escape : assemLbl.
    lbls : ref integer.  
    min : integer.
    hwm : ref integer.
    brks : map[string,Cont]
  }

  stack ~> option[cons[ltipe]].

  emptyCtx:(map[string,srcLoc])=>codeCtx.
  emptyCtx(Glbs) => codeCtx{
    vars = Glbs.
    end = .al("$$").
    escape = .al("$$").
    lbls = ref 0.
    min = 0.
    hwm = ref 0.
    brks = {}.
  }

  defineLbl:(string,codeCtx)=>assemLbl.
  defineLbl(Pr,C) => valof{
    CurLbl = C.lbls!;
    C.lbls := CurLbl+1;
    valis al("#(Pr)$(CurLbl)")
  }

  defineExitLbl:(string,codeCtx) => (assemLbl,codeCtx).
  defineExitLbl(Pr,C) => valof{
    Lb = defineLbl(Pr,C);
    valis (Lb,C.escape<<-Lb)
  }

  pushStack:(ltipe,stack) => stack.
  pushStack(Tp,?Stk) => ?[Tp,..Stk].

  loadStack:(cons[ltipe],stack) => stack.
  loadStack(Tps,.some(Stk)) => .some(Tps++Stk).

  popTo:all e ~~ (cons[e],integer) => cons[e].
  popTo(LL,D) => let{.
    pop(Ls,Ix) => case Ls in {
      [] => [].
      [_,..L] where Ix>0 => pop(L,Ix-1).
      _ where Ix==0 => Ls
    }
  .} in pop(LL,[|LL|]-D).

  implementation display[codeCtx] => {
    disp(C) => "<C hwm:$(C.hwm!)>C>".
  }

  implementation display[srcLoc] => {
    disp(L) => case L in {
      .lclVar(Off,Tpe) => "lcl $(Off)\:$(Tpe)".
      .argVar(Off,Tpe) => "arg $(Off)\:$(Tpe)".
      .glbVar(Off,Tpe) => "glb $(Off)\:$(Tpe)".
      .glbFun(Off,Tpe) => "fun $(Off)\:$(Tpe)".
    }
  }
}
