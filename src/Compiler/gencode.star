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

  declGlobal(.varDec(_,_,Nm,Tp), Vrs) => Vrs[Nm->.glbVar(Nm,Tp::ltipe)].
  declGlobal(.funDec(_,_,Nm,Tp), Vrs) => Vrs[Nm->.glbVar(Nm,Tp::ltipe)].
  declGlobal(.accDec(_,_,_,Nm,Tp), Vrs) => Vrs[Nm->.glbVar(Nm,Tp::ltipe)].
  declGlobal(.updDec(_,_,_,Nm,Tp), Vrs) => Vrs[Nm->.glbVar(Nm,Tp::ltipe)].
  declGlobal(_,Vrs) => Vrs.

  localFuns:(cons[cDefn],map[string,srcLoc])=>map[string,srcLoc].
  localFuns(Defs,Vars) => foldRight(defFun,Vars,Defs).

  defFun(Def,Vrs) => case Def in {
    .fnDef(Lc,Nm,Tp,_,_) => Vrs[Nm->.glbFun(.tLbl(Nm,arity(Tp)),Tp::ltipe)].
    .vrDef(Lc,Nm,Tp,_) => Vrs[Nm->.glbVar(Nm,Tp::ltipe)].
    _ default => Vrs
  }
  
  compDefs:(cons[cDefn],map[string,srcLoc])=> cons[codeSegment].
  compDefs(Dfs,Glbs) => (Dfs//(D)=>genDef(D,Glbs)).

  genDef:(cDefn,map[string,srcLoc]) => codeSegment.
  genDef(Defn,Glbs) => case Defn in {
    .fnDef(Lc,Nm,Tp,Args,Val) => valof{
      if traceCodegen! then
	logMsg("compile $(.fnDef(Lc,Nm,Tp,Args,Val))");
      Ctx = emptyCtx(argVars(Args,Glbs,0));
      (_,AbortCde) = abortCont(Lc,"function: $(Nm)").C(Ctx,?[],[]);
      (_Stk,Code) = compExp(Val,.last,retCont,Ctx,.some([]));
      if traceCodegen! then
	logMsg("non-peep code is $((Code++[.iLbl(Ctx.escape),..AbortCde])::cons[assemOp])");
      Peeped = peepOptimize(([.iLocals(Ctx.hwm!),..Code]++[.iLbl(Ctx.escape),..AbortCde])::cons[assemOp]);
      if traceCodegen! then{
	logMsg("code is $(.func(.tLbl(Nm,size(Args)),.hardDefinition,Tp,Ctx.hwm!,Peeped))");
      };
      valis .func(.tLbl(Nm,size(Args)),.hardDefinition,Tp,Ctx.hwm!,Peeped)
    }
  | .vrDef(Lc,Nm,Tp,Val) => valof{
      if traceCodegen! then
	logMsg("compile global $(Nm)\:$(Tp) = $(Val))");
      Ctx = emptyCtx(Glbs);
      (_,AbortCde) = abortCont(Lc,"global: $(Nm)").C(Ctx,.none,[]);
      (_Stk,Code) = compExp(Val,.notLast,glbRetCont(Nm),Ctx,.some([]));

      if traceCodegen! then
	logMsg("non-peep code is $((Code++[.iLbl(Ctx.escape),..AbortCde])::cons[assemOp])");
      
      Peeped = peepOptimize(([.iLocals(Ctx.hwm!),..Code]++[.iLbl(Ctx.escape),..AbortCde])::cons[assemOp]);
      if traceCodegen! then
	logMsg("code is $(.global(.tLbl(Nm,0),Tp,Ctx.hwm!,Peeped))");
    
      valis .global(.tLbl(Nm,0),Tp,Ctx.hwm!,Peeped)
    }
  | .tpDef(Lc,Tp,TpRl,Index) => .tipe(Tp,TpRl,Index)
  | .lblDef(_Lc,Lbl,Tp,Ix) => .struct(Lbl,Tp,Ix)
  }

  compExp:(cExp,tailMode,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compExp(Exp,TM,Cont,Ctx,Stk) => case Exp in {
    E where isGround(E) =>
      Cont.C(Ctx,pushStack(typeOf(Exp)::ltipe,Stk),[.iLdC(Exp::data)])
    | .cVar(Lc,.cId(Vr,Tp)) => valof{
      if Loc?=locateVar(Vr,Ctx) then {
	valis compVar(Lc,Vr,Loc,Cont,Ctx,Stk)
      } else {
	reportError("cannot locate variable $(Vr)\:$(Tp)",Lc);
	valis Cont.C(Ctx,pushStack(Tp::ltipe,Stk),[.iLdV])
      }
    }
    | .cVoid(Lc,Tp) => Cont.C(Ctx,pushStack(Tp::ltipe,Stk),[.iLdV])
    | .cTerm(_,Nm,Args,Tp) => valof{
      valis compExps(Args,allocCont(.tLbl(Nm,size(Args)),pushStack(Tp::ltipe,Stk),Cont),Ctx,Stk)
    }
    | .cECall(Lc,Op,Args,Tp) where (_,Ins,Frm)?=intrinsic(Op) =>
      compExps(Args,intrinsicCont(Ins,Frm,pushStack(Tp::ltipe,Stk),Cont),Ctx,Stk)
    | .cECall(Lc,Es,Args,Tp) =>
      compExps(Args,escapeCont(Es,pushStack(Tp::ltipe,Stk),Cont),Ctx,Stk)
    | .cCall(Lc,Nm,Args,Tp) =>
      compExps(Args,callCont(.tLbl(Nm,[|Args|]),TM,pushStack(Tp::ltipe,Stk),Cont),Ctx,Stk)
    | .cOCall(Lc,Op,Args,Tp) => 
      compExps(Args,expCont(Op,.notLast,oclCont([|Args|]+1,TM,pushStack(Tp::ltipe,Stk),Cont)),Ctx,Stk)
    | .cNth(Lc,E,Ix,Tp) =>
      compExp(E,.notLast,nthCont(Ix,Cont,pushStack(Tp::ltipe,Stk)),Ctx,Stk)
    | .cSetNth(Lc,R,Ix,V) => compExp(R,.notLast,expCont(V,.notLast,setNthCont(Ix,Cont,Stk)),Ctx,Stk)
    | .cThunk(Lc,Lam,_) => compExp(Lam,.notLast,thunkCont(Stk,Cont),Ctx,Stk)
    | .cThGet(Lc,Thnk,Tp) =>
      compExp(Thnk,.notLast,thGetCont(pushStack(Tp::ltipe,Stk),Ctx.escape,Cont),Ctx,Stk)
    | .cSeq(_,L,R) =>
      compExp(L,.notLast,resetCont(Stk,expCont(R,TM,Cont)),Ctx,Stk)
    | .cCnd(Lc,G,L,R) => valof{
      Ctx1 = glCtx(Ctx,G);
      
      LC = splitCont(Lc,Ctx,expCont(L,TM,Cont));
      RC = splitCont(Lc,Ctx,ctxCont(Ctx,ctxCont(Ctx,expCont(R,TM,Cont))));
      
      valis compCond(G,TM,LC,RC,Ctx,Stk)
    }
    | .cCase(Lc,Gov,Cases,Deflt,_Tp) =>
      compCase(Lc,Gov,Cases,Deflt,(E,C1)=>expCont(E,TM,C1),Cont,Ctx,Stk)
    | .cUnpack(Lc,Gov,Cases,_Tp) => compCnsCase(Lc,Gov,Cases,(Ac,C1)=>expCont(Ac,TM,C1),Cont,Ctx,Stk)
    | .cLtt(Lc,.cId(Vr,VTp),Val,Bnd) => valof{
      valis compExp(Val,.notLast,stoCont(Vr,VTp::ltipe,Stk,expCont(Bnd,TM,Cont)),Ctx,Stk)
    }
    | .cAbort(Lc,Msg,Tp) => abortCont(Lc,Msg).C(Ctx,Stk,[])
    | .cSusp(Lc,Fb,Ev,Tp) => 
      compExp(Fb,.notLast,expCont(Ev,.notLast,suspendCont(pushStack(Tp::ltipe,Stk),Cont)),Ctx,Stk)
    | .cResume(Lc,Fb,Ev,Tp) => 
      compExp(Fb,.notLast,expCont(Ev,.notLast,resumeCont(pushStack(Tp::ltipe,Stk),Cont)),Ctx,Stk)
    | .cTry(Lc,B,.cVar(_,.cId(Th,ThTp)),.cVar(_,.cId(Er,ETp)),H,Tp) => valof{
      (CLb,Ctx0) = defineExitLbl("Tr",Ctx);
      Blk = defineLbl("H",Ctx);
      (TOff,CtxB) = defineLclVar(Th,ThTp::ltipe,Ctx0);
      (EOff,Ctx1) = defineLclVar(Er,ETp::ltipe,Ctx);
      
      (Stk1,BCde) = compExp(B,.notLast,Cont,CtxB,Stk); -- critical: body of try is not tail rec
      (Stk2,HCde) = compExp(H,TM,Cont,Ctx1,Stk);

      if ~reconcileable(Stk1,Stk2) then
	reportError("cannot reconcile try exp $(B) with handler $(H)",Lc);

      valis (reconcileStack(Stk1,Stk2),[.iTry(Blk),.iStL(TOff)]++BCde++[.iLbl(Blk),.iTL(EOff)]++HCde)
    }
    | .cThrow(Lc,T,E,_) => compExp(E,.notLast,expCont(T,.notLast,throwCont),Ctx,Stk)
    | .cValof(Lc,A,Tp) => compAction(A,TM,abortCont(Lc,"missing valis action"),Cont,Ctx,Stk)
    |  C where isCond(C) => valof{
      Nx = defineLbl("E",Ctx);
      Stk0 = pushStack(boolType::ltipe,Stk);
      (Stk1,Cde) = compCond(C,.notLast,trueCont(jmpCont(Nx,Stk0)),falseCont(jmpCont(Nx,Stk0)),
	glCtx(Ctx,C),Stk);
      valis Cont.C(Ctx,Stk1,Cde++[.iLbl(Nx)]) -- fix me
    }
    | C => valof{
      reportError("cannot compile expression $(C)",locOf(C));
      valis Cont.C(Ctx,Stk,[])
    }
  }
    
  -- Expressions are evaluated in reverse order
  compExps:(cons[cExp],Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compExps([],Cont,Ctx,Stk)=>Cont.C(Ctx,Stk,[]).
  compExps([Exp,..Es],Cont,Ctx,Stk)=>
    compExps(Es,ctxCont(Ctx,expCont(Exp,.notLast,Cont)),Ctx,Stk).

  compVar:(option[locn],string,srcLoc,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compVar(Lc,_Nm,Loc,Cont,Ctx,Stk) => case Loc in {
    .argVar(Off,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdA(Off)]).
    .lclVar(Off,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdL(Off)]).
    .glbVar(Nm,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdG(Nm)]).
    .glbFun(Nm,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdC(.symb(Nm))]).
  }
    
  compCond:(cExp,tailMode,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compCond(C,TM,Succ,Fail,Ctx,Stk) => case C in {
    .cTerm(_,"star.core#true",[],_) => Succ.C(Ctx,Stk,[])
    | .cTerm(_,"star.core#false",[],_) => Fail.C(Ctx,Stk,[])
    | .cCnj(Lc,L,R) => valof{
      valis compCond(L,.notLast,condCont(R,TM,Succ,Fail,Stk),Fail,Ctx,Stk)
    }
    | .cDsj(Lc,L,R) => valof{
      SC = splitCont(Lc,Ctx,Succ);
      
      Ctxa = dsjCtx(Ctx,L,R);
      
      valis compCond(L,TM,SC,ctxCont(Ctxa,condCont(R,TM,SC,Fail,Stk)),Ctxa,Stk)
    }
    | .cNeg(Lc,R) => compCond(R,TM,ctxCont(Ctx,Fail),ctxCont(Ctx,Succ),Ctx,Stk)
    | .cCnd(Lc,T,L,R) => valof{
      if traceCodegen! then
	logMsg("compiling conditional cond $(C)");
      SC = splitCont(Lc,Ctx,Succ);
      FC = splitCont(Lc,Ctx,Fail);
      
      Ctxa = dsjCtx(glCtx(Ctx,T),L,R); -- Make sure that common variables are kept in same place
      
      valis compCond(T,.notLast,condCont(L,TM,SC,FC,Stk),condCont(R,TM,SC,FC,Stk),Ctxa,Stk)
    }
    | .cMatch(Lc,Ptn,Exp) => compExp(Exp,.notLast,ptnCont(Ptn,Succ,Fail),Ctx,Stk)
    | Exp default => compExp(Exp,.notLast,ifCont(locOf(Exp),Stk,Succ,Fail),Ctx,Stk)
  }

  compAction:(aAction,tailMode,Cont,Cont,codeCtx,stack) =>(stack,multi[assemOp]).
  compAction(A,TM,ACont,Cont,Ctx,Stk) => case A in {
    .aNop(Lc) => ACont.C(Ctx,Stk,[])
    | .aSeq(Lc,L,R) => compAction(L,.notLast,resetCont(Stk,actionCont(R,TM,ACont,Cont)),Cont,Ctx,Stk)
    | .aLbld(Lc,Lb,LbldA) => valof{
      Ctxl = (Ctx.brks<<-Ctx.brks[Lb->ctxCont(Ctx,resetCont(Stk,ACont))]);
      valis compAction(LbldA,TM,ACont,Cont,Ctxl,Stk)
    }
    | .aBreak(Lc,Lb) => valof{
      if XCont?=Ctx.brks[Lb] then{
	(_,Cde) = XCont.C(Ctx,Stk,[]); -- Special handling for breaks, we can't relate their stack
	valis (.none,Cde)
      }
      else{
	reportError("unknown break label $(Lb)",Lc);
	valis ACont.C(Ctx,Stk,[])
      }
    }
    | .aValis(Lc,E) => compExp(E,TM,Cont,Ctx,Stk)
    | .aThrow(Lc,T,E) => compExp(E,.notLast,expCont(T,.notLast,throwCont),Ctx,Stk)
    | .aPerf(Lc,E) => compExp(E,TM,resetCont(Stk,ACont),Ctx,Stk)
    | .aDefn(Lc,P,E) => compExp(E,.notLast,ptnCont(P,ACont,abortCont(Lc,"define error")),Ctx,Stk)
    | .aAsgn(Lc,P,E) => compExp(E,.notLast,expCont(P,.notLast,asgnCont(ACont,Ctx,Stk)),Ctx,Stk)
    | .aSetNth(Lc,T,Ix,E) => compExp(T,.notLast,expCont(E,.notLast,setNthCont(Ix,ACont,Stk)),Ctx,Stk)
    | .aCase(Lc,G,Cs,D) => compCase(Lc,G,Cs,D,(Ac,C1)=>actionCont(Ac,TM,ACont,C1),Cont,Ctx,Stk)
    | .aUnpack(Lc,G,Cs) => compCnsCase(Lc,G,Cs,(Ac,C1)=>actionCont(Ac,TM,ACont,C1),Cont,Ctx,Stk)
    | .aIftte(Lc,G,L,R) => valof{
      AC = splitCont(Lc,Ctx,ACont);
      CC = splitCont(Lc,Ctx,Cont);
      valis compCond(G,.notLast,actionCont(L,TM,AC,CC),
	resetCont(Stk,actionCont(R,TM,AC,CC)),glCtx(Ctx,G),Stk)
    }
    | .aWhile(Lc,G,B) => valof{
      Lp = defineLbl("Lp",Ctx);
      Tst = defineLbl("Tst",Ctx);
      Ex = defineLbl("Ex",Ctx);
      GCtx = glCtx(Ctx,G);

      (Stk1,WCde) = compCond(G,.notLast,jmpCont(Lp,Stk),jmpCont(Ex,Stk),GCtx,Stk);
      if traceCodegen! then
	logMsg("While test code $(WCde)");

      (Stk0,BCde) = compAction(B,.notLast,jmpCont(Tst,Stk),Cont,GCtx,Stk);

      valis ACont.C(Ctx,reconcileStack(Stk0,Stk1),
	[.iJmp(Tst),.iLbl(Lp)]++BCde++[.iLbl(Tst)]++WCde++[.iLbl(Ex)])
    }
    |.aLtt(Lc,.cId(Vr,VTp),Val,Bnd) => valof{
      valis compExp(Val,.notLast,stoCont(Vr,VTp::ltipe,Stk,actionCont(Bnd,TM,ACont,Cont)),Ctx,Stk)
    }
    | .aTry(Lc,B,.cVar(_,.cId(Th,ThTp)), .cVar(_,.cId(Er,ETp)),H) => valof{
      Blk = defineLbl("H",Ctx);
      (TOff,Ctx0) = defineLclVar(Th,ETp::ltipe,Ctx);
      (EOff,Ctx1) = defineLclVar(Er,ETp::ltipe,Ctx);
      AC = splitCont(Lc,Ctx,ACont);
      CC = splitCont(Lc,Ctx,Cont);
      
      (Stk1,BCde) = compAction(B,.notLast,AC,CC,Ctx0,Stk);
      (Stk2,CCde) = compAction(H,TM,AC,CC,Ctx1,Stk);

      if ~reconcileable(Stk1,Stk2) then
	reportError("cannot reconcile try body $(B) with handler $(H)",Lc);
      
      valis (reconcileStack(Stk1,Stk2),[.iTry(Blk),.iStL(TOff)]++BCde++[.iLbl(Blk),.iTL(EOff)]++CCde)
    }
    | .aRetire(Lc,F,E) => compExp(F,.notLast,expCont(E,.notLast,retireCont(.none)),Ctx,Stk)
    | .aAbort(Lc,Msg) => abortCont(Lc,Msg).C(Ctx,Stk,[])
    | _ default => valof{
      reportError("cannot compile action $(A)",locOf(A));
      valis ACont.C(Ctx,Stk,[])
    }
  }

  compCase:all e ~~ display[e] |:
    (option[locn],cExp,cons[cCase[e]],e,(e,Cont)=>Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compCase(Lc,Gv,Cases,Deflt,Comp,Cont,Ctx,Stk) => valof{
    if traceCodegen! then
      logMsg("compiling case, Stk $(Stk)");
    Nxt = defineLbl("CN",Ctx);
    DLbl = defineLbl("CD",Ctx);
    (Stk1,GCode) = compExp(Gv,.notLast,jmpCont(Nxt,pushStack(typeOf(Gv)::ltipe,Stk)),Ctx,Stk);
    (Table,Max) = genCaseTable(Cases);
    OC = splitCont(Lc,Ctx,Cont);
    (Stkc,DCode) = Comp(Deflt,OC).C(Ctx,Stk,[]);

    (Stkb,TCode,CCode) = compCases(Table,0,Max,Comp,OC,jmpCont(DLbl,Stkc),DLbl,Ctx,Stk1);
    Hgt = ^[|Stk|];

--    logMsg("In case Stkb=$(Stkb), Stkc=$(Stkc)");
    valis (reconcileStack(Stkb,Stkc),
      GCode++[.iLbl(Nxt),.iCase(Max)]++TCode++CCode++[.iLbl(DLbl),.iRst(Hgt)]++DCode)
  }

  compCases:all e ~~ display[e] |: (cons[csEntry[e]],integer,integer,
    (e,Cont)=>Cont,Cont,Cont,assemLbl,codeCtx,stack) =>(stack,multi[assemOp],multi[assemOp]).
  compCases(Cs,Ix,Mx,Comp,Succ,Fail,Deflt,Ctx,Stk) => case Cs in {
    [] => valof{
      if Ix==Mx then
	valis (.none,[],[])
      else{
	(Stk1,TCde,Cde) = compCases([],Ix+1,Mx,Comp,Succ,Fail,Deflt,Ctx,Stk);
	valis (Stk1,[.iJmp(Deflt)]++TCde,Cde)
      }
    }
    | [(Ix,Case),..Cases] => valof{
      Lb = defineLbl("CC",Ctx);
      (Stkb,TCde2,Cde2) = compCases(Cases,Ix+1,Mx,Comp,Succ,Fail,Deflt,Ctx,Stk);
      (Stkc,CCde) = compCaseBranch(Case,Comp,Succ,Fail,Ctx,Stk);
--      logMsg("In cases Stkb=$(Stkb), Stkc=$(Stkc)");
      valis (reconcileStack(Stkb,Stkc),[.iJmp(Lb)]++TCde2,Cde2++[.iLbl(Lb),..CCde])
    }
    | [(Iy,Case),..Cases] => valof{
      (Stk1,TCde,CCde) = compCases([(Iy,Case),..Cases],Ix+1,Mx,Comp,Succ,Fail,Deflt,Ctx,Stk);
      valis (Stk1,[.iJmp(Deflt)]++TCde,CCde)
    }
  }

  compCaseBranch:all e ~~ display[e] |: (cons[cCase[e]],
    (e,Cont)=>Cont,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).

  compCaseBranch(Cs,Comp,Succ,Fail,Ctx,Stk) => case Cs in {
    [(Lc,Ptn,Exp)] => compPttrn(Ptn,Comp(Exp,Succ),Fail,Ctx,Stk)
    | [(Lc,Ptn,Exp),..More] => valof{
      Fl = defineLbl("CF",Ctx);
      VLb = defineLbl("CN",Ctx);
      Vr = genSym("__");
      (Off,Ctx1) = defineLclVar(Vr,typeOf(Ptn)::ltipe,Ctx);
      (Stkb,RlCde) = compPttrn(Ptn,Comp(Exp,Succ),jmpCont(Fl,Stk),Ctx1,Stk);
      (Stkc,AltCde) = compMoreCase(More,Off,Comp,Succ,Fail,Ctx,Stk);
--      logMsg("in case branch Stkb=$(Stkb), Stkc=$(Stkc)");
      valis (reconcileStack(Stkb,Stkc),[.iTL(Off)]++RlCde++[.iLbl(Fl)]++AltCde)
    }
  }

  compMoreCase:all e ~~ (cons[(option[locn],cExp,e)],integer,(e,Cont)=>Cont,
    Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compMoreCase(Cs,Off,Comp,Succ,Fail,Ctx,Stk) => case Cs in {
    [] => Fail.C(Ctx,Stk,[])
    | [(Lc,Ptn,Exp),..More] => valof{
      Fl = defineLbl("CM",Ctx);
      (Stk2,RlCde) = compPttrn(Ptn,Comp(Exp,Succ),jmpCont(Fl,Stk),Ctx,Stk);
      (Stk3,RstCde) = compMoreCase(More,Off,Comp,Succ,Fail,Ctx,Stk);
--      logMsg("In more case Stk2=$(Stk2), Stk3=$(Stk3)");
      valis (reconcileStack(Stk2,Stk3),[.iLdL(Off)]++RlCde++[.iLbl(Fl)]++RstCde)
    }
  }
  
  compCnsCase:all e ~~ display[e] |: (option[locn],cExp,cons[cCase[e]],(e,Cont)=>Cont,
    Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compCnsCase(Lc,Gv,Cs,Comp,Cont,Ctx,Stk) => case Cs in {
    [(_,Ptn,Exp)] => compExp(Gv,.notLast, ptnCont(Ptn,Comp(Exp,Cont),
	abortCont(Lc,"match error")),Ctx,Stk)
    | Cases default =>
      compExp(Gv,.notLast,cnsCaseCont(Cases,Comp,splitCont(Lc,Ctx,Cont)),Ctx,Stk)
  }

  cnsCaseCont:all e ~~ display[e] |: (cons[cCase[e]],(e,Cont)=>Cont,Cont) => Cont.
  cnsCaseCont(Cases,Comp,Cont) => cont{
    C(Ctx,AStk,GCde) => valof{
      (Stk2,JCde,CCde) = compCnsCases(Cases,Comp,Cont,Ctx,AStk);
    
      valis (Stk2,GCde++[.iIndxJmp([|Cases|])]++JCde++CCde)
    }
  }

  compCnsCases:all e ~~ display[e] |: (cons[cCase[e]],(e,Cont)=>Cont,Cont,codeCtx,stack) =>
    (stack,multi[assemOp],multi[assemOp]).
  compCnsCases(Cs,Comp,Succ,Ctx,Stk) => case Cs in {
    [] => (.none,[],[])
    | [(Lc,Ptn,Exp),..Cases] => valof{
      Lb = cseLbl(Ptn,Ctx);
      (Stk2a,TCde2,Cde2) = compCnsCases(Cases,Comp,Succ,Ctx,Stk);
      (Stk3a,CCde) = compPtn(Ptn,Comp(Exp,Succ),abortCont(Lc,"match error"),Ctx,Stk);
--      logMsg("Stk2a=$(Stk2a), Stk3a=$(Stk3a)");
      if ~reconcileable(Stk2a,Stk3a) then
	reportError("cannot reconcile case $(Ptn)=>$(Exp) with $(Cases)",Lc);
      valis (reconcileStack(Stk2a,Stk3a),[.iJmp(Lb),..TCde2],Cde2++[.iLbl(Lb),..CCde])
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

  compPttrn:(cExp,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPttrn(Ptn,Succ,Fail,Ctx,Stk) => compPtn(Ptn,Succ,Fail,Ctx,Stk).
  
  compPtn:(cExp,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtn(Ptn,Succ,Fail,Ctx,Stk) => case Ptn in {
    .cVar(_,.cId("_",_)) => Succ.C(Ctx,dropStack(Stk),[.iDrop])
    | .cVar(Lc,.cId(Vr,Tp)) => valof{
      if Loc ?= locateVar(Vr,Ctx) then 
	valis compPtnVar(Lc,Vr,Loc,Succ,Ctx,dropStack(Stk))
      else{
	LTp = Tp::ltipe;
	(Off,Ctx1) = defineLclVar(Vr,LTp,Ctx);
	valis compPtnVar(Lc,Vr,.lclVar(Off,LTp),Succ,Ctx1,dropStack(Stk))
      }
    }
    | .cVoid(Lc,_) => Succ.C(Ctx,dropStack(Stk),[.iDrop])
    | .cAnon(Lc,_) => Succ.C(Ctx,dropStack(Stk),[.iDrop])
    | .cTerm(Lc,Nm,Args,Tp) => valof{
      Stk0 = dropStack(Stk);
      Flb = defineLbl("U",Ctx);
      (Stk1,FCde) = Fail.C(Ctx,Stk0,[]);
      
      (Stk2,SCde) = compPtnArgs(Args,Succ,jmpCont(Flb,Stk1),Ctx,loadStack(Args//(A)=>(typeOf(A)::ltipe),Stk0));

--      logMsg("Unpack $(Nm), Args=$(Args), Stk1=$(Stk1), Stk2=$(Stk2), SCde=$(SCde)");
      valis (reconcileStack(Stk1,Stk2),[.iUnpack(.tLbl(Nm,size(Args)),Flb)]++SCde++[.iLbl(Flb),..FCde])
    }
    | _ default => valof{
      if isGround(Ptn) then{
	Flb = defineLbl("Tst",Ctx);
	Stk0 = dropStack(Stk); 
	(Stk1,FCde) = Fail.C(Ctx,Stk0,[]);
	(Stk2,SCde) = Succ.C(Ctx,Stk0,[]);
--	logMsg("ground ptn Stk1=$(Stk1), Stk2=$(Stk2)");
	valis (reconcileStack(Stk1,Stk2),
	  [.iLdC(Ptn::data),ptnCmp(Ptn,Flb)]++SCde++[.iLbl(Flb),..FCde])
      } else{
	reportError("uncompilable pattern $(Ptn)",locOf(Ptn));
	valis Succ.C(Ctx,Stk,[])
      }
    }
  }

  ptnCmp(Ptn,Lb) => case Ptn in {
    .cInt(_,Ix) => .iICmp(Lb).
    .cChar(_,Cx) => .iCmp(Lb).
    .cFloat(_,Dx) => .iFCmp(Lb).
    _ => .iCmp(Lb)
  }.

  compPtnVar:(option[locn],string,srcLoc,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtnVar(Lc,Nm,.lclVar(Off,Tp),Cont,Ctx,Stk) => Cont.C(Ctx,Stk,[.iStL(Off)]).
  compPtnVar(Lc,Nm,.argVar(Off,Tp),Cont,Ctx,Stk) => Cont.C(Ctx,Stk,[.iDrop]).

  compPtnArgs:(cons[cExp],Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtnArgs(Es,Succ,Fail,Ctx,Stk) => case Es in {
    [] => Succ.C(Ctx,Stk,[])
    | [A,..As] => compPtn(A,argsPtnCont(As,Succ,Fail),Fail,Ctx,Stk)
  }

  argsPtnCont(As,Succ,Fail) => cont{
    C(Ctx,Stk,Cde) => valof{
      (Stk2,Cde2) = compPtnArgs(As,Succ,Fail,Ctx,Stk);
      valis (Stk2,Cde++Cde2)
    }
  }

  -- continuations

  Cont ::= cont{
    C:(codeCtx,stack,multi[assemOp])=>(stack,multi[assemOp]).
  }.

  allocCont:(termLbl,stack,Cont) => Cont.
  allocCont(Lbl,Stk,Cont) => cont{
    C(Ctx,_AStk,Cde) => Cont.C(Ctx,Stk,Cde++[.iAlloc(Lbl),frameIns(Stk)])
  }.

  escapeCont:(string,stack,Cont) => Cont.
  escapeCont(Es,Stk,Cont) => cont{
    C(Ctx,_Stk,Cde) => Cont.C(Ctx,Stk,Cde++[.iEscape(Es),frameIns(Stk)]).
  }

  intrinsicCont:(assemOp,boolean,stack,Cont) => Cont.
  intrinsicCont(I,Frm,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,Stk,Cde++[I]++(Frm??[frameIns(Stk)]||[])).
  }

  callCont:(termLbl,tailMode,stack,Cont) => Cont.
  callCont(Lbl,.notLast,Stk,Cont) => cont{
    C(Ctx,_AStk,Cde) => Cont.C(Ctx,Stk,Cde++[.iCall(Lbl),frameIns(Stk)]).
  }
  callCont(Lbl,.last,Stk,Cont) => cont{
    C(Ctx,_AStk,Cde) => (.none,Cde++[.iTCall(Lbl)]).
  }

  oclCont:(integer,tailMode,stack,Cont) => Cont.
  oclCont(Ar,.notLast,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,Stk,Cde++[.iOCall(Ar),frameIns(Stk)]).
  }
  oclCont(Ar,.last,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => (.none,Cde++[.iTOCall(Ar)]).
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

  throwCont:Cont.
  throwCont = cont{
    C(_,_,Cde) => (.none,Cde++[.iThrow])
  }

  contCont:(assemLbl)=>Cont.
    contCont(Lbl) => cont{
    C(Ctx,Stk,Cde) => (Stk,Cde++[.iJmp(Lbl)]).
  }

  stoCont:(string,ltipe,stack,Cont) => Cont.
  stoCont(Vr,Tp,Stk,Cont) => cont{
    C(Ctx,_,Cde) => valof{
      (Off,Ctx1) = defineLclVar(Vr,Tp,Ctx);
      valis Cont.C(Ctx1,Stk,Cde++[.iStL(Off)])
    }
  }

  ifCont:(option[locn],stack,Cont,Cont) => Cont.
  ifCont(Lc,Stk,Succ,Fail) => cont{
    C(Ctx,_,Cde) => valof{
      Flb = defineLbl("F",Ctx);
      (SStk,SCde) = Succ.C(Ctx,Stk,[.iIfNot(Flb)]);
      (FStk,FCde) = Fail.C(Ctx,Stk,[]);
      
--      logMsg("SStk=$(SStk), FStk=$(FStk)");
      valis (reconcileStack(SStk,FStk),Cde++SCde++[.iLbl(Flb),..FCde])
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

  retireCont:(stack) => Cont.
  retireCont(Stk) => cont{
    C(Ctx,_AStk,Cde) => (Stk,Cde++[.iRetire]).
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

  expCont:(cExp,tailMode,Cont) => Cont.
  expCont(Exp,TM,Cont) => cont{
    C(Ctx,Stk,Cde) => valof{
--      logMsg("Compile cont $(Exp), with stack $(Stk)");
      (Stk1,OCde) = compExp(Exp,TM,Cont,Ctx,Stk);
--      logMsg("Stack after Compile cont $(Exp) is $(Stk1)");
      valis (Stk1,Cde++OCde)
    }
  }

  traceCont:(string,Cont) => Cont.
  traceCont(Msg,Cont) => cont{
    C(Ctx,Stk,Cde) => valof{
--      logMsg("invoking continuation #(Msg), stack = $(Stk)");
      valis Cont.C(Ctx,Stk,Cde)
    }
  }

  checkCont:((codeCtx)=>boolean,string,option[locn],Cont) => Cont.
  checkCont(Chk,Msg,Lc,Cont) => cont{
    C(Ctx,Stk,Cde) => valof{
      if ~Chk(Ctx) then{
	reportError("check #(Msg) failed",Lc);
	valis (Stk,Cde)
      }
      else
      valis Cont.C(Ctx,Stk,Cde)
    }
  }

  condCont:(cExp,tailMode,Cont,Cont,stack) => Cont.
  condCont(Cond,TM,Succ,Fail,Stk) => cont{
    C(Ctx,XStk,Cde) => valof{
      (Stk1,SSCde) = resetStack([|Stk|],XStk);
      (Stk2,CCde) = compCond(Cond,TM,Succ,Fail,Ctx,Stk1);
      valis (Stk2,Cde++SSCde++CCde)
    }
  }

  ptnCont:(cExp,Cont,Cont) => Cont.
  ptnCont(Ptn,Succ,Fail) => cont{
    C(Ctx,Stk,Cde) => valof{
      (Stk1,Cde1) = compPttrn(Ptn,Succ,Fail,Ctx,Stk);
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

  thunkCont:(stack,Cont) => Cont.
  thunkCont(Stk,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.iThunk])
  }

  thGetCont:(stack,assemLbl,Cont) => Cont.
  thGetCont(Stk,Exit,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.iLdTh])
  }

  thSetCont:(stack,Cont) => Cont.
  thSetCont(Stk,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.iTTh])
  }

  catchCont:all e ~~ (assemLbl,integer,()=>(stack,multi[assemOp])) => Cont.
  catchCont(CLb,EOff,HComp) => cont{
    C(_,_,Cde) => valof{
      (SStk,HCde) = HComp();
      valis (SStk,Cde++[.iLbl(CLb),.iStL(EOff)]++HCde)
    }
  }

  asgnCont:(Cont,codeCtx,stack) => Cont.
  asgnCont(ACont,Ctx,Stk) => cont{
    C(_,_,Cde) => 
      ACont.C(Ctx,Stk,Cde++[.iAssign])
  }

  abortCont:(option[locn],string) => Cont.
  abortCont(.some(Lc),Msg) => cont{
    C(_,_,Cde) => valof{
--      logMsg("abort cont $(Lc), $(Msg)");
      valis (.none,Cde++[.iLdC(Lc::data),.iLdC(.strg(Msg)),.iAbort])
    }
  }

  errorCont:(option[locn],string) => Cont.
  errorCont(Lc,Msg) => cont{
    C(_,Stk,Cde) => valof{
      if _?=Stk then
	reportError(Msg,Lc);
      valis (Stk,Cde)
    }
  }

  actionCont:(aAction,tailMode,Cont,Cont) => Cont.
  actionCont(A,TM,ACont,Cont) => cont{
    C(Ctx,Stk,Cde) => valof{
--      logMsg("action $(A) at stack $(Stk)");
      (SStk,SCde) = compAction(A,TM,ACont,Cont,Ctx,Stk);
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

  reconcileable:(stack,stack)=>boolean.
  reconcileable(S1,S2) => case S1 in {
    .none => .true.
    .some(Sl) => case S2 in {
      .none => .true.
      .some(Sr) => valof{
	if Sl==Sr then
	  valis .true
	else{
	  valis .false
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
--      logMsg("reset stack $(XStk) to $(Stk)");
      (NStk,SCde) = resetStack([|Stk|],XStk);
      valis Cont.C(Ctx,NStk,Cde++SCde)
    }
  }

  dropCont:(stack,Cont) => Cont.
  dropCont(EStk,Cont) => cont{
    C(Ctx,XStk,Cde) => valof{
      if TgtDpth ?= [|EStk|] && CurDpth?=[|XStk|] then{
	if CurDpth > TgtDpth then{
	  (_,DCde) = resetStack([|EStk|],XStk);
	  valis Cont.C(Ctx,EStk,Cde++[.iRot(CurDpth-TgtDpth-1)]++DCde)
	} else
	valis Cont.C(Ctx,EStk,Cde)
      } else{
	reportTrap("illegal stacks");
	valis Cont.C(Ctx,EStk,Cde)
      }
    }
  }

  frameIns:(stack)=>assemOp.
  frameIns(.some(Stk)) => .iFrame(.tplTipe(Stk)).

  locateVar:(string,codeCtx)=>option[srcLoc].
  locateVar(Nm,Ctx) => Ctx.vars[Nm].

  defineLclVar:(string,ltipe,codeCtx) => (integer,codeCtx).
  defineLclVar(Nm,Tp,Ctx) => valof{
    hwm = Ctx.hwm;
    
    Off = hwm!+1;
    hwm := Off;

    valis (Off,Ctx.vars<<-Ctx.vars[Nm->.lclVar(Off,Tp)])
  }

  argVars:(cons[cId],map[string,srcLoc],integer) => map[string,srcLoc].
  argVars([],Mp,_)=>Mp.
  argVars([.cId(Nm,Tp),..As],Vars,Ix) =>
    argVars(As,Vars[Nm->.argVar(Ix,Tp::ltipe)],Ix+1).
  argVars([_,..As],Map,Ix) => argVars(As,Map,Ix+1).

  glCtx:(codeCtx,cExp) => codeCtx.
  glCtx(Ctx,Exp) => valof{
    Vrs = glVars(Exp,[]);
    valis foldLeft((.cId(Nm,Tp),C)=>snd(defineLclVar(Nm,Tp::ltipe,C)),Ctx,Vrs)
  }

  dsjCtx:(codeCtx,cExp,cExp) => codeCtx.
  dsjCtx(Ctx,L,R) => valof{
    CommonVrs = glVars(L,[]) /\ glVars(R,[]);

    valis foldLeft((.cId(Nm,Tp),Cx)=>snd(defineLclVar(Nm,Tp::ltipe,Cx)),Ctx,CommonVrs)
  }

  drop:all x,e ~~ stream[x->>e] |: (x,integer)=>x.
  drop(S,0)=>S.
  drop([_,..S],N)=>drop(S,N-1).

  dropStack(.none) => .none.
  dropStack(.some([_,..Stk])) => .some(Stk).

  srcLoc ::= .lclVar(integer,ltipe) |
    .argVar(integer,ltipe) |
    .glbVar(string,ltipe) |
    .glbFun(termLbl,ltipe).

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

  tailMode ::= .last | .notLast.

  emptyCtx:(map[string,srcLoc])=>codeCtx.
  emptyCtx(Glbs) => codeCtx{
    vars = Glbs.
    end = .al("$$").
    escape = .al("$$").
    lbls = ref 0.
    min = 0.
    hwm = ref 0.
    brks = [].
  }

  defineLbl:(string,codeCtx)=>assemLbl.
  defineLbl(Pr,C) => valof{
    CurLbl = C.lbls!;
    C.lbls := CurLbl+1;
    valis .al("#(Pr)$(CurLbl)")
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
    disp(C) => "<C hwm:$(C.hwm!)\:$(C.vars)>C>".
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
