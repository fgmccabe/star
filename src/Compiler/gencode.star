star.compiler.gencode{
  import star.
  import star.assert.
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
    valis compDefs(Defs,Vars)
  }

  declGlobal(.varDec(_,_,Nm,Tp), Vrs) => Vrs[Nm->.glbVar(Nm,Tp::ltipe)].
  declGlobal(.funDec(_,_,Nm,Tp), Vrs) => Vrs[Nm->.glbVar(Nm,Tp::ltipe)].
  declGlobal(_,Vrs) => Vrs.

  compDefs:(cons[cDefn],map[string,srcLoc])=> cons[codeSegment].
  compDefs(Dfs,Glbs) => (Dfs//(D)=>genDef(traceCodeGen! trace D,Glbs)).

  genDef:(cDefn,map[string,srcLoc]) => codeSegment.
  genDef(.fnDef(Lc,Nm,Tp,Args,Val),Glbs) => genFun(Lc,Nm,Tp,Args,Val,Glbs).
  genDef(.glDef(Lc,Nm,Tp,Val),Glbs) => genGlb(Lc,Nm,Tp,Val,Glbs).
  genDef(.tpDef(Lc,Tp,TpRl,Index),_) => .tipe(Tp,TpRl,Index).
  genDef(.lblDef(_Lc,Lbl,Tp,Ix),_) => .struct(Lbl,Tp,Ix).

  genFun:(option[locn],string,tipe,cons[cExp],cExp,map[string,srcLoc]) => codeSegment.
  genFun(Lc,Nm,Tp,Args,Val,Glbs) => valof{
    Ctx = emptyCtx(Glbs);

    (AbrtCde,_) = compAbort(Lc,"function: $(Nm) aborted",Ctx,.some([]));

    BlkSig = nearlyFlatSig(funTypeRes(Tp)::ltipe);

    AbrtLbl = defineLbl(Ctx,"Abrt");

    AbrtBrks = [("$abort",AbrtLbl,.none)];

    (FC,Ct1,Stk0) = compArgs(Args,Lc,0,AbrtBrks,Ctx,.some([]));
    (EC,Stk1) = compExp(Val,Lc,AbrtBrks,.noMore,Ct1,Stk0);
    
    C0 = (genDbg([.iEntry])++chLine(.none,Lc)++
      [.iLbl(AbrtLbl,.iBlock(BlkSig,FC++EC++genRet()))]++AbrtCde)::cons[assemOp];
    Code = .func(.tLbl(Nm,arity(Tp)),.hardDefinition,Tp::ltipe,varInfo(Ct2),C0);

    if traceCodegen! then
      showMsg("non-peep code is $(Code)");
    Peeped = peepOptimize(Code);
    if traceCodegen! then
      showMsg("peeped code is $(Peeped)");

    valis Peeped;
  }

  genGlb:(option[locn],string,tipe,cExp,map[string,srcLoc]) => codeSegment.
  genGlb(Lc,Nm,Tp,Val,Glbs) => valof{
    Ctx = emptyCtx(Glbs);

    (AbrtCde,_) = compAbort(Lc,"global eval: $(Nm) aborted",Ctx,.some([]));

    AbrtLbl = defineLbl(Ctx,"Abrt");

    BlkSig = nearlyFlatSig(Tp::ltipe);

    (EC,_Ct2,Stk1) = compExp(Val,Lc,[("$abort",AbrtLbl,.none)],.noMore,Ctx,.some([]));
    
    C0 = (genDbg([.iEntry])++chLine(.none,Lc)++
      [.iLbl(AbrtLbl,.iBlock(BlkSig,FC++EC++genRet()))]++AbrtCde)::cons[assemOp];

    Code = .func(.tLbl(Nm,0),.hardDefinition,Tp::ltipe,varInfo(Ct2),C0);

    if traceCodegen! then
      showMsg("non-peep code is $(Code)");
    Peeped = peepOptimize(Code);
    if traceCodegen! then
      showMsg("peeped code is $(Peeped)");

    valis Peeped;
  }

  compExp:(cExp,option[locn],cons[breakLvl],tailMode,codeCtx,stack) => (stack,multi[assemOp]).
  compExp(Exp,OLc,Brks,Last,Ctx,Stk) => case Exp in {
    | E where isGround(E) =>
      genLastReturn(Last,pshStack(typeOf(Exp),Stk),[.iLdC(Exp::data)])
    | .cVar(Lc,.cId(Vr,Tp)) => valof{
      if Loc?=locateVar(Vr,Ctx) then {
	valis case Loc in {
	  | .argVar(Off,Tp) => genLastReturn(Last,pushStack(Tp,Stk),[.iLdA(Off)])
	  | .lclVar(Nm,Tp) => genLastReturn(Last,pushStack(Tp,Stk),[.iLdL(Nm)])
	  | .glbVar(Nm,Tp) => genLastReturn(Last,pushStack(Tp,Stk),[.iLdG(Nm)])
	  | .glbFun(Nm,Tp) => genLastReturn(Last,pushStack(Tp,Stk),[.iLdC(.symb(Nm))])
	}
      } else {
	reportError("cannot locate variable $(Vr)\:$(Tp)",Lc);
	valis (pshStack(Tp,Stk),[.iLdV])
      }
    }
    | .cVoid(Lc,Tp) => genLastReturn(Last,pshStack(Tp,Stk),[.iLdV])
    | .cAnon(Lc,Tp) => genLastReturn(Last,pshStack(Tp,Stk),[.iLdV])
    | .cTerm(Lc,Nm,Args,Tp) => valof{
      (_,ArgCode) = compExps(Args,Lc,Brks,Ctx,Stk);
      Stk1 = pshStack(Tp,Stk);
      valis genLastReturn(Last,Stk1,
	chLine(OLc,Lc)++ArgCode++[.iAlloc(.tLbl(Nm,size(Args))),frameIns(Stk1)])
    }
    | .cCall(Lc,Nm,Args,Tp) where (_,Ins,Frm,_)?=intrinsic(Nm) => valof{
      (_,ArgCode) = compExps(Args,Lc,Brks,Ctx,Stk);
      Stk1 = pshStack(Tp,Stk);
      valis genLastReturn(Last,Stk1,chLine(OLc,Lc)++ArgCode++[Ins]++(Frm??[frameIns(Stk1)]||[]))
    }
    | .cCall(Lc,Nm,Args,Tp) where isEscape(Nm) => valof{
      (_,ArgCode) = compExps(Args,Lc,Brks,Ctx,Stk);
      Stk1 = pshStack(Tp,Stk);
      valis genLastReturn(Last,Stk1,
	chLine(OLc,Lc)++ArgCode++[.iEscape(Nm),frameIns(Stk1)])
    }
    | .cCall(Lc,Nm,Args,Tp) => valof{
      (_,ArgCode) = compExps(Args,Lc,Brks,Ctx,Stk);
      Stk1 = pshStack(Tp,Stk);
      if .noMore.=Last then
	valis (Stk1,chLine(OLc,Lc)++ArgCode++[.iTCall(.tLbl(Nm,[|Args|]))])
      else
      valis (Stk1,chLine(OLc,Lc)++ArgCode++[.iCall(.tLbl(Nm,[|Args|])),frameIns(Stk1)])
    }
    | .cOCall(Lc,Op,Args,Tp) => valof{
      (Stk0,ArgCode) = compExps(Args,Lc,Brks,Ctx,Stk);
      (_,OCode) = compExp(Op,Lc,Brks,.notLast,Ctx,Stk0);
      Stk1 = pshStack(Tp,Stk);
      if Last==.noMore then
	valis (Stk1,chLine(OLc,Lc)++ArgCode++OCode++[.iTOCall([|Args|])])
      else
      valis (Stk1,chLine(OLc,Lc)++ArgCode++OCode++[.iOCall([|Args|]),frameIns(Stk1)])
    }
    | .cClos(Lc,Nm,Ar,F,Tp) => valof{
      (Stk1,FCode) = compExp(F,Lc,Brks,.notLast,Ctx,Stk);
      Stk2 = pshStack(Tp,Stk);
      valis genLastReturn(Last,Stk2,
	chLine(OLc,Lc)++FCode++[.iClosure(.tLbl(Nm,Ar)),frameIns(Stk2)])
    }
    | .cTask(Lc,T,Tp) => valof{
      (_,TC) = compExp(T,Lc,Brks,.notLast,Ctx,Stk);
      valis genLastReturn(Last,pshStack(Tp,Stk),chLine(OLc,Lc)++TC++[.iFiber])
    }
    | .cSv(Lc,Tp) => genLastReturn(Last,pshStack(Tp,Stk),chLine(OLc,Lc)++[.iSav])
    | .cSvSet(Lc,Th,Vl) => valof{
      (Stk0,VlC) = compExp(Vl,Lc,Brks,.notLast,Ctx,Stk);
      (_Stk,ThC) = compExp(Th,Lc,Brks,.notLast,Ctx,Stk0);
      valis genLastReturn(Last,pshStack(typeOf(Vl),Stk),chLine(OLc,Lc)++VlC++ThC++[.iTSav])
    }
    | .cNth(Lc,E,Ix,Tp) => valof{
      (Stk0,Vl) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
      valis genLastReturn(Last,pshStack(Tp,Stk),chLine(OLc,Lc)++Vk++[.iNth(Ix)])
    }
    | .cSetNth(Lc,R,Ix,V) => valof{
      (Stk0,RC) = compExp(R,Lc,Brks,.notLast,Ctx,Stk);
      (_Stk,VC) = compExp(V,Lc,Brks,.notLast,Ctx,Stk0);

      valis genLastReturn(Last,pshStack(typeOf(R),Stk),
	chLine(OLc,Lc)++RC++VC++[.iStNth(Ix)])
    }
    | .cSeq(Lc,L,R) => valof{
      (Stk0,LC) = compExp(L,Lc,Brks,.notLast,Ctx,Stk);
      (Stk1,RC) = compExp(R,Lc,Brks,Last,Ctx,Stk);
      valis (Stk1,chLine(OLc,Lc)++LC++resetStack([|Stk0|],Stk)++RC)
    }
    | .cCnd(Lc,G,L,R) => valof{
      Fl = defineLbl(Ctx,"Fl");
      Ok = defineLbl(Ctx,"Ok");
      (CC,Ctx1) = compCond(G,Lc,.Brks,normal,Ctx,Stk);
      (Stk1,LC) = compExp(L,Lc,Brks,Last,Ctx1,Stk);
      (Stk2,RC) = compExp(R,Lc,Brks,Last,Ctx,Stk);
      valis (reconcileStack(Stk1,Stk2),chLine(OLc,Lc)++
	[.iLbl(Ok,.iBlock(nearlyFlatSig(typeOf(L)::ltipe),
	      [.iLbl(Fl,.iBlock(flatSig,CC++LC++[.iBreak(Ok)]))]++
	      RC++[.iBreak(Ok)]))])
    }
    | .cCase(Lc,Gov,Cases,Deflt,_Tp) => compCase(Lc,Gov,Cases,Deflt,compExp,Brks,Last,Ctx,Stk)
    | .cLtt(Lc,.cId(Vr,VTp),Val,Bnd) => valof{
      Ctx1 = defineLclVar(Vr,ThTp::ltipe,Ctx);
      (Stk1,VV) = compExp(Val,Lc,Brks,.notLast,Ctx,Stk);
      (Stkx,BB) = compExp(Bnd,Lc,Brks,Last,Ctx1,Stk);
      valis (Stkx,chLine(OLc,Lc)++VV++[.iStL(Vr)]++BB)
    }
    | .cAbort(Lc,Msg,Tp) => compAbort(Lc,Msg,Ctx,Stk)
    | .cTry(Lc,B,.cVar(_,.cId(Th,ThTp)),.cVar(_,.cId(Er,ETp)),H,Tp) => valof{
      if traceCodegen! then
	showMsg("compiling try catch @$(Lc), $(B), catch $(H)");
      
      (CLb,Ctx0) = defineExitLbl("Tr",Ctx);
      Blk = defineLbl(Ctx,"H");
      (TOff,CtxB) = defineLclVar(Th,ThTp::ltipe,Ctx0);
      (EOff,Ctx1) = defineLclVar(Er,ETp::ltipe,Ctx);
      
      (Stk1,BCde) = compExp(B,Lc,.notLast,tryEndCont(TOff,Cont),CtxB,Stk); -- critical: body of try is not tail rec
      (Stk2,HCde) = compExp(H,Lc,TM,Cont,Ctx1,Stk);

      if ~reconcileable(Stk1,Stk2) then
	reportError("cannot reconcile try exp $(B) with handler $(H)",Lc);

      valis (reconcileStack(Stk1,Stk2),[.iTry(Blk),.iStL(TOff)]++BCde++[.iLbl(Blk),.iStL(EOff)]++HCde)
    }
    | .cRaise(Lc,T,E,_) => valof{
      (Stk1,EC) = compExp(E,Lc,Brks,.notLast,Stk);
      (Stk2,TC) = compExp(T,Lc,Brks,.notLast,Stk1);
      valis (.none,chLine(OLc,Lc)++EC++TC++[.iThrow])
    }
    | .cValof(Lc,A,Tp) => valof{
      Ok = defineLbl(Ok,"Ctx");
      Sig = nearlyFlatSig(Tp::ltipe);
      Stkx = pshStack(Tp,Stk);
      
      valis (Stkx,
	chLine(OLc,Lc)++[.iLbl(Ok,.iBlock(Sig,compAction(A,Lc,[("$valof",(S)=>(resetStack([|S|],Stkx)++[.iBreak(Ok)],Stkx))|Brks],Last,Ctx,Stk)))])
    }
    |  C where isCond(C) => valof{
      Nx = defineLbl(Ctx,"E");
      Stk0 = pshStack(boolType,Stk);
      (Stk1,Cde) = compCond(C,OLc,.notLast,trueCont(jmpCont(Nx,Stk0)),falseCont(jmpCont(Nx,Stk0)),
	Ctx,Stk);
      valis Cont.C(Ctx,Stk1,Cde++[.iLbl(Nx)]) -- fix me
    }
    | C => valof{
      reportError("cannot compile expression $(C)",locOf(C));
      valis Cont.C(Ctx,Stk,[])
    }
  }

    -- Expressions are evaluated in reverse order
  compExps:(cons[cExp],option[locn],cons[breakLvl],codeCtx,stack) => (stack,multi[assemOp]).
  compExps([],_,_,_,Stk)=>(Ctx,Stk,[]).
  compExps([Exp,..Es],Lc,Brks,Ctx,Stk)=> valof{
    (Stk1,Rest) = compExps(Es,locOf(Exp),Brks,Ctx,Stk);
    (Stk2,EC) = compExp(Exp,Lc,Brks,.notLast,Ctx,Stk1);
    valis (Stk2,Rest++EC)
  }

  condMode ::= .normal | .negated.

  compCond:(cExp,option[locn],assemLbl,cons[breakLvl],condMode,codeCtx,stack) => (multi[assemOp],codeCtx).
  compCond(C,OLc,Fail,Brks,.negated,Ctx,Stk) => compNegated(C,OLc,Fail,Brks,Ctx,Stk).
  compCond(C,OLc,Fail,Brks,.normal,Ctx,Stk) => compNormal(C,OLc,Fail,Brks,Ctx,Stk).

  compNormal:(cExp,option[locn],assemLbl,cons[breakLvl],codeCtx,stack) => (multi[assemOp],codeCtx).
  compNormal(C,OLc,Fail,Brks,Ctx,Stk) => case C in {
    | .cTerm(_,"true",[],_) => ([],Ctx)
    | .cTerm(_,"false",[],_) => ([.iBreak(Fail)],Ctx)
    | .cCnj(Lc,L,R) => valof{
      (LC,Ctxa) = compNormal(L,Lc,Fail,Brks,Ctx,Stk);
      (RC,Ctxb) = compNormal(R,Lc,Fail,Brks,Ctxa,Stk);
      valis (chLine(OLc,Lc)++LC++Rc,Ctxb)
    }
    | .cDsj(Lc,L,R) => valof{
      Fl = defineLbl(Ctx,"Fl");
      Ok = defineLbl(Ctx,"Ok");
      (LC,Ctxa) = compNormal(L,Lc,Fl,Brks,Ctx,Stk);
      (RC,Ctxb) = compNormal(R,Lc,Fail,Brks,Ctx,Stk);

      valis (chLine(OLc,Lc)++[.iLbl(Ok,.iBlock(flatSig,[.iLbl(Fl,.iBlock(flatSig,LC++[.iBreak(Ok)]))]++RC++[.iBreak(Ok)]))],mergeCtx(Ctxa,Ctxb))
    }
    | .cNeg(Lc,R) => compNegated(R,OLc,Fail,Brks,Ctx,Stk)
    | .cCnd(Lc,T,L,R) => valof{
      Ok = defineLbl(Ctx,"Ok");
      Fl = defineLbl(Ctx,"El");
      (TC,Ctx1) = compNormal(T,Lc,Fl,Brks,Ctx,Stk);
      (LC,Ctxa) = compNormal(L,Lc,Fail,Brks,Ctx1,Stk);
      (RC,Ctxb) = compNormal(R,Lc,Fail,Brks,Ctk,Stk);
      valis (chLine(OLc,Lc)++
	[.iLbl(Ok,.iBlock(flatSig,[.iLbl(Fl,TC++LC++[.iBreak(Ok)])]
	      ++RC++[.iBreak(Ok)]))],mergeCtx(Ctxa,Ctxb))
    }
    | .cMatch(Lc,Ptn,Exp) => valof{
      (EC,Stk0) = compExp(Exp,Lc,.notLast,Brks,Ctx,Stk);
      (PC,Ctx1) = compPtn(Ptn,Lc,Fail,Brks,Ctx,Stk0);
      valis (chLine(OLc,Lc)++EC++PC,Ctx1)
    }
    | Exp default => valof{
      (EC,_) = compExp(Exp,Lc,.notLast,Brks,Ctx,Stk);
      valis (EC++[.iIfNot(Fail)],Ctx)
    }
  }

  compNegated:(cExp,option[locn],assemLbl,cons[breakLvl],codeCtx,stack) => (multi[assemOp],codeCtx).
  compNegated(C,OLc,Fail,Brks,Ctx,Stk) => case C in {
    | .cTerm(_,"true",[],_) => ([.iBreak(Fail)],Ctx)
    | .cTerm(_,"false",[],_) => ([],Ctx)
    | .cCnj(Lc,L,R) => compNormal(.cDsj(Lc,.cNeg(Lc,L),.cNeg(Lc,R)),OLc,Fail,Brks,Ctx,Stk)
    | .cDsj(Lc,L,R) => compNormal(.cCnj(Lc,.cNeg(Lc,L),.cNeg(Lc,R)),OLc,Fail,Brks,Ctx,Stk)
    | .cNeg(_Lc,I) => compNormal(I,OLc,Fail,Brks,Ctx,Stk)
    | .cCnd(Lc,T,L,R) => compNormal(.cCnd(Lc,T,R,L),OLc,Fail,Brks,Ctx,Stk)
    | .cMatch(Lc,Ptn,Exp) => valof{
      Ok = defineLbl(Ctx,"Ok");
      (EC,Stk0) = compExp(Exp,Lc,.notLast,Brks,Ctx,Stk);
      (PC,Ctx1) = compPtn(Ptn,Lc,Ok,Brks,Ctx,Stk0);
      valis (chLine(OLc,Lc)++[.iLbl(Ok,.iBlock(flatSig,EC++PC++[.iBreak(Fail)]))],Ctx)
    }
    | Exp default => valof{
      (EC,_) = compExp(Exp,Lc,.notLast,Brks,Ctx,Stk);
      valis (EC++[.iIf(Fail)],Ctx)
    }
  }
  
  compAction:(aAction,option[locn],cons[breakLvl],tailMode,codeCtx,stack) =>(multi[assemOp],codeCtx).
  compAction(A,OLc,Brks,Last,Ctx,Stk) => case A in {
    | .aNop(_Lc) => ([],Ctx)
    | .aSeq(Lc,L,R) => valof{
      (LC,Ctx0) = compAction(L,Brks,.notLast,Ctx,Stk);
      (RC,Ctx1) = compAction(R,Brks,Last,Ctx0,Stk);
      valis (LC++RC,Ctx1)
    }
    | .aLbld(Lc,Lb,LbldA) =>
      compAction(LbldA,OLc,[(Lb,(Stk0)=>resetStack([|Stk|],Stk0)++[.iBreak(Lbl)]),..Brks],Last,Ctx,Stk)
      
      Ctxl = (Ctx.brks=Ctx.brks[Lb->ctxCont(Ctx,resetStkCont(Stk,ACont))]);
      valis compAction(LbldA,Lc,TM,ACont,Cont,Ctxl,Stk)
    }
    | .aBreak(Lc,Lb) => valof{
      if XCont?=Ctx.brks[Lb] then{
	(_,Cde) = XCont.C(Ctx,Stk,[]); -- Special handling for breaks, we cannot relate their stack
	valis (.none,Cde)
      }
      else{
	reportError("unknown break label $(Lb)",Lc);
	valis ACont.C(Ctx,Stk,[])
      }
    }
    | .aValis(Lc,E) => compExp(E,Lc,TM,Cont,Ctx,Stk)
    | .aDo(Lc,E) => compExp(E,Lc,TM,resetStkCont(Stk,ACont),Ctx,Stk)
    | .aDefn(Lc,P,E) => compExp(E,Lc,.notLast,ptnCont(P,Lc,ACont,abortCont(Lc,"define error")),Ctx,Stk)
    | .aAsgn(Lc,P,E) => compExp(E,Lc,.notLast,expCont(P,Lc,.notLast,asgnCont(ACont,Ctx,Stk)),Ctx,Stk)
    | .aSetNth(Lc,T,Ix,E) => compExp(T,Lc,.notLast,expCont(E,Lc,.notLast,setNthCont(Ix,ACont,Stk)),Ctx,Stk)
    | .aCase(Lc,G,Cs,D) => compCase(Lc,G,Cs,D,(Ac,C1)=>actionCont(Ac,Lc,TM,ACont,C1),Cont,Ctx,Stk)
    | .aIftte(Lc,G,L,R) => valof{
      AC = splitCont(Lc,Ctx,ACont);
      CC = splitCont(Lc,Ctx,Cont);
      valis compCond(G,Lc,.notLast,actionCont(L,Lc,TM,AC,CC),
	resetStkCont(Stk,ctxCont(Ctx,actionCont(R,Lc,TM,AC,CC))),Ctx,Stk)
    }
    | .aWhile(Lc,G,B) => valof{
      Lp = defineLbl(Ctx,"Lp");
      Tst = defineLbl(Ctx,"Tst");
      Ex = defineLbl(Ctx,"Ex");
      GCtx = glCtx(Ctx,G);

      (Stk1,WCde) = compCond(G,Lc,.notLast,jmpCont(Lp,Stk),jmpCont(Ex,Stk),GCtx,Stk);
      (Stk0,BCde) = compAction(B,Lc,.notLast,jmpCont(Tst,Stk),Cont,GCtx,Stk);

      valis ACont.C(Ctx,reconcileStack(Stk0,Stk1),
	[.iJmp(Tst),.iLbl(Lp)]++BCde++[.iLbl(Tst)]++WCde++[.iLbl(Ex)])
    }
    |.aLtt(Lc,.cId(Vr,VTp),Val,Bnd) => valof{
      valis compExp(Val,Lc,.notLast,stoCont(Vr,VTp::ltipe,Stk,actionCont(Bnd,Lc,TM,ACont,Cont)),Ctx,Stk)
    }
    | .aTry(Lc,B,.cVar(_,.cId(Th,ThTp)), .cVar(_,.cId(Er,ETp)),H) => valof{

      if traceCodegen! then
	showMsg("compiling try catch @$(Lc), $(B), catch $(H)");
      
      Blk = defineLbl(Ctx,"H");
      (TOff,Ctx0) = defineLclVar(Th,ETp::ltipe,Ctx);
      (EOff,Ctx1) = defineLclVar(Er,ETp::ltipe,Ctx);
      AC = splitCont(Lc,Ctx,ACont);
      CC = splitCont(Lc,Ctx,Cont);
      
      (Stk1,BCde) = compAction(B,Lc,.notLast,tryEndCont(TOff,AC),tryEndCont(TOff,CC),Ctx0,Stk);
      (Stk2,CCde) = compAction(H,Lc,TM,AC,CC,Ctx1,Stk);

      if ~reconcileable(Stk1,Stk2) then
	reportError("cannot reconcile try body $(B) with handler $(H)",Lc);
      
      valis (reconcileStack(Stk1,Stk2),[.iTry(Blk),.iStL(TOff)]++BCde++[.iLbl(Blk),.iStL(EOff)]++CCde)
    }
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
      showMsg("compiling case @$(Lc), Gov=$(Gv), Deflt=$(Deflt), Cases=$(Cases)");
    Nxt = defineLbl(Ctx,"CN");
    DLbl = defineLbl(Ctx,"CD");

    (Stk1,GCode) = compExp(Gv,Lc,.notLast,nxCont(Nxt),Ctx,Stk);
    (Table,Max) = genCaseTable(Cases);

    if traceCodegen! then{
      showMsg("$(Max) case table: $(Table)");
      showMsg("stack going into cases: $(Stk1)");
    };
    
    OC = splitCont(Lc,Ctx,Cont);
    (Stkc,DCode) = Comp(Deflt,OC).C(Ctx,Stk,[]);

    if traceCodegen! then{
      showMsg("stack after default: $(Stkc)");
    };

    (Stkb,TCode,CCode) = compArms(Table,0,Max,Comp,OC,jmpCont(DLbl,Stkc),DLbl,Ctx,Stk1);

    if traceCodegen! then
      showMsg("stack after case arms: $(Stkb)");

    if ~reconcileable(Stkb,Stkc) then
      reportError("cannot cases' stack $(Cases) with default $(Deflt)",Lc);

    valis (reconcileStack(Stkb,Stkc),
      GCode++[.iLbl(Nxt),.iCase(Max)]++TCode++CCode++[.iLbl(DLbl),.iRst(_optval([|Stk|]))]++DCode)
  }

  compArms:all e ~~ display[e] |: (cons[csEntry[e]],integer,integer,
    (e,Cont)=>Cont,Cont,Cont,assemLbl,codeCtx,stack) =>(stack,multi[assemOp],multi[assemOp]).
  compArms(Cs,Ix,Mx,Comp,Succ,Fail,Deflt,Ctx,Stk) => case Cs in {
    | [] => valof{
      if Ix==Mx then
	valis (.none,[],[])
      else{
	(Stk1,TCde,Cde) = compArms([],Ix+1,Mx,Comp,Succ,Fail,Deflt,Ctx,Stk);
	valis (Stk1,[.iJmp(Deflt)]++TCde,Cde)
      }
    }
    | [(Ix,Case),..Cases] => valof{
      if traceCodegen! then{
	showMsg("compile case $(Ix) of $(Mx)\:$(Case)");
	showMsg("incoming case stack $(Stk)");
      };
      (Stkc,CCde) = compArm(Case,Comp,Succ,Fail,Ctx,Stk);

      if traceCodegen! then
	showMsg("stack (c) after arg $(Stkc)");

      (Stkb,TCde2,Cde2) = compArms(Cases,Ix+1,Mx,Comp,Succ,Fail,Deflt,Ctx,Stk);
      if traceCodegen! then{
	showMsg("stack after arms $(Stkb)");
	showMsg("compile stack case $(Case)");
      };


      if ~reconcileable(Stkb,Stkc) then
	reportError("cannot recon stack of first case $(Case)=$(Stkb) with other cases $(Cases)=$(Stkc)",.none);

      Lb = defineLbl(Ctx,"CC");
      valis (reconcileStack(Stkb,Stkc),[.iJmp(Lb)]++TCde2,Cde2++[.iLbl(Lb),..CCde])
    }
    | [(Iy,Case),..Cases] => valof{
      (Stk1,TCde,CCde) = compArms([(Iy,Case),..Cases],Ix+1,Mx,Comp,Succ,Fail,Deflt,Ctx,Stk);
      valis (Stk1,[.iJmp(Deflt)]++TCde,CCde)
    }
  }

  compArm:all e ~~ display[e] |: (cons[cCase[e]],
    (e,Cont)=>Cont,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).

  compArm(Cs,Comp,Succ,Fail,Ctx,Stk) => case Cs in {
    | [(Lc,Ptn,Exp)] => valof{
      if traceCodegen! then
	showMsg("short arm: $(Ptn) -> $(Exp)");
      valis compPttrn(Ptn,Lc,Comp(Exp,Succ),Fail,Ctx,Stk)
    }
    | [(Lc,Ptn,Exp),..More] => valof{
      if traceCodegen! then
	showMsg("stack going into this long arm $(Ptn) -> $(Exp) is $(Stk)\nMore\: $(More)");
      
      Fl = defineLbl(Ctx,"CF");
      VLb = defineLbl(Ctx,"CN");
      Vr = genSym("__");
      Ptipe = typeOf(Ptn)::ltipe;
      (Off,Ctx1) = defineLclVar(Vr,Ptipe,Ctx);

      (Stkc,AltCde) = compMoreArm(More,Off,Comp,Succ,Fail,Ctx1,Stk);

      if traceCodegen! then{
	showMsg("$(Lc)\: stack after more arm (a): $(More) is $(Stkc)");
      };
      
      (Stkb,RlCde) = compPttrn(Ptn,Lc,Comp(Exp,Succ),jmpCont(Fl,Stkc),Ctx1,Stk);

      if traceCodegen! then
	showMsg("$(Lc)\: stack after this arm: $(Ptn) -> $(Exp) is $(Stkb)");
      
      valis (reconcileStack(Stkb,Stkc),[.iTL(Off)]++RlCde++[.iLbl(Fl),.iLdL(Off)]++AltCde)
    }
  }

  compMoreArm:all e ~~ display[e] |: (cons[(option[locn],cExp,e)],integer,(e,Cont)=>Cont,
    Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compMoreArm(Cs,Off,Comp,Succ,Fail,Ctx,Stk) => case Cs in {
    | [] => Fail.C(Ctx,.none,[])
    | [(Lc,Ptn,Exp),..More] => valof{
      Fl = defineLbl(Ctx,"CM");
      
      (Stk3,RstCde) = compMoreArm(More,Off,Comp,Succ,Fail,Ctx,Stk);

      if traceCodegen! then
	showMsg("$(Lc)\: stack after more arms: $(More) is $(Stk3)");
      
      (Stk2,RlCde) = compPttrn(Ptn,Lc,Comp(Exp,Succ),jmpCont(Fl,Stk3),Ctx,Stk);

      if traceCodegen! then
	showMsg("$(Lc)\: stack after more arm (b) $(Ptn)->$(Exp) is $(Stk2)");

      if ~reconcileable(Stk2,Stk3) then
	reportError("cannot recon stack [$(Stk2)] of first arm $(Ptn) -> $(Exp) with more arms $(More) [$(Stk3)]",Lc);

      valis (reconcileStack(Stk2,Stk3),RlCde++[.iLbl(Fl),.iLdL(Off)]++RstCde)
    }
  }
  
  all e ~~ csEntry[e] ~> (integer,cons[cCase[e]]).

  genCaseTable(Cases) where Mx.=nextPrime(size(Cases)) =>
    (sortCases(caseHashes(Cases,Mx)),Mx).

  caseHashes:all e ~~ (cons[cCase[e]],integer)=>cons[(option[locn],cExp,integer,e)].
  caseHashes(Cases,Mx) => (Cases//((Lc,Pt,Ex))=>(Lc,Pt,
      (try caseHash(Pt)%Mx catch exception in {_ => 0}),Ex)).

  caseHash:(cExp)=>integer.
  caseHash(E) => case E in {
    | .cVar(_,_) => 0
    | .cInt(_,Ix) => Ix
    | .cBig(_,Bx) => hash(Bx)
    | .cFlt(_,Dx) => hash(Dx)
    | .cChar(_,Cx) => hash(Cx)
    | .cString(_,Sx) => hash(Sx)
    | .cTerm(_,Nm,Args,_) => size(Args)*37+hash(Nm)
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

  compArgPtrn:(cExp,option[locn],integer,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compArgPtrn(Ptn,Lc,Ix,Succ,Fail,Ctx,Stk) where .cVar(_,.cId(Vr,Tp)) .= Ptn =>
    Succ.C(defineArgVar(Vr,Tp,Ix,Ctx),Stk,[]).
  compArgPtrn(Ptn,Lc,Ix,Succ,Fail,Ctx,Stk) => valof{
    (Stk1,Code1) = compPttrn(Ptn,Lc,Succ,Fail,Ctx,pshStack(typeOf(Ptn),Stk));
    valis (Stk1,[.iLdA(Ix)]++Code1)
  }

  compArgPtns:(cons[cExp],option[locn],integer,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compArgPtns(Es,Lc,Ix,Succ,Fail,Ctx,Stk) => case Es in {
    | [] => Succ.C(Ctx,Stk,[])
    | [A,..As] => compArgPtrn(A,Lc,Ix,argPtnCont(As,locOf(A),Ix+1,Succ,Fail),Fail,Ctx,Stk)
  }

  argPtnCont(As,Lc,Ix,Succ,Fail) => cont{
    C(Ctx,Stk,Cde) => valof{
      (Stk2,Cde2) = compArgPtns(As,Lc,Ix,Succ,Fail,Ctx,Stk);
      valis (Stk2,Cde++Cde2)
    }
  }

  compPtn:(cExp,option[locn],assemLbl,cons[breakLvl],codeCtx,stack) => (codeCtx,stack,multi[assemOp]).
  compPtn(Ptn,OLc,Fail,Brks,Ctx,Stk) => case Ptn in {
    | .cVar(_,.cId("_",_)) => (codeCtx,dropStack(Stk),[.iDrop])
    | .cVar(Lc,.cId(Vr,Tp)) => valof{
      if Loc ?= locateVar(Vr,Ctx) then 
	valis compPtnVar(Vr,Loc,Stk)
      else{
	LTp = Tp::ltipe;
	Ctx1 = defineLclVar(Vr,LTp,Ctx);
	valis compPtnVar(Vr,.lclVar(Vr,LTp),Stk)
      }
    }
    | .cVoid(Lc,_) => (dropStack(Stk),[.iDrop])
    | .cAnon(Lc,_) => (dropStack(Stk),[.iDrop])
    | .cTerm(Lc,Nm,Args,Tp) => valof{
      Stk0 = dropStack(Stk);
      Flb = defineLbl(Ctx,"U");


      (Stk1,FCde) = Fail.C(Ctx,Stk0,[]);
      
      (Stk2,SCde) = compPtnArgs(Args,Lc,Succ,resetStkCont(Stk0,jmpCont(Flb,Stk1)),Ctx,loadStack(Args//(A)=>(typeOf(A)::ltipe),Stk0));

      if traceCodegen! then
	showMsg("Succ stack $(Stk2), Fail stack $(Stk1)");

      valis (reconcileStack(Stk1,Stk2),[.iUnpack(.tLbl(Nm,size(Args)),Flb)]++SCde++[.iLbl(Flb),..FCde])
    }
    | _ default => ( isGround(Ptn) ??
      (dropStack(Stk),[.iCLit(Ptn::data,Fail)]) || valof{
	reportError("uncompilable pattern $(Ptn)",locOf(Ptn));
	valis (dropStack(Stk),[.iBreak(Fail)])
      }
    )
  }

  ptnCmp(Ptn,Lb) => case Ptn in {
    | .cInt(_,Ix) => .iICmp(Lb)
    | .cChar(_,Cx) => .iCCmp(Lb)
    | .cFlt(_,Dx) => .iFCmp(Lb)
    | _ => .iCmp(Lb)
  }.

  compPtnVar:(string,srcLoc,codeCtx,stack) => (codeCtx,stack,multi[assemOp]).
  compPtnVar(Nm,.lclVar(Off,Tp),Ctx,Stk) => (Ctx,dropStack(Stk),[.iStL(Off)]).
  compPtnVar(Nm,.argVar(Off,Tp),Ctx,Stk) => (Ctx,dropStack(Stk),[.iDrop]).

  compPtnArgs:(cons[cExp],option[locn],assemLbl,cons[breakLvl],codeCtx,stack) => (codeCtx,stack,multi[assemOp]).
  compPtnArgs(Es,Lc,Fail,Brks,Ctx,Stk) => case Es in {
    | [] => (Ctx,Stk,[])
    | [A,..As] => valof{
      (Ctx0,Stka,AC) = compPtn(A,Lc,Fail,Brks,Ctx,Stk);

      compPtn(A,Lc,argsPtnCont(As,locOf(A),Succ,Fail),Fail,Ctx,Stk)
  }

  argsPtnCont(As,Lc,Succ,Fail) => cont{
    C(Ctx,Stk,Cde) => valof{
      (Stk2,Cde2) = compPtnArgs(As,Lc,Succ,Fail,Ctx,Stk);
      valis (Stk2,Cde++Cde2)
    }
  }

  -- continuations

  Cont ::= cont{
    C:(codeCtx,stack,multi[assemOp])=>(stack,multi[assemOp]).
  }.

  lineCont:(option[locn],option[locn],Cont) => Cont.
  lineCont(OLc,Lc,Cont) => cont{
    C(Ctx,Stk,Cde) => Cont.C(Ctx,Stk,chLine(OLc,Lc)++Cde)
  }.
  
  allocCont:(termLbl,stack,Cont) => Cont.
  allocCont(Lbl,Stk,Cont) => cont{
    C(Ctx,_AStk,Cde) => Cont.C(Ctx,Stk,Cde++[.iAlloc(Lbl),frameIns(Stk)])
  }.

  escapeCont:(string,stack,Cont) => Cont.
  escapeCont(Es,Stk,Cont) => cont{
    C(Ctx,_Stk,Cde) => Cont.C(Ctx,Stk,Cde++[.iEscape(Es),frameIns(Stk)]).
  }

  intrinsicCont:(assemOp,boolean,tailMode,stack,Cont) => Cont.
  intrinsicCont(I,Frm,.noMore,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => (.none,Cde++[I]).
  }
  intrinsicCont(I,Frm,.notLast,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,Stk,Cde++[I]++(Frm??[frameIns(Stk)]||[])).
  }

  callCont:(termLbl,tailMode,stack,Cont) => Cont.
  callCont(Lbl,.notLast,Stk,Cont) => cont{
    C(Ctx,_AStk,Cde) => Cont.C(Ctx,Stk,Cde++[.iCall(Lbl),frameIns(Stk)]).
  }
  callCont(Lbl,.noMore,Stk,Cont) => cont{
    C(Ctx,_AStk,Cde) => (.none,Cde++[.iTCall(Lbl)]).
  }

  oclCont:(integer,tailMode,stack,Cont) => Cont.
  oclCont(Ar,.notLast,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,Stk,Cde++[.iOCall(Ar),frameIns(Stk)]).
  }
  oclCont(Ar,.noMore,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => (.none,Cde++[.iTOCall(Ar)]).
  }

  retCont:Cont.
  retCont = cont{
    C(_,_,Cde) => (.none,Cde++[.iRet])
  }

  glbRetCont:(string)=>Cont.
  glbRetCont(Nm) => cont{
    C(_,_,Cde) => (.none,Cde++[.iTG(Nm),.iRet])
  }

  tryEndCont:(integer,Cont) => Cont.
  tryEndCont(Off,Cont) => cont{
    C(Ctx,Stk,Cde) => Cont.C(Ctx,Stk,Cde++[.iLdL(Off),.iEndTry])
  }

  jmpCont:(assemLbl,stack)=>Cont.
  jmpCont(Lbl,Stk) => cont{
    C(Ctx,_Stk1,Cde) => (Stk,Cde++[.iJmp(Lbl)]).
    }

  nxCont:(assemLbl)=>Cont.
  nxCont(Lbl) => cont{
    C(Ctx,Stk,Cde) => (Stk,Cde++[.iJmp(Lbl)]).
  }

  raiseCont:Cont.
  raiseCont = cont{
    C(_,_,Cde) => (.none,Cde++[.iThrow])
  }

  resetCont:(stack,Cont)=>Cont.
  resetCont(Stk,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.iReset,frameIns(Stk)]).
  }

  shiftCont:(stack,Cont)=>Cont.
  shiftCont(Stk,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.iShift,frameIns(Stk)])
  }

  invokeCont:(tailMode,stack,Cont)=>Cont.
  invokeCont(_,Stk,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.iInvoke,frameIns(Stk)])
  }

  stoCont:(string,ltipe,stack,Cont) => Cont.
  stoCont(Vr,Tp,Stk,Cont) => cont{
    C(Ctx,_,Cde) => valof{
      (Off,Ctx1) = defineLclVar(Vr,Tp,Ctx);
      valis Cont.C(Ctx1,Stk,Cde++[.iStL(Off)])
    }
  }

  lclCont:(integer,stack,Cont) => Cont.
  lclCont(Off,Stk,Cont) => cont{
    C(Ctx,_,Cde) => valof{
      valis Cont.C(Ctx,Stk,Cde++[.iStL(Off)])
    }
  }

  ifCont:(option[locn],stack,Cont,Cont) => Cont.
  ifCont(Lc,Stk,Succ,Fail) => cont{
    C(Ctx,_,Cde) => valof{
      Flb = defineLbl(Ctx,"F");
      (SStk,SCde) = Succ.C(Ctx,Stk,[.iIfNot(Flb)]);
      (FStk,FCde) = Fail.C(Ctx,Stk,[]);
      
      valis (reconcileStack(SStk,FStk),Cde++SCde++[.iLbl(Flb),..FCde])
    }
  }

  falseCont:(Cont) => Cont.
  falseCont(Cont) => cont{
    C(Cxt,Stk,Cde) => Cont.C(Cxt,pushStack(.bool,Stk),Cde++[.iLdC(falseEnum)]).
  }

  trueCont:(Cont) => Cont.
  trueCont(Cont) => cont{
    C(Cxt,Stk,Cde) => Cont.C(Cxt,pushStack(.bool,Stk),Cde++[.iLdC(trueEnum)]).
  }

  expCont:(cExp,option[locn],tailMode,Cont) => Cont.
  expCont(Exp,Lc,TM,Cont) => cont{
    C(Ctx,Stk,Cde) => valof{
      (Stk1,OCde) = compExp(Exp,Lc,TM,Cont,Ctx,Stk);
      valis (Stk1,Cde++OCde)
    }
  }

  traceCont:(string,Cont) => Cont.
  traceCont(Msg,Cont) => cont{
    C(Ctx,Stk,Cde) => valof{
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

  condCont:(cExp,option[locn],tailMode,Cont,Cont,stack) => Cont.
  condCont(Cond,Lc,TM,Succ,Fail,Stk) => cont{
    C(Ctx,XStk,Cde) => valof{
      (Stk1,SSCde) = resetStack([|Stk|],XStk);
      (tk2,CCde) = compCond(Cond,Lc,TM,Succ,Fail,Ctx,Stk1);
      valis (Stk2,Cde++SSCde++CCde)
    }
  }

  ptnCont:(cExp,option[locn],Cont,Cont) => Cont.
  ptnCont(Ptn,Lc,Succ,Fail) => cont{
    C(Ctx,Stk,Cde) => valof{
      (Stk1,Cde1) = compPttrn(Ptn,Lc,Succ,Fail,Ctx,Stk);
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

  compAbort:(option[locn],string,codeCtx,stack) => (cons[assemOp],stack).
  compAbort(.some(Lc),Msg,Ctx,Stk) =>
    ([.iLdC(Lc::data),.iLdC(.strg(Msg)),.iAbort],.none).

  errorCont:(option[locn],string) => Cont.
  errorCont(Lc,Msg) => cont{
    C(_,Stk,Cde) => valof{
      if _?=Stk then
	reportError(Msg,Lc);
      valis (Stk,Cde)
    }
  }

  actionCont:(aAction,option[locn],tailMode,Cont,Cont) => Cont.
  actionCont(A,OLc,TM,ACont,Cont) => cont{
    C(Ctx,Stk,Cde) => valof{
      (SStk,SCde) = compAction(A,OLc,TM,ACont,Cont,Ctx,Stk);
      valis (SStk,Cde++SCde)
    }
  }

  reconcileStack:(stack,stack)=>stack.
  reconcileStack(S1,S2) => case S1 in {
    | .none => S2
    | .some(Sl) => case S2 in {
      | .none => S1
      | .some(Sr) => valof{
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
    | .none => .true
    | .some(Sl) => case S2 in {
      | .none => .true
      | .some(Sr) => valof{
	if Sl==Sr then
	  valis .true
	else{
	  valis .false
	}
      }
    }
  }

  resetStack:(option[integer],stack) => (stack,multi[assemOp]).
  resetStack(.some(Dp),.some(Stk)) => case [|Stk|] in {
    | Dp => (.some(Stk),[])
    | Dp1 where Dp == Dp1-1 => (tail(Stk),[.iDrop])
    | Dp1 where Dp1>Dp => (.some(popTo(Stk,Dp)),[.iRst(Dp)])
    | Dp1 default => valof{	
	reportTrap("invalid stack height in $(Stk)\:$(Dp1)~$(Dp)");
	valis (.some(Stk),[])
      }
    }.
  resetStack(_,.none) => (.none,[]).

  resetStkCont:(stack,Cont) => Cont.
  resetStkCont(Stk,Cont) => cont{
    C(Ctx,XStk,Cde) => valof{
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

  defineLclVar:(string,ltipe,codeCtx) => codeCtx.
  defineLclVar(Nm,Tp,Ctx) => valof{
    valis (Ctx.vars=Ctx.vars[Nm->.lclVar(Nm,Tp)])
  }

  ensureLclVar:(string,ltipe,codeCtx) => codeCtx.
  ensureLclVar(Nm,Tp,Ctx) => valof{
    if _ ?=Ctx.vars[Nm] then
      valis Ctx
    else
    valis defineLclVar(Nm,Tp,Ctx)
  }

  argSrcLocs:(cons[cId],map[string,srcLoc],integer) => map[string,srcLoc].
  argSrcLocs([],Mp,_)=>Mp.
  argSrcLocs([.cId(Nm,Tp),..As],Vars,Ix) =>
    argSrcLocs(As,Vars[Nm->.argVar(Ix,Tp::ltipe)],Ix+1).
  argSrcLocs([_,..As],Map,Ix) => argSrcLocs(As,Map,Ix+1).

  argSrcLoc:(cId,integer) => srcLoc.
  argSrcLoc(.cId(_,Tp),Ix) => .argVar(Ix,Tp::ltipe).

  defineArgVar:(string,tipe,integer,codeCtx) => codeCtx.
  defineArgVar(Nm,Tp,Ix,Ctx) => (Ctx.vars=Ctx.vars[Nm->.argVar(Ix,Tp::ltipe)]).

  glCtx:(codeCtx,cExp) => codeCtx.
  glCtx(Ctx,Exp) => valof{
    Vrs = glVars(Exp,[]);
    valis foldLeft((.cId(Nm,Tp),C)=>snd(defineLclVar(Nm,Tp::ltipe,C)),Ctx,Vrs)
  }

  dsjCtx:(codeCtx,cExp,cExp) => codeCtx.
  dsjCtx(Ctx,L,R) => valof{
    CommonVrs = glVars(L,[]) /\ glVars(R,[]);

    valis foldLeft((.cId(Nm,Tp),Cx)=>ensureLclVar(Nm,Tp::ltipe,Cx),Ctx,CommonVrs)
  }

  drop:all x,e ~~ stream[x->>e] |: (x,integer)=>x.
  drop(S,0)=>S.
  drop([_,..S],N)=>drop(S,N-1).

  dropStack(.none) => .none.
  dropStack(.some([_,..Stk])) => .some(Stk).

  srcLoc ::= .lclVar(string,ltipe) |
  .argVar(integer,ltipe) |
  .glbVar(string,ltipe) |
  .glbFun(termLbl,ltipe) |
  .thnkFn(termLbl,ltipe).

  stack ~> option[cons[ltipe]].
  breakLvl ~> (string,(stack)=>(multi[assemOp],stack)).

  codeCtx ::= codeCtx{
    vars : map[string,srcLoc].
    lbls : ref integer.  
  }

  emptyCtx:(map[string,srcLoc])=>codeCtx.
  emptyCtx(Glbs) => codeCtx{
    vars = Glbs.
    lbls = ref 0.
  }

  defineLbl:(codeCtx,string)=>assemLbl.
  defineLbl(C,Pr) => valof{
    CurLbl = C.lbls!;
    C.lbls := CurLbl+1;
    valis "L#(Pr)$(CurLbl)"
  }

  pushStack:(ltipe,stack) => stack.
  pushStack(Tp,.some(Stk)) => .some([Tp,..Stk]).

  pshStack:(tipe,stack) => stack.
  pshStack(Tp,.some(Stk)) => .some([Tp::ltipe,..Stk]).

  loadStack:(cons[ltipe],stack) => stack.
  loadStack(Tps,.some(Stk)) => .some(Tps++Stk).

  popTo:all e ~~ (cons[e],integer) => cons[e].
  popTo(LL,D) => let{.
    pop(Ls,Ix) => case Ls in {
      | [] => []
      | [_,..L] where Ix>0 => pop(L,Ix-1)
      | _ where Ix==0 => Ls
    }
  .} in pop(LL,[|LL|]-D).

  implementation display[codeCtx] => {
    disp(C) => "<C $(C.vars) C>".
  }

  implementation display[srcLoc] => {
    disp(L) => case L in {
      | .lclVar(Off,Tpe) => "lcl $(Off)\:$(Tpe)"
      | .argVar(Off,Tpe) => "arg $(Off)\:$(Tpe)"
      | .glbVar(Off,Tpe) => "glb $(Off)\:$(Tpe)"
      | .glbFun(Off,Tpe) => "fun $(Off)\:$(Tpe)"
      | .thnkFn(Off,Tpe) => "thk $(Off)\:$(Tpe)"
    }
  }

  chLine:(option[locn],option[locn]) => multi[assemOp].
  chLine(_,.none) => [].
  chLine(.some(Lc),.some(Lc)) => [].
  chLine(_,.some(Lc)) => (genDebug! ?? [.iLine(Lc::data)] || []).

  genDbg:(multi[assemOp]) => multi[assemOp].
  genDbg(Ins) => (genDebug! ?? [.iDBug,..Ins] || Ins).

  flatSig = funTipe(.tplTipe([]),.voidTipe)::string.
  nearlyFlatSig(T) => .funTipe(.tplTipe([]),T)::string.
  blockSig(Args,Rs) => .funTipe(.tplTipe(Args),Rs)::string.

}
