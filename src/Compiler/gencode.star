star.compiler.gencode{
  import star.
  import star.assert.
  import star.multi.
  import star.pkg.
  import star.sort.

  import star.compiler.assem.
  import star.compiler.term.
  import star.compiler.encode.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.opts.
  import star.compiler.peephole.
  import star.compiler.ltipe.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.data.

  public compProg:(pkg,cons[cDefn],cons[decl])=>cons[codeSegment].
  compProg(Pkg,Defs,Decls) => valof{
    Vars = foldLeft(declGlobal,[],Decls);
    Tps = foldLeft(declType,[],Decls);

    if traceCodegen! then
      showMsg("Compilation dictionary vars: $(Vars), types: $(Tps)");
    
    valis compDefs(Defs,Vars)
  }

  declGlobal(.varDec(_,_,Nm,Tp), Vrs) => Vrs[Nm->(Tp,.glbVar(Nm,Tp::ltipe))].
  declGlobal(.funDec(_,_,Nm,Tp), Vrs) => Vrs[Nm->(Tp,.glbVar(Nm,Tp::ltipe))].
  declGlobal(_,Vrs) => Vrs.

  declType:(decl,map[string,(tipe,cons[tipe])])=>map[string,(tipe,cons[tipe])].
  declType(.tpeDec(_,Nm,Tp,_,Map), Tps) => 
    ((_Tp,Cns) ?= Tps[Nm] ?? Tps[Nm->(Tp,Cns)] || Tps[Nm->(Tp,[])]).
  declType(.cnsDec(_,Nm,_,CnTp),Tps) => valof{
    RTp = funTypeRes(CnTp);
    TpNm = tpName(RTp);
    if (Tpe,Cns) ?= Tps[TpNm] then
      valis Tps[TpNm->(Tpe,[CnTp,..Cns])] else
    valis Tps[TpNm->(RTp,[CnTp])]
  }
  declType(_,Tps) => Tps.

  compDefs:(cons[cDefn],map[string,(tipe,srcLoc)])=> cons[codeSegment].
  compDefs(Dfs,Glbs) => (Dfs//(D)=>genDef(D,Glbs)).

  genDef:(cDefn,map[string,(tipe,srcLoc)]) => codeSegment.
  genDef(.fnDef(Lc,Nm,Tp,Args,Val),Glbs) => genFun(Lc,Nm,Tp,Args,Val,Glbs).
  genDef(.glDef(Lc,Nm,Tp,Val),Glbs) => genGlb(Lc,Nm,Tp,Val,Glbs).
  genDef(.tpDef(Lc,Tp,TpRl,Index),_) => .tipe(Tp,TpRl,Index).
  genDef(.lblDef(_Lc,Lbl,Tp,Ix),_) => .struct(Lbl,Tp,Ix).

  genFun:(option[locn],string,tipe,cons[cExp],cExp,map[string,(tipe,srcLoc)]) => codeSegment.
  genFun(Lc,Nm,Tp,Args,Val,Glbs) => valof{
    Ctx = emptyCtx(Glbs);

    if traceCodegen! then
      showMsg("Compile $(.fnDef(Lc,Nm,Tp,Args,Val))\n");

    AbrtCde = compAbort(Lc,"function: $(Nm) aborted",Ctx);
    AbrtLbl = defineLbl(Ctx,"Abrt");
    ExLbl = defineLbl(Ctx,"TrExit");

    Brks = ["$abort" -> (((C,S)=>(AbrtCde,C,.none)),AbrtLbl),
      "$try" -> (((C,S)=>([.iResult(ExLbl)],C,.none)),ExLbl)];

    (FC,Ct1,Stk0) = compArgs(Args,0,AbrtLbl,Brks,Ctx,.some([]));
    (EC,Ct2,Stk1) = compExp(Val,Lc,Brks,.noMore,Ct1,Stk0);
    
    C0 = genDbg([.iEntry(size(varInfo(Ct2)))])++
    chLine(.none,Lc)++[.iLbl(AbrtLbl,.iBlock(0,
	  [.iLbl(ExLbl,.iBlock(1,FC++EC++[.iRet])),.iXRet])),..AbrtCde];
    
    Code = .func(.tLbl(Nm,arity(Tp)),.hardDefinition,Tp::ltipe,varInfo(Ct2),C0);

    if traceCodegen! then
      showMsg("non-peep code is $(Code)");
    Peeped = peepOptimize(Code);
    if traceCodegen! then
      showMsg("peeped code is $(Peeped)");

    valis Peeped;
  }

  genGlb:(option[locn],string,tipe,cExp,map[string,(tipe,srcLoc)]) => codeSegment.
  genGlb(Lc,Nm,Tp,Val,Glbs) => valof{
    Ctx = emptyCtx(Glbs);

    AbrtCde = compAbort(Lc,"global eval: $(Nm) aborted",Ctx);

    AbrtLbl = defineLbl(Ctx,"Abrt");
    Brks = ["$abort" -> (((C,S)=>(AbrtCde,C,.none)),AbrtLbl)];

    BlkSig = nearlyFlatSig(Tp::ltipe);

    (EC,Ct2,Stk1) = compExp(Val,Lc,Brks,.notLast,Ctx,.some([]));
    
    C0 = genDbg([.iEntry(size(varInfo(Ct2)))])++chLine(.none,Lc)++
      [.iLbl(AbrtLbl,.iBlock(0,EC++[.iTG(Nm),.iRet]))]
      ++AbrtCde;

    Code = .func(.tLbl(Nm,0),.hardDefinition,Tp::ltipe,varInfo(Ct2),C0);

    if traceCodegen! then
      showMsg("non-peep code is $(Code)");
    Peeped = peepOptimize(Code);
    if traceCodegen! then
      showMsg("peeped code is $(Peeped)");

    valis Peeped;
  }

  compReturn ~> (multi[assemOp],codeCtx,stack).

  compExp:(cExp,option[locn],breakLvls,tailMode,codeCtx,stack) => compReturn.
  compExp(Exp,OLc,Brks,Last,Ctx,Stk) => case Exp in {
    | E where isGround(E) =>
      genReturn(Last,[.iLdC(Exp::data)],Ctx,pshStack(typeOf(Exp),Stk))
    | .cVar(Lc,Vr) => compVar(Vr,Lc,Last,Ctx,Stk)
    | .cVoid(Lc,Tp) => genReturn(Last,[.iLdV],Ctx,pshStack(Tp,Stk))
    | .cAnon(Lc,Tp) => genReturn(Last,[.iLdV],Ctx,pshStack(Tp,Stk))
    | .cTerm(Lc,Nm,Args,Tp) => valof{
      (ArgCode,_,_) = compExps(Args,Lc,Brks,Ctx,Stk);
      Stk1 = pshStack(Tp,Stk);
      valis genReturn(Last,chLine(OLc,Lc)++
	ArgCode++[.iAlloc(.tLbl(Nm,[|Args|])),frameIns(Stk1)],Ctx,Stk1)
    }
    | .cCall(Lc,Nm,Args,Tp) => valof{
      if isEscape(Nm) then
	valis compEscape(OLc,Lc,Nm,Args,Tp,Brks,Last,Ctx,Stk)
      else {
	(ArgCode,_,_) = compExps(Args,Lc,Brks,Ctx,Stk);
	Stk1 = pshStack(Tp,Stk);
	if .noMore.=Last then
	  valis (chLine(OLc,Lc)++ArgCode++
	  [.iTCall(.tLbl(Nm,[|Args|]))],Ctx,Stk1)
	else
	valis (chLine(OLc,Lc)++ArgCode++
	  [.iCall(.tLbl(Nm,[|Args|])),frameIns(Stk1)],Ctx,Stk1)
      }
    }
    | .cXCall(Lc,Nm,Args,Tp,_) => valof{
      if isEscape(Nm) then
	valis compEscape(OLc,Lc,Nm,Args,Tp,Brks,Last,Ctx,Stk)
      else {
	(ArgCode,_,_) = compExps(Args,Lc,Brks,Ctx,Stk);
	Stk1 = pshStack(Tp,Stk);

	if (_,XLbl) ?= Brks["$try"] then{
	  valis genReturn(Last,chLine(OLc,Lc)++ArgCode++
	    [.iXCall(.tLbl(Nm,[|Args|]),XLbl),frameIns(Stk1)],Ctx,Stk1)
	} else{
	  reportError("invoke throwing function: #(Nm) outside a try scope",Lc);
	  valis ([],Ctx,Stk1)
	}
      }
    }
    | .cOCall(Lc,Op,Args,Tp) => valof{
      (ArgCode,_,Stk0) = compExps(Args,Lc,Brks,Ctx,Stk);
      (OCode,_,_) = compExp(Op,Lc,Brks,.notLast,Ctx,Stk0);
      Stk1 = pshStack(Tp,Stk);

      if isThrowingType(typeOf(Op)) then{
	if (_,XLbl) ?= Brks["$try"] then{
	  valis genReturn(Last,chLine(OLc,Lc)++ArgCode++OCode++
	    [.iXOCall([|Args|]+1,XLbl),frameIns(Stk1)],Ctx,Stk1)
	} else{
	  reportError("invoke throwing function: $(Op) outside a try scope",Lc);
	  valis ([],Ctx,Stk1)
	}
      } else if Last==.noMore then
	valis (chLine(OLc,Lc)++ArgCode++OCode++[.iTOCall([|Args|]+1)],Ctx,Stk1)
      else
      valis (chLine(OLc,Lc)++ArgCode++OCode++
	[.iOCall([|Args|]+1),frameIns(Stk1)],Ctx,Stk1)
    }
    | .cXOCall(Lc,Op,Args,Tp,ErTp) => valof{
      (ArgCode,_,Stk0) = compExps(Args,Lc,Brks,Ctx,Stk);
      (OCode,_,_) = compExp(Op,Lc,Brks,.notLast,Ctx,Stk0);
      Stk1 = pshStack(Tp,Stk);

      if traceCodegen! then{
	showMsg("compiling throwing call @$(Op)\:$(typeOf(Op))");
      }

      if (_,XLbl) ?= Brks["$try"] then{
	valis genReturn(Last,chLine(OLc,Lc)++ArgCode++OCode++
	  [.iXOCall([|Args|]+1,XLbl),frameIns(Stk1)],Ctx,Stk1)
      } else{
	reportError("invoke throwing function: $(Op) outside a try scope",Lc);
	valis ([],Ctx,Stk1)
      }
    }
    | .cClos(Lc,Nm,Ar,F,Tp) => valof{
      (FCode,_,Stk1) = compExp(F,Lc,Brks,.notLast,Ctx,Stk);
      Stk2 = pshStack(Tp,Stk);
      valis genReturn(Last,
	chLine(OLc,Lc)++FCode++
	[.iClosure(.tLbl(Nm,Ar)),frameIns(Stk2)],Ctx,Stk2)
    }
    | .cSv(Lc,Tp) => genReturn(Last,chLine(OLc,Lc)++[.iSav],Ctx,pshStack(Tp,Stk))
    | .cSvSet(Lc,Th,Vl) => valof{
      (VlC,_,Stk0) = compExp(Vl,Lc,Brks,.notLast,Ctx,Stk);
      (ThC,_,_) = compExp(Th,Lc,Brks,.notLast,Ctx,Stk0);
      valis genReturn(Last,chLine(OLc,Lc)++VlC++ThC++[.iTSav],Ctx,pshStack(typeOf(Vl),Stk))
    }
    | .cCel(Lc,E,Tp) => valof{
      (EC,_,Stk0) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
      valis genReturn(Last,chLine(OLc,Lc)++EC++[.iCell],Ctx,pshStack(Tp,Stk))
    }
    | .cGet(Lc,E,Tp) => valof{
      (EC,_,Stk0) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
      valis genReturn(Last,chLine(OLc,Lc)++EC++[.iGet],Ctx,pshStack(Tp,Stk))
    }
    | .cNth(Lc,E,Ix,Tp) => valof{
      (VL,_,_) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
      valis genReturn(Last,chLine(OLc,Lc)++VL++[.iNth(Ix)],Ctx,pshStack(Tp,Stk))
    }
    | .cSetNth(Lc,R,Ix,V) => valof{
      (RC,_,Stk0) = compExp(R,Lc,Brks,.notLast,Ctx,Stk);
      (VC,_,_) = compExp(V,Lc,Brks,.notLast,Ctx,Stk0);

      valis genReturn(Last,
	chLine(OLc,Lc)++RC++VC++[.iStNth(Ix)],Ctx,pshStack(typeOf(R),Stk))
    }
    | .cSeq(Lc,L,R) => valof{
      (LC,_,Stk0) = compExp(L,Lc,Brks,.notLast,Ctx,Stk);
      (RC,_,Stkx) = compExp(R,Lc,Brks,Last,Ctx,Stk);
      valis (chLine(OLc,Lc)++LC++resetStack([|Stk|],Stk0)++RC,Ctx,Stkx)
    }
    | .cCnd(Lc,G,L,R) => valof{
      Fl = defineLbl(Ctx,"Fl");
      Ok = defineLbl(Ctx,"Ok");
      (CC,Ctx1) = compCond(G,Lc,Fl,Brks,Ctx,Stk);
      (LC,_,Stk1) = compExp(L,Lc,Brks,Last,Ctx1,Stk);
      (RC,_,Stk2) = compExp(R,Lc,Brks,Last,Ctx,Stk);
      Lvl = stkLvl(Stk);
      valis (chLine(OLc,Lc)++
	[.iLbl(Ok,.iBlock(Lvl+1,
	      [.iLbl(Fl,.iBlock(Lvl,CC++LC++[.iBreak(Ok)]))]++
	      RC++[.iBreak(Ok)]))],Ctx,reconcileStack(Stk1,Stk2))
    }
    | .cCase(Lc,Gov,Cases,Deflt,Tp) =>
      compCase(Lc,Gov,stkLvl(Stk)+1,Cases,Deflt,compExp,Brks,Last,Ctx,Stk)
    | .cLtt(Lc,.cV(Vr,VTp),Val,Bnd) => valof{
      Ctx1 = defineLclVar(Vr,VTp,Ctx);
      (VV,_,Stk1) = compExp(Val,Lc,Brks,.notLast,Ctx,Stk);
      (BB,_,Stkx) = compExp(Bnd,Lc,Brks,Last,Ctx1,Stk);
      valis (chLine(OLc,Lc)++VV++[.iStL(Vr)]++BB,Ctx,Stkx)
    }
    | .cAbort(Lc,Msg,Tp) => (compAbort(Lc,Msg,Ctx),Ctx,.none)
    | .cTry(Lc,B,.cVar(_,.cV(Er,ETp)),H,Tp) => valof{
      if traceCodegen! then{
	showMsg("compiling try catch @$(Lc), $(B), catch $(H)");
      }

      Ok = defineLbl(Ctx,"Tr");
      TrX = defineLbl(Ctx,"TrX");
      Stkx = pshStack(Tp,Stk);
      Ctx1 = defineLclVar(Er,ETp,Ctx);

      TBrks = Brks["$try" -> (((C,S)=>([.iResult(TrX)],C,.none)),TrX)];

      (BC,_,Stka) = compExp(B,Lc,TBrks,.notLast,Ctx,Stk);
      (HC,_,Stkb) = compExp(H,Lc,Brks,Last,Ctx1,Stk);

      if ~reconcileable(Stka,Stkb) then
	reportError("cannot reconcile try exp $(B) with handler $(H)",Lc);
	  
      RLvl = stkLvl(Stk)+1;

      valis (chLine(OLc,Lc)++
	[.iLbl(Ok,.iBlock(RLvl,
	      [.iLbl(TrX,.iBlock(RLvl,BC++[.iResult(Ok)])),
		.iStL(Er)]++HC++[.iResult(Ok)]))],
	Ctx,reconcileStack(Stka,Stkb))
    }
    | .cThrw(Lc,E,_) => valof{
      if (_,Lbl) ?= Brks["$try"] then{
	(EC,_,Stk1) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
	valis (chLine(OLc,Lc)++EC++[.iResult(Lbl)],Ctx,.none)
      } else{
	reportError("throw outside of try scope",Lc);
	valis ([],Ctx,Stk)
      }
    }
    | .cResum(Lc,T,E,Tp) => valof{
      (EC,_,Stk1) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
      (TC,_,Stk2) = compExp(T,Lc,Brks,.notLast,Ctx,Stk1);
      valis (chLine(OLc,Lc)++EC++TC++[.iResume],Ctx,pushStack(Tp::ltipe,Stk))
    }
    | .cSusp(Lc,T,E,Tp) => valof{
      (EC,_,Stk1) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
      (TC,_,Stk2) = compExp(T,Lc,Brks,.notLast,Ctx,Stk1);
      valis (chLine(OLc,Lc)++EC++TC++[.iSuspend],Ctx,pushStack(Tp::ltipe,Stk))
    }
    | .cRetyr(Lc,T,E,Tp) => valof{
      (EC,_,Stk1) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
      (TC,_,Stk2) = compExp(T,Lc,Brks,.notLast,Ctx,Stk1);
      valis (chLine(OLc,Lc)++EC++TC++[.iRetire],Ctx,.none)
    }
    | .cValof(Lc,A,Tp) => valof{
      Vlbl = defineLbl(Ctx,"Vl");
      Stkx = pshStack(Tp,Stk);
      (AC,_,_) = compAction(A,Lc,
	Brks["$valof"->(((C,S)=>([.iResult(Vlbl)],C,Stkx)),Vlbl)],
	Last,.noMore,Ctx,Stk);
      
      valis (chLine(OLc,Lc)++[.iLbl(Vlbl,.iBlock(stkLvl(Stkx),AC))],Ctx,Stkx)
    }
    |  C where isCond(C) => valof{
      Ok = defineLbl(Ctx,"Ok");
      Fl = defineLbl(Ctx,"Fl");
      Stk0 = pshStack(boolType,Stk);
      (CC,_Ctx1) = compCond(C,OLc,Fl,Brks,Ctx,Stk);
      Lvl = stkLvl(Stk);
      valis ([.iLbl(Ok,.iBlock(Lvl+1,
	      [.iLbl(Fl,.iBlock(Lvl,
		    CC++[.iLdC(trueEnum),.iBreak(Ok)])),
		.iLdC(falseEnum),.iBreak(Ok)]))],Ctx,Stk0)
    }
    | C => valof{
      reportError("cannot compile expression $(C)",locOf(C));
      valis ([],Ctx,Stk)
    }
  }

  compEscape:(option[locn],option[locn],string,cons[cExp],tipe,
    breakLvls,tailMode,codeCtx,stack) => compReturn.
  compEscape(OLc,Lc,Nm,Args,Tp,Brks,Last,Ctx,Stk) => valof{
    (ArgCode,_,Stka) = compExps(Args,Lc,Brks,Ctx,Stk);
    Stk1 = pshStack(Tp,Stk);

    if (ITp,Ins,Frm)?=intrinsic(Nm) then{
      if isThrowingType(ITp) then{
	if (_,XLbl) ?= Brks["$try"] then{
	  valis genReturn(Last,chLine(OLc,Lc)++ArgCode++
	    [Ins(XLbl)]++(Frm??[frameIns(Stk1)]||[]),Ctx,Stk1)
	} else{
	  reportError("invoke throwing escape: #(Nm) outside a try scope",Lc);
	  valis ([],Ctx,Stk1)
	}
      } else {
	valis genReturn(Last,chLine(OLc,Lc)++ArgCode++
	  [Ins("")]++(Frm??[frameIns(Stk1)]||[]),Ctx,Stk1)
      }
    } else if ETp?=escapeType(Nm) && isThrowingType(ETp) then{
      if (_,XLbl) ?= Brks["$try"] then{
	valis genReturn(Last,chLine(OLc,Lc)++ArgCode++
	  [.iXEscape(Nm,XLbl),frameIns(Stk1)],Ctx,Stk1)
      } else {
	reportError("invoke throwing escape: #(Nm) outside a try scope",Lc);
	valis ([],Ctx,Stk1)
      }
    } else {
      valis genReturn(Last,chLine(OLc,Lc)++
	ArgCode++[.iEscape(Nm),frameIns(Stk1)],Ctx,Stk1)
    }
  }

  loadSrc:(cV) => (option[locn],codeCtx) => multi[assemOp].
  loadSrc(.cV(Vr,Tp)) => (Lc,Ctx) => valof{
    if Loc?=locateVar(Vr,Ctx) then {
      valis case Loc in {
	| .argVar(Off,_) => [.iLdA(Off)]
	| .lclVar(Nm,_) => [.iLdL(Nm)]
	| .glbVar(Nm,_) => [.iLdG(Nm)]
	| .glbFun(Nm,_) => [.iLdC(.symb(Nm))]
      }
    } else {
      reportError("cannot locate variable $(Vr)\:$(Tp)",Lc);
      valis [.iLdV]
    }
  }

  compVar:(cV,option[locn],tailMode,codeCtx,stack) => compReturn.
  compVar(.cV(Vr,Tp),Lc,Last,Ctx,Stk) => valof{
    if Loc?=locateVar(Vr,Ctx) then {
      valis case Loc in {
	| .argVar(Off,_) => genReturn(Last,[.iLdA(Off)],Ctx,pshStack(Tp,Stk))
	| .lclVar(Nm,_) => genReturn(Last,[.iLdL(Nm)],Ctx,pshStack(Tp,Stk))
	| .glbVar(Nm,_) => genReturn(Last,[.iLdG(Nm)],Ctx,pshStack(Tp,Stk))
	| .glbFun(Nm,_) => genReturn(Last,[.iLdC(.symb(Nm))],Ctx,pshStack(Tp,Stk))
      }
    } else {
      reportError("cannot compile variable $(Vr)\:$(Tp)",Lc);
      valis ([.iLdV],Ctx,pshStack(Tp,Stk))
    }
  }

    -- Expressions are evaluated in reverse order
  compExps:(cons[cExp],option[locn],breakLvls,codeCtx,stack) => compReturn.
  compExps([],_,_,Ctx,Stk)=>([],Ctx,Stk).
  compExps([Exp,..Es],Lc,Brks,Ctx,Stk)=> valof{
    (Rest,_,Stk1) = compExps(Es,locOf(Exp),Brks,Ctx,Stk);
    (EC,_,Stk2) = compExp(Exp,Lc,Brks,.notLast,Ctx,Stk1);
    valis (Rest++EC,Ctx,Stk2)
  }

  compCond:(cExp,option[locn],assemLbl,breakLvls,codeCtx,stack) => (multi[assemOp],codeCtx).
  compCond(C,OLc,Fail,Brks,Ctx,Stk) => case C in {
    | .cTerm(_,"true",[],_) => ([],Ctx)
    | .cTerm(_,"false",[],_) => ([.iBreak(Fail)],Ctx)
    | .cCnj(Lc,L,R) => valof{
      (LC,Ctxa) = compCond(L,Lc,Fail,Brks,Ctx,Stk);
      (RC,Ctxb) = compCond(R,Lc,Fail,Brks,Ctxa,Stk);
      valis (chLine(OLc,Lc)++LC++RC,Ctxb)
    }
    | .cDsj(Lc,L,R) => valof{
      Fl = defineLbl(Ctx,"Fl");
      Ok = defineLbl(Ctx,"Ok");
      (LC,Ctxa) = compCond(L,Lc,Fl,Brks,Ctx,Stk);
      (RC,Ctxb) = compCond(R,Lc,Fail,Brks,Ctx,Stk);

      Lvl = stkLvl(Stk);

      valis (chLine(OLc,Lc)++[.iLbl(Ok,.iBlock(Lvl,
	      [.iLbl(Fl,.iBlock(Lvl,LC++[.iBreak(Ok)]))]++RC
	      ++[.iBreak(Ok)]))],mergeCtx(Ctxa,Ctxb))
    }
    | .cNeg(Lc,R) => compNegated(R,OLc,Fail,Brks,Ctx,Stk)
    | .cCnd(Lc,T,L,R) => valof{
      Ok = defineLbl(Ctx,"Ok");
      Fl = defineLbl(Ctx,"El");
      (TC,Ctx1) = compCond(T,Lc,Fl,Brks,Ctx,Stk);
      (LC,Ctxa) = compCond(L,Lc,Fail,Brks,Ctx1,Stk);
      (RC,Ctxb) = compCond(R,Lc,Fail,Brks,Ctx,Stk);
      Lvl = stkLvl(Stk);
      valis (chLine(OLc,Lc)++
	[.iLbl(Ok,.iBlock(Lvl,[.iLbl(Fl,
		    .iBlock(Lvl,TC++LC++[.iBreak(Ok)]))]
		++RC++[.iBreak(Ok)]))],mergeCtx(Ctxa,Ctxb))
    }
    | .cMatch(Lc,Ptn,Exp) => valof{
      (EC,_,Stk0) = compExp(Exp,Lc,Brks,.notLast,Ctx,Stk);
      (PC,Ctx1,_) = compPtn(Ptn,Lc,Fail,Brks,Ctx,Stk0);
      valis (chLine(OLc,Lc)++EC++PC,Ctx1)
    }
    | Exp default => valof{
      (EC,_,_) = compExp(Exp,OLc,Brks,.notLast,Ctx,Stk);
      valis (EC++[.iIfNot(Fail)],Ctx)
    }
  }

  compNegated:(cExp,option[locn],assemLbl,breakLvls,codeCtx,stack) => (multi[assemOp],codeCtx).
  compNegated(C,OLc,Fail,Brks,Ctx,Stk) => case C in {
    | .cTerm(_,"true",[],_) => ([.iBreak(Fail)],Ctx)
    | .cTerm(_,"false",[],_) => ([],Ctx)
    | .cCnj(Lc,L,R) => compCond(.cDsj(Lc,.cNeg(Lc,L),.cNeg(Lc,R)),OLc,Fail,Brks,Ctx,Stk)
    | .cDsj(Lc,L,R) => compCond(.cCnj(Lc,.cNeg(Lc,L),.cNeg(Lc,R)),OLc,Fail,Brks,Ctx,Stk)
    | .cNeg(_Lc,I) => compCond(I,OLc,Fail,Brks,Ctx,Stk)
    | .cCnd(Lc,T,L,R) => compCond(.cCnd(Lc,T,R,L),OLc,Fail,Brks,Ctx,Stk)
    | .cMatch(Lc,Ptn,Exp) => valof{
      Ok = defineLbl(Ctx,"Ok");
      (EC,_,Stk0) = compExp(Exp,Lc,Brks,.notLast,Ctx,Stk);
      (PC,Ctx1,_) = compPtn(Ptn,Lc,Ok,Brks,Ctx,Stk0);
      valis (chLine(OLc,Lc)++
	[.iLbl(Ok,.iBlock(stkLvl(Stk),EC++PC++[.iBreak(Fail)]))],Ctx)
    }
    | Exp default => valof{
      (EC,_,_) = compExp(Exp,OLc,Brks,.notLast,Ctx,Stk);
      valis (EC++[.iIf(Fail)],Ctx)
    }
  }
  
  compAction:(aAction,option[locn],breakLvls,tailMode,tailMode,codeCtx,stack) => compReturn.
  compAction(A,OLc,Brks,Last,Next,Ctx,Stk) => case A in {
    | .aNop(_Lc) => ([],Ctx,Stk)
    | .aSeq(Lc,L,R) => valof{
      (LC,Ctx0,_) = compAction(L,Lc,Brks,.notLast,.notLast,Ctx,Stk);
      (RC,Ctx1,_) = compAction(R,Lc,Brks,Last,Next,Ctx0,Stk);
      valis (LC++RC,Ctx1,Stk)
    }
    | .aLbld(Lc,Lb,LbldA) => valof{
      Ex = defineLbl(Ctx,Lb);
      LBrks = Brks[Lb->(((C,S0)=>([.iBreak(Ex)],C,Stk)),Ex)];
      (LC,_,_) = compAction(LbldA,Lc,LBrks,Last,.notLast,Ctx,Stk);
      valis(chLine(OLc,Lc)++[.iLbl(Ex,
	    .iBlock(stkLvl(Stk),LC++[.iBreak(Ex)]))],Ctx,Stk)
    }
    | .aBreak(Lc,Lb) => valof{
      if (XCont,_)?=Brks[Lb] then{
	valis XCont(Ctx,Stk)
      }
      else{
	reportError("unknown break label $(Lb)",Lc);
	valis ([],Ctx,Stk)
      }
    }
    | .aValis(Lc,E) => valof{
      (VC,_,Stk0) = compExp(E,Lc,Brks,Last,Ctx,Stk);
      if (XF,_) ?= Brks["$valof"] then{
	(XC,C,_) = XF(Ctx,Stk0);
	valis (chLine(OLc,Lc)++VC++XC,C,.none)
      }
      else{
	reportError("not in scope of valof",Lc);
	valis ([],Ctx,Stk)
      }
    }
    | .aThrw(Lc,E) => valof{
      if (_,Lbl) ?= Brks["$try"] then{
	(EC,_,Stk1) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
	valis (chLine(OLc,Lc)++EC++[.iResult(Lbl)],Ctx,.none)
      } else{
	reportError("throw outside of try scope",Lc);
	valis ([],Ctx,Stk)
      }
    }
    | .aDo(Lc,E) => valof{
      case Next in {
	| .noMore => valis compExp(E,Lc,Brks,Last,Ctx,Stk)
	| .notLast => {
	  (EC,_,Stk0) = compExp(E,Lc,Brks,Last,Ctx,Stk);
	  valis (EC++resetStack([|Stk|],Stk0),Ctx,Stk)
	}
      }
    }
    | .aDefn(Lc,P,E) => valof{
      (EC,_,Stk0) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);

      if .cVar(_,.cV(Nm,Tp)).=P then{
	Ctx1 = defineLclVar(Nm,Tp,Ctx);
	valis (chLine(OLc,Lc)++EC++[.iStL(Nm)],Ctx1,Stk)
      } else{
	Ab = defineLbl(Ctx,"Ab");
	Ok = defineLbl(Ctx,"Ok");
	(PC,Ctx1,_) = compPtn(P,Lc,Ab,Brks,Ctx,Stk0);
	valis (chLine(OLc,Lc)++[.iLbl(Ok,
	      .iBlock(stkLvl(Stk),[.iLbl(Ab,
		    .iBlock(stkLvl(Stk),EC++PC++[.iBreak(Ok)]))]
		  ++compAbort(Lc,"definition failed",Ctx)))],Ctx1,Stk)
      }
    }
    | .aMatch(Lc,P,E) => valof{
      (EC,_,Stk0) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
      Ab = defineLbl(Ctx,"Ab");
      Ok = defineLbl(Ctx,"Ok");
      (PC,Ctx1,_) = compPtn(P,Lc,Ab,Brks,Ctx,Stk0);
      valis (chLine(OLc,Lc)++[.iLbl(Ok,
	    .iBlock(stkLvl(Stk),[.iLbl(Ab,
		  .iBlock(stkLvl(Stk),EC++PC++[.iBreak(Ok)]))]++
	      compAbort(Lc,"match failed",Ctx)))],Ctx1,Stk)
    }
    | .aAsgn(Lc,P,E) => valof{
      (EC,_,Stk0) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
      (PC,_,Stk1) = compExp(P,Lc,Brks,.notLast,Ctx,Stk0);
      valis (chLine(OLc,Lc)++EC++PC++[.iAssign],Ctx,Stk)
    }
    | .aSetNth(Lc,T,Ix,E) => valof{
      (EC,_,Stk0) = compExp(E,Lc,Brks,.notLast,Ctx,Stk);
      (TC,_,Stk1) = compExp(T,Lc,Brks,.notLast,Ctx,Stk0);
      valis (chLine(OLc,Lc)++EC++TC++[.iStNth(Ix)],Ctx,Stk)
    }
    | .aCase(Lc,G,Cs,D) => case Next in {
      | .noMore => compCase(Lc,G,stkLvl(Stk)+1,Cs,D,
	(AA,LL,BB,RR,CC,SS) => compAction(AA,LL,BB,RR,.notLast,CC,SS),
	Brks,Last,Ctx,Stk)
      | .notLast => compCase(Lc,G,stkLvl(Stk),Cs,D,
	(AA,LL,BB,RR,CC,SS) => compAction(AA,LL,BB,RR,.notLast,CC,SS),
	Brks,Last,Ctx,Stk)
    }
    | .aIftte(Lc,G,L,R) => valof{
      Fl = defineLbl(Ctx,"Fl");
      Ok = defineLbl(Ctx,"Ok");
      (CC,Ctx1) = compCond(G,Lc,Fl,Brks,Ctx,Stk);
      (LC,Ctxa,Stka) = compAction(L,Lc,Brks,Last,Next,Ctx1,Stk);
      (RC,Ctxb,Stkb) = compAction(R,Lc,Brks,Last,Next,Ctx,Stk);
      valis (chLine(OLc,Lc)++
	[.iLbl(Ok,.iBlock(stkLvl(Stk),
	      [.iLbl(Fl,.iBlock(stkLvl(Stk),
		    CC++LC++[.iBreak(Ok)]))]++
	      RC++[.iBreak(Ok)]))],mergeCtx(Ctxa,Ctxb),reconcileStack(Stka,Stkb))
    }
    | .aWhile(Lc,G,B) => valof{
      Lp = defineLbl(Ctx,"Lp");
      Done = defineLbl(Ctx,"Done");

      (GC,Ctx1) = compCond(G,Lc,Done,Brks,Ctx,Stk);
      (BC,_,_) = compAction(B,Lc,Brks,.notLast,.notLast,Ctx1,Stk);

      valis ([.iLbl(Done,
	    .iBlock(stkLvl(Stk),[.iLbl(Lp,
		    .iBlock(stkLvl(Stk),
		      GC++BC++[.iLoop(Lp)]))]))],Ctx,Stk)
    }
    |.aLtt(Lc,.cV(Vr,VTp),Val,Bnd) => valof{
      Ctx1 = defineLclVar(Vr,VTp,Ctx);
      (VV,_,Stk1) = compExp(Val,Lc,Brks,.notLast,Ctx,Stk);
      (BB,Ctx2,_) = compAction(Bnd,Lc,Brks,Last,Next,Ctx1,Stk);
      valis (chLine(OLc,Lc)++VV++[.iStL(Vr)]++BB,Ctx2,Stk)
    }
    | .aTry(Lc,B,.cVar(_,.cV(Er,ETp)),H) => valof{
      if traceCodegen! then
	showMsg("compiling try catch @$(Lc), Er=$(Er)");

      Ok = defineLbl(Ctx,"Ok");
      TrX = defineLbl(Ctx,"TrX");
      Stkx = pshStack(ETp,Stk);
      Ctx1 = defineLclVar(Er,ETp,Ctx);

      TBrks = Brks["$try" -> (((C,S)=>([.iBreak(TrX)],C,.none)),TrX)];

      (BC,_,Stka) = compAction(B,Lc,TBrks,.notLast,.notLast,Ctx1,Stk);
      (HC,_,Stkb) = compAction(H,Lc,Brks,Last,.notLast,Ctx,Stk);

      if ~reconcileable(Stka,Stkb) then
	reportError("cannot reconcile try exp $(B) with handler $(H)",Lc);
	  
      RLvl = stkLvl(Stk);

      valis (chLine(OLc,Lc)++
	[.iLbl(Ok,.iBlock(RLvl,
	      [.iLbl(TrX,.iBlock(RLvl+1,BC++[.iBreak(Ok)])),
		.iStL(Er)]++HC++[.iBreak(Ok)]))],
	Ctx,reconcileStack(Stka,Stkb))
    }
    | .aAbort(Lc,Msg) => (compAbort(Lc,Msg,Ctx),Ctx,.none)
    | _ default => valof{
      reportError("cannot compile action $(A)",locOf(A));
      valis ([],Ctx,.none)
    }
  }

  all e ~~ caseHandler[e] ~> (e,option[locn],breakLvls,tailMode,codeCtx,stack)=>compReturn.

  compCase:all e ~~ display[e] |:
    (option[locn],cExp,integer,cons[cCase[e]],e,
    caseHandler[e],breakLvls,tailMode,codeCtx,stack) => compReturn.
  compCase(Lc,Gv,OkLvl,Cases,Deflt,Hndlr,Brks,Last,Ctx,Stk) => valof{
    if traceCodegen! then
      showMsg("compiling case @$(Lc), Gov=$(Gv), Deflt=$(Deflt), Cases=$(Cases)");
    Df = defineLbl(Ctx,"Df");
    Ok = defineLbl(Ctx,"Ok");
    Lvl = stkLvl(Stk);
    (GVar,GC,Ctx0,Stk0) = compGVExp(Gv,Lc,Brks,Ctx,Stk);
    
    (Table,Max) = genCaseTable(Cases);

    (DC,Ctxd,Stkd) = Hndlr(Deflt,Lc,Brks,Last,Ctx,Stk);

    CaseIns = (intType==deRef(typeOf(Gv)) ?? .iICase(Max) || .iCase(Max));

    (CC,Ctxc,Stkc) = compCases(Table,0,Max,GVar,Ok,Df,Hndlr,Brks,Last,[CaseIns],Ctx,Stk);

    if ~reconcileable(Stkc,Stkd) then
      reportError("cannot reconcile cases' stack $(Cases) with default $(Deflt)",Lc);

    valis ([.iLbl(Ok,.iBlock(OkLvl,
	    GC++[.iLbl(Df,.iBlock(Lvl,CC))]++DC++[.iBreak(Ok)]))],
      mergeCtx(Ctxc,Ctxd),reconcileStack(Stkd,Stkc))
  }

  compGVExp(.cVar(Lc,V),OLc,Brks,Ctx,Stk) => 
    (V,chLine(OLc,Lc)++loadSrc(V)(Lc,Ctx),Ctx,pshStack(typeOf(V),Stk)).
  compGVExp(E,OLc,Brks,Ctx,Stk) => valof{
    (GC,_,Stkx) = compExp(E,OLc,Brks,.notLast,Ctx,Stk);
    V = genSym("__");
    VTp = typeOf(E);
    Ctx1 = defineLclVar(V,VTp,Ctx);
    valis (.cV(V,VTp),GC++[.iTL(V)],Ctx1,Stkx)
  }

  compCases:all e ~~ display[e] |:
    (cons[csEntry[e]],integer,integer,cV,assemLbl,assemLbl,caseHandler[e],
    breakLvls,tailMode,multi[assemOp],codeCtx,stack) => compReturn.

  compCases([],Ix,Mx,_GVar,_Ok,_Df,_Hndlr,_Brks,_Last,CaseCode,Ctx,Stk) where
      Ix>=Mx => (CaseCode,Ctx,.none).
  compCases([],Ix,Mx,GVar,Ok,Df,Hndlr,Brks,Last,CaseCode,Ctx,Stk) =>
    compCases([],Ix+1,Mx,GVar,Ok,Df,Hndlr,Brks,Last,CaseCode++[.iBreak(Df)],Ctx,Stk).
  compCases([(Ix,Case),..Cs],Ix,Mx,GVar,Ok,Df,Hndlr,Brks,Last,CaseCode,Ctx,Stk) => valof{
    El = defineLbl(Ctx,"El");
    (CSC,Ctxb,Stkb) = compCases(Cs,Ix+1,Mx,GVar,Ok,Df,Hndlr,Brks,Last,CaseCode++[.iBreak(El)],Ctx,Stk);
    (CC,Ctxa,Stka) = compCaseBranch(Case,GVar,Hndlr,Ok,Df,Brks,Last,Ctx,Stk);

    valis ([.iLbl(El,.iBlock(stkLvl(Stk),CSC))]++CC++[.iBreak(Ok)],mergeCtx(Ctxa,Ctxb),reconcileStack(Stka,Stkb))
  }
  compCases([(Iy,Case),..Cs],Ix,Mx,GVar,Ok,Df,Hndlr,Brks,Last,CaseCode,Ctx,Stk) where Ix<Iy =>
    compCases([(Iy,Case),..Cs],Ix+1,Mx,GVar,Ok,Df,Hndlr,Brks,Last,CaseCode++[.iBreak(Df)],Ctx,Stk).

  compCaseBranch:all e ~~ display[e] |:
    (cons[cCase[e]],cV,caseHandler[e],assemLbl,assemLbl,
    breakLvls,tailMode,codeCtx,stack) => compReturn.
  compCaseBranch([(Lc,P,E)],Gv,Hndlr,Ok,Df,Brks,Last,Ctx,Stk) => valof{
    (PC,Ctx1,Stk1) = compPttrn(P,Lc,loadSrc(Gv),Df,Brks,Ctx,Stk);
    (EC,Ctx2,Stk2) = Hndlr(E,Lc,Brks,Last,Ctx1,Stk1);

    valis (chLine(.none,Lc)++PC++EC++[.iBreak(Ok)],Ctx2,Stk2)
  }
  compCaseBranch([],_Gv,_Hndlr,_Ok,Df,_Brks,_Last,Ctx,_Stk) =>
    ([.iBreak(Df)],Ctx,.none).
  compCaseBranch([(Lc,P,E),..Cs],Gv,Hndlr,Ok,Df,Brks,Last,Ctx,Stk) => valof{
    Fl = defineLbl(Ctx,"Fl");
    (PC,Ctx1,Stk1) = compPttrn(P,Lc,loadSrc(Gv),Fl,Brks,Ctx,Stk);
    (EC,Ctx2,Stk2) = Hndlr(E,Lc,Brks,Last,Ctx1,Stk1);
    (CC,CCtx,Stkx) = compCaseBranch(Cs,Gv,Hndlr,Ok,Df,Brks,Last,Ctx,Stk);
    valis (chLine(.none,Lc)++[.iLbl(Fl,.iBlock(stkLvl(Stk),
	    PC++EC++[.iBreak(Ok)]))]++CC,
      mergeCtx(CCtx,Ctx2),
      reconcileStack(Stk2,Stkx))
  }
    
  all e ~~ csEntry[e] ~> (integer,cons[cCase[e]]).

  genCaseTable(Cases) where Mx.=nextPrime(size(Cases)) =>
    (sortCases(caseHashes(Cases,Mx)),Mx).

  caseHashes:all e ~~ (cons[cCase[e]],integer)=>cons[(option[locn],cExp,integer,e)].
  caseHashes(Cases,Mx) => (Cases//((Lc,Pt,Ex))=>(Lc,Pt,
      (try caseHash(Pt)%Mx catch {_ => 0}),Ex)).

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

  srcLoader ~> (option[locn],codeCtx)=>multi[assemOp].

  compPttrn:(cExp,option[locn],srcLoader,assemLbl,breakLvls,codeCtx,stack) => compReturn.
  compPttrn(Ptn,OLc,Src,Fail,Brks,Ctx,Stk) => valof{
    VC = Src(OLc,Ctx);
    case Ptn in {
      | .cVar(_,.cV("_",_)) => valis ([],Ctx,Stk)
      | .cVar(Lc,.cV(Vr,Tp)) => {
	if Loc ?= locateVar(Vr,Ctx) then{
	  valis (VC++storeVar(Loc),Ctx,Stk)
	}
	else{
	  Ctx1 = defineLclVar(Vr,Tp,Ctx);
	  valis (VC++storeVar(.lclVar(Vr,Tp::ltipe)),Ctx1,Stk)
	}
      }
      | .cVoid(Lc,_) => valis ([],Ctx,Stk)
      | .cAnon(Lc,_) => valis ([],Ctx,Stk)
      | .cTerm(Lc,Nm,Args,Tp) => {
	(SCde,Ctx2,Stk2) = compPttrnArgs(Args,Lc,0,Src,Fail,Brks,Ctx,Stk);

	valis (chLine(OLc,Lc)++VC++[.iCLbl(.tLbl(Nm,size(Args)),Fail)]++SCde,Ctx2,Stk2)
      }
      | .cInt(_,Ix) => valis (VC++[.iCInt(Ptn::data,Fail)],Ctx,Stk)
      | .cChar(_,Cx) => valis (VC++[.iCChar(Ptn::data,Fail)],Ctx,Stk)
      | .cFlt(_,Dx) => valis (VC++[.iCFlt(Ptn::data,Fail)],Ctx,Stk)
      | _ default => {
	if isGround(Ptn) then
	  valis (VC++[.iCLit(Ptn::data,Fail)],Ctx,Stk)
	else {
	  reportError("uncompilable pattern $(Ptn)",locOf(Ptn));
	  valis ([.iBreak(Fail)],Ctx,Stk)
	}
      }
    }
  }

  ixLoader:(srcLoader,integer) => srcLoader.
  ixLoader(Src,Ix) => (Lc,Ctx) => Src(Lc,Ctx)++[.iNth(Ix)].

  compPttrnArgs:(cons[cExp],option[locn],integer,srcLoader,assemLbl,breakLvls,codeCtx,stack) => compReturn.
  compPttrnArgs(Es,Lc,Ix,Src,Fail,Brks,Ctx,Stk) => case Es in {
    | [] => ([],Ctx,Stk)
    | [A,..As] => valof{
      (AC,Ctx0,Stka) = compPttrn(A,Lc,ixLoader(Src,Ix),Fail,Brks,Ctx,Stk);
      (AsC,Ctx1,Stkb) = compPttrnArgs(As,Lc,Ix+1,Src,Fail,Brks,Ctx0,Stka);
      valis (AC++AsC,Ctx1,Stkb)
    }
  }

  compPtn:(cExp,option[locn],assemLbl,breakLvls,codeCtx,stack) => compReturn.
  compPtn(Ptn,OLc,Fail,Brks,Ctx,Stk) => case Ptn in {
    | .cVar(_,.cV("_",_)) => ([.iDrop],Ctx,dropStack(Stk))
    | .cVar(Lc,.cV(Vr,Tp)) => valof{
      if Loc ?= locateVar(Vr,Ctx) then 
	valis (storeVar(Loc),Ctx,Stk)
      else{
	Ctx1 = defineLclVar(Vr,Tp,Ctx);
	valis (storeVar(.lclVar(Vr,Tp::ltipe)),Ctx1,Stk)
      }
    }
    | .cVoid(Lc,_) => ([.iDrop],Ctx,dropStack(Stk))
    | .cAnon(Lc,_) => ([.iDrop],Ctx,dropStack(Stk))
    | .cTerm(Lc,Nm,Args,Tp) where canFail(Nm,Tp,Ctx) => valof{
      Stk0 = dropStack(Stk);
      V = genSym("__");
      Ctx1 = defineLclVar(V,Tp,Ctx);

      (SCde,Ctx2,Stk2) = compArgPtns(Args,Lc,0,.cV(V,Tp),Fail,Brks,Ctx1,Stk0);

      valis (chLine(OLc,Lc)++[.iTL(V),.iCLbl(.tLbl(Nm,size(Args)),Fail)]++SCde,Ctx2,Stk2)
    }
    | .cTerm(Lc,Nm,Args,Tp) where ~canFail(Nm,Tp,Ctx) => valof{
      Stk0 = dropStack(Stk);
      V = genSym("__");
      Ctx1 = defineLclVar(V,Tp,Ctx);

      (SCde,Ctx2,Stk2) = compArgPtns(Args,Lc,0,.cV(V,Tp),Fail,Brks,Ctx1,Stk0);

      valis (chLine(OLc,Lc)++[.iStL(V),..SCde],Ctx2,Stk2)
    }
    | .cSvDrf(Lc,P,_) => valof{
      (PC,PCxt,Stk0) = compPtn(P,Lc,Fail,Brks,Ctx,Stk);
      valis ([.iLdSav(Fail)]++PC,PCxt,Stk0)
    }
    | .cInt(_,Ix) => ([.iCInt(Ptn::data,Fail)],Ctx,Stk)
    | .cChar(_,Cx) => ([.iCChar(Ptn::data,Fail)],Ctx,Stk)
    | .cFlt(_,Dx) => ([.iCFlt(Ptn::data,Fail)],Ctx,Stk)
    | _ default => ( isGround(Ptn) ??
      ([.iCLit(Ptn::data,Fail)],Ctx,dropStack(Stk)) || valof{
	reportError("uncompilable pattern $(Ptn)",locOf(Ptn));
	valis ([.iBreak(Fail)],Ctx,dropStack(Stk))
      }
    )
  }

  storeVar:(srcLoc) => multi[assemOp].
  storeVar(.lclVar(Off,Tp)) => [.iStL(Off)].
  storeVar(.argVar(Off,Tp)) => [.iDrop].

  compArgPtns:(cons[cExp],option[locn],integer,cV,assemLbl,breakLvls,codeCtx,stack) => compReturn.
  compArgPtns(Es,Lc,Ix,Src,Fail,Brks,Ctx,Stk) => case Es in {
    | [] => ([],Ctx,Stk)
    | [A,..As] => valof{
      (VC,_,Stk0) = compVar(Src,Lc,.notLast,Ctx,Stk);
      (AC,Ctx0,Stka) = compPtn(A,Lc,Fail,Brks,Ctx,Stk0);
      (AsC,Ctx1,Stkb) = compArgPtns(As,Lc,Ix+1,Src,Fail,Brks,Ctx0,Stka);
      valis (VC++[.iNth(Ix)]++AC++AsC,Ctx1,Stkb)
    }
    
  }

  compArgs(Es,Ix,Fail,Brks,Ctx,Stk) => case Es in {
    | [] => ([],Ctx,Stk)
    | [A,..As] => valof{
      (AC,Ctx0,Stka) = (.cVar(Lc,.cV(Nm,Tp)).=A ??
	([],defineArgVar(Nm,Tp,Ix,Ctx),Stk) ||
	valof{
	  (AC,Ctx0,Stka) = compPttrn(A,locOf(A),(_,_)=>[.iLdA(Ix)],Fail,Brks,Ctx,Stk);
	  valis (AC,Ctx0,Stka)
	});
      (AsC,Ctx1,Stkb) = compArgs(As,Ix+1,Fail,Brks,Ctx0,Stka);
      valis (AC++AsC,Ctx1,Stkb)
    }
  }

  compAbort:(option[locn],string,codeCtx) => multi[assemOp].
  compAbort(.some(Lc),Msg,Ctx) => [.iLdC(Lc::data),.iLdC(.strg(Msg)),.iAbort].

  genReturn:(tailMode,multi[assemOp],codeCtx,stack) => compReturn.
  genReturn(.notLast,Cd,Ctx,Stk) => (Cd,Ctx,Stk).
  genReturn(.noMore,Cd,Ctx,Stk) => (Cd++[.iRet],Ctx,.none).

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

  -- reset stack to a target depth
  resetStack:(option[integer],stack) => multi[assemOp].
  resetStack(.some(Dp),.some(Stk)) => case [|Stk|] in {
    | Dp => []
    | Dp1 where Dp == Dp1-1 => [.iDrop]
    | Dp1 where Dp1>Dp => [.iRst(Dp)]
    | Dp1 default => valof{	
	reportTrap("invalid stack height in $(Stk)\:$(Dp1)~$(Dp)");
	valis []
      }
    }.
  resetStack(_,.none) => [].

  pickStack:(option[integer],stack) => multi[assemOp].
  pickStack(.some(Dp),.some(Stk)) => case [|Stk|] in {
    | Dp => []
    | Dp1 where Dp1>Dp => [.iPick(Dp,1)]
    | Dp1 default => valof{	
	reportTrap("invalid stack height in $(Stk)\:$(Dp1)~$(Dp)");
	valis []
      }
    }.
  pickStack(_,.none) => [].

  frameIns:(stack)=>assemOp.
  frameIns(.some(Stk)) => .iFrame(size(Stk)).

  locateVar:(string,codeCtx)=>option[srcLoc].
  locateVar(Nm,Ctx) where (_,Loc) ?= Ctx.vars![Nm] => .some(Loc).
  locateVar(_,_) default => .none.

  defineLclVar:(string,tipe,codeCtx) => codeCtx.
  defineLclVar(Nm,Tp,Ctx) => valof{
    Ctx.vars:=Ctx.vars![Nm->(Tp,.lclVar(Nm,Tp::ltipe))];
    valis Ctx
  }

  defineArgVar:(string,tipe,integer,codeCtx) => codeCtx.
  defineArgVar(Nm,Tp,Ix,Ctx) => valof{
    Ctx.vars:=Ctx.vars![Nm->(Tp,.argVar(Ix,Tp::ltipe))];
    valis Ctx
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
  breakFun ~> (codeCtx,stack)=>(multi[assemOp],codeCtx,stack).
  breakLvls ~> map[string,(breakFun,assemLbl)].

  codeDict ::= codeDict{
    vars : map[string,(tipe,srcLoc)].
    types : map[string,cons[(termLbl,tipe,integer)]].
  }

  codeCtx ::= codeCtx{
    vars : ref map[string,(tipe,srcLoc)].
    lbls : ref integer.  
  }

  emptyCtx:(map[string,(tipe,srcLoc)])=>codeCtx.
  emptyCtx(Glbs) => codeCtx{
    vars = ref Glbs.
    lbls = ref 0.
  }

  mergeCtx:(codeCtx,codeCtx)=>codeCtx.
  mergeCtx(C1,C2) => codeCtx{
    vars = C1.vars.
    lbls = C1.lbls.
  }

  varInfo:(codeCtx) => cons[(string,data)].
  varInfo(Ctx) => { (Nm, Tp::data) | (Nm -> (Tp,_)) in Ctx.vars! }.

  defineLbl:(codeCtx,string)=>assemLbl.
  defineLbl(C,Pr) => valof{
    CurLbl = C.lbls!;
    C.lbls := CurLbl+1;
    valis "#(Pr)$(CurLbl)"
  }

  pushStack:(ltipe,stack) => stack.
  pushStack(Tp,.some(Stk)) => .some([Tp,..Stk]).

  pshStack:(tipe,stack) => stack.
  pshStack(Tp,.some(Stk)) => .some([Tp::ltipe,..Stk]).

  loadStack:(cons[ltipe],stack) => stack.
  loadStack(Tps,.some(Stk)) => .some(Tps++Stk).

  stkLvl:(stack)=>integer.
  stkLvl(.some(Stk)) => size(Stk).
  stkLvl(.none) => -1.

  popTo:all e ~~ (cons[e],integer) => cons[e].
  popTo(LL,D) => let{.
    pop(Ls,Ix) => case Ls in {
      | [] => []
      | [_,..L] where Ix>0 => pop(L,Ix-1)
      | _ where Ix==0 => Ls
    }
  .} in pop(LL,[|LL|]-D).

  implementation display[codeCtx] => {
    disp(C) => "<C $(C.vars!) C>".
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

  flatSig = .funTipe([],.tplTipe([])).
  nearlyFlatSig(T) => .funTipe([],.tplTipe([T])).
  blockSig(Args,Rs) => .funTipe(Args,Rs).

  tailMode ::= .noMore | .notLast.

   implementation equality[tailMode] => {
    .noMore == .noMore => .true.
    .notLast == .notLast => .true.
    _ == _ default => .false.
  }

  implementation display[tailMode] => {
    disp(.noMore) => "noMore".
    disp(.notLast) => "notLast".
  }

  canFail:(string,tipe,codeCtx) => boolean.
  canFail(Nm,_Tp,_Ctx) where isTplLbl(Nm) => .false.
  canFail(_,_,_) default => .true.
}
