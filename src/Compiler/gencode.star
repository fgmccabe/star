star.compiler.gencode{
  import star.
  import star.multi.
  import star.pkg.
  import star.sort.

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
  import star.compiler.ssa.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.data.

  public compProg:(pkg,cons[cDefn],cons[decl])=>cons[codeSegment].
  compProg(Pkg,Defs,Decls) => valof{
    Vars = foldLeft(declGlobal,[],Decls);
    Tps = foldLeft(declType,foldLeft(declType,[],Decls),stdTypes);

    valis compDefs(Defs,Vars,Tps)
  }

  declGlobal(.varDec(_,_,Nm,Tp), Vrs) => Vrs[Nm->(Tp,.glbVar(Nm,Tp))].
  declGlobal(.funDec(_,_,Nm,Tp), Vrs) => Vrs[Nm->(Tp,.glbVar(Nm,Tp))].
  declGlobal(_,Vrs) => Vrs.

  declType:(decl,map[string,indexMap])=>map[string,indexMap].
  declType(.tpeDec(_,Nm,Tp,_,Map),Tps) => Tps[tpName(Tp)->Map].
  declType(_,Tps) => Tps.

  compDefs:(cons[cDefn],map[string,(tipe,srcLoc)],map[string,indexMap])=> cons[codeSegment].
  compDefs(Dfs,Glbs,Tps) => (Dfs//(D)=>genDef(D,Glbs,Tps)).

  genDef:(cDefn,map[string,(tipe,srcLoc)],map[string,indexMap]) => codeSegment.
  genDef(.fnDef(Lc,Nm,Tp,Args,Val),Glbs,Tps) => genFun(Lc,Nm,Tp,Args,Val,Glbs,Tps).
  genDef(.prDef(Lc,Nm,Tp,Args,Val),Glbs,Tps) => genPrc(Lc,Nm,Tp,Args,Val,Glbs,Tps).
  genDef(.glDef(Lc,Nm,Tp,Val),Glbs,Tps) => genGlb(Lc,Nm,Tp,Val,Glbs,Tps).
  genDef(.tpDef(Lc,Tp,TpRl,Index),_,_) => .tipe(Tp,TpRl,Index).
  genDef(.lblDef(_Lc,Lbl,Tp,Ix),_,_) => .struct(Lbl,Tp,Ix).

  identifier ~> string.

  genFun:(option[locn],identifier,tipe,cons[cV],cExp,map[identifier,(tipe,srcLoc)],map[identifier,indexMap]) => codeSegment.
  genFun(Lc,FnNm,Tp,Args,Val,Glbs,Tps) => valof{
    Ctx = emptyCtx(Glbs,Tps);

    if traceCodegen! then
      showMsg("Compile $(.fnDef(Lc,FnNm,Tp,Args,Val))\n");

    AbrtCde = compAbort(Lc,"function: $(FnNm) aborted",Ctx);
    AbrtLbl = defineLbl(Ctx,"Abrt");
    ExLbl = defineLbl(Ctx,"TrExit");
    Brks = ["$abort" -> AbrtLbl,"$try" -> ExLbl];

    Ct1 = declareArgs(Args,Ctx);
    (EC,EV) = bindExpToVar(Val,Lc,Brks,.noMore,Ct1);
    Er = defineTmpVar(typeThrows(Tp),Ct1);

    C0 = [.iEntry(Args//((.cV(ArgNm,_))=>ArgNm),varNms(Ctx))]++
    chLine(.none,Lc)++[.iLbl(AbrtLbl,.iBlock([],
	  [.iLbl(ExLbl,.iBlock([Er],EC++genDbg(Lc,[.iRet(EV)])))]++genDbg(Lc,[.iXRet(Er)]))),..AbrtCde];
    
    Code = .func(.tLbl(FnNm,arity(Tp)),.hardDefinition,Tp::ltipe,varInfo(Ct1),C0);

    if traceCodegen! then{
      showMsg("non-peep code is $(Code)");
    };

    Peeped = peepOptimize(Code);

    if traceCodegen! then
      showMsg("peeped code is $(Peeped)");

--    validateCode(Peeped);

    valis Peeped;
  }

  genPrc:(option[locn],identifier,tipe,cons[cV],aAction,map[identifier,(tipe,srcLoc)],map[identifier,indexMap]) => codeSegment.
  genPrc(Lc,PrNm,Tp,Args,Act,Glbs,Tps) => valof{
    Ctx = emptyCtx(Glbs,Tps);

    if traceCodegen! then
      showMsg("Compile $(.prDef(Lc,PrNm,Tp,Args,Act))\n");

    AbrtCde = compAbort(Lc,"procedure: $(PrNm) aborted",Ctx);
    AbrtLbl = defineLbl(Ctx,"Abrt");
    ExLbl = defineLbl(Ctx,"TrExit");

    Brks = ["$abort" -> AbrtLbl, "$try" -> ExLbl];

    Ct1 = declareArgs(Args,Ctx);
    EC = compAction(Act,Lc,Brks,.noMore,Ct1);
    Er = defineTmpVar(typeThrows(Tp),Ct1);
    
    C0 = [.iEntry(Args//((.cV(Nm,_))=>Nm),varNms(Ctx))]++
    chLine(.none,Lc)++[.iLbl(AbrtLbl,.iBlock([],
	  [.iLbl(ExLbl,.iBlock([Er],EC++genDbg(Lc,[.iRtn])))]++genDbg(Lc,[.iXRet(Er)]))),..AbrtCde];
    
    Code = .func(.tLbl(PrNm,arity(Tp)),.hardDefinition,Tp::ltipe,varInfo(Ct1),C0);

    if traceCodegen! then
      showMsg("non-peep code is $(Code)");
    Peeped = peepOptimize(Code);
    if traceCodegen! then
      showMsg("peeped code is $(Peeped)");

    valis Peeped;
  }

  genGlb:(option[locn],identifier,tipe,cExp,map[identifier,(tipe,srcLoc)],map[identifier,indexMap]) => codeSegment.
  genGlb(Lc,GNm,Tp,Val,Glbs,Tps) => valof{
    Ctx = emptyCtx(Glbs,Tps);

    AbrtCde = compAbort(Lc,"global eval: $(GNm) aborted",Ctx);
    AbrtLbl = defineLbl(Ctx,"Abrt");
    Brks = ["$abort" -> AbrtLbl];

    (GC,GV) = bindExpToVar(Val,Lc,Brks,.notLast,Ctx);
    
    C0 = [.iEntry([],varNms(Ctx))]++chLine(.none,Lc)++
    [.iLbl(AbrtLbl,.iBlock([],GC++[.iSG(GNm,GV)]++genDbg(Lc,[.iRet(GV)])))]
      ++AbrtCde;

    Code = .func(.tLbl(GNm,0),.hardDefinition,Tp::ltipe,varInfo(Ctx),C0);

    if traceCodegen! then
      showMsg("non-peep code is $(Code)");
    Peeped = peepOptimize(Code);
    if traceCodegen! then
      showMsg("peeped code is $(Peeped)");

    valis Peeped;
  }

  compReturn ~> (multi[insOp],cV).

  bindExpToVar:(cExp,option[locn],breakLvls,tailMode,codeCtx) => (multi[insOp],identifier).
  bindExpToVar(.cVar(_,.cV(Nm,Tp)),Lc,Bks,Tail,Ctx) =>
    compIdExp(Nm,Tp,Lc,Bks,Tail,Ctx).
  bindExpToVar(Exp,Lc,Bks,Tail,Ctx) => valof{
    TV = defineTmpVar(typeOf(Exp),Ctx);
    valis (compExp(Exp,Lc,TV,Bks,Tail,Ctx),TV)
  }

  compIdExp(Nm,_Tp,_Lc,_Bks,_Tail,Ctx) where (_,VrSpec) ?= locateVar(Nm,Ctx) =>
    compVar(VrSpec,Ctx).
  compIdExp(Nm,Tp,Lc,_,_,_) => valof{
    reportError("Cannot locate variable '#(Nm)'",Lc);
    valis ([],Nm)
  }

  compVar:(srcLoc,codeCtx) => (multi[insOp],identifier).
  compVar(.argVar(Nm,_),_) => ([],Nm).
  compVar(.lclVar(Nm,_),_) => ([],Nm).
  compVar(.glbVar(Nm,Tp),Ctx) => valof{
    TV = defineTmpVar(Tp,Ctx);
    valis ([.iLG(Nm),.iRSP(TV)],TV)
  }

  bindExpsToVars:(cons[cExp],option[locn],breakLvls,codeCtx) => (multi[insOp],cons[identifier]).
  bindExpsToVars(Exps,Lc,Brks,Ctx) =>
    foldLeft((E,(Es,Vs))=>valof{
      (C,V) = bindExpToVar(E,Lc,Brks,.notLast,Ctx);
      valis (Es++C,Vs++[V])
    },([],[]),Exps).

  compExp:(cExp,option[locn],identifier,breakLvls,tailMode,codeCtx) => multi[insOp].
  compExp(.cUnrch(Lc,_),_,_,_,_,Ctx) => compAbort(Lc,"unreachable",Ctx).
  compExp(.cAbort(Lc,Msg,_),_,_,_,_,Ctx) => compAbort(Lc,Msg,Ctx).
  compExp(E,_Lc,Vr,Brks,Last,Ctx) where isGround(E) => [.iMC(Vr,E:?data)].
  compExp(.cVar(_,.cV(S,_)),_Lc,D,Brks,Last,Ctx) => [.iMv(D,S)].
  compExp(.cVoid(_),_Lc,Vr,Brks,Last,Ctx) => [.iMC(Vr,.symb(.tLbl("void",0)))].
  compExp(.cAnon(_,_),_Lc,Vr,Brks,Last,Ctx) => [.iMC(Vr,.symb(.tLbl("void",0)))].
  compExp(.cTerm(_,Nm,Args,_),Lc,Vr,Brks,Last,Ctx) => valof{
    (AC,AV) = bindExpsToVars(Args,Lc,Brks,Ctx);
    valis AC++[.iAlloc(.tLbl(Nm,[|Args|]),Vr,AV)]
  }
  compExp(.cCall(Lc,Nm,Args,Tp),OLc,Vr,Brks,Last,Ctx) => valof{
    if isEscape(Nm) then
      valis compEscape(OLc,Lc,Nm,Args,Tp,Vr,Brks,Last,Ctx)
    else {
      (ArgCode,AV) = bindExpsToVars(Args,Lc,Brks,Ctx);
      if .noMore.=Last then
	valis chLine(OLc,Lc)++ArgCode++
	genDbg(Lc,[.iTCall(.tLbl(Nm,[|Args|]),AV)])
      else
      valis chLine(OLc,Lc)++ArgCode++
	genDbg(Lc,[.iCall(.tLbl(Nm,[|Args|]),AV),.iRSP(Vr)])
    }
  }
  compExp(.cXCall(Lc,Nm,Args,Tp,_),OLc,Vr,Brks,Last,Ctx) => valof{
    if isEscape(Nm) then{
      valis compEscape(OLc,Lc,Nm,Args,Tp,Vr,Brks,Last,Ctx)
    }
    else {
      (ArgCode,AV) = bindExpsToVars(Args,Lc,Brks,Ctx);

      if XLbl ?= Brks["$try"] then{
	valis chLine(OLc,Lc)++ArgCode++
	genDbg(Lc,[.iCall(.tLbl(Nm,[|Args|]),AV),.iRSX(XLbl,Vr)])
      } else{
	reportError("invoke throwing function: #(Nm) outside a try scope",Lc);
	valis []
      }
    }
  }
  compExp(.cOCall(Lc,Lm,Args,_Tp),OLc,Vr,Brks,Last,Ctx) => valof{
    (ArgCode,AV) = bindExpsToVars(Args,Lc,Brks,Ctx);
    (LmCode,LV) = bindExpToVar(Lm,OLc,Brks,.notLast,Ctx);
    if .noMore.=Last then
      valis chLine(OLc,Lc)++ArgCode++LmCode++genDbg(Lc,[.iTOCall(LV,AV)])
    else
    valis chLine(OLc,Lc)++ArgCode++LmCode++genDbg(Lc,[.iOCall(LV,AV),.iRSP(Vr)])
  }
  compExp(.cXOCall(Lc,Lm,Args,_Tp,_ErTp),OLc,Vr,Brks,Last,Ctx) => valof{
    (ArgCode,AV) = bindExpsToVars(Args,Lc,Brks,Ctx);
    (LmCode,LV) = bindExpToVar(Lm,OLc,Brks,.notLast,Ctx);

    if XLbl ?= Brks["$try"] then{
      valis chLine(OLc,Lc)++ArgCode++LmCode++
      genDbg(Lc,[.iOCall(LV,AV),.iRSX(XLbl,Vr)])
    } else{
      reportError("invoke throwing function: $(Lm) outside a try scope",Lc);
      valis []
    }
  }
  compExp(.cClos(Lc,Nm,Ar,F,Tp),OLc,Vr,Brks,_Last,Ctx) => valof{
    (FreeCode,FV) = bindExpToVar(F,Lc,Brks,.notLast,Ctx);
    valis chLine(OLc,Lc)++FreeCode++[.iClosure(.tLbl(Nm,Ar),Vr,FV)]
  }
  compExp(.cSv(Lc,Tp),OLc,Vr,Brks,_Last,Ctx) => [.iSav(Vr)].
  compExp(.cSvSet(Lc,Th,Vl),OLc,Vr,Brks,_Last,Ctx) => valof{
    (ThCode,ThVr) = bindExpToVar(Th,Lc,Brks,.notLast,Ctx);
    (VlCode,VlVr) = bindExpToVar(Vl,Lc,Brks,.notLast,Ctx);
    valis chLine(OLc,Lc)++VlCode++ThCode++[.iStSav(Vr,ThVr,VlVr)]
  }
  compExp(.cCel(Lc,Vl,_Tp),OLc,Vr,Brks,_Last,Ctx) => valof{
    (VlCode,VlVr) = bindExpToVar(Vl,Lc,Brks,.notLast,Ctx);
    valis chLine(OLc,Lc)++VlCode++[.iCell(Vr,VlVr)]
  }
  compExp(.cGet(Lc,Vl,_Tp),OLc,Vr,Brks,_Last,Ctx) => valof{
    (ClCode,ClVr) = bindExpToVar(Vl,Lc,Brks,.notLast,Ctx);
    valis chLine(OLc,Lc)++ClCode++[.iGet(Vr,ClVr)]
  }
  compExp(.cNth(Lc,E,Ix,_),OLc,Vr,Brks,_Last,Ctx) => valof{
    (ECode,EVr) = bindExpToVar(E,Lc,Brks,.notLast,Ctx);
    valis chLine(OLc,Lc)++ECode++[.iNth(Vr,Ix,EVr)]
  }
  compExp(.cSetNth(Lc,R,Ix,V),OLc,Vr,Brks,_Last,Ctx) => valof{
    RCode = compExp(R,Lc,Vr,Brks,.notLast,Ctx);
    (VCode,VlVr) = bindExpToVar(V,Lc,Brks,.notLast,Ctx);
    valis chLine(OLc,Lc)++RCode++VCode++[.iStNth(Vr,Ix,VlVr)]
  }
  compExp(.cSeq(Lc,L,R),OLc,Vr,Brks,_Last,Ctx) => valof{
    (LCode,_) = bindExpToVar(L,Lc,Brks,.notLast,Ctx);
    RCode = compExp(R,Lc,Vr,Brks,.notLast,Ctx);
    valis chLine(OLc,Lc)++LCode++RCode
  }
  compExp(.cCnd(Lc,G,A,B),OLc,Vr,Brks,Last,Ctx) => valof{
    Fl = defineLbl(Ctx,"Fl");
    Ok = defineLbl(Ctx,"CnOk");
    CC = compCond(G,Lc,Fl,Brks,Ctx);
    (AC,AV) = bindExpToVar(A,Lc,Brks,Last,Ctx);
    (BC,BV) = bindExpToVar(B,Lc,Brks,Last,Ctx);
    valis chLine(OLc,Lc)++[.iLbl(Ok,.iBlock([Vr],
	  [.iLbl(Fl,.iBlock([],CC++AC++[.iResult(Ok,[AV])]))]++
	  BC++[.iResult(Ok,[BV])]))]
  }
  compExp(.cCase(Lc,Gov,Cases,Deflt,Tp),OLc,Vr,Brks,Last,Ctx) => valof{
    Ok = defineLbl(Ctx,"Ok");
    valis [.iLbl(Ok,.iBlock([Vr],
	  compCase(Lc,Gov,Cases,Deflt,
	    (E,L,B,T,C) => valof{
	      (EC,EV) = bindExpToVar(E,L,B,T,C);
	      valis EC++[.iResult(Ok,[EV])]
	    },Brks,Last,Ctx)))]
  }
  compExp(.cIxCase(Lc,Gov,Cases,Deflt,Tp),OLc,Vr,Brks,Last,Ctx) => valof{
    Ok = defineLbl(Ctx,"Ok");
    valis [.iLbl(Ok,.iBlock([Vr],
	  compIndexCase(Lc,Gov,Cases,Deflt,
	    (E,L,B,T,C) => valof{
	      (EC,EV) = bindExpToVar(E,L,B,T,C);
	      valis EC++[.iResult(Ok,[EV])]
	    },Brks,Last,Ctx)))]
  }
  compExp(.cLtt(Lc,.cV(LVr,LVTp),Val,Bnd),OLc,Vr,Brks,Last,Ctx) => valof{
    defineLclVar(LVr,LVTp,Ctx);
    VC = compExp(Val,Lc,LVr,Brks,.notLast,Ctx);
    BC = compExp(Bnd,Lc,Vr,Brks,Last,Ctx);
    valis chLine(OLc,Lc)++VC++BC
  }
  compExp(.cTry(Lc,B,.cVar(_,.cV(Er,ETp)),H,Tp),OLc,Vr,Brks,Last,Ctx) => valof{
    if traceCodegen! then{
      showMsg("compiling try catch @$(Lc), $(B), catch $(H)");
    }

    Ok = defineLbl(Ctx,"Ok");
    TrX = defineLbl(Ctx,"TrX");

    TBrks = Brks["$try" -> TrX];

    (BC,BV) = bindExpToVar(B,Lc,TBrks,.notLast,Ctx);
    defineLclVar(Er,ETp,Ctx);
    (HC,HV) = bindExpToVar(H,Lc,Brks,Last,Ctx);

    valis chLine(OLc,Lc)++[.iLbl(Ok,.iBlock([Vr],
	  [.iLbl(TrX,.iBlock([Er],
		BC++[.iResult(Ok,[BV])]))]++
	  HC++[.iResult(Ok,[HV])]))]
  }
  compExp(.cThrw(Lc,E,_),OLc,_Vr,Brks,Last,Ctx) => valof{
    if Lbl ?= Brks["$try"] then{
      (EC,EV) = bindExpToVar(E,Lc,Brks,.notLast,Ctx);
      valis chLine(OLc,Lc)++EC++[.iResult(Lbl,[EV])]
    } else{
      reportError("throw outside of try scope",Lc);
      valis []
    }
  }
  compExp(.cResum(Lc,T,E,Tp),OLc,Vr,Brks,_Last,Ctx) => valof{
    (EC,EV) = bindExpToVar(E,Lc,Brks,.notLast,Ctx);
    (TC,TV) = bindExpToVar(T,Lc,Brks,.notLast,Ctx);
    valis chLine(OLc,Lc)++EC++TC++[.iResume(TV,EV),.iRSP(Vr)]
  }
  compExp(.cSusp(Lc,T,E,Tp),OLc,Vr,Brks,_Last,Ctx) => valof{
    (EC,EV) = bindExpToVar(E,Lc,Brks,.notLast,Ctx);
    (TC,TV) = bindExpToVar(T,Lc,Brks,.notLast,Ctx);
    valis chLine(OLc,Lc)++EC++TC++[.iSuspend(TV,EV),.iRSP(Vr)]
  }
  compExp(.cRetyr(Lc,T,E,Tp),OLc,_,Brks,_,Ctx) => valof{
    (EC,EV) = bindExpToVar(E,Lc,Brks,.notLast,Ctx);
    (TC,TV) = bindExpToVar(T,Lc,Brks,.notLast,Ctx);
    valis chLine(OLc,Lc)++EC++TC++[.iRetire(TV,EV)]
  }
  compExp(.cValof(Lc,A,Tp),OLc,Vr,Brks,Last,Ctx) => valof{
    VLbl = defineLbl(Ctx,"Vl");
    AC = compAction(A,Lc,Brks["$valof"->VLbl],Last,Ctx);
    valis chLine(OLc,Lc)++[.iLbl(VLbl,.iBlock([Vr],AC))]
  }
  compExp(C where isCond(C),OLc,Vr,Brks,Last,Ctx) => valof{
    Ok = defineLbl(Ctx,"CndOk");
    Fl = defineLbl(Ctx,"Fl");
    CC = compCond(C,OLc,Fl,Brks,Ctx);
    OkVr = defineTmpVar(boolType,Ctx);
    FlVr = defineTmpVar(boolType,Ctx);
    valis [.iLbl(Ok,.iBlock([Vr],
	  [.iLbl(Fl,.iBlock([],CC++[.iMC(OkVr,.symb(.tLbl("true",0))),.iResult(Ok,[OkVr])])),
	    .iMC(OkVr,.symb(.tLbl("false",0))),.iResult(Ok,[OkVr])]))]
  }
  compExp(C,Lc,_,_,_,_) => valof{
    reportError("cannot compile expression $(C)",Lc);
    valis []
  }

  compEscape:(option[locn],option[locn],identifier,cons[cExp],tipe,
    identifier,breakLvls,tailMode,codeCtx) => multi[insOp].
  compEscape(OLc,Lc,Nm,Args,Tp,Vr,Brks,Last,Ctx) => valof{
    (ArgCode,AV) = bindExpsToVars(Args,Lc,Brks,Ctx);

    if (ITp,Ins)?=intrinsic(Nm) then{
      if traceCodegen! then
	showMsg("Compile intrinsic #(Nm)$(Args)\:$(ITp)");

      if isThrowingType(ITp) then{
	if XLbl ?= Brks["$try"] then{
	  valis chLine(OLc,Lc)++ArgCode++[Ins([Vr,..AV],XLbl)]
	} else{
	  reportError("invoke throwing escape: #(Nm) outside a try scope",Lc);
	  valis []
	}
      } else {
	valis chLine(OLc,Lc)++ArgCode++[Ins([Vr,..AV],"")]
      }
    } else if ETp?=escapeType(Nm) && isThrowingType(ETp) then{
      if XLbl ?= Brks["$try"] then{
	valis ArgCode++[.iEscape(Nm,AV),.iRSX(XLbl,Vr)]
      } else {
	reportError("invoke throwing escape: #(Nm) outside a try scope",Lc);
	valis []
      }
    } else {
      valis chLine(OLc,Lc)++ArgCode++[.iEscape(Nm,AV),.iRSP(Vr)]
    }
  }

  compCond:(cExp,option[locn],assemLbl,breakLvls,codeCtx) => multi[insOp].
  compCond(C,OLc,Fail,Brks,Ctx) => case C in {
    | .cTerm(_,"true",[],_) => []
    | .cTerm(_,"false",[],_) => [.iBreak(Fail)]
    | .cCnj(Lc,L,R) => valof{
      LC = compCond(L,Lc,Fail,Brks,Ctx);
      RC = compCond(R,Lc,Fail,Brks,Ctx);
      valis chLine(OLc,Lc)++LC++RC
    }
    | .cDsj(Lc,L,R) => valof{
      Fl = defineLbl(Ctx,"Fl");
      Ok = defineLbl(Ctx,"DsjOk");
      LC = compCond(L,Lc,Fl,Brks,Ctx);
      RC = compCond(R,Lc,Fail,Brks,Ctx);

      valis chLine(OLc,Lc)++[.iLbl(Ok,.iBlock([],
	    [.iLbl(Fl,.iBlock([],LC++[.iBreak(Ok)]))]++RC
	    ++[.iBreak(Ok)]))]
    }
    | .cNeg(Lc,R) => compNegated(R,OLc,Fail,Brks,Ctx)
    | .cCnd(Lc,T,L,R) => compCond(.cDsj(Lc,.cCnj(Lc,T,L),R),OLc,Fail,Brks,Ctx)
    | .cMatch(Lc,Ptn,Exp) => valof{
      (EC,EV) = bindExpToVar(Exp,Lc,Brks,.notLast,Ctx);
      PC = compPtn(Ptn,Lc,EV,Fail,Brks,Ctx);
      valis chLine(OLc,Lc)++EC++PC
    }
    | Exp default => valof{
      (EC,EV) = bindExpToVar(Exp,OLc,Brks,.notLast,Ctx);
      valis EC++[.iIfNot(Fail,EV)]
    }
  }

  compNegated:(cExp,option[locn],assemLbl,breakLvls,codeCtx) => multi[insOp].
  compNegated(C,OLc,Fail,Brks,Ctx) => case C in {
    | .cTerm(_,"false",[],_) => []
    | .cTerm(_,"true",[],_) => [.iBreak(Fail)]
    | .cCnj(Lc,L,R) => compCond(.cDsj(Lc,.cNeg(Lc,L),.cNeg(Lc,R)),OLc,Fail,Brks,Ctx)
    | .cDsj(Lc,L,R) => compCond(.cCnj(Lc,.cNeg(Lc,L),.cNeg(Lc,R)),OLc,Fail,Brks,Ctx)
    | .cNeg(Lc,R) => compCond(R,OLc,Fail,Brks,Ctx)
    | .cCnd(Lc,T,L,R) => compCond(.cCnd(Lc,T,R,L),OLc,Fail,Brks,Ctx)
    | .cMatch(Lc,Ptn,Exp) => valof{
      NegOk = defineLbl(Ctx,"NegOk");
      (EC,EV) = bindExpToVar(Exp,Lc,Brks,.notLast,Ctx);
      PC = compPtn(Ptn,Lc,EV,NegOk,Brks,Ctx);
      valis chLine(OLc,Lc)++EC++[.iLbl(NegOk,.iBlock([],EC++PC++[.iBreak(Fail)]))]
    }
    | Exp default => valof{
      (EC,EV) = bindExpToVar(Exp,OLc,Brks,.notLast,Ctx);
      valis EC++[.iIf(Fail,EV)]
    }
  }

  compAction:(aAction,option[locn],breakLvls,tailMode,codeCtx) => multi[insOp].
  compAction(A,OLc,Brks,Last,Ctx) => case A in {
    | .aNop(_Lc) => []
    | .aSeq(Lc,L,R) =>
      compAction(L,Lc,Brks,.notLast,Ctx)++compAction(R,Lc,Brks,Last,Ctx)
    | .aLbld(Lc,Lb,LbldA) => valof{
      Ex = defineLbl(Ctx,Lb);
      LBrks = Brks[Lb->Ex];
      LC = compAction(LbldA,Lc,LBrks,.notLast,Ctx);
      valis chLine(OLc,Lc)++[.iLbl(Ex,.iBlock([],LC++[.iBreak(Ex)]))]
    }
    | .aBreak(Lc,Lb) => valof{
      if XLbl?=Brks[Lb] then{
	valis [.iBreak(XLbl)]
      }
      else{
	reportError("unknown break label $(Lb)",Lc);
	valis []
      }
    }
    | .aValis(Lc,E) => valof{
      (VC,VV) = bindExpToVar(E,Lc,Brks,Last,Ctx);
      if VLbl ?= Brks["$valof"] then{
	valis chLine(OLc,Lc)++VC++[.iResult(VLbl,[VV])]
      }
      else{
	reportError("not in scope of valof",Lc);
	valis []
      }
    }
    | .aThrw(Lc,E) => valof{
      if Lbl ?= Brks["$try"] then{
	(EC,EV) = bindExpToVar(E,Lc,Brks,.notLast,Ctx);
	valis chLine(OLc,Lc)++EC++[.iResult(Lbl,[EV])]
      } else{
	reportError("throw outside of try scope",Lc);
	valis []
      }
    }
    | .aDo(Lc,E) => bindExpToVar(E,Lc,Brks,Last,Ctx).0
    | .aDefn(Lc,P,E) => valof{
      if .cVar(_,.cV(Nm,Tp)).=P then{
	defineLclVar(Nm,Tp,Ctx);
	EC = compExp(E,Lc,Nm,Brks,.notLast,Ctx);
	valis chLine(OLc,Lc)++EC++genBind(Nm)
      } else{
	(EC,EV) = bindExpToVar(E,Lc,Brks,.notLast,Ctx);
	Ab = defineLbl(Ctx,"Ab");
	Ok = defineLbl(Ctx,"MtchOk");
	PC = compPtn(P,Lc,EV,Ab,Brks,Ctx);
	valis chLine(OLc,Lc)++[.iLbl(Ok,
	    .iBlock([],[.iLbl(Ab,
		  .iBlock([],EC++PC++[.iBreak(Ok)]))]++
	      compAbort(Lc,"definition failed",Ctx)))]
      }
    }
    | .aMatch(Lc,P,E) => valof{
      (EC,EV) = bindExpToVar(E,Lc,Brks,.notLast,Ctx);
      Ab = defineLbl(Ctx,"Ab");
      Ok = defineLbl(Ctx,"MtchOk");
      PC = compPtn(P,Lc,EV,Ab,Brks,Ctx);
      valis chLine(OLc,Lc)++[.iLbl(Ok,
	  .iBlock([],[.iLbl(Ab,
		.iBlock([],EC++PC++[.iBreak(Ok)]))]++
	    compAbort(Lc,"match failed",Ctx)))]
    }
    | .aAsgn(Lc,P,E) => valof{
      (EC,EV) = bindExpToVar(E,Lc,Brks,.notLast,Ctx);
      (PC,PV) = bindExpToVar(P,Lc,Brks,.notLast,Ctx);
      valis chLine(OLc,Lc)++EC++PC++genDbg(Lc,[.iAssign(PV,EV)])
    }
    | .aSetNth(Lc,P,Ix,E) => valof{
      (EC,EV) = bindExpToVar(E,Lc,Brks,.notLast,Ctx);
      (PC,PV) = bindExpToVar(P,Lc,Brks,.notLast,Ctx);
      valis chLine(OLc,Lc)++EC++PC++[.iStNth(PV,Ix,EV)]
    }
    | .aCase(Lc,Gov,Cases,Deflt) => valof{
      Ok = defineLbl(Ctx,"Ok");

      valis [.iLbl(Ok,.iBlock([],    
	  compCase(Lc,Gov,Cases,Deflt,
	    (E,L,B,T,C) => valof{
		EC = compAction(E,L,B,T,C);
		valis EC++[.iBreak(Ok)]
	    },Brks,Last,Ctx)))]
    }      
    | .aIxCase(Lc,Gov,Cases,Deflt) => valof{
      Ok = defineLbl(Ctx,"Ok");

      valis [.iLbl(Ok,.iBlock([],    
	  compIndexCase(Lc,Gov,Cases,Deflt,
	    (E,L,B,T,C) => valof{
		EC = compAction(E,L,B,T,C);
		valis EC++[.iBreak(Ok)]
	    },Brks,Last,Ctx)))]
    }
    | .aIftte(Lc,G,L,R) => valof{
      Fl = defineLbl(Ctx,"Fl");
      Ok = defineLbl(Ctx,"IfOk");
      CC = compCond(G,Lc,Fl,Brks,Ctx);
      LC = compAction(L,Lc,Brks,Last,Ctx);
      RC = compAction(R,Lc,Brks,Last,Ctx);
      valis chLine(OLc,Lc)++[.iLbl(Ok,.iBlock([],
	    [.iLbl(Fl,.iBlock([],
		  CC++LC++[.iBreak(Ok)]))]++
	      RC++[.iBreak(Ok)]))]
    }
    | .aWhile(Lc,G,B) => valof{
      Lp = defineLbl(Ctx,"Lp");
      Done = defineLbl(Ctx,"Done");

      GC = compCond(G,Lc,Done,Brks,Ctx);
      BC = compAction(B,Lc,Brks,.notLast,Ctx);

      valis [.iLbl(Done,.iBlock([],[.iLbl(Lp,
		.iLoop(GC++BC++[.iCont(Lp)]))]))]
    }
    |.aLtt(Lc,.cV(Vr,VTp),Val,Bnd) => valof{
      Ctx1 = defineLclVar(Vr,VTp,Ctx);
      VV = compExp(Val,Lc,Vr,Brks,.notLast,Ctx);
      BB = compAction(Bnd,Lc,Brks,Last,Ctx1);
      valis chLine(OLc,Lc)++VV++BB
    }
    | .aTry(Lc,B,.cVar(_,.cV(Er,ETp)),H) => valof{
      if traceCodegen! then
	showMsg("compiling try catch @$(Lc), Er=$(Er)");

      Ok = defineLbl(Ctx,"ATrOk");
      TrX = defineLbl(Ctx,"TrX");
      Ctx1 = defineLclVar(Er,ETp,Ctx);

      TBrks = Brks["$try" -> TrX];

      BC = compAction(B,Lc,TBrks,.notLast,Ctx1);
      HC = compAction(H,Lc,Brks,Last,Ctx);

      valis chLine(OLc,Lc)++
      [.iLbl(Ok,.iBlock([],
	    [.iLbl(TrX,.iBlock([Er],BC++[.iBreak(Ok)]))]++HC++[.iBreak(Ok)]))]
    }
    | .aAbort(Lc,Msg) => compAbort(Lc,Msg,Ctx)
    | _ default => valof{
      reportError("cannot compile action $(A)",locOf(A));
      valis []
    }
  }

  all e ~~ caseHandler[e] ~> (e,option[locn],breakLvls,tailMode,codeCtx)=>multi[insOp].
  caseWrap ~> (multi[insOp]) => multi[insOp].

  compCase:all e ~~ display[e] |=
  (option[locn],cExp,cons[cCase[e]],e,
    caseHandler[e],breakLvls,tailMode,codeCtx) => multi[insOp].
  compCase(Lc,Gv,Cases,Deflt,Hndlr,Brks,Last,Ctx) => valof{
    if traceCodegen! then
      showMsg("compiling case @$(Lc), Gov=$(Gv), Deflt=$(Deflt), Cases=$(Cases)");
    Df = defineLbl(Ctx,"Df");

    (GC,GV) = bindExpToVar(Gv,Lc,Brks,.notLast,Ctx);
    
    (Table,Max) = genCaseTable(Cases);

    if traceCodegen! then
      showMsg("case tablees=$(Table); max=$(Max)");
    DC = Hndlr(Deflt,Lc,Brks,Last,Ctx);

    if traceCodegen! then
      showMsg("DC=$(DC)");

    Caser = ((Bks)=>(intType==deRef(typeOf(Gv)) ?? [.iICase(GV,Bks)] || [.iCase(GV,Bks)]));

    CC = compCases(Table,0,Max,GV,Df,Hndlr,Brks,Last,GC,Caser,Ctx);

    valis [.iBlock([],[.iLbl(Df,.iBlock([],CC))]++DC)]
  }

  compIndexCase:all e ~~ display[e] |=
    (option[locn],cExp,cons[cCase[e]],e,
    caseHandler[e],breakLvls,tailMode,codeCtx) => multi[insOp].
  compIndexCase(Lc,Gv,Cases,Deflt,Hndlr,Brks,Last,Ctx) where hasIndexMap(Ctx,tpName(typeOf(Gv))) => valof{
    if traceCodegen! then
      showMsg("compiling case @$(Lc), Gov=$(Gv), Deflt=$(Deflt), Cases=$(Cases)");
    Df = defineLbl(Ctx,"Df");
    (GC,GVr) = bindExpToVar(Gv,Lc,Brks,.notLast,Ctx);

    Table = genIndexTable(Cases,Ctx);
    Mx = maxIndex(Table).

    DC = Hndlr(Deflt,Lc,Brks,Last,Ctx);

    CC = compCases(Table,0,Mx,GVr,Df,Hndlr,Brks,Last,GC,((Bks)=>[.iIxCase(GVr,Bks)]),Ctx);

    valis [.iBlock([],[.iLbl(Df,.iBlock([],CC))]++DC)]
  }
  compIndexCase(Lc,Gv,Cases,Deflt,Hndlr,Brks,Last,Ctx) =>
    compCase(Lc,Gv,Cases,Deflt,Hndlr,Brks,Last,Ctx).
  
  compCases:all e ~~ display[e] |=
    (cons[csEntry[e]],integer,integer,identifier,assemLbl,caseHandler[e],
    breakLvls,tailMode,multi[insOp],caseWrap,codeCtx) => multi[insOp].
  compCases([],Ix,Mx,_GVar,_Df,_Hndlr,_Brks,_Last,CaseCode,Caser,Ctx) where
      Ix>=Mx => Caser(CaseCode).
  compCases([],Ix,Mx,GVar,Df,Hndlr,Brks,Last,CaseCode,Caser,Ctx) =>
    compCases([],Ix+1,Mx,GVar,Df,Hndlr,Brks,Last,CaseCode++[.iBreak(Df)],Caser,Ctx).
  compCases([(Ix,Case),..Cs],Ix,Mx,GVar,Df,Hndlr,Brks,Last,CaseCode,Caser,Ctx) => valof{
    El = defineLbl(Ctx,"El");
    CSC = compCases(Cs,Ix+1,Mx,GVar,Df,Hndlr,Brks,Last,CaseCode++[.iBreak(El)],Caser,Ctx);
    CC = compCaseBranch(Case,GVar,Hndlr,Df,Brks,Last,Ctx);

    valis [.iLbl(El,.iBlock([],CSC))]++CC
  }
  compCases([(Iy,Case),..Cs],Ix,Mx,GVar,Df,Hndlr,Brks,Last,CaseCode,Caser,Ctx) where Ix<Iy =>
    compCases([(Iy,Case),..Cs],Ix+1,Mx,GVar,Df,Hndlr,Brks,Last,CaseCode++[.iBreak(Df)],Caser,Ctx).

  compCaseBranch:all e ~~ display[e] |=
  (cons[cCase[e]],identifier,caseHandler[e],assemLbl,breakLvls,tailMode,codeCtx) =>
    multi[insOp].
  compCaseBranch([(Lc,P,E)],Gv,Hndlr,Df,Brks,Last,Ctx) => valof{
    PC = compPtn(P,Lc,Gv,Df,Brks,Ctx);
    EC = Hndlr(E,Lc,Brks,Last,Ctx);
    valis chLine(.none,Lc)++PC++EC
  }
  compCaseBranch([],_Gv,_Hndlr,Df,_Brks,_Last,Ctx) =>
    [.iBreak(Df)].
  compCaseBranch([(Lc,P,E),..Cs],Gv,Hndlr,Df,Brks,Last,Ctx) => valof{
    Fl = defineLbl(Ctx,"Fl");
    PC = compPtn(P,Lc,Gv,Fl,Brks,Ctx);
    EC = Hndlr(E,Lc,Brks,Last,Ctx);
    CC = compCaseBranch(Cs,Gv,Hndlr,Df,Brks,Last,Ctx);
    valis chLine(.none,Lc)++[.iLbl(Fl,.iBlock([],PC++EC))]++CC
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

  genIndexTable:all e ~~ (cons[cCase[e]],codeCtx) => cons[csEntry[e]].
  genIndexTable(Cases,Ctx) => sortCases(caseIndices(Cases,Ctx)).

  caseIndices(Cases,Ctx) => (Cases//((Lc,Pt,Ex))=>(Lc,Pt,caseIndex(Pt,Ctx),Ex)).

  caseIndex(.cTerm(Lc,Nm,Els,Tp),Ctx) => valof{
    if IxMap ?= Ctx.tps[tpName(Tp)] && Ix ?= IxMap[.tLbl(Nm,[|Els|])] then
      valis Ix
    else{
      reportError("cannot find index of #(Nm)",Lc);
      valis 0
    }
  }

  hasIndexMap(Ctx,TpNm) => _ ?= Ctx.tps[TpNm].

  maxIndex:all e ~~ (cons[csEntry[e]]) => integer.
  maxIndex(Cases) => foldRight(((Ix,_),Mx) => max(Ix,Mx),0,Cases).

  sortCases:all e ~~ (cons[(option[locn],cExp,integer,e)]) => cons[csEntry[e]].
  sortCases(Cases) => mergeDuplicates(sort(Cases,((_,_,H1,_),(_,_,H2,_))=>H1<H2)).

  mergeDuplicates:all e ~~ (cons[(option[locn],cExp,integer,e)])=>cons[csEntry[e]].
  mergeDuplicates([])=>[].
  mergeDuplicates([(Lc,Pt,Hx,Ex),..M]) where (D,Rs).=mergeDuplicate(M,Hx,[]) =>
    [(Hx,[(Lc,Pt,Ex),..D]),..mergeDuplicates(Rs)].

  mergeDuplicate:all e ~~ (cons[(option[locn],cExp,integer,e)],integer,cons[(option[locn],cExp,e)]) => (cons[(option[locn],cExp,e)],cons[(option[locn],cExp,integer,e)]).
  mergeDuplicate([(Lc,Pt,Hx,Ex),..M],Hx,SoFar) =>
    mergeDuplicate(M,Hx,SoFar++[(Lc,Pt,Ex)]).
  mergeDuplicate(M,_,SoFar) default => (SoFar,M).

  compPtn:(cExp,option[locn],identifier,assemLbl,breakLvls,codeCtx) => multi[insOp].
  compPtn(Ptn,OLc,PVr,Fail,Brks,Ctx) => case Ptn in {
    | .cVar(_,.cV("_",_)) => []
    | .cVar(Lc,.cV(Vr,Tp)) => valof{
      defineLclVar(Vr,Tp,Ctx);
      valis [.iMv(Vr,PVr)]++genBind(Vr)
    }
    | .cVoid(Lc) => []
    | .cAnon(Lc,_) => []
    | .cTerm(Lc,Nm,Args,Tp) where canFail(Nm,Tp,Ctx) => valof{
      SCde = compArgPtns(Args,Lc,0,PVr,Fail,Brks,Ctx);
      valis chLine(OLc,Lc)++[.iCLbl(.tLbl(Nm,size(Args)),Fail,PVr)]++SCde
    }
    | .cTerm(Lc,Nm,Args,Tp) where ~canFail(Nm,Tp,Ctx) => 
      chLine(OLc,Lc)++compArgPtns(Args,Lc,0,PVr,Fail,Brks,Ctx)
    | .cSvDrf(Lc,P,Tp) => valof{
      SVr = defineTmpVar(Tp,Ctx);
      PC = compPtn(P,Lc,SVr,Fail,Brks,Ctx);
      valis [.iLdSav(SVr,Fail,PVr)]++PC
    }
    | .cInt(_,Ix) => [.iCInt(Ptn:?data,Fail,PVr)]
    | .cChar(_,Cx) => [.iCChar(Ptn:?data,Fail,PVr)]
    | .cFlt(_,Dx) => [.iCFlt(Ptn:?data,Fail,PVr)]
    | _ default => ( isGround(Ptn) ??
      [.iCLit(Ptn:?data,Fail,PVr)] || valof{
	reportError("uncompilable pattern $(Ptn)",locOf(Ptn));
	valis [.iBreak(Fail)]
      }
    )
  }

  compArgPtns:(cons[cExp],option[locn],integer,identifier,assemLbl,breakLvls,codeCtx) => multi[insOp].
  compArgPtns(Es,Lc,Ix,Src,Fail,Brks,Ctx) => case Es in {
    | [] => []
    | [A,..As] => valof{
      AV = defineTmpVar(typeOf(A),Ctx);
      AC = compPtn(A,Lc,AV,Fail,Brks,Ctx);
      AsC = compArgPtns(As,Lc,Ix+1,Src,Fail,Brks,Ctx);
      valis [.iNth(AV,Ix,Src)]++AC++AsC
    }
  }

  declareArgs:(cons[cV],codeCtx) => codeCtx.
  declareArgs(Args,Ctx) => foldLeft(((.cV(Nm,Tp),C)=>defineArgVar(Nm,Tp,C)),Ctx,Args).

  defineArgVar:(identifier,tipe,codeCtx) => codeCtx.
  defineArgVar(Nm,Tp,Ctx) => valof{
    Ctx.vars:=Ctx.vars![Nm->(Tp,.argVar(Nm,Tp))];
    valis Ctx
  }

  compAbort:(option[locn],string,codeCtx) => multi[insOp].
  compAbort(.some(Lc),Msg,Ctx) => valof{
    TV = defineTmpVar(strType,Ctx);
    valis [.iMC(TV,.strg(Msg)),.iAbort(Lc::data,TV)]
  }

  defineLclVar:(identifier,tipe,codeCtx) => codeCtx.
  defineLclVar(Nm,Tp,Ctx) => valof{
    Ctx.vars:=Ctx.vars![Nm->(Tp,.lclVar(Nm,Tp))];
    valis Ctx
  }

  defineTmpVar:(tipe,codeCtx) => identifier.
  defineTmpVar(Tp,Ctx) => valof{
    VrNm = genId("\u3bd;");
    defineLclVar(VrNm,Tp,Ctx);
    valis VrNm
  }

  locateVar:(string,codeCtx) => option[(tipe,srcLoc)].
  locateVar(Nm,Ctx) => Ctx.vars![Nm].

  srcLoc ::=
    .argVar(identifier,tipe) |
    .lclVar(identifier,tipe) |
    .glbVar(identifier,tipe) |
    .glbFun(termLbl,tipe).


  breakLvls ~> map[identifier,assemLbl].

  codeCtx ::= codeCtx{
    vars : ref map[identifier,(tipe,srcLoc)].
    tps : map[identifier,indexMap].
    lbls : ref integer.  
  }

  emptyCtx:(map[identifier,(tipe,srcLoc)],map[identifier,indexMap])=>codeCtx.
  emptyCtx(Glbs,Tps) => codeCtx{
    vars = ref Glbs.
    tps = Tps.
    lbls = ref 0.
  }

  varInfo:(codeCtx) => cons[(identifier,data)].
  varInfo(Ctx) => { (Nm, Tp::data) | (Nm -> (Tp,.lclVar(_,_))) in Ctx.vars! }.

  varNms:(codeCtx) => cons[identifier].
  varNms(Ctx) => { Nm | (Nm -> (_,.lclVar(_,_))) in Ctx.vars! }.

  defineLbl:(codeCtx,identifier)=>assemLbl.
  defineLbl(C,Pr) => valof{
    CurLbl = C.lbls!;
    C.lbls := CurLbl+1;
    valis "#(Pr)$(CurLbl)"
  }

  implementation display[codeCtx] => {
    disp(C) => "<C $(C.vars!) C>".
  }

  implementation display[srcLoc] => {
    disp(L) => case L in {
      | .lclVar(Nm,Tpe) => "lcl $(Nm)\:$(Tpe)"
      | .glbVar(Off,Tpe) => "glb $(Off)\:$(Tpe)"
      | .glbFun(Off,Tpe) => "fun $(Off)\:$(Tpe)"
    }
  }

  chLine:(option[locn],option[locn]) => multi[insOp].
  chLine(_,.none) => [].
  chLine(.some(Lc),.some(Lc)) => [].
  chLine(_,.some(Lc)) => (genDebug! ?? [.iLine(Lc::data)] || []).

  genDbg:(option[locn],multi[insOp]) => multi[insOp].
  genDbg(.some(Lc),Ins) => (genDebug! ?? [.iDBug(Lc::data),..Ins] || Ins).
  genDbg(.none,Ins) => Ins.

  genBind:(identifier) => multi[insOp].
  genBind(Nm) => (genDebug! ?? [.iBind(.strg(Nm),Nm)] || []).

  flatSig = .fnTipe([],.tplTipe([])).
  nearlyFlatSig(T) => .fnTipe([],.tplTipe([T])).
  blockSig(Args,Rs) => .fnTipe(Args,Rs).

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

  canFail:(identifier,tipe,codeCtx) => boolean.
  canFail(Nm,_Tp,_Ctx) where isTplLbl(Nm) => .false.
  canFail(_,_,_) default => .true.
}
