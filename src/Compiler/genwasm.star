star.compiler.wasm.gen{
  import star.
  import star.multi.
  import star.pkg.
  import star.sort.

  import star.compiler.term.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.wasm.instr.
  import star.compiler.wasm.module.
  import star.compiler.wasm.gentypes.
  import star.compiler.wasm.types.

  import star.compiler.location.
  import star.compiler.data.

  public genWasm:(pkg,cons[cDefn],cons[decl],cons[recType])=>cons[wasmDefn].
  genWasm(Pkg,Defs,Decls,TpMap) => valof{
    Vars = foldLeft(declareDecl,emptyMap,Decls);
    valis compDefs(Defs,localDecls(Defs,Vars))
  }

  declareDecl:(decl,srcMap)=>srcMap.
  declareDecl(Def,Map) => case Def in {
    | .funDec(_,_,Nm,Tp) => declareGlobal(Nm,.glbFun(.tLbl(Nm,arity(Tp)),Tp),Map)
    | .varDec(_,_,Nm,Tp) => declareGlobal(Nm,.glbVar(.tLbl(Nm,arity(Tp)),Tp),Map)
    | _ default => Map
  }

  localDecls:(cons[cDefn],srcMap)=>srcMap.
  localDecls(Defs,Vars) => foldRight(declareFun,Vars,Defs).

  declareFun(Def,Map) => case Def in {
    | .fnDef(Lc,Nm,Tp,_,_) => declareGlobal(Nm,.glbFun(.tLbl(Nm,arity(Tp)),Tp),Map)
    | .glDef(Lc,Nm,Tp,_) => declareGlobal(Nm,.glbVar(Nm,Tp),Map)
    | _ default => Map
  }
  
  compDefs:(cons[cDefn],srcMap)=> cons[wasmDefn].
  compDefs(Dfs,Glbs) => (Dfs//(D)=>genDef(D,Glbs)).

  genDef:(cDefn,srcMap) => wasmDefn.
  genDef(Defn,Glbs) => case Defn in {
    | .fnDef(Lc,Nm,Tp,Args,Val) => valof{
      if traceCodegen! then
	showMsg("compile $(Defn)");
      Ctx = emptyCtx(collectLocals(Val,argVars(Args,Glbs)));
      (_Stk,Code) = compExp(Val,.noMore,retCont,Ctx,.some([]));
      Compiled = .wasmFunc(Nm,wasmFunctionType(Tp,Glbs),functionLocals(Ctx),Code::cons[wOp]);

      if traceCodegen! then{
	showMsg("code is $(Compiled)");
      };
      valis Compiled
    }
  | .glDef(Lc,Nm,Tp,Val) => valof{
      if traceCodegen! then
	showMsg("compile global $(Nm)\:$(Tp) = $(Val))");
      Ctx = emptyCtx(Glbs);
      (_,AbortCde) = abortCont(Lc,"global: $(Nm)").C(Ctx,.none,[]);
      (_Stk,Code) = compExp(Val,.notLast,glbRetCont(Nm),Ctx,.some([]));

      valis .wasmGlob(.tLbl(Nm,0),Tp,Ctx.hwm!,Code::cons[wOp])
    }
  | .tpDef(Lc,Tp,TpRl,Index) => .tipe(Tp,TpRl,Index)
  | .lblDef(_Lc,Lbl,Tp,Ix) => .struct(Lbl,Tp,Ix)
  }

  compExp:(cExp,tailMode,Cont,codeCtx,stack) => (stack,multi[wOp],codeCtx).
  compExp(Exp,TM,Cont,Ctx,Stk) => case Exp in {
    | .cVoid(Lc,Tp) => Cont.C(Ctx,pushStack(voidType,Stk),wasmConstant(voidSymbol))
    | .cInt(Lc,Ix) => Cont.C(Ctx,pushStack(intType,Stk),wasmInt(Ix))
    | .cChar(Lc,Cx) => Cont.C(Ctx,pushStack(intType,Stk),wasmInt(Cx::integer))
--    | .cBig(Lc,Bx) => Cont.C(Ctx,pushStack(bigintType,Stk),wasmBigInt(Bx))
    | .cFlt(Lc,Dx) => Cont.C(Ctx,pushStack(fltType,Stk),wasmFloat(Dx))
    | .cString(Lc,Sx) => wasmString(Sx,Stk,Ctx)
    | .cVar(Lc,.cV(Vr,Tp)) => valof{
      if Loc?=locateVar(Vr,Ctx) then {
	case Loc in {
	  | .lclVar(Nm,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.LocalGet(Nm)])
	  | .glbVar(Nm,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.GlobalGet(Nm)])
	}
      } else {
	reportError("cannot locate variable $(Vr)\:$(Tp)",Lc);
	valis Cont.C(Ctx,pushStack(voidType,Stk),wasmConstant(voidSymbol))
      }
    }
    | .cTerm(_,Nm,Args,Tp) =>
      compExps(Args,Lc,allocCont(.tLbl(Nm,size(Args)),pushStack(Tp,Stk),Cont),Ctx,Stk)
    | .cCall(Lc,Nm,Args,Tp) =>
      compExps(Args,callCont(.tLbl(Nm,[|Args|]),TM,pushStack(Tp,Stk),Cont),Ctx,Stk)
    | .cOCall(Lc,Op,Args,Tp) => 
      compExps(Args,expCont(Op,.notLast,oclCont([|Args|]+1,TM,pushStack(Tp,Stk),Cont)),Ctx,Stk)
    | .cNth(Lc,E,Ix,Tp) =>
      compExp(E,.notLast,nthCont(Ix,Cont,pushStack(Tp,Stk)),Ctx,Stk)
    | .cSetNth(Lc,R,Ix,V) => compExp(R,.notLast,expCont(V,.notLast,setNthCont(Ix,Cont,Stk)),Ctx,Stk)
    | .cClos(_,L,A,F,Tp) => compExp(F,TM,closCont(pushStack(Tp,Stk),.tLbl(L,A),Cont),Ctx,Stk)
    | .cSeq(_,L,R) =>
      compExp(L,.notLast,resetCont(Stk,expCont(R,TM,Cont)),Ctx,Stk)
    | .cCnd(Lc,G,L,R) => valof{
      CC = splitCont(Lc,Ctx,Cont);
      valis compCond(G,TM,expCont(L,TM,CC),ctxCont(Ctx,expCont(R,TM,CC)),Ctx,Stk)
    }
    | .cCase(Lc,Gov,Cases,Deflt,_Tp) => valof{
      valis compCase(Lc,Gov,Cases,Deflt, (E,C1)=>expCont(E,TM,C1),Cont,Ctx,Stk)
    }
    | .cLtt(Lc,.cV(Vr,VTp),Val,Bnd) => valof{
      valis compExp(Val,.notLast,stoCont(Vr,VTp,Stk,expCont(Bnd,TM,Cont)),Ctx,Stk)
    }
    | .cAbort(Lc,Msg,Tp) => abortCont(Lc,Msg).C(Ctx,Stk,[])
    | .cTry(Lc,B,.cVar(_,.cV(Th,ThTp)),.cVar(_,.cV(Er,ETp)),H,Tp) => valof{
      (CLb,Ctx0) = defineExitLbl("Tr",Ctx);
      Blk = defineLbl("H",Ctx);
      (TOff,CtxB) = defineLclVar(Th,ThTp,Ctx0);
      (EOff,Ctx1) = defineLclVar(Er,ETp,Ctx);
      
      (Stk1,BCde) = compExp(B,.notLast,Cont,CtxB,Stk); -- critical: body of try is not tail rec
      (Stk2,HCde) = compExp(H,TM,Cont,Ctx1,Stk);

      if ~reconcileable(Stk1,Stk2) then
	reportError("cannot reconcile try exp $(B) with handler $(H)",Lc);

      valis (reconcileStack(Stk1,Stk2),[.iTry(Blk),.iStL(TOff)]++BCde++[.iLbl(Blk),.iTL(EOff)]++HCde)
    }
    | .cRaise(Lc,T,E,_) => compExp(E,.notLast,expCont(T,.notLast,raiseCont),Ctx,Stk)
    | .cValof(Lc,A,Tp) =>
      compAction(A,TM,abortCont(Lc,"missing valis action"),splitCont(Lc,Ctx,Cont),Ctx,Stk)
    |  C where isCond(C) => valof{
      Nx = defineLbl("E",Ctx);
      Stk0 = pushStack(boolType,Stk);
      (Stk1,Cde) = compCond(C,.notLast,trueCont(jmpCont(Nx,Stk0)),falseCont(jmpCont(Nx,Stk0)),
	Ctx,Stk);
      valis Cont.C(Ctx,Stk1,Cde++[.iLbl(Nx)]) -- fix me
    }
    | C => valof{
      reportError("cannot compile expression $(C)",locOf(C));
      valis Cont.C(Ctx,Stk,[])
    }
  }
    
  compExps:(cons[cExp],codeCtx,stack) => (stack,multi[wOp]).
  compExps([],_Ctx,Stk)=> (Stk,[]).
  compExps([Exp,..Es],Ctx,Stk)=> valof{
    (S1,C1) = compExp(Exp,.notLast,Ctx,Stk);
    (S2,C2) = compExps(Es,Ctx,S1);
    valis (S2,C1++C2)
  }

  compCond:(cExp,tailMode,Cont,Cont,codeCtx,stack) => (stack,multi[wOp]).
  compCond(C,TM,Succ,Fail,Ctx,Stk) => case C in {
    | .cTerm(_,"true",[],_) => Succ.C(Ctx,Stk,[])
    | .cTerm(_,"false",[],_) => Fail.C(Ctx,Stk,[])
    | .cCnj(Lc,L,R) => valof{
      FC = splitCont(Lc,Ctx,ctxCont(Ctx,Fail));
      valis compCond(L,.notLast,condCont(R,TM,Succ,FC,Stk),FC,Ctx,Stk)
    }
    | .cDsj(Lc,L,R) => valof{
      Ctxa = dsjCtx(Ctx,L,R);
      SC = splitCont(Lc,Ctx,ctxCont(Ctxa,Succ));
      
      valis compCond(L,TM,SC,ctxCont(Ctxa,condCont(R,TM,SC,Fail,Stk)),Ctxa,Stk)
    }
    | .cNeg(Lc,R) => compCond(R,TM,ctxCont(Ctx,Fail),ctxCont(Ctx,Succ),Ctx,Stk)
    | .cCnd(Lc,T,L,R) => valof{
      Ctxa = dsjCtx(Ctx,L,R);
      SC = splitCont(Lc,Ctx,Succ);
      FC = splitCont(Lc,Ctx,Fail);
      
      valis compCond(T,.notLast,condCont(L,TM,SC,FC,Stk),ctxCont(Ctx,condCont(R,TM,SC,FC,Stk)),Ctx,Stk)
    }
    | .cMatch(Lc,Ptn,Exp) => compExp(Exp,.notLast,ptnCont(Ptn,Succ,Fail),Ctx,Stk)
    | Exp default => compExp(Exp,.notLast,ifCont(locOf(Exp),Stk,Succ,Fail),Ctx,Stk)
  }

  compAction:(aAction,tailMode,Cont,Cont,codeCtx,stack) =>(stack,multi[wOp]).
  compAction(A,TM,ACont,Cont,Ctx,Stk) => case A in {
    | .aNop(Lc) => ACont.C(Ctx,Stk,[])
    | .aSeq(Lc,L,R) => compAction(L,.notLast,resetCont(Stk,actionCont(R,TM,ACont,Cont)),Cont,Ctx,Stk)
    | .aLbld(Lc,Lb,LbldA) => valof{
      Ctxl = (Ctx.brks=Ctx.brks[Lb->ctxCont(Ctx,resetCont(Stk,ACont))]);
      valis compAction(LbldA,TM,ACont,Cont,Ctxl,Stk)
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
    | .aValis(Lc,E) => compExp(E,TM,Cont,Ctx,Stk)
    | .aRaise(Lc,T,E) => compExp(E,.notLast,expCont(T,.notLast,raiseCont),Ctx,Stk)
    | .aDo(Lc,E) => compExp(E,TM,resetCont(Stk,ACont),Ctx,Stk)
    | .aDefn(Lc,P,E) => compExp(E,.notLast,ptnCont(P,ACont,abortCont(Lc,"define error")),Ctx,Stk)
    | .aAsgn(Lc,P,E) => compExp(E,.notLast,expCont(P,.notLast,asgnCont(ACont,Ctx,Stk)),Ctx,Stk)
    | .aSetNth(Lc,T,Ix,E) => compExp(T,.notLast,expCont(E,.notLast,setNthCont(Ix,ACont,Stk)),Ctx,Stk)
    | .aCase(Lc,G,Cs,D) => compCase(Lc,G,Cs,D,(Ac,C1)=>actionCont(Ac,TM,ACont,C1),Cont,Ctx,Stk)
    | .aIftte(Lc,G,L,R) => valof{
      AC = splitCont(Lc,Ctx,ACont);
      CC = splitCont(Lc,Ctx,Cont);
      valis compCond(G,.notLast,actionCont(L,TM,AC,CC),
	resetCont(Stk,ctxCont(Ctx,actionCont(R,TM,AC,CC))),Ctx,Stk)
    }
    | .aWhile(Lc,G,B) => valof{
      Lp = defineLbl("Lp",Ctx);
      Tst = defineLbl("Tst",Ctx);
      Ex = defineLbl("Ex",Ctx);
      GCtx = glCtx(Ctx,G);

      (Stk1,WCde) = compCond(G,.notLast,jmpCont(Lp,Stk),jmpCont(Ex,Stk),GCtx,Stk);
      (Stk0,BCde) = compAction(B,.notLast,jmpCont(Tst,Stk),Cont,GCtx,Stk);

      valis ACont.C(Ctx,reconcileStack(Stk0,Stk1),
	[.iJmp(Tst),.iLbl(Lp)]++BCde++[.iLbl(Tst)]++WCde++[.iLbl(Ex)])
    }
    |.aLtt(Lc,.cV(Vr,VTp),Val,Bnd) => valof{
      valis compExp(Val,.notLast,stoCont(Vr,VTp,Stk,actionCont(Bnd,TM,ACont,Cont)),Ctx,Stk)
    }
    | .aTry(Lc,B,.cVar(_,.cV(Th,ThTp)), .cVar(_,.cV(Er,ETp)),H) => valof{
      Blk = defineLbl("H",Ctx);
      (TOff,Ctx0) = defineLclVar(Th,ETp,Ctx);
      (EOff,Ctx1) = defineLclVar(Er,ETp,Ctx);
      AC = splitCont(Lc,Ctx,ACont);
      CC = splitCont(Lc,Ctx,Cont);
      
      (Stk1,BCde) = compAction(B,.notLast,AC,CC,Ctx0,Stk);
      (Stk2,CCde) = compAction(H,TM,AC,CC,Ctx1,Stk);

      if ~reconcileable(Stk1,Stk2) then
	reportError("cannot reconcile try body $(B) with handler $(H)",Lc);
      
      valis (reconcileStack(Stk1,Stk2),[.iTry(Blk),.iStL(TOff)]++BCde++[.iLbl(Blk),.iTL(EOff)]++CCde)
    }
    | .aAbort(Lc,Msg) => abortCont(Lc,Msg).C(Ctx,Stk,[])
    | _ default => valof{
      reportError("cannot compile action $(A)",locOf(A));
      valis ACont.C(Ctx,Stk,[])
    }
  }

  compCase:all e ~~ display[e] |:
    (option[locn],cExp,cons[cCase[e]],e,(e,Cont)=>Cont,Cont,codeCtx,stack) => (stack,multi[wOp]).
  compCase(Lc,Gv,Cases,Deflt,Comp,Cont,Ctx,Stk) => valof{
    if traceCodegen! then
      showMsg("compiling case @$(Lc), Gov=$(Gv)");
    Nxt = defineLbl("CN",Ctx);
    DLbl = defineLbl("CD",Ctx);
    (Stk1,GCode) = compExp(Gv,.notLast,jmpCont(Nxt,pushStack(typeOf(Gv),Stk)),Ctx,Stk);
    (Table,Max) = genCaseTable(Cases);
    
    OC = splitCont(Lc,Ctx,Cont);
    (Stkc,DCode) = Comp(Deflt,OC).C(Ctx,Stk,[]);

    (Stkb,TCode,CCode) = compCases(Table,0,Max,Comp,OC,resetCont(Stk,jmpCont(DLbl,Stkc)),DLbl,Ctx,Stk1);

    valis (reconcileStack(Stkb,Stkc),
      GCode++[.iLbl(Nxt),.iCase(Max)]++TCode++CCode++[.iLbl(DLbl)]++DCode)
  }

  compCases:all e ~~ display[e] |: (cons[csEntry[e]],integer,integer,
    (e,Cont)=>Cont,Cont,Cont,wasmLbl,codeCtx,stack) =>(stack,multi[wOp],multi[wOp]).
  compCases(Cs,Ix,Mx,Comp,Succ,Fail,Deflt,Ctx,Stk) => case Cs in {
    | [] => valof{
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

      valis (reconcileStack(Stkb,Stkc),[.iJmp(Lb)]++TCde2,Cde2++[.iLbl(Lb),..CCde])
    }
    | [(Iy,Case),..Cases] => valof{
      (Stk1,TCde,CCde) = compCases([(Iy,Case),..Cases],Ix+1,Mx,Comp,Succ,Fail,Deflt,Ctx,Stk);
      valis (Stk1,[.iJmp(Deflt)]++TCde,CCde)
    }
  }

  compCaseBranch:all e ~~ display[e] |: (cons[cCase[e]],
    (e,Cont)=>Cont,Cont,Cont,codeCtx,stack) => (stack,multi[wOp]).

  compCaseBranch(Cs,Comp,Succ,Fail,Ctx,Stk) => case Cs in {
    | [(Lc,Ptn,Exp)] => compPttrn(Ptn,Comp(Exp,Succ),Fail,Ctx,Stk)
    | [(Lc,Ptn,Exp),..More] => valof{
      Fl = defineLbl("CF",Ctx);
      VLb = defineLbl("CN",Ctx);
      Vr = genSym("__");
      (Off,Ctx1) = defineLclVar(Vr,typeOf(Ptn),Ctx);
      (Stkc,AltCde) = compMoreCase(More,Off,Comp,Succ,Fail,Ctx,Stk);

      (Stkb,RlCde) = compPttrn(Ptn,Comp(Exp,Succ),jmpCont(Fl,Stkc),Ctx1,Stk);
      valis (reconcileStack(Stkb,Stkc),[.iTL(Off)]++RlCde++[.iLbl(Fl),.iLdL(Off)]++AltCde)
    }
  }

  compMoreCase:all e ~~ display[e] |: (cons[(option[locn],cExp,e)],integer,(e,Cont)=>Cont,
    Cont,Cont,codeCtx,stack) => (stack,multi[wOp]).
  compMoreCase(Cs,Off,Comp,Succ,Fail,Ctx,Stk) => case Cs in {
    | [] => Fail.C(Ctx,.none,[])
    | [(Lc,Ptn,Exp),..More] => valof{
      Fl = defineLbl("CM",Ctx);
      
      (Stk3,RstCde) = compMoreCase(More,Off,Comp,Succ,Fail,Ctx,Stk);
      (Stk2,RlCde) = compPttrn(Ptn,Comp(Exp,Succ),jmpCont(Fl,Stk3),Ctx,Stk);
      valis (reconcileStack(Stk2,Stk3),[.iLdL(Off)]++RlCde++[.iLbl(Fl)]++RstCde)
    }
  }
  
  compCnsCase:all e ~~ display[e] |: (option[locn],cExp,cons[cCase[e]],(e,Cont)=>Cont,
    Cont,codeCtx,stack) => (stack,multi[wOp]).
  compCnsCase(Lc,Gv,Cs,Comp,Cont,Ctx,Stk) => case Cs in {
    | [(_,Ptn,Exp)] => compExp(Gv,.notLast, ptnCont(Ptn,Comp(Exp,Cont),
	abortCont(Lc,"match error")),Ctx,Stk)
    | Cases default => valof{
      CC = splitCont(Lc,Ctx,Cont);
      valis compExp(Gv,.notLast,cnsCaseCont(Cases,Comp,CC),Ctx,Stk)
    }
  }

  cnsCaseCont:all e ~~ display[e] |: (cons[cCase[e]],(e,Cont)=>Cont,Cont) => Cont.
  cnsCaseCont(Cases,Comp,Cont) => cont{
    C(Ctx,AStk,GCde) => valof{
      (Stk2,JCde,CCde) = compCnsCases(Cases,Comp,Cont,Ctx,AStk);
    
      valis (Stk2,GCde++[.iIndxJmp([|Cases|])]++JCde++CCde)
    }
  }

  compCnsCases:all e ~~ display[e] |: (cons[cCase[e]],(e,Cont)=>Cont,Cont,codeCtx,stack) =>
    (stack,multi[wOp],multi[wOp]).
  compCnsCases(Cs,Comp,Succ,Ctx,Stk) => case Cs in {
    | [] => (.none,[],[])
    | [(Lc,Ptn,Exp),..Cases] => valof{
      Lb = cseLbl(Ptn,Ctx);
      (Stk2a,TCde2,Cde2) = compCnsCases(Cases,Comp,Succ,Ctx,Stk);
      (Stk3a,CCde) = compPtn(Ptn,Comp(Exp,Succ),abortCont(Lc,"match error"),Ctx,Stk);
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

  compPttrn:(cExp,Cont,Cont,codeCtx,stack) => (stack,multi[wOp]).
  compPttrn(Ptn,Succ,Fail,Ctx,Stk) => compPtn(Ptn,Succ,Fail,Ctx,Stk).
  
  compPtn:(cExp,Cont,Cont,codeCtx,stack) => (stack,multi[wOp]).
  compPtn(Ptn,Succ,Fail,Ctx,Stk) => case Ptn in {
    | .cVar(_,.cV("_",_)) => Succ.C(Ctx,dropStack(Stk),[.iDrop])
    | .cVar(Lc,.cV(Vr,Tp)) => valof{
      if Loc ?= locateVar(Vr,Ctx) then 
	valis compPtnVar(Lc,Vr,Loc,Succ,Ctx,dropStack(Stk))
      else{
	LTp = Tp;
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
      
      (Stk2,SCde) = compPtnArgs(Args,Succ,resetCont(Stk0,jmpCont(Flb,Stk1)),Ctx,loadStack(Args//(A)=>(typeOf(A)),Stk0));

      valis (reconcileStack(Stk1,Stk2),[.iUnpack(.tLbl(Nm,size(Args)),Flb)]++SCde++[.iLbl(Flb),..FCde])
    }
    | _ default => valof{
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
    }
  }

  ptnCmp(Ptn,Lb) => case Ptn in {
    | .cInt(_,Ix) => .iICmp(Lb)
    | .cChar(_,Cx) => .iCCmp(Lb)
    | .cFlt(_,Dx) => .iFCmp(Lb)
    | _ => .iCmp(Lb)
  }.

  compPtnVar:(option[locn],string,srcLoc,Cont,codeCtx,stack) => (stack,multi[wOp]).
  compPtnVar(Lc,Nm,.lclVar(Off,Tp),Cont,Ctx,Stk) => Cont.C(Ctx,Stk,[.iStL(Off)]).

  compPtnArgs:(cons[cExp],Cont,Cont,codeCtx,stack) => (stack,multi[wOp]).
  compPtnArgs(Es,Succ,Fail,Ctx,Stk) => case Es in {
    | [] => Succ.C(Ctx,Stk,[])
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
    C:(codeCtx,stack,multi[wOp])=>(stack,multi[wOp]).
  }.

  allocCont:(termLbl,stack,Cont) => Cont.
  allocCont(Lbl,Stk,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.StructNew(wasmId(Lbl),.Implicit)])
  }.

  escapeCont:(string,stack,Cont) => Cont.
  escapeCont(Es,Stk,Cont) => cont{
    C(Ctx,_Stk,Cde) => Cont.C(Ctx,Stk,Cde++[.iEscape(Es),frameIns(Stk)]).
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
    C(_,_,Cde) => (.none,Cde++[.Return])
  }

  glbRetCont:(string)=>Cont.
  glbRetCont(Nm) => cont{
    C(_,_,Cde) => (.none,Cde++[.iTG(Nm),.iRtG])
  }

  jmpCont:(wasmLbl,stack)=>Cont.
  jmpCont(Lbl,Stk) => cont{
    C(Ctx,_Stk1,Cde) => (Stk,Cde++[.iJmp(Lbl)]).
    }

  raiseCont:Cont.
  raiseCont = cont{
    C(_,_,Cde) => (.none,Cde++[.iThrow])
  }

  contCont:(wasmLbl)=>Cont.
    contCont(Lbl) => cont{
    C(Ctx,Stk,Cde) => (Stk,Cde++[.iJmp(Lbl)]).
  }

  stoCont:(string,tipe,stack,Cont) => Cont.
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
      (Stk1,OCde) = compExp(Exp,TM,Cont,Ctx,Stk);
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

  closCont:(stack,termLbl,Cont) => Cont.
  closCont(Stk,Lbl,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.iClosure(Lbl)])
  }

  catchCont:all e ~~ (wasmLbl,integer,()=>(stack,multi[wOp])) => Cont.
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

  resetStack:(option[integer],stack) => (stack,multi[wOp]).
  resetStack(.some(Dp),.some(Stk)) =>
    case [|Stk|] in {
    | Dp => (.some(Stk),[])
    | Dp1 where Dp == Dp1-1 => (tail(Stk),[.iDrop])
    | Dp1 where Dp1>Dp => (.some(popTo(Stk,Dp)),[.iRst(Dp)])
    | Dp1 default => valof{	
	reportTrap("invalid stack height in $(Stk)\:$(Dp1)~$(Dp)");
	valis (.some(Stk),[])
      }
    }.
  resetStack(_,.none) => (.none,[]).

  resetCont:(stack,Cont) => Cont.
  resetCont(Stk,Cont) => cont{
    C(Ctx,XStk,Cde) => valof{
--      showMsg("reset stack $(XStk) to $(Stk)");
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

  frameIns:(stack)=>wOp.
  frameIns(.some(Stk)) => .iFrame(size(Stk)).

  locateVar:(string,codeCtx)=>option[srcLoc].
  locateVar(Nm,Ctx) => Ctx.vars[Nm].

  defineLclVar:(string,tipe,codeCtx) => (integer,codeCtx).
  defineLclVar(Nm,Tp,Ctx) => valof{
    hwm = Ctx.hwm;
    
    Off = hwm!+1;
    hwm := Off;

    valis (Off,Ctx.vars=Ctx.vars[Nm->.lclVar(Off,Tp)])
  }

  ensureLclVar:(string,tipe,codeCtx) => codeCtx.
  ensureLclVar(Nm,Tp,Ctx) => valof{
    if _ ?=Ctx.vars[Nm] then
      valis Ctx
    else
    valis snd(defineLclVar(Nm,Tp,Ctx))
  }

  argVars:(cons[cV],srcMap) => srcMap.
  argVars([],Mp)=>Mp.
  argVars([.cV(Nm,Tp),..As],Vars) =>
    argVars(As,Vars[Nm->.lclVar(Ix,Tp)]).
  argVars([_,..As],Map) => argVars(As,Map).

  glCtx:(codeCtx,cExp) => codeCtx.
  glCtx(Ctx,Exp) => valof{
    Vrs = glVars(Exp,[]);
    valis foldLeft((.cV(Nm,Tp),C)=>snd(defineLclVar(Nm,Tp,C)),Ctx,Vrs)
  }

  dsjCtx:(codeCtx,cExp,cExp) => codeCtx.
  dsjCtx(Ctx,L,R) => valof{
    CommonVrs = glVars(L,[]) /\ glVars(R,[]);

    valis foldLeft((.cV(Nm,Tp),Cx)=>snd(defineLclVar(Nm,Tp,Cx)),Ctx,CommonVrs)
  }

  drop:all x,e ~~ stream[x->>e] |: (x,integer)=>x.
  drop(S,0)=>S.
  drop([_,..S],N)=>drop(S,N-1).

  dropStack(.none) => .none.
  dropStack(.some([_,..Stk])) => .some(Stk).

  srcLoc ::= .lclVar(string,tipe) |
    .glbVar(string,tipe) |
    .glbFun(termLbl,tipe).

  codeCtx ::= codeCtx{
    vars : srcMap.
    end : wasmLbl.
    escape : wasmLbl.
    lbls : ref integer.  
    min : integer.
    hwm : ref integer.
    brks : map[string,Cont].
    constants : map[data,multi[wOp]].
  }

  stack ~> option[cons[tipe]].

  emptyCtx:(srcMap)=>codeCtx.
  emptyCtx(Glbs) => codeCtx{
    vars = Glbs.
    end = .al("$$").
    escape = .al("$$").
    lbls = ref 0.
    min = 0.
    hwm = ref 0.
    brks = [].
  }

  defineLbl:(string,codeCtx)=>wasmLbl.
  defineLbl(Pr,C) => valof{
    CurLbl = C.lbls!;
    C.lbls := CurLbl+1;
    valis .al("#(Pr)$(CurLbl)")
  }

  defineExitLbl:(string,codeCtx) => (wasmLbl,codeCtx).
  defineExitLbl(Pr,C) => valof{
    Lb = defineLbl(Pr,C);
    valis (Lb,C.escape=Lb)
  }

  pushStack:(tipe,stack) => stack.
  pushStack(Tp,?Stk) => ?[Tp,..Stk].

  loadStack:(cons[tipe],stack) => stack.
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
    disp(C) => "<C hwm:$(C.hwm!)\:$(C.vars)>C>".
  }

  srcMap ::= srcMap{
    globals:map[string,srcLoc].
    locals:map[string,srcLoc].
    types:map[string,(tipe,integer)]
  }

  declareGlobal:(string,srcLoc,srcMap)=>srcMap.
  declareGlobal(Nm,Loc,Mp) => Mp.globals=Mp.globals[Nm->Loc].

  declareLocal:(string,srcLoc,srcMap)=>srcMap.
  declareLocal(Nm,Loc,Mp) =>  Mp.locals=Mp.locals[Nm->Loc].

  implementation display[srcLoc] => {
    disp(L) => case L in {
      | .lclVar(Off,Tpe) => "lcl $(Off)\:$(Tpe)"
      | .glbVar(Off,Tpe) => "glb $(Off)\:$(Tpe)"
      | .glbFun(Off,Tpe) => "fun $(Off)\:$(Tpe)"
    }
  }

  collectLocals:(cExp,srcMap) => srcMap.
  collectLocals(Exp,Mp) => collectExpLcls(Exp,Mp).

  collectExpLcls:(cExp,srcMap) => srcMap.
  collectExpLcls(Exp,Mp) => case Exp in {
    | .cVoid(_,_) => Mp
    | .cAnon(_,_) => Mp
    | .cVar(_,.cV(Nm,Tp)) => declareLocal(Nm,.lclVar(Nm,Tp),Mp)
    | .cInt(_,_) => Mp
    | .cChar(_,_) => Mp
    | .cBig(_,_) => Mp
    | .cFlt(_,_) => Mp
    | .cString(_,_) => Mp
    | .cTerm(_,_,Args,_) => collectArgs(Args,Mp)
    | .cNth(_,Rc,_,_) => collectExpLcls(Rc,Mp)
    | .cSetNth(_,Rc,_,Vl) => collectExpLcls(Rc,collectExpLcls(Vl,Mp))
    | .cClos(_,_,_,Cl,_) => collectExpLcls(Cl,Mp)
    | .cThnk(_,Exp,_) => collectExpLcls(Exp,Mp)
    | .cThDrf(_,Exp,_) => collectExpLcls(Exp,Mp)
    | .cCall(_,_,Args,_) => collectArgs(Args,Mp)
    | .cOCall(_,Op,Args,_) => collectArgs(Args,collectExpLcls(Op,Mp))
    | .cRaise(_,Vr,Ex,_) => collectExpLcls(Vr,collectExpLcls(Ex,Mp))
    | .cSeq(_,L,R) => collectExpLcls(L,collectExpLcls(R,Mp))
    | .cCnj(_,L,R) => collectExpLcls(L,collectExpLcls(R,Mp))
    | .cDsj(_,L,R) => collectExpLcls(L,collectExpLcls(R,Mp))
    | .cNeg(_,R) => collectExpLcls(R,Mp)
    | .cCnd(_,T,L,R) => collectExpLcls(L,collectExpLcls(R,collectExpLcls(T,Mp)))
    | .cLtt(Lc,V,F,B) => collectExpLcls(F,collectExpLcls(B,collectExpLcls(.cVar(Lc,V),Mp)))
    | .cCase(_,G,Cses,Dflt,_) => collectExlLcls(Dflt,collectCses(Cses,collectExpLcls,collectExpLcls(G,Mp)))
    | .cMatch(_,L,R) => collectExpLcls(L,collectExpLcls(R,Mp))
    | .cVarNmes(_,_,X) => collectExpLcls(X,Mp)
    | .cAbort(_,_,_) => Mp
    | .cTry(_,B,T,E,H,_) => collectExpLcls(B,collectExpLcls(T,collectExpLcls(E,collectExpLcls(H,Mp))))
    | .cValof(_,A,_) => collectActLcls(A,Mp)
  }

  collectArgs(Args,Mp) => foldLeft((E,MM) => collectExpLcls(E,MM),Mp,Args).

  collectCses:all e ~~ (cons[cCase[e]],(e,srcMap)=>srcMap,srcMap)=>srcMap.
  collectCses([],_,Mp) => Mp.
  collectCses([(_,Ptn,Vl),..Cs],F,Mp) =>
    collectSes(Cs,F,collectExpLcls(Ptn,F(Vl,Mp))).
  
  collectActLcls(A,Mp) => case A in {
    | .aNop(_) => Mp
    | .aSeq(_,L,R) => collectActLcls(L,collectActLcls(R,Mp))
    | .aLbld(_,_,B) => collectActLcls(B,Mp)
    | .aBreak(_,_) => Mp
    | .aValis(_,X) => collectExpLcls(X,Mp)
    | .aRetire(_,F,X) => collectExpLcls(X,collectExpLcls(F,Mp))
    | .aDo(_,X) => collectExpLcls(X,Mp)
    | .aSetNth(_,R,_,V) => collectExpLcls(R,collectExpLcls(V,Mp))
    | .aDefn(_,V,X) => collectExpLcls(X,collectExpLcls(V,Mp))
    | .aAsgn(_,V,X) => collectExpLcls(X,collectExpLcls(V,Mp))
    | .aCase(_,G,Cs,D) => collectCses(Cs,collectExpLcls(G,collectActLcls(D,Mp)))
    | .aIftte(_,T,L,R) => collectExpLcls(T,collectActLcls(L,collectActLcls(R,Mp)))
    | .aWhile(_,T,B) => collectActLcls(B,collectExpLcls(T,Mp))
    | .aTry(_,B,V,E,H) => collectActLcls(B,collectExpLcls(V,collectExpLcls(E,collectActLcls(H,Mp))))
    | .aLtt(Lc,V,X,B) =>
      collectActLcls(B,collectExpLcls(.cVar(Lc,V),collectExpLcls(X,Mp)))
    | .aVarNmes(_,_,B) => collectActLcls(B,Mp)
    | .aAbort(_,_) => Mp
  }

  wasmInt(Ix) where isSMI(Ix) => [.IConst(.I32Type,Ix),.i31Ref].
  wasmInt(Ix) => [.IConst(.I64Type,Ix),.StructNew(boxedIntType)].

  wasmFloat(Dx) => [.FConst(.F64Type,Dx),.StructNew(boxedFltType)].

  wasmId(.tLbl(Nm,_)) => Nm.
  
}
