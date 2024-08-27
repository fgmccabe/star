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
  declGlobal(_,Vrs) => Vrs.

  localFuns:(cons[cDefn],map[string,srcLoc])=>map[string,srcLoc].
  localFuns(Defs,Vars) => foldRight(defFun,Vars,Defs).

  defFun(Def,Vrs) => case Def in {
    | .fnDef(Lc,Nm,Tp,_,_) => Vrs[Nm->.glbFun(.tLbl(Nm,arity(Tp)),Tp::ltipe)]
    | .glDef(Lc,Nm,Tp,_) => Vrs[Nm->.glbVar(Nm,Tp::ltipe)]
    | _ default => Vrs
  }
  
  compDefs:(cons[cDefn],map[string,srcLoc])=> cons[codeSegment].
  compDefs(Dfs,Glbs) => (Dfs//(D)=>genDef(D,Glbs)).

  genDef:(cDefn,map[string,srcLoc]) => codeSegment.
  genDef(Defn,Glbs) => case Defn in {
    | .fnDef(Lc,Nm,Tp,Args,Val) => valof {
      if traceCodegen! then
	showMsg("compile $(.fnDef(Lc,Nm,Tp,Args,Val))");
      Ctx = emptyCtx(Glbs);
      (_,AbortCde) = abortCont(Lc,"function: $(Nm)").C(Ctx,.some([]),[]);

      (_Stk,Code) = compArgPtns(Args,Lc,0,expCont(Val,Lc,.noMore,retCont),
	jmpCont(Ctx.escape,.none),Ctx,.some([]));

      if traceCodegen! then
	showMsg("non-peep code is $((Code++[.iLbl(Ctx.escape),..AbortCde])::cons[assemOp])");
      Peeped = peepOptimize(([.iLocals(Ctx.hwm!),..Code]++[.iLbl(Ctx.escape),..AbortCde])::cons[assemOp]);
      if traceCodegen! then{
	showMsg("code is $(.func(.tLbl(Nm,size(Args)),.hardDefinition,Tp,Ctx.hwm!,Peeped))");
      };
      valis .func(.tLbl(Nm,size(Args)),.hardDefinition,Tp,Ctx.hwm!,Peeped)
    }
    | .glDef(Lc,Nm,Tp,Val) => valof{
      if traceCodegen! then
	showMsg("compile global $(Nm)\:$(Tp) = $(Val))");
      Ctx = emptyCtx(Glbs);
      (_,AbortCde) = abortCont(Lc,"global: $(Nm)").C(Ctx,.none,[]);
      (_Stk,Code) = compExp(Val,Lc,.notLast,glbRetCont(Nm),Ctx,.some([]));

      if traceCodegen! then
	showMsg("non-peep code is $((Code++[.iLbl(Ctx.escape),..AbortCde])::cons[assemOp])");
      
      Peeped = peepOptimize(([.iLocals(Ctx.hwm!),..Code]++[.iLbl(Ctx.escape),..AbortCde])::cons[assemOp]);
      if traceCodegen! then
	showMsg("code is $(.global(.tLbl(Nm,0),Tp,Ctx.hwm!,Peeped))");
    
      valis .global(.tLbl(Nm,0),Tp,Ctx.hwm!,Peeped)
    }
  | .tpDef(Lc,Tp,TpRl,Index) => .tipe(Tp,TpRl,Index)
  | .lblDef(_Lc,Lbl,Tp,Ix) => .struct(Lbl,Tp,Ix)
  }

  compExp:(cExp,option[locn],tailMode,Cont,codeCtx,stack) =>
    (stack,multi[assemOp]).
  compExp(Exp,OLc,TM,Cont,Ctx,Stk) => case Exp in {
    | E where isGround(E) =>
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
    | .cTerm(Lc,Nm,Args,Tp) => valof{
      valis compExps(Args,Lc,allocCont(.tLbl(Nm,size(Args)),pushStack(Tp::ltipe,Stk),Cont),Ctx,Stk)
    }
    | .cCall(Lc,Nm,Args,Tp) where (_,Ins,Frm,Tail)?=intrinsic(Nm) =>
      compExps(Args,Lc,intrinsicCont(Ins,Frm,Tail,pushStack(Tp::ltipe,Stk),Cont),Ctx,Stk)
    | .cCall(Lc,Nm,Args,Tp) where isEscape(Nm) =>
      compExps(Args,Lc,escapeCont(Nm,pushStack(Tp::ltipe,Stk),Cont),Ctx,Stk)
    | .cCall(Lc,Nm,Args,Tp) =>
      compExps(Args,Lc,callCont(.tLbl(Nm,[|Args|]),TM,pushStack(Tp::ltipe,Stk),Cont),Ctx,Stk)
    | .cOCall(Lc,Op,Args,Tp) => 
      compExps(Args,Lc,expCont(Op,Lc,.notLast,oclCont([|Args|]+1,TM,pushStack(Tp::ltipe,Stk),Cont)),Ctx,Stk)
    | .cNth(Lc,E,Ix,Tp) =>
      compExp(E,Lc,.notLast,nthCont(Ix,Cont,pushStack(Tp::ltipe,Stk)),Ctx,Stk)
    | .cSetNth(Lc,R,Ix,V) => compExp(R,Lc,.notLast,expCont(V,Lc,.notLast,setNthCont(Ix,Cont,Stk)),Ctx,Stk)
    | .cClos(Lc,L,A,F,Tp) => compExp(F,Lc,TM,closCont(pushStack(Tp::ltipe,Stk),.tLbl(L,A),Cont),Ctx,Stk)
    | .cThnk(Lc,Th,Tp) => compExp(Th,Lc,.notLast,
      thunkCont(pushStack(Tp::ltipe,Stk),Cont),Ctx,Stk)
    | .cThDrf(Lc,Th,Tp) => valof{
      Lb = defineLbl("Th",Ctx);
      valis compExp(Th,Lc,.notLast,thunkRefCont(Lb,pushStack(Tp::ltipe,Stk),Cont),Ctx,Stk)
    }
    | .cSeq(Lc,L,R) =>
      compExp(L,Lc,.notLast,resetStkCont(Stk,expCont(R,Lc,TM,Cont)),Ctx,Stk)
    | .cCnd(Lc,G,L,R) => valof{
      CC = splitCont(Lc,Ctx,Cont);
      valis compCond(G,Lc,TM,expCont(L,Lc,TM,CC),ctxCont(Ctx,expCont(R,Lc,TM,CC)),Ctx,Stk)
    }
    | .cCase(Lc,Gov,Cases,Deflt,_Tp) => valof{
      valis compCase(Lc,Gov,Cases,Deflt, (E,C1)=>expCont(E,Lc,TM,C1),Cont,Ctx,Stk)
    }
    | .cLtt(Lc,.cId(Vr,VTp),Val,Bnd) => valof{
      valis compExp(Val,Lc,.notLast,stoCont(Vr,VTp::ltipe,Stk,expCont(Bnd,Lc,TM,Cont)),Ctx,Stk)
    }
    | .cAbort(Lc,Msg,Tp) => abortCont(Lc,Msg).C(Ctx,Stk,[])
    | .cTry(Lc,B,.cVar(_,.cId(Th,ThTp)),.cVar(_,.cId(Er,ETp)),H,Tp) => valof{

      if traceCodegen! then
	showMsg("compiling try catch @$(Lc), $(B), catch $(H)");
      
      (CLb,Ctx0) = defineExitLbl("Tr",Ctx);
      Blk = defineLbl("H",Ctx);
      (TOff,CtxB) = defineLclVar(Th,ThTp::ltipe,Ctx0);
      (EOff,Ctx1) = defineLclVar(Er,ETp::ltipe,Ctx);
      
      (Stk1,BCde) = compExp(B,Lc,.notLast,tryEndCont(TOff,Cont),CtxB,Stk); -- critical: body of try is not tail rec
      (Stk2,HCde) = compExp(H,Lc,TM,Cont,Ctx1,Stk);

      if ~reconcileable(Stk1,Stk2) then
	reportError("cannot reconcile try exp $(B) with handler $(H)",Lc);

      valis (reconcileStack(Stk1,Stk2),[.iTry(Blk),.iStL(TOff)]++BCde++[.iLbl(Blk),.iStL(EOff)]++HCde)
    }
    | .cReset(Lc,E,Tp) => compExp(E,Lc,.notLast,resetCont(pushStack(Tp::ltipe,Stk),Cont),Ctx,Stk)
    | .cInvoke(Lc,K,E,Tp) => compExp(K,Lc,.notLast,expCont(E,Lc,.notLast,invokeCont(TM,pushStack(Tp::ltipe,Stk),Cont)),Ctx,Stk)
    | .cShift(Lc,T,E,Tp) => compExp(T,Lc,.notLast,expCont(E,Lc,.notLast,shiftCont(pushStack(Tp::ltipe,Stk),Cont)),Ctx,Stk)
    | .cRaise(Lc,T,E,_) => compExp(E,Lc,.notLast,expCont(T,Lc,.notLast,raiseCont),Ctx,Stk)
    | .cValof(Lc,A,Tp) =>
      compAction(A,Lc,TM,abortCont(Lc,"missing valis action"),splitCont(Lc,Ctx,Cont),Ctx,Stk)
    |  C where isCond(C) => valof{
      Nx = defineLbl("E",Ctx);
      Stk0 = pushStack(boolType::ltipe,Stk);
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
  compExps:(cons[cExp],option[locn],Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compExps([],_,Cont,Ctx,Stk)=>Cont.C(Ctx,Stk,[]).
  compExps([Exp,..Es],Lc,Cont,Ctx,Stk)=>
    compExps(Es,locOf(Exp),ctxCont(Ctx,expCont(Exp,Lc,.notLast,Cont)),Ctx,Stk).

  compVar:(option[locn],string,srcLoc,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compVar(Lc,_Nm,Loc,Cont,Ctx,Stk) => case Loc in {
    | .argVar(Off,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdA(Off)])
    | .lclVar(Off,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdL(Off)])
    | .glbVar(Nm,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdG(Nm)])
    | .glbFun(Nm,Tp) => Cont.C(Ctx,pushStack(Tp,Stk),[.iLdC(.symb(Nm))])
  }
    
  compCond:(cExp,option[locn],tailMode,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compCond(C,OLc,TM,Succ,Fail,Ctx,Stk) => case C in {
    | .cTerm(_,"star.core#true",[],_) => Succ.C(Ctx,Stk,[])
    | .cTerm(_,"star.core#false",[],_) => Fail.C(Ctx,Stk,[])
    | .cCnj(Lc,L,R) => valof{
      FC = splitCont(Lc,Ctx,ctxCont(Ctx,Fail));
      valis compCond(L,Lc,.notLast,condCont(R,Lc,TM,Succ,FC,Stk),FC,Ctx,Stk)
    }
    | .cDsj(Lc,L,R) => valof{
      Ctxa = dsjCtx(Ctx,L,R);
      SC = splitCont(Lc,Ctxa,ctxCont(Ctxa,Succ));
      
      valis compCond(L,Lc,TM,SC,ctxCont(Ctxa,condCont(R,Lc,TM,SC,Fail,Stk)),Ctxa,Stk)
    }
    | .cNeg(Lc,R) => compCond(R,Lc,TM,ctxCont(Ctx,Fail),ctxCont(Ctx,Succ),Ctx,Stk)
    | .cCnd(Lc,T,L,R) => valof{
      Ctxa = dsjCtx(Ctx,L,R);
      SC = splitCont(Lc,Ctxa,Succ);
      FC = splitCont(Lc,Ctxa,Fail);
      
      valis compCond(T,Lc,.notLast,condCont(L,Lc,TM,SC,FC,Stk),ctxCont(Ctxa,condCont(R,Lc,TM,SC,FC,Stk)),Ctxa,Stk)
    }
    | .cMatch(Lc,Ptn,Exp) => compExp(Exp,Lc,.notLast,ptnCont(Ptn,Lc,Succ,Fail),Ctx,Stk)
    | Exp default => compExp(Exp,OLc,.notLast,ifCont(locOf(Exp),Stk,Succ,Fail),Ctx,Stk)
  }

  compAction:(aAction,option[locn],tailMode,Cont,Cont,codeCtx,stack) =>(stack,multi[assemOp]).
  compAction(A,OLc,TM,ACont,Cont,Ctx,Stk) => case A in {
    | .aNop(_Lc) => ACont.C(Ctx,Stk,[])
    | .aSeq(Lc,L,R) => compAction(L,Lc,.notLast,resetStkCont(Stk,actionCont(R,Lc,TM,ACont,Cont)),Cont,Ctx,Stk)
    | .aLbld(Lc,Lb,LbldA) => valof{
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
      Lp = defineLbl("Lp",Ctx);
      Tst = defineLbl("Tst",Ctx);
      Ex = defineLbl("Ex",Ctx);
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
      
      Blk = defineLbl("H",Ctx);
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
    Nxt = defineLbl("CN",Ctx);
    DLbl = defineLbl("CD",Ctx);

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

      Lb = defineLbl("CC",Ctx);
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
      
      Fl = defineLbl("CF",Ctx);
      VLb = defineLbl("CN",Ctx);
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
      Fl = defineLbl("CM",Ctx);
      
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
    (Stk1,Code1) = compPttrn(Ptn,Lc,Succ,Fail,Ctx,pushStack(typeOf(Ptn)::ltipe,Stk));
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

  compPttrn:(cExp,option[locn],Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPttrn(Ptn,Lc,Succ,Fail,Ctx,Stk) => compPtn(Ptn,Lc,Succ,Fail,Ctx,Stk).
  
  compPtn:(cExp,option[locn],Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtn(Ptn,OLc,Succ,Fail,Ctx,Stk) => case Ptn in {
    | .cVar(_,.cId("_",_)) => Succ.C(Ctx,dropStack(Stk),[.iDrop])
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
      
      (Stk2,SCde) = compPtnArgs(Args,Lc,Succ,resetStkCont(Stk0,jmpCont(Flb,Stk1)),Ctx,loadStack(Args//(A)=>(typeOf(A)::ltipe),Stk0));

      if traceCodegen! then
	showMsg("Succ stack $(Stk2), Fail stack $(Stk1)");

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

  compPtnVar:(option[locn],string,srcLoc,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtnVar(Lc,Nm,.lclVar(Off,Tp),Cont,Ctx,Stk) => Cont.C(Ctx,Stk,[.iStL(Off)]).
  compPtnVar(Lc,Nm,.argVar(Off,Tp),Cont,Ctx,Stk) => Cont.C(Ctx,Stk,[.iDrop]).

  compPtnArgs:(cons[cExp],option[locn],Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtnArgs(Es,Lc,Succ,Fail,Ctx,Stk) => case Es in {
    | [] => Succ.C(Ctx,Stk,[])
    | [A,..As] => compPtn(A,Lc,argsPtnCont(As,locOf(A),Succ,Fail),Fail,Ctx,Stk)
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

  thunkCont:(stack,Cont)=>Cont.
  thunkCont(Stk,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.iThunk])
  }

  thunkRefCont:(assemLbl,stack,Cont) => Cont.
  thunkRefCont(Lbl,Stk,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.iLdTh(Lbl),.iRot(1),.iTTh,.iLbl(Lbl)])
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
      Flb = defineLbl("F",Ctx);
      (SStk,SCde) = Succ.C(Ctx,Stk,[.iIfNot(Flb)]);
      (FStk,FCde) = Fail.C(Ctx,Stk,[]);
      
      valis (reconcileStack(SStk,FStk),Cde++SCde++[.iLbl(Flb),..FCde])
    }
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
      (Stk2,CCde) = compCond(Cond,Lc,TM,Succ,Fail,Ctx,Stk1);
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

  closCont:(stack,termLbl,Cont) => Cont.
  closCont(Stk,Lbl,Cont) => cont{
    C(Ctx,_,Cde) => Cont.C(Ctx,Stk,Cde++[.iClosure(Lbl)])
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

  actionCont:(aAction,option[locn],tailMode,Cont,Cont) => Cont.
  actionCont(A,OLc,TM,ACont,Cont) => cont{
    C(Ctx,Stk,Cde) => valof{
      (SStk,SCde) = compAction(A,OLc,TM,ACont,Cont,Ctx,Stk);
      valis (SStk,Cde++SCde)
    }
  }

  splitCont:(option[locn],codeCtx,Cont) => Cont.
  splitCont(Lc,Ctx0,Cnt) => valof{
    Lb = defineLbl("Splt",Ctx0);      -- Create a new label
    tStk = ref .none;
    triggered = ref .false;
    
    valis cont{
      C(_Ctx,Stk,Cde) => valof{
	if triggered! then{
	  valis (tStk!,Cde++[.iJmp(Lb)])
	} else{
	  triggered := .true;
	  
	  (rStk,rCde) = Cnt.C(Ctx0,Stk,Cde++[.iLbl(Lb)]);
	  
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

  defineLclVar:(string,ltipe,codeCtx) => (integer,codeCtx).
  defineLclVar(Nm,Tp,Ctx) => valof{
    hwm = Ctx.hwm;
    
    Off = hwm!+1;
    hwm := Off;

    valis (Off,Ctx.vars=Ctx.vars[Nm->.lclVar(Off,Tp)])
  }

  ensureLclVar:(string,ltipe,codeCtx) => codeCtx.
  ensureLclVar(Nm,Tp,Ctx) => valof{
    if _ ?=Ctx.vars[Nm] then
      valis Ctx
    else
    valis snd(defineLclVar(Nm,Tp,Ctx))
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

  srcLoc ::= .lclVar(integer,ltipe) |
  .argVar(integer,ltipe) |
  .glbVar(string,ltipe) |
  .glbFun(termLbl,ltipe) |
  .thnkFn(termLbl,ltipe).

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
    valis (Lb,C.escape=Lb)
  }

  pushStack:(ltipe,stack) => stack.
  pushStack(Tp,.some(Stk)) => .some([Tp,..Stk]).

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
    disp(C) => "<C hwm:$(C.hwm!)\:$(C.vars)>C>".
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
  chLine(.none,_) => [].
  chLine(.some(Lc),.some(Lc)) => [].
  chLine(_,.some(Lc)) => (genDebug! ?? [.iLine(Lc::data)] || []).
}
