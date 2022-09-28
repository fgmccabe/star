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
  compProg(Pkg,Defs,Globals) =>
    compDefs(Defs,localFuns(Defs,foldRight(((Pk,Tp),G)=>G[Pk->glbVar(Pk,Tp)],[],Globals)),
      []).

  localFuns:(cons[cDefn],map[string,srcLoc])=>map[string,srcLoc].
  localFuns(Defs,Vars) => foldRight(defFun,Vars,Defs).

  defFun(.fnDef(Lc,Nm,Tp,_,_),Vrs) => Vrs[Nm->glbFun(tLbl(Nm,arity(Tp)),Tp::ltipe)].
  defFun(.vrDef(Lc,Nm,Tp,_),Vrs) => Vrs[Nm->glbVar(Nm,Tp::ltipe)].
  defFun(D,Vrs) => Vrs.
  
  compDefs:(cons[cDefn],map[string,srcLoc],cons[codeSegment])=> cons[codeSegment].
  compDefs([],_,Cs)=>Cs.
  compDefs([D,..Dfs],Glbs,Cs) => 
    compDefs(Dfs,Glbs,[genDef(D,Glbs),..Cs]).

  genDef:(cDefn,map[string,srcLoc]) => codeSegment.
  genDef(.fnDef(Lc,Nm,Tp,Args,Val),Glbs) => valof{
    if showCode! then
      logMsg("compile $(fnDef(Lc,Nm,Tp,Args,Val))");
    Ctx = emptyCtx(argVars(Args,Glbs,0));
    (Code,_Stk) = compExp(Val,retCont,abortCont(Lc,"function: $(Nm)"),Ctx,.some([]));
    valis func(tLbl(Nm,size(Args)),.hardDefinition,Tp,peepOptimize(Code::cons[assemOp]))
  }
  genDef(.vrDef(Lc,Nm,Tp,Val),Glbs) => valof{
    if showCode! then
      logMsg("compile global $(Nm)\:$(Tp) = $(Val))");
    Ctx = emptyCtx(Glbs);
    (Code,_Stk) = compExp(Val,glbRetCont(Nm),abortCont(Lc,"global: $(Nm)"),Ctx,.some([]));
    valis global(tLbl(Nm,0),Tp,peepOptimize(Code::cons[assemOp]))
  }
  genDef(.tpDef(Lc,Tp,TpRl,Index),_) => tipe(TpRl,Index).

  compExp:(cExp,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compExp(Exp,Cont,_ECont,Ctx,Stk) where isGround(Exp) =>
    Cont.C(Ctx,pushStack(typeOf(Exp)::ltipe,Stk),[.iLdC(Exp::data)]).
  compExp(.cVar(Lc,.cId(Vr,Tp)),Cont,_ECont,Ctx,Stk) => valof{
    if Loc^=locateVar(Vr,Ctx) then {
      valis compVar(Lc,Vr,Loc,Cont,Ctx,Stk)
    } else {
      reportError("cannot locate variable $(Vr)\:$(Tp)",Lc);
      valis Cont.C(Ctx,pushStack(Tp::ltipe,Stk),[.iLdV])
    }
  }
  compExp(.cTerm(_,Nm,Args,Tp),Cont,ECont,Ctx,Stk) =>
    compExps(Args,allocCont(tLbl(Nm,size(Args)),Tp::ltipe,Stk,Cont),ECont,Ctx,Stk).
  compExp(.cECall(Lc,Op,Args,Tp),Cont,ECont,Ctx,Stk) where (_,Ins)^=intrinsic(Op) =>
    compExps(Args,intrinsicCont(Ins,Tp::ltipe,Stk,Cont),ECont,Ctx,Stk).
  compExp(.cECall(Lc,Es,Args,Tp),Cont,ECont,Ctx,Stk) =>
    compExps(Args,escapeCont(Es,Tp::ltipe,Stk,Cont),ECont,Ctx,Stk).
  compExp(.cCall(Lc,Nm,Args,Tp),Cont,ECont,Ctx,Stk) =>
    compExps(Args,callCont(tLbl(Nm,[|Args|]),Tp::ltipe,Stk,Cont),ECont,Ctx,Stk).
  compExp(.cOCall(Lc,Op,Args,Tp),Cont,ECont,Ctx,Stk) => 
    compExps(Args,expCont(Op,oclCont([|Args|],Tp::ltipe,Stk,Cont),ECont),ECont,Ctx,Stk).
  compExp(.cNth(Lc,E,Ix,Tp),Cont,ECont,Ctx,Stk) =>
    compExp(E,nthCont(Ix,Tp,Cont,Stk),ECont,Ctx,Stk).
  compExp(.cSetNth(Lc,R,Ix,V),Cont,ECont,Ctx,Stk) =>
    compExp(R,expCont(V,setCont(Ix,typeOf(R),Cont,Stk),ECont),ECont,Ctx,Stk).
  compExp(.cThrow(Lc,Exp,_),_Cont,ECont,Ctx,Stk) =>
    compExp(Exp,ECont,abortCont(Lc,"throw"),Ctx,Stk).
  compExp(.cSeq(_,L,R),Cont,ECont,Ctx,Stk) =>
    compExp(L,resetCont(Stk,expCont(R,Cont,ECont)),ECont,Ctx,Stk).
  compExp(.cCnd(Lc,G,L,R),Cont,ECont,Ctx,Stk) => valof{
    CC = splitCont(Lc,Ctx,Cont);
    EE = splitCont(Lc,Ctx,ECont);
    valis compCond(G,expCont(L,CC,EE),resetCont(Stk,expCont(R,CC,EE)),ECont,Ctx,Stk)
  }
  compExp(.cCase(Lc,Exp,Cases,Deflt,Tp),Cont,ECont,Ctx,Stk) =>
    compCase(Lc,Exp,Cases,Deflt,Tp,Cont,ECont,Ctx,Stk).
  compExp(.cUnpack(Lc,Exp,Cases,Tp),Cont,ECont,Ctx,Stk) =>
    compCnsCase(Lc,Exp,Cases,Tp,Cont,ECont,Ctx,Stk).
  compExp(.cLtt(Lc,.cId(Vr,VTp),Val,Exp),Cont,ECont,Ctx,Stk) => valof{
    (Off,Ctx1) = defineLclVar(Vr,VTp::ltipe,Ctx);
    (EX,Ctx2) = defineExitLbl("_",Ctx1);
    valis compExp(Val,stoCont(Off,Stk,expCont(Exp,Cont,ECont)),ECont,Ctx2,Stk)
  }
  compExp(.cAbort(Lc,Msg,Tp),Cont,ECont,Ctx,Stk) =>
    abortCont(Lc,Msg).C(Ctx,Stk,[]).
  compExp(.cWhere(Lc,E,C),Cont,ECont,Ctx,Stk) =>
    compCond(C,expCont(E,Cont,ECont),ECont,ECont,Ctx,Stk).
  compExp(.cSusp(Lc,Fb,Ev,Tp),Cont,ECont,Ctx,Stk) => 
    compExp(Fb,expCont(Ev,suspendCont(Stk,Tp::ltipe,Cont),ECont),ECont,Ctx,Stk).
  compExp(.cResume(Lc,Fb,Ev,Tp),Cont,ECont,Ctx,Stk) => 
    compExp(Fb,expCont(Ev,resumeCont(Stk,Tp::ltipe,Cont),ECont),ECont,Ctx,Stk).
  compExp(.cTry(Lc,B,H,Tp),Cont,ECont,Ctx,Stk) => valof{
    (CLb,CtxB) = defineExitLbl("C",Ctx);
    valis compExp(B,Cont,catchCont(H,CLb,Cont,ECont,Ctx,Stk,Tp::ltipe),CtxB,Stk)
  }
--  compExp(cValof(Lc,A,Tp),Cont,ECont,Ctx,Stk) => valof{
--  }
    
  compExp(C,Cont,ECont,Ctx,Stk) where isCond(C) => valof{
    Nx = defineLbl("E",Ctx);
    (Stk1,Cde) = compCond(C,trueCont(jmpCont(Nx,Stk)),
      falseCont(jmpCont(Nx,Stk)),ECont,Ctx,Stk);
    valis Cont.C(Ctx,pushStack(boolType::ltipe,Stk),Cde++[.iLbl(Nx)])
  }
  compExp(C,Cont,_,Ctx,Stk) => valof{
    reportError("cannot compile expression $(C)",locOf(C));
    valis Cont.C(Ctx,Stk,[])
  }
    
  -- Expressions are evaluated in reverse order
  compExps:(cons[cExp],Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compExps([],Cont,_,Ctx,Stk)=>Cont.C(Ctx,Stk,[]).
  compExps([Exp,..Es],Cont,ECont,Ctx,Stk)=>
    compExps(Es,expCont(Exp,Cont,ECont),ECont,Ctx,Stk).

  compVar:(option[locn],string,srcLoc,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compVar(_Lc,Nm,.argVar(Off,Tp),Cont,Ctx,Stk) =>
    Cont.C(Ctx,pushStack(Tp,Stk),[.iLdA(Off)]).
  compVar(_Lc,Nm,.lclVar(Off,Tp),Cont,Ctx,Stk) =>
    Cont.C(Ctx,pushStack(Tp,Stk),[.iLdL(Off)]).
  compVar(_Lc,_,.glbVar(Nm,Tp),Cont,Ctx,Stk) =>
    Cont.C(Ctx,pushStack(Tp,Stk),[.iLdG(Nm,Ctx.escape)]).
  compVar(_Lc,_,.glbFun(Nm,Tp),Cont,Ctx,Stk) =>
    Cont.C(Ctx,pushStack(Tp::ltipe,Stk),[.iLdC(symb(Nm))]).

  compCond:(cExp,Cont,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compCond(.cTerm(_,"star.core#true",[],_),Succ,_Fail,_ECont,Ctx,Stk) =>
    Succ.C(Ctx,Stk,[]).
  compCond(.cTerm(_,"star.core#false",[],_),_Succ,Fail,_ECont,Ctx,Stk) =>
    Fail.C(Ctx,Stk,[]).
  compCond(.cCnj(Lc,L,R),Succ,Fail,ECont,Ctx,Stk) => valof{
    FC = splitCont(Lc,Ctx,Fail);
    valis compCond(L,condCont(R,Succ,FC,ECont,Stk),FC,ECont,Ctx,Stk)
  }
  compCond(.cDsj(Lc,L,R),Succ,Fail,ECont,Ctx,Stk) => valof{
    SC = splitCont(Lc,Ctx,Succ);
    valis compCond(L,SC,ctxCont(Ctx,condCont(R,SC,Fail,ECont,Stk)),ECont,Ctx,Stk)
  }
  compCond(.cNeg(Lc,R),Succ,Fail,ECont,Ctx,Stk) =>
    compCond(R,ctxCont(Ctx,Fail),ctxCont(Ctx,Succ),ECont,Ctx,Stk).
  compCond(.cCnd(Lc,T,L,R),Succ,Fail,ECont,Ctx,Stk) => valof{
    FC = splitCont(Lc,Ctx,Fail);
    valis compCond(T,condCont(L,Succ,FC,ECont,Stk),condCont(R,Succ,FC,ECont,Stk),
      ECont,Ctx,Stk)
  }
  compCond(.cMatch(Lc,Ptn,Exp),Succ,Fail,ECont,Ctx,Stk) => valof{
    valis compExp(Exp,ptnCont(Ptn,Succ,Fail,ECont),ECont,Ctx,Stk)
  }
  compCond(Exp,Succ,Fail,ECont,Ctx,Stk) =>
    compExp(Exp,ifCont(locOf(Exp),Stk,Succ,Fail),ECont,Ctx,Stk).

  compCase:(option[locn],cExp,cons[cCase[cExp]],cExp,tipe,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compCase(Lc,Gv,Cases,Deflt,Tp,Cont,ECont,Ctx,Stk) => valof{
    Nxt = defineLbl("CN",Ctx);
    DLbl = defineLbl("CD",Ctx);
    (Stk1,GCode) = compExp(Gv,jmpCont(Nxt,pushStack(typeOf(Gv)::ltipe,Stk)),ECont,Ctx,Stk);
    (Table,Max) = genCaseTable(Cases);
    OC = splitCont(Lc,Ctx,Cont);
    EC = splitCont(Lc,Ctx,ECont);
    (Stk2,CCode,TCode) = compCases(Table,0,Max,OC,jmpCont(DLbl,Stk),EC,DLbl,Ctx,Stk1);
    (Stk3,DCode) = compExp(Deflt,OC,EC,Ctx,Stk);
    Hgt = ^[|Stk|];

    valis (reconcileStack(Lc,Stk2,Stk3),
      GCode++[.iLbl(Nxt),.iCase(Max)]++TCode++CCode++[.iLbl(DLbl),.iRst(Hgt)]++DCode)
  }

  compCases:(cons[csEntry],integer,integer,Cont,Cont,Cont,assemLbl,codeCtx,stack) =>
    (stack,multi[assemOp],multi[assemOp]).
  compCases([],Mx,Mx,_,_,_,_,_,Stk) => (Stk,[],[]).
  compCases([],Ix,Mx,Succ,Fail,ECont,Deflt,Ctx,Stk) where Ix<Mx => valof{
    (Stk1,TCde,Cde) = compCases([],Ix+1,Mx,Succ,Fail,ECont,Deflt,Ctx,Stk);
    valis (Stk1,TCde++[.iJmp(Deflt)],Cde)
  }
  compCases([(Ix,Case),..Cases],Ix,Mx,Succ,Fail,ECont,Deflt,Ctx,Stk) => valof{
    Lb = defineLbl("CC",Ctx);
    (Stk2,TCde2,Cde2) = compCases(Cases,Ix+1,Mx,Succ,Fail,ECont,Deflt,Ctx,Stk);
    (Stk3,CCde) = compCaseBranch(Case,Succ,Fail,ECont,Deflt,Ctx,Stk);
    valis (reconcileStack(fst(Case),Stk2,Stk3),TCde2++[iJmp(Lb)],Cde2++[.iLbl(Lb),..CCde])
  }
  compCases([(Iy,Case),..Cases],Ix,Mx,Succ,Fail,ECont,Deflt,Ctx,Stk) => valof{
    (Stk1,TCde,CCde) = compCases([(Iy,Case),..Cases],Ix+1,Mx,Succ,Fail,ECont,Deflt,Ctx,Stk);
    valis (Stk1,TCde++[iJmp(Deflt)],CCde)
  }

  compCaseBranch([(Lc,Ptn,Exp)],Succ,Fail,ECont,Deflt,Ctx,Stk) =>
    compPttrn(Ptn,expCont(Exp,Succ,ECont),Fail,ECont,Ctx,Stk).
  compCaseBranch([(Lc,Ptn,Exp),..More],Succ,Fail,ECont,Deflt,Ctx,Stk) => valof{
    Fl = defineLbl("CF",Ctx);
    VLb = defineLbl("CN",Ctx);
    Vr = genSym("__");
    (Off,Ctx1) = defineLclVar(Vr,typeOf(Ptn)::ltipe,Ctx);
    (Stk2,RlCde) = compPttrn(Ptn,expCont(Exp,Succ,ECont),jmpCont(Fl,Stk),ECont,Ctx1,Stk);
    (Stk3,AltCde) = compMoreCase(More,Off,Succ,Fail,ECont,Ctx,Stk);
    valis (reconcileStack(Lc,Stk2,Stk3),[.iTL(Off)]++RlCde++[.iLbl(Fl)]++AltCde)
  }

  compMoreCase([],_,_,Fail,ECont,Ctx,Stk) => Fail.C(Ctx,Stk,[]).
  compMoreCase([(Lc,Ptn,Exp),..More],Off,Succ,Fail,ECont,Ctx,Stk) => valof{
    Fl = defineLbl("CM",Ctx);
    (Stk2,RlCde) = compPttrn(Ptn,expCont(Exp,Succ,ECont),jmpCont(Fl,Stk),ECont,Ctx,Stk);
    (Stk3,RstCde) = compMoreCase(More,Off,Succ,Fail,ECont,Ctx,Stk);
    valis (reconcileStack(Lc,Stk2,Stk3),[.iLdL(Off)]++RlCde++[.iLbl(Fl)]++RstCde)
  }

  compCnsCase(Lc,Gv,[(Lc,Ptn,Exp)],Tp,Cont,ECont,Ctx,Stk) => valof{
    Nxt = defineLbl("CN",Ctx);
    (Stk1,GCde) = compExp(Gv,jmpCont(Nxt,Stk),ECont,Ctx,Stk);
    (Stk2,Cde) = compPtn(Ptn,expCont(Exp,Cont,ECont),jmpCont(Ctx.escape,Stk),ECont,Ctx,Stk);
    valis (reconcileStack(Lc,Stk1,Stk2),GCde++[.iLbl(Nxt)]++Cde)
  }
  compCnsCase(Lc,Gv,Cases,Tp,Cont,ECont,Ctx,Stk) => valof{
    Nxt = defineLbl("CN",Ctx);
    (Stk1,GCde) = compExp(Gv,jmpCont(Nxt,Stk),ECont,Ctx,Stk);
    (Stk2,Cde) = compCnsCases(Cases,Cont,ECont,Ctx,Stk);
    
    valis (reconcileStack(Lc,Stk1,Stk2),GCde++[.iLbl(Nxt)]++Cde)
  }

  compCnsCases:(cons[cCase[cExp]],Cont,Cont,codeCtx,stack) =>
    (stack,multi[assemOp],multi[assemOp]).
  compCnsCases([],_,_,_,Stk) => (Stk,[],[]).
  compCnsCases([(Lc,Ptn,Exp),..Cases],Succ,ECont,Ctx,Stk) => valof{
    Lb = defineLbl("CC",Ctx);
    (Stk2,TCde2,Cde2) = compCnsCases(Cases,Succ,ECont,Ctx,Stk);
    (Stk3,CCde) = compPtn(Ptn,expCont(Exp,Succ,ECont),jmpCont(Ctx.escape,Stk),ECont,Ctx,Stk);
    valis (reconcileStack(Lc,Stk2,Stk3),TCde2++[iJmp(Lb)],Cde2++[.iLbl(Lb),..CCde])
  }

  csEntry ~> (integer,cons[(option[locn],cExp,cExp)]).

  genCaseTable(Cases) where Mx.=nextPrime(size(Cases)) =>
    (sortCases(caseHashes(Cases,Mx)),Mx).

  caseHashes:all e ~~ (cons[cCase[e]],integer)=>cons[(option[locn],cExp,integer,e)].
  caseHashes(Cases,Mx) => (Cases//((Lc,Pt,Ex))=>(Lc,Pt,caseHash(Pt)%Mx,Ex)).

  caseHash:(cExp)=>integer.
  caseHash(.cVar(_,_)) => 0.
  caseHash(.cInt(_,Ix)) => Ix.
  caseHash(.cFloat(_,Dx)) => hash(Dx).
  caseHash(.cString(_,Sx)) => hash(Sx).
  caseHash(.cTerm(_,Nm,Args,_)) => size(Args)*37+hash(Nm).

  sortCases(Cases) => mergeDuplicates(sort(Cases,((_,_,H1,_),(_,_,H2,_))=>H1<H2)).

  mergeDuplicates:(cons[(option[locn],cExp,integer,cExp)])=>cons[csEntry].
  mergeDuplicates([])=>[].
  mergeDuplicates([(Lc,Pt,Hx,Ex),..M]) where (D,Rs).=mergeDuplicate(M,Hx,[]) =>
    [(Hx,[(Lc,Pt,Ex),..D]),..mergeDuplicates(Rs)].

  mergeDuplicate([(Lc,Pt,Hx,Ex),..M],Hx,SoFar) =>
    mergeDuplicate(M,Hx,SoFar++[(Lc,Pt,Ex)]).
  mergeDuplicate(M,_,SoFar) default => (SoFar,M).

  compPttrn:(cExp,Cont,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPttrn(Ptn,Succ,Fail,ECont,Ctx,Stk) => valof{
    Lc = locOf(Ptn);
    valis compPtn(Ptn,splitCont(Lc,Ctx,Succ),splitCont(Lc,Ctx,Fail),ECont,Ctx,Stk)
  }
  
  compPtn:(cExp,Cont,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtn(.cVar(Lc,.cId(Vr,Tp)),Succ,Fail,_,Ctx,[STp,..Stk]) => valof{
    if Loc ^= locateVar(Vr,Ctx) then 
      valis compPtnVar(Lc,Vr,Loc,Succ,Ctx,Stk)
    else{
      LTp = Tp::ltipe;
      (Off,Ctx1) = defineLclVar(Vr,LTp,Ctx);
      valis compPtnVar(Lc,Vr,lclVar(Off,LTp),Succ,Ctx1,Stk)
    }
  }
  compPtn(.cVoid(Lc,_),Succ,Fail,_ECont,Ctx,.some([_,..Stk])) =>
    Succ.C(Ctx,Stk,[.iDrop]).
  compPtn(.cAnon(Lc,_),Succ,Fail,_ECont,Ctx,.some([_,..Stk])) =>
    Succ.C(Ctx,Stk,[.iDrop]).
  compPtn(L,Succ,Fail,ECont,Ctx,[Tp,..Stk]) where isGround(L) => valof{
    Flb = defineLbl("Tst",Ctx);
    (Stk1,FCde) = Fail.C(Ctx,Stk,[.iLbl(Flb)]);
    (Stk2,SCde) = Succ.C(Ctx,Stk,[]);
    valis (reconcileStack(locOf(L),Stk1,Stk2),[iCmp(Flb)]++SCde++FCde)
  }
  compPtn(.cTerm(Lc,Nm,Args,Tp),Succ,Fail,ECont,Ctx,[_,..Stk]) => 
    compPtnArgs(Args,unpackCont(Lc,tLbl(Nm,size(Args)),Succ,Fail,Stk),Fail,ECont,Ctx,
      loadStack(Args//(A)=>(typeOf(A)::ltipe),Stk)).
  compPtn(.cWhere(Lc,Ptn,Cond),Succ,Fail,ECont,Ctx,Stk) =>
    compPtn(Ptn,condCont(Cond,Succ,Fail,ECont,trimStack(Stk,1)),Fail,ECont,Ctx,Stk).
  compPtn(Ptn,Succ,Fail,ECont,Ctx,Stk) => valof{
    reportError("uncompilable pattern $(Ptn)",locOf(Ptn));
    valis Succ.C(Ctx,Stk,[])
  }

  compPtnVar:(option[locn],string,srcLoc,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtnVar(Lc,Nm,.lclVar(Off,Tp),Cont,Ctx,Stk) => Cont.C(Ctx,Stk,[.iStL(Off)]).

  compPtnArgs:(cons[cExp],Cont,Cont,Cont,codeCtx,stack) => (stack,multi[assemOp]).
  compPtnArgs([],Succ,_Fail,_ECont,Ctx,Stk) => Succ.C(Ctx,Stk,[]).
  compPtnArgs([A,..As],Succ,Fail,ECont,Ctx,Stk) =>
    compPtn(A,argsPtnCont(As,Succ,Fail,ECont),Fail,ECont,Ctx,Stk).

  argsPtnCont(As,Succ,Fail,ECont) => cont{
    C(Ctx,Stk,Cde) => valof{
      (Stk1,Cde1) = compPtnArgs(As,Succ,Fail,ECont,Ctx,Stk);
      valis (Stk1,Cde++Cde1)
    }
  }

  -- continuations

  Cont ::= cont{
    C:(codeCtx,stack,multi[assemOp])=>(stack,multi[assemOp]).
  }.

  allocCont:(termLbl,ltipe,stack,Cont) => Cont.
  allocCont(Lbl,Tp,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,pushStack(Tp,Stk),Cde++[.iAlloc(Lbl),frameIns(AStk)])
  }.

  escapeCont:(string,ltipe,stack,Cont) => Cont.
  escapeCont(Es,Tp,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,pushStack(Tp,Stk),Cde++[.iEscape(Es,Ctx.escape),
	frameIns(AStk)]).
  }

  intrinsicCont:(assemOp,ltipe,stack,Cont) => Cont.
  intrinsicCont(I,Tp,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,pushStack(Tp,Stk),Cde++[I,frameIns(AStk)]).
  }

  callCont:(termLbl,ltipe,stack,Cont) => Cont.
  callCont(Lbl,Tp,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,pushStack(Tp,Stk),Cde++[.iCall(Lbl,Ctx.escape),
	frameIns(AStk)]).
  }

  oclCont:(integer,ltipe,stack,Cont) => Cont.
  oclCont(Ar,Tp,Stk,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,pushStack(Tp,Stk),Cde++[.iOCall(Ar,Ctx.escape),
	frameIns(AStk)]).
  }

  retCont:Cont.
  retCont = cont{
    C(_,_,Cde) => (.none,Cde++[.iRet])
  }

  glbRetCont:(string)=>Cont.
  glbRetCont(Nm) => cont{
    C(_,_,Cde) => (.none,Cde++[.iTG(Nm),.iRet])
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
      (SStk,SCde) = Succ.C(Ctx,Stk,[.iCmp(Flb)]);
      (FStk,FCde) = Fail.C(Ctx,Stk,[.iLbl(Flb)]);
      
      valis (reconcileStack(Lc,SStk,FStk),Cde++SCde++FCde)
    }
  }

  unpackCont:(option[locn],termLbl,Cont,Cont,stack) => Cont.
  unpackCont(Lc,Lbl,Succ,Fail,Stk0) => cont{
    C(Ctx,Stk,Cde) => valof{
      Flb = defineLbl("U",Ctx);
      (Stk1,FCde) = Fail.C(Ctx,Stk0,[.iLbl(Flb)]);
      (Stk2,SCde) = Succ.C(Ctx,Stk,[]);
      valis (reconcileStack(Lc,Stk1,Stk2),[iUnpack(Lbl,Flb)]++SCde++FCde)
    }
  }

  suspendCont:(stack,ltipe,Cont) => Cont.
  suspendCont(Stk,Tp,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,pushStack(Tp,Stk),Cde++[.iSuspend,frameIns(AStk)]).
  }

  resumeCont:(stack,ltipe,Cont) => Cont.
  resumeCont(Stk,Tp,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,pushStack(Tp,Stk),Cde++[.iResume,frameIns(AStk)]).
  }

  retireCont:(stack,ltipe,Cont) => Cont.
  retireCont(Stk,Tp,Cont) => cont{
    C(Ctx,AStk,Cde) => Cont.C(Ctx,pushStack(Tp,Stk),Cde++[.iRetire,frameIns(AStk)]).
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
      valis (Stk2,SSCde++CCde)
    }
  }

  ptnCont:(cExp,Cont,Cont,Cont) => Cont.
  ptnCont(Ptn,Succ,Fail,ECont) => cont{
    C(Ctx,Stk,Cde) => valof{
      (Stk1,Cde1) = compPttrn(Ptn,Succ,Fail,ECont,Ctx,Stk);
      valis (Stk1,Cde++Cde1)
    }
  }

  nthCont:(integer,tipe,Cont,stack)=>Cont.
  nthCont(Ix,Tp,Cont,Stk) => cont{
    C(Ctx,SS,Cde) => valof{
      (_,CCde) = Cont.C(Ctx,SS,Cde);
      valis (pushStack(Tp::ltipe,Stk),CCde++[.iNth(Ix)])
    }
  }
      
  setCont:(integer,tipe,Cont,stack)=>Cont.
  setCont(Ix,Tp,Cont,Stk) => cont{
    C(Ctx,SS,Cde) => valof{
      (_,CCde) = Cont.C(Ctx,SS,Cde);
      valis (pushStack(Tp::ltipe,Stk),CCde++[.iStNth(Ix)])
    }
  }

  abortCont:(option[locn],string) => Cont.
  abortCont(.some(Lc),Msg) => cont{
    C(_,_,Cde) => (.none,Cde++[.iLdC(Lc::data),.iLdC(strg(Msg)),.iAbort]).
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
	  tStk := reconcileStack(Lc,Stk,tStk!);
	  valis (tStk!,Cde++[.iJmp(Lb)])
	} else{
	  triggered := .true;
	  tStk := Stk;
	  valis Cnt.C(Ctx,Stk,Cde++[.iLbl(Lb)])
	}
      }
    }
  }

  reconcileStack:(option[locn],stack,stack)=>stack.
  reconcileStack(_Lc,.none,S) => S.
  reconcileStack(_Lc,S,.none) => S.
  reconcileStack(Lc,.some(S1),.some(S2)) => valof{
    if S1==S2 then
      valis .some(S1)
    else{
      reportError("misaligned stacks $(S1) vs $(S2)",Lc);
      valis .some(S1)
    }
  }

  resetStack:(integer,stack) => (stack,multi[assemOp]).
  resetStack(Dp,.some(Stk)) =>
    case [|Stk|] in {
      Dp => (.some(Stk),[]).
      Dp1 where Dp1 == Dp-1 => (tail(Stk),[.iDrop]).
      Dp1 where Dp1<Dp-1 => (.some(popTo(Stk,Dp)),[.iRst(Dp)]).
      Dp1 default => valof{	
	reportTrap("invalid stack height in $(Stk)");
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
    
    Off = hwm!;
    hwm := Off+1;

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

  srcLoc ::= lclVar(integer,ltipe) |
    argVar(integer,ltipe) |
    glbVar(string,ltipe) |
    glbFun(termLbl,ltipe).

  codeCtx ::= codeCtx{
    vars:map[string,srcLoc].
    end:assemLbl.
    escape:assemLbl.
    lbls : ref integer.  
    min:integer.
    hwm:ref integer}

  stack ~> option[cons[ltipe]].

  emptyCtx:(map[string,srcLoc])=>codeCtx.
  emptyCtx(Glbs) => codeCtx{
    vars = Glbs.
    end = .al("$$").
    escape = .al("$$").
    lbls = ref 0.
    min = 0.
    hwm = ref 0.
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
  pushStack(Tp,.some(Stk)) => .some([Tp,..Stk]).

  loadStack:(cons[ltipe],stack) => stack.
  loadStack(Tps,.some(Stk)) => .some(Tps++Stk).

  popTo:all e ~~ (cons[e],integer) => cons[e].
  popTo(LL,D) => let{.
    pop([],_) => [].
    pop(L,0) => L.
    pop([_,..L],Cx) where Cx>0 => pop(L,Cx-1).
  .} in pop(LL,[|LL|]-D).

  implementation display[codeCtx] => {
    disp(C) => "<C hwm:$(C.hwm!)>C>".
  }

  implementation display[srcLoc] => {
    disp(.lclVar(Off,Tpe)) => "lcl $(Off)\:$(Tpe)".
    disp(.argVar(Off,Tpe)) => "arg $(Off)\:$(Tpe)".
    disp(.glbVar(Off,Tpe)) => "glb $(Off)\:$(Tpe)".
    disp(.glbFun(Off,Tpe)) => "fun $(Off)\:$(Tpe)".
  }
}
