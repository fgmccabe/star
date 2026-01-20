:- module(genssa,[genSSA/4]).

:- use_module(meta).
:- use_module(misc).
:- use_module(types).
:- use_module(lterms).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).
:- use_module(ssa).
:- use_module(errors).
:- use_module(gensig).
:- use_module(location).
:- use_module(peephole).
:- use_module(intrinsics).
:- use_module(escapes).

genSSA(PkgDecls,mdule(Pkg,Imports,Decls,LDecls,Defs),Opts,Text) :-
  encPkg(Pkg,PT),
  initDict(D0,Opts),
  genImports(Imports,ImpTpl),
  rfold(PkgDecls,gencode:decl(Opts),D0,D2),
  genDefs(Defs,Opts,D2,C,[]),
  mkTpl(C,Cdes),
  map(Decls,gensig:formatDecl,Ds),
  mkTpl(Ds,DTpl),
  map(LDecls,gensig:formatDecl,LDs),
  mkTpl(LDs,LDTpl),
  ssaHash(OpSig),
  mkTpl([PT,intgr(OpSig),ImpTpl,DTpl,LDTpl,Cdes],Tp),
  encode(Tp,Txt),
  encode(strg(Txt),Text).

traceTrm(Cl,Trm,Out) :- call(Cl,Trm,Out), dispTerm(Out).

genImports(Imps,ImpTpl) :-
  map(Imps,gencode:genImport,Els),
  mkTpl(Els,ImpTpl).

genImport(importPk(_,_,Pkg),PkgTrm) :-
  encPkg(Pkg,PkgTrm).

decl(Opts,varDec(_,Nm,Tp),D,Dx) :-!,
  defineGlbVar(Nm,Tp,Opts,D,Dx).
decl(_,typeDec(TpNm,Tp,Rl,IxMap),D,Dx) :-
  tpName(Tp,Nm),
  defineType(Nm,typeDec(TpNm,Tp,Rl,IxMap),D,Dx).
decl(_,_,D,D).

genDefs(Defs,Opts,D,O,Ox) :-
  rfold(Defs,gencode:genDef(D,Opts),Ox,O).

genDef(D,Opts,Def,O,Ox) :-
  (is_member(traceGenCode,Opts) -> dispRuleSet(Def) ; true),
  genDf(D,Opts,Def,O,Ox).

genDf(D,Opts,fnDef(Lc,Nm,H,Tp,Args,Value),O,[Cde|O]) :-
  genFun(Lc,Nm,H,Tp,Args,Value,D,Opts,Cde).
genDf(D,Opts,prDef(Lc,Nm,Tp,Args,Act),O,[Cde|O]) :-
  genPrc(Lc,Nm,Tp,Args,Act,D,Opts,Cde).
genDf(D,Opts,glbDef(Lc,Nm,Tp,Value),O,[Cde|O]) :-
  genGlb(Lc,Nm,Tp,Value,D,Opts,Cde).
genDf(_,_,lblDef(_,Lbl,Tp,Ix),O,[LblTrm|O]) :-
  encType(Tp,Sig),
  assemSSA(struct(Lbl,strg(Sig),Ix),LblTrm).
genDf(_,_,typDef(_,Tp,Rl,IxMap),O,[TpTrm|O]) :-
  assemSSA(tipe(Tp,Rl,IxMap),TpTrm).

genFun(Lc,Nm,H,Tp,Args,Value,D,Opts,CdTrm) :-
  isThrowingType(Tp,RsTp,ErTp),!,
  toLtipe(Tp,LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L0),
  genLbl(L0,Lx,L1),
  genLbl(L1,Er,L2),
  defineTmpVar(Lc,AbrtVr,ErTp,Opts,D,D0),
  defineTmpVar(Lc,RsltVr,RsTp,Opts,D0,D1),

  genLine(Opts,Lc,C,[iLbl(Abrt,iBlock([iLbl(Lx,iValof(RsltVr,
						      [iLbl(Er,iValof(AbrtVr,FC)),
							iXRet(AbrtVr)])),iRet(RsltVr)]))|CA]),
  BaseBrks = [("$try",gencode:result,Er,AbrtVr)],
  compArgs(Args,Lc,0,Abrt,BaseBrks,Opts,L2,L3,D1,D2,FC,FC0),
  compExp(Value,Lc,[("$abort",gencode:breakOut,Abrt,none)|BaseBrks],notLast,
	  Opts,L3,L4,D2,D3,FC0,FC1,Stk0,Stk1),
  genRet(Opts,Lc,FC1,[],Stk1,_),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[iLdC(intgr(10)),iHalt],Stk0,_),
  getLsMap(Dx,LsMap),
  getAgMap(Args,AgMap),
  length(LsMap,LclCnt),
  GenFunc = func(Nm,H,Sig,AgMap,LsMap,[iEntry(LclCnt)|C]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenFunc);true ),
  peepOptimize(GenFunc,PFunc),
  (is_member(showGenCode,Opts) -> dispSSA(PFunc);true ),
  assemSSA(PFunc,CdTrm).
genFun(Lc,Nm,H,Tp,Args,Value,D,Opts,CdTrm) :-
  toLtipe(Tp,LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L0),
  genLbl(L0,Lx,L1),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(0,
				       [iLbl(Lx,iValof(1,FC)),iRet]))|CA]),
  compArgs(Args,Lc,0,Abrt,[],Opts,L1,L3,D,D1,FC,FC0,some(0),Stk0),
  compExp(Value,Lc,[("$abort",gencode:breakOut,Abrt,none)],last,
	  Opts,L3,L4,D1,D3,FC0,FC1,Stk0,Stk1),
  genRet(Opts,Lc,FC1,[],Stk1,_),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[iLdC(intgr(10)),iHalt],Stk0,_),
  getLsMap(Dx,LsMap),
  getAgMap(Args,AgMap),
  length(LsMap,LclCnt),
  GenFunc = func(Nm,H,Sig,AgMap,LsMap,[iEntry(LclCnt)|C0]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenFunc);true ),
  peepOptimize(GenFunc,PFunc),
  (is_member(showGenCode,Opts) -> dispSSA(PFunc);true ),
  assemSSA(PFunc,CdTrm).

genPrc(Lc,Nm,Tp,Args,Act,D,Opts,CdTrm) :-
  isThrowingType(Tp,_RsTp,ErTp),!,
  toLtipe(Tp,LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L0),
  genLbl(L0,Lx,L1),
  genLbl(L1,Er,L2),
  defineTmpVar(Lc,AbrtVr,ErTp,Opts,D,D0),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(0,
				       [iLbl(Lx,iBlock(0,
						       [iLbl(Er,iValof(1,FC)),
							iXRet])),iLdV,iRet]))|CA]),
  BaseBrks = [("$try",gencode:result,Er,AbrtVr)],
  compArgs(Args,Lc,0,Abrt,BaseBrks,Opts,L2,L3,D,D1,FC,FC0,some(0),Stk0),
  compAction(Act,Lc,[("$abort",gencode:breakOut,Abrt,none)|BaseBrks],
	     notLast,notLast,Opts,L3,L4,D1,D3,FC0,[iBreak(Lx)],Stk0,_),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[iLdC(intgr(10)),iHalt],Stk0,_),
  getLsMap(Dx,LsMap),
  getAgMap(Args,AgMap),
  length(LsMap,LclCnt),
  GenFunc = func(Nm,hard,Sig,AgMap,LsMap,[iEntry(LclCnt)|C0]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenFunc);true ),
  peepOptimize(GenFunc,PFunc),
  (is_member(showGenCode,Opts) -> dispSSA(PFunc);true ),
  assemSSA(PFunc,CdTrm).
genPrc(Lc,Nm,Tp,Args,Act,D,Opts,CdTrm) :-
  toLtipe(Tp,LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L0),
  genLbl(L0,Lx,L1),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(0,
				       [iLbl(Lx,iBlock(0,FC)),iLdV,iRet]))|CA]),
  compArgs(Args,Lc,0,Abrt,[],Opts,L1,L3,D,D1,FC,FC0,some(0),Stk0),
  compAction(Act,Lc,[("$abort",gencode:breakOut,Abrt,none)],
	     notLast,notLast,Opts,L3,L4,D1,D3,FC0,[iBreak(Lx)],Stk0,_),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[iLdC(intgr(10)),iHalt],Stk0,_),
  getLsMap(Dx,LsMap),
  getAgMap(Args,AgMap),
  length(LsMap,LclCnt),
  GenFunc = func(Nm,hard,Sig,AgMap,LsMap,[iEntry(LclCnt)|C0]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenFunc);true ),
  peepOptimize(GenFunc,PFunc),
  (is_member(showGenCode,Opts) -> dispSSA(PFunc);true ),
  assemSSA(PFunc,CdTrm).

genGlb(Lc,Nm,Tp,Value,D,Opts,Cd) :-
  toLtipe(funType(tplType([]),Tp,voidType),LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L0),
  genLbl(L0,_Lx,L1),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(0,FC))|CA]),
  compExp(Value,Lc,[],notLast,Opts,L1,L2,D,D1,FC,[iTG(Nm),iRet|FC1],some(0),Stk0),
  compAbort(Lc,strg("def failed"),[],Opts,L2,_L3,D1,Dx,CA,[iLdC(intgr(10)),iHalt],some(0),_),
  genRet(Opts,Lc,FC1,[],Stk0,_),
  getLsMap(Dx,LsMap),
  length(LsMap,LclCnt),
  GenGlb = func(lbl(Nm,0),hard,Sig,[],LsMap,[iEntry(LclCnt)|C0]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenGlb);true ),
  peepOptimize(GenGlb,PGlb),
  (is_member(showGenCode,Opts) -> dispSSA(PGlb);true ),
  assemSSA(PGlb,Cd).

initDict(D,Opts) :-
  stdDecl(Decls),
  rfold(Decls,gencode:decl(Opts),scope([],tps{}),D).

getLsMap(scope(Dx,_),Vrs) :-
  extractLMap(Dx,Vrs),!.

extractLMap([],[]).
extractLMap([(Nm,Spec,l(_))|Dx],[(Nm,Spec)|LsMap]) :-
  extractLMap(Dx,LsMap).
extractLMap([_|Dx],LsMap) :-
  extractLMap(Dx,LsMap).

getAgMap(Args,AgMap) :-
  extractAMap(Args,AgMap),!.

extractAMap([],[]).
extractAMap([idnt(Nm,Tp)|As],[(Nm,strg(Sig))|LsMap]) :-
  toLtipe(Tp,T),
  encLtp(T,Sig),
  extractAMap(As,LsMap).
extractAMap([_|Dx],LsMap) :-
  extractAMap(Dx,LsMap).

buildArgs([A|R],Ix,Lc,Opts,D,Dx) :-
  buildArg(A,Ix,Lc,Opts,D,D0),
  Ix1 is Ix+1,
  buildArgs(R,Ix1,Lc,Opts,D0,Dx).

buildArg(idnt(Nm,Tp),_Ix,Lc,Opts,D,Dx) :-!,
  defineVar(Lc,Nm,Tp,Opts,a(Nm),D,Dx).
buildArg(_,_,_,_,D,D).

lclVar(Nm,T,Wh,scope(Vrs,_)) :-
  is_member((Nm,T,Wh),Vrs),!.

defineLclVar(Lc,Nm,Tp,Opts,D,Dx) :-
  defineVar(Lc,Nm,Tp,Opts,l(Nm),D,Dx).

defineVar(_Lc,Nm,Tp,_Opts,Mode,scope(Vrs,Tps),scope(NVrs,Tps)) :-
  toLtipe(Tp,T),
  encLtp(T,Sig),
  add_mem((Nm,strg(Sig),Mode),Vrs,NVrs).

defineTmpVar(Lc,TmpNm,Tp,Opts,D,Dx) :-
  genTmpVar(D,TmpNm),
  defineLclVar(Lc,TmpNm,Tp,Opts,D,Dx).

defineGlbVar(Nm,Tp,_Opts,scope(Vrs,Tps),scope([(Nm,strg(Sig),g(Nm))|Vrs],Tps)) :-
  toLtipe(Tp,T),
  encLtp(T,Sig).

defineType(Nm,TpDec,scope(Vrs,Tps),scope(Vrs,NTps)) :-
  makeKey(Nm,Key),
  put_dict(Key,Tps,TpDec,NTps).

getTypeIndex(Tp,scope(_,Tps),Index) :-
  tpName(Tp,Nm),
  makeKey(Nm,Key),
  get_dict(Key,Tps,typeDec(_,_,_,Index)).

genLbl(Lbs,Lb,[Lb|Lbs]) :-
  length(Lbs,N),
  swritef(Lb,"_L%d",[N]).

genTmpVar(scope(Vrs,_),Nm) :-
  length(Vrs,Mx),
  swritef(Nm,"_Φ%d",[Mx]).

compExpCase(Term,Lc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compExp(Term,Lc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

compIdExp(idnt(Nm,_),Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-
  lclVar(Nm,Tp,V,Dx),!,
  compVar(V,Tp,Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx).
compIdExp(idnt(Nm,_),Lc,_Brks,_Next,_Opts,Lx,Lx,Dx,Dx,Cx,Cx) :-
  reportError("cannot locate variable %s",[id(Nm)],Lc),
  abort.

compVar(l(X),_,Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :- !,
  call(Next,Lc,X,Brks,Opts,L,Lx,D,Dx,C,Cx).
compVar(g(GlbNm),Tp,Lc,Brks,Next,Opts,L,Lx,D,Dx,[iMvG(V,GlbNm)|C],Cx) :-
  defineTmpVar(Lc,V,Tp,Opts,D,D1),
  call(Next,Lc,V,Brks,Opts,L,Lx,D1,Dx,C,Cx).

/* Compile actions */
compAction(nop(_),_Lc,_Brks,_Return,_Next,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stkx,Stkx) :-!.
compAction(seq(Lc,A,B),OLc,Brks,Return,Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compAction(A,Lc,Brks,notLast,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk0),
  verify(gencode:consistentStack(Stk,Stk0),"actions not permitted to leave stuff on stack"),
  compAction(B,Lc,Brks,Return,Next,Opts,L1,Lx,D1,Dx,C1,Cx,Stk,Stkx).
compAction(lbld(Lc,Lb,A),OLc,Brks,Return,_Next,Opts,L,Lx,D,Dx,
	   [iLbl(BrkLb,iBlock(Lvl,BC))|Cx],Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,BC,C0),
  genLbl(L,BrkLb,L1),
  stkLvl(Stk,Lvl),
  compAction(A,Lc,[(Lb,gencode:breakOut,BrkLb,Stk)|Brks],Return,notLast,Opts,L1,Lx,D,Dx,C0,[iBreak(BrkLb)],Stk,Stk0),
  verify(gencode:consistentStack(Stk,Stk0),"labeled actions not permitted to return values").
compAction(brk(Lc,Nm),OLc,Brks,_Return,_Next,Opts,Lx,Lx,Dx,Dx,C,Cx,Stk,none) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  (is_member((Nm,Brkr,Lbl,Stkx),Brks) ->
   call(Brkr,Lbl,Stk,Stkx,C0,Cx);
   reportError("not in scope of break label %s",[ss(Nm)],Lc),
   C0=Cx).
compAction(aThrow(Lc,E),OLc,Brks,_Return,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-
  (is_member(("$try",Brker,Ok,AbrtVr),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   compExp(E,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C0,C1x,Stk,Stka),
   call(Brker,Ok,Stka,Stkx,C1x,Cx);
   reportError("not in scope of try",[],Lc)).
compAction(perf(Lc,Cll),OLc,Brks,_,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(Cll,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C0,C1,Stk,Stk0),
  resetStack(Stk,Stk0,C1,Cx).
compAction(perf(Lc,Cll),OLc,Brks,Last,last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(Cll,Lc,Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(mtch(Lc,P,E),OLc,Brks,_Return,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  stkLvl(Stk,Lvl),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(Lvl,[iLbl(Abrt,iBlock(Lvl,CB))|Ca]))|Cx]),
  genLbl(L,Ok,L1),
  genLbl(L1,Abrt,L2),
  compExp(E,Lc,Brks,notLast,Opts,L2,L3,D,D1,CB,C1,Stk,Stk1),
  compPtn(P,Lc,Abrt,Brks,Opts,L3,L4,D1,D2,C1,C2,Stk1,Stka),
  resetStack(Stk,Stka,C2,[iBreak(Ok)]),
  compAbort(Lc,strg("match fail"),Brks,Opts,L4,Lx,D2,Dx,Ca,[],Stk,_).
compAction(defn(Lc,idnt(Nm,Tp),E),OLc,Brks,_Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,Brks,notLast,Opts,L,Lx,D,D1,C0,[iSt(Nm)|C1],Stk,Stk0),
  genBind(Opts,Nm,C1,Cx),
  defineLclVar(Lc,Nm,Tp,Opts,D1,Dx),
  dropStk(Stk0,1,Stk).
compAction(setix(Lc,Trm,Off,Vl),OLc,Brks,_Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Vl,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(Trm,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iStNth(Off)|Cx],Stka,Stkb),
  dropStk(Stkb,2,Stk).
compAction(asgn(Lc,Cll,Exp),OLc,Brks,_Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(Cll,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,C2,Stka,Stkb),
  genDbg(Opts,Lc,C2,[iAssign|Cx]),
  dropStk(Stkb,2,Stk).
compAction(case(Lc,T,Cases,Deflt),OLc,Brks,Last,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  stkLvl(Stk,Lvl),
  compCase(T,Lc,gencode:wrapAction(Lvl),Cases,Deflt,gencode:compAct(notLast),Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx),
  verify(gencode:consistentStack(Stk,Stkx),"case action not permitted to leave stuff on stack").
compAction(case(Lc,T,Cases,Deflt),OLc,Brks,Last,last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  stkNxtLvl(Stk,Lvl),
  compCase(T,Lc,gencode:wrapExpr(Lvl),Cases,Deflt,gencode:compAct(last),Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(unpack(Lc,T,Cases,Deflt),OLc,Brks,Last,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  stkLvl(Stk,Lvl),
  compUnpack(T,Lc,gencode:wrapAction(Lvl),gencode:break,Cases,Deflt,gencode:compAct(notLast),Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx),
  verify(gencode:consistentStack(Stk,Stkx),"case action not permitted to leave stuff on stack").
compAction(unpack(Lc,T,Cases,Deflt),OLc,Brks,Last,last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  stkNxtLvl(Stk,Lvl),
  compUnpack(T,Lc,gencode:wrapExpr(Lvl),gencode:result,Cases,Deflt,gencode:compAct(last),Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(whle(Lc,G,B),OLc,Brks,_Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  stkLvl(Stk,Lvl),
  chLine(Opts,OLc,Lc,C,[iLbl(Done,iBlock(Lvl,[iLbl(Lp,iBlock(Lvl,LC))]))|Cx]),!,
  genLbl(L,Lp,L1),
  genLbl(L1,Done,L2),
  compCond(G,Lc,Done,Brks,normal,Opts,L2,L3,D,D1,LC,LC1,Stk,Stka),
  verify(gencode:consistentStack(Stk,Stka),"condition not permitted to leave stuff on stack"),
  compAction(B,Lc,Brks,notLast,notLast,Opts,L3,Lx,D1,Dx,LC1,[iLoop(Lp)],Stk,Stkb),
  verify(gencode:consistentStack(Stk,Stkb),"while action not permitted to leave stuff on stack").
compAction(ltt(Lc,idnt(Nm,Tp),Val,Act),OLc,Brks,Last,Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C1),!,
  defineLclVar(Lc,Nm,Tp,Opts,D,D1),
  compExp(Val,Lc,Brks,notLast,Opts,L,L1,D1,D2,C1,[iSt(Nm)|C2],Stk,Stk1),
  verify(gencode:bumpStk(Stk,Stk1),"expecting a single bump in stack"),
  compAction(Act,Lc,Brks,Last,Next,Opts,L1,Lx,D2,Dx,C2,Cx,Stk,Stkx).
compAction(iftte(Lc,G,A,B),OLc,Brks,Last,Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  stkLvl(Stk,Lvl),
  chLine(Opts,OLc,Lc,C,[iLbl(Thn,iBlock(Lvl,[iLbl(Fl,iBlock(Lvl,AC))|BC]))|Cx]),
  genLbl(L,Fl,L0),
  genLbl(L0,Thn,L1),
  compCond(G,Lc,Fl,Brks,normal,Opts,L1,L2,D,D1,AC,AC1,Stk,Stk0),
  verify(gencode:consistentStack(Stk,Stk0),"conditions should not increase stack"),
  compAction(A,Lc,Brks,Last,Next,Opts,L2,L3,D1,D2,AC1,[iBreak(Thn)],Stk,Stka),
  compAction(B,Lc,Brks,Last,Next,Opts,L3,Lx,D2,Dx,BC,[iBreak(Thn)],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"conditional action").
compAction(aTry(Lc,B,E,H),OLc,Brks,Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  compTryA(Lc,B,E,H,OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stka),
  verify(gencode:consistentStack(Stk,Stka),"try action not permitted to leave stuff on stack").
compAction(vls(Lc,E),OLc,Brks,Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  (is_member(("$valof",Brker,Ok,Stkx),Brks) ->
     chLine(Opts,OLc,Lc,C,C0),
     compExp(E,Lc,Brks,Last,Opts,L,Lx,D,Dx,C0,C1x,Stk,Stka),
     call(Brker,Ok,Stka,Stkx,C1x,Cx);
   reportError("not in scope of valof",[],Lc)).
compAction(error(Lc,Msg),_OLc,Brks,_Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,_).
compAction(A,Lc,_Brks,_Last,_Next,_Opts,Lx,Lx,Dx,Dx,C,C,Stk,Stk) :-
  reportError("cannot compile action %s",[lact(A)],Lc),
  abort.

compAct(Next,A,Lc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compAction(A,Lc,Brks,Last,Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

breakOut(Ok,Stk,Stkx,C,Cx) :-
  resetStack(Stkx,Stk,C,[iBreak(Ok)|Cx]).
result(Lbl,_Stk,_Stkx,[iResult(Lbl)|Cx],Cx).
  
frameIns(some(Depth),[iFrame(Depth)|Cx],Cx).
frameIns(none,Cx,Cx).

stkLvl(some(Lvl),Lvl).

stkNxtLvl(some(Lvl),L) :-
  L is Lvl+1.

resetStack(Stk,Stk,C,C) :-!.
resetStack(none,_Stk,C,C) :- !.
resetStack(Stk,Stk0,[iDrop|C],C) :-
  dropStk(Stk0,1,Stk),!.
resetStack(some(Lvl),_,[iRst(Lvl)|Cx],Cx).

reconcileStack(Stk,none,Stk,C,C) :-!.
reconcileStack(none,Stk,Stk,C,C) :-!.
reconcileStack(Stk,Stk,Stk,C,C) :-!.
reconcileStack(some(Stki),some(Stk),some(Stk),[iRst(Stk)|C],C) :-
  Stki>Stk,!.

consistentStack(_Stk,none) :-!.
consistentStack(none,_Stk) :-!.
consistentStack(Stk,Stk).

compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  locTerm(Lc,LT),
  defineTmpVar(Lc,MVr,Tp,Opts,D,D1),
  compExp(Msg,Lc,Brks,genssa:next(MVr),Opts,L,Lx,D1,Dx,C,[iAbort(LT,MVr)|Cx]).

varGetter(Vr,Lc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compIdExp(Vr,Lc,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

nthGetter(Vr,Off,Lc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compIdExp(Vr,Lc,notLast,Opts,L,Lx,D,Dx,C,[iNth(Off)|Cx],Stk,Stkx).

already(_Lc,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stk,Stk).

chLine(_,Lc,Lc,C,C) :- !.
chLine(Opts,_,Lc,C,Cx) :-
  genLine(Opts,Lc,C,Cx).

genLine(Opts,Lc,[iLine(Lt)|Cx],Cx) :-
  is_member(debugging,Opts),!,
  locTerm(Lc,Lt).
genLine(_,_,Cx,Cx).

genDbg(Opts,Lc,[iDBug(Loc)|Cx],Cx) :-
  is_member(debugging,Opts),!,
  locTerm(Lc,Loc).
genDbg(_,_,Cx,Cx).

genBind(Opts,Vr,[iBind(strg(Vr),Vr)|Cx],Cx) :-
  is_member(debugging,Opts),!.
genBind(_,_,Cx,Cx).

bumpStk(some(Stk),some(Stk1)) :- Stk1 is Stk+1.
dropStk(some(Stk),Cnt,some(Stk1)) :- !, Stk1 is Stk-Cnt.
dropStk(none,_,none).

sameStk(none,none) :-!.
sameStk(some(S),some(S)).

mergeStkLvl(none,Stk,Stk,_) :-!.
mergeStkLvl(Stk,none,Stk,_) :-!.
mergeStkLvl(Stk1,Stk2,Stk1,Msg) :-
  verify(gencode:sameStk(Stk1,Stk2),Msg).

% Compile arguments to function
compArgs([],_Lc,_Ix,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stkx,Stkx) :-!.
compArgs([idnt(Nm,Tp)|Args],Lc,Ix,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  defineVar(Lc,Nm,Tp,Opts,a(Nm),D,D1),
  Ix1 is Ix+1,
  compArgs(Args,Lc,Ix1,Fail,Brks,Opts,L,Lx,D1,Dx,C,Cx,Stk,Stkx).

% compile a pattern. Patterns form a reduced subset of possible expression forms
compPtn(voyd,_,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iDrop|Cx],Cx,Stk,Stkx) :-!,
  dropStk(Stk,1,Stkx).
compPtn(ann(_),_,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iDrop|Cx],Cx,Stk,Stkx) :-!,
  dropStk(Stk,1,Stkx).
compPtn(intgr(Ix),_,Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iCInt(intgr(Ix),Fail)|Cx],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stkx).
compPtn(Lit,_Lc,Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iCLit(Lit,Fail)|Cx],Cx,Stk,Stkx) :-
  isLiteral(Lit),!,
  dropStk(Stk,1,Stkx).
compPtn(idnt(Nm,Tp),Lc,_Fail,_Brks,Opts,Lx,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  defineLclVar(Lc,Nm,Tp,Opts,D,Dx),
%  C,[iSt(Nm)|C1]),
  genBind(Opts,Nm,C1,Cx),
  dropStk(Stk,1,Stkx).
compPtn(ctpl(St,Args),Lc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  tipeOf(ctpl(St,Args),Tp),
  defineTmpVar(Lc,TmpNm,Tp,Opts,D,D1),
  C = [iTee(TmpNm),iCLbl(St,Fail)|C1],
  dropStk(Stk,1,Stk0),
  compPtnArgs(Args,Lc,idnt(TmpNm,Tp),0,Fail,Brks,notLast,Opts,L,Lx,D1,Dx,C1,Cx,Stk0,Stkx).
compPtn(savGet(Lc,V,_),_,Fail,Brks,Opts,L,Lx,D,Dx,[iLdSav(Fail)|C],Cx,Stk,Stkx) :-
  compPtn(V,Lc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compPtn(whr(Lc,P,Cnd),OLc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compPtn(P,Lc,Fail,Brks,Opts,L,L1,D,D1,C0,C1,Stk,Stkx),
  compCond(Cnd,Lc,Fail,Brks,normal,Opts,L1,Lx,D1,Dx,C1,Cx,Stkx,Stky),
  verify((Stkx=Stky),"where condition does not leave stack alone").
compPtn(T,Lc,Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iBreak(Fail)|Cx],Cx,Stk,Stk) :-
  reportError("(internal) cannot compile pattern %s",[ltrm(T)],Lc).

% compile argument patterns. If fail, then break out
compPtnArgs([],Lc,_PVar,_Ix,_Fail,_Brks,Last,Opts,Lx,Lx,Dx,Dx,C,Cx,Stk,Stkx) :-!,
  genLastReturn(Last,Opts,Lc,C,Cx,Stk,Stkx).
compPtnArgs([A|R],Lc,PVar,Ix,Fail,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compIdExp(PVar,Lc,notLast,Opts,L,L1,D,D1,C,[iNth(Ix)|C0],Stk,Stk0),
  compPtn(A,Lc,Fail,Brks,Opts,L1,L2,D1,D2,C0,C1,Stk0,Stk1),
  Ix1 is Ix+1,
  verify(Stk=Stk1,"arg pattern should not affect stack"),
  compPtnArgs(R,Lc,PVar,Ix1,Fail,Brks,Last,Opts,L2,Lx,D2,Dx,C1,Cx,Stk,Stkx).

% compile a condition. Invoke passed in Fail label if the condition is false
compCond(enum(Sy),_Lc,_Fail,_Brks,normal,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stkx,Stkx) :-
  isTrueSymb(Sy),!.
compCond(enum(Sy),_Lc,Fail,_Brks,negated,_Opts,Lx,Lx,Dx,Dx,[iBreak(Fail)|Cx],Cx,Stkx,Stkx) :-
  isTrueSymb(Sy),!.
compCond(enum(Sy),_,Fail,_Brks,normal,_Opts,Lx,Lx,Dx,Dx,[iBreak(Fail)|Cx],Cx,Stkx,Stkx) :-
  isFalseSymb(Sy),!.
compCond(enum(Sy),_,_Fail,_Brks,negated,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stkx,Stkx) :-
  isFalseSymb(Sy),!.
compCond(cnj(Lc,A,B),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compCond(A,Lc,Fail,Brks,normal,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  compCond(B,Lc,Fail,Brks,normal,Opts,L1,Lx,D1,Dx,C1,Cx,Stk1,Stkx).
compCond(cnj(Lc,A,B),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !, % apply de morgan's law
  compCond(dsj(Lc,ng(Lc,A),ng(Lc,B)),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compCond(dsj(Lc,A,B),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  stkLvl(Stk,Lvl),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(Lvl,[iLbl(Fl,iBlock(Lvl,AC))|BC]))|Cx]),
  genLbl(L,Fl,L0),
  genLbl(L0,Ok,L1),
  compCond(A,Lc,Fl,Brks,normal,Opts,L1,L2,D,D0,AC,[iBreak(Ok)],Stk,Stka),
  compCond(B,Lc,Fail,Brks,normal,Opts,L2,Lx,D0,Dx,BC,[iBreak(Ok)],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"disjunction").
compCond(dsj(Lc,A,B),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !, % apply de morgan's law
  compCond(cnj(Lc,ng(Lc,A),ng(Lc,B)),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compCond(ng(Lc,A),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compCond(A,Lc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compCond(ng(Lc,A),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compCond(A,Lc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compCond(cnd(Lc,T,A,B,_),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  stkLvl(Stk,Lvl),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(Lvl,[iLbl(El,iBlock(Lvl,CA))|CB]))|Cx]),
  genLbl(L,El,L0),
  compCond(T,Lc,El,Brks,normal,Opts,L0,L1,D,D1,CA,C0,Stk,Stk0),
  genLbl(L1,Ok,L2),
  compCond(A,Lc,Fail,Brks,normal,Opts,L2,L3,D1,D2,C0,[iBreak(Ok)],Stk0,Stka),
  compCond(B,Lc,Fail,Brks,normal,Opts,L3,Lx,D2,Dx,CB,[iBreak(Ok)],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"conditional").
compCond(cnd(Lc,T,A,B,Tp),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compCond(cnd(Lc,T,B,A,Tp),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compCond(mtch(Lc,P,E),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,Brks,notLast,Opts,L,L3,D,D1,C0,C1,Stk,Stka),
  compPtn(P,Lc,Fail,Brks,Opts,L3,Lx,D1,Dx,C1,Cx,Stka,Stkb),
  verify(Stkb=Stk,"match condition should not affect stack").
compCond(mtch(Lc,P,E),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  stkLvl(Stk,Lvl),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(Lvl,CB))|Cx]),
  genLbl(L,Ok,L1),
  compExp(E,Lc,Brks,notLast,Opts,L1,L3,D,D1,CB,C1,Stk,Stka),
  compPtn(P,Lc,Ok,Brks,Opts,L3,Lx,D1,Dx,C1,[iBreak(Fail)],Stka,Stkb),
  verify(Stkb=Stk,"match condition should not affect stack").
compCond(E,Lc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compExp(E,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C,[iIfNot(Fail)|Cx],Stk,Stk0),
  dropStk(Stk0,1,Stka),
  mergeStkLvl(Stk,Stka,Stkx,"value conditional").
compCond(E,Lc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compExp(E,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C,[iIf(Fail)|Cx],Stk,Stk0),
  dropStk(Stk0,1,Stka),
  mergeStkLvl(Stk,Stka,Stkx,"value conditional").

% compile an expression
compExp(unreach(Lc,Tp),_Lc,_Brks,_Next,_Opts,Lx,Lx,D,Dx,[iMvC(UVar,strg("unreachable")),iAbort(Loc,UVar)|C],C) :-
  defineTmpVar(Lc,UVar,Tp,Opts,D,Dx),
  locTerm(Lc,Loc),!.
compExp(Trm,Lc,Brks,Next,Opts,L,Lx,D,Dx,[iMvC(Rslt,Trm)|C],Cx) :-
  isLiteral(Trm),!,
  tipeOf(Trm,Tp),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D1),
  call(Next,Lc,Rslt,Brks,Opts,L,Lx,D1,Dx,C,Cx).
compExp(idnt(Nm,Tp),Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  compIdExp(idnt(Nm,Tp),Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx).
compExp(ctpl(St,A),Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  tipeOf(ctpl(St,A),Tp),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C,[iAlloc(Rslt,As)|C1],As),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).
compExp(ecll(Lc,Nm,A,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-
  isIntrinsic(Nm,_,Op,Thrw),!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,D,D0),
  compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C0,C1,As),
  genIntrinsic(Op,As,Tp,Rslt,Thrw,Lc,Next,Brks,As,L1,Lx,D1,Dx,C1,Cx).
compExp(ecll(Lc,Nm,A,_Tp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C0,[iEscape(Nm,Rslt,As)|C1],As),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).
compExp(xecll(Lc,Nm,A,Tp,_ErTp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx) :-
  isIntrinsic(Nm,_,Op,Thrw),!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  compExps(A,Lc,Brks,Opts,L1,Lx,D0,D1,C0,C1,As),
  genIntrinsic(Op,As,Tp,Rslt,Thrw,Lc,Next,Brks,As,L1,Lx,D1,Dx,C1,Cx).
compExp(xecll(Lc,Nm,A,_Tp,_ErTp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  (is_member(("$try",_,TryLvl,TryVr),Brks) ->
   compExps(A,Lc,Brks,Opts,L,L1,D,D1,C0,[iXEscape(Nm,TryLvl,Rslt,TryVr,As)|C1],As),
   call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D1,Dx,C1,Cx);
   reportError("not in scope of try",[],Lc),
   D0=Dx,C=Cx,L=Lx).
compExp(cll(Lc,Nm,A,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C0,C1,As),
  genDbg(Opts,Lc,C1,[iCall(Nm,Rslt,As)|C2]),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D1,Dx,C2,Cx).
compExp(xcll(Lc,Nm,A,Tp,_ErTp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  (is_member(("$try",_,TryLvl,TryVr),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
   compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C0,C1,As),
   genDbg(Opts,Lc,C1,[iXCall(Nm,Lvl,Rslt,TryVr,As)|C2]),
   call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D1,Dx,C2,Cx);
   reportError("not in scope of try",[],Lc),
   D=Dx,C=Cx,L=Lx).
compExp(ocall(Lc,O,A,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  length(A,Ar),
  Arity is Ar+1,
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  tipeOf(O,OTp),
  defineTmpVar(Lc,OP,OTp,Opts,D0,D1),
  compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C0,C1,As),
  compExp(O,Lc,Brks,genssa:next(OP),Opts,L1,L2,D1,D2,C1,C2),
  genDbg(Opts,Lc,C2,[iOCall(Ar,OP,Rslt,As)|C3]),
  call(Next,Lc,Rslt,Brks,Opts,L2,Lx,D2,Dx,C3,Cx).
compExp(xocall(Lc,O,A,_Tp,_ErTp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  (is_member(("$try",_,TryLvl,TryVr),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   length(A,Ar),
   Arity is Ar+1,
   defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
   tipeOf(O,OTp),
   defineTmpVar(Lc,OP,OTp,Opts,D0,D1),
   compExps(A,Lc,Brks,Opts,L,L1,D1,D2,C0,C1,As),
   compExp(O,Lc,Brks,genssa:next(OP),Opts,L1,L2,D2,D3,C1,C2),
   genDbg(Opts,Lc,C2,[iXOCall(Ar,OP,ErLvl,Rslt,As)|C3]),
   call(Next,Lc,Rslt,Brks,Opts,L2,Lx,D3,Dx,C3,Cx);
   reportError("not in scope of try",[],Lc),
   D=Dx,C=Cx,L=Lx).
compExp(clos(Lb,Ar,Free,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  tipeOf(Free,FreeTp),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  defineTmpVar(Lc,FrVr,FreeTp,Opts,D1,D2),
  compExp(Free,OLc,Brks,genssa:next(FrVr),Opts,L,L1,D2,D3,C,C1),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D3,Dx,C1,[iClosure(Rslt,lbl(Lb,Ar),FrVr)|Cx]).
compExp(nth(Lc,Rc,Off,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  tipeOf(Rc,RcTp),
  defineTmpVar(Lc,RcVr,RcTp,Opts,D1,D2),
  compExp(Rc,Lc,Brks,genssa:next(RcVr),Opts,L,L1,D2,D3,C0,[iNth(Rslt,Off,RcVr)|C1]),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D3,Dx,C1,Cx).
compExp(setix(Lc,Exp,Off,Vl),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  tipeOf(Rc,RcTp),
  defineTmpVar(Lc,RcVr,RcTp,Opts,D0,D1),
  tipeOf(Vl,VlTp),
  defineTmpVar(Lc,VlVr,VlTp,Opts,D1,D2),
  compExp(Vl,Lc,Brks,genssa:next(VlVr),Opts,L,L1,D2,D3,C0,C1),
  compExp(Exp,Lc,Brks,genssa:next(RcVr),Opts,L1,L2,D3,D4,C1,[iStNth(Rslt,Off,RcVr,VlVr)|C2]),
  call(Next,Lc,Rslt,Brks,Opts,L2,Lx,D4,Dx,C2,Cx).
compExp(cel(Lc,Vl),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  tipeOf(Vl,VlTp),
  mkRefTp(VlTp,Tp),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  defineTmpVar(Lc,VlVr,VlTp,Opts,D0,D1),
  compExp(Vl,Lc,Brks,genssa:next(VlVr),Opts,L,L1,D1,D2,C0,[iCell(Rslt,VlVr)|C1]),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D2,Dx,C1,Cx).
compExp(get(Lc,Exp),_,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  tipeOf(Exp,ExTp),
  isRefTp(ExTp,Tp),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  defineTmpVar(Lc,ExVr,ExTp,Opts,D0,D1),
  compExp(Vl,Lc,Brks,genssa:next(VlVr),Opts,L,L1,D1,D2,C0,[iGet(Rslt,VlVr)|C1]),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D2,Dx,C1,Cx).
compExp(sav(Lc,Tp),OLc,_Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  chLine(Opts,OLc,Lc,C,[iSav(Rslt)|C1]),!,
  call(Next,Lc,Rslt,Brks,Opts,L,Lx,D0,Dx,C1,Cx).
compExp(savIsSet(Lc,Sv),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,type("boolean"),Opts,D,D0),
  tipeOf(Sv,SvTp),
  defineTmpVar(Lc,SvVr,SvTp,Opts,D0,D1),
  compExp(Sv,Lc,Brks,genssa:next(SvVr),Opts,L,L1,D1,D2,C0,[iTstSav(Rslt,SvVr)|C1]),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D2,Dx,C1,Cx).
compExp(savGet(Lc,Sv,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  (is_member(("$abort",_Brker,Abort,_),Brks) ->
   defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
   tipeOf(Sv,SvTp),
   defineTmpVar(Lc,SvVr,SvTp,Opts,D0,D1),
   compExp(Sv,Lc,Brks,genssa:next(SvVr),Opts,L,L1,D1,D2,C0,[iLdSav(Rslt,Abort,SvVr)|C2]),
   call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D2,Dx,C2,Cx);
   reportError("not in scope of abort",[],Lc),
   C=Cx,
   D=Dx,
   L=Lx).
compExp(savSet(Lc,Sv,Val),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  tipeOf(Sv,SvTp),
  defineTmpVar(Lc,SvVr,SvTp,Opts,D,D1),
  tipeOf(Val,VlTp),
  defineTmpVar(Lc,VlVr,VlTp,Opts,D1,D2),
  compExp(Val,Lc,Brks,genssa:next(VlVr),Opts,L,L1,D2,D3,C0,C1),
  compExp(Sv,Lc,Brks,genssa:next(SvVr),Opts,L1,L2,D3,D4,C1,[iStSav(SvVr,VlVr)|C2]),
  call(Next,Lc,SvVr,Brks,Opts,L2,Lx,D4,Dx,C2,Cx).

compExp(case(Lc,T,Cases,Deflt),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  
  stkNxtLvl(Stk,Lvl),
  compCase(T,Lc,gencode:wrapExpr(Lvl),gencode:result,Cases,Deflt,gencode:compExp,Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compExp(unpack(Lc,T,Cases,Deflt),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  stkNxtLvl(Stk,Lvl),
  compUnpack(T,Lc,gencode:wrapExpr(Lvl),gencode:result,Cases,Deflt,gencode:compExp,Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compExp(tryX(Lc,B,E,H),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  compTryX(Lc,B,E,H,OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx).
compExp(ltt(Lc,idnt(Nm,Tp),Val,Exp),OLc,Next,Last,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C1),!,
  defineLclVar(Lc,Nm,Tp,Opts,D,D1),
  compExp(Val,Lc,Brks,genssa:next(Nm),Opts,L,L1,D1,D2,C1,C2),
  compExp(Exp,Lc,Brks,Next,Opts,L1,Lx,D2,Dx,C2,Cx).
compExp(error(Lc,Msg),_OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx). % no continuation after an error
compExp(thrw(Lc,E),OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :-
  (is_member(("$try",Brker,TryLvl,TryVr),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   compExp(E,Lc,Brks,ssagen:next(TryVr),Opts,L,Lx,D,Dx,C0,C1x),
   call(Brker,TryLvl,TryVr,C1x,Cx);
   reportError("not in scope of try",[],Lc)).
compExp(cnd(Lc,Cnd,A,B,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  defineTmpVar(Lc,VlVr,Tp,Opts,D,D0),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iValof(VlVr,[iLbl(Fl,iBlock(AC))|BC]))|C1]),
  genLbl(L,Fl,L0),
  genLbl(L0,Ok,L1),
  compCond(Cnd,Lc,Fl,Brks,normal,Opts,L1,L2,D0,D1,AC,AC1),
  compExp(A,Lc,Brks,genssa:result(Ok,VlVr),Opts,L2,L3,D1,D2,AC1,[]),
  compExp(B,Lc,Brks,genssa:result(Ok,VlVr),Opts,L3,L4,D2,D3,BC,[]),
  call(Next,Lc,VlVr,Brks,Opts,L4,Lx,D3,Dx,C1,Cx).
compExp(seqD(Lc,A,B),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(A,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  resetStack(Stk,Stk1,C1,C2),
  compExp(B,Lc,Brks,Last,Opts,L1,Lx,D1,Dx,C2,Cx,Stk,Stkx).
compExp(vlof(Lc,A),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  stkNxtLvl(Stk,NxtLvl),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iValof(NxtLvl,CA))|Cx]),
  genLbl(L,Ok,L0),
  bumpStk(Stk,Stkx),
  compAction(A,Lc,[("$valof",gencode:result,Ok,Stkx)|Brks],Last,last,Opts,L0,Lx,D,Dx,CA,[],Stk,_Stkx).
compExp(tsk(Lc,F),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(F,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C0,C1,Stk,Stka),
  genLastReturn(Last,Opts,Lc,C1,[iFiber|Cx],Stka,Stkx).
compExp(susp(Lc,T,M,_Tp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(M,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  compExp(T,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,C2,Stk1,_Stka),
  genDbg(Opts,Lc,C2,[iSuspend|C3]),
  genLastReturn(Last,Opts,Lc,C3,Cx,Stk1,Stkx).
compExp(rtire(Lc,T,M,_Tp),OLc,Brks,_Last,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(M,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  compExp(T,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,C2,Stk1,_Stka),
  genDbg(Opts,Lc,C2,[iRetire|Cx]).
compExp(resme(Lc,T,M,_Tp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(M,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  compExp(T,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,C2,Stk1,_Stka),
  genDbg(Opts,Lc,C2,[iResume|C3]),
  genLastReturn(Last,Opts,Lc,C3,Cx,Stk1,Stkx).
compExp(Cond,Lc,Brks,Last,Opts,L,Lx,D,Dx,[iLbl(Ok,iValof(NLvl,[iLbl(Fl,iBlock(Lvl,C1)),iLdC(enum(False)),iResult(Ok)]))|Cx],Cx,Stk,Stkx) :-
  isCond(Cond),!,
  isTrueSymb(True),
  isFalseSymb(False),
  stkNxtLvl(Stk,NLvl),
  stkLvl(Stk,Lvl),
  genLbl(L,Ok,L0),
  genLbl(L0,Fl,L1),
  compCond(Cond,Lc,Fl,Brks,normal,Opts,L1,Lx,D,Dx,C1,[iLdC(enum(True)),iResult(Ok)|C2],Stk,_Stka),
  bumpStk(Stk,Stka),
  genLastReturn(Last,Opts,Lc,C2,Cx,Stka,Stkx).
compExp(T,Lc,_Brks,_Last,_Opts,Lx,Lx,Dx,Dx,C,C,Stk,Stk) :-
  reportError("cannot compile %s",[ltrm(T)],Lc),
  abort.

compExps([],_,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,[]) :-!.
compExps([T|Ts],Lc,Brks,Opts,L,Lx,D,Dx,C,Cx,[R|Rs]) :-
  compExp(T,Lc,Brks,genssa:next(R),Opts,L,L1,D,D1,C,C0),
  compExps(Ts,Lc,Brks,Opts,L1,Lx,D1,Dx,C0,Cx,Rs).

genIntrinsic(Op,Tp,Rslt,noThrow,Lc,_Brks,As,D,Dx,[Ins|Cx],Cx) :-!,
  Ins =.. [Op,RVr,As].
genIntrinsic(Op,Tp,Rslt,throwing,Lc,Brks,As,D,Dx,[Ins|Cx],Cx) :-
  is_member(("$try",_,Er,_),Brks),!,
  defineTmpVar(Lc,Rslt,Tp,D,Dx),
  Ins =.. [Op,Er,As].
genIntrinsic(Op,Tp,throwing,Lc,_Brks,As,Dx,Dx,Cx,Cx,"") :-
  reportError("%s not in scope of try",[ss(Op)],Lc).

/* try E in H
   compiles to
   Ok:[
     Tr:[
       E-code
       break Ok
     ]
     H]
*/

compTryX(Lc,B,idnt(E,ETp),H,OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-
  genLbl(L,Ok,L0),
  genLbl(L0,Tr,L1),
  tipeOf(B,ResTp),
  defineTmpVar(Lc,RsVr,ResTp,Opts,D,D0),
  defineLclVar(Lc,E,ETp,Opts,D0,D1),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iValof(RsVr,[iLbl(Tr,iValof(E,BC))|HC]))|Cz]),
  compExp(B,Lc,[("$try",genssa:result,Tr,E)|Brks],genssa:result(Ok,RsVr),
	  Opts,L1,L2,D1,D2,BC,[]),
  genLine(Opts,Lc,HC,H1),
  compExp(H,Lc,Brks,genssa:next(RsVr),Opts,L2,Lx,D2,Dx,H2,[iResult(Ok,RsVr)]),
  call(Next,Lc,RsVr,Brks,Opts,L,Lx,D,Dx,Cz,Cx).

compTryA(Lc,B,idnt(E,ETp),H,OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  genLbl(L,Ok,L0),
  genLbl(L0,Tr,L1),
  stkLvl(Stk,Lvl),
  stkNxtLvl(Stk,ELvl),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(Lvl,[iLbl(Tr,iValof(ELvl,BC))|HC]))|Cz]),
  compAction(B,Lc,[("$try",gencode:result,Tr,Stk)|Brks],notLast,notLast,
	  Opts,L1,L2,D,D2,BC,[iBreak(Ok)],Stk,Stka),
  genLine(Opts,Lc,HC,H1),
  defineLclVar(Lc,E,ETp,Opts,D2,D3,H1,[iSt(E)|H2]),
  compAction(H,Lc,Brks,Last,notLast,Opts,L2,Lx,D3,Dx,H2,[iBreak(Ok)],Stk,Stkb),
  reconcileStack(Stka,Stkb,Stkx,Cz,Cx),!.

isCond(cnj(_,_,_)).
isCond(cnd(_,_,_,_,_)).
isCond(dsj(_,_,_)).
isCond(ng(_,_)).
isCond(mtch(_,_,_)).

isTrueSymb("true").
isFalseSymb("false").

wrapAction(Lvl,C,iBlock(Lvl,C)).
wrapExpr(Lvl,C,iValof(Lvl,C)).

break(Lbl,iBreak(Lbl)).

result(Lbl,iResult(Lbl)).

compCase(Gv,Lc,Wrap,Break,Cases,Deflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,[iLbl(Ok,OC)|Cx],Cx,Stk,Stkx) :-
  genLbl(L,Df,L0),
  genLbl(L0,Ok,L1),
  stkLvl(Stk,Lvl),
  call(Wrap,C0,OC),

  C0 = [iLbl(Df,iBlock(Lvl,CC))|DC],

  genCaseTable(Cases,Mx,Table),
  compGvExp(Gv,GVar,Lc,Brks,Opts,L1,L2,D,D1,CG,[Case|CB],Stk,_Stk1),

  ((tipeOf(GVar,GTp),isIntegerType(GTp)) ->
     Case = iICase(Mx) ;
   Case = iCase(Mx)),

  call(Break,Ok,OkBrk),
  compCases(Table,0,Mx,GVar,OkBrk,Df,Hndlr,Brks,Last,Opts,L2,L3,D1,D2,CB,[],CC,CG,Stk,Stka),
  call(Hndlr,Deflt,Lc,Brks,Last,Opts,L3,Lx,D2,Dx,DC,[OkBrk],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"case exp").

compGvExp(idnt(Nm,Tp),idnt(Nm,Tp),Lc,_Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compIdExp(idnt(Nm,Tp),Lc,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compGvExp(Exp,idnt(GVar,Tp),Lc,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  tipeOf(Exp,Tp),
  compExp(Exp,Lc,Brks,notLast,Opts,L,Lx,D,D1,C,C1,Stk,Stkx),
  defineTmpVar(Lc,GVar,Tp,Opts,D1,Dx),
  C1=[iTee(GVar)|Cx].

genCaseTable(Cases,P,Table) :-
  length(Cases,L),
  nextPrime(L,P),
  caseHashes(Cases,P,Hs),
  sortCases(Hs,Tbl),
  (length(Tbl,L) -> Table=Tbl ;		% try up to two primes
   nextPrime(P,NP),
   caseHashes(Cases,NP,NHs),
   sortCases(NHs,Table)).

caseHashes([],_,[]).
caseHashes([(P,E,Lc)|Cases],Mx,[(P,Hx,E,Lc)|C]) :-
  caseHash(P,Mx,Hx),
  caseHashes(Cases,Mx,C).

caseHash(T,Mx,Hx) :-
  isLiteral(T),
  termHash(T,Hs),
  Hx is Hs mod Mx.
%  reportMsg("raw hash of %s is %s, mod %s is %s",[T,Hs,Mx,Hx]).

caseHash(ctpl(O,_),Mx,Hx) :-
  caseHash(O,Mx,Hx).
caseHash(whr(_,P,_),Mx,Hx) :-
  caseHash(P,Mx,Hx).

sortCases(Cases,Sorted) :-
  sort(Cases,gencode:compareHash,SS),
  mergeDuplicates(SS,Sorted).

compareHash((_,H1,_),(_,H2,_)) :- H1<H2.

mergeDuplicates([],[]).
mergeDuplicates([(P,H,E)|M],[(H,[(P,E)|D])|Mr]) :-
  mergeDuplicate(M,H,D,Rs),
  mergeDuplicates(Rs,Mr).

mergeDuplicate([(P,H,E)|M],H,[(P,E)|Ds],Rs) :-!,
  mergeDuplicate(M,H,Ds,Rs).
mergeDuplicate(M,_,[],M).

compCases([],Ix,Mx,_GVar,_OkBrk,_Df,_Hndlr,_Brks,_Last,_Opts,Lx,Lx,D,D,Tx,Tx,Cx,Cx,_Stk,none) :-
  Ix>=Mx.
compCases([],Ix,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,[iBreak(Dflt)|T],Tx,C,Cx,Stk,Stkx) :-
  Ix1 is Ix+1,
  compCases([],Ix1,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,T,Tx,C,Cx,Stk,Stkx).
compCases([(Ix,Case)|Cs],Ix,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,[iBreak(El)|T],Tx,[iLbl(El,iBlock(Lvl,CC))|CCx],Cx,Stk,Stkx) :-!,
  stkLvl(Stk,Lvl),
  Ix1 is Ix+1,
  genLbl(L,El,L0),
  compCases(Cs,Ix1,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L0,L1,D,D1,T,Tx,CC,Cx,Stk,Stk2),
  compCaseBranch(Case,GVar,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L1,Lx,D1,Dx,CCx,[],Stk,Stk1),
  mergeStkLvl(Stk1,Stk2,Stkx,"case branch").
compCases(Cs,Ix,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,[iBreak(Dflt)|T],Tx,C,Cx,Stk,Stkx) :-
  Ix1 is Ix+1,
  compCases(Cs,Ix1,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,T,Tx,C,Cx,Stk,Stkx).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E,Lc)],GV,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  genLine(Opts,Lc,C,C0),
  compIdExp(GV,Lc,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk0),
  compPtn(P,Lc,Dflt,Brks,Opts,L1,L2,D1,D2,C1,C2,Stk0,Stk1),
  verify(Stk=Stk1,"patterns not allowed to modify stack"),
  call(Hndlr,E,Lc,Brks,Last,Opts,L2,Lx,D2,Dx,C2,[OkBrk|Cx],Stk1,Stkx).
compCaseBranch(Entries,GVar,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compCaseCond(Entries,GVar,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

compCaseCond([],_GVar,_OkBrk,Dflt,_Hndlr,_Last,Lx,Lx,Dx,Dx,[iBreak(Dflt)|Cx],Cx,_Stk,none).
compCaseCond([(P,E,Lc)],GV,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compIdExp(GV,Lc,notLast,Opts,L,L1,D,D1,C,C1,Stk,Stk0),
  compPtn(P,Lc,Dflt,Brks,Opts,L1,L2,D1,D2,C1,C2,Stk0,Stk1),
  verify(Stk=Stk1,"patterns not allowed to modify stack"),
  call(Hndlr,E,Lc,Brks,Last,Opts,L2,Lx,D2,Dx,C2,[OkBrk|Cx],Stk,Stkx).
compCaseCond([(P,E,Lc)|More],GV,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,
	     [iLbl(Fl,iBlock(Lvl,AC))|BC],[],Stk,Stkx) :-
  stkLvl(Stk,Lvl),
  genLbl(L,Fl,L0),
  compIdExp(GV,Lc,notLast,Opts,L0,L1,D,D1,AC,C1,Stk,Stk0),
  compPtn(P,Lc,Fl,Brks,Opts,L1,L2,D1,D2,C1,C2,Stk0,Stk1),
  verify(Stk=Stk1,"patterns not allowed to modify stack"),
  call(Hndlr,E,Lc,Brks,Last,Opts,L2,L3,D2,D3,C2,[OkBrk],Stk,Stka),
  compCaseCond(More,GV,OkBrk,Dflt,Hndlr,Brks,Last,Opts,L3,Lx,D3,Dx,BC,[],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"disjunction").

compUnpack(Gv,Lc,Wrap,Break,Cases,Deflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,[iLbl(Ok,OC)|Cx],Cx,Stk,Stkx) :-
  genLbl(L,Df,L0),
  genLbl(L0,Ok,L1),
  stkLvl(Stk,Lvl),
  tipeOf(Gv,Tp),
  getTypeIndex(Tp,D,Index),
  call(Wrap,[iLbl(Df,iBlock(Lvl,CC))|DC],OC),
  genUnpackTable(Cases,Index,Table),
  maxTableEntry(Table,Mx),
  checkOpt(Opts,traceGenCode,showMsg(Lc,"unpack table %s size %s",[Table,ix(Mx)])),
  compGvExp(Gv,GVar,Lc,Brks,Opts,L1,L2,D,D1,CG,[iIxCase(Mx)|CB],Stk,_Stk1),
  call(Break,Ok,OkBrk),
  compCases(Table,0,Mx,GVar,OkBrk,Df,Hndlr,Brks,Last,Opts,L2,L3,D1,D2,CB,[],CC,CG,Stk,Stka),
  call(Hndlr,Deflt,Lc,Brks,Last,Opts,L3,Lx,D2,Dx,DC,[OkBrk],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"case exp").

genUnpackTable(Cases,Index,Table) :-
  genConsTable(Cases,Index,[],Table),!.

genConsTable([],_,Tbl,Tbl).
genConsTable([(P,E,Lc)|Cases],Index,Tbl,Table) :-
  consLabelIndex(P,Index,Ix),
  insertInTable((P,E,Lc),Ix,Tbl,Tbl1),
  genConsTable(Cases,Index,Tbl1,Table).

maxTableEntry(Tbl,Mx) :-
  mxEntry(Tbl,0,M),!,
  Mx is M+1.

mxEntry([],M,M).
mxEntry([(Ix,_)|Tbl],M,Mx) :-
  Ix>M ->
  M1 is Ix+1,
  mxEntry(Tbl,M1,Mx);
  mxEntry(Tbl,M,Mx).

consLabelIndex(ctpl(Lbl,_),Index,Ix) :-
  is_member((Lbl,Ix),Index).
consLabelIndex(enum(Lbl),Index,Ix) :-
  is_member((lbl(Lbl,0),Ix),Index).

insertInTable(P,Ix,[],[(Ix,[P])]).
insertInTable(P,Ix,[(Iy,E)|Tbl],[(Ix,[P]),(Iy,E)|Tbl]) :-
  Iy>Ix,!.
insertInTable(P,Ix,[(Iy,E)|Tbl],[(Iy,E)|Tblx]) :-
  Iy<Ix,
  insertInTable(P,Ix,Tbl,Tblx).
insertInTable(P,Ix,[(Ix,E)|Tbl],[(Ix,EE)|Tbl]) :-
  concat([P],E,EE).

% comp exp continuations
next(R,_Lc,R,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx).

valof(Lvl,_Lc,R,_Brks,_Opts,Lx,Lx,Dx,Dx,[iResult(Lvl,R)|Cx],Cx).

return(Lc,R,_Brks,Opts,Lx,Lx,Dx,Dx,C,Cx) :-
  genDbg(Opts,R,C,[iRet(R)|Cx]).

genLastReturn(last(R),Opts,Lc,C,Cx) :-
  genRet(R,Opts,Lc,C,Cx).
genLastReturn(notLast,_R,_Opts,_Lc,C,C).
genLastReturn(valof(Lvl),R,Opts,Lc,.


genRet(R,Opts,Lc,C,Cx) :-
  genDbg(Opts,Lc,C,[iRet(R)|Cx]).

