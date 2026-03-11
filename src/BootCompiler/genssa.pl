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
:- use_module(ssapeep).
:- use_module(intrinsics).
:- use_module(escapes).

genSSA(PkgDecls,mdule(Pkg,Imports,Decls,LDecls,Defs),Opts,Text) :-
  encPkg(Pkg,PT),
  initDict(D0,Opts),
  genImports(Imports,ImpTpl),
  rfold(PkgDecls,genssa:decl(Opts),D0,D2),
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
  map(Imps,genssa:genImport,Els),
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
  rfold(Defs,genssa:genDef(D,Opts),Ox,O).

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
  isThrowingType(Tp,_RsTp,ErTp),!,
  toLtipe(Tp,LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L1),
  genLbl(L1,Er,L2),
  defineTmpVar(Lc,AbrtVr,ErTp,Opts,D,D1),
  genLine(Opts,Lc,C,
	  [iLbl(Abrt,iBlock([iLbl(Er,iValof(AbrtVr,FC)),iXRet(AbrtVr)]))|CA]),
  BaseBrks = [("$try",Er,AbrtVr)],
  compArgs(Args,Lc,0,Abrt,BaseBrks,Opts,L2,L3,D1,D2),
  compExp(Value,Lc,[("$abort",Abrt)|BaseBrks],genssa:return,
	  Opts,L3,L4,D2,D3,FC,[]),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[]),
  getLsMap(Dx,LsMap),
  getAgMap(Args,_Ags,AgMap),
  length(LsMap,LcCnt),
  length(AgMap,Arity),
  GenFunc = func(Nm,H,Sig,AgMap,LsMap,[iEntry(Arity,LcCnt)|C]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenFunc);true ),
  peepOptimize(GenFunc,PFunc),
  (is_member(showGenCode,Opts) -> dispSSA(PFunc);true ),
  assemSSA(PFunc,CdTrm).
genFun(Lc,Nm,H,Tp,Args,Value,D,Opts,CdTrm) :-
  toLtipe(Tp,LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L1),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(FC))|CA]),
  compArgs(Args,Lc,0,Abrt,[],Opts,L1,L3,D,D2),
  compExp(Value,Lc,[("$abort",Abrt)],genssa:return,Opts,L3,L4,D2,D3,FC,[]),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[]),
  getLsMap(Dx,LsMap),
  getAgMap(Args,_Ags,AgMap),
  length(LsMap,LcCnt),
  length(AgMap,Arity),
  GenFunc = func(Nm,H,Sig,AgMap,LsMap,[iEntry(Arity,LcCnt)|C0]),
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
  genLbl(L1,ErLbl,L2),
  defineTmpVar(Lc,AbrtVr,ErTp,Opts,D,D1),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock([iLbl(ErLbl,iValof(AbrtVr,FC)),
					iXRet(AbrtVr)]))|CA]),
  BaseBrks = [("$try",ErLbl,AbrtVr)],
  compArgs(Args,Lc,0,Abrt,BaseBrks,Opts,L2,L3,D1,D2),
  compAction(Act,Lc,[("$abort",Abrt)|BaseBrks],
	     genssa:rtn,Opts,L3,L4,D2,D3,FC,[iBreak(Lx)]),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[]),
  getLsMap(Dx,LsMap),
  getAgMap(Args,_Ags,AgMap),
  length(LsMap,LcCnt),
  length(AgMap,Arity),
  GenFunc = func(Nm,hard,Sig,AgMap,LsMap,[iEntry(Arity,LcCnt)|C0]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenFunc);true ),
  peepOptimize(GenFunc,PFunc),
  (is_member(showGenCode,Opts) -> dispSSA(PFunc);true ),
  assemSSA(PFunc,CdTrm).
genPrc(Lc,Nm,Tp,Args,Act,D,Opts,CdTrm) :-
  toLtipe(Tp,LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L0),
  genLbl(L0,Lx,L1),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(FC))|CA]),
  compArgs(Args,Lc,0,Abrt,[],Opts,L1,L3,D,D2),
  compAction(Act,Lc,[("$abort",Abrt)],genssa:rtn,Opts,L3,L4,D2,D3,FC,[iBreak(Lx)]),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[]),
  getLsMap(Dx,LsMap),
  getAgMap(Args,_Ags,AgMap),
  length(LsMap,LcCnt),
  length(AgMap,Arity),
  GenFunc = func(Nm,hard,Sig,AgMap,LsMap,[iEntry(Arity,LcCnt)|C0]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenFunc);true ),
  peepOptimize(GenFunc,PFunc),
  (is_member(showGenCode,Opts) -> dispSSA(PFunc);true ),
  assemSSA(PFunc,CdTrm).

genGlb(Lc,Nm,Tp,Value,D,Opts,Cd) :-
  toLtipe(funType(tplType([]),Tp,voidType),LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L0),
  genLbl(L0,_Lx,L1),
  defineTmpVar(Lc,RsltVr,Tp,Opts,D,D1),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(FC))|CA]),
  compExp(Value,Lc,[],genssa:next(RsltVr),Opts,L1,L2,D,D1,FC,[iSG(Nm,RsltVr),iRet(RsltVr)|FC1]),
  compAbort(Lc,strg("def failed"),[],Opts,L2,_L3,D1,Dx,CA,[]),
  genRet(Opts,Lc,FC1,[]),
  getLsMap(Dx,LsMap),
  length(LsMap,LclCnt),
  GenGlb = func(lbl(Nm,0),hard,Sig,[],LsMap,[iEntry(0,LclCnt)|C0]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenGlb);true ),
  peepOptimize(GenGlb,PGlb),
  (is_member(showGenCode,Opts) -> dispSSA(PGlb);true ),
  assemSSA(PGlb,Cd).

initDict(D,Opts) :-
  stdDecl(Decls),
  rfold(Decls,genssa:decl(Opts),scope([],tps{}),D).

getLsMap(scope(Dx,_),Vrs) :-
  extractLMap(Dx,Vrs),!.

extractLMap([],[]).
extractLMap([(Nm,Spec,l(_))|Dx],[(Nm,Spec)|LsMap]) :-
  extractLMap(Dx,LsMap).
extractLMap([_|Dx],LsMap) :-
  extractLMap(Dx,LsMap).

getAgMap(Args,Map,AgMap) :-
  extractAMap(Args,Map,AgMap),!.

extractAMap([],[],[]).
extractAMap([idnt(Nm,Tp)|As],[Nm|Map],[(Nm,strg(Sig))|LsMap]) :-
  toLtipe(Tp,T),
  encLtp(T,Sig),
  extractAMap(As,Map,LsMap).

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

compIdExp(idnt(Nm,_),Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-
  lclVar(Nm,Tp,V,D),!,
  compVar(V,Tp,Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx).
compIdExp(idnt(Nm,_),Lc,_Brks,_Next,_Opts,Lx,Lx,Dx,Dx,Cx,Cx) :-
  reportError("cannot locate variable %s",[id(Nm)],Lc),
  abort.

compVar(l(X),_,Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :- !,
  call(Next,Lc,X,Brks,Opts,L,Lx,D,Dx,C,Cx).
compVar(g(GlbNm),Tp,Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-
   defineTmpVar(Lc,V,Tp,Opts,D,D1),
   genDbg(Opts,Lc,C,[iLG(GlbNm),iRSP(V)|C1]),
   call(Next,Lc,V,Brks,Opts,L,Lx,D1,Dx,C1,Cx).

/* Compile actions */
compAction(nop(_),_Lc,_Brks,_Next,_Opts,Lx,Lx,Dx,Dx,Cx,Cx) :-!.
compAction(seq(Lc,A,B),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compAction(A,Lc,Brks,genssa:drop,Opts,L,L1,D,D1,C0,C1),
  compAction(B,Lc,Brks,Next,Opts,L1,Lx,D1,Dx,C1,Cx).
compAction(lbld(Lc,Lb,A),OLc,Brks,Next,Opts,L,Lx,D,Dx,[iLbl(BrkLb,iBlock(BC))|Cx],Cx) :-!,
  chLine(Opts,OLc,Lc,BC,C0),
  genLbl(L,BrkLb,L1),
  compAction(A,Lc,[(Lb,BrkLb)|Brks],Next,Opts,L1,Lx,D,Dx,C0,[iBreak(BrkLb)]).
compAction(brk(Lc,Nm),OLc,Brks,_,Opts,Lx,Lx,Dx,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  (is_member((Nm,Lbl),Brks) ->
   C0=[iBreak(Lbl)|Cx];
   reportError("not in scope of break label %s",[ss(Nm)],Lc),
   C0=Cx).
compAction(aThrow(Lc,E),OLc,Brks,_,Opts,L,Lx,D,Dx,C,Cx) :-
  (is_member(("$try",AbrtLvl),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   compExp(E,Lc,Brks,genssa:valof(AbrtLvl),Opts,L,Lx,D,Dx,C0,Cx);
   reportError("not in scope of try",[],Lc)).
compAction(perf(Lc,Cll),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(Cll,Lc,Brks,Next,Opts,L,Lx,D,Dx,C0,Cx).
compAction(mtch(Lc,P,E),OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :- !,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(Lvl,[iLbl(Abrt,iBlock(Lvl,CB))|Ca]))|Cx]),
  genLbl(L,Ok,L1),
  genLbl(L1,Abrt,L2),
  tipeOf(E,Tp),
  defineTmpVar(Lc,ExpVr,Tp,Opts,D,D0),
  compExp(E,Lc,Brks,genssa:next(ExpVr),Opts,L2,L3,D0,D1,CB,C1),
  compPtn(P,ExpVr,Lc,Abrt,Brks,Opts,L3,L4,D1,D2,C1,[iBreak(Ok)]),
  compAbort(Lc,strg("match fail"),Brks,Opts,L4,Lx,D2,Dx,Ca,[]).
compAction(defn(Lc,idnt(Nm,Tp),E),OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  defineLclVar(Lc,Nm,Tp,Opts,D,D0),
  compExp(E,Lc,Brks,genssa:next(Nm),Opts,L,Lx,D0,Dx,C0,C1),
  genBind(Opts,Nm,C1,Cx).
compAction(setix(Lc,Trm,Off,Vl),OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  tipeOf(Vl,VlTp),
  defineTmpVar(Lc,VlVr,VlTp,Opts,D,D0),
  tipeOf(Trm,TrmTp),
  defineTmpVar(Lc,TrmVr,TrmTp,Opts,D0,D1),
  compExp(Vl,Lc,Brks,genssa:next(VlVr),Opts,L,L1,D1,D2,C0,C1),
  compExp(Trm,Lc,Brks,genssa:next(TrmVr),Opts,L1,Lx,D2,Dx,C1,[iStNth(TrmVr,Off,VlVr)|Cx]).
compAction(asgn(Lc,Cll,Vl),OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  tipeOf(Vl,VlTp),
  defineTmpVar(Lc,VlVr,VlTp,Opts,D,D0),
  tipeOf(Cll,CllTp),
  defineTmpVar(Lc,ClVr,CllTp,Opts,D0,D1),
  compExp(Vl,Lc,Brks,genssa:next(VlVr),Opts,L,L1,D1,D2,C0,C1),
  compExp(Cll,Lc,Brks,genssa:next(ClVr),Opts,L1,Lx,D2,Dx,C1,[iAssign(ClVr,VlVr)|Cx]).
compAction(case(Lc,T,Cases,Deflt),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compCase(T,Lc,genssa:wrapAction,Cases,Deflt,genssa:compAction,Brks,Next,Opts,L,Lx,D,Dx,C0,Cx).
compAction(unpack(Lc,T,Cases,Deflt),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compUnpack(T,Lc,genssa:wrapAction,genssa:break,Cases,Deflt,genssa:compAction,Brks,Next,Opts,L,Lx,D,Dx,C0,Cx).
compAction(whle(Lc,G,B),OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Done,iBlock([iLbl(Lp,iBlock(LC))]))|Cx]),!,
  genLbl(L,Lp,L1),
  genLbl(L1,Done,L2),
  compCond(G,Lc,Done,Brks,normal,Opts,L2,L3,D,D1,LC,LC1),
  compAction(B,Lc,Brks,genssa:drop,Opts,L3,Lx,D1,Dx,LC1,[iLoop(Lp)]).
compAction(ltt(Lc,idnt(Nm,Tp),Val,Act),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C1),!,
  defineLclVar(Lc,Nm,Tp,Opts,D,D1),
  compExp(Val,Lc,Brks,genssa:next(Nm),Opts,L,L1,D1,D2,C1,C2),
  compAction(Act,Lc,Brks,Next,Opts,L1,Lx,D2,Dx,C2,Cx).
compAction(iftte(Lc,G,A,B),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Thn,iBlock([iLbl(Fl,iBlock(AC))|BC]))|Cx]),
  genLbl(L,Fl,L0),
  genLbl(L0,Thn,L1),
  compCond(G,Lc,Fl,Brks,normal,Opts,L1,L2,D,D1,AC,AC1),
  compAction(A,Lc,Brks,Next,Opts,L2,L3,D1,D2,AC1,[iBreak(Thn)]),
  compAction(B,Lc,Brks,Next,Opts,L3,Lx,D2,Dx,BC,[iBreak(Thn)]).
compAction(aTry(Lc,B,E,H),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  compTryA(Lc,B,E,H,OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx).
compAction(vls(Lc,E),OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  (is_member(("$valof",Ok),Brks) ->
     chLine(Opts,OLc,Lc,C,C0),
     compExp(E,Lc,Brks,genssa:valof(Ok),Opts,L,Lx,D,Dx,C0,Cx);
   reportError("not in scope of valof",[],Lc)).
compAction(error(Lc,Msg),_OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx).
compAction(A,Lc,_Brks,_Next,_Opts,Lx,Lx,Dx,Dx,C,C) :-
  reportError("cannot compile action %s",[lact(A)],Lc),
  abort.

compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  locTerm(Lc,LT),
  compExp(Msg,Lc,Brks,genssa:next(MVr),Opts,L,Lx,D,Dx,C,[iAbort(LT,MVr)|Cx]).

varGetter(Vr,Lc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compIdExp(Vr,Lc,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

nthGetter(Vr,Off,Lc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compIdExp(Vr,Lc,notLast,Opts,L,Lx,D,Dx,C,[iNth(Off)|Cx],Stk,Stkx).

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

% Compile arguments to function
compArgs([],_Lc,_Ix,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx) :-!.
compArgs([idnt(Nm,Tp)|Args],Lc,Ix,Fail,Brks,Opts,L,Lx,D,Dx) :-!,
  defineVar(Lc,Nm,Tp,Opts,l(Nm),D,D1),
  Ix1 is Ix+1,
  compArgs(Args,Lc,Ix1,Fail,Brks,Opts,L,Lx,D1,Dx).

% compile a pattern. Patterns form a reduced subset of possible expression forms
compPtn(voyd,_Vr,_Lc,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx).
compPtn(ann(_),_Vr,_,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx).
compPtn(intgr(Ix),Vr,_,Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iCInt(intgr(Ix),Fail,Vr)|Cx],Cx).
compPtn(Lit,Vr,_Lc,Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iCLit(Lit,Fail,Vr)|Cx],Cx) :-
  isLiteral(Lit).
compPtn(idnt(Nm,Tp),Vr,Lc,_Fail,_Brks,Opts,Lx,Lx,D,Dx,[iMV(Nm,Vr)|C],Cx) :-
  defineLclVar(Lc,Nm,Tp,Opts,D,Dx),
  genBind(Opts,Nm,C,Cx).
compPtn(ctpl(St,Args),Vr,Lc,Fail,Brks,Opts,L,Lx,D,Dx,[iCLbl(Fail,St,Vr)|C],Cx) :-
  compPtnArgs(Args,Vr,Lc,0,Fail,Brks,notLast,Opts,L,Lx,D,Dx,C,Cx).
compPtn(savGet(Lc,V,STp),Vr,_,Fail,Brks,Opts,L,Lx,D,Dx,[iLdSav(SVr,Fail,Vr)|C],Cx) :-
  defineTmpVar(Lc,SVr,STp,Opts,D,D0),
  compPtn(V,SVr,Lc,Fail,Brks,Opts,L,Lx,D0,Dx,C,Cx).
compPtn(whr(Lc,P,Cnd),Vr,OLc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compPtn(P,Vr,Lc,Fail,Brks,Opts,L,L1,D,D1,C0,C1),
  compCond(Cnd,Lc,Fail,Brks,normal,Opts,L1,Lx,D1,Dx,C1,Cx).
compPtn(T,_Vr,Lc,Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iBreak(Fail)|Cx],Cx) :-
  reportError("(internal) cannot compile pattern %s",[ltrm(T)],Lc).

% compile argument patterns. If fail, then break out
compPtnArgs([],_PVar,_Lc,_Ix,_Fail,_Brks,_Last,_Opts,Lx,Lx,Dx,Dx,Cx,Cx).
compPtnArgs([A|R],PVar,Lc,Ix,Fail,Brks,Last,Opts,L,Lx,D,Dx,[iNth(AVr,Ix,PVar)|C],Cx) :-
  tipeOf(A,ATp),
  defineTmpVar(Lc,AVr,ATp,Opts,D,D0),
  compPtn(A,AVr,Lc,Fail,Brks,Opts,L,L1,D0,D2,C,C1),
  Ix1 is Ix+1,
  compPtnArgs(R,PVar,Lc,Ix1,Fail,Brks,Last,Opts,L1,Lx,D2,Dx,C1,Cx).

% compile a condition. Invoke passed in Fail label if the condition is false
compCond(enum(Sy),_Lc,_Fail,_Brks,normal,_Opts,Lx,Lx,Dx,Dx,Cx,Cx) :-
  isTrueSymb(Sy),!.
compCond(enum(Sy),_Lc,Fail,_Brks,negated,_Opts,Lx,Lx,Dx,Dx,[iBreak(Fail)|Cx],Cx) :-
  isTrueSymb(Sy),!.
compCond(enum(Sy),_,Fail,_Brks,normal,_Opts,Lx,Lx,Dx,Dx,[iBreak(Fail)|Cx],Cx) :-
  isFalseSymb(Sy),!.
compCond(enum(Sy),_,_Fail,_Brks,negated,_Opts,Lx,Lx,Dx,Dx,Cx,Cx) :-
  isFalseSymb(Sy),!.
compCond(cnj(Lc,A,B),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compCond(A,Lc,Fail,Brks,normal,Opts,L,L1,D,D1,C0,C1),
  compCond(B,Lc,Fail,Brks,normal,Opts,L1,Lx,D1,Dx,C1,Cx).
compCond(cnj(Lc,A,B),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx) :- !, % apply de morgan's law
  compCond(dsj(Lc,ng(Lc,A),ng(Lc,B)),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx).
compCond(dsj(Lc,A,B),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock([iLbl(Fl,iBlock(AC))|BC]))|Cx]),
  genLbl(L,Fl,L0),
  genLbl(L0,Ok,L1),
  compCond(A,Lc,Fl,Brks,normal,Opts,L1,L2,D,D0,AC,[iBreak(Ok)]),
  compCond(B,Lc,Fail,Brks,normal,Opts,L2,Lx,D0,Dx,BC,[iBreak(Ok)]).
compCond(dsj(Lc,A,B),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx) :- !, % apply de morgan's law
  compCond(cnj(Lc,ng(Lc,A),ng(Lc,B)),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx).
compCond(ng(Lc,A),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compCond(A,Lc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C0,Cx).
compCond(ng(Lc,A),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compCond(A,Lc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C0,Cx).
compCond(cnd(Lc,T,A,B),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock([iLbl(El,iBlock(CA))|CB]))|Cx]),
  genLbl(L,El,L0),
  compCond(T,Lc,El,Brks,normal,Opts,L0,L1,D,D1,CA,C0),
  genLbl(L1,Ok,L2),
  compCond(A,Lc,Fail,Brks,normal,Opts,L2,L3,D1,D2,C0,[iBreak(Ok)]),
  compCond(B,Lc,Fail,Brks,normal,Opts,L3,Lx,D2,Dx,CB,[iBreak(Ok)]).
compCond(cnd(Lc,T,A,B),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx) :-!,
  compCond(cnd(Lc,T,B,A),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx).
compCond(mtch(Lc,P,E),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,Brks,genssa:next(Vr),Opts,L,L3,D,D1,C0,C1),
  compPtn(P,Vr,Lc,Fail,Brks,Opts,L3,Lx,D1,Dx,C1,Cx).
compCond(mtch(Lc,P,E),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(CB))|Cx]),
  genLbl(L,Ok,L1),
  tipeOf(E,ETp),
  defineTmpVar(Lc,Vr,ETp,Opts,D,D0),
  compExp(E,Lc,Brks,genssa:next(Vr),Opts,L1,L3,D0,D1,CB,C1),
  compPtn(P,Vr,Lc,Ok,Brks,Opts,L3,Lx,D1,Dx,C1,[iBreak(Fail)]).
compCond(E,Lc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx) :-!,
  defineTmpVar(Lc,Vr,type("boolean"),Opts,D,D0),
  compExp(E,Lc,Brks,genssa:next(Vr),Opts,L,Lx,D0,Dx,C,[iIfNot(Fail,Vr)|Cx]).
compCond(E,Lc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx) :-
  defineTmpVar(Lc,Vr,type("boolean"),Opts,D,D0),
  compExp(E,Lc,Brks,genssa:next(Vr),Opts,L,Lx,D0,Dx,C,[iIf(Fail)|Cx]).

% compile an expression
compExp(unreach(Lc,Tp),_Lc,_Brks,_Next,Opts,Lx,Lx,D,Dx,[iMC(UVar,strg("unreachable")),iAbort(Loc,UVar)|C],C) :-
  defineTmpVar(Lc,UVar,Tp,Opts,D,Dx),
  locTerm(Lc,Loc),!.
compExp(Trm,Lc,Brks,Next,Opts,L,Lx,D,Dx,[iMC(Rslt,Trm)|C],Cx) :-
  isLiteral(Trm),!,
  tipeOf(Trm,Tp),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D1),
  call(Next,Lc,Rslt,Brks,Opts,L,Lx,D1,Dx,C,Cx).
compExp(idnt(Nm,Tp),Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  compIdExp(idnt(Nm,Tp),Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx).
compExp(ctpl(St,A),Lc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  tipeOf(ctpl(St,A),Tp),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C,[iAlloc(St,Rslt,As)|C1],As),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).
compExp(ecll(Lc,Nm,A,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-
  isIntrinsic(Nm,_,Op,Thrw),!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C0,C1,As),
  genIntrinsic(Op,Rslt,As,Thrw,Lc,Brks,Next,Opts,L1,Lx,D1,Dx,C1,Cx).
compExp(ecll(Lc,Nm,A,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C0,[iEscape(Nm,As),iRSP(Rslt)|C1],As),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).
compExp(xecll(Lc,Nm,A,Tp,_ErTp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-
  isIntrinsic(Nm,_,Op,Thrw),!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C0,C1,As),
  genIntrinsic(Op,Rslt,As,Thrw,Lc,Brks,Next,Opts,L1,Lx,D1,Dx,C1,Cx).
compExp(xecll(Lc,Nm,A,Tp,_ErTp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  (is_member(("$try",TryLvl),Brks) ->
   compExps(A,Lc,Brks,Opts,L,L1,D,D1,C0,[iEscape(Nm,As),iRSX(TryLvl,Rslt)|C1],As),
   call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D1,Dx,C1,Cx);
   reportError("not in scope of try",[],Lc),
   D0=Dx,C=Cx,L=Lx).
compExp(cll(Lc,Nm,A,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C0,C1,As),
  genDbg(Opts,Lc,C1,[iCall(Nm,As),iRSP(Rslt)|C2]),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D1,Dx,C2,Cx).
compExp(xcll(Lc,Nm,A,Tp,_ErTp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  (is_member(("$try",TryLvl),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
   compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C0,C1,As),
   genDbg(Opts,Lc,C1,[iCall(Nm,As),iRSX(TryLvl,Rslt)|C2]),
   call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D1,Dx,C2,Cx);
   reportError("not in scope of try",[],Lc),
   D=Dx,C=Cx,L=Lx).
compExp(ocall(Lc,O,A,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  tipeOf(O,OTp),
  defineTmpVar(Lc,OP,OTp,Opts,D0,D1),
  compExps(A,Lc,Brks,Opts,L,L1,D0,D1,C0,C1,As),
  compExp(O,Lc,Brks,genssa:next(OP),Opts,L1,L2,D1,D2,C1,C2),
  genDbg(Opts,Lc,C2,[iOCall(OP,As),iRSP(Rslt)|C3]),
  call(Next,Lc,Rslt,Brks,Opts,L2,Lx,D2,Dx,C3,Cx).
compExp(xocall(Lc,O,A,Tp,_ErTp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  (is_member(("$try",TryLvl),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
   tipeOf(O,OTp),
   defineTmpVar(Lc,OP,OTp,Opts,D0,D1),
   compExps(A,Lc,Brks,Opts,L,L1,D1,D2,C0,C1,As),
   compExp(O,Lc,Brks,genssa:next(OP),Opts,L1,L2,D2,D3,C1,C2),
   genDbg(Opts,Lc,C2,[iOCall(OP,As),iRSP(TryLvl,Rslt)|C3]),
   call(Next,Lc,Rslt,Brks,Opts,L2,Lx,D3,Dx,C3,Cx);
   reportError("not in scope of try",[],Lc),
   D=Dx,C=Cx,L=Lx).
compExp(clos(Lb,Ar,Free,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  tipeOf(Free,FreeTp),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  defineTmpVar(Lc,FrVr,FreeTp,Opts,D0,D1),
  compExp(Free,OLc,Brks,genssa:next(FrVr),Opts,L,L1,D1,D2,C,C1),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D2,Dx,C1,[iClosure(lbl(Lb,Ar),Rslt,FrVr)|Cx]).
compExp(nth(Lc,Rc,Off,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  tipeOf(Rc,RcTp),
  defineTmpVar(Lc,RcVr,RcTp,Opts,D0,D1),
  compExp(Rc,Lc,Brks,genssa:next(RcVr),Opts,L,L1,D1,D2,C0,[iNth(Rslt,Off,RcVr)|C1]),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D2,Dx,C1,Cx).
compExp(setix(Lc,Rc,Off,Vl),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  tipeOf(Rc,RcTp),
  defineTmpVar(Lc,RcVr,RcTp,Opts,D,D1),
  tipeOf(Vl,VlTp),
  defineTmpVar(Lc,VlVr,VlTp,Opts,D1,D2),
  compExp(Vl,Lc,Brks,genssa:next(VlVr),Opts,L,L1,D2,D3,C0,C1),
  compExp(Rc,Lc,Brks,genssa:next(RcVr),Opts,L1,L2,D3,D4,C1,[iStNth(RcVr,Off,VlVr)|C2]),
  call(Next,Lc,RcVr,Brks,Opts,L2,Lx,D4,Dx,C2,Cx).
compExp(cel(Lc,Vl),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  tipeOf(Vl,VlTp),
  mkRefTp(VlTp,Tp),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  defineTmpVar(Lc,VlVr,VlTp,Opts,D0,D1),
  compExp(Vl,Lc,Brks,genssa:next(VlVr),Opts,L,L1,D1,D2,C0,[iCell(Rslt,VlVr)|C1]),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D2,Dx,C1,Cx).
compExp(get(Lc,Exp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  tipeOf(Exp,ExTp),
  isRefTp(ExTp,Tp),
  defineTmpVar(Lc,Rslt,Tp,Opts,D,D0),
  defineTmpVar(Lc,ExVr,ExTp,Opts,D0,D1),
  compExp(Exp,Lc,Brks,genssa:next(ExVr),Opts,L,L1,D1,D2,C0,[iGet(Rslt,ExVr)|C1]),
  call(Next,Lc,Rslt,Brks,Opts,L1,Lx,D2,Dx,C1,Cx).
compExp(sav(Lc,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
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
  (is_member(("$abort",Abort),Brks) ->
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

compExp(case(Lc,Gv,Cases,Deflt),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compCase(Gv,Lc,genssa:wrapExpr,genssa:result,Cases,Deflt,genssa:compExp,Brks,Next,Opts,L,Lx,D,Dx,C0,Cx).
compExp(unpack(Lc,Gv,Cases,Deflt),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compUnpack(Gv,Lc,genssa:wrapExpr,genssa:result,Cases,Deflt,genssa:compExp,Brks,Next,Opts,L,Lx,D,Dx,C0,Cx).

compExp(tryX(Lc,B,E,H),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  compTryX(Lc,B,E,H,OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx).
compExp(ltt(Lc,idnt(Nm,Tp),Val,Exp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C1),!,
  defineLclVar(Lc,Nm,Tp,Opts,D,D1),
  compExp(Val,Lc,Brks,genssa:next(Nm),Opts,L,L1,D1,D2,C1,C2),
  compExp(Exp,Lc,Brks,Next,Opts,L1,Lx,D2,Dx,C2,Cx).
compExp(error(Lc,Msg),_OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx). % no continuation after an error
compExp(thrw(Lc,E),OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :-
  (is_member(("$try",TryLvl),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   compExp(E,Lc,Brks,ssagen:valof(TryLvl),Opts,L,Lx,D,Dx,C0,Cx);
   reportError("not in scope of try",[],Lc)).
compExp(cnd(Lc,Cnd,A,B),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  tipeOf(A,Tp),
  defineTmpVar(Lc,VlVr,Tp,Opts,D,D0),
  genLbl(L,Fl,L0),
  genLbl(L0,OkLbl,L1),
  chLine(Opts,OLc,Lc,C,[iLbl(OkLbl,iValof(VlVr,[iLbl(Fl,iBlock(AC))|BC]))|C1]),
  compCond(Cnd,Lc,Fl,Brks,normal,Opts,L1,L2,D0,D1,AC,AC1),
  compExp(A,Lc,Brks,genssa:valof(OkLbl),Opts,L2,L3,D1,D2,AC1,[]),
  compExp(B,Lc,Brks,genssa:valof(OkLbl),Opts,L3,L4,D2,D3,BC,[]),
  call(Next,Lc,VlVr,Brks,Opts,L4,Lx,D3,Dx,C1,Cx).
compExp(seqD(Lc,A,B),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  tipeOf(A,ATp),
  defineTmpVar(Lc,TmpVr,ATp,Opts,D,D0),
  compExp(A,Lc,Brks,genssa:next(TmpVr),Opts,L,L1,D0,D1,C0,C1),
  compExp(B,Lc,Brks,Next,Opts,L1,Lx,D1,Dx,C1,Cx).
compExp(vlof(Lc,A,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  defineTmpVar(Lc,TmpVr,Tp,Opts,D,D0),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iValof(TmpVr,CA))|C1]),
  genLbl(L,Ok,L0),
  compAction(A,Lc,[("$valof",Ok)|Brks],genssa:valof(Ok),Opts,L0,L1,D0,D1,CA,[]),
  call(Next,Lc,TmpVr,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).
compExp(tsk(Lc,F,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  defineTmpVar(Lc,TVr,Tp,Opts,D,D0),
  defineTmpVar(Lc,FVr,Tp,Opts,D0,D1),
  compExp(F,Lc,Brks,genssa:next(TVr),Opts,L,L1,D1,D2,C0,[iFiber(FVr,TVr)|C1]),
  call(Next,FVr,Brks,Opts,L1,Lx,D2,Dx,C1,Cx).
compExp(susp(Lc,T,M,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  tipeOf(T,TTp),
  tipeOf(M,MTp),
  defineTmpVar(Lc,TVr,TTp,Opts,D,D0),
  defineTmpVar(Lc,MVr,MTp,Opts,D0,D1),
  defineTmpVar(Lc,RsltVr,Tp,Opts,D1,D2),
  compExp(M,Lc,Brks,genssa:next(MVr),Opts,L,L1,D2,D3,C0,C1),
  compExp(T,Lc,Brks,genssa:next(TVr),Opts,L1,L2,D3,D4,C1,C2),
  genDbg(Opts,Lc,C2,[iSuspend(TVr,MVr),iRSP(RsltVr)|C3]),
  call(Next,RsltVr,Brks,Opts,L2,Lx,D4,Dx,C3,Cx).
compExp(resme(Lc,T,M,Tp),OLc,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  tipeOf(T,TTp),
  tipeOf(M,MTp),
  defineTmpVar(Lc,TVr,TTp,Opts,D,D0),
  defineTmpVar(Lc,MVr,MTp,Opts,D0,D1),
  defineTmpVar(Lc,RsltVr,Tp,Opts,D1,D2),
  compExp(M,Lc,Brks,genssa:next(MVr),Opts,L,L1,D2,D3,C0,C1),
  compExp(T,Lc,Brks,genssa:next(TVr),Opts,L1,L2,D3,D4,C1,C2),
  genDbg(Opts,Lc,C2,[iResume(TVr,MVr),iRSP(RsltVr)|C3]),
  call(Next,RsltVr,Brks,Opts,L2,Lx,D4,Dx,C3,Cx).
compExp(rtire(Lc,T,M,_Tp),OLc,Brks,_Next,Opts,L,Lx,D,Dx,C,Cx) :-
  chLine(Opts,OLc,Lc,C,C0),
  tipeOf(T,TTp),
  tipeOf(M,MTp),
  defineTmpVar(Lc,TVr,TTp,Opts,D,D0),
  defineTmpVar(Lc,MVr,MTp,Opts,D0,D1),
  compExp(M,Lc,Brks,genssa:next(MVr),Opts,L,L1,D1,D2,C0,C1),
  compExp(T,Lc,Brks,genssa:next(TVr),Opts,L1,Lx,D2,Dx,C1,C2),
  genDbg(Opts,Lc,C2,[iRetire(TVr,MVr)|Cx]).
compExp(Cond,Lc,Brks,Next,Opts,L,Lx,D,Dx,[iLbl(Ok,iValof(RsltVr,[iLbl(Fl,iBlock(C1)),iMC(RsltVr,enum(False)),iResult(Ok,RsltVr)]))|Cx],Cx) :-
  isCond(Cond),!,
  isTrueSymb(True),
  isFalseSymb(False),
  defineTmpVar(Lc,RsltVr,type("boolean"),Opts,D,D1),
  genLbl(L,Ok,L0),
  genLbl(L0,Fl,L1),
  compCond(Cond,Lc,Fl,Brks,normal,Opts,L1,L2,D1,D2,C1,[iMC(RsltVr,enum(True)),iResult(Ok,RsltVr)|C2]),
  call(Next,RsltVr,Brks,Opts,L2,Lx,D2,Dx,C2,Cx).
compExp(T,Lc,_Brks,_Last,_Opts,Lx,Lx,Dx,Dx,C,C) :-
  reportError("cannot compile %s",[ltrm(T)],Lc),
  abort.

compExps([],_,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,[]) :-!.
compExps([T|Ts],Lc,Brks,Opts,L,Lx,D,Dx,C,Cx,[R|Rs]) :-
  compExp(T,Lc,Brks,genssa:next(R),Opts,L,L1,D,D1,C,C0),
  compExps(Ts,Lc,Brks,Opts,L1,Lx,D1,Dx,C0,Cx,Rs).

genIntrinsic(Op,Rslt,As,noThrow,Lc,Brks,Next,Opts,L,Lx,D,Dx,[Ins|C],Cx) :-!,
  Ins =.. [Op,Rslt|As],
  call(Next,Lc,Rslt,Brks,Opts,L,Lx,D,Dx,C,Cx).
genIntrinsic(Op,Rslt,As,throwing,Lc,Brks,Next,Opts,L,Lx,D,Dx,[Ins|C],Cx) :-
  is_member(("$try",Er),Brks),!,
  Ins =.. [Op,Er,Rslt|As],
  call(Next,Lc,Rslt,Brks,Opts,L,Lx,D,Dx,C,Cx).
genIntrinsic(Op,_,_,throwing,Lc,_Brks,_Next,_Opts,Lx,Lx,Dx,Dx,Cx,Cx) :-
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
  compExp(B,Lc,[("$try",Tr)|Brks],genssa:valof(Ok),
	  Opts,L1,L2,D1,D2,BC,[]),
  genLine(Opts,Lc,HC,H1),
  compExp(H,Lc,Brks,genssa:next(RsVr),Opts,L2,Lx,D2,Dx,H1,[iResult(Ok,RsVr)]),
  call(Next,Lc,RsVr,Brks,Opts,L,Lx,D,Dx,Cz,Cx).

compTryA(Lc,B,idnt(E,ETp),H,OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  genLbl(L,Ok,L0),
  genLbl(L0,Tr,L1),
  defineLclVar(Lc,E,ETp,Opts,D,D0),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock([iLbl(Tr,iValof(E,BC))|HC]))|Cx]),
  compAction(B,Lc,[("$try",Tr)|Brks],
	  Opts,L1,L2,D0,D2,BC,[iBreak(Ok)]),
  genLine(Opts,Lc,HC,H1),
  defineLclVar(Lc,E,ETp,Opts,D2,D3,H1,[iSt(E)|H2]),
  compAction(H,Lc,Brks,Opts,L2,Lx,D3,Dx,H2,[iBreak(Ok)]).

isCond(cnj(_,_,_)).
isCond(cnd(_,_,_,_)).
isCond(dsj(_,_,_)).
isCond(ng(_,_)).
isCond(mtch(_,_,_)).

isTrueSymb("true").
isFalseSymb("false").

wrapAction(Lvl,C,iBlock(Lvl,C)).
wrapExpr(Vr,C,iValof(Vr,C)).

break(Lbl,iBreak(Lbl)).

result(Lbl,iResult(Lbl)).

breakOut(Ok,[iBreak(Ok)|Cx],Cx).

result(Lbl,[iResult(Lbl)|Cx],Cx).
  


compCase(Gv,Lc,Wrap,Break,Cases,Deflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,[iLbl(Ok,OC)|Cx],Cx) :-
  genLbl(L,Df,L0),
  genLbl(L0,Ok,L1),
  tipeOf(Gv,GvTp),
  defineTmpVar(Lc,GvVr,GvTp,Opts,D,D0),
  genCaseTable(Cases,Mx,Table),
  call(Wrap,[iLbl(Df,iBlock(CC))|DC],OC),

  compExp(Gv,Lc,Brks,genssa:next(GvVr),Opts,L1,L2,D0,D1,CC,C2),

  (isIntegerType(GvTp) -> Case = iICase(GvVr,CB) ; Case = iCase(GvVr,CB)),

  call(Break,Ok,OkBrk),

  compCases(Table,0,Mx,GvVr,OkBrk,Df,Hndlr,Brks,Next,Opts,L2,L3,D1,D2,CB,[],C2,[Case]),

  call(Hndlr,Deflt,Lc,Brks,Next,Opts,L3,Lx,D2,Dx,DC,[OkBrk]).

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
  sort(Cases,genssa:compareHash,SS),
  mergeDuplicates(SS,Sorted).

compareHash((_,H1,_),(_,H2,_)) :- H1<H2.

mergeDuplicates([],[]).
mergeDuplicates([(P,H,E)|M],[(H,[(P,E)|D])|Mr]) :-
  mergeDuplicate(M,H,D,Rs),
  mergeDuplicates(Rs,Mr).

mergeDuplicate([(P,H,E)|M],H,[(P,E)|Ds],Rs) :-!,
  mergeDuplicate(M,H,Ds,Rs).
mergeDuplicate(M,_,[],M).

compCases([],Ix,Mx,_GVar,_OkBrk,_Df,_Hndlr,_Brks,_Next,_Opts,Lx,Lx,D,D,Tx,Tx,Cx,Cx) :-
  Ix>=Mx.
compCases([],Ix,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,[iBreak(Dflt)|T],Tx,C,Cx) :-
  Ix1 is Ix+1,
  compCases([],Ix1,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,T,Tx,C,Cx).
compCases([(Ix,Case)|Cs],Ix,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,[iBreak(El)|T],Tx,[iLbl(El,iBlock(CC))|CCx],Cx) :-!,
  Ix1 is Ix+1,
  genLbl(L,El,L0),
  compCases(Cs,Ix1,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L0,L1,D,D1,T,Tx,CC,Cx),
  compCaseBranch(Case,GVar,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L1,Lx,D1,Dx,CCx,[]).
compCases(Cs,Ix,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,[iBreak(Dflt)|T],Tx,C,Cx) :-
  Ix1 is Ix+1,
  compCases(Cs,Ix1,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,T,Tx,C,Cx).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E,Lc)],GV,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-!,
  genLine(Opts,Lc,C,C0),
  compPtn(P,GV,Lc,Dflt,Brks,Opts,L,L1,D,D1,C0,C2),
  call(Hndlr,E,Lc,Brks,Next,Opts,L1,Lx,D1,Dx,C2,[OkBrk|Cx]).
compCaseBranch(Entries,GVar,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-
  compCaseCond(Entries,GVar,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,C,Cx).

compCaseCond([],_GVar,_OkBrk,Dflt,_Hndlr,_Next,Lx,Lx,Dx,Dx,[iBreak(Dflt)|Cx],Cx).
compCaseCond([(P,E,Lc)],GV,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,C,Cx) :-
  compPtn(P,GV,Lc,Dflt,Brks,Opts,L,L1,D,D1,C,C1),
  call(Hndlr,E,Lc,Brks,Next,Opts,L1,Lx,D1,Dx,C1,[OkBrk|Cx]).
compCaseCond([(P,E,Lc)|More],GV,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,
	     [iLbl(Fl,iBlock(AC))|BC],Cx) :-
  genLbl(L,Fl,L0),
  compPtn(P,GV,Lc,Fl,Brks,Opts,L0,L1,D,D1,AC,C1),
  call(Hndlr,E,Lc,Brks,Next,Opts,L1,L2,D1,D2,C1,[OkBrk]),
  compCaseCond(More,GV,OkBrk,Dflt,Hndlr,Brks,Next,Opts,L2,Lx,D2,Dx,BC,Cx).

compUnpack(Gv,Lc,Wrap,Break,Cases,Deflt,Hndlr,Brks,Next,Opts,L,Lx,D,Dx,[iLbl(Ok,OC)|Cx],Cx) :-
  genLbl(L,Df,L0),
  genLbl(L0,Ok,L1),
  tipeOf(Gv,GvTp),
  getTypeIndex(GvTp,D,Index),
  call(Wrap,[iLbl(Df,iBlock(CC))|DC],OC),
  genUnpackTable(Cases,Index,Table),
  maxTableEntry(Table,Mx),
  defineTmpVar(Lc,GvVr,GvTp,Opts,D,D0),
  compExp(Gv,GvVr,Lc,Brks,genssa:next(GvVr),Opts,L1,L2,D0,D1,CG,[iIxCase(GvVr,CB)]),
  call(Break,Ok,OkBrk),
  compCases(Table,0,Mx,GvVr,OkBrk,Df,Hndlr,Brks,Next,Opts,L2,L3,D1,D2,CB,[],CC,CG),
  call(Hndlr,Deflt,Lc,Brks,Next,Opts,L3,Lx,D2,Dx,DC,[OkBrk]).

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

drop(_,_,_,_,Lx,Lx,Dx,Dx,Cx,Cx).

valof(Lbl,_Lc,R,_Brks,_Opts,Lx,Lx,Dx,Dx,[iResult(Lbl,R)|Cx],Cx).

return(Lc,R,_Brks,Opts,Lx,Lx,Dx,Dx,C,Cx) :-
  genRet(R,Opts,Lc,C,Cx).

genRet(R,Opts,Lc,C,Cx) :-
  genDbg(Opts,Lc,C,[iRet(R)|Cx]).

rtn(Lc,_R,_Brks,Opts,Lx,Lx,Dx,Dx,C,Cx) :-
  genRtn(Opts,Lc,C,Cx).

genRtn(Opts,Lc,C,Cx) :-
  genDbg(Opts,Lc,C,[iRtn|Cx]).



