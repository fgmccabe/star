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
  BaseBrks = [("$try",Er)],
  compArgs(Args,Lc,0,Abrt,BaseBrks,Opts,L2,L3,D1,D2),
  bindExpToVar(Value,Lc,[("$abort",Abrt)|BaseBrks],Opts,RsVr,
	  L3,L4,D2,D3,FC,[iRet(RsVr)]),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[]),!,
  getLsMap(Dx,LsMap),
  prepareEntry(LsMap,Args,Ags,Lcs),
  GenFunc = func(Nm,H,Sig,LsMap,[iEntry(Ags,Lcs)|C]),
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
  bindExpToVar(Value,Lc,[("$abort",Abrt)],Opts,RsVr,L3,L4,D2,D3,FC,[iRet(RsVr)]),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[]),
  getLsMap(Dx,LsMap),
  prepareEntry(LsMap,Args,Ags,Lcs),
  GenFunc = func(Nm,H,Sig,LsMap,[iEntry(Ags,Lcs)|C0]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenFunc);true ),
  peepOptimize(GenFunc,PFunc),
  (is_member(showGenCode,Opts) -> dispSSA(PFunc);true ),
  assemSSA(PFunc,CdTrm).

genPrc(Lc,Nm,Tp,Args,Act,D,Opts,CdTrm) :-
  isThrowingType(Tp,_RsTp,ErTp),!,
  toLtipe(Tp,LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L1),
  genLbl(L1,ErLbl,L2),
  defineTmpVar(Lc,AbrtVr,ErTp,Opts,D,D1),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock([iLbl(ErLbl,iValof(AbrtVr,FC)),
					iXRet(AbrtVr)]))|CA]),
  BaseBrks = [("$try",ErLbl)],
  compArgs(Args,Lc,0,Abrt,BaseBrks,Opts,L2,L3,D1,D2),
  compAction(Act,Lc,[("$abort",Abrt)|BaseBrks],Opts,L3,L4,D2,D3,FC,[iRtn]),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[]),
  getLsMap(Dx,LsMap),
  prepareEntry(LsMap,Args,Ags,Lcs),
  GenFunc = func(Nm,hard,Sig,LsMap,[iEntry(Ags,Lcs)|C0]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenFunc);true ),
  peepOptimize(GenFunc,PFunc),
  (is_member(showGenCode,Opts) -> dispSSA(PFunc);true ),
  assemSSA(PFunc,CdTrm).
genPrc(Lc,Nm,Tp,Args,Act,D,Opts,CdTrm) :-
  toLtipe(Tp,LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L1),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(FC))|CA]),
  compArgs(Args,Lc,0,Abrt,[],Opts,L1,L3,D,D2),
  compAction(Act,Lc,[("$abort",Abrt)],Opts,L3,L4,D2,D3,FC,[iRtn]),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[]),
  getLsMap(Dx,LsMap),
  prepareEntry(LsMap,Args,Ags,Lcs),
  GenFunc = func(Nm,hard,Sig,LsMap,[iEntry(Ags,Lcs)|C0]),
  (is_member(traceGenCode,Opts) -> dispSSA(GenFunc);true ),
  peepOptimize(GenFunc,PFunc),
  (is_member(showGenCode,Opts) -> dispSSA(PFunc);true ),
  assemSSA(PFunc,CdTrm).

genGlb(Lc,Nm,Tp,Value,D,Opts,Cd) :-
  toLtipe(funType(tplType([]),Tp,voidType),LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L0),
  genLbl(L0,_Lx,L1),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(FC))|CA]),
  bindExpToVar(Value,Lc,[],Opts,RsltVr,L1,L2,D,D1,FC,[iSG(Nm,RsltVr)|FC1]),
  compAbort(Lc,strg("def failed"),[],Opts,L2,_L3,D1,Dx,CA,[]),
  genRet(RsltVr,Opts,Lc,FC1,[]),
  getLsMap(Dx,LsMap),
  prepareEntry(LsMap,[],_Ags,Lcs),
  GenGlb = func(lbl(Nm,0),hard,Sig,LsMap,[iEntry([],Lcs)|C0]),
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

prepareEntry(LsMap,Args,Ags,Lcs) :-
  getAgMap(Args,Ags,AgMap),
  getLcs(LsMap,AgMap,Lcs).

getLcs([],_,[]).
getLcs([(Nm,_)|Lx],Ags,[Nm|Lcs]) :-
  \+is_member((Nm,_),Ags),
  getLcs(Lx,Ags,Lcs).
getLcs([_|Lx],Ags,Lcs) :-
  getLcs(Lx,Ags,Lcs).

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

/* Compile actions */
compAction(nop(_),_Lc,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx) :-!.
compAction(seq(Lc,A,B),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compAction(A,Lc,Brks,Opts,L,L1,D,D1,C0,C1),
  compAction(B,Lc,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).
compAction(lbld(Lc,Lb,A),OLc,Brks,Opts,L,Lx,D,Dx,[iLbl(BrkLb,iBlock(BC))|Cx],Cx) :-!,
  chLine(Opts,OLc,Lc,BC,C0),
  genLbl(L,BrkLb,L1),
  compAction(A,Lc,[(Lb,BrkLb)|Brks],Opts,L1,Lx,D,Dx,C0,[iBreak(BrkLb)]).
compAction(brk(Lc,Nm),OLc,Brks,Opts,Lx,Lx,Dx,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  (is_member((Nm,Lbl),Brks) ->
   C0=[iBreak(Lbl)|Cx];
   reportError("not in scope of break label %s",[ss(Nm)],Lc),
   C0=Cx).
compAction(aThrow(Lc,E),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  (is_member(("$try",AbrtLvl),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   bindExpToVar(E,Lc,Brks,Opts,EVr,L,Lx,D,Dx,C0,[iResult(AbrtLvl,EVr)|Cx]);
   reportError("not in scope of try",[],Lc)).
compAction(perf(Lc,Cll),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  bindExpToVar(Cll,Lc,Brks,Opts,_Vr,L,Lx,D,Dx,C0,Cx).
compAction(mtch(Lc,P,E),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :- !,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock([iLbl(Abrt,iBlock(CB))|Ca]))|Cx]),
  genLbl(L,Ok,L1),
  genLbl(L1,Abrt,L2),
  bindExpToVar(E,Lc,Brks,Opts,ExpVr,L2,L3,D,D1,CB,C1),
  compPtn(P,ExpVr,Lc,Abrt,Brks,Opts,L3,L4,D1,D2,C1,[iBreak(Ok)]),
  compAbort(Lc,strg("match fail"),Brks,Opts,L4,Lx,D2,Dx,Ca,[]).
compAction(defn(Lc,idnt(Nm,Tp),E),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  defineLclVar(Lc,Nm,Tp,Opts,D,D0),
  bindExpToVar(E,Lc,Brks,Opts,VlVr,L,Lx,D0,Dx,C0,[iMv(Nm,VlVr)|C1]),
  genBind(Opts,Nm,C1,Cx).
compAction(setix(Lc,Rc,Off,Vl),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpToVar(Vl,Lc,Brks,Opts,VlVr,L,L1,D,D1,C0,C1),
  bindExpToVar(Rc,Lc,Brks,Opts,RcVr,L1,Lx,D1,Dx,C1,[iStNth(RcVr,Off,VlVr)|Cx]).
compAction(asgn(Lc,Cl,Vl),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpToVar(Vl,Lc,Brks,Opts,VlVr,L,L1,D,D1,C0,C1),
  bindExpToVar(Cl,Lc,Brks,Opts,ClVr,L1,Lx,D1,Dx,C1,[iAssign(ClVr,VlVr)|Cx]).
compAction(case(Lc,T,Cases,Deflt),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compCase(T,Lc,genssa:wrapAction,Cases,Deflt,genssa:compActX,Brks,Opts,L,Lx,D,Dx,C0,Cx).
compAction(unpack(Lc,T,Cases,Deflt),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compUnpack(T,Lc,genssa:wrapAction,Cases,Deflt,genssa:compActX,Brks,Opts,L,Lx,D,Dx,C0,Cx).
compAction(whle(Lc,G,B),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Done,iBlock([iLbl(Lp,iBlock(LC))]))|Cx]),!,
  genLbl(L,Lp,L1),
  genLbl(L1,Done,L2),
  compCond(G,Lc,Done,Brks,normal,Opts,L2,L3,D,D1,LC,LC1),
  compAction(B,Lc,Brks,Opts,L3,Lx,D1,Dx,LC1,[iLoop(Lp)]).
compAction(ltt(Lc,idnt(Nm,Tp),Val,Act),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C1),
  defineLclVar(Lc,Nm,Tp,Opts,D,D1),
  compExp(Val,Lc,Nm,Brks,Opts,L,L1,D1,D2,C1,C2),
  compAction(Act,Lc,Brks,Opts,L1,Lx,D2,Dx,C2,Cx).
compAction(iftte(Lc,G,A,B),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Thn,iBlock([iLbl(Fl,iBlock(AC))|BC]))|Cx]),
  genLbl(L,Fl,L0),
  genLbl(L0,Thn,L1),
  compCond(G,Lc,Fl,Brks,normal,Opts,L1,L2,D,D1,AC,AC1),
  compAction(A,Lc,Brks,Opts,L2,L3,D1,D2,AC1,[iBreak(Thn)]),
  compAction(B,Lc,Brks,Opts,L3,Lx,D2,Dx,BC,[iBreak(Thn)]).
compAction(aTry(Lc,B,E,H),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compTryA(Lc,B,E,H,Brks,Opts,L,Lx,D,Dx,C0,Cx).
compAction(vls(Lc,E),OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  (is_member(("$valof",Ok),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   bindExpToVar(E,Lc,Brks,Opts,EVr,L,Lx,D,Dx,C0,[iResult(Ok,EVr)|Cx]);
   reportError("not in scope of valof",[],Lc),
   D=Dx,C=Cx,L=Lx).
compAction(error(Lc,Msg),_OLc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx).
compAction(A,Lc,_Brks,_Opts,Lx,Lx,Dx,Dx,C,C) :-
  reportError("cannot compile action %s",[lact(A)],Lc),
  abort.

compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  locTerm(Lc,LT),
  bindExpToVar(Msg,Lc,Brks,Opts,MVr,L,Lx,D,Dx,C,[iAbort(LT,MVr)|Cx]).

compActX(Ok,Act,Lc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  compAction(Act,Lc,Brks,Opts,L,Lx,D,Dx,C,[iBreak(Ok)|Cx]).

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
compPtn(idnt(Nm,Tp),Vr,Lc,_Fail,_Brks,Opts,Lx,Lx,D,Dx,[iMv(Nm,Vr)|C],Cx) :-
  defineLclVar(Lc,Nm,Tp,Opts,D,Dx),
  genBind(Opts,Nm,C,Cx).
compPtn(ctpl(St,Args),Vr,Lc,Fail,Brks,Opts,L,Lx,D,Dx,[iCLbl(St,Fail,Vr)|C],Cx) :-
  compPtnArgs(Args,Vr,Lc,0,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx).
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
compPtnArgs([],_PVar,_Lc,_Ix,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx).
compPtnArgs([A|R],PVar,Lc,Ix,Fail,Brks,Opts,L,Lx,D,Dx,[iNth(AVr,Ix,PVar)|C],Cx) :-
  tipeOf(A,ATp),
  defineTmpVar(Lc,AVr,ATp,Opts,D,D0),
  compPtn(A,AVr,Lc,Fail,Brks,Opts,L,L1,D0,D2,C,C1),
  Ix1 is Ix+1,
  compPtnArgs(R,PVar,Lc,Ix1,Fail,Brks,Opts,L1,Lx,D2,Dx,C1,Cx).

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
  bindExpToVar(E,Lc,Brks,Opts,Vr,L,L1,D,D1,C0,C1),
  compPtn(P,Vr,Lc,Fail,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).
compCond(mtch(Lc,P,E),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(CB))|Cx]),
  genLbl(L,Ok,L1),
  bindExpToVar(E,Lc,Brks,Opts,Vr,L1,L2,D,D1,CB,C1),
  compPtn(P,Vr,Lc,Ok,Brks,Opts,L2,Lx,D1,Dx,C1,[iBreak(Fail)]).
compCond(E,Lc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx) :-!,
  bindExpToVar(E,Lc,Brks,Opts,Vr,L,Lx,D,Dx,C,[iIfNot(Fail,Vr)|Cx]).
compCond(E,Lc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx) :-
  bindExpToVar(E,Lc,Brks,Opts,Vr,L,Lx,D,Dx,C,[iIf(Fail,Vr)|Cx]).

% compile expressions

bindExpToVar(idnt(Nm,Tp),Lc,Brks,Opts,Vr,L,Lx,D,Dx,C,Cx) :-
  compIdExp(Nm,Tp,Lc,Brks,Opts,Vr,L,Lx,D,Dx,C,Cx).
bindExpToVar(Exp,Lc,Brks,Opts,Vr,L,Lx,D,Dx,C,Cx) :-
  tipeOf(Exp,Tp),
  defineTmpVar(Lc,Vr,Tp,Opts,D,D0),
  compExp(Exp,Lc,Vr,Brks,Opts,L,Lx,D0,Dx,C,Cx).

compIdExp(Nm,_,Lc,Brks,Opts,Vr,L,Lx,D,Dx,C,Cx) :-
  lclVar(Nm,Tp,V,D),!,
  compVar(V,Tp,Lc,Brks,Opts,Vr,L,Lx,D,Dx,C,Cx).
compIdExp(Nm,_,Lc,_Brks,_Opts,_,Lx,Lx,Dx,Dx,Cx,Cx) :-
  reportError("cannot locate variable %s",[id(Nm)],Lc),
  abort.

compVar(l(X),_,_Lc,_Brks,_Opts,X,Lx,Lx,Dx,Dx,Cx,Cx) :- !.
compVar(g(GlbNm),Tp,Lc,_Brks,Opts,X,Lx,Lx,D,Dx,C,Cx) :-
   defineTmpVar(Lc,X,Tp,Opts,D,Dx),
   genDbg(Opts,Lc,C,[iLG(GlbNm),iRSP(X)|Cx]).

compExpX(Ok,Exp,Lc,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  bindExpToVar(Exp,Lc,Brks,Opts,XVar,L,Lx,D,Dx,C,[iResult(Ok,XVar)|Cx]).

compExp(unreach(Lc,Tp),_Lc,_,_Brks,Opts,Lx,Lx,D,Dx,[iMC(UVar,strg("unreachable")),iAbort(Loc,UVar)|C],C) :-
  defineTmpVar(Lc,UVar,Tp,Opts,D,Dx),
  locTerm(Lc,Loc),!.
compExp(Trm,_Lc,RsVr,_Brks,_Opts,Lx,Lx,Dx,Dx,[iMC(RsVr,Trm)|Cx],Cx) :-
  isLiteral(Trm),!.
compExp(ctpl(Lbl,A),Lc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  bindExpsToVars(A,Lc,As,Brks,Opts,L,Lx,D,Dx,C,[iAlloc(Lbl,RsVr,As)|Cx]).
compExp(ecll(Lc,Nm,A,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  isIntrinsic(Nm,_,Op,Thrw),!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpsToVars(A,Lc,As,Brks,Opts,L,L1,D,D1,C0,C1),
  genIntrinsic(Op,RsVr,As,Thrw,Lc,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).
compExp(ecll(Lc,Nm,A,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpsToVars(A,Lc,As,Brks,Opts,L,Lx,D,Dx,C0,[iEscape(Nm,As),iRSP(RsVr)|Cx]).
compExp(xecll(Lc,Nm,A,_Tp,_ErTp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  isIntrinsic(Nm,_,Op,Thrw),!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpsToVars(A,Lc,As,Brks,Opts,L,L1,D,D1,C0,C1),
  genIntrinsic(Op,RsVr,As,Thrw,Lc,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).
compExp(xecll(Lc,Nm,A,_Tp,_ErTp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  (is_member(("$try",TryLvl),Brks) ->
   bindExpsToVars(A,Lc,As,Brks,Opts,L,Lx,D,Dx,C0,[iEscape(Nm,As),iRSX(TryLvl,RsVr)|Cx]);
   reportError("not in scope of try",[],Lc),
   D=Dx,C=Cx,L=Lx).
compExp(cll(Lc,Nm,A,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpsToVars(A,Lc,As,Brks,Opts,L,Lx,D,Dx,C0,C1),
  genDbg(Opts,Lc,C1,[iCall(Nm,As),iRSP(RsVr)|Cx]).
compExp(xcll(Lc,Nm,A,_Tp,_ErTp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  (is_member(("$try",TryLvl),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   bindExpsToVars(A,Lc,As,Brks,Opts,L,Lx,D,Dx,C0,C1),
   genDbg(Opts,Lc,C1,[iCall(Nm,As),iRSX(TryLvl,RsVr)|Cx]);
   reportError("not in scope of try",[],Lc),
   D=Dx,C=Cx,L=Lx).
compExp(ocall(Lc,O,A,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpsToVars(A,Lc,As,Brks,Opts,L,L1,D,D1,C0,C1),
  bindExpToVar(O,Lc,Brks,Opts,OpVr,L1,Lx,D1,Dx,C1,C2),
  genDbg(Opts,Lc,C2,[iOCall(OpVr,As),iRSP(RsVr)|Cx]).
compExp(xocall(Lc,O,A,_Tp,_ErTp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  (is_member(("$try",TryLvl),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   bindExpsToVars(A,Lc,As,Brks,Opts,L,L1,D,D1,C0,C1),
   bindExpToVar(O,Lc,Brks,Opts,OpVr,L1,Lx,D1,Dx,C1,C2),
   genDbg(Opts,Lc,C2,[iOCall(OpVr,As),iRSX(TryLvl,RsVr)|Cx]);
   reportError("not in scope of try",[],Lc),
   D=Dx,C=Cx,L=Lx).
compExp(clos(Lb,Ar,Free,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  bindExpToVar(Free,OLc,Brks,Opts,FrVr,L,Lx,D,Dx,C,[iClosure(lbl(Lb,Ar),RsVr,FrVr)|Cx]).
compExp(nth(Lc,Rc,Off,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpToVar(Rc,Lc,Brks,Opts,RcVr,L,Lx,D,Dx,C0,[iNth(RsVr,Off,RcVr)|Cx]).
compExp(setix(Lc,Rc,Off,Vl),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpToVar(Rc,Lc,Brks,Opts,RsVr,L,L1,D,D1,C0,C1),
  bindExpToVar(Vl,Lc,Brks,Opts,VlVr,L1,Lx,D1,Dx,C1,[iStNth(RsVr,Off,VlVr)|Cx]).
compExp(cel(Lc,Vl),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpToVar(Vl,Lc,Brks,Opts,VlVr,L,Lx,D,Dx,C0,[iCell(RsVr,VlVr)|Cx]).
compExp(get(Lc,Exp,_),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpToVar(Exp,Lc,Brks,Opts,ClVr,L,Lx,D,Dx,C0,[iGet(RsVr,ClVr)|Cx]).
compExp(sav(Lc,_Tp),OLc,RsVr,_Brks,Opts,Lx,Lx,Dx,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,[iSav(RsVr)|Cx]).
compExp(savIsSet(Lc,Sv),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  chLine(Opts,OLc,Lc,C,C0),
  bindExpToVar(Sv,Lc,Brks,Opts,SvVr,L,Lx,D,Dx,C0,[iTstSav(RsVr,SvVr)|Cx]).
compExp(savGet(Lc,Sv,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  (is_member(("$abort",Abort),Brks) ->
   bindExpToVar(Sv,Lc,Brks,Opts,SvVr,L,Lx,D,Dx,C0,[iLdSav(RsVr,Abort,SvVr)|Cx]);
   reportError("not in scope of abort",[],Lc),
   C=Cx,
   D=Dx,
   L=Lx).
compExp(savSet(Lc,Sv,Val),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpToVar(Sv,Lc,Brks,Opts,SvVr,L,L1,D,D1,C0,C1),
  bindExpToVar(Val,Lc,Brks,Opts,VlVr,L1,Lx,D1,Dx,C1,[iStSav(RsVr,SvVr,VlVr)|Cx]).
compExp(cse(Lc,Gv,Cases,Deflt,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compCase(Gv,Lc,genssa:wrapExpr(RsVr),Cases,Deflt,genssa:compExpX,Brks,Opts,L,Lx,D,Dx,C0,Cx).
compExp(unpack(Lc,Gv,Cases,Deflt,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compUnpack(Gv,Lc,genssa:wrapExpr(RsVr),Cases,Deflt,genssa:compExpX,Brks,Opts,L,Lx,D,Dx,C0,Cx).
compExp(tryX(Lc,B,idnt(E,ETp),H),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  genLbl(L,Ok,L0),
  genLbl(L0,Tr,L1),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iValof(RsVr,[iLbl(Tr,iValof(E,BC))|HC]))|Cx]),
  bindExpToVar(B,Lc,[("$try",Tr)|Brks],Opts,BdVr,L1,L2,D,D0,BC,[iResult(Ok,BdVr)]),
  defineLclVar(Lc,E,ETp,Opts,D0,D1),
  genLine(Opts,Lc,HC,H1),
  bindExpToVar(H,Lc,Brks,Opts,HVr,L2,Lx,D1,Dx,H1,[iResult(Ok,HVr)]).
compExp(ltt(Lc,idnt(Nm,Tp),Val,Exp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C1),
  defineLclVar(Lc,Nm,Tp,Opts,D,D1),
  compExp(Val,Lc,Nm,Brks,Opts,L,L1,D1,D2,C1,C2),
  compExp(Exp,Lc,RsVr,Brks,Opts,L1,Lx,D2,Dx,C2,Cx).
compExp(error(Lc,Msg),_OLc,_,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx). % no continuation after an error
compExp(thrw(Lc,E),OLc,_,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  (is_member(("$try",TryLvl),Brks) ->
   chLine(Opts,OLc,Lc,C,C0),
   bindExpToVar(E,Lc,Brks,Opts,ErVr,L,Lx,D,Dx,C0,[iResult(TryLvl,ErVr)|Cx]);
   reportError("not in scope of try",[],Lc),
   D=Dx,L=Lx,C=Cx).
compExp(cnd(Lc,Cnd,A,B),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  genLbl(L,Fl,L0),
  genLbl(L0,OkLbl,L1),
  chLine(Opts,OLc,Lc,C,[iLbl(OkLbl,iValof(RsVr,[iLbl(Fl,iBlock(AC))|BC]))|Cx]),
  compCond(Cnd,Lc,Fl,Brks,normal,Opts,L1,L2,D,D1,AC,AC1),
  bindExpToVar(A,Lc,Brks,Opts,AVr,L2,L3,D1,D2,AC1,[iResult(OkLbl,AVr)]),
  bindExpToVar(B,Lc,Brks,Opts,BVr,L3,Lx,D2,Dx,BC,[iResult(OkLbl,BVr)]).
compExp(seqD(Lc,A,B),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  bindExpToVar(A,Lc,Brks,Opts,_AVr,L,L1,D,D1,C0,C1),
  compExp(B,Lc,RsVr,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).
compExp(vlof(Lc,A,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iValof(RsVr,CA))|Cx]),
  genLbl(L,Ok,L0),
  compAction(A,Lc,[("$valof",Ok)|Brks],Opts,L0,Lx,D,Dx,CA,[]).
compExp(tsk(Lc,F,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  bindExpToVar(F,Lc,Brks,Opts,FnVr,L,Lx,D,Dx,C0,[iFiber(RsVr,FnVr)|Cx]).
compExp(susp(Lc,T,M,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpToVar(T,Lc,Brks,Opts,TVr,L,L1,D,D1,C0,C1),
  bindExpToVar(M,Lc,Brks,Opts,MVr,L1,Lx,D1,Dx,C1,C2),
  genDbg(Opts,Lc,C2,[iSuspend(TVr,MVr),iRSP(RsVr)|Cx]).
compExp(resme(Lc,T,M,_Tp),OLc,RsVr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  bindExpToVar(T,Lc,Brks,Opts,TVr,L,L1,D,D1,C0,C1),
  bindExpToVar(M,Lc,Brks,Opts,MVr,L1,Lx,D1,Dx,C1,C2),
  genDbg(Opts,Lc,C2,[iResume(TVr,MVr),iRSP(RsVr)|Cx]).
compExp(rtire(Lc,T,M,_Tp),OLc,_,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  chLine(Opts,OLc,Lc,C,C0),
  bindExpToVar(T,Lc,Brks,Opts,TVr,L,L1,D,D1,C0,C1),
  bindExpToVar(M,Lc,Brks,Opts,MVr,L1,Lx,D1,Dx,C1,C2),
  genDbg(Opts,Lc,C2,[iRetire(TVr,MVr)|Cx]).
compExp(Cond,Lc,RsVr,Brks,Opts,L,Lx,D,Dx,[iLbl(Ok,iValof(RsVr,[iLbl(Fl,iBlock(C1)),iMC(RsltVr,enum(False)),iResult(Ok,RsltVr)]))|Cx],Cx) :-
  isCond(Cond),!,
  isTrueSymb(True),
  isFalseSymb(False),
  defineTmpVar(Lc,RsltVr,type("boolean"),Opts,D,D1),
  genLbl(L,Ok,L0),
  genLbl(L0,Fl,L1),
  compCond(Cond,Lc,Fl,Brks,normal,Opts,L1,Lx,D1,Dx,C1,[iMC(RsltVr,enum(True)),iResult(Ok,RsltVr)|Cx]).
compExp(T,Lc,_,_Brks,_Opts,Lx,Lx,Dx,Dx,C,C) :-
  reportError("cannot compile %s",[ltrm(T)],Lc),
  abort.

bindExpsToVars([],_,[],_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx) :-!.
bindExpsToVars([T|Ts],Lc,[R|Rs],Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  bindExpToVar(T,Lc,Brks,Opts,R,L,L1,D,D1,C,C1),
  bindExpsToVars(Ts,Lc,Rs,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).

genIntrinsic(Op,Rslt,As,noThrow,_Lc,_Brks,_Opts,Lx,Lx,Dx,Dx,[Ins|Cx],Cx) :-!,
  Ins =.. [Op,Rslt|As].
genIntrinsic(Op,Rslt,As,throwing,_Lc,Brks,_Opts,Lx,Lx,Dx,Dx,[Ins|Cx],Cx) :-
  is_member(("$try",ErLvl),Brks),!,
  Ins =.. [Op,ErLvl,Rslt|As].
genIntrinsic(Op,_,_,throwing,Lc,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx) :-
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
compTryA(Lc,B,idnt(E,ETp),H,Brks,Opts,L,Lx,D,Dx,[iLbl(Ok,iBlock([iLbl(Tr,iValof(E,BC))|HC]))|Cx],Cx) :-
  genLbl(L,Ok,L0),
  genLbl(L0,Tr,L1),
  compAction(B,Lc,[("$try",Tr)|Brks],Opts,L1,L2,D,D0,BC,[iBreak(Ok)]),
  defineLclVar(Lc,E,ETp,Opts,D0,D1),
  genLine(Opts,Lc,HC,H1),
  compAction(H,Lc,Brks,Opts,L2,Lx,D1,Dx,H1,[iBreak(Ok)]).

isCond(cnj(_,_,_)).
isCond(cnd(_,_,_,_)).
isCond(dsj(_,_,_)).
isCond(ng(_,_)).
isCond(mtch(_,_,_)).

isTrueSymb("true").
isFalseSymb("false").

wrapAction(C,iBlock(C)).
wrapExpr(Vr,C,iValof(Vr,C)).

break(Lbl,iBreak(Lbl)).

breakOut(Lbl,iBreak(Lbl)).

result(Vr,Lbl,iResult(Lbl,Vr)).

compCase(Gv,Lc,Wrap,Cases,Deflt,Hndlr,Brks,Opts,L,Lx,D,Dx,[iLbl(Ok,OC)|Cx],Cx) :-
  genLbl(L,Df,L0),
  genLbl(L0,Ok,L1),
  tipeOf(Gv,GvTp),
  bindExpToVar(Gv,Lc,Brks,Opts,GvVr,L1,L2,D,D1,CC,C2),
  genCaseTable(Cases,Mx,Table),
  call(Wrap,[iLbl(Df,iBlock(CC))|DC],OC),
  (isIntegerType(GvTp) -> Case = iICase(GvVr,CB) ; Case = iCase(GvVr,CB)),
  compCases(Table,0,Mx,GvVr,Ok,Df,Hndlr,Brks,Opts,L2,L3,D1,D2,CB,[],C2,[Case]),
  call(Hndlr,Ok,Deflt,Lc,Brks,Opts,L3,Lx,D2,Dx,DC,[]).

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

compCases([],Ix,Mx,_GVar,_OkBrk,_Df,_Hndlr,_Brks,_Opts,Lx,Lx,D,D,Tx,Tx,Cx,Cx) :-
  Ix>=Mx.
compCases([],Ix,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,[iBreak(Dflt)|T],Tx,C,Cx) :-
  Ix1 is Ix+1,
  compCases([],Ix1,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,T,Tx,C,Cx).
compCases([(Ix,Case)|Cs],Ix,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,[iBreak(El)|T],Tx,[iLbl(El,iBlock(CC))|CCx],Cx) :-!,
  Ix1 is Ix+1,
  genLbl(L,El,L0),
  compCases(Cs,Ix1,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Opts,L0,L1,D,D1,T,Tx,CC,Cx),
  compCaseBranch(Case,GVar,OkBrk,Dflt,Hndlr,Brks,Opts,L1,Lx,D1,Dx,CCx,[]).
compCases(Cs,Ix,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,[iBreak(Dflt)|T],Tx,C,Cx) :-
  Ix1 is Ix+1,
  compCases(Cs,Ix1,Mx,GVar,OkBrk,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,T,Tx,C,Cx).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E,Lc)],GV,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-!,
  genLine(Opts,Lc,C,C0),
  compPtn(P,GV,Lc,Dflt,Brks,Opts,L,L1,D,D1,C0,C2),
  call(Hndlr,Ok,E,Lc,Brks,Opts,L1,Lx,D1,Dx,C2,Cx).
compCaseBranch(Entries,GVar,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  compCaseCond(Entries,GVar,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,C,Cx).

compCaseCond([],_GVar,_OkBrk,Dflt,_Hndlr,Lx,Lx,Dx,Dx,[iBreak(Dflt)|Cx],Cx).
compCaseCond([(P,E,Lc)],GV,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,C,Cx) :-
  compPtn(P,GV,Lc,Dflt,Brks,Opts,L,L1,D,D1,C,C1),
  call(Hndlr,Ok,E,Lc,Brks,Opts,L1,Lx,D1,Dx,C1,Cx).
compCaseCond([(P,E,Lc)|More],GV,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,
	     [iLbl(Fl,iBlock(AC))|BC],Cx) :-
  genLbl(L,Fl,L0),
  compPtn(P,GV,Lc,Fl,Brks,Opts,L0,L1,D,D1,AC,C1),
  call(Hndlr,Ok,E,Lc,Brks,Opts,L1,L2,D1,D2,C1,[]),
  compCaseCond(More,GV,Ok,Dflt,Hndlr,Brks,Opts,L2,Lx,D2,Dx,BC,Cx).

compUnpack(Gv,Lc,Wrap,Cases,Deflt,Hndlr,Brks,Opts,L,Lx,D,Dx,[iLbl(Ok,OC)|Cx],Cx) :-
  genLbl(L,Df,L0),
  genLbl(L0,Ok,L1),
  tipeOf(Gv,GvTp),
  getTypeIndex(GvTp,D,Index),
  call(Wrap,[iLbl(Df,iBlock(CC))|DC],OC),
  genUnpackTable(Cases,Index,Table),
  maxTableEntry(Table,Mx),
  bindExpToVar(Gv,Lc,Brks,Opts,GvVr,L1,L2,D,D1,CG,[iIxCase(GvVr,CB)]),
  compCases(Table,0,Mx,GvVr,Ok,Df,Hndlr,Brks,Opts,L2,L3,D1,D2,CB,[],CC,CG),
  call(Hndlr,Ok,Deflt,Lc,Brks,Opts,L3,Lx,D2,Dx,DC,[]).

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
next(Nx,_Lc,R,_Brks,_Opts,Lx,Lx,Dx,Dx,[iMv(Nx,R)|Cx],Cx).

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
