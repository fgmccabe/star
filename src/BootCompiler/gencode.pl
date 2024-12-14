:- module(gencode,[genCode/4]).

:- use_module(misc).
:- use_module(types).
:- use_module(lterms).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).
:- use_module(assem).
:- use_module(errors).
:- use_module(gensig).
:- use_module(location).
:- use_module(peephole).
:- use_module(intrinsics).
:- use_module(escapes).

genCode(PkgDecls,mdule(Pkg,Imports,Decls,LDecls,Defs),Opts,Text) :-
  encPkg(Pkg,PT),
  initDict(D0),
  genImports(Imports,ImpTpl),
  rfold(PkgDecls,gencode:defGlbl(Opts),D0,D2),
  genDefs(Defs,Opts,D2,C,[]),
  mkTpl(C,Cdes),
  map(Decls,gensig:formatDecl,Ds),
  mkTpl(Ds,DTpl),
  map(LDecls,gensig:formatDecl,LDs),
  mkTpl(LDs,LDTpl),
  opcodeHash(OpSig),
  mkTpl([PT,intgr(OpSig),ImpTpl,DTpl,LDTpl,Cdes],Tp),
  encode(Tp,Txt),
  encode(strg(Txt),Text).

genImports(Imps,ImpTpl) :-
  map(Imps,gencode:genImport,Els),
  mkTpl(Els,ImpTpl).

genImport(importPk(_,_,Pkg),PkgTrm) :-
  encPkg(Pkg,PkgTrm).

defGlbl(Opts,varDec(_,Nm,Tp),D,Dx) :-!,
  defineGlbVar(Nm,Tp,Opts,D,Dx).
defGlbl(_,_,D,D).

genDefs(Defs,Opts,D,O,Ox) :-
  rfold(Defs,gencode:genDef(D,Opts),Ox,O).

genDef(D,Opts,fnDef(Lc,Nm,H,Tp,Args,Value),O,[Cde|O]) :-
  genFun(D,Opts,Lc,Nm,H,Tp,Args,Value,Cde).
genDef(D,Opts,glbDef(Lc,Nm,Tp,Value),O,[Cde|O]) :-
  genGlb(D,Opts,Lc,Nm,Tp,Value,Cde).
genDef(_,_,lblDef(_,Lbl,Tp,Ix),O,[LblTrm|O]) :-
  encType(Tp,Sig),
  assem(struct(Lbl,strg(Sig),Ix),LblTrm).
genDef(_,_,typDef(_,Tp,Rl,IxMap),O,[TpTrm|O]) :-
  assem(tipe(Tp,Rl,IxMap),TpTrm).

genFun(D,Opts,Lc,Nm,H,Tp,Args,Value,CdTrm) :-
  (is_member(traceGenCode,Opts) -> dispRuleSet(fnDef(Lc,Nm,H,Tp,Args,Value)) ; true),
  toLtipe(Tp,LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L1),
  funResType(Tp,ResTp),
  toLtipe(ResTp,LRTp),
  nearlyFlatSig(LRTp,BlkTp),
  genDbg(Opts,C,[iEntry|C0]),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(BlkTp,FC))|CA]),
  compArgs(Args,Lc,0,Abrt,[],Opts,L1,L2,D,D1,FC,FC0,some(0),Stk0),
  compExp(Value,Lc,[("$abort",gencode:breakOut,Abrt,none)],last,
	  Opts,L2,L3,D1,D3,FC0,FC1,Stk0,Stk1),
  genRet(Opts,FC1,[],Stk1,_),
  compAbort(Lc,strg("def failed"),[],Opts,L3,_,D3,Dx,CA,[iHalt(10)],Stk0,_),
  getLsMap(Dx,LsMap),
  GenFunc = func(Nm,H,Sig,LsMap,C),
  (is_member(traceGenCode,Opts) -> dispCode(GenFunc);true ),
  peepOptimize(GenFunc,PFunc),
  (is_member(showGenCode,Opts) -> dispCode(PFunc);true ),
  assem(PFunc,CdTrm).

genGlb(D,Opts,Lc,Nm,Tp,Value,Cd) :-
  toLtipe(funType(tplType([]),Tp),LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L1),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(strg(Sig),FC))|CA]),
  compExp(Value,Lc,[],notLast,Opts,L1,L2,D,D1,FC,[iTG(Nm),iRet|FC1],some(0),Stk0),
  compAbort(Lc,strg("def failed"),[],Opts,L2,_L3,D1,Dx,CA,[iHalt(10)],some(0),_),
  genRet(Opts,FC1,[],Stk0,_),
  genDbg(Opts,C,[iEntry|C0]),
  getLsMap(Dx,LsMap),
  GenGlb = func(lbl(Nm,0),hard,Sig,LsMap,C),
  (is_member(traceGenCode,Opts) -> dispCode(GenGlb);true ),
  peepOptimize(GenGlb,PGlb),
  (is_member(showGenCode,Opts) -> dispCode(PGlb);true ),
  assem(PGlb,Cd).

genRet(Opts,C,Cx,_Stk,none) :-
  genDbg(Opts,C,[iRet|Cx]).

initDict(scope([])).

getLsMap(scope(Dx),Vrs) :-
  extractMap(Dx,Vrs),!.

extractMap([],[]).
extractMap([(Nm,Spec,l(_))|Dx],[(Nm,Spec)|LsMap]) :-
  extractMap(Dx,LsMap).
extractMap([_|Dx],LsMap) :-
  extractMap(Dx,LsMap).

buildArgs([],_,_,_,D,D) :-!.
buildArgs([A|R],Ix,Lc,Opts,D,Dx) :-
  buildArg(A,Ix,Lc,Opts,D,D0),
  Ix1 is Ix+1,
  buildArgs(R,Ix1,Lc,Opts,D0,Dx).

buildArg(idnt(Nm,Tp),Ix,Lc,Opts,D,Dx) :-!,
  defineVar(Lc,Nm,Tp,Opts,a(Ix),D,Dx).
buildArg(_,_,_,_,D,D).

lclVar(Nm,T,Wh,scope(Vrs)) :-
  is_member((Nm,T,Wh),Vrs),!.

defineLclVar(Lc,Nm,Tp,Opts,D,Dx,Cx,Cx) :-
  defineVar(Lc,Nm,Tp,Opts,l(Nm),D,Dx).

defineVar(_Lc,Nm,Tp,_Opts,Mode,scope(Vrs),scope(NVrs)) :-
  toLtipe(Tp,T),
  encLtp(T,Sig),
  add_mem((Nm,strg(Sig),Mode),Vrs,NVrs).

defineTmpVar(Lc,TmpNm,Tp,Opts,D,Dx,C,Cx) :-
  genTmpVar(D,TmpNm),
  defineLclVar(Lc,TmpNm,Tp,Opts,D,Dx,C,Cx).

genDebug(Opts,Debug,[Debug|Cx],Cx) :-
  is_member(debugging,Opts),!.
genDebug(_,_,Cx,Cx).

defineGlbVar(Nm,Tp,_Opts,scope(Vrs),scope([(Nm,strg(Sig),g(Nm))|Vrs])) :-
  toLtipe(Tp,T),
  encLtp(T,Sig).

genLbl(Lbs,Lb,[Lb|Lbs]) :-
  length(Lbs,N),
  swritef(Lb,"_L%d",[N]).

genTmpVar(scope(Vrs),Nm) :-
  length(Vrs,Mx),
  swritef(Nm,"_Φ%d",[Mx]).

compExpCase(Term,Lc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compExp(Term,Lc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

compIdExp(idnt(Nm,_),Lc,Last,Opts,Lx,Lx,Dx,Dx,C,Cx,Stk,Stkx) :-
  (lclVar(Nm,_T,V,Dx) ->
   compVar(V,Last,Opts,C,Cx,Stk,Stkx) ;
   reportError("cannot locate variable %s",[id(Nm)],Lc),
   abort).

compVar(a(A),Last,Opts,[iLdA(A)|C],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stka),
  genLastReturn(Last,Opts,C,Cx,Stka,Stkx).
compVar(l(X),Last,Opts,[iLdL(X)|C],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stka),
  genLastReturn(Last,Opts,C,Cx,Stka,Stkx).
compVar(g(GlbNm),Last,Opts,[iLdG(GlbNm)|C],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stka),
  genLastReturn(Last,Opts,C,Cx,Stka,Stkx).

/* Compile actions */
compAction(nop(_),_Lc,_Brks,_Return,_Next,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stkx,Stkx) :-!.
compAction(seq(Lc,A,B),OLc,Brks,Return,Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compAction(A,Lc,Brks,notLast,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk0),
  verify(gencode:consistentStack(Stk,Stk0),"actions not permitted to leave stuff on stack"),
  compAction(B,Lc,Brks,Return,Next,Opts,L1,Lx,D1,Dx,C1,Cx,Stk,Stkx).
compAction(lbld(Lc,Lb,A),OLc,Brks,Return,_Next,Opts,L,Lx,D,Dx,[iLbl(BrkLb,iBlock(FlatTp,BC))|Cx],Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,BC,C0),
  genLbl(L,BrkLb,L1),
  flatBlockSig(FlatTp),
  compAction(A,Lc,[(Lb,gencode:breakOut,BrkLb,Stk)|Brks],Return,notLast,Opts,L1,Lx,D,Dx,C0,[iBreak(BrkLb)],Stk,Stk0),
  verify(gencode:consistentStack(Stk,Stk0),"labeled actions not permitted to return values").
compAction(brk(Lc,Nm),OLc,Brks,_Return,_Next,Opts,Lx,Lx,Dx,Dx,C,Cx,_Stk,none) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  (is_member((Nm,Brkr,Lbl,_Stkx),Brks) ->
   call(Brkr,Lbl,C0,Cx);
   reportError("not in scope of break label %s",[ss(Nm)],Lc),
   C0=Cx).
compAction(perf(_,rais(Lc,T,E)),OLc,Brks,_Return,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(T,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iThrow|Cx],Stka,_Stka).
compAction(perf(Lc,Cll),OLc,Brks,_,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(Cll,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C0,C1,Stk,Stk0),
  resetStack(Stk,Stk0,C1,Cx).
compAction(perf(Lc,Cll),OLc,Brks,Last,last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(Cll,Lc,Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(mtch(Lc,P,E),OLc,Brks,_Return,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatTp,[iLbl(Abrt,iBlock(FlatTp,CB))|Ca]))|Cx]),
  genLbl(L,Ok,L1),
  genLbl(L1,Abrt,L2),
  flatBlockSig(FlatTp),
  compExp(E,Lc,Brks,notLast,Opts,L2,L3,D,D1,CB,C1,Stk,Stk1),
  compPtn(P,Lc,Abrt,Brks,Opts,L3,L4,D1,D2,C1,C2,Stk1,Stka),
  resetStack(Stk,Stka,C2,[iBreak(Ok)]),
  compAbort(Lc,strg("match fail"),Brks,Opts,L4,Lx,D2,Dx,Ca,[],Stk,_).
compAction(defn(Lc,idnt(Nm,Tp),E),OLc,Brks,_Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,Brks,notLast,Opts,L,Lx,D,D1,C0,[iStL(Nm)|C1],Stk,Stk0),
  defineLclVar(Lc,Nm,Tp,Opts,D1,Dx,C1,Cx),
  dropStk(Stk0,1,Stk).
compAction(setix(Lc,Trm,Off,Vl),OLc,Brks,_Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Vl,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(Trm,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iStNth(Off)|Cx],Stka,Stkb),
  dropStk(Stkb,2,Stk).
compAction(asgn(Lc,Cll,Exp),OLc,Brks,_Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(Cll,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iAssign|Cx],Stka,Stkb),
  dropStk(Stkb,2,Stk).
compAction(case(Lc,T,Cases,Deflt),OLc,Brks,Last,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  flatBlockSig(FlatSig),
  compCase(T,Lc,FlatSig,Cases,Deflt,gencode:compAct(notLast),Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx),
  verify(gencode:consistentStack(Stk,Stkx),"case action not permitted to leave stuff on stack").
compAction(case(Lc,T,Cases,Deflt),OLc,Brks,Last,last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  nearlyFlatSig(ptrTipe,NFlatSig),
  compCase(T,Lc,NFlatSig,Cases,Deflt,gencode:compAct(last),Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(whle(Lc,G,B),OLc,Brks,_Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Done,iBlock(FlatTp,[iLbl(Lp,iBlock(FlatTp,LC))]))|Cx]),!,
  flatBlockSig(FlatTp),
  genLbl(L,Lp,L1),
  genLbl(L1,Done,L2),
  compCond(G,Lc,Done,Brks,normal,Opts,L2,L3,D,D1,LC,LC1,Stk,Stka),
  verify(gencode:consistentStack(Stk,Stka),"condition not permitted to leave stuff on stack"),
  compAction(B,Lc,Brks,notLast,notLast,Opts,L3,Lx,D1,Dx,LC1,[iLoop(Lp)],Stk,Stkb),
  verify(gencode:consistentStack(Stk,Stkb),"while action not permitted to leave stuff on stack").
compAction(ltt(Lc,idnt(Nm,Tp),Val,Act),OLc,Brks,Last,Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  defineLclVar(Lc,Nm,Tp,Opts,D,D1,C0,[iStV(Nm)|C1]),
  compExp(Val,Lc,Brks,notLast,Opts,L,L1,D1,D2,C1,[iStL(Nm)|C2],Stk,Stk1),
  verify(gencode:bumpStk(Stk,Stk1),"expecting a single bump in stack"),
  compAction(Act,Lc,Brks,Last,Next,Opts,L1,Lx,D2,Dx,C2,Cx,Stk,Stkx).
compAction(iftte(Lc,G,A,B),OLc,Brks,Last,Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Thn,iBlock(FlatTp,[iLbl(Fl,iBlock(FlatTp,AC))|BC]))|Cx]),
  genLbl(L,Fl,L0),
  genLbl(L0,Thn,L1),
  flatBlockSig(FlatTp),
  compCond(G,Lc,Fl,Brks,normal,Opts,L1,L2,D,D1,AC,AC1,Stk,Stk0),
  verify(gencode:consistentStack(Stk,Stk0),"conditions should not increase stack"),
  compAction(A,Lc,Brks,Last,Next,Opts,L2,L3,D1,D2,AC1,[iBreak(Thn)],Stk,Stka),
  compAction(B,Lc,Brks,Last,Next,Opts,L3,Lx,D2,Dx,BC,[iBreak(Thn)],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"conditional action").
compAction(try(Lc,B,T,E,H),OLc,Brks,Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  compTry(Lc,B,tplTipe([]),T,E,H,OLc,gencode:compAct(notLast),Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stka),
  verify(gencode:consistentStack(Stk,Stka),"try action not permitted to leave stuff on stack").
compAction(vls(Lc,E),OLc,Brks,Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  (is_member(("$valof",Brker,Ok,_),Brks) ->
     chLine(Opts,OLc,Lc,C,C0),
     compExp(E,Lc,Brks,Last,Opts,L,Lx,D,Dx,C0,C1x,Stk,_),
     call(Brker,Ok,C1x,Cx);
   reportError("not in scope of valof",[],Lc)).
compAction(error(Lc,Msg),_OLc,Brks,_Last,_Next,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,_).
compAction(A,Lc,_Brks,_Last,_Next,_Opts,Lx,Lx,Dx,Dx,C,C,Stk,Stk) :-
  reportError("cannot compile action %s",[lact(A)],Lc),
  abort.

compAct(Next,A,Lc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compAction(A,Lc,Brks,Last,Next,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

compTry(Lc,B,ResTp,idnt(TV,Tp),idnt(E,ETp),H,OLc,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(BlkTp,[iTry(TryTp,[iStL(TV)|BC])|HC]))|Cz]),
  genLbl(L,Ok,L1),
  nearlyFlatSig(ResTp,BlkTp),
  toLtipe(ETp,ETipe),
  blockSig([ETipe],ResTp,TryTp),
  defineLclVar(Lc,TV,Tp,Opts,D,D1,BC,B1),
  replace(Brks,("$valof",_,Lbl,VStk),("$valof",gencode:tryEnder(TV),Lbl,VStk),NBrks),
  call(Hndlr,B,Lc,NBrks,notLast,Opts,L1,L2,D1,D2,B1,[iLdL(TV),iEndTry(Ok)],Stk,Stka),
  genLine(Opts,Lc,HC,H1),
  defineLclVar(Lc,E,ETp,Opts,D2,D4,H1,[iStL(E)|H2]),
  call(Hndlr,H,Lc,Brks,Last,Opts,L2,Lx,D4,Dx,H2,[iBreak(Ok)],Stk,Stkb),
  reconcileStack(Stka,Stkb,Stkx,Cz,Cx),!.

tryEnder(TV,Ok,[iLdL(TV),iEndTry(Ok)|Cx],Cx).
breakOut(Ok,[iBreak(Ok)|Cx],Cx).

blockSig(ArgTps,ResTp,strg(Sig)) :-
  mkFnTipe(ArgTps,ResTp,BlkTp),
  encLtp(BlkTp,Sig).

caseBlockSig(ResTp,Sig) :-
  blockSig([ptrTipe],ResTp,Sig). % consume at least one existing el from stack

flatBlockSig(Sig) :-
  blockSig([],tplTipe([]),Sig).

nearlyFlatSig(Tp,Sig) :-
  blockSig([],Tp,Sig).

frameSig(Stk,strg(Sig)) :-
  mkTplTipe(Stk,FrTp),
  encLtp(FrTp,Sig).

frameIns(some(Stk),[iFrame(Sig)|Cx],Cx) :-
  frameSig(Stk,Sig).
frameIns(none,Cx,Cx).

stkLvl(some(Lvl),Lvl).

resetStack(Stk,Stk,C,C) :-!.
resetStack(_Stk,none,C,C) :- !.
resetStack(Stk,Stk0,[iDrop|C],C) :-
  dropStk(Stk0,1,Stk),!.
resetStack(some(Lvl),_,[iRst(Lvl)|Cx],Cx).

reconcileStack(_,_,none,C,C) :-!.
reconcileStack(Stk,Stk,_,C,C) :-!.
reconcileStack(some(Stki),some(Stk),_,[iRst(Stk)|C],C) :-
  Stki>Stk,!.

consistentStack(_Stk,none) :-!.
consistentStack(none,_Stk) :-!.
consistentStack(Stk,Stk).

compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-
  locTerm(Lc,LT),
  compExps([LT,Msg],Lc,Brks,Opts,L,Lx,D,Dx,C,[iAbort|Cx],Stk,_Stkx).

argGetter(Ix,_Lc,_Opts,Lx,Lx,D,D,[iLdA(Ix)|Cx],Cx,Stk,Stk1) :-
  bumpStk(Stk,Stk1).

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

genDbg(Opts,[iDBug|Cx],Cx) :-
  is_member(debugging,Opts),!.
genDbg(_,Cx,Cx).

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
  defineVar(Lc,Nm,Tp,Opts,a(Ix),D,D1),
  Ix1 is Ix+1,
  compArgs(Args,Lc,Ix1,Fail,Brks,Opts,L,Lx,D1,Dx,C,Cx,Stk,Stkx).
compArgs([A|R],Lc,Ix,Fail,Brks,Opts,L,Lx,D,Dx,[iLdA(Ix)|C],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stk0),
  compPtn(A,Lc,Fail,Brks,Opts,L,L1,D,D1,C,C1,Stk0,Stk1),
  verify(Stk1=Stk,"Argument pattern not allowed to adjust stack"),
  Ix1 is Ix+1,
  compArgs(R,Lc,Ix1,Fail,Brks,Opts,L1,Lx,D1,Dx,C1,Cx,Stk1,Stkx).

% compile a pattern. Patterns form a reduced subset of possible expression forms
compPtn(voyd,_,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iDrop|Cx],Cx,Stk,Stkx) :-!,
  dropStk(Stk,1,Stkx).
compPtn(ann(_),_,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iDrop|Cx],Cx,Stk,Stkx) :-!,
  dropStk(Stk,1,Stkx).
compPtn(Lit,_Lc,Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iCLit(Lit,Fail)|Cx],Cx,Stk,Stkx) :-
  isLiteral(Lit),!,
  dropStk(Stk,1,Stkx).
compPtn(idnt(Nm,Tp),Lc,_Fail,_Brks,Opts,Lx,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  defineLclVar(Lc,Nm,Tp,Opts,D,Dx,C,[iStL(Nm)|Cx]),
  dropStk(Stk,1,Stkx).
compPtn(ctpl(St,Args),Lc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  tipeOf(ctpl(St,Args),Tp),
  defineTmpVar(Lc,TmpNm,Tp,Opts,D,D1,C,[iTL(TmpNm),iCLbl(St,Fail)|C1]),
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
compPtnArgs([],_Lc,_PVar,_Ix,_Fail,_Brks,Last,Opts,Lx,Lx,Dx,Dx,C,Cx,Stk,Stkx) :-!,
  genLastReturn(Last,Opts,C,Cx,Stk,Stkx).
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
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatTp,[iLbl(Fl,iBlock(FlatTp,AC))|BC]))|Cx]),
  flatBlockSig(FlatTp),
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
compCond(cnd(Lc,T,A,B),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  flatBlockSig(FlatTp),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatTp,[iLbl(El,iBlock(FlatTp,CA))|CB]))|Cx]),
  genLbl(L,El,L0),
  compCond(T,Lc,El,Brks,normal,Opts,L0,L1,D,D1,CA,C0,Stk,Stk0),
  genLbl(L1,Ok,L2),
  compCond(A,Lc,Fail,Brks,normal,Opts,L2,L3,D1,D2,C0,[iBreak(Ok)],Stk0,Stka),
  compCond(B,Lc,Fail,Brks,normal,Opts,L3,Lx,D2,Dx,CB,[iBreak(Ok)],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"conditional").
compCond(cnd(Lc,T,A,B),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compCond(cnd(Lc,T,B,A),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compCond(mtch(Lc,P,E),OLc,Fail,Brks,normal,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,Brks,notLast,Opts,L,L3,D,D1,C0,C1,Stk,Stka),
  compPtn(P,Lc,Fail,Brks,Opts,L3,Lx,D1,Dx,C1,Cx,Stka,Stkb),
  verify(Stkb=Stk,"match condition should not affect stack").
compCond(mtch(Lc,P,E),OLc,Fail,Brks,negated,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  flatBlockSig(FlatSig),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatSig,CB))|Cx]),
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
compExp(voyd,_,_Brks,Last,Opts,Lx,Lx,Dx,Dx,[iLdV|C],Cx,Stk,Stkx) :-!,
  bumpStk(Stk,Stka),
  genLastReturn(Last,Opts,C,Cx,Stka,Stkx).
compExp(ann(_),_,_Brks,Last,Opts,Lx,Lx,Dx,Dx,[iLdV|C],Cx,Stk,Stkx) :-!,
  bumpStk(Stk,Stka),
  genLastReturn(Last,Opts,C,Cx,Stka,Stkx).
compExp(Trm,_Lc,_Brks,Last,Opts,Lx,Lx,Dx,Dx,[iLdC(Trm)|C],Cx,Stk,Stkx) :-
  isLiteral(Trm),!,
  bumpStk(Stk,Stka),
  genLastReturn(Last,Opts,C,Cx,Stka,Stkx).
compExp(idnt(Nm,Tp),Lc,_Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compIdExp(idnt(Nm,Tp),Lc,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compExp(ctpl(St,A),Lc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compExps(A,Lc,Brks,Opts,L,Lx,D,Dx,C,[iAlloc(St)|C1],Stk,_Stka),
  bumpStk(Stk,Stka),
  frameIns(Stka,C1,C2),
  genLastReturn(Last,Opts,C2,Cx,Stka,Stkx).
compExp(ecll(Lc,Nm,A,_Tp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  isIntrinsic(Nm,_,Op),!,
  chLine(Opts,OLc,Lc,C,C0),
  compExps(A,Lc,Brks,Opts,L,Lx,D,Dx,C0,[Op|C1],Stk,_Stka),
  bumpStk(Stk,Stka),
  frameIns(Stka,C1,C2),
  genLastReturn(Last,Opts,C2,Cx,Stka,Stkx).
compExp(ecll(Lc,Nm,A,_Tp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExps(A,Lc,Brks,Opts,L,Lx,D,Dx,C0,[iEscape(Nm)|C1],Stk,_Stka),
  bumpStk(Stk,Stka),
  frameIns(Stka,C1,C2),
  genLastReturn(Last,Opts,C2,Cx,Stka,Stkx).
compExp(cll(Lc,Nm,A,_Tp),OLc,Brks,last,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExps(A,Lc,Brks,Opts,L,Lx,D,Dx,C0,[iTCall(Nm)|Cx],Stk,_).
compExp(cll(Lc,Nm,A,_Tp),OLc,Brks,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExps(A,Lc,Brks,Opts,L,Lx,D,Dx,C0,[iCall(Nm)|C1],Stk,_Stka),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C1,Cx).
compExp(ocall(Lc,O,A,_Tp),OLc,Brks,last,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  length(A,Ar),
  Arity is Ar+1,
  compExps(A,Lc,Brks,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(O,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iTOCall(Arity)|Cx],Stka,_).
compExp(ocall(Lc,O,A,_Tp),OLc,Brks,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  length(A,Ar),
  Arity is Ar+1,
  compExps(A,Lc,Brks,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(O,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iOCall(Arity)|C2],Stka,_),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C2,Cx).
compExp(clos(Lb,Ar,Free),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compExp(Free,OLc,Brks,notLast,Opts,L,Lx,D,Dx,C,[iClosure(lbl(Lb,Ar))|C1],Stk,_Stka),
  bumpStk(Stk,Stka),
  frameIns(Stka,C1,C2),
  genLastReturn(Last,Opts,C2,Cx,Stka,Stkx).
compExp(nth(Lc,Exp,Off,_),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C0,[iNth(Off)|C1],Stk,_Stka),
  bumpStk(Stk,Stka),
  genLastReturn(Last,Opts,C1,Cx,Stka,Stkx).
compExp(setix(Lc,Exp,Off,Vl),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Vl,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk0),
  compExp(Exp,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iStNth(Off)|C2],Stk0,_Stkx),
  genLastReturn(Last,Opts,C2,Cx,Stk,Stkx).
compExp(cel(Lc,Exp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C0,[iCell|C1],Stk,Stka),
  frameIns(Stka,C1,C2),
  genLastReturn(Last,Opts,C2,Cx,Stka,Stkx).
compExp(get(Lc,Exp),_,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compExp(Exp,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C,[iGet|C1],Stk,Stka),
  genLastReturn(Last,Opts,C1,Cx,Stka,Stkx).
compExp(set(Lc,Cl,Val),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Val,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  compExp(Cl,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iSet|C1],Stk1,_Stka),
  genLastReturn(Last,Opts,C1,Cx,Stk,Stkx).
compExp(sav(Lc,_),OLc,_Brks,Last,Opts,Lx,Lx,Dx,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,[iSav|C1]),!,
  bumpStk(Stk,Stka),
  frameIns(Stka,C1,C2),
  genLastReturn(Last,Opts,C2,Cx,Stka,Stkx).
compExp(savIsSet(Lc,Sv),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Sv,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C0,[iTstSav|C1],Stk,Stka),
  genLastReturn(Last,Opts,C1,Cx,Stka,Stkx).
compExp(savGet(Lc,Sv,_Tp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  (is_member(("$abort",_Brker,Abort,_),Brks) ->
   compExp(Sv,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C0,[iLdSav(Abort)|C2],Stk,Stka),
   genLastReturn(Last,Opts,C2,Cx,Stka,Stkx);
   reportError("not in scope of abort",[],Lc),
   Stkx=none,
   C=Cx,
   D=Dx,
   L=Lx).
compExp(savSet(Lc,Sv,Val),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Val,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  compExp(Sv,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iTSav|C2],Stk1,_Stka),
  genLastReturn(Last,Opts,C2,Cx,Stk,Stkx).
compExp(case(Lc,T,Cases,Deflt),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  nearlyFlatSig(ptrTipe,CaseBlkTp),
  compCase(T,Lc,CaseBlkTp,Cases,Deflt,gencode:compExp,Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compExp(try(Lc,B,T,E,H),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  tipeOf(B,BTp),
  toLtipe(BTp,ResTp),
  compTry(Lc,B,ResTp,T,E,H,OLc,gencode:compExp,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compExp(ltt(Lc,idnt(Nm,Tp),Val,Exp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  defineLclVar(Lc,Nm,Tp,Opts,D,D1,C0,[iStV(Nm)|C1]),
  compExp(Val,Lc,Brks,notLast,Opts,L,L1,D1,D2,C1,[iStL(Nm)|C2],Stk,Stk1),
  verify(gencode:bumpStk(Stk,Stk1),"expecting a single bump in stack"),
  compExp(Exp,Lc,Brks,Last,Opts,L1,Lx,D2,Dx,C2,Cx,Stk,Stkx).
compExp(error(Lc,Msg),_OLc,Brks,_Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx). % no continuation after an error
compExp(rais(Lc,T,E),OLc,Brks,_Last,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(E,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  compExp(T,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iThrow|Cx],Stk1,_).
compExp(cnd(Lc,Cnd,A,B),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(CondSig,[iLbl(Fl,iBlock(FlatTp,AC))|BC]))|Cx]),
  genLbl(L,Fl,L0),
  genLbl(L0,Ok,L1),
  flatBlockSig(FlatTp),
  nearlyFlatSig(ptrTipe,CondSig),
  compCond(Cnd,Lc,Fl,Brks,normal,Opts,L1,L2,D,D1,AC,AC1,Stk,Stk0),
  verify(Stk=Stk0,"conditions should not increase stack"),
  compExp(A,Lc,Brks,Last,Opts,L2,L3,D1,D2,AC1,[iBreak(Ok)],Stk,Stka),
  compExp(B,Lc,Brks,Last,Opts,L3,Lx,D2,Dx,BC,[iBreak(Ok)],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"conditional expression stack").
compExp(seqD(Lc,A,B),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(A,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  resetStack(Stk,Stk1,C1,C2),
  compExp(B,Lc,Brks,Last,Opts,L1,Lx,D1,Dx,C2,Cx,Stk,Stkx).
compExp(vlof(Lc,A),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  nearlyFlatSig(ptrTipe,BlkTp),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(BlkTp,CA))|Cx]),
  genLbl(L,Ok,L0),
  compAction(A,Lc,[("$valof",gencode:breakOut,Ok,Stkx)|Brks],Last,last,Opts,L0,Lx,D,Dx,CA,[],Stk,_Stkx),
  bumpStk(Stk,Stkx).
compExp(tsk(Lc,F),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,[iFiber|C0]),!,
  compExp(F,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C0,C1,Stk,Stka),
  genLastReturn(Last,Opts,C1,Cx,Stka,Stkx).
compExp(Cond,Lc,Brks,Last,Opts,L,Lx,D,Dx,[iLbl(Ok,iBlock(BlkTp,[iLbl(Fl,iBlock(FlatTp,C1)),iLdC(enum(False)),iBreak(Ok)]))|Cx],Cx,Stk,Stkx) :-
  isCond(Cond),!,
  isTrueSymb(True),
  isFalseSymb(False),
  nearlyFlatSig(blTipe,BlkTp),
  flatBlockSig(FlatTp),
  genLbl(L,Ok,L0),
  genLbl(L0,Fl,L1),
  compCond(Cond,Lc,Fl,Brks,normal,Opts,L1,Lx,D,Dx,C1,[iLdC(enum(True)),iBreak(Ok)|C2],Stk,_Stka),
  bumpStk(Stk,Stka),
  genLastReturn(Last,Opts,C2,Cx,Stka,Stkx).
compExp(T,Lc,_Brks,_Last,_Opts,Lx,Lx,Dx,Dx,C,C,Stk,Stk) :-
  reportError("cannot compile %s",[ltrm(T)],Lc),
  abort.

/* Argument terms are generated in reverse order*/
compExps([],_,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stk,Stk) :-!.
compExps([T|Ts],Lc,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compExps(Ts,Lc,Brks,Opts,L,L1,D,D1,C,C0,Stk,Stk0),
  compExp(T,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C0,Cx,Stk0,Stkx).

isCond(cnj(_,_,_)).
isCond(cnd(_,_,_,_)).
isCond(dsj(_,_,_)).
isCond(ng(_,_)).
isCond(mtch(_,_,_)).

isTrueSymb("true").
isFalseSymb("false").

compCase(Gv,Lc,OkBlkTp,Cases,Deflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,[iLbl(Ok,iBlock(OkBlkTp,C0))|Cx],Cx,Stk,Stkx) :-
  genLbl(L,Df,L0),
  genLbl(L0,Ok,L1),
  caseBlockSig(tplTipe([]),CaseBlkTp),
  compGvExp(Gv,GVar,Lc,Brks,Opts,L1,L2,D,D1,C0,[iLbl(Df,iBlock(CaseBlkTp,CC))|DC],Stk,_Stk1),
  genCaseTable(Cases,Mx,Table),
  compCases(Table,0,Mx,GVar,CaseBlkTp,Ok,Df,Hndlr,Brks,Last,Opts,L2,L3,D1,D2,CB,[],CC,[iCase(Mx)|CB],Stk,Stka),
  call(Hndlr,Deflt,Lc,Brks,Last,Opts,L3,Lx,D2,Dx,DC,[iBreak(Ok)],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"case exp").

compGvExp(idnt(Nm,Tp),idnt(Nm,Tp),Lc,_Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compIdExp(idnt(Nm,Tp),Lc,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compGvExp(Exp,idnt(GVar,Tp),Lc,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  tipeOf(Exp,Tp),
  compExp(Exp,Lc,Brks,notLast,Opts,L,Lx,D,D1,C,C1,Stk,Stkx),
  defineTmpVar(Lc,GVar,Tp,Opts,D1,Dx,C1,[iTL(GVar)|Cx]).

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

compCases([],Ix,Mx,_GVar,_BlkTp,_Ok,_Df,_Hndlr,_Brks,_Last,_Opts,Lx,Lx,D,D,Tx,Tx,Cx,Cx,_Stk,none) :-
  Ix>=Mx.
compCases([],Ix,Mx,GVar,BlkTp,Ok,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,[iBreak(Dflt)|T],Tx,C,Cx,Stk,Stkx) :-
  Ix1 is Ix+1,
  compCases([],Ix1,Mx,GVar,BlkTp,Ok,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,T,Tx,C,Cx,Stk,Stkx).
compCases([(Ix,Case)|Cs],Ix,Mx,GVar,BlkTp,Ok,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,[iBreak(CL)|T],Tx,[iLbl(CL,iBlock(BlkTp,CC))|CCx],Cx,Stk,Stkx) :-!,
  Ix1 is Ix+1,
  genLbl(L,CL,L0),
  compCases(Cs,Ix1,Mx,GVar,BlkTp,Ok,Dflt,Hndlr,Brks,Last,Opts,L0,L1,D,D1,T,Tx,CC,Cx,Stk,Stk2),
  compCaseBranch(Case,GVar,BlkTp,Ok,Dflt,Hndlr,Brks,Last,Opts,L1,Lx,D1,Dx,CCx,[],Stk,Stk1),
  mergeStkLvl(Stk1,Stk2,Stkx,"case branch").
compCases(Cs,Ix,Mx,GVar,BlkTp,Ok,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,[iBreak(Dflt)|T],Tx,C,Cx,Stk,Stkx) :-
  Ix1 is Ix+1,
  compCases(Cs,Ix1,Mx,GVar,BlkTp,Ok,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,T,Tx,C,Cx,Stk,Stkx).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E,Lc)],GV,_BlkTp,Ok,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  genLine(Opts,Lc,C,C0),
  compIdExp(GV,Lc,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk0),
  compPtn(P,Lc,Dflt,Brks,Opts,L1,L2,D1,D2,C1,C2,Stk0,Stk1),
  verify(Stk=Stk1,"patterns not allowed to modify stack"),
  call(Hndlr,E,Lc,Brks,Last,Opts,L2,Lx,D2,Dx,C2,[iBreak(Ok)|Cx],Stk1,Stkx).
compCaseBranch(Entries,GVar,_BlkTp,Ok,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compCaseCond(Entries,GVar,Ok,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

compCaseCond([],_GVar,_Ok,Dflt,_Hndlr,_Last,Lx,Lx,Dx,Dx,[iBreak(Dflt)|Cx],Cx,_Stk,none).
compCaseCond([(P,E,Lc)],GV,Ok,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compIdExp(GV,Lc,notLast,Opts,L,L1,D,D1,C,C1,Stk,Stk0),
  compPtn(P,Lc,Dflt,Brks,Opts,L1,L2,D1,D2,C1,C2,Stk0,Stk1),
  verify(Stk=Stk1,"patterns not allowed to modify stack"),
  call(Hndlr,E,Lc,Brks,Last,Opts,L2,Lx,D2,Dx,C2,[iBreak(Ok)|Cx],Stk,Stkx).
compCaseCond([(P,E,Lc)|More],GV,Ok,Dflt,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,[iLbl(Fl,iBlock(FlatTp,AC))|BC],[],Stk,Stkx) :-
  flatBlockSig(FlatTp),
  genLbl(L,Fl,L0),
  compIdExp(GV,Lc,notLast,Opts,L0,L1,D,D1,AC,C1,Stk,Stk0),
  compPtn(P,Lc,Fl,Brks,Opts,L1,L2,D1,D2,C1,C2,Stk0,Stk1),
  verify(Stk=Stk1,"patterns not allowed to modify stack"),
  call(Hndlr,E,Lc,Brks,Last,Opts,L2,L3,D2,D3,C2,[iBreak(Ok)],Stk,Stka),
  compCaseCond(More,GV,Ok,Dflt,Hndlr,Brks,Last,Opts,L3,Lx,D3,Dx,BC,[],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"disjunction").

genLastReturn(last,Opts,C,Cx,Stk,Stkx) :-
  genRet(Opts,C,Cx,Stk,Stkx).
genLastReturn(_,_,C,C,Stk,Stk).
