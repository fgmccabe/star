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

genCode(PkgDecls,mdule(Pkg,Imports,Decls,LDecls,Defs),Opts,Text) :-
  encPkg(Pkg,PT),
  initDict(D0),
  genImports(Imports,ImpTpl),
  rfold(PkgDecls,gencode:defGlbl,D0,D2),
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

defGlbl(varDec(_,Nm,Tp),D,Dx) :-!,
  defineGlbVar(Nm,Tp,D,Dx).
defGlbl(_,D,D).

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
  buildArgs(Args,0,D,D1),
  nearlyFlatSig(LTp,BlkTp),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(BlkTp,FC))|CA]),
  compPtnArgs(Args,Lc,argGetter,0,Abrt,[],notLast,Opts,L1,L3,D1,D2,FC,FC0,some(0),Stk0),
  compExp(Value,Lc,[],last,Opts,L3,L4,D2,D3,FC0,FC1,Stk0,Stk1),
  genRet(Opts,FC1,[],Stk1,_),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,CA,[iHalt(10)],Stk0,_),
  findMaxLocal(Dx,Mx),
  genDbg(Opts,C,[iEntry|C0]),
  (is_member(traceGenCode,Opts) -> dispCode(func(Nm,H,Sig,Mx,C));true ),
  peepOptimize(C,Cde),
  (is_member(showGenCode,Opts) -> dispCode(func(Nm,H,Sig,Mx,Cde));true ),
  assem(func(Nm,H,Sig,Mx,Cde),CdTrm).

genGlb(D,Opts,Lc,Nm,Tp,Value,Cd) :-
  toLtipe(funType(tplType([]),Tp),LTp),
  encLtp(LTp,Sig),
  genLbl([],Abrt,L1),
  nearlyFlatSig(LTp,Sig),
  genLine(Opts,Lc,C0,[iLbl(Abrt,iBlock(Sig,FC))|CA]),
  compExp(Value,Lc,[],last,Opts,L1,L2,D,D1,FC,[iTG(Nm)|FC1],some(0),Stk0),
  compAbort(Lc,strg("def failed"),[],Opts,L2,_L3,D1,Dx,CA,[iHalt(10)],some(0),_),
  genRet(Opts,FC1,[],Stk0,_),
  findMaxLocal(Dx,Mx),
  genDbg(Opts,C,[iEntry|C0]),
  (is_member(traceGenCode,Opts) -> dispCode(func(lbl(Nm,0),hard,Sig,Mx,C));true ),
  peepOptimize(C,Cde),
  (is_member(showGenCode,Opts) -> dispCode(func(lbl(Nm,0),hard,Sig,Mx,Cde));true ),
  assem(func(lbl(Nm,0),hard,Sig,Mx,Cde),Cd).

genRet(Opts,C,Cx,_Stk,none) :-
  genDbg(Opts,C,[iRet|Cx]).

dropCont(Lx,Lx,D,D,[iDrop|Cx],Cx,Stk,Stk1) :-
  dropStk(Stk,1,Stk1).

resetVars(scope(Vrs,M0),scope(_,M1),scope(Vrs,Mx)) :-
  Mx is max(M0,M1).

mergeVars(scope(V1,Fr1,M1),scope(V2,Fr2,M2),scope(Vx,Frx,Mx)) :-
  intersect(V1,V2,Vx),
  intersect(Fr1,Fr2,Frx),
  Mx is max(M1,M2).

initDict(scope([],0)).

buildArgs([],_,D,D) :-!.
buildArgs([A|R],Ix,D,Dx) :-
  buildArg(A,Ix,D,D0),
  Ix1 is Ix+1,
  buildArgs(R,Ix1,D0,Dx).

buildArg(idnt(Nm,Tp),Ix,scope(D,Mx),scope([(Nm,T,a(Ix))|D],Mx)) :-!,
  toLtipe(Tp,T).
buildArg(_,_,D,D).

lclVar(Nm,T,Wh,scope(Vrs,_,_)) :-
  is_member((Nm,T,Wh),Vrs),!.

defineLclVar(Nm,T,Opts,scope(Vrs,FreeRg,Mx),scope([(Nm,T,l(Off))|Vrs],NFreeRg,Mx1),Off,C,Cx) :-
  nextFreeOff(FreeRg,Mx,Off,NFreeRg,Mx1),
  genDebug(Opts,iLocal(Off,strng(Nm)),C,Cx).

defineTmpVar(TmpNm,T,Opts,D,Dx,Off,C,Cx) :-
  genTmpVar(D,TmpNm),
  defineLclVar(TmpNm,T,Opts,D,Dx,Off,C,Cx).

genDebug(Opts,Debug,[Debug|Cx],Cx) :-
  is_member(debugging,Opts),!.
genDebug(_,_,Cx,Cx).

clearLclVar(_Nm,Scope,Scope).

%% scope(Vrs,Mx),scope(NVrs,NMx)) :-
%%   subtract((Nm,_,l(Off),_,_),Vrs,NVrs),
%%   addToFree(Off,FreeRg,Mx,NFreeRg,NMx).

nextFreeOff([Off|FreeRg],Mx,Off,FreeRg,Mx).
nextFreeOff([],Mx,Mx1,[],Mx1) :-
  Mx1 is Mx+1.

addToFree(Off,FreeRg,Mx,[Off|FreeRg],Mx) :-!.

defineGlbVar(Nm,Tp,scope(Vrs,Mx),scope([(Nm,T,g(Nm),none,none)|Vrs],Mx)) :-
  toLtipe(Tp,T).

populateVarNames([],_,_,C,C).
populateVarNames([(Nw,idnt(Ex,T))|Vs],Lc,D,C,Cx) :-
  populateVarNm(Nw,T,Lc,Ex,D,C,C0),
  populateVarNames(Vs,Lc,D,C0,Cx).

populateVarNm(Nw,_,_,Ex,scope(Vrs,_,_),[iLocal(Nw,Frm,End,Off)|Cx],Cx) :-
  is_member((Ex,_,l(Off),Frm,End),Vrs),!.
populateVarNm(_,T,_,Ex,scope(Vrs,_,_),Cx,Cx) :-
  is_member((Ex,T,a(_),_,_),Vrs),!.
populateVarNm(_,_,Lc,Ex,_,C,C) :-
  reportError("variable %s not known",[id(Ex)],Lc),
  abort.

genLbl(Lbs,Lb,[Lb|Lbs]) :-
  length(Lbs,N),
  swritef(Lb,"_L%d",[N]).

genTmpVar(scope(_,_,Mx),Nm) :-
  swritef(Nm,"_𝜏%d",[Mx]).

findMaxLocal(scope(_,_,Mx),Mx).

localMx((_,l(Off),_),M,Mx) :- !, Mx is max(Off,M).
localMx(_,M,M).


compExpCase(T,Lc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compExp(T,Lc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

compVar(a(A),Last,[iLdA(A)|C],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stka),
  genLastReturn(Last,C,Cx,Stka,Stkx).
compVar(l(X),Last,[iLdL(X)|C],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stka),
  genLastReturn(Last,C,Cx,Stka,Stkx).
compVar(g(GlbNm),Last,[iLdG(GlbNm)|C],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stka),
  genLastReturn(Last,C,Cx,Stka,Stkx).

/* Compile actions */
compAction(nop(_),_Lc,_Brks,Last,_Opts,Lx,Lx,Dx,Dx,C,Cx,Stk,Stkx) :-!,
  genLastReturn(Last,C,Cx,Stk,Stkx).
compAction(seq(Lc,A,B),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compAction(A,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk0),
  resetStack(Stk,Stk0,C1,C2),
  compAction(B,Lc,Brks,Last,Opts,L1,Lx,D1,Dx,C2,Cx,Stk,Stkx).
compAction(lbld(Lc,Lb,A),OLc,Brks,Last,Opts,L,Lx,D,Dx,[iLbl(BrkLb,iBlock(FlatTp,BC))|C],Cx,Stk,Stk) :-
  chLine(Opts,OLc,Lc,BC,C0),
  genLbl(L,BrkLb,L1),
  flatBlockSig(FlatTp),
  compAction(A,Lc,[(Lb,BrkLb,Stk)|Brks],Last,Opts,L1,Lx,D,Dx,C0,[iBreak(Lb)],Stk,Stk0),
  resetStack(Stk,Stk0,C,Cx).
compAction(brk(Lc,Nm),OLc,Brks,_Last,Opts,Lx,Lx,Dx,Dx,C,Cx,Stk,Stkx) :-!,
  (is_member((Nm,BrkLb,Stkx),Brks) ->
   chLine(Opts,OLc,Lc,C,[iBreak(BrkLb)|Cx]) ;
   reportError("not in scope of break label %s",[ss(Nm)],Lc),
   Stk=Stkx).
compAction(rais(Lc,T,E),OLc,Brks,_Last,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(T,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iThrow|Cx],Stka,_Stka).
compAction(perf(Lc,Cll),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(Cll,Lc,Brks,Last,Opts,L,Lx,D,Dx,C0,C1,Stk,Stk0),
  resetStack(Stk,Stk0,C1,Cx).
compAction(mtch(Lc,P,E),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatTp,[iLbl(Abrt,iBlock(FlatTp,CB))|Ca]))|Cx]),
  genLbl(L,Ok,L1),
  genLbl(L1,Abrt,L2),
  flatBlockSig(FlatTp),
  compExp(E,Lc,Brks,notLast,Opts,L2,L3,D,D1,CB,C1,Stk,Stk1),
  compPtn(P,Lc,Brks,Last,Opts,L3,L4,D1,D2,C1,C2,Stk1,Stka),
  resetStack(Stk,Stka,C2,[iBreak(Ok)]),
  compAbort(Lc,strg("match fail"),Brks,Opts,L4,Lx,D2,Dx,Ca,[],Stk,_).
compAction(defn(Lc,idnt(Nm,Tp),E),OLc,_Last,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,Brks,notLast,Opts,L,Lx,D,D1,C0,[iStL(Off)|C1],Stk,Stk0),
  toLtipe(Tp,T),
  defineLclVar(Nm,T,Opts,D1,Dx,Off,C1,Cx),
  dropStk(Stk,1,Stk0).
compAction(setix(Lc,Exp,Off,Vl),OLc,Brks,_Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(Vl,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iStNth(Off)|Cx],Stka,Stkb),
  dropStk(Stkb,2,Stk).
compAction(asgn(Lc,Cll,Exp),OLc,Brks,_Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(Cll,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iAssign|Cx],Stka,Stkb),
  dropStk(Stkb,2,Stk).
compAction(case(Lc,T,Cases,Deflt),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  flatBlockSig(FlatTp),
  compCase(T,Lc,FlatTp,Cases,Deflt,gencode:compAction,Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stk).
compAction(whle(Lc,G,B),OLc,Brks,_Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-
  chLine(Opts,OLc,Lc,C,[iLbl(Done,iBlock(FlatTp,[iLbl(Lp,iBlock(FlatTp,LC))]))|Cx]),!,
  flatBlockSig(FlatTp),
  genLbl(L,Lp,L1),
  genLbl(L1,Done,L2),
  compCond(G,Lc,Done,Brks,Opts,L2,L3,D,D1,LC,LC1,Stk,Stka),
  compAction(B,Lc,Brks,notLast,Opts,L3,Lx,D1,D2,LC1,LC2,Stk,Stkb),
  resetVars(D,D2,Dx),
  reconcileStack(Stka,Stkb,Stk,LC2,[iLoop(Lp)]),!.
compAction(ltt(Lc,idnt(Nm,Tp),Val,Act),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  toLtipe(Tp,T),
  defineLclVar(Nm,T,Opts,D,D1,Off,C0,[iStV(Off)|C1]),
  compExp(Val,Lc,Brks,notLast,Opts,L,L1,D1,D2,C1,[iStL(Off)|C2],Stk,Stk1),
  verify(gencode:bumpStk(Stk,1,Stk1),"expecting a single bump in stack"),
  compAction(Act,Lc,Brks,Last,Opts,L1,Lx,D2,Dx,C2,Cx,Stk,_).
compAction(iftte(Lc,G,A,B),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatTp,[iLbl(Fl,iBlock(FlatTp,AC))|BC]))|Cx]),
  genLbl(L,Fl,L0),
  genLbl(L0,Ok,L1),
  flatBlockSig(FlatTp),
  compCond(G,Lc,Fl,Brks,Opts,L1,L2,D,D1,AC,AC1,Stk,Stk0),
  verify(Stk=Stk0,"conditions should not increase stack"),
  compAction(A,Lc,Brks,Last,Opts,L2,L3,D1,D2,AC1,[iBreak(Ok)],Stk,Stka),
  compAction(B,Lc,Brks,Last,Opts,L3,Lx,D1,D3,BC,[iBreak(Ok)],Stk,Stkb),
  resetVars(D2,D3,Dx),
  mergeStkLvl(Stka,Stkb,Stk,"conditional action").
compAction(try(Lc,B,T,E,H),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!, 
  compTry(Lc,B,T,E,H,OLc,gencode:compAction,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk).
compAction(error(Lc,Msg),_OLc,Brks,_Last,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,_).
compAction(A,Lc,_Brks,_Last,_Opts,Lx,Lx,Dx,Dx,C,C,Stk,Stk) :-
  reportError("cannot compile action %s",[lact(A)],Lc),
  abort.

compTry(Lc,B,idnt(V,Tp),idnt(E,ETp),H,OLc,Hndlr,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatTp,[iTry(FlatTp,[iStL(TOff)|BC])|HC]))|Cz]),
  genLbl(L,Ok,L1),
  flatBlockSig(FlatTp),
  toLtipe(Tp,T),
  defineLclVar(V,T,Opts,D,D1,TOff,BC,B1),
  call(Hndlr,B,Lc,Brks,notLast,Opts,L1,L2,D1,D2,B1,[iLdL(TOff),iEndTry(Ok)],Stk,Stka),
  genLine(Opts,Lc,HC,H1),
  resetVars(D,D2,D3),
  toLtipe(ETp,ET),
  defineLclVar(E,ET,Opts,D3,D4,EOff,H1,[iStL(EOff)|H2]),
  call(Hndlr,H,Lc,Brks,Last,Opts,L2,Lx,D4,Dx,H2,[iBreak(Ok)],Stk,Stkb),
  reconcileStack(Stka,Stkb,Stkx,Cz,Cx),!.

flatBlockSig(strg(Sig)) :-
  mkFnTipe([],tplTipe([]),FlatTp),
  encLtp(FlatTp,Sig).

nearlyFlatSig(Tp,strg(Sig)) :-
  mkFnTipe([],Tp,BlkTp),
  encLtp(BlkTp,Sig).

frameSig(Stk,strg(Sig)) :-
  mkTplTipe(Stk,FrTp),
  encLtp(FrTp,Sig).

frameIns(some(Stk),[iFrame(Sig)|Cx],Cx) :-
  frameSig(Stk,Sig).
frameIns(none,Cx,Cx).

popStack(lbl(_,Ar),St,Stx) :-
  dropStk(St,Ar-1,Stx).

stkLvl(some(Lvl),Lvl).

resetStack(Stk,Stk,C,C) :-!.
resetStack(Stk,Stk0,[iDrop|C],C) :-
  dropStk(Stk0,1,Stk),!.
resetStack(some(Lvl),_,[iRst(Lvl)|Cx],Cx).

asmCont(Op,Cont,Stk,L,Lx,D,Dx,[Op|C],Cx,_,Stkx) :-
  call(Cont,L,Lx,D,Dx,C,Cx,Stk,Stkx).

reconcileStack(_,_,none,C,C) :-!.
reconcileStack(Stk,Stk,_,C,C) :-!.
reconcileStack(some(Stki),some(Stk),_,[iRst(Stk)|C],C) :-
  Stki>Stk,!.

compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-
  locTerm(Lc,LT),
  compExps([LT,Msg],Lc,Brks,Opts,L,Lx,D,Dx,C,[iAbort|Cx],Stk,_Stkx).

argGetter(Ix,Lx,Lx,D,D,[iLdA(Ix)|Cx],Cx,Stk,Stk1) :-
  bumpStk(Stk,Stk1).

useTmp(Off,_T,Ix,Lx,Lx,D,D,[iLdL(Off),iNth(Ix)|Cx],Cx,Stk,Stk1) :-
  bumpStk(Stk,Stk1).

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
dropStk(some(Stk),Cnt,some(Stk1)) :- Stk1 is Stk-Cnt.

sameStk(none,none) :-!.
sameStk(some(S),some(S)).

mergeStkLvl(none,Stk,Stk,_) :-!.
mergeStkLvl(Stk,none,Stk,_) :-!.
mergeStkLvl(Stk1,Stk2,Stk1,Msg) :-
  verify(gencode:sameStk(Stk1,Stk2),Msg).

% compile argument patterns. If fail, then break out
compPtnArgs([],_Lc,_Getter,_Ix,_Fail,_Brks,Last,_Opts,Lx,Lx,Dx,Dx,C,Cx,Stk,Stkx) :-!,
  genLastReturn(Last,C,Cx,Stk,Stkx).
compPtnArgs([A|R],Lc,Getter,Ix,Fail,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compPtnArg(A,Lc,Getter,Ix,Fail,Brks,Opts,L,L1,D,D1,C,C1,Stk,Stk1),
  Ix1 is Ix+1,
  compPtnArgs(R,Lc,Getter,Ix1,Fail,Brks,Last,Opts,L1,Lx,D1,Dx,C1,Cx,Stk1,Stkx).

compPtnArg(idnt(V,_),_,argGetter,Ix,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stkx,Stkx) :-
  lclVar(V,_,a(Ix),Dx),!.
compPtnArg(voyd,_Lc,_Getter,_Ix,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stkx,Stkx) :- !.
compPtnArg(ann(_),_Lc,_Getter,_Ix,_Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stk,Stk) :- !.
compPtnArg(P,Lc,Getter,Ix,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  call(Getter,Ix,L,L0,D,D0,C,C0,Stk,Stk0),
  compPtn(P,Lc,Fail,Brks,Opts,L0,Lx,D0,Dx,C0,Cx,Stk0,Stkx).

% compile a pattern. Patterns form a reduced subset of possible expression forms

compPtn(voyd,_,_Fail,_Opts,Lx,Lx,Dx,Dx,[iDrop|Cx],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stkx).
compPtn(ann(_),_,_Fail,_Opts,Lx,Lx,Dx,Dx,[iDrop|Cx],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stkx).
compPtn(Lit,_,Fail,_Opts,Lx,Lx,Dx,Dx,[iLdC(Lit),iCmp(Fail)|Cx],Cx,Stk,Stkx) :-
  isLiteral(Lit),!,
  dropStk(Stk,1,Stkx).
compPtn(idnt(Nm,Tp),_,_Fail,_Brks,Opts,Lx,Lx,D,Dx,[iStL(Off)|C],Cx,Stk,Stkx) :-
  toLtipe(Tp,T),
  defineLclVar(Nm,T,Opts,D,Dx,Off,C,Cx),
  dropStk(Stk,1,Stkx).
compPtn(ctpl(St,Args),Lc,Fail,Brks,Opts,L,Lx,D,Dx,[iCLbl(St,Fail),iStL(Off)|C],Cx,Stk,Stkx) :-
  defineTmpVar(_TmpNm,ptrTipe,Opts,D,D1,Off,C,C0),
  dropStk(Stk,1,Stk0),
  compPtnArgs(Args,Lc,gencode:useTmp(Off,ptrTipe),0,Fail,Brks,notLast,Opts,L,Lx,D1,D2,C0,Cx,Stk0,Stkx),
  clearLclVar(Off,D2,Dx).
compPtn(whr(Lc,P,Cnd),OLc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compPtn(P,Lc,Fail,Brks,Opts,L,L1,D,D1,C0,C1,Stk,Stkx),
  compCond(Cnd,Lc,Fail,Brks,Opts,L1,Lx,D1,Dx,C1,Cx,Stkx,Stky),
  verify((Stkx=Stky),"where condition does not leave stack alone").
compPtn(T,Lc,Fail,_Brks,_Opts,Lx,Lx,Dx,Dx,[iBreak(Fail)|Cx],Cx,Stk,Stk) :-
  reportError("(internal) cannot compile pattern %s",[ltrm(T)],Lc).


% compile a condition. Invoke passed in Fail label if the condition is false
compCond(enum(Sy),_Lc,_Fail,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stkx,Stkx) :-
  isTrueSymb(Sy),!.
compCond(enum(Sy),_,Fail,_Opts,Lx,Lx,Dx,Dx,[iBreak(Fail)|Cx],Cx,Stkx,Stkx) :-
  isFalseSymb(Sy),!.
compCond(cnj(Lc,A,B),OLc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCond(A,Lc,Fail,Brks,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  compCond(B,Lc,Fail,Brks,Opts,L1,Lx,D1,Dx,C1,Cx,Stk1,Stkx).
compCond(dsj(Lc,A,B),OLc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatTp,[iLbl(Fl,iBlock(FlatTp,AC))|BC]))|Cx]),
  flatBlockSig(FlatTp),
  genLbl(L,Fl,L0),
  genLbl(L0,Ok,L1),
  compCond(A,Lc,Fl,Brks,Opts,L1,L2,D,D0,AC,[iBreak(Ok)],Stk,Stka),
  compCond(B,Lc,Fail,Brks,Opts,L2,Lx,D0,Dx,BC,[],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"disjunction").
compCond(ng(Lc,A),OLc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,[iBlock(FlatTp,[iLbl(Fl,iBlock(FlatTp,AC))])|Cx]),
  flatBlockSig(FlatTp),
  genLbl(L,Fl,L0),
  compCond(A,Lc,Fl,Brks,Opts,L0,Lx,D,Dx,AC,[iBreak(Fail)],Stk,Stkx).
compCond(cnd(Lc,T,A,B),OLc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  flatBlockSig(FlatTp),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatTp,[iLbl(El,iBlock(FlatTp,CA))|CB]))|Cx]),
  genLbl(L,El,L0),
  compCond(T,Lc,El,Brks,Opts,L0,L1,D,D1,CA,C0,Stk,Stk0),
  genLbl(L1,Ok,L2),
  compCond(A,Lc,Fail,Brks,Opts,L2,L3,D1,D2,C0,[iBreak(Ok)],Stk0,Stka),
  compCond(B,Lc,Fail,Brks,Opts,L3,Lx,D2,Dx,CB,[iBreak(Ok)],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"conditional").
compCond(mtch(Lc,P,E),OLc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compPtn(P,Lc,Fail,Brks,Opts,L1,Lx,D1,Dx,C1,Cx,Stka,Stkb),
  mergeStkLvl(Stk,Stkb,Stkx,"pattern conditional").
compCond(E,Lc,Fail,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compExp(E,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C,[iIfNot(Fail)|Cx],Stk,Stka),
  mergeStkLvl(Stk,Stka,Stkx,"pattern conditional").

% compile an expression
compExp(voyd,_,_Brks,Last,_Opts,Lx,Lx,Dx,Dx,[iLdV|C],Cx,Stk,Stkx) :-!,
  bumpStk(Stk,Stka),
  genLastReturn(Last,C,Cx,Stka,Stkx).
compExp(Trm,_Lc,_Brks,Last,_Opts,Lx,Lx,Dx,Dx,[iLdC(Trm)|C],Cx,Stk,Stkx) :-
  isLiteral(Trm),!,
  bumpStk(Stk,Stka),
  genLastReturn(Last,C,Cx,Stka,Stkx).
compExp(idnt(Nm,_),Lc,_Brks,Last,_Opts,Lx,Lx,Dx,Dx,C,Cx,Stk,Stkx) :-!,
  (lclVar(Nm,_T,V,Dx) -> 
   compVar(V,Last,C,Cx,Stk,Stkx) ;
   reportError("cannot locate variable %s",[id(Nm)],Lc),
   abort).
compExp(ctpl(St,A),Lc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compExps(A,Lc,Brks,Opts,L,Lx,D,Dx,C,[iAlloc(St)|C1],Stk,_Stka),
  bumpStk(Stk,Stka),
  frameIns(Stkx,C1,C2),
  genLastReturn(Last,C2,Cx,Stka,Stkx).
compExp(ecll(Lc,Nm,A,_Tp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  isIntrinsic(Nm,_,Op),!,
  chLine(Opts,OLc,Lc,C,C0),
  compExps(A,Lc,Brks,Opts,L,Lx,D,Dx,C0,[Op|C1],Stk,_Stka),
  bumpStk(Stk,Stka),
  frameIns(Stkx,C1,C2),
  genLastReturn(Last,C2,Cx,Stka,Stkx).
compExp(ecll(Lc,Nm,A,_Tp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExps(A,Lc,Brks,Opts,L,Lx,D,Dx,C0,[iEscape(Nm)|C1],Stk,_Stka),
  bumpStk(Stk,Stka),
  frameIns(Stkx,C1,C2),
  genLastReturn(Last,C2,Cx,Stka,Stkx).
compExp(cll(Lc,Nm,A),OLc,Brks,last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExps(A,Lc,Brks,Opts,L,Lx,D,Dx,C0,[iTCall(Nm)|C1],Stk,_Stka),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C1,Cx).
compExp(cll(Lc,Nm,A),OLc,Brks,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExps(A,Lc,Brks,Opts,L,Lx,D,Dx,C0,[iCall(Nm)|C1],Stk,_Stka),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C1,Cx).
compExp(ocall(Lc,O,A,_Tp),OLc,Brks,last,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  length(A,Ar),
  Arity is Ar+1,
  compExps(A,Lc,Brks,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(O,Lc,Brks,Opts,L1,Lx,D1,Dx,C1,[iTOCall(Arity)|Cx],Stka,_).
compExp(ocall(Lc,O,A,_Tp),OLc,Brks,notLast,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  length(A,Ar),
  Arity is Ar+1,
  compExps(A,Lc,Brks,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(O,Lc,Brks,Opts,L1,Lx,D1,Dx,C1,[iOCall(Arity)|C2],Stka,_),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C2,Cx).
compExp(voke(Lc,K,A),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  length(A,Ar),
  Arity is Ar+1,
  compExps(A,Lc,Brks,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(K,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iInvoke(Arity)|C2],Stka,_Stkb),
  bumpStk(Stk,Stka),
  frameIns(Stkx,C2,C3),
  genLastReturn(Last,C3,Cx,Stka,Stkx).
compExp(clos(Lb,Ar,Free),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compExp(Free,OLc,Brks,Opts,L,Lx,D,Dx,C,[iClosure(lbl(Lb,Ar))|C1],Stk,_Stka),
  bumpStk(Stk,Stka),
  frameIns(Stkx,C1,C2),
  genLastReturn(Last,C2,Cx,Stka,Stkx).
compExp(nth(Lc,Exp,Off,_),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C0,[iNth(Off)|C1],Stk,_Stka),
  bumpStk(Stk,Stka),
  genLastReturn(Last,C1,Cx,Stka,Stkx).
compExp(setix(Lc,Exp,Off,Vl),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk0),
  compExp(Vl,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iStNth(Off)|C1],Stk0,_Stkx),
  genLastReturn(Last,C1,Cx,Stk,Stkx).
compExp(cel(Lc,Exp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Brks,Opts,L,Lx,D,Dx,C0,[iCell|C1],Stk,Stka),
  frameIns(Stka,C1,C2),
  genLastReturn(Last,C2,Cx,Stka,Stkx).
compExp(get(Lc,Exp),_,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compExp(Exp,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C,[iGet|C1],Stk,Stka),
  genLastReturn(Last,C1,Cx,Stka,Stkx).
compExp(set(Lc,Cl,Val),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Val,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  compExp(Cl,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iSet|C1],Stk1,_Stka),
  genLastReturn(Last,C1,Cx,Stk,Stkx).
compExp(case(Lc,T,Cases,Deflt),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  nearlyFlatSig(ptrTipe,BlkTp),
  compCase(T,Lc,BlkTp,Cases,Deflt,gencode:compExp,Brks,Last,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compExp(try(Lc,B,T,E,H),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  compTry(Lc,B,T,E,H,OLc,gencode:compExp,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk).
compExp(varNames(Lc,Vrs,T),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  populateVarNames(Vrs,Lc,D,C0,C1),
  compExp(T,Lc,Brks,Last,Opts,L,Lx,D,Dx,C1,Cx,Stk,Stkx).
compExp(ltt(Lc,idnt(Nm,Tp),Val,Exp),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  toLtipe(Tp,T),
  defineLclVar(Nm,T,Opts,D,D1,Off,C0,[iStV(Off)|C1]),
  compExp(Val,Lc,Brks,notLast,Opts,L,L1,D1,D2,C1,[iStL(Off)|C2],Stk,Stk1),
  verify(gencode:bumpStk(Stk,1,Stk1),"expecting a single bump in stack"),
  compExp(Exp,Lc,Lc,Brks,Last,Opts,L1,Lx,D2,Dx,C2,Cx,Stk,Stkx).
compExp(error(Lc,Msg),_OLc,Brks,_Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx). % no continuation after an error
compExp(rais(Lc,T,E),OLc,Brks,_Last,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(E,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  compExp(T,Lc,Brks,notLast,Opts,L1,Lx,D1,Dx,C1,[iThrow|Cx],Stk1,_).
compExp(cnd(Lc,Cnd,A,B),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(BlkTp,[iLbl(Fl,iBlock(BlkTp,AC))|BC]))|Cx]),
  genLbl(L,Fl,L0),
  genLbl(L0,Ok,L1),
  nearlyFlatSig(ptrTipe,BlkTp),
  compCond(Cnd,Lc,Fl,Brks,Opts,L1,L2,D,D1,AC,AC1,Stk,Stk0),
  verify(Stk=Stk0,"conditions should not increase stack"),
  compExp(A,Lc,Brks,Last,Opts,L2,L3,D1,D2,AC1,[iBreak(Ok)],Stk,Stka),
  compExp(B,Lc,Brks,Last,Opts,L3,Lx,D1,D3,BC,[iBreak(Ok)],Stk,Stkb),
  resetVars(D2,D3,Dx),
  mergeStkLvl(Stka,Stkb,Stkx,"conditional expression stack").
compExp(seqD(Lc,A,B),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(A,Lc,Brks,notLast,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  resetStack(Stk,Stk1,C1,C2),
  compExp(B,Lc,Brks,Last,Opts,L1,Lx,D1,Dx,C2,Cx,Stk,Stkx).
compExp(vlof(Lc,A),_,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compAction(A,Lc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compExp(tsk(Lc,F),OLc,Brks,Last,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,[iFiber|C0]),!,
  compExp(F,Lc,Brks,notLast,Opts,L,Lx,D,Dx,C0,C1,Stk,Stka),
  genLastReturn(Last,C1,Cx,Stka,Stkx).
compExp(Cond,Lc,Brks,Last,Opts,L,Lx,D,Dx,[iLbl(Ok,iBlock(BlkTp,[iLbl(Fl,iBlock(BlkTp,C1)),iLdC(enum(False)),iBreak(Ok)]))|Cx],Cx,Stk,Stkx) :-
  isCond(Cond),!,
  isTrueSymb(True),
  isFalseSymb(False),
  nearlyFlatSig(blTipe,BlkTp),
  genLbl(L,Ok,L0),
  genLbl(L0,Fl,L1),
  compCond(Cond,Lc,Fl,Brks,Opts,L1,Lx,D,Dx,C1,[iLdC(enum(True)),iBreak(Ok)|C2],Stk,Stka),
  genLastReturn(Last,C2,Cx,Stka,Stkx).
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

isTrueSymb("star.core#true").
isFalseSymb("star.core#false").

compCase(Gv,Lc,BlkTp,Cases,Deflt,Hndlr,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compExp(Gv,Lc,Brks,Opts,L,L1,D,D1,C,
	  [iLbl(Ok,iBlock(BlkTp,
			  [iLbl(Df,iBlock(BlkTp,[iCase(Mx)|C1]))|DC]))|Cx],Stk,Stk1),
  genLbl(L,Df,L0),
  genLbl(L0,Ok,L1),
  genCaseTable(Cases,Mx,Table),
  nearlyFlatSig(ptrTipe,BlkTp),

  compCases(Table,0,Mx,BlkTp,Ok,Df,Hndlr,Brks,Opts,L1,L2,D1,D2,C1,[iBreak(Df)],Stk1,Stka),
  call(Hndlr,Deflt,Lc,Brks,Opts,L2,Lx,D2,Dx,DC,[iBreak(Ok)],Stk1,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"case exp").

genCaseTable(Cases,P,Table) :-
  length(Cases,L),
  nextPrime(L,P),
  caseHashes(Cases,P,Hs),
  sortCases(Hs,Table).

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

compCases([],Ix,Mx,_BlkTp,_Ok,_Df,_Hndlr,_Opts,Lx,Lx,D,D,Cx,Cx,_Stk,none) :-
  Ix>=Mx.
compCases([],Ix,Mx,BlkTp,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,[iBreak(Dflt)|C],Cx,Stk,Stkx) :-
  Ix1 is Ix+1,
  compCases([],Ix1,Mx,BlkTp,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compCases([(Ix,Case)|Cs],Ix,Mx,BlkTp,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,[iBlock(BlkTp,CC)|C],Cx,Stk,Stkx) :-!,
  compCaseBranch(Case,BlkTp,Ok,Dflt,Hndlr,Brks,Opts,L,L1,D,D1,CC,Stk,Stk1),
  Ix1 is Ix+1,
  compCases(Cs,Ix1,Mx,BlkTp,Ok,Dflt,Brks,Opts,L1,Lx,D1,Dx,C,Cx,Stk,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"case branch").
compCases(Cs,Ix,Mx,BlkTp,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,[iBreak(Dflt)|C],Cx,Stk,Stkx) :-
  Ix1 is Ix+1,
  compCases(Cs,Ix1,Mx,BlkTp,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E,Lc)],_BlkTp,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,C,Stk,Stkx) :-!,
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,Dflt,Brks,Opts,L,L1,D,D2,C0,C1,Stk,Stk1),
  call(Hndlr,E,Lc,Brks,Opts,L1,Lx,D2,Dx,C1,[iBreak(Ok)],Stk1,Stkx).
compCaseBranch([(P,E,Lc)|SC],BlkTp,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,[iTL(Off),iLbl(Fl,iBlock(BlkTp,PC))|C],Stk,Stkx) :-
  genLbl(L,Fl,L1),
  genLine(Opts,Lc,PC,PC0),
  defineLclVar("__",ptrTipe,Brks,Opts,D,D1,Off,PC0,PC1),
  compPtn(P,Lc,Fl,Brks,Opts,L1,L2,D1,D2,PC1,PC2,Stk,Stk0),
  call(Hndlr,E,Lc,Ok,Brks,Opts,L2,L3,D2,D3,PC2,[iBreak(Ok)],Stk0,Stka),
  resetVars(D1,D3,D4),
  compMoreCase(SC,Off,BlkTp,Ok,Dflt,Hndlr,Brks,Opts,L3,Lx,D4,Dx,C,Stk0,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"case branch stack").

compMoreCase([],_Vlb,_BlkTp,_Ok,Dflt,_Hndlr,_Opts,Lx,Lx,Dx,Dx,[iBreak(Dflt)],Stkx,Stkx) :-!.
compMoreCase([(P,E,Lc)|SC],VLb,BlkTp,Ok,Dflt,Hndlr,Brks,Opts,L,Lx,D,Dx,[iLdL(VLb),iLbl(Fl,iBlock(BlkTp,PC))|C],Stk,Stkx) :-
  genLbl(L,Fl,L1),
  bumpStk(Stk,Stk0),
  genLine(Opts,Lc,PC,PC0),
  compPtn(P,Lc,Fl,Brks,Opts,L1,L2,D,D1,PC0,PC1,Stk,Stk0),
  resetVars(D,D1,D2),
  call(Hndlr,E,Lc,Brks,Opts,L2,L3,D1,D2,PC1,[iBreak(Ok)],Stk0,Stka),
  compMoreCase(SC,VLb,BlkTp,Ok,Dflt,Hndlr,Brks,Opts,L3,Lx,D2,Dx,C,Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"more case branch stack").

genLastReturn(last,[iRet|Cx],Cx,_,none).
genLastReturn(notLast,C,C,Stk,Stk).
