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

defGlbl(varDec(_,Nm,_),D,Dx) :-!,
  defineGlbVar(Nm,D,Dx).
defGlbl(_,D,D).

genDefs(Defs,Opts,D,O,Ox) :-
  rfold(Defs,gencode:genDef(D,Opts),Ox,O).

genDef(D,Opts,fnDef(Lc,Nm,H,Tp,Args,Value),O,[CdTrm|O]) :-
  (is_member(showTrCode,Opts) -> dispRuleSet(fnDef(Lc,Nm,H,Tp,Args,Value)) ; true),
  toLtipe(Tp,LTp),
  encLtp(LTp,Sig),
  genLbl([],Ex,L0),
  genLbl(L0,Abrt,L1),
  
  buildArgs(Args,0,D,D1),
  genLine(Opts,Lc,C0,C1),
  genLbl(L1,Abrt,L2),
  compPtnArgs(Args,Lc,argCont,0,contCont(Ex),jmpCont(Abrt,none),End,
	      [],Opts,L2,L3,D1,D2,C1,[iLbl(Ex)|C2],some(0),Stk0),
  compExp(Value,Lc,retCont(Opts),End,[],Opts,L3,L4,D2,D3,
	  C2,[iLbl(Abrt)|C3],Stk0,_Stk),
  compAbort(Lc,strg("def failed"),[],Opts,L4,_,D3,Dx,C3,[iLbl(End),iHalt(10)],Stk0,_),
  findMaxLocal(Dx,Mx),
  genDbg(Opts,C,[iLocals(Mx)|C0]),
  (is_member(showGenCode,Opts) -> dispIns(func(Nm,H,Sig,Mx,C));true ),
  peepOptimize(C,Cde),
  (is_member(showGenCode,Opts) -> dispIns(func(Nm,H,Sig,Mx,Cde));true ),
  assem(func(Nm,H,Sig,Mx,Cde),CdTrm).
genDef(D,Opts,glbDef(Lc,Nm,Tp,Value),O,[Cd|O]) :-
  toLtipe(funType(tplType([]),Tp),LTp),
  encLtp(LTp,Sig),
  genLbl([],End,L1),
  genLine(Opts,Lc,C0,C1),
  genLbl(L1,Abrt,L2),
  compExp(Value,Lc,bothCont(glbCont(Nm),rtgCont(Opts)),
	  End,[],Opts,L2,L3,D,D1,C1,[iLbl(Abrt)|C2],some(0),_Stk0),
  compAbort(Lc,strg("def failed"),[],Opts,L3,_,D1,Dx,C2,[iLbl(End),iHalt(10)],some(0),_),
  findMaxLocal(Dx,Mx),
  genDbg(Opts,C,[iLocals(Mx)|C0]),
  (is_member(showGenCode,Opts) -> dispIns(func(lbl(Nm,0),hard,Sig,Mx,C));true ),
  peepOptimize(C,Cde),
  (is_member(showGenCode,Opts) -> dispIns(func(lbl(Nm,0),hard,Sig,Mx,Cde));true ),
  assem(func(lbl(Nm,0),hard,Sig,Mx,Cde),Cd).
genDef(_,_,lblDef(_,Lbl,Tp,Ix),O,[LblTrm|O]) :-
  encType(Tp,Sig),
  assem(struct(Lbl,strg(Sig),Ix),LblTrm).
genDef(_,_,typDef(_,Tp,Rl,IxMap),O,[TpTrm|O]) :-
  assem(tipe(Tp,Rl,IxMap),TpTrm).

glbCont(Nm,Lx,Lx,D,D,[iTG(Nm)|Cx],Cx,Stk,Stk).

retCont(Opts,Lx,Lx,D,D,C,Cx,_Stk,none) :-
  genDbg(Opts,C,[iRet|Cx]).

rtgCont(Opts,Lx,Lx,D,D,C,Cx,_Stk,none) :-
  genDbg(Opts,C,[iRet|Cx]).

dropCont(Lx,Lx,D,D,[iDrop|Cx],Cx,Stk,Stk1) :-
  dropStk(Stk,1,Stk1).

idxCont(Off,Cont,L,Lx,D,Dx,[iNth(Off)|C],Cx,Stk,Stkx) :-
  call(Cont,L,Lx,D,Dx,C,Cx,Stk,Stkx).

sxCont(Off,Cont,L,Lx,D,Dx,[|C],Cx,Stk,Stkx) :-
  dropStk(Stk,2,Stk0),
  call(Cont,L,Lx,D,Dx,C,Cx,Stk0,Stkx).

txCont(Off,Cont,L,Lx,D,Dx,[iTNth(Off)|C],Cx,Stk,Stkx) :-
  call(Cont,L,Lx,D,Dx,C,Cx,Stk,Stkx).

resetVrCont(scope(Vrs,FrReg,M0),Cont,L,Lx,scope(_,_,M1),Dx,C,Cx,Stk,Stkx) :-
  Mx is max(M0,M1),
  call(Cont,L,Lx,scope(Vrs,FrReg,Mx),Dx,C,Cx,Stk,Stkx).

resetVars(scope(Vrs,FrReg,M0),scope(_,_,M1),scope(Vrs,FrReg,Mx)) :-
  Mx is max(M0,M1).

mergeVars(scope(V1,Fr1,M1),scope(V2,Fr2,M2),scope(Vx,Frx,Mx)) :-
  intersect(V1,V2,Vx),
  intersect(Fr1,Fr2,Frx),
  Mx is max(M1,M2).

initDict(scope([],[],0)).

buildArgs([],_,D,D) :-!.
buildArgs([A|R],Ix,D,Dx) :-
  buildArg(A,Ix,D,D0),
  Ix1 is Ix+1,
  buildArgs(R,Ix1,D0,Dx).

buildArg(idnt(Nm),Ix,scope(D,FreeRg,Mx),
	 scope([(Nm,a(Ix))|D],FreeRg,Mx)) :-!.
buildArg(_,_,D,D).

lclVar(Nm,Wh,scope(Vrs,_,_)) :-
  is_member((Nm,Wh),Vrs),!.

defineLclVar(Nm,Opts,scope(Vrs,FreeRg,Mx),scope([(Nm,l(Off))|Vrs],NFreeRg,Mx1),Off,C,Cx) :-
  nextFreeOff(FreeRg,Mx,Off,NFreeRg,Mx1),
  genDebug(Opts,iLocal(Off,strng(Nm)),C,Cx).

genDebug(Opts,Debug,[Debug|Cx],Cx) :-
  is_member(debugging,Opts),!.
genDebug(_,_,Cx,Cx).

clearLclVar(Nm,scope(Vrs,FreeRg,Mx),scope(NVrs,NFreeRg,NMx)) :-
  subtract((Nm,l(Off),_,_),Vrs,NVrs),
  addToFree(Off,FreeRg,Mx,NFreeRg,NMx).

nextFreeOff([Off|FreeRg],Mx,Off,FreeRg,Mx).
nextFreeOff([],Mx,Mx1,[],Mx1) :-
  Mx1 is Mx+1.

addToFree(Off,FreeRg,Mx,[Off|FreeRg],Mx) :-!.

defineGlbVar(Nm,scope(Vrs,FreeRg,Mx),
	     scope([(Nm,g(Nm),none,none)|Vrs],FreeRg,Mx)).

populateVarNames([],_,_,C,C).
populateVarNames([(Nw,idnt(Ex))|Vs],Lc,D,C,Cx) :-
  populateVarNm(Nw,Lc,Ex,D,C,C0),
  populateVarNames(Vs,Lc,D,C0,Cx).

populateVarNm(Nw,_,Ex,scope(Vrs,_,_),[iLocal(Nw,Frm,End,Off)|Cx],Cx) :-
  is_member((Ex,l(Off),Frm,End),Vrs),!.
populateVarNm(_,_,Ex,scope(Vrs,_,_),Cx,Cx) :-
  is_member((Ex,a(_),_,_),Vrs),!.
populateVarNm(_,Lc,Ex,_,C,C) :-
  reportError("variable %s not known",[id(Ex)],Lc),
  abort.

genLbl(Lbs,Lb,[Lb|Lbs]) :-
  length(Lbs,N),
  swritef(Lb,"_L%d",[N]).

findMaxLocal(scope(_,_,Mx),Mx).

localMx((_,l(Off),_),M,Mx) :- !, Mx is max(Off,M).
localMx(_,M,M).


compExpCase(T,Lc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compExp(T,Lc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

compVar(a(A),_End,Cont,L,Lx,D,Dx,[iLdA(A)|C0],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,C0,Cx,Stk1,Stkx).
compVar(l(X),_End,Cont,L,Lx,D,Dx,[iLdL(X)|C0],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,C0,Cx,Stk1,Stkx).
compVar(g(GlbNm),_End,Cont,L,Lx,D,Dx,[iLdG(GlbNm)|C],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,C,Cx,Stk1,Stkx).


/* Compile actions */
compAction(nop(_),_Lc,_Cont,ACont,_End,_Brks,_Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  call(ACont,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compAction(seq(Lc,A,B),OLc,Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compAction(A,Lc,Cont,resetCont(Stk,
				 compAction(B,Lc,Cont,ACont,End,Brks,Opts)),
	     End,Brks,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(lbld(Lc,Lb,A),OLc,Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compAction(A,Lc,Cont,ACont,End,[brk(Lb,ACont)|Brks],Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(brk(Lc,Lb),OLc,_,_ACont,_,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  (is_member(brk(Lb,ACont),Brks) ->
   call(ACont,L,Lx,D,Dx,C0,Cx,Stk,Stkx);
   reportError("not in scope of label %s",[ss(Lb)],Lc),
   L=Lx,D=Dx,C0=Cx,Stk=Stkx).
compAction(vls(Lc,E),OLc,Cont,_ACont,_End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,End,L0),
  compExp(E,Lc,Cont,End,Brks,Opts,L0,Lx,D,Dx,C0,[iLbl(End)|Cx],Stk,Stkx).
compAction(rais(Lc,T,E),OLc,_Cont,_ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,compExp(T,Lc,throwCont,End,Brks,Opts),
	  End,Brks,Opts,L,Lx,D,Dx,C0,[iLbl(End)|Cx],Stk,_Stkx).
compAction(perf(Lc,Cll),OLc,_Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(Cll,Lc,resetCont(Stk,ACont),End,Brks,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(mtch(Lc,P,E),OLc,_Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,compPtn(P,Lc,ACont,compAbort(Lc,strg("match fail"),Brks,Opts),End,Brks,Opts),
	  End,Brks,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(defn(Lc,idnt(V),E),OLc,_Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,declCont(V,ACont,End,Opts),End,Brks,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(setix(Lc,Exp,Off,Vl),OLc,_Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,
	  compExp(Vl,Lc,
		  sxCont(Off,ACont),End,Brks,Opts),
	  End,Brks,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(asgn(Lc,P,E),OLc,_Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,compExp(P,Lc,asmCont(iAssign,ACont,Stk),End,Brks,Opts),
	  End,Brks,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(case(Lc,T,Cases,Deflt),OLc,Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compCase(T,Lc,Cases,Deflt,Cont,gencode:compAct(ACont),End,Brks,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compAction(whle(Lc,G,B),OLc,Cont,ACont,_End,Brks,Opts,L,Lx,D,Dx,
	   [iJmp(TstLbl),iLbl(LpLbl)|C],Cx,Stk,Stk) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  genLbl(L,LpLbl,L1),
  genLbl(L1,TstLbl,L2),
  genLbl(L2,End,L3),
  compCond(G,Lc,contCont(LpLbl),resetCont(Stk,ACont),
	   End,Brks,Opts,L3,L4,D,D1,C2,[iLbl(End)|Cx],Stk,Stk1),
  compAction(B,Lc,Cont,contCont(TstLbl),End,Brks,Opts,L4,Lx,D1,Dx,C0,[iLbl(TstLbl)|C2],Stk1,_).
compAction(ltt(Lc,idnt(Nm),Val,Act),OLc,Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  genLbl(L,Lb,L1),
  chLine(Opts,OLc,Lc,C,C0),!,
  genLbl(L1,LEnd,L2),
  defineLclVar(Nm,Lb,LEnd,Opts,D,D1,Off,C0,[iStV(Off)|C1]),
  compExp(Val,Lc,stoCont(Off,Lb,
			 compAction(Act,Lc,Cont,ACont,LEnd,Brks,Opts)),
	  End,Brks,Opts,L2,Lx,D1,Dx,C1,[iLbl(LEnd)|Cx],Stk,Stkx).
compAction(iftte(Lc,G,T,E),OLc,Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,ACont,AC),
  splitCont(Lc,Cont,CC),
  genLbl(L,TEnd,L0),
  compCond(G,Lc,compAction(T,Lc,CC,AC,TEnd,Brks,Opts),
	   resetCont(Stk,compAction(E,Lc,CC,AC,End,Brks,Opts)),
	   TEnd,Brks,Opts,L0,Lx,D,Dx,C0,[iLbl(TEnd)|Cx],Stk,Stkx).
compAction(unpack(Lc,T,Cases),OLc,Cont,ACont,_End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  compUnpackAction(Lc,T,Cases,OLc,Cont,ACont,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk).
compAction(try(Lc,B,T,E,H),OLc,Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  genLbl(L,Nxt,L0),
  compTry(Lc,B,T,E,H,OLc,Cont,gencode:compAct(contCont(Nxt)),Brks,Opts,End,L0,L1,D,D1,C,[iLbl(Nxt)|C1],Stk),
  call(ACont,L1,Lx,D1,Dx,C1,Cx,Stk,Stkx).
compAction(error(Lc,Msg),_OLc,_Cont,_,_End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compAbort(Lc,Msg,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx). % no continuation after an error
compAction(A,Lc,_,_,_End,_Brks,_Opts,Lx,Lx,Dx,Dx,C,C,Stk,Stk) :-
  reportError("cannot compile action %s",[lact(A)],Lc),
  abort.

compUnpackAction(Lc,T,Cases,OLc,Cont,ACont,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,Nxt,L1),
  compExp(T,Lc,contCont(Nxt),Nxt,Brks,Opts,L1,L2,D,D2,C0,[iLbl(Nxt)|C1],Stk,Stk0),
  splitCont(Lc,ACont,AC),
  compCnsCases(Cases,Lc,gencode:compActCase(AC),Cont,Brks,Opts,
	       L2,Lx,D2,Dx,C1,Cx,Stk0,_).

compActCase(ACont,A,Lc,Cont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compAction(A,Lc,Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

compAct(ACont,A,Lc,Cont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compAction(A,Lc,Cont,ACont,End,Brks,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

compTry(Lc,B,idnt(T),idnt(E),H,OLc,Hndlr,Opts,End,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatTp,[iTry(FlatTp,[iStL(TOff)|BC]))|HC]))|Cz]),
  genLbl(L,Ok,L1),
  genLbl(L1,Fl,L2),
  mkFnTipe([],tplTipe([]),FlatTp),
  defineLclVar(T,Opts,D,D1,TOff,BC,B1),
  call(Hndlr,B,Lc,Opts,L2,L3,D1,D2,B1,[iLdL(TOff),iEndTry(Ok)],Stk,Stka)
  genLine(Opts,Lc,HC,H1),
  resetVars(D,D2,D3),
  defineLclVar(E,Opts,D3,D4,EOff,H1,H2),
  call(Hndlr,H,Lc,Opts,L4,Lx,D4,Dx,H2,[iBreak(Ok)],Stk,Stkb),
  reconcileStack(Stka,Stkb,Stkx,Cz,Cx),!.

throwCont(Lx,Lx,Dx,Dx,[iThrow|Cx],Cx,_Stk,none).

/* Compile actions as sequences with several possible continuations */

contCont(Lbl,Lx,Lx,D,D,C,Cx,Stk,Stk) :-
  (nonvar(Cx),Cx=[iLbl(Lbl)|_]) ->
  C=Cx ;
  C=[iJmp(Lbl)|Cx].			% some special logic here

bothCont(C1,C2,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  call(C1,L,L1,D,D0,C,C0,Stk,Stk0),
  call(C2,L1,Lx,D0,Dx,C0,Cx,Stk0,Stkx).

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

stoCont(Off,Lb,Cont,L,Lx,D,Dx,[iStL(Off),iLbl(Lb)|C],Cx,Stk,Stkx) :-!,
  dropStk(Stk,1,Stk1),
  call(Cont,L,Lx,D,Dx,C,Cx,Stk1,Stkx).

declCont(Nm,Succ,End,Opts,L,Lx,D,Dx,[iStL(Off),iLbl(Lb)|C],Cx,Stk,Stkx) :-
  genLbl(L,Lb,L0),
  defineLclVar(Nm,Lb,End,Opts,D,D1,Off,C,C0),
  dropStk(Stk,1,Stk1),
  call(Succ,L0,Lx,D1,Dx,C0,Cx,Stk1,Stkx).

releaseCont(Nm,Lx,Lx,D,Dx,Cx,Cx,Stk,Stk) :-
  clearLclVar(Nm,D,Dx).

asmCont(Op,Cont,Stk,L,Lx,D,Dx,[Op|C],Cx,_,Stkx) :-
  call(Cont,L,Lx,D,Dx,C,Cx,Stk,Stkx).

escCont(Nm,Stk0,Lx,Lx,D,D,[iEscape(Nm)|C],Cx,_Stk,Stkx) :-
  bumpStk(Stk0,Stkx),
  frameIns(Stkx,C,Cx).

cllCont(Nm,_,retCont(_),Lx,Lx,Dx,Dx,[iTCall(Nm)|Cx],Cx,_Stk,none) :-!.
cllCont(Nm,Stk0,Cont,L,Lx,D,Dx,[iCall(Nm)|C],Cx,_Stk,Stkx) :-
  bumpStk(Stk0,Stk1),
  frameIns(Stk1,C,C1),
  call(Cont,L,Lx,D,Dx,C1,Cx,Stk1,Stkx).

oclCont(Arity,retCont(_),Lx,Lx,Dx,Dx,[iTOCall(Arity)|C],C,_,none) :-!.
oclCont(Arity,Cont,L,Lx,D,Dx,[iOCall(Arity)|C],Cx,Stk,Stkx) :-
  dropStk(Stk,Arity,Stk0),
  bumpStk(Stk0,Stk1),
  frameIns(Stk1,C,C1),
  call(Cont,L,Lx,D,Dx,C1,Cx,Stk1,Stkx).

tskCont(Cont,L,Lx,D,Dx,[iFiber|C],Cx,Stk,Stkx) :-
  call(Cont,L,Lx,D,Dx,C,Cx,Stk,Stkx).

invokeCont(Arity,Cont,L,Lx,D,Dx,[iInvoke(Arity)|C],Cx,Stk,Stkx) :-
  dropStk(Stk,Arity,Stk0),
  bumpStk(Stk0,Stk1),
  frameIns(Stk1,C,C1),
  call(Cont,L,Lx,D,Dx,C1,Cx,Stk1,Stkx).

closureCont(Lb,Ar,Cont,L,Lx,D,Dx,[iClosure(lbl(Lb,Ar))|C],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stk0),
  bumpStk(Stk0,Stk1),
  frameIns(Stk1,C,C1),
  call(Cont,L,Lx,D,Dx,C1,Cx,Stk1,Stkx).

jmpCont(Lbl,Stk,Lx,Lx,D,D,[iJmp(Lbl)|Cx],Cx,_,Stk).

contHasLbl(jmpCont(Lbl,_),Lbl) :-!.
contHasLbl(contCont(Lbl),Lbl) :- !, nonvar(Lbl).
contHasLbl(bothCont(C1,_),Lbl) :- !, contHasLbl(C1,Lbl).
contHasLbl(onceCont(_,L,_),Lbl) :- nonvar(L), !, L=(Lbl,_,_).
contHasLbl(onceCont(_,_,C),Lbl) :- !, contHasLbl(C,Lbl).

isSimpleCont(retCont(_)).
isSimpleCont(rtgCont(_)).
isSimpleCont(jmpCont(_,_)).
isSimpleCont(contCont(_)).
isSimpleCont(bothCont(L,R)) :-
  isSimpleCont(L),
  isSimpleCont(R).
isSimpleCont(resetCont(_,C)) :- isSimpleCont(C).
isSimpleCont(trueCont).
isSimpleCont(falseCont).
isSimpleCont(onceCont(_,_,_)).

splitCont(_,Cont,Cont) :- isSimpleCont(Cont),!.
splitCont(Lc,Cont,onceCont(Lc,_,Cont)).

onceCont(_,Lf,Cont,L,Lx,D,Dx,[iLbl(Lb)|C],Cx,Stk,Stkx) :-
  var(Lf),!,
  genLbl(L,Lb,L0),
  call(Cont,L0,Lx,D,Dx,C,Cx,Stk,Stkx),
  Lf=(Lb,Stk,Stkx).
onceCont(_,(Lb,Stkin,Stkout),_,Lx,Lx,D,D,C,Cx,Stk,Stkout) :-
  reconcileStack(Stk,Stkin,Stkout,C,[iJmp(Lb)|Cx]),!.
onceCont(Lc,(_,Stkin,Stkout),_,Lx,Lx,D,D,Cx,Cx,Stk,Stkout) :-
  reportError("cannot reconcile stacks [%s,%s]",[Stk,Stkin],Lc),
  abort.

reconcileStack(_,_,none,C,C) :-!.
reconcileStack(Stk,Stk,_,C,C) :-!.
reconcileStack(some(Stki),some(Stk),_,[iRst(Stk)|C],C) :-
  Stki>Stk,!.

trueCont(Lx,Lx,D,D,[iLdC(enum("star.core#true"))|Cx],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stkx).

falseCont(Lx,Lx,D,D,[iLdC(enum("star.core#false"))|Cx],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stkx).

nullCont(Lx,Lx,D,D,C,C,Stk,Stk).

insCont(Ins,Lx,Lx,D,D,[Ins|C],C,Stk,Stk).

compAbort(Lc,Msg,L,Lx,D,Dx,C,Cx,Stk,none) :-
  locTerm(Lc,LT),
  compExps([LT,Msg],Lc,Opts,L,Lx,D,Dx,C,[iAbort|Cx],Stk,_Stkx).

indexCont(Ix,Lx,Lx,D,D,Stk,Stk1,[iDup,iNth(Ix)|Cx],Cx) :-
  bumpStk(Stk,Stk1).

stkArgCont(_,Lx,Lx,D,D,Stk,Stk,C,C).

argCont(_Ix,Lx,Lx,D,D,Stk,Stk,Cx,Cx).

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
compPtnArgs([],_Lc,_Ix,_Fail,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stkx,Stkx) :-!.
compPtnArgs([voyd|R],Lc,Ix,Fail,Opts,L,Lx,D,Dx,[iDrop|C],Cx,Stk,Stkx) :- 
  dropStk(Stk,1,Stk1),
  Ix1 is Ix+1,
  compPtnArgs(R,Lc,Ix1,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk1,Stkx).
compPtnArgs([A|R],Lc,Ix,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compPtnArg(A,Ix,Lc,Fail,Opts,L,L1,D,D1,C,C1,Stk,Stk1),
  Ix1 is Ix+1,
  compPtnArgs(R,Lc,Ix1,Fail,Opts,L1,Lx,D1,Dx,C1,Cx,Stk1,Stkx).

compPtnArg(idnt(V),Ix,_Fail,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stkx,Stkx) :-
  lclVar(V,a(Ix),D),!.
compPtnArg(P,_Ix,Lc,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compPtn(P,Lc,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

% compile a pattern. Patterns form a reduced subset of possible expression forms

compPtn(voyd,_,_Fail,_Opts,Lx,Lx,Dx,Dx,[iDrop|Cx],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stkx).
compPtn(anon,_,_Fail,_Opts,Lx,Lx,Dx,Dx,[iDrop|Cx],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stkx).
compPtn(Lit,_,Fail,_Opts,Lx,Lx,Dx,Dx,[iLdC(Lit),iCmp(Fail)|Cx],Cx,Stk,Stkx) :-
  isLiteral(Lit),!,
  dropStk(Stk,1,Stkx).
compPtn(idnt(Nm),_,_Fail,Opts,Lx,Lx,D,Dx,[iStL(Off)|C],Cx,Stk,Stkx) :-
  defineLclVar(Nm,Opts,D,D1,Off,C,Cx),
  dropStk(Stk,1,Stkx).
compPtn(ctpl(St,Args),Lc,Fail,Opts,L,Lx,D,Dx,[iUnpack(St,Fail)|C],Cx,Stk,Stkx) :-
  length(Args,Ar),
  dropStk(Stk,1-Ar,Stk0),
  compPtnArgs(Args,Lc,0,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk0,Stkx).
compPtn(whr(Lc,P,Cnd),OLc,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compPtn(P,Lc,Fail,Opts,L,L1,D,D1,C,C1,Stk,Stkx),
  compCond(Cnd,Lc,Fail,Opts,L1,Lx,D1,Dx,C1,Cx,Stkx,Stky),
  verify((Stkx=Stky),"where condition does not leave stack alone").
compPtn(T,Lc,Fail,Opts,Lx,Lx,Dx,Dx,[iBreak(Fail)|Cx],Cx,Stk,Stk) :-
  reportError("(internal) cannot compile pattern %s",[ltrn(T)],Lc).


% compile a condition. Invoke passed in Fail label if the condition is false
compCond(enum(Sy),_Lc,_Fail,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stkx,Stkx) :-
  isTrueSymb(Sy),!.
compCond(enum(Sy),_,Fail,_Opts,Lx,Lx,Dx,Dx,[iBreak(Fail)|Cx],Cx,Stkx,Stkx) :-
  isFalseSymb(Sy),!.
compCond(cnj(Lc,A,B),OLc,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCond(A,Lc,Fail,Opts,L,L1,D,D1,C,C1,Stk,Stk1),
  compCond(B,Lc,Fail,Opts,L1,Lx,D1,Dx,C1,Cx,Stk1,Stkx).
compCond(dsj(Lc,A,B),OLc,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatTp,[iLbl(Fl,iBlock(FlatTp,AC))|BC]))|Cx]),
  mkFnTipe([],tplTipe([]),FlatTp),
  genLbl(L,Fl,L0),
  genLbl(L0,Ok,L1),
  compCond(A,Lc,Fl,Opts,L1,L2,D,D0,AC,[iBreak(Ok)],Stk,Stka),
  compCond(B,Lc,Fail,Opts,L2,Lx,D,D1,BC,[],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"disjunction").
compCond(ng(Lc,A),OLc,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,[iBlock(FlatTp,[iLbl(Fl,iBlock(FlatTp,AC))])|Cx]),
  mkFnTipe([],tplTipe([]),FlatTp),
  genLbl(L,Fl,L0),
  compCond(A,Lc,Fl,Opts,L0,Lx,D,Dx,AC,[iBreak(Fail)],Stk,Stkx).
compCond(cnd(Lc,T,A,B),OLc,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  mkFnTipe([],tplTipe([]),FlatTp),
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(FlatTp,[iLbl(El,iBlock(FlatTp,CA))]))|C0]),
  genLbl(L,El,L0),
  compCond(T,Lc,El,Opts,L0,L1,D,D1,C0,CA,Stk,Stk0),
  genLbl(L1,Ok,L2),
  compCond(A,Lc,Fail,Opts,L2,L3,D1,D2,CA,[iBreak(Ok)],Stk0,Stka),
  compCond(B,Lc,Fail,Opts,L3,Lx,D2,Dx,CB,[iBreak(Ok)],Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"conditional").
compCond(mtch(Lc,P,E),OLc,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,Opts,L,L1,D,D1,C0,C1,Stk,Stka),
  compPtn(P,Lc,Fail,Opts,L1,Lx,D1,Dx,C1,Cx,Stka,Stkb),
  mergeStkLvl(Stk,Stkb,Stkx,"pattern conditional").
compCond(E,Lc,Fail,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compExp(E,Lc,Opts,L,Lx,D,Dx,C,[iIfNot(Fail)|Cx],Stk,Stka),
  mergeStkLvl(Stk,Stka,Stkx,"pattern conditional").

% compile an expression
compExp(voyd,_,_Opts,Lx,Lx,Dx,Dx,[iLdV|Cx],Cx,Stk,Stkx) :-!,
  bumpStk(Stk,Stkx).
compExp(Trm,_Lc,_Opts,Lx,Lx,Dx,Dx,[iLdC(Trm)|Cx],Cx,Stk,Stkx) :-
  isLiteral(Trm),!,
  bumpStk(Stk,Stkx).
compExp(idnt(Nm),Lc,_Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  (lclVar(Nm,V,D) -> 
   compVar(V,L,Lx,D,Dx,C,Cx,Stk,Stkx) ;
   reportError("cannot locate variable %s",[id(Nm)],Lc),
   abort).
compExp(ctpl(St,A),Lc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compExps(A,Lc,Opts,L,Lx,D,Dx,C,[iAlloc(St)|C1],Stk,_Stka),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C1,Cx).
compExp(ecll(Lc,Nm,A),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  isIntrinsic(Nm,_,Op),!,
  chLine(Opts,OLc,Lc,C,C0),
  compExps(A,Lc,Opts,L,Lx,D,Dx,C0,[Op|C1],Stk,_Stka),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C1,Cx).
compExp(ecll(Lc,Nm,A),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExps(A,Lc,Opts,L,Lx,D,Dx,C0,[iEscape(Nm)|C1],Stk,_Stka),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C1,Cx).
compExp(cll(Lc,Nm,A),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExps(A,Lc,Opts,L,Lx,D,Dx,C0,[iCall(Nm)|C1],Stk,_Stka),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C1,Cx).
compExp(ocall(Lc,O,A),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  length(A,Ar),
  Arity is Ar+1,
  compExps(A,Lc,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(O,Lc,Lc,L1,Lx,D1,Dx,C1,[iOCall(arity)|C2],Stka,_),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C2,Cx).
compExp(voke(Lc,K,A),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  length(A,Ar),
  Arity is Ar+1,
  compExps(A,Lc,L,L1,D,D1,C0,C1,Stk,Stka),
  compExp(K,Lc,L1,Lx,D1,Dx,C1,[iInvoke(Arity)|C2],Stka,_Stkb),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C2,Cx).
compExp(clos(Lb,Ar,Free),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compExp(Free,OLc,Opts,L,Lx,D,Dx,C,[iClosure(lbl(Lb,Ar))|C1],Stk,_Stka),
  bumpStk(Stk,Stkx),
  frameIns(Stkx,C1,Cx).
compExp(nth(Lc,Exp,Off),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Opts,L,Lx,D,Dx,C0,[iNth(Off)|Cx],Stk,_Stka),
  bumpStk(Stk,Stkx).
compExp(setix(Lc,Exp,Off,Vl),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Opts,L,L1,D,D1,C,C1,Stk,Stk0),
  compExp(Vl,Lc,Opts,L1,Lx,D1,Dx,C1,[iStNth(Off)|Cx],Stk0,_Stkx).
compExp(cel(Lc,Exp),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Exp,Lc,Opts,L,Lx,D,Dx,C,[iCell|C1],Stk,Stkx),
  frameIns(Stkx,C1,Cx).
compExp(get(Lc,Exp),_,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compExp(Exp,Lc,Opts,L,Lx,D,Dx,C,[iGet|Cx],Stk,Stkx).
compExp(set(Lc,Cl,Val),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Val,Lc,Opts,L,L1,D,D1,C,C1,Stk,Stk1),
  compExp(Cl,Lc,Opts,L1,Lx,D1,C1,[iSet|Cx],Stk1,_Stkx).
compExp(case(Lc,T,Cases,Deflt),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compCase(T,Lc,Cases,Deflt,gencode:compExp,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compExp(unpack(Lc,T,Cases),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(T,Lc,Opts,L,L1,D,D1,C,C1,Stk,Stk1),
  compCnsCases(Cases,Lc,gencode:compExpCase,Opts,L1,Lx,D1,Dx,C1,Cx,Stk1,Stkx).
compExp(try(Lc,B,T,E,H),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stk) :-!,
  compTry(Lc,B,T,E,H,OLc,gencode:compExp,Opts,L,Lx,D,Dx,C,Cx,Stk).
compExp(varNames(Lc,Vrs,T),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),
  populateVarNames(Vrs,Lc,D,C0,C1),
  compExp(T,Lc,Opts,L,Lx,D,Dx,C1,Cx,Stk,Stkx).
compExp(ltt(Lc,idnt(Nm),Val,Exp),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  genLbl(L,Lb,L1),
  chLine(Opts,OLc,Lc,C,C0),!,
  defineLclVar(Nm,Opts,D,D1,Off,C,[iStV(Off)|C1]),
  compExp(Val,Lc,Opts,L,L1,D1,D2,C1,[iStL(Off)|C2],Stk,Stk1),
  verify(gencode:bumpStk(Stk,1,Stk1),"expecting a single bump in stack"),
  compExp(Exp,Lc,Lc,Opts,L1,L2,D2,Dx,C2,Cx,Stk,Stkx).
compExp(error(Lc,Msg),_OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compAbort(Lc,Msg,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx). % no continuation after an error
compExp(rais(Lc,T,E),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,none) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(E,Lc,Opts,L,L1,D,D1,C0,C1,Stk,Stk1),
  compExp(T,Lc,Opts,L1,Lx,D1,Dx,C1,[iThrow|Cx],Stk1,_).
compExp(cnd(Lc,Cnd,A,B),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,[iLbl(Ok,iBlock(BlkTp,[iLbl(Fl,iBlock(BlkTp,AC))|BC]))|Cx]),
  genLbl(L,Fl,L0),
  genLbl(L0,Ok,L1),
  mkFnTipe([],tplTipe([ptrTipe]),BlkTp),
  compCond(Cnd,Lc,Fl,Opts,L1,L2,D,D1,AC,AC1,Stk,Stk0),
  verify(Stk=Stk0,"conditions should not increase stack"),
  compExp(A,Lc,Opts,L2,L3,D1,D2,AC1,[iBreak(Ok)],Stk,Stka),
  compExp(B,Lc,Opts,L3,Lx,D1,D3,BC,[iBreak(Ok)],Stk,Stkb),
  resetVars(D2,D3,Dx),
  mergeStkLvl(Stka,Stkb,Stkx,"conditional expression stack").
compExp(seqD(Lc,A,B),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(A,Lc,Opts,L,L1,D,D1,C,C1,Stk,Stk1),
  resetStack(Stk,Stk1,C1,C2),
  compExp(B,Lc,Opts,L1,Lx,D1,Dx,C2,Cx,Stk,Stkx).
compExp(vlof(Lc,A),_,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  compAction(A,Lc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compExp(tsk(Lc,F),OLc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,[iFiber|C0]),!,
  compExp(F,Lc,Opts,L,Lx,D,Dx,C0,Cx,Stk,Stkx).
compExp(Cond,Lc,Opts,L,Lx,D,Dx,[iLbl(Ok,iBlock(BlkTp,[iLbl(Fl,iBlock(BlkTp,C1)),iLdC(enum(False)),iBreak(Ok)]))|Cx],Cx,Stk,Stkx) :-
  isCond(Cond),!,
  isTrueSymb(True),
  isFalseSymb(False),
  mkFnTipe([],tplTipe([blTipe]),BlkTp),
  genLbl(L,Ok,L0),
  genLbl(L0,Fl,L1),
  compCond(Cond,Lc,Fl,Opts,L1,Lx,D,Dx,C1,[iLdC(enum(True)),iBreak(Ok)],Stk,Stkx).
compExp(T,Lc,_Opts,Lx,Lx,Dx,Dx,C,C,Stk,Stk) :-
  reportError("cannot compile %s",[ltrm(T)],Lc),
  abort.

/* Argument terms are generated in reverse order*/
compExps([],_,_Opts,Lx,Lx,Dx,Dx,Cx,Cx,Stk,Stk) :-!.
compExps([T|Ts],Lc,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compExps(Ts,Lc,Opts,L0,L1,D,D1,C,C0,Stk,Stk0),
  compExp(T,Lc,Opts,L1,Lx,D1,Dx,C0,Cx,Stk0,Stkx).

isCond(cnj(_,_,_)).
isCond(cnd(_,_,_,_)).
isCond(dsj(_,_,_)).
isCond(ng(_,_)).
isCond(mtch(_,_,_)).

isTrueSymb("star.core#true").
isFalseSymb("star.core#false").

compCase(Gv,Lc,Cases,Deflt,Hndlr,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx) :-
  compExp(Gv,Lc,Opts,L,L1,D,D1,C,
	  [iLbl(Ok,iBlock(BlkTp,
			  [iLbl(Df,iBlock(BlkTp,[iCase(Mx)|C1]))|DC]))|Cx],Stk,Stk1),
  genLbl(L,Df,L0),
  genLbl(L0,Ok,L1),
  genCaseTable(Cases,Mx,Table),
  mkFnTipe([],tplTipe([ptrTipe]),BlkTp),

  compCases(Table,0,Mx,Ok,Df,Hndlr,Opts,L1,L2,D1,D2,C1,[iBreak(Df)],Stk1,Stka),
  call(Hndlr,Deflt,Lc,Opts,L2,Lx,D2,Dx,DC,[iBreak(Ok)],Stk1,Stkb),
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

compCases([],Ix,Mx,_Ok,_Df,_Hndlr,_Opts,Lx,Lx,D,D,Cx,Cx,_Stk,none) :-
  Ix>=Mx.
compCases([],Ix,Mx,Ok,Dflt,Hndlr,Opts,L,Lx,D,Dx,[iBreak(Dflt)|C],Cx,Stk,Stkx) :-
  Ix1 is Ix+1,
  compCases([],Ix1,Mx,Ok,Dflt,Hndlr,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).
compCases([(Ix,Case)|Cs],Ix,Mx,Ok,Dflt,Hndlr,Opts,L,Lx,D,Dx,[iBlock(BlkTp,CC)|C],Cx,Stk,Stkx) :-!,
  compCaseBranch(Case,Ok,Dflt,Hndlr,Opts,L,L1,D,D1,CC,Stk,Stk1),
  Ix1 is Ix+1,
  compCases(Cs,Ix1,Mx,Ok,Dflt,Brks,Opts,L1,Lx,D1,Dx,C,Cx,Stk,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"case branch").
compCases(Cs,Ix,Mx,Ok,Dflt,Hndlr,Opts,L,Lx,D,Dx,[iBreak(Dflt)|C],Cx,Stk,Stkx) :-
  Ix1 is Ix+1,
  compCases(Cs,Ix1,Mx,Ok,Dflt,Hndlr,Opts,L,Lx,D,Dx,C,Cx,Stk,Stkx).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E,Lc)],Ok,Dflt,Hndlr,Opts,L,Lx,D,Dx,C,Stk,Stkx) :-!,
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,Dflt,Opts,L2,L3,D,D2,C0,C1,Stk,Stk1),
  call(Hndlr,E,Lc,Opts,L3,Lx,D2,Dx,C1,[iBreak(Ok)],Stk1,Stkx).
compCaseBranch([(P,E,Lc)|SC],Ok,Dflt,Hndlr,Opts,L,Lx,D,Dx,[iTL(Off),iLbl(Fl,iBlock(BlkTp,PC))|C],Stk,Stkx) :-
  genLbl(L,Fl,L1),
  defineLclVar("__",Opts,D,D1,Off,C,C0),
  genLine(Opts,Lc,C0,C1),
  compPtn(P,Lc,Fl,Opts,L1,L2,D1,D2,PC,PC1,Stk,Stk0),
  call(Hndlr,E,Lc,Ok,Opts,L2,L3,D2,D3,PC1,[iBreak(Ok)],Stk0,Stka),
  resetVars(D1,D3,D4),
  compMoreCase(SC,Off,Ok,Dflt,Hndlr,Opts,L3,Lx,D4,Dx,C,Stk0,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"case branch stack").

compMoreCase([],_Vlb,_Ok,Dflt,_Hndlr,_Opts,Lx,Lx,Dx,Dx,[iBreak(Fail)],Stkx,Stkx) :-!.
compMoreCase([(P,E,Lc)|SC],VLb,Ok,Dflt,Hndlr,Opts,L,Lx,D,Dx,[iLdL(VLb),iLbl(Fl,iBlock(BlkTp,PC))|C],Stk,Stkx) :-
  genLbl(L,Fl,L1),
  bumpStk(Stk,Stk0),
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,Fl,Opts,L1,L2,D,D1,PC,PC1,Stk,Stk0),
  resetVars(D,D1,D2),
  call(Hndlr,E,Lc,Opts,L2,L3,D1,D2,PC1,[iBreak(Ok)],Stk0,Stka),
  compMoreCase(SC,VLb,Ok,Dflt,Hndlr,Opts,L3,Lx,D2,Dx,C,Stk,Stkb),
  mergeStkLvl(Stka,Stkb,Stkx,"more case branch stack").

compCnsCases([(ctpl(Cn,Args),E,Lc)],_,CompRhs,Cont,Brks,Opts,L,Lx,D,Dx,
	     [iUnpack(Cn,Fl)|C],Cx,Stk,Stkx) :-!,
  genLbl(L,Nxt,L1),
  genLbl(L1,Fl,L2),
  compAbort(Lc,strg("unpack failed"),Brks,Opts,L2,L4,D,D2,Fl,Cy,Cx,Stk,_),
  length(Args,Ar),

  dropStk(Stk,1-Ar,Stk0),
  compPtnArgs(Args,Lc,stkArgCont,0,contCont(Nxt),contCont(Fl),
	      Fl,Brks,Opts,L4,L5,D2,D3,Fl,C,[iLbl(Nxt)|A1],Stk0,Stk1),
  call(CompRhs,E,Lc,Cont,Brks,Opts,L5,Lx,D3,Dx,Fl,A1,[iLbl(Fl)|Cy],Stk1,Stkx).
compCnsCases(Cases,Lc,CompRhs,Cont,Brks,Opts,L,Lx,D,Dx,
	     [iIndxJmp(Mx)|T0],Cx,Stk,Stkx) :-
  splitCont(Lc,Cont,OC),
  length(Cases,Mx),
  compArms(Cases,Lc,CompRhs,OC,Brks,Opts,L,Lx,D,Dx,T0,Tx,Tx,Cx,Stk,Stkx).

compArms([],_Lc,_,_,_Cont,_Brks,_Opts,Lx,Lx,Dx,Dx,Tc,Tc,Cx,Cx,Stk,Stk).
compArms([(ctpl(Cn,Args),E,Lc)],_Lc,CompRhs,Cont,Brks,Opts,L,Lx,D,Dx,
	 [iJmp(Lbl)|Tx],Tx,
	 [iLbl(Lbl),iUnpack(Cn,End)|C],Cx,Stk,Stkx) :-!,
  genLbl(L,Lbl,L0),
  genLbl(L0,Nxt,L1),
  genLbl(L1,End,L2),

  compAbort(Lc,strg("arm failed"),Brks,Opts,L2,L3,D,D3,Cy,Cx,Stk,_),

  length(Args,Ar),
  dropStk(Stk,1-Ar,Stk0),
 
  compPtnArgs(Args,Lc,stkArgCont,0,contCont(Nxt),contCont(End),
	      End,Brks,Opts,L3,L4,D3,D4,C,[iLbl(Nxt)|A1],Stk0,Stk1),
  call(CompRhs,E,Lc,Cont,End,Brks,Opts,L4,Lx,D4,Dx,A1,[iLbl(End)|Cy],Stk1,Stkx).
compArms([(ctpl(Cn,Args),E,Lc)|Cases],_Lc,CompRhs,Cont,Brks,Opts,L,Lx,D,Dx,
	 [iJmp(Lbl)|T1],Tx,
	 [iLbl(Lbl),iUnpack(Cn,End)|C],Cx,Stk,Stkx) :-
  genLbl(L,Lbl,L0),
  genLbl(L0,Nxt,L1),
  genLbl(L1,End,L2),
  compAbort(Lc,strg("arm failed"),Brks,Opts,L2,L3,D,D3,Cy,Cz,Stk,_),
  length(Args,Ar),
  dropStk(Stk,1-Ar,Stk0),
  compPtnArgs(Args,Lc,stkArgCont,0,contCont(Nxt),contCont(End),
	      End,Brks,Opts,L3,L4,D3,D4,C,[iLbl(Nxt)|A1],Stk0,Stk1),
  call(CompRhs,E,Lc,Cont,End,Brks,Opts,L4,L5,D4,D5,A1,[iLbl(End)|Cy],Stk1,Stk2),
  compArms(Cases,Lc,CompRhs,Cont,Brks,Opts,L5,Lx,D5,Dx,T1,Tx,Cz,Cx,Stk,Stk3),
  mergeStkLvl(Stk2,Stk3,Stkx,"bad unpack cases").
