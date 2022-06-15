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
  mkTpl([PT,ImpTpl,DTpl,LDTpl,Cdes],Tp),
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
  encType(Tp,Sig),
  genLbl([],Ex,L0),
  genLbl(L0,End,L1),
  buildArgs(Args,0,D,D1),
  genLine(Opts,Lc,C0,C1),
  compPtnArgs(Args,Lc,argCont,
	      trapCont(Lc),0,contCont(Ex),abortCont(Lc,strg("def failed"),Opts),Opts,L1,L2,D1,D2,End,
	      C1,[iLbl(Ex)|C2],some(0),Stk0),
  compExp(Value,Lc,retCont(Opts),trapCont(Lc),Opts,L2,_Lx,D2,Dx,End,C2,[iLbl(End)],Stk0,_Stk),
  findMaxLocal(Dx,Mx),
%  (is_member(showGenCode,Opts) -> dispIns(func(Nm,H,Sig,Mx,C0));true ),
  peepOptimize(C0,Cde),
  (is_member(showGenCode,Opts) -> dispIns(func(Nm,H,Sig,Mx,Cde));true ),
  assem(func(Nm,H,Sig,Mx,Cde),CdTrm).
genDef(D,Opts,glbDef(Lc,Nm,Tp,Value),O,[Cd|O]) :-
  encType(funType(tplType([]),Tp),Sig),
  genLbl([],End,L1),
  genLine(Opts,Lc,C0,C1),
  compExp(Value,Lc,bothCont(glbCont(Nm),rtgCont(Opts)),trapCont(Lc),
	   Opts,L1,_Lx,D,Dx,End,C1,[iLbl(End)],some(0),_Stk),
  findMaxLocal(Dx,Mx),
  peepOptimize(C0,Cde),
  (is_member(showGenCode,Opts) -> dispIns(func(lbl(Nm,0),hard,Sig,Mx,Cde));true ),
  assem(func(lbl(Nm,0),hard,Sig,Mx,Cde),Cd).
genDef(_,_,lblDef(_,Lbl,Tp,Ix),O,[LblTrm|O]) :-
  encType(Tp,Sig),
  assem(struct(Lbl,strg(Sig),Ix),LblTrm).
genDef(_,_,typDef(_,Tp,Rl,IxMap),O,[TpTrm|O]) :-
  assem(tipe(Tp,Rl,IxMap),TpTrm).

glbCont(Nm,Lx,Lx,D,D,_,[iTG(Nm)|Cx],Cx,Stk,Stk).

retCont(Opts,Lx,Lx,D,D,_,C,Cx,_Stk,none) :-
  genDbg(Opts,C,[iRet|Cx]).

rtgCont(_Opts,Lx,Lx,D,D,_,[iRtG|Cx],Cx,_Stk,none).

dropCont(Lx,Lx,D,D,_,[iDrop|Cx],Cx,Stk,Stk1) :-
  dropStk(Stk,1,Stk1).

trapCont(_Lc,Lx,Lx,D,D,_,Cx,Cx,none,none) :- !.
trapCont(Lc,Lx,Lx,D,D,_,Cx,Cx,Stk,Stk) :-
  reportError("not permitted to raise an exception",[],Lc).

idxCont(Off,Lx,Lx,Dx,Dx,_End,[iNth(Off)|Cx],Cx,Stkx,Stkx).

sxCont(Off,Cont,L,Lx,D,Dx,End,[iStNth(Off)|C],Cx,Stk,Stkx) :-
  dropStk(Stk,2,Stk0),
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk0,Stkx).

txCont(Off,Cont,L,Lx,D,Dx,End,[iTNth(Off)|C],Cx,Stk,Stkx) :-
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

resetVrCont(scope(Vrs,FrReg,M0),Cont,L,Lx,scope(_,_,M1),Dx,End,C,Cx,Stk,Stkx) :-
  Mx is max(M0,M1),
  call(Cont,L,Lx,scope(Vrs,FrReg,Mx),Dx,End,C,Cx,Stk,Stkx).

resetVars(scope(Vrs,FrReg,M0),scope(_,_,M1),scope(Vrs,FrReg,Mx)) :-
  Mx is max(M0,M1).

mergeVars(scope(V1,Fr1,M1),scope(V2,Fr2,M2),scope(Vx,Frx,Mx)) :-
  intersect(V1,V2,Vx),
  intersect(Fr1,Fr2,Frx),
  Mx is max(M1,M2).

initDict(scope([],[],0)).

buildArgs([],_,D,D).
buildArgs([A|R],Ix,D,Dx) :-
  buildArg(A,Ix,D,D0),
  Ix1 is Ix+1,
  buildArgs(R,Ix1,D0,Dx).

buildArg(idnt(Nm),Ix,scope(D,FreeRg,Mx),
	 scope([(Nm,a(Ix),void,void)|D],FreeRg,Mx)).
buildArg(_,_,D,D).

lclVar(Nm,Wh,scope(Vrs,_,_)) :-
  is_member((Nm,Wh,_,_),Vrs),!.

defineLclVar(Nm,Lbl,End,Opts,scope(Vrs,FreeRg,Mx),scope([(Nm,l(Off),Lbl,End)|Vrs],NFreeRg,Mx1),Off,C,Cx) :-
  nextFreeOff(FreeRg,Mx,Off,NFreeRg,Mx1),
  genDebug(Opts,iLocal(Nm,Lbl,End,Off),C,Cx).

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
  reportError("variable %s not known",[Ex],Lc).

genLbl(Lbs,Lb,[Lb|Lbs]) :-
  length(Lbs,N),
  swritef(Lb,"_L%d",[N]).

findMaxLocal(scope(_,_,Mx),Mx).

localMx((_,l(Off),_),M,Mx) :- !, Mx is max(Off,M).
localMx(_,M,M).

compExp(voyd,_,Cont,_TCont,_Opts,L,Lx,D,Dx,End,[iLdV|C0],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk1,Stkx).
compExp(Trm,_Lc,Cont,_ECont,_Opts,L,Lx,D,Dx,End,[iLdC(Trm)|C],Cx,Stk,Stkx) :-
  isLiteral(Trm),!,
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk1,Stkx).
compExp(idnt(Nm),Lc,Cont,_TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  (lclVar(Nm,V,D) -> 
   compVar(V,Opts,Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) ;
   reportError("cannot locate variable %s",[Nm],Lc),
   C=[iLdV|Cx],
   bumpStk(Stk,Stkx),
   D=Dx,L=Lx).
compExp(ctpl(St,A),Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stk2) :-!,
  compExps(A,Lc,bothCont(allocCont(St),Cont),TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stk2).
compExp(intrinsic(Lc,Op,A),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  bumpStk(Stk,Stk1),
  compExps(A,Lc,bothCont(asmCont(Op,Stk1),Cont),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compExp(ecll(Lc,Nm,A),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExps(A,Lc,bothCont(escCont(Nm,Stk),Cont),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compExp(cll(Lc,Nm,A),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  length(A,Ar),
  compExps(A,Lc,cllCont(Nm,Ar,Cont,Opts),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compExp(ocall(Lc,Fn,A),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  length(A,Ar),
  Arity is Ar+1,
  compExps(A,Lc,compExp(Fn,Lc,oclCont(Arity,Cont,Opts),TCont,Opts),TCont,
	    Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compExp(nth(Lc,Exp,Off),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(Exp,Lc,bothCont(idxCont(Off),Cont),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compExp(setix(Lc,Exp,Off,Vl),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(Exp,Lc,
	   compExp(Vl,Lc,
		    sxCont(Off,Cont),TCont,Opts),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compExp(cel(Lc,Exp),_,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  bumpStk(Stk,Stk1),
  compExp(Exp,Lc,bothCont(asmCont(iCell,Stk1),Cont),
	   TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compExp(get(Lc,Exp),_,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  bumpStk(Stk,Stk1),
  compExp(Exp,Lc,bothCont(asmCont(iGet,Stk1),Cont),
	   TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compExp(set(Lc,Cl,Val),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compExp(Val,Lc,
	   compExp(Cl,Lc,
		    bothCont(asmCont(iSet,Stk),Cont),
		    TCont,Opts),
	   TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compExp(case(Lc,T,Cases,Deflt),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compCase(T,Lc,Cases,Deflt,Cont,TCont,gencode:compExp,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compExp(unpack(Lc,T,Cases),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,Nxt,L1),
  compExp(T,Lc,contCont(Nxt),TCont,Opts,L1,L2,D,D2,End,C0,[iLbl(Nxt)|C1],Stk,Stk0),
  compCnsCases(Cases,Lc,gencode:compExpCase(TCont),Cont,TCont,Opts,
	       L2,Lx,D2,Dx,C1,Cx,Stk0,Stkx).
compExp(varNames(Lc,Vrs,T),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  populateVarNames(Vrs,Lc,D,C0,C1),!,
  compExp(T,Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C1,Cx,Stk,Stkx).
compExp(whr(Lc,T,Cnd),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCond(Cnd,Lc,compExp(T,Lc,Cont,TCont,Opts),abortCont(Lc,strg("where fail"),Opts),
	    TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compExp(ltt(Lc,idnt(Nm),Val,Exp),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Lb,L1),
  chLine(Opts,OLc,Lc,C,C0),!,
  defineLclVar(Nm,Lb,End,Opts,D,D1,Off,C0,[iStV(Off)|C1]),
  compExp(Val,Lc,stoCont(Off,Lb,Opts,
			  compExp(Exp,Lc,Cont,TCont,Opts)),TCont,Opts,
	   L1,Lx,D1,Dx,End,C1,Cx,Stk,Stkx).
compExp(error(Lc,Msg),_OLc,_Cont,_TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  abortCont(Lc,Msg,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx). % no continuation after an error
compExp(rais(Lc,E),_,_,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,none) :-
  compExp(E,Lc,TCont,trapCont(Lc),Opts,L,Lx,D,Dx,End,C,Cx,Stk,_).
compExp(cnd(Lc,T,A,B),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compCondExp(Lc,T,A,B,OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compExp(seqD(Lc,A,B),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(A,Lc,resetCont(Stk,compExp(B,Lc,Cont,TCont,Opts)),TCont,
	  Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compExp(vlof(Lc,A),_,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compAction(A,Lc,Cont,abortCont(Lc,strg("illegal action"),Opts),TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compExp(perf(Lc,A),_,Cont,_TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compAction(A,Lc,Cont,abortCont(Lc,strg("no action permitted after this"),Opts),Cont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compExp(tsk(Lc,F),_,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compExp(F,Lc,tskCont(Cont),TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compExp(susp(Lc,T,E),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stk) :-
  chLine(Opts,OLc,Lc,C,C0),
  compExp(T,Lc,compExp(E,Lc,suspendCont(Cont,Stk,Opts),TCont,Opts),TCont,Opts,
	  L,Lx,D,Dx,End,C0,Cx,Stk,_).
compExp(resme(Lc,T,E),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stk) :-
  chLine(Opts,OLc,Lc,C,C0),
  compExp(T,Lc,compExp(E,Lc,resumeCont(Cont,Stk,Opts),TCont,Opts),TCont,Opts,
	  L,Lx,D,Dx,End,C0,Cx,Stk,_).
compExp(Cond,Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  isCond(Cond),!,
  genLbl(L,Nx,L0),
  compCond(Cond,Lc,resetCont(Stk,bothCont(trueCont,contCont(Nx))),
	   resetCont(Stk,bothCont(falseCont,contCont(Nx))),
	   TCont,Opts,L0,L1,D,D1,End,C,[iLbl(Nx)|C1],Stk,Stk1),
  call(Cont,L1,Lx,D1,Dx,End,C1,Cx,Stk1,Stkx).
compExp(T,Lc,_,_,_,Lx,Lx,Dx,Dx,_,C,C,Stk,Stk) :-
  reportError("cannot compile %s",[T],Lc),
  abort.

compExpCase(TCont,T,Lc,Cont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compExp(T,Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

compCondExp(Lc,T,A,B,OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Cont,OC),!,
  compCond(T,Lc,compExp(A,Lc,OC,TCont,Opts),
	   resetCont(Stk,compExp(B,Lc,OC,TCont,Opts)),
	   TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).

compVar(a(A),_,Cont,L,Lx,D,Dx,End,[iLdA(A)|C0],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk1,Stkx).
compVar(l(X),_,Cont,L,Lx,D,Dx,End,[iLdL(X)|C0],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk1,Stkx).
compVar(g(GlbNm),Opts,Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genDbg(Opts,C,[iLdG(GlbNm)|C0]),
  genRtnDbg(Opts,C0,C1),
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C1,Cx,Stk1,Stkx).

/* Terms are generated in reverse order*/
compExps([],_,Cont,_,_,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compExps([T|Ts],Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L0),
  compExps(Ts,Lc,contCont(Nxt),TCont,Opts,L0,L1,D,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk0),
  compExp(T,Lc,Cont,TCont,Opts,L1,Lx,D1,Dx,End,C0,Cx,Stk0,Stkx).


/* Compile actions */
compAction(nop(_),_Lc,Cont,_ACont,_TCont,_Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(seq(Lc,A,B),OLc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compAction(A,Lc,Cont,resetCont(Stk,compAction(B,Lc,Cont,ACont,TCont,Opts)),TCont,
	     Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(vls(Lc,E),OLc,Cont,_ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(rse(Lc,E),OLc,_Cont,_ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,TCont,trapCont(Lc),Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(rtire(Lc,T,E),OLc,_Cont,_ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(T,Lc,compExp(E,Lc,retireCont(Opts),
		       TCont,Opts),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(perf(Lc,L),OLc,_Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),!,
  compExp(L,Lc,resetCont(Stk,ACont),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(mtch(Lc,P,E),OLc,_Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,compPtn(P,Lc,ACont,abortCont(Lc,strg("match fail")),TCont,Opts),
	  TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(asgn(Lc,P,E),OLc,_Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :- !,
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,compExp(P,Lc,bothCont(asmCont(iAssign,Stk),ACont),TCont,Opts),
	  TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(case(Lc,T,Cases,Deflt),OLc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compCase(T,Lc,Cases,Deflt,Cont,TCont,gencode:compAct(ACont),Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(whle(Lc,G,B),OLc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,
	   [iJmp(TstLbl),iLbl(LpLbl)|C],Cx,Stk,Stk) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  genLbl(L,LpLbl,L1),
  genLbl(L1,TstLbl,L2),
  compCond(G,Lc,contCont(LpLbl),resetCont(Stk,ACont),
	   TCont,Opts,L2,L3,D,D1,End,C2,Cx,Stk,Stk1),
  compAction(B,Lc,Cont,contCont(TstLbl),TCont,Opts,L3,Lx,D1,Dx,End,C0,[iLbl(TstLbl)|C2],Stk1,_).
compAction(untl(Lc,B,G),OLc,Cont,ACont,TCont,Opts,L,Lx,D,D,End,
	   [iLbl(LpLbl)|C],Cx,Stk,Stk) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  genLbl(L,LpLbl,L1),
  genLbl(L1,TstLbl,L2),
  compAction(B,Lc,Cont,contCont(TstLbl),TCont,
	     Opts,L2,L3,D,D1,End,C0,[iLbl(TstLbl)|C2],Stk,Stk1),
  compCond(G,Lc,resetCont(Stk,ACont),contCont(LpLbl),TCont,
	   Opts,L3,Lx,D1,_D1,End,C2,Cx,Stk1,_Stk2).
compAction(ltt(Lc,idnt(Nm),Val,Act),OLc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Lb,L1),
  chLine(Opts,OLc,Lc,C,C0),!,
  defineLclVar(Nm,Lb,End,Opts,D,D1,Off,C0,[iStV(Off)|C1]),
  compExp(Val,Lc,stoCont(Off,Lb,Opts,
			 compAction(Act,Lc,Cont,ACont,TCont,Opts)),TCont,Opts,
	  L1,Lx,D1,Dx,End,C1,Cx,Stk,Stkx).
compAction(iftte(Lc,G,T,E),OLc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  splitCont(Lc,ACont,AC),
  splitCont(Lc,Cont,CC),
  compCond(G,Lc,compAction(T,Lc,CC,AC,TCont,Opts),
	   resetCont(Stk,compAction(E,Lc,CC,AC,TCont,Opts)),
	   TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(iftt(Lc,G,T),OLc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  splitCont(Lc,ACont,AC),
  splitCont(Lc,Cont,CC),
  compCond(G,Lc,compAction(T,Lc,CC,AC,TCont,Opts),resetCont(Stk,AC),
	   TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(unpack(Lc,T,Cases),OLc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,Nxt,L1),
  compExp(T,Lc,contCont(Nxt),TCont,Opts,L1,L2,D,D2,End,C0,[iLbl(Nxt)|C1],Stk,Stk0),
  compCnsCases(Cases,Lc,gencode:compActCase(ACont,TCont),Cont,TCont,Opts,
	       L2,Lx,D2,Dx,C1,Cx,Stk0,Stkx).
compAction(try(Lc,B,E,H),OLc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stk) :-
  compTry(Lc,B,E,H,OLc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk).

compAction(A,Lc,_,_,_,_,Lx,Lx,Dx,Dx,_,C,C,Stk,Stk) :-
  reportError("cannot compile action %s",[lact(A)],Lc),
  abort.

compActCase(ACont,TCont,A,Lc,Cont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compAction(A,Lc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

compAct(ACont,A,Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compAction(A,Lc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

compTry(Lc,B,idnt(E),H,OLc,Cont,ACont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,Ctch,L1),
  compAction(B,Lc,Cont,ACont,jmpCont(Ctch),Opts,L1,L2,D,D0,Ctch,C0,
	     [iLbl(Ctch),iStL(Off),iLbl(ELb)|C1],Stk,_),
  genLbl(L2,ELb,L3),
  defineLclVar(E,ELb,End,Opts,D0,D1,Off,C1,C2),
  compAction(H,Lc,Cont,ACont,TCont,Opts,L3,Lx,D1,Dx,End,C2,Cx,Stk,_).

  
/* Compile actions as sequences with several possible continuations */

contCont(Lbl,Lx,Lx,D,D,_,C,Cx,Stk,Stk) :-
  (nonvar(Cx),Cx=[iLbl(Lbl)|_]) ->
  C=Cx ;
  C=[iJmp(Lbl)|Cx].			% some special logic here

bothCont(C1,C2,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(C1,L,L1,D,D0,End,C,C0,Stk,Stk0),
  call(C2,L1,Lx,D0,Dx,End,C0,Cx,Stk0,Stkx).

frameSig(Stk,strg(Sig)) :-
  mkTplTipe(Stk,FrTp),
  encLtp(FrTp,Sig).

frameIns(some(Stk),[iFrame(Sig)|Cx],Cx) :-
  frameSig(Stk,Sig).
frameIns(none,Cx,Cx).

allocCont(Str,Lx,Lx,D,D,_,[iAlloc(Str)|C],Cx,Stk,Stkx) :-
  frameIns(Stk,C,Cx),
  popStack(Str,Stk,Stkx).

popStack(lbl(_,Ar),St,Stx) :-
  dropStk(St,Ar-1,Stx).

stkLvl(some(Lvl),Lvl).

resetCont(Stk,Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
resetCont(Stk,Cont,L,Lx,D,Dx,End,[iDrop|C],Cx,Stk0,Stkx) :-
  dropStk(Stk0,1,Stk),
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
resetCont(some(Lvl),Cont,L,Lx,D,Dx,End,[iRst(Lvl)|C],Cx,_,Stkx) :-
  call(Cont,L,Lx,D,Dx,End,C,Cx,some(Lvl),Stkx).

stoCont(Off,Lb,_Opts,Cont,L,Lx,D,Dx,End,[iStL(Off),iLbl(Lb)|C],Cx,Stk,Stkx) :-!,
  dropStk(Stk,1,Stk1),
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk1,Stkx).

releaseCont(Nm,Lx,Lx,D,Dx,_,Cx,Cx,Stk,Stk) :-
  clearLclVar(Nm,D,Dx).

asmCont(IOp,Stkx,Lx,Lx,D,D,_,[IOp|Cx],Cx,_,Stkx).

escCont(Nm,Stk0,Lx,Lx,D,D,_,[iEscape(Nm)|C],Cx,_Stk,Stkx) :-
  bumpStk(Stk0,Stkx),
  frameIns(Stkx,C,Cx).

cllCont(Nm,_Ar,retCont(_),Opts,Lx,Lx,Dx,Dx,_,C,Cx,_Stk,none) :-!,
  genDbg(Opts,C,[iTCall(Nm)|Cx]).
cllCont(Nm,Ar,Cont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genDbg(Opts,C,[iCall(Nm)|C0]),
  dropStk(Stk,Ar,Stk0),
  bumpStk(Stk0,Stk1),
  frameIns(Stk,C0,C1),
  call(Cont,L,Lx,D,Dx,End,C1,Cx,Stk1,Stkx).

oclCont(Arity,retCont(_),Opts,Lx,Lx,Dx,Dx,_End,C,Cx,_,none) :-!,
  genDbg(Opts,C,[iTOCall(Arity)|Cx]).
oclCont(Arity,Cont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genDbg(Opts,C,[iOCall(Arity)|C0]),
  dropStk(Stk,Arity,Stk0),
  bumpStk(Stk0,Stk1),
  frameIns(Stk,C0,C1),
  call(Cont,L,Lx,D,Dx,End,C1,Cx,Stk1,Stkx).

tskCont(Cont,L,Lx,D,Dx,End,[iTask|C],Cx,Stk,Stkx) :-
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

suspendCont(Cont,Stk,Opts,L,Lx,D,Dx,End,C,Cx,_Stk,Stkx) :-
  genDbg(Opts,C,[iSuspend|C0]),
  bumpStk(Stk,Stk1),
  frameIns(Stk1,C0,C1),
  call(Cont,L,Lx,D,Dx,End,C1,Cx,Stk1,Stkx).

resumeCont(Cont,Stk,Opts,L,Lx,D,Dx,End,C,Cx,_Stk,Stkx) :-
  genDbg(Opts,C,[iResume|C0]),
  bumpStk(Stk,Stk1),
  frameIns(Stk1,C0,C1),
  call(Cont,L,Lx,D,Dx,End,C1,Cx,Stk1,Stkx).

retireCont(Opts,Lx,Lx,Dx,Dx,_End,C,Cx,_Stk,none) :-
  genDbg(Opts,C,[iRetire|Cx]).

jmpCont(Lbl,Lx,Lx,D,D,_End,[iJmp(Lbl)|Cx],Cx,Stk,Stk).

mkLbledCont(L,Lx,Cont,lblCont(Lbl,Cont)) :-
  genLbl(L,Lbl,Lx).

lblCont(Lbl,Cont,L,Lx,D,Dx,End,[iLbl(Lbl)|C],Cx,Stk,Stkx) :-
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

contHasLbl(lblCont(Lbl,_),Lbl) :-!.
contHasLbl(jmpCont(Lbl),Lbl) :-!.
contHasLbl(contCont(Lbl),Lbl) :- !, nonvar(Lbl).
contHasLbl(bothCont(C1,_),Lbl) :- !, contHasLbl(C1,Lbl).
contHasLbl(onceCont(_,L,_),Lbl) :- nonvar(L), !, L=(Lbl,_,_).
contHasLbl(onceCont(_,_,C),Lbl) :- !, contHasLbl(C,Lbl).

isSimpleCont(retCont(_)).
isSimpleCont(rtgCont(_)).
isSimpleCont(jmpCont(_)).
isSimpleCont(contCont(_)).
isSimpleCont(bothCont(L,R)) :-
  isSimpleCont(L),
  isSimpleCont(R).
isSimpleCont(resetCont(_,C)) :- isSimpleCont(C).
isSimpleCont(trueCont).
isSimpleCont(falseCont).

splitCont(_,Cont,Cont) :- isSimpleCont(Cont),!.
splitCont(Lc,Cont,onceCont(Lc,_,Cont)).

onceCont(_,Lf,Cont,L,Lx,D,Dx,End,[iLbl(Lb)|C],Cx,Stk,Stkx) :-
  var(Lf),!,
  genLbl(L,Lb,L0),
  call(Cont,L0,Lx,D,Dx,End,C,Cx,Stk,Stkx),
  Lf=(Lb,Stk,Stkx).
onceCont(_,(Lb,Stkin,Stkout),_,Lx,Lx,D,D,_,C,Cx,Stk,Stkout) :-
  reconcileStack(Stk,Stkin,Stkout,C,[iJmp(Lb)|Cx]).
onceCont(Lc,(_,Stkin,Stkout),_,Lx,Lx,D,D,_,Cx,Cx,Stk,Stkout) :-
  reportError("cannot reconcile stacks [%s,%s]",[Stk,Stkin],Lc).

onceTriggered(onceCont(_,L,_)) :- nonvar(L).

reconcileStack(_,_,none,C,C) :-!.
reconcileStack(Stk,Stk,_,C,C) :-!.
reconcileStack(some(Stki),some(Stk),_,[iRst(Stk)|C],C) :-
  Stki>Stk,!.

trueCont(Lx,Lx,D,D,_,[iLdC(enum("star.core#true"))|Cx],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stkx).

falseCont(Lx,Lx,D,D,_,[iLdC(enum("star.core#false"))|Cx],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stkx).

nullCont(Lx,Lx,D,D,_,C,C,Stk,Stk).

insCont(Ins,Lx,Lx,D,D,_,[Ins|C],C,Stk,Stk).

abortCont(Lc,Msg,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  locTerm(Lc,LT),
  compExps([LT,Msg],Lc,abrtCont(Opts),trapCont(Lc),Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

abrtCont(Opts,Lx,Lx,D,D,_,C,Cx,_,none) :-
  genDbg(Opts,C,[iAbort|Cx]).

indexCont(Ix,Lx,Lx,D,D,_,Stk,Stk1,[iDup,iNth(Ix)|Cx],Cx) :-
  bumpStk(Stk,Stk1).

stkArgCont(_,Lx,Lx,D,D,_,Stk,Stk,C,C).

argCont(_Ix,Lx,Lx,D,D,_,Stk,Stk,Cx,Cx).

chLine(_,Lc,Lc,C,C) :- !.
chLine(Opts,_,Lc,C,Cx) :-
  genLine(Opts,Lc,C,Cx).

genLine(Opts,Lc,[iLine(Lt)|Cx],Cx) :-
  is_member(debugging,Opts),!,
  locTerm(Lc,Lt).
genLine(_,_,Cx,Cx).

genRtnDbg(Opts,[iDBug,iNop|Cx],Cx) :-
  is_member(debugging,Opts),!.
genRtnDbg(_,Cx,Cx).

genDbg(Opts,[iDBug|Cx],Cx) :-
  is_member(debugging,Opts),!.
genDbg(_,Cx,Cx).

compPtn(voyd,_,Succ,_Fail,_TCont,_Opts,L,Lx,D,Dx,End,[iDrop|C],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stk1),
  call(Succ,L,Lx,D,Dx,End,C,Cx,Stk1,Stkx).
compPtn(Lit,_,Succ,Fail,_TCont,_Opts,L,Lx,D,Dx,End,[iLdC(Lit),iCmp(Fl)|C],Cx,Stk,Stkx) :-
  isLiteral(Lit),!,
  dropStk(Stk,1,Stk1),
  ptnTest(Succ,Fail,Fl,L,Lx,D,Dx,End,C,Cx,Stk1,Stkx).
compPtn(idnt(Nm),_,Succ,_,_,Opts,L,Lx,D,Dx,End,[iStL(Off),iLbl(Lb)|C],Cx,Stk,Stkx) :-
  genLbl(L,Lb,L0),
  defineLclVar(Nm,Lb,End,Opts,D,D1,Off,C,C0),
  dropStk(Stk,1,Stk1),
  call(Succ,L0,Lx,D1,Dx,End,C0,Cx,Stk1,Stkx).
compPtn(anon,_,Succ,_,_,_Opts,L,Lx,D,Dx,End,[iDrop|C],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stk1),
  call(Succ,L,Lx,D,Dx,End,C,Cx,Stk1,Stkx).
compPtn(ctpl(St,Args),Lc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,[iUnpack(St,Fl)|C],Cx,Stk,Stkx) :-
  genLbl(L,Fl,L0),
  dropStk(Stk,1,Stka),
  stkLvl(Stka,Lvla),
  length(Args,Ar),
  dropStk(Stk,1-Ar,Stk0),

  compPtnArgs(Args,Lc,stkArgCont,TCont,0,Succ,contCont(Fl),
	      Opts,L0,L1,D,D1,End,C,[iLbl(Fl),iRst(Lvla)|C1],Stk0,Stkx),
  call(Fail,L1,Lx,D,D2,End,C1,Cx,Stka,_),
  resetVars(D1,D2,Dx).
compPtn(whr(Lc,P,Cnd),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Fail,SFail),
  compPtn(P,Lc,compCond(Cnd,Lc,Succ,SFail,TCont,Opts),SFail,
	  TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compPtn(T,Lc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L0),
  compExp(T,Lc,contCont(Nxt),TCont,Opts,L0,L1,D,D1,End,C,[iLbl(Nxt),iCmp(Fl)|C0],Stk,Stk1),
  dropStk(Stk1,2,Stk2),
  verify(gencode:dropStk(Stk,1,Stk2),"Stk2 off"),
  ptnTest(Succ,Fail,Fl,L1,Lx,D1,Dx,End,C0,Cx,Stk2,Stkx).

ptnTest(Succ,Fail,Fl,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  contHasLbl(Fail,Fl),!,
  call(Succ,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
ptnTest(Succ,Fail,Fl,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Fl,L0),
  call(Succ,L0,L1,D,D1,End,C,[iLbl(Fl)|C1],Stk,Stk1),
  call(Fail,L1,Lx,D,D2,End,C1,Cx,Stk,Stk2),
  mergeVars(D1,D2,Dx),
  mergeStkLvl(Stk1,Stk2,Stkx,"ptn test").

bumpStk(some(Stk),some(Stk1)) :- Stk1 is Stk+1.
dropStk(some(Stk),Cnt,some(Stk1)) :- Stk1 is Stk-Cnt.

sameStk(none,none) :-!.
sameStk(some(S),some(S)).

mergeStkLvl(none,Stk,Stk,_) :-!.
mergeStkLvl(Stk,none,Stk,_) :-!.
mergeStkLvl(Stk1,Stk2,Stk1,Msg) :-
  verify(gencode:sameStk(Stk1,Stk2),Msg).

compPtnArgs([],_,_,_,_,Succ,_,_,L,Lx,D,Dx,End,C,Cx,Stk,Stkx)	:- 
  call(Succ,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compPtnArgs([voyd|R],Lc,ArgCont,TCont,Ix,Succ,Fail,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :- 
  call(ArgCont,Ix,D,_D0,End,Stk,_Stk0,C,[iDrop|C0]),
  dropStk(Stk,1,Stk1),
  Ix1 is Ix+1,
  compPtnArgs(R,Lc,ArgCont,TCont,Ix1,Succ,Fail,Opts,L,Lx,D,Dx,End,C0,Cx,Stk1,Stkx).
compPtnArgs([A|R],Lc,ArgCont,TCont,Ix,Succ,Fail,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :- 
  call(ArgCont,Ix,L,L0,D,D1,End,Stk,Stk0,C,C0),
  genLbl(L0,Nxt,L1),
  compPtnArg(A,Ix,Lc,
	     contCont(Nxt),Fail,TCont,Opts,L1,L2,D1,D2,End,C0,[iLbl(Nxt)|C1],Stk0,Stk1),
  Ix1 is Ix+1,
  compPtnArgs(R,Lc,ArgCont,TCont,Ix1,Succ,Fail,Opts,L2,Lx,D2,Dx,End,C1,Cx,Stk1,Stkx).

trivialRemaining([]) :-!.
trivialRemaining([voyd|R]) :-
  trivialRemaining(R).

compPtnArg(idnt(V),Ix,_,Succ,_Fail,_TCont,_,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  lclVar(V,a(Ix),D),!,
  call(Succ,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compPtnArg(P,_Ix,Lc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compPtn(P,Lc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

isCond(cnj(_,_,_)).
isCond(cnd(_,_,_,_)).
isCond(dsj(_,_,_)).
isCond(ng(_,_)).
isCond(mtch(_,_,_)).

isTrueSymb(enum("star.core#true")).
isFalseSymb(enum("star.core#false")).

compCond(enum(Sy),_Lc,Succ,_Fail,_TCont,_Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  isTrueSymb(Sy),!,
  call(Succ,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compCond(enum(Sy),_,_,Fail,_T,_,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  isFalseSymb(Sy),!,
  call(Fail,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compCond(cnj(Lc,dsj(LLc,LL,LR),B),_,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  compCond(dsj(LLc,cnj(Lc,LL,B),
	       cnj(Lc,LR,B)),Lc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compCond(cnj(Lc,A,B),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Fail,OF),
  compCond(A,Lc,compCond(B,Lc,Succ,OF,TCont,Opts),OF,TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(dsj(Lc,A,B),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCond(A,Lc,Succ,
	   resetVrCont(D,compCond(B,Lc,Succ,Fail,TCont,Opts)),
	   TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
  % splitCont(Lc,Succ,OSc),
  % compCond(A,Lc,OSc,
  % 	   resetVrCont(D,compCond(B,Lc,OSc,Fail,TCont,Opts)),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(ng(Lc,Cn),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compNeg(Lc,Cn,OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compCond(cnd(Lc,T,A,B),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCond(T,Lc,compCond(A,Lc,Succ,Fail,TCont,Opts),
	   resetVrCont(D,compCond(B,Lc,Succ,Fail,TCont,Opts)),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(mtch(Lc,P,E),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compExp(E,Lc,compPtn(P,Lc,Succ,Fail,TCont,Opts),
	   TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(E,Lc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compExp(E,Lc,ifCont(Succ,Fail),TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

ifCont(Succ,Fail,L,Lx,D,Dx,End,[iIfNot(Fl)|C],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stk0),
  genLbl(L,Fl,L0),
  call(Succ,L0,L1,D,D1,End,C,[iLbl(Fl)|C0],Stk0,Stk1),
  call(Fail,L1,Lx,D,D2,End,C0,Cx,Stk0,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"test exp"),
  mergeVars(D1,D2,Dx).

compNeg(Lc,Cn,OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCond(Cn,Lc,Fail,Succ,TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).

compCase(T,Lc,Cases,Deflt,Cont,TCont,Hndlr,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L1),
  splitCont(Lc,Cont,OC),
  compExp(T,Lc,contCont(Nxt),TCont,Opts,L1,L2,D,D2,End,C,[iLbl(Nxt),iCase(Mx)|T0],Stk,Stk0),
  genLbl(L2,Dflt,L3),
  genCaseTable(Cases,Mx,Table),
  stkLvl(Stk,Lvl),
  compCases(Table,0,Mx,OC,contCont(Dflt),TCont,Dflt,Hndlr,
	    Opts,L3,L4,D2,D4,End,T0,Tx,Tx,[iLbl(Dflt),iRst(Lvl)|C1],Stk0),
  call(Hndlr,Deflt,Lc,OC,TCont,Opts,L4,Lx,D4,Dx,End,C1,Cx,Stk,Stkx).

genCaseTable(Cases,P,Table) :-
  length(Cases,L),
  nextPrime(L,P),
  caseHashes(Cases,P,Hs),
%  reportMsg("case hashes %s",[Hs]),
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

compCases([],Mx,Mx,_Succ,_,_TCont,_,_,_Opts,Lx,Lx,D,D,_End,Tc,Tc,C,C,_Stk).
compCases([],Ix,Mx,Succ,Fail,TCont,Dflt,Hndlr,Opts,L,Lx,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk) :-
  Ix1 is Ix+1,
  compCases([],Ix1,Mx,Succ,Fail,TCont,Dflt,Hndlr,Opts,L,Lx,D,Dx,End,Tc,Tx,C,Cx,Stk).
compCases([(Ix,Case)|Cs],Ix,Mx,Succ,Fail,TCont,Dflt,Hndlr,
	  Opts,L,Lx,D,Dx,End,[iJmp(Lbl)|Tc],Tx,C,Cx,Stk) :-!,
  genLbl(L,Lbl,L0),
  compCaseBranch(Case,Lbl,Succ,Fail,TCont,Hndlr,Opts,L0,L1,D,D1,End,C,C1,Stk,_),
  H1 is Ix+1,
  compCases(Cs,H1,Mx,Succ,Fail,TCont,Dflt,Hndlr,Opts,L1,Lx,D1,Dx,End,Tc,Tx,C1,Cx,Stk).
compCases(Cs,Ix,Mx,Succ,Fail,TCont,Dflt,Hndlr,Opts,L,Lx,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk) :-
  H1 is Ix+1,
  compCases(Cs,H1,Mx,Succ,Fail,TCont,Dflt,Hndlr,Opts,L,Lx,D,Dx,End,Tc,Tx,C,Cx,Stk).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E,Lc)],Lbl,Succ,Fail,TCont,Hndlr,Opts,L,Lx,D,Dx,End,[iLbl(Lbl)|C],Cx,Stk,Stkx) :-!,
  genLbl(L,Nxt,L1),
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,contCont(Nxt),Fail,TCont,Opts,L1,L2,D,D2,End,C0,[iLbl(Nxt)|C1],Stk,Stk1),
  call(Hndlr,E,Lc,Succ,TCont,Opts,L2,Lx,D2,Dx,End,C1,Cx,Stk1,Stkx).
compCaseBranch([(P,E,Lc)|SC],Lbl,Succ,Fail,TCont,Hndlr,Opts,L,Lx,D,Dx,End,
	       [iLbl(Lbl),iTL(Off),iLbl(VLb)|C],Cx,Stk,Stkx) :-
  genLbl(L,Fl,L1),
  genLbl(L1,VLb,L2),
  genLbl(L2,VLE,L3),
  defineLclVar("__",VLb,Opts,End,D,D1,Off,C,C0),
  genLine(Opts,Lc,C0,C1),
  dropStk(Stk,1,Stk0),
  compPtn(P,Lc,compExp(E,Lc,Succ,TCont,Opts),resetCont(Stk0,contCont(Fl)),
	  TCont,Opts,L3,L4,D1,D2,VLE,C1,[iLbl(Fl)|C2],Stk,Stk1),
  resetVars(D1,D2,D3),
  compMoreCase(SC,Off,Succ,Fail,TCont,Hndlr,Opts,L4,Lx,
	       D3,Dx,VLE,C2,[iLbl(VLE)|Cx],Stk0,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"case branch stack").

compMoreCase([],_,_Succ,Fail,_TCont,_,_Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Fail,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compMoreCase([(P,E,Lc)|SC],VLb,Succ,Fail,TCont,Hndlr,Opts,L,Lx,D,Dx,End,[iLdL(VLb)|C],Cx,Stk,Stkx) :-
  genLbl(L,Fl,L1),
  bumpStk(Stk,Stk0),
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,varCont(Hndlr,E,Lc,Succ,TCont,Opts),
	  resetCont(Stk,contCont(Fl)),
	  TCont,Opts,L1,L2,D,D1,End,C0,[iLbl(Fl)|C1],Stk0,Stk1),
  resetVars(D,D1,D2),
  compMoreCase(SC,VLb,Succ,Fail,TCont,Hndlr,Opts,L2,Lx,D2,Dx,End,C1,Cx,Stk,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"more case branch stack").

varCont(H,E,Lc,Succ,TCont,Opts,Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(H,E,Lc,Succ,TCont,Opts,Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

compCnsCases([(ctpl(Cn,Args),E,Lc)],_,CompRhs,Cont,TCont,Opts,L,Lx,D,Dx,
	     [iUnpack(Cn,Fl)|C],Cx,Stk,Stkx) :-!,
  genLbl(L,Nxt,L1),
  genLbl(L1,Fl,L2),
  abortCont(Lc,strg("unpack failed"),Opts,L2,L3,D,D2,Fl,Cy,Cx,Stk,_),
  length(Args,Ar),

  dropStk(Stk,1-Ar,Stk0),
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,contCont(Nxt),contCont(Fl),
	      Opts,L3,L4,D2,D3,Fl,C,[iLbl(Nxt)|A1],Stk0,Stk1),
  call(CompRhs,E,Lc,Cont,Opts,L4,Lx,D3,Dx,Fl,A1,[iLbl(Fl)|Cy],Stk1,Stkx).
compCnsCases(Cases,Lc,CompRhs,Cont,TCont,Opts,L,Lx,D,Dx,[iIndxJmp(Mx)|T0],Cx,Stk,Stkx) :-
  splitCont(Lc,Cont,OC),
  length(Cases,Mx),
  compArms(Cases,Lc,CompRhs,OC,TCont,Opts,L,Lx,D,Dx,T0,Tx,Tx,Cx,Stk,Stkx).

compArms([],_Lc,_,_Cont,_TCont,_Opts,Lx,Lx,Dx,Dx,Tc,Tc,Cx,Cx,Stk,Stk).
compArms([(ctpl(Cn,Args),E,Lc)],_Lc,CompRhs,Cont,TCont,Opts,L,Lx,D,Dx,
	 [iJmp(Lbl)|Tx],Tx,
	 [iLbl(Lbl),iUnpack(Cn,End)|C],Cx,Stk,Stkx) :-!,
  genLbl(L,Lbl,L0),
  genLbl(L0,Nxt,L1),
  genLbl(L1,End,L2),

  abortCont(Lc,strg("arm failed"),Opts,L2,L3,D,D3,End,Cy,Cx,Stk,_),

  length(Args,Ar),
  dropStk(Stk,1-Ar,Stk0),
 
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,contCont(Nxt),contCont(End),
	      Opts,L3,L4,D3,D4,End,C,[iLbl(Nxt)|A1],Stk0,Stk1),
  call(CompRhs,E,Lc,Cont,Opts,L4,Lx,D4,Dx,End,A1,[iLbl(End)|Cy],
       Stk1,Stkx).
compArms([(ctpl(Cn,Args),E,Lc)|Cases],_Lc,CompRhs,Cont,TCont,Opts,L,Lx,D,Dx,
	 [iJmp(Lbl)|T1],Tx,
	 [iLbl(Lbl),iDup,iUnpack(Cn,End)|C],Cx,Stk,Stkx) :-
  genLbl(L,Lbl,L0),
  genLbl(L0,Nxt,L1),
  genLbl(L1,End,L2),
  abortCont(Lc,strg("arm failed"),Opts,L2,L3,D,D3,End,Cy,Cz,Stk,_),

  length(Args,Ar),
  dropStk(Stk,1-Ar,Stk0),
  
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,contCont(Nxt),contCont(End),
	      Opts,L3,L4,D3,D4,End,C,[iLbl(Nxt)|A1],Stk0,Stk1),
  call(CompRhs,E,Lc,Cont,Opts,L4,L5,D4,D5,End,A1,[iLbl(End)|Cy],Stk1,Stk2),
  compArms(Cases,Lc,CompRhs,Cont,TCont,Opts,L5,Lx,D5,Dx,T1,Tx,Cz,Cx,Stk,Stk3),
  mergeStkLvl(Stk2,Stk3,Stkx,"bad unpack cases").

removeExtraLines([],[]).
removeExtraLines([iLine(_),iLine(Lc)|Ins], Out) :-
  removeExtraLines([iLine(Lc)|Ins],Out).
removeExtraLines([I|Ins],[I|Out]) :-
  removeExtraLines(Ins,Out).
