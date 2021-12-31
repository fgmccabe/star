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
	      trapCont(Lc),0,contCont(Ex),abortCont(Lc,strg("failed"),Opts),Opts,L1,L2,D1,D2,End,
	      C1,[iLbl(Ex)|C2],some(0),Stk0),
  compTerm(Value,Lc,retCont(Opts),trapCont(Lc),Opts,L2,_Lx,D2,Dx,End,C2,[iLbl(End)],Stk0,_Stk),
  findMaxLocal(Dx,Mx),
%  (is_member(showGenCode,Opts) -> dispIns(func(Nm,H,Sig,Mx,C0));true ),
  peepOptimize(C0,Cde),
  (is_member(showGenCode,Opts) -> dispIns(func(Nm,H,Sig,Mx,Cde));true ),
  assem(func(Nm,H,Sig,Mx,Cde),CdTrm).
genDef(D,Opts,glbDef(Lc,Nm,Tp,Value),O,[Cd|O]) :-
  encType(funType(tplType([]),Tp),Sig),
  genLbl([],End,L1),
  genLine(Opts,Lc,C0,C1),
  compTerm(Value,Lc,bothCont(glbCont(Nm),rtgCont(Opts)),trapCont(Lc),
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

rtgCont(Opts,Lx,Lx,D,D,_,C,Cx,_Stk,none) :-
  genDbg(Opts,C,[iRtG|Cx]).

dropCont(Lx,Lx,D,D,_,[iDrop|Cx],Cx,Stk,Stk1) :-
  dropStk(Stk,1,Stk1).

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

compTerm(voyd,_,Cont,_TCont,_Opts,L,Lx,D,Dx,End,[iLdV|C0],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk1,Stkx).
compTerm(Trm,_Lc,Cont,_ECont,_Opts,L,Lx,D,Dx,End,[iLdC(Trm)|C],Cx,Stk,Stkx) :-
  isLiteral(Trm),!,
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk1,Stkx).
compTerm(idnt(Nm),Lc,Cont,_TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  (lclVar(Nm,V,D) -> 
   compVar(V,Opts,Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) ;
   reportError("cannot locate variable %s",[Nm],Lc),
   C=[iLdV|Cx],
   bumpStk(Stk,Stkx),
   D=Dx,L=Lx).
compTerm(ctpl(St,A),Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stk2) :-!,
  compTerms(A,Lc,bothCont(allocCont(St),Cont),TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stk2).
compTerm(intrinsic(Lc,Op,A),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  bumpStk(Stk,Stk1),
  compTerms(A,Lc,bothCont(asmCont(Op,Stk1),Cont),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(ecll(Lc,Nm,A),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compTerms(A,Lc,bothCont(escCont(Nm,Stk),Cont),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(cll(Lc,Nm,A),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  length(A,Ar),
  compTerms(A,Lc,cllCont(Nm,Ar,Cont,Opts),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(ocall(Lc,Fn,A),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  length(A,Ar),
  Arity is Ar+1,
  compTerms(A,Lc,compTerm(Fn,Lc,oclCont(Arity,Cont,Opts),TCont,Opts),TCont,
	    Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(tg(Lc,_),OLc,Cont,_TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,[iTag(lbl(H,0))|C1]),
  locHash(Lc,H),
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C1,Cx,Stk1,Stkx).
compTerm(prmpt(Lc,Lb,Lam),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(Lb,Lc,
	   compTerm(Lam,Lc,
		    promptCont(Stk,Cont,Opts),TCont,Opts),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(shft(Lc,Lb,Lam),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(Lb,Lc,
	   compTerm(Lam,Lc,cutCont(Stk,Cont,Opts),TCont,Opts),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(resme(Lc,K,A),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(A,Lc,
	   compTerm(K,Lc,
		    resumeCont(Cont,Opts),TCont,Opts),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
  
compTerm(nth(Lc,Exp,Off),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compTerm(Exp,Lc,bothCont(idxCont(Off),Cont),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(setix(Lc,Exp,Off,Vl),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compTerm(Exp,Lc,
	   compTerm(Vl,Lc,
		    sxCont(Off,Cont),TCont,Opts),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(cel(Lc,Exp),_,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  bumpStk(Stk,Stk1),
  compTerm(Exp,Lc,bothCont(asmCont(iCell,Stk1),Cont),
	   TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compTerm(get(Lc,Exp),_,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  bumpStk(Stk,Stk1),
  compTerm(Exp,Lc,bothCont(asmCont(iGet,Stk1),Cont),
	   TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compTerm(set(Lc,Cl,Val),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(Val,Lc,
	   compTerm(Cl,Lc,
		    bothCont(asmCont(iSet,Stk),Cont),
		    TCont,Opts),
	   TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(case(Lc,T,Cases,Deflt),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compCase(T,Lc,Cases,Deflt,Cont,TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(unpack(Lc,T,Cases),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,Nxt,L1),
  compTerm(T,Lc,contCont(Nxt),TCont,Opts,L1,L2,D,D2,End,C0,[iLbl(Nxt)|C1],Stk,Stk0),
  compCnsCases(Cases,Lc,gencode:compTermCase(TCont),Cont,TCont,Opts,
	       L2,Lx,D2,Dx,C1,Cx,Stk0,Stkx).
compTerm(varNames(Lc,Vrs,T),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  populateVarNames(Vrs,Lc,D,C0,C1),!,
  compTerm(T,Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C1,Cx,Stk,Stkx).
compTerm(whr(Lc,T,Cnd),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCond(Cnd,Lc,compTerm(T,Lc,Cont,TCont,Opts),abortCont(Lc,strg("where fail"),Opts),
	    TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(ltt(Lc,idnt(Nm),Val,Exp),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Lb,L1),
  chLine(Opts,OLc,Lc,C,C0),!,
  defineLclVar(Nm,Lb,End,Opts,D,D1,Off,C0,[iStV(Off)|C1]),
  compTerm(Val,Lc,stoCont(Off,Lb,Opts,
			  compTerm(Exp,Lc,Cont,TCont,Opts)),TCont,Opts,
	   L1,Lx,D1,Dx,End,C1,Cx,Stk,Stkx).
compTerm(error(Lc,Msg),_OLc,_Cont,_TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  abortCont(Lc,Msg,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx). % no continuation after an error
compTerm(rais(Lc,E),_,_,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,none) :-
  compTerm(E,Lc,TCont,trapCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,_).
compTerm(cnd(Lc,T,A,B),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compCondExp(Lc,T,A,B,OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compTerm(seqD(Lc,A,B),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compTerm(A,Lc,resetCont(Stk,compTerm(B,Lc,Cont,TCont,Opts)),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(Cond,Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  isCond(Cond),!,
  genLbl(L,Nx,L0),
  compCond(Cond,Lc,resetCont(Stk,bothCont(trueCont,contCont(Nx))),
	   resetCont(Stk,bothCont(falseCont,contCont(Nx))),
	   TCont,Opts,L0,L1,D,D1,End,C,[iLbl(Nx)|C1],Stk,Stk1),
  call(Cont,L1,Lx,D1,Dx,End,C1,Cx,Stk1,Stkx).
compTerm(doAct(Lc,Act),OLc,Cont,_TCont,Opts,L,Lx,D,Dx,_End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,End,L1),
  splitCont(Lc,Cont,RCont),
  compAction(Act,Lc,RCont,RCont,RCont,
	     Opts,L1,Lx,D,Dx,End,C0,[iLbl(End)|Cx],Stk,Stkx).
compTerm(T,Lc,_,_,_,Lx,Lx,Dx,Dx,_,C,C,Stk,Stk) :-
  reportError("cannot compile %s",[T],Lc),
  abort.

compTermCase(TCont,T,Lc,Cont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(T,Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

compCondExp(Lc,T,A,B,OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Cont,OC),!,
  compCond(T,Lc,compTerm(A,Lc,OC,TCont,Opts),
%	   resetCont(Stk,compTerm(B,Lc,OC,TCont,Opts)),
	   resetVrCont(D,resetCont(Stk,compTerm(B,Lc,OC,TCont,Opts))),
	   TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).

compVar(a(A),_,Cont,L,Lx,D,Dx,End,[iLdA(A)|C0],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk1,Stkx).
compVar(l(X),_,Cont,L,Lx,D,Dx,End,[iLdL(X)|C0],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk1,Stkx).
compVar(g(GlbNm),Opts,Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genDbg(Opts,C,[iLdG(GlbNm)|C0]),
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk1,Stkx).

/* Terms are generated in reverse order*/
compTerms([],_,Cont,_,_,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compTerms([T|Ts],Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L0),
  compTerms(Ts,Lc,contCont(Nxt),TCont,Opts,L0,L1,D,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk0),
  compTerm(T,Lc,Cont,TCont,Opts,L1,Lx,D1,Dx,End,C0,Cx,Stk0,Stkx).

/* Compile actions as sequences with several possible continuations */

compActionCase(RCont,ECont,Action,Lc,Cont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compAction(Action,Lc,Cont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

compAction(nop(_),_,Cont,_RCont,_ECont,_Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(seqD(Lc,A,B),_,Cont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  propagatingAction(A),!,
  compAction(A,Lc,combineCont(handleCont(RCont),
			      resetCont(Stk,compAction(B,Lc,Cont,RCont,ECont,Opts))),
	     RCont,ECont,
	     Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(seqD(Lc,A,B),_,Cont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compAction(A,Lc,compAction(B,Lc,Cont,RCont,ECont,Opts),
	     RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(perfDo(Lc,Exp),OLc,Cont,_RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(Exp,Lc,Cont,ECont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(justDo(Lc,Exp),_,Cont,_,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(Exp,Lc,Cont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(promptD(Lc,Lb,Lam),OLc,Cont,_RCont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(Lb,Lc,
	   compTerm(Lam,Lc,
		    promptCont(Stk,Cont,Opts),TCont,Opts),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(cutD(Lc,Lb,Lam),OLc,Cont,_RCont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(Lb,Lc,
	   compTerm(Lam,Lc,
		    cutCont(Stk,Cont,Opts),TCont,Opts),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(resumeD(Lc,K,A),OLc,Cont,_,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(A,Lc,
	   compTerm(K,Lc,
		    resumeCont(Cont,Opts),ECont,Opts),ECont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(varD(Lc,Ptn,E),OLc,Cont,_,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(E,Lc,compPtn(Ptn,Lc,Cont,resetCont(Stk,ECont),ECont,Opts),ECont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(assignD(Lc,V,E),OLc,Cont,_RCont,_ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(E,Lc,
	   compTerm(V,Lc,
		    bothCont(assignCont(Stk,Opts),Cont),ECont,Opts),ECont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(bindD(Lc,P,E),OLc,Cont,_RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(E,Lc,bindCont(compPtn(P,Lc,Cont,resetCont(Stk,ECont),ECont,Opts),
			 ECont),ECont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(setix(Lc,Exp,Off,Vl),OLc,Cont,_RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compTerm(Exp,Lc,
	   compTerm(Vl,Lc,
		    sxCont(Off,Cont),ECont,Opts),ECont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(ltt(Lc,idnt(Nm),Val,Act),OLc,Cont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Lb,L1),
  chLine(Opts,OLc,Lc,C,C0),!,
  defineLclVar(Nm,Lb,End,Opts,D,D1,Off,C0,[iStV(Off)|C1]),
  compTerm(Val,Lc,stoCont(Off,Lb,Opts,
			  compAction(Act,Lc,Cont,RCont,ECont,Opts)),ECont,Opts,
	   L1,Lx,D1,Dx,End,C1,Cx,Stk,Stkx).
compAction(cnd(Lc,T,A,B),OLc,Cont,RCont,ECont,Opts,
	   L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compIfThen(Lc,T,A,B,OLc,Cont,RCont,ECont,Opts,
	     L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(unpack(Lc,T,Cases),OLc,Cont,RCont,ECont,Opts,
	   L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,Nxt,L1),
  compTerm(T,Lc,contCont(Nxt),ECont,Opts,L1,L2,D,D2,End,C0,[iLbl(Nxt)|C1],Stk,Stk0),
  compCnsCases(Cases,Lc,gencode:compActionCase(RCont,ECont),
	       Cont,ECont,Opts,L2,Lx,D2,Dx,C1,Cx,Stk0,Stkx).
compAction(rtnDo(Lc,E),_Lc,_Cont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(E,Lc,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(raisDo(Lc,E),_Lc,_Cont,_RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(E,Lc,ECont,trapCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(whle(Lc,Cond,Body),OLc,Cont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compWhile(Lc,Cond,Body,OLc,Cont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(untl(Lc,Cond,Body),_,Cont,RCont,ECont,Opts,L,Lx,D,Dx,End,
	   [iLbl(Nxt)|C],Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L0),
  genLbl(L0,Tst,L1),
  genLbl(L1,Ext,L2),
  compAction(Body,Lc,contCont(Tst),RCont,ECont,
	     Opts,L2,L3,D,D2,End,C,[iLbl(Tst)|C1],Stk,Stk1),
  compCond(Cond,Lc,contCont(Ext),resetCont(Stk,contCont(Nxt)),
	   ECont,Opts,L3,L4,D2,_,End,
	   C1,[iLbl(Ext)|C2],Stk1,_Stk2),
  call(Cont,L4,Lx,D,Dx,End,C2,Cx,Stk,Stkx).
compAction(tryDo(Lc,A,H),OLc,Cont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Cont,SuccCont),
  splitCont(Lc,catchAction(Lc,SuccCont,ECont,H,Opts),ThCont),
  compAction(A,Lc,SuccCont,RCont,ThCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).

compIfThen(Lc,T,A,B,OLc,Cont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Cont,SCont),
  compCond(T,Lc,compAction(A,Lc,SCont,RCont,ECont,Opts),
	   resetCont(Stk,resetVrCont(D,compAction(B,Lc,SCont,RCont,ECont,Opts))),
	   ECont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).

compWhile(Lc,Cond,Body,OLc,Cont,RCont,ECont,Opts,L,Lx,D,Dx,End,
	  [iLbl(Loop)|C],Cx,Stk,Stk) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,Loop,L0),
  compCond(Cond,Lc,
	   compAction(Body,Lc,resetCont(Stk,contCont(Loop)),RCont,ECont,Opts),
	   resetCont(Stk,Cont),ECont,Opts,L0,Lx,D,Dx,End,C0,Cx,Stk,Stkx),
  verify(gencode:sameStk(Stk,Stkx),"while stack").

catchAction(Lc,Cont,ECont,H,Opts,L,Lx,D,Dx,End,
	     [iIndxJmp(2),iJmp(Err),iJmp(Ok),iLbl(Trp),iHalt(2),
	      iLbl(Ok),iCLbl(lbl("star.action#ok",1),Trp)|C],Cx,Stk,Stkx) :-
  genLbl(L,Ok,L0),
  genLbl(L0,Err,L1),
  genLbl(L1,Trp,L2),
  call(Cont,L2,L3,D,D1,End,C,[iLbl(Err),iUnpack(lbl("star.action#err",1),Trp)|C1],Stk,_Stk0),
  compTerm(H,Lc,oclCont(2,handleCont(ECont,Cont),Opts),ECont,Opts,L3,Lx,D,D2,End,C1,Cx,Stk,Stkx),
  mergeVars(D1,D2,Dx).

handleCont(ECont,Cont,L,Lx,D,Dx,End,[iCLbl(lbl("star.action#err",1),Nxt)|C],Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L1),
  call(ECont,L1,L2,D,D2,End,C,[iLbl(Nxt)|C0],Stk,Stk1),
  call(Cont,L2,Lx,D2,Dx,End,C0,Cx,Stk,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"propagate").

propagatingAction(perfDo(_,_)) :-!.
propagatingAction(justDo(_,_)) :-!.
propagatingAction(seqD(_,_,R)) :- propagatingAction(R).

propagateCont(ECont,Cont,L,Lx,D,Dx,End,[iCLbl(lbl("star.action#err",1),Nxt)|C],Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L1),
  dropStk(Stk,1,Stk0),
  call(ECont,L1,L2,D,D2,End,C,[iLbl(Nxt),iDrop|C0],Stk0,Stk1),
  call(Cont,L2,Lx,D2,Dx,End,C0,Cx,Stk0,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"propagate").

bindCont(PCont,ECont,L,Lx,D,Dx,End,[iIndxJmp(2),iJmp(Err),iJmp(Ok),iLbl(Trp),iHalt(2),
				    iLbl(Ok),iUnpack(lbl("star.action#ok",1),Trp)|C],Cx,Stk,Stkx) :-
  genLbl(L,Ok,L0),
  genLbl(L0,Err,L1),
  genLbl(L1,Trp,L2),
  call(PCont,L2,L3,D,D1,End,C,[iLbl(Err)|C1],Stk,_Stk0),
  call(ECont,L3,Lx,D,D2,End,C1,Cx,Stk,Stkx),
  mergeVars(D1,D2,Dx).

combineCont(ACont,BCont,L,Lx,D,Dx,End,C,Cx,S,Sx) :-
  call(ACont,BCont,L,Lx,D,Dx,End,C,Cx,S,Sx).

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

% stoCont(Off,Lb,Opts,Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
%   genDbg(Opts,C,[iStL(Off),iLbl(Lb)|C1]),
%   dropStk(Stk,1,Stk1),
%   call(Cont,L,Lx,D,Dx,End,C1,Cx,Stk1,Stkx).

assignCont(Stk,Opts,Lx,Lx,Dx,Dx,_End,C,Cx,_,Stk) :-
  genDbg(Opts,C,[iAssign|Cx]).

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
  frameIns(Stk1,C0,C1),
  call(Cont,L,Lx,D,Dx,End,C1,Cx,Stk1,Stkx).

oclCont(Arity,retCont(_),Opts,Lx,Lx,Dx,Dx,_End,C,Cx,_,none) :-!,
  genDbg(Opts,C,[iTOCall(Arity)|Cx]).
oclCont(Arity,Cont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genDbg(Opts,C,[iOCall(Arity)|C0]),
  dropStk(Stk,Arity,Stk0),
  bumpStk(Stk0,Stk1),
  frameIns(Stk1,C0,C1),
  call(Cont,L,Lx,D,Dx,End,C1,Cx,Stk1,Stkx).

promptCont(Stk,Cont,Opts,L,Lx,D,Dx,End,C,Cx,_Stk,Stkx) :-
  genDbg(Opts,C,[iPrompt|C0]),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).

cutCont(Stk,Cont,Opts,L,Lx,D,Dx,End,C,Cx,_Stk,Stkx) :-
  genDbg(Opts,C,[iCut|C0]),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).

resumeCont(retCont(_),Opts,Lx,Lx,Dx,Dx,_End,C,Cx,_,none) :-!,
  genDbg(Opts,C,[iTResume|Cx]).
resumeCont(Cont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genDbg(Opts,C,[iResume|C0]),
  frameIns(Stk,C0,C1),
  call(Cont,L,Lx,D,Dx,End,C1,Cx,Stk,Stkx).

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
  compTerms([LT,Msg],Lc,asmCont(iAbort,none),trapCont(Lc),Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

indexCont(Ix,Lx,Lx,D,D,_,Stk,Stk1,[iDup,iNth(Ix)|Cx],Cx) :-
  bumpStk(Stk,Stk1).
stkArgCont(_,Lx,Lx,D,D,_,Stk,Stk1,C,C) :-
  bumpStk(Stk,Stk1).

argCont(_Ix,Lx,Lx,D,D,_,Stk,Stk,Cx,Cx).

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
  dropStk(Stk,1,Stka),
  genLbl(L,Fl,L0),
  stkLvl(Stka,Lvla),
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,Succ,contCont(Fl),
	      Opts,L0,L1,D,D1,End,C,[iLbl(Fl),iRst(Lvla)|C1],Stka,Stkx),
  call(Fail,L1,Lx,D,D2,End,C1,Cx,Stka,_),
  resetVars(D1,D2,Dx).
compPtn(whr(Lc,P,Cnd),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Fail,SFail),
  compPtn(P,Lc,compCond(Cnd,Lc,Succ,SFail,TCont,Opts),SFail,
	  TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compPtn(T,Lc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L0),
  compTerm(T,Lc,contCont(Nxt),TCont,Opts,L0,L1,D,D1,End,C,[iLbl(Nxt),iCmp(Fl)|C0],Stk,Stk1),
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
  Ix1 is Ix+1,
  compPtnArgs(R,Lc,ArgCont,TCont,Ix1,Succ,Fail,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compPtnArgs([A|R],Lc,ArgCont,TCont,Ix,Succ,Fail,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :- 
  call(ArgCont,Ix,L,L0,D,D1,End,Stk,Stk0,C,C0),
  genLbl(L0,Nxt,L1),
  compPtnArg(A,Ix,Lc,
	     contCont(Nxt),Fail,TCont,Opts,L1,L2,D1,D2,End,C0,[iLbl(Nxt)|C1],Stk0,Stk1),
  Ix1 is Ix+1,
  verify(gencode:sameStk(Stk,Stk1),"argument pattern"),
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

compCondT(Cnd,Lc,S,F,T,O,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
%  reportMsg("Compile condition %s, stack %s",[ltrm(Cnd),stk(Stk)],Lc),
  compCond(Cnd,Lc,S,F,T,O,L,Lx,D,Dx,End,C,Cx,Stk,Stkx),!.
%  reportMsg("after compiling   %s, stack %s",[ltrm(Cnd),stk(Stkx)],Lc).
  
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
  splitCont(Lc,Succ,OSc),
  chLine(Opts,OLc,Lc,C,C0),
  compCond(A,Lc,OSc,
	   resetVrCont(D,compCond(B,Lc,OSc,Fail,TCont,Opts)),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(ng(Lc,Cn),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compNeg(Lc,Cn,OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compCond(cnd(Lc,T,A,B),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  splitCont(Lc,Succ,OSc),
  splitCont(Lc,Fail,OFl),
  chLine(Opts,OLc,Lc,C,C0),
  compCond(T,Lc,compCond(A,Lc,OSc,OFl,TCont,Opts),
	   resetVrCont(D,compCond(B,Lc,OSc,OFl,TCont,Opts)),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(mtch(Lc,P,E),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(E,Lc,compPtn(P,Lc,Succ,Fail,TCont,Opts),
	   TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(E,Lc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(E,Lc,ifCont(Succ,Fail),TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

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

compCase(T,Lc,Cases,Deflt,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L1),
  splitCont(Lc,Cont,OC),
  compTerm(T,Lc,contCont(Nxt),TCont,Opts,L1,L2,D,D2,End,C,[iLbl(Nxt),iCase(Mx)|T0],Stk,Stk0),
  genLbl(L2,Dflt,L3),
  genCaseTable(Cases,Mx,Table),
  stkLvl(Stk,Lvl),
  compCases(Table,0,Mx,OC,contCont(Dflt),TCont,Dflt,
	    Opts,L3,L4,D2,D4,End,T0,Tx,Tx,[iLbl(Dflt),iRst(Lvl)|C1],Stk0),
  compTerm(Deflt,Lc,OC,TCont,Opts,L4,Lx,D4,Dx,End,C1,Cx,Stk,Stkx).

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

compCases([],Mx,Mx,_Succ,_,_TCont,_,_Opts,Lx,Lx,D,D,_End,Tc,Tc,C,C,_Stk).
compCases([],Ix,Mx,Succ,Fail,TCont,Dflt,Opts,L,Lx,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk) :-
  Ix1 is Ix+1,
  compCases([],Ix1,Mx,Succ,Fail,TCont,Dflt,Opts,L,Lx,D,Dx,End,Tc,Tx,C,Cx,Stk).
compCases([(Ix,Case)|Cs],Ix,Mx,Succ,Fail,TCont,Dflt,
	  Opts,L,Lx,D,Dx,End,[iJmp(Lbl)|Tc],Tx,C,Cx,Stk) :-!,
  genLbl(L,Lbl,L0),
  compCaseBranch(Case,Lbl,Succ,Fail,TCont,Opts,L0,L1,D,D1,End,C,C1,Stk,_),
  H1 is Ix+1,
  compCases(Cs,H1,Mx,Succ,Fail,TCont,Dflt,Opts,L1,Lx,D1,Dx,End,Tc,Tx,C1,Cx,Stk).
compCases(Cs,Ix,Mx,Succ,Fail,TCont,Dflt,Opts,L,Lx,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk) :-
  H1 is Ix+1,
  compCases(Cs,H1,Mx,Succ,Fail,TCont,Dflt,Opts,L,Lx,D,Dx,End,Tc,Tx,C,Cx,Stk).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E,Lc)],Lbl,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,[iLbl(Lbl)|C],Cx,Stk,Stkx) :-!,
  genLbl(L,Nxt,L1),
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,contCont(Nxt),Fail,TCont,Opts,L1,L2,D,D2,End,C0,[iLbl(Nxt)|C1],Stk,Stk1),
  compTerm(E,Lc,Succ,TCont,Opts,L2,Lx,D2,Dx,End,C1,Cx,Stk1,Stkx).

compCaseBranch([(P,E,Lc)|SC],Lbl,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,
	       [iLbl(Lbl),iTL(Off),iLbl(VLb)|C],Cx,Stk,Stkx) :-
  genLbl(L,Fl,L1),
  genLbl(L1,VLb,L2),
  genLbl(L2,VLE,L3),
  defineLclVar("__",VLb,Opts,End,D,D1,Off,C,C0),
  genLine(Opts,Lc,C0,C1),
  dropStk(Stk,1,Stk0),
  compPtn(P,Lc,compTerm(E,Lc,Succ,TCont,Opts),resetCont(Stk0,contCont(Fl)),
	  TCont,Opts,L3,L4,D1,D2,VLE,C1,[iLbl(Fl)|C2],Stk,Stk1),
  resetVars(D1,D2,D3),
  compMoreCase(SC,Off,Succ,Fail,TCont,Opts,L4,Lx,
	       D3,Dx,VLE,C2,[iLbl(VLE)|Cx],Stk0,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"case branch stack").
compMoreCase([],_,_Succ,Fail,_TCont,_Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Fail,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compMoreCase([(P,E,Lc)|SC],VLb,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,[iLdL(VLb)|C],Cx,Stk,Stkx) :-
  genLbl(L,Fl,L1),
  bumpStk(Stk,Stk0),
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,compTerm(E,Lc,Succ,TCont,Opts),
	  resetCont(Stk,contCont(Fl)),
	  TCont,Opts,L1,L2,D,D1,End,C0,[iLbl(Fl)|C1],Stk0,Stk1),
  resetVars(D,D1,D2),
  compMoreCase(SC,VLb,Succ,Fail,TCont,Opts,L2,Lx,D2,Dx,End,C1,Cx,Stk,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"more case branch stack").

compCnsCases([(ctpl(Cn,Args),E,Lc)],_,CompRhs,Cont,TCont,Opts,L,Lx,D,Dx,
	     [iUnpack(Cn,Fl)|C],Cx,Stk,Stkx) :-!,
  genLbl(L,Nxt,L1),
  genLbl(L1,Fl,L2),
  abortCont(Lc,strg("failed"),Opts,L2,L3,D,D2,Fl,Cy,Cx,Stk,_),
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
  abortCont(Lc,strg("failed"),Opts,L2,L3,D,D3,End,Cy,Cx,Stk,_),
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,contCont(Nxt),contCont(End),
	      Opts,L3,L4,D3,D4,End,C,[iLbl(Nxt)|A1],Stk,Stk1),
  call(CompRhs,E,Lc,Cont,Opts,L4,Lx,D4,Dx,End,A1,[iLbl(End)|Cy],
       Stk1,Stkx).
compArms([(ctpl(Cn,Args),E,Lc)|Cases],_Lc,CompRhs,Cont,TCont,Opts,L,Lx,D,Dx,
	 [iJmp(Lbl)|T1],Tx,
	 [iLbl(Lbl),iDup,iUnpack(Cn,End)|C],Cx,Stk,Stkx) :-
  genLbl(L,Lbl,L0),
  genLbl(L0,Nxt,L1),
  genLbl(L1,End,L2),
  abortCont(Lc,strg("failed"),Opts,L2,L3,D,D3,End,Cy,Cz,Stk,_),
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,contCont(Nxt),contCont(End),
	      Opts,L3,L4,D3,D4,End,C,[iLbl(Nxt)|A1],Stk,Stk1),
  call(CompRhs,E,Lc,Cont,Opts,L4,L5,D4,D5,End,A1,[iLbl(End)|Cy],Stk1,Stk2),
  compArms(Cases,Lc,CompRhs,Cont,TCont,Opts,L5,Lx,D5,Dx,T1,Tx,Cz,Cx,Stk,Stk3),
  mergeStkLvl(Stk2,Stk3,Stkx,"bad unpack cases").

removeExtraLines([],[]).
removeExtraLines([iLine(_),iLine(Lc)|Ins], Out) :-
  removeExtraLines([iLine(Lc)|Ins],Out).
removeExtraLines([I|Ins],[I|Out]) :-
  removeExtraLines(Ins,Out).
