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
  dispRuleSet(fnDef(Lc,Nm,H,Tp,Args,Value)),
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
  (is_member(showGenCode,Opts) -> dispIns(func(Nm,H,Sig,Mx,C0));true ),
  removeExtraLines(C0,Cde),
  assem(func(Nm,H,Sig,Mx,Cde),CdTrm).
genDef(D,Opts,glbDef(Lc,Nm,Tp,Value),O,[Cd|O]) :-
  encType(funType(tplType([]),Tp),Sig),
  genLbl([],End,L1),
  genLine(Opts,Lc,C0,C1),
  compTerm(Value,Lc,bothCont(glbCont(Nm),retCont(Opts)),trapCont(Lc),
	   Opts,L1,_Lx,D,Dx,End,C1,[iLbl(End)],some(0),_Stk),
  findMaxLocal(Dx,Mx),
  (is_member(showGenCode,Opts) -> dispIns(func(lbl(Nm,0),hard,Sig,Mx,C0));true ),
  removeExtraLines(C0,Cde),
  assem(func(lbl(Nm,0),hard,Sig,Mx,Cde),Cd).
genDef(_,_,lblDef(_,Lbl,Tp,Ix),O,[LblTrm|O]) :-
  encType(Tp,Sig),
  assem(struct(Lbl,strg(Sig),Ix),LblTrm).
genDef(_,_,typDef(_,Tp,Rl,IxMap),O,[TpTrm|O]) :-
  assem(tipe(Tp,Rl,IxMap),TpTrm).

glbCont(Nm,Lx,Lx,D,D,_,[iTG(Nm)|Cx],Cx,Stk,Stk).

retCont(Opts,Lx,Lx,D,D,_,C,Cx,_Stk,none) :-
  genDbg(Opts,C,[iRet|Cx]).

dropCont(Lx,Lx,D,D,_,[iDrop|Cx],Cx,Stk,Stk1) :-
  dropStk(Stk,1,Stk1).

dropToCont(N,Lx,Lx,D,D,_,[iDropTo(N)|Cx],Cx,Stk,Stkx) :-
  dropStk(Stk,N,Stkx).

trapCont(Lc,Lx,Lx,D,D,_,Cx,Cx,Stk,Stk) :-
  reportError("not permitted to throw",[],Lc),
  abort.

idxCont(Off,Lx,Lx,Dx,Dx,_End,[iNth(Off)|Cx],Cx,Stkx,Stkx).

sxCont(Off,Lx,Lx,Dx,Dx,_End,[iStNth(Off)|Cx],Cx,Stkx,Stkx).

resetVrCont(scope(Vrs,FrReg,M0),Cont,L,Lx,scope(_,_,M1),Dx,End,C,Cx,Stk,Stkx) :-
  Mx is max(M0,M1),
  call(Cont,L,Lx,scope(Vrs,FrReg,Mx),Dx,End,C,Cx,Stk,Stkx).

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

defineLclVar(Nm,Lbl,End,scope(Vrs,FreeRg,Mx),
	     scope([(Nm,l(Off),Lbl,End)|Vrs],NFreeRg,Mx1),Off,
	     [iLocal(Nm,Lbl,End,Off)|Cx],Cx) :-
  nextFreeOff(FreeRg,Mx,Off,NFreeRg,Mx1).

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
compTerm(Lit,_,Cont,_TCont,_,L,Lx,D,Dx,End,[iLdC(Lit)|C0],Cx,Stk,Stkx) :-
  isGround(Lit),!,
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk1,Stkx).
compTerm(idnt(Nm),Lc,Cont,_TCont,_,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  (lclVar(Nm,V,D) -> 
   compVar(V,Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) ;
   reportError("cannot locate variable %s",[Nm],Lc),
   abort).
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
		    bothCont(asmCont(iPrompt,Stk),Cont),TCont,Opts),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(shft(Lc,Lb,Lam),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(Lb,Lc,
	   compTerm(Lam,Lc,
		    bothCont(asmCont(iCut,Stk),Cont),TCont,Opts),TCont,
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
		    bothCont(sxCont(Off),Cont),TCont,Opts),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stk0),
  mergeStkLvl(Stk,Stk0,Stkx,"set ix").
compTerm(case(Lc,T,Cases,Deflt),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compCase(T,Lc,Cases,Deflt,Cont,TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(unpack(Lc,T,Cases),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,Nxt,L1),
  compTerm(T,Lc,contCont(Nxt),TCont,Opts,L1,L2,D,D2,End,C0,[iLbl(Nxt)|C1],Stk,_Stk),
  compCnsCases(Cases,Lc,Cont,TCont,Opts,L2,Lx,D2,Dx,C1,Cx,Stk,Stkx).
compTerm(varNames(Lc,Vrs,T),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  populateVarNames(Vrs,Lc,D,C0,C1),!,
  compTerm(T,Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C1,Cx,Stk,Stkx).
compTerm(whr(Lc,T,Cnd),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCond(Cnd,Lc,compTerm(T,Lc,Cont,TCont,Opts),
	   abortCont(Lc,strg("where fail"),Opts),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(ltt(Lc,idnt(Nm),Val,Exp),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Lb,L1),
  chLine(Opts,OLc,Lc,C,C0),!,
  defineLclVar(Nm,Lb,End,D,D1,Off,C0,[iStV(Off)|C1]),
  compTerm(Val,Lc,bothCont(stoCont(Off,Lb),
			   compTerm(Exp,Lc,Cont,TCont,Opts)),TCont,Opts,
	   L1,Lx,D1,Dx,End,C1,Cx,Stk,Stkx).
compTerm(error(Lc,Msg),_OLc,_Cont,_TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  abortCont(Lc,Msg,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx). % no continuation after an error
compTerm(rais(Lc,E),_,_,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,none) :-
  compTerm(E,Lc,TCont,trapCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,_).
compTerm(cnd(Lc,T,A,B),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Cont,OC),!,
  compCond(T,Lc,compTerm(A,Lc,OC,TCont,Opts),
	   compTerm(B,Lc,OC,TCont,Opts),TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(seq(Lc,A,B),OLc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compTerm(A,Lc,resetCont(Stk,compTerm(B,Lc,Cont,TCont,Opts)),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(Cond,Lc,Cont,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  isCond(Cond),!,
  genLbl(L,Nx,L0),
  compCond(Cond,Lc,bothCont(trueCont,contCont(Nx)),bothCont(falseCont,contCont(Nx)),TCont,
      Opts,L0,L1,D,D1,End,C,[iLbl(Nx)|C1],Stk,Stk1),
  call(Cont,L1,Lx,D1,Dx,End,C1,Cx,Stk1,Stkx).
compTerm(doAct(Lc,Act),OLc,Cont,_TCont,Opts,L,Lx,D,Dx,_End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,End,L1),
  splitCont(Lc,Cont,RCont),
  compAction(Act,Lc,RCont,propagateCont(RCont),RCont,RCont,
	     Opts,L1,Lx,D,Dx,End,C0,[iLbl(End)|Cx],Stk,Stkx).
compTerm(T,Lc,_,_,_,Lx,Lx,Dx,Dx,_,C,C,Stk,Stk) :-
  reportError("cannot compile %s",[T],Lc),
  abort.

compVar(a(A),Cont,L,Lx,D,Dx,End,[iLdA(A)|C0],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk1,Stkx).
compVar(l(X),Cont,L,Lx,D,Dx,End,[iLdL(X)|C0],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stk1),
  call(Cont,L,Lx,D,Dx,End,C0,Cx,Stk1,Stkx).
compVar(g(GlbNm),Cont,L,Lx,D,Dx,End,[iLdG(GlbNm)|C0],Cx,Stk,Stkx) :-
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

compAction(seq(Lc,A,B),_,Cont,PCont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  propagatingAction(A),!,
  compAction(A,Lc,combineCont(PCont,
			      compAction(B,Lc,Cont,PCont,RCont,ECont,Opts)),
	     PCont,RCont,ECont,
	     Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(seq(Lc,A,B),_,Cont,PCont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compAction(A,Lc,resetCont(Stk,compAction(B,Lc,Cont,PCont,RCont,ECont,Opts)),
	     PCont,RCont,ECont,
	     Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(perfDo(Lc,Exp),OLc,Cont,_PCont,_RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(Exp,Lc,Cont,ECont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(justDo(Lc,Exp),_,Cont,_PCont,_,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(Exp,Lc,Cont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(varD(Lc,Ptn,E),OLc,Cont,_,_,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(E,Lc,compPtn(Ptn,Lc,Cont,ECont,ECont,Opts),ECont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(assignD(Lc,V,E),OLc,Cont,_,_RCont,_ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  bumpStk(Stk,Stk0),
  compTerm(E,Lc,
	   compTerm(V,Lc,
		    bothCont(asmCont(iAssign,Stk0),Cont),ECont,Opts),ECont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(cnd(Lc,T,A,B),OLc,Cont,PCont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Cont,SCont),
  compCond(T,Lc,compAction(A,Lc,SCont,PCont,RCont,ECont,Opts),
	   compAction(B,Lc,SCont,PCont,RCont,ECont,Opts),ECont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(rtnDo(Lc,E),_Lc,_Cont,_,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(E,Lc,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(raisDo(Lc,E),_Lc,_Cont,_PCont,_RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(E,Lc,ECont,trapCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compAction(whle(Lc,Cond,Body),OLc,Cont,PCont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stk) :-
  compWhile(whle(Lc,Cond,Body),OLc,Cont,PCont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stk).
compAction(untl(Lc,Cond,Body),_,Cont,PCont,RCont,ECont,Opts,L,Lx,D,Dx,End,
	   [iLbl(Nxt)|C],Cx,Stk,Stk) :-
  genLbl(L,Nxt,L0),
  genLbl(L0,Tst,L1),
  compAction(Body,Lc,resetCont(Stk,contCont(Tst)),PCont,RCont,ECont,
	     Opts,L1,L2,D,D2,End,C,[iLbl(Tst)|C1],Stk,Stk1),
  compCond(Cond,Lc,Cont,contCont(Nxt),ECont,Opts,L2,Lx,D2,Dx,End,C1,Cx,Stk1,Stkx),
  verify(gencode:sameStk(Stk,Stk1),"until test stack"),
  verify(gencode:sameStk(Stk,Stkx),"until body stack").
compAction(tryDo(Lc,A,H),_,Cont,_PCont,RCont,ECont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L0),
  genLbl(L0,ThLbl,L1),
  stkLvl(Stk,Lvl),
  compAction(A,Lc,Cont,throwCont(jmpCont(ThLbl)),RCont,
	     jmpCont(ThLbl),Opts,L1,L2,D,D2,End,C,[iLbl(ThLbl),iDropTo(Lvl)|C1],Stk,Stk1),
  bumpStk(Stk,Stk0),
  compTerm(H,Lc,contCont(Nxt),ECont,Opts,L2,L3,D2,D3,End,C1,[iLbl(Nxt)|C2],Stk0,Stk2),
  oclCont(1,Cont,Opts,L3,Lx,D3,Dx,End,C2,Cx,Stk2,Stk3),
  mergeStkLvl(Stk1,Stk3,Stkx,"catch").

compWhile(whle(Lc,Cond,Body),OLc,Cont,PCont,RCont,ECont,Opts,L,Lx,D,Dx,End,
	  [iJmp(Tst),iLbl(Nxt)|C],Cx,Stk,Stk) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(L,Nxt,L0),
  genLbl(L0,Tst,L1),
  compCond(Cond,Lc,jmpCont(Nxt),Cont,ECont,Opts,L1,L2,D,D2,End,C2,Cx,Stk,Stk1),
  compAction(Body,Lc,resetCont(Stk,contCont(Tst)),PCont,RCont,ECont,
	     Opts,L2,Lx,D2,Dx,End,C0,[iLbl(Tst)|C2],Stk,Stk2),
  verify(gencode:sameStk(Stk,Stk1),"while test stack"),
  verify(gencode:sameStk(Stk,Stk2),"while body stack").

propagatingAction(perfDo(_,_)) :-!.
propagatingAction(justDo(_,_)) :-!.
propagatingAction(seq(_,_,R)) :- propagatingAction(R).

propagateCont(ECont,Cont,L,Lx,D,Dx,End,[iCLbl(lbl("star.action#ok",1),Nxt)|C],Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L1),
  dropStk(Stk,1,Stk0),
  call(ECont,L1,L2,D,D2,End,C,[iLbl(Nxt),iDrop|C0],Stk0,Stk1),
  call(Cont,L2,Lx,D2,Dx,End,C0,Cx,Stk0,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"propagate").

throwCont(ThCont,Cont,L,Lx,D,Dx,End,[iUnpack(lbl("star.action#err",1),Nxt)|C],Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L0),
  call(ThCont,L0,L1,D,D1,End,C,[iLbl(Nxt)|C1],Stk,_Stk0),
  call(Cont,L1,Lx,D1,Dx,End,C1,Cx,Stk,Stkx).

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

stoCont(Off,Lb,Lx,Lx,Dx,Dx,_End,[iStL(Off),iLbl(Lb)|Cx],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stkx).

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

resumeCont(retCont(_),Opts,Lx,Lx,Dx,Dx,_End,C,Cx,_,none) :-!,
  genDbg(Opts,C,[iTResume|Cx]).
resumeCont(Cont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genDbg(Opts,C,[iResume|C0]),
  frameIns(Stk,C0,C1),
  call(Cont,L,Lx,D,Dx,End,C1,Cx,Stk,Stkx).

jmpCont(Lbl,Lx,Lx,D,D,_End,[iJmp(Lbl)|Cx],Cx,Stk,Stk).

contHasLbl(jmpCont(Lbl),Lbl) :-!.
contHasLbl(contCont(Lbl),Lbl) :-!.
contHasLbl(bothCont(C1,_),Lbl) :- contHasLbl(C1,Lbl).
contHasLbl(resetVrCont(_,C),Lbl) :- contHasLbl(C,Lbl).
contHasLbl(resetCont(_,C),Lbl) :- contHasLbl(C,Lbl).
contHasLbl(lblCont(Lbl),Lbl).

lblCont(Lbl,Cont,L,Lx,D,Dx,End,[iLbl(Lbl)|C],Cx,Stk,Stkx) :-
  call(Cont,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

isSimpleCont(retCont(_)).
isSimpleCont(jmpCont(_)).
isSimpleCont(contCont(_)).
isSimpleCont(bothCont(L,R)) :-
  isSimpleCont(L),
  isSimpleCont(R).
isSimpleCont(resetCont(_,_)).
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
onceCont(Lc,(_,Stkin,Stkout),_,Lx,Lx,D,D,_,Cx,Cx,_Stk,Stkout) :-
  reportError("cannot reconcile stacks [%w,%w]",[Stkin,Stkout],Lc),
  abort.

contLbl(onceCont(_,(Lb,_,_),_),Lb).

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
compPtn(idnt(Nm),_,Succ,_,_,_Opts,L,Lx,D,Dx,End,[iStL(Off),iLbl(Lb)|C],Cx,Stk,Stkx) :-
  genLbl(L,Lb,L0),
  defineLclVar(Nm,Lb,End,D,D1,Off,C,C0),
  dropStk(Stk,1,Stk1),
  call(Succ,L0,Lx,D1,Dx,End,C0,Cx,Stk1,Stkx).
compPtn(anon,_,Succ,_,_,_Opts,L,Lx,D,Dx,End,[iDrop|C],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stk1),
  call(Succ,L,Lx,D,Dx,End,C,Cx,Stk1,Stkx).
compPtn(ctpl(St,Args),Lc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,[iUnpack(St,Fl)|C],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stka),
  genLbl(L,Fl,L0),
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,Succ,contCont(Fl),
	      Opts,L0,L1,D,Dx,End,C,[iLbl(Fl)|C1],Stka,Stkx),
  call(Fail,L1,Lx,D,_D2,End,C1,Cx,Stk,_).
compPtn(whr(Lc,P,Cnd),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(L,Nxt,L0),
  chLine(Opts,OLc,Lc,C,C0),
  compPtn(P,Lc,contCont(Nxt),Fail,TCont,Opts,L0,L1,D,D1,End,C0,[iLbl(Nxt)|C1],Stk,Stk1),
  verify(gencode:dropStk(Stk,1,Stk1),"where stack"),
  compCond(Cnd,Lc,Succ,Fail,TCont,Opts,L1,Lx,D1,Dx,End,C1,Cx,Stk1,Stkx).
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
  chLine(Opts,OLc,Lc,C,C0),
  compCond(Cn,Lc,Fail,Succ,TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(cnd(Lc,T,A,B),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  splitCont(Lc,Succ,OSc),
  splitCont(Lc,Fail,OFl),
  chLine(Opts,OLc,Lc,C,C0),
  compCond(T,Lc,compCond(A,Lc,OSc,OFl,TCont,Opts),
	   resetVrCont(D,compCond(B,Lc,OSc,OFl,TCont,Opts)),TCont,
	   Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(mtch(Lc,P,E),OLc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(E,Lc,compPtn(P,Lc,Succ,resetVrCont(D,resetCont(Stk,Fail)),TCont,Opts),
	   TCont,Opts,L,Lx,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(E,Lc,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(E,Lc,testCont(Succ,Fail),TCont,Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).

testCont(Succ,Fail,L,Lx,D,Dx,End,[iIfNot(Fl)|C],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stk0),
  genLbl(L,Fl,L0),
  call(Succ,L0,L1,D,D1,End,C,[iLbl(Fl)|C0],Stk0,Stk1),
  call(Fail,L1,Lx,D,D2,End,C0,Cx,Stk0,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"test exp"),
  mergeVars(D1,D2,Dx).

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
  genLbl(L,Nxt,L1),
  genLbl(L1,Fl,L2),
  genLbl(L2,VLb,L3),
  defineLclVar("__",VLb,End,D,D1,Off,C,C0),
  genLine(Opts,Lc,C0,C1),
  compPtn(P,Lc,contCont(Nxt),contCont(Fl),TCont,Opts,L3,L4,D1,D5,End,C1,[iLbl(Nxt)|C2],Stk,Stk1),
  verify(gencode:dropStk(Stk1,1,Stk),"case branch"),
  stkLvl(Stk1,Lvl1),
  compTerm(E,Lc,Succ,TCont,Opts,L4,L5,D5,D6,End,C2,[iLbl(Fl),iRst(Lvl1)|C3],Stk1,Stk2),
  compMoreCase(SC,Off,Succ,Fail,TCont,Opts,L5,Lx,D6,Dx,End,C3,Cx,Stk1,Stk3),
  mergeStkLvl(Stk2,Stk3,Stkx,"case branch stack").

compMoreCase([],_,_Succ,Fail,_TCont,_Opts,L,Lx,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Fail,L,Lx,D,Dx,End,C,Cx,Stk,Stkx).
compMoreCase([(P,E,Lc)|SC],VLb,Succ,Fail,TCont,Opts,L,Lx,D,Dx,End,[iLdL(VLb)|C],Cx,Stk,Stkx) :-
  genLbl(L,Fl,L0),
  genLbl(L0,Nxt,L1),
  bumpStk(Stk,Stk0),
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,contCont(Nxt),contCont(Fl),TCont,Opts,L1,L2,D,D2,End,C0,[iLbl(Nxt)|C1],Stk0,Stk1),
  verify(gencode:sameStk(Stk,Stk1),"more case branch"),
  stkLvl(Stk,Lvl),
  compTerm(E,Lc,Succ,TCont,Opts,L2,L3,D2,D3,End,C1,[iLbl(Fl),iRst(Lvl)|C2],Stk,Stk2),
  compMoreCase(SC,VLb,Succ,Fail,TCont,Opts,L3,Lx,D3,Dx,End,C2,Cx,Stk,Stk3),
  mergeStkLvl(Stk2,Stk3,Stkx,"more case branch stack").

compCnsCases([(ctpl(Cn,Args),E,Lc)],_,Cont,TCont,Opts,L,Lx,D,Dx,
	     [iUnpack(Cn,Fl)|C],Cx,Stk,Stkx) :-!,
  genLbl(L,Nxt,L1),
  genLbl(L1,Fl,L2),
  abortCont(Lc,strg("failed"),Opts,L2,L3,D,D2,Fl,Cy,Cx,Stk,_),
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,contCont(Nxt),contCont(Fl),
	      Opts,L3,L4,D2,D3,Fl,C,[iLbl(Nxt)|A1],Stk,Stk1),
  compTerm(E,Lc,Cont,TCont,Opts,L4,Lx,D3,Dx,Fl,A1,[iLbl(Fl)|Cy],Stk1,Stkx).
compCnsCases(Cases,Lc,Cont,TCont,Opts,L,Lx,D,Dx,[iIndxJmp(Mx)|T0],Cx,Stk,Stkx) :-
  splitCont(Lc,Cont,OC),
  length(Cases,Mx),
  compArms(Cases,Lc,OC,TCont,Opts,L,Lx,D,Dx,T0,Tx,Tx,Cx,Stk,Stkx).

compArms([],_Lc,_Cont,_TCont,_Opts,Lx,Lx,Dx,Dx,Tc,Tc,Cx,Cx,Stk,Stk).
compArms([(ctpl(Cn,Args),E,Lc)|Cases],_Lc,Cont,TCont,Opts,L,Lx,D,Dx,
	 [iJmp(Lbl)|T1],Tx,
	 [iLbl(Lbl),iUnpack(Cn,End)|C],Cx,Stk,Stkx) :-
  genLbl(L,Lbl,L0),
  genLbl(L0,Nxt,L1),
  genLbl(L1,End,L2),
  abortCont(Lc,strg("failed"),Opts,L2,L3,D,D3,End,Cy,Cz,Stk,_),
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,contCont(Nxt),contCont(End),
	      Opts,L3,L4,D3,D4,End,C,[iLbl(Nxt)|A1],Stk,Stk1),
  compTerm(E,Lc,Cont,TCont,Opts,L4,L5,D4,D5,End,A1,[iLbl(End)|Cy],Stk1,Stk2),
  compArms(Cases,Lc,Cont,TCont,Opts,L5,Lx,D5,Dx,T1,Tx,Cz,Cx,Stk,Stk3),
  mergeStkLvl(Stk2,Stk3,Stkx,"bad unpack cases").

removeExtraLines([],[]).
removeExtraLines([iLine(_),iLine(Lc)|Ins], Out) :-
  removeExtraLines([iLine(Lc)|Ins],Out).
removeExtraLines([I|Ins],[I|Out]) :-
  removeExtraLines(Ins,Out).



