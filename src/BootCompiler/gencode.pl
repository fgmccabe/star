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
  genLbl(D,Ex,D0),
  genLbl(D0,End,D1),
  buildArgs(Args,0,D1,D1a),
  genLine(Opts,Lc,C0,C1),
  compPtnArgs(Args,Lc,argCont,trapCont(Lc),0,contCont(Ex),abortCont(Lc,strg("failed"),Opts),Opts,D1a,D2,End,
	      C1,[iLbl(Ex)|C2],some(0),Stk0),
  compTerm(Value,Lc,retCont(Opts),trapCont(Lc),Opts,D2,Dx,End,C2,[iLbl(End)],Stk0,_Stk),
  findMaxLocal(Dx,Lx),
  (is_member(showGenCode,Opts) -> dispIns(func(Nm,H,Sig,Lx,C0));true ),
  removeExtraLines(C0,Cde),
  assem(func(Nm,H,Sig,Lx,Cde),CdTrm).
genDef(D,Opts,glbDef(Lc,Nm,Tp,Value),O,[Cd|O]) :-
  encType(funType(tplType([]),Tp),Sig),
  genLbl(D,End,D1),
  genLine(Opts,Lc,C0,C1),
  compTerm(Value,Lc,bothCont(glbCont(Nm),retCont(Opts)),trapCont(Lc),Opts,D1,Dx,End,C1,[iLbl(End)],some(0),_Stk),
  findMaxLocal(Dx,Lx),
  (is_member(showGenCode,Opts) -> dispIns(func(lbl(Nm,0),hard,Sig,Lx,C0));true ),
  removeExtraLines(C0,Cde),
  assem(func(lbl(Nm,0),hard,Sig,Lx,Cde),Cd).
genDef(_,_,lblDef(_,Lbl,Tp,Ix),O,[LblTrm|O]) :-
  encType(Tp,Sig),
  assem(struct(Lbl,strg(Sig),Ix),LblTrm).
genDef(_,_,typDef(_,Tp,Rl,IxMap),O,[TpTrm|O]) :-
  assem(tipe(Tp,Rl,IxMap),TpTrm).

glbCont(Nm,D,D,_,[iDup,iStG(Nm)|Cx],Cx,Stk,Stk).

retCont(Opts,D,D,_,C,Cx,_Stk,none) :-
  genDbg(Opts,C,[iRet|Cx]).

dropCont(D,D,_,[iDrop|Cx],Cx,Stk,Stk1) :-
  dropStk(Stk,1,Stk1).

trapCont(Lc,D,D,_,Cx,Cx,Stk,Stk) :-
  reportError("not permitted to throw",[],Lc),
  abort.

idxCont(Off,Dx,Dx,_End,[iNth(Off)|Cx],Cx,Stkx,Stkx).

sxCont(Off,Dx,Dx,_End,[iStNth(Off)|Cx],Cx,Stkx,Stkx).

initDict(scope([],[],[],0)).

buildArgs([],_,D,D).
buildArgs([A|R],Ix,D,Dx) :-
  buildArg(A,Ix,D,D0),
  Ix1 is Ix+1,
  buildArgs(R,Ix1,D0,Dx).

buildArg(idnt(Nm),Ix,scope(D,Lbs,FreeRg,Mx),
	 scope([(Nm,a(Ix),void,void)|D],Lbs,FreeRg,Mx)).
buildArg(_,_,D,D).

lclVar(Nm,Wh,scope(Vrs,_,_,_)) :-
  is_member((Nm,Wh,_,_),Vrs),!.

varLabels(scope(Vrs,_,_,_),Vrs).

defineLclVar(Nm,Lbl,End,scope(Vrs,Lbs,FreeRg,Mx),
	     scope([(Nm,l(Off),Lbl,End)|Vrs],Lbs,NFreeRg,Mx1),Off,
	     [iLocal(Nm,Lbl,End,Off)|Cx],Cx) :-
  nextFreeOff(FreeRg,Mx,Off,NFreeRg,Mx1).

clearLclVar(Nm,scope(Vrs,Lbs,FreeRg,Mx),scope(NVrs,Lbs,NFreeRg,NMx)) :-
  subtract((Nm,l(Off),_,_),Vrs,NVrs),
  addToFree(Off,FreeRg,Mx,NFreeRg,NMx).

nextFreeOff([Off|FreeRg],Mx,Off,FreeRg,Mx).
nextFreeOff([],Mx,Mx1,[],Mx1) :-
  Mx1 is Mx+1.

addToFree(Off,FreeRg,Mx,[Off|FreeRg],Mx) :-!.

defineGlbVar(Nm,scope(Vrs,Lbs,FreeRg,Mx),
	     scope([(Nm,g(Nm),none,none)|Vrs],Lbs,FreeRg,Mx)).

populateVarNames([],_,_,C,C).
populateVarNames([(Nw,idnt(Ex))|Vs],Lc,D,C,Cx) :-
  populateVarNm(Nw,Lc,Ex,D,C,C0),
  populateVarNames(Vs,Lc,D,C0,Cx).

populateVarNm(Nw,_,Ex,scope(Vrs,_,_,_),[iLocal(Nw,Frm,End,Off)|Cx],Cx) :-
  is_member((Ex,l(Off),Frm,End),Vrs),!.
populateVarNm(_,_,Ex,scope(Vrs,_,_,_),Cx,Cx) :-
  is_member((Ex,a(_),_,_),Vrs),!.
populateVarNm(_,Lc,Ex,_,C,C) :-
  reportError("variable %s not known",[Ex],Lc).

genLbl(scope(Nms,Lbs,FreeRg,Mx),Lb,scope(Nms,[Lb|Lbs],FreeRg,Mx)) :-
  length(Lbs,N),
  swritef(Lb,"_L%d",[N]).

findMaxLocal(scope(_,_,_,Mx),Mx).

localMx((_,l(Off),_),M,Mx) :- !, Mx is max(Off,M).
localMx(_,M,M).

compTerm(voyd,_,Cont,_TCont,_Opts,D,Dx,End,[iLdV|C0],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stk1),
  call(Cont,D,Dx,End,C0,Cx,Stk1,Stkx).
compTerm(Lit,_,Cont,_TCont,_,D,Dx,End,[iLdC(Lit)|C0],Cx,Stk,Stkx) :-
  isGround(Lit),!,
  bumpStk(Stk,Stk1),
  call(Cont,D,Dx,End,C0,Cx,Stk1,Stkx).
compTerm(idnt(Nm),Lc,Cont,_TCont,_,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  (lclVar(Nm,V,D) -> 
   compVar(V,Cont,D,Dx,End,C,Cx,Stk,Stkx) ;
   reportError("cannot locate variable %s",[Nm],Lc),
   abort).
compTerm(ctpl(St,A),Lc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stk2) :-!,
  compTerms(A,Lc,bothCont(allocCont(St),Cont),TCont,Opts,D,Dx,End,C,Cx,Stk,Stk2).
compTerm(intrinsic(Lc,Op,A),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  bumpStk(Stk,Stk1),
  compTerms(A,Lc,bothCont(asmCont(Op,Stk1),Cont),TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(ecll(Lc,Nm,A),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compTerms(A,Lc,bothCont(escCont(Nm,Stk),Cont),TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(cll(Lc,Nm,A),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  length(A,Ar),
  compTerms(A,Lc,cllCont(Nm,Ar,Cont,Opts),TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(ocall(Lc,Fn,A),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  length(A,Ar),
  Arity is Ar+1,
  compTerms(A,Lc,compTerm(Fn,Lc,oclCont(Arity,Cont,Opts),TCont,Opts),TCont,
	    Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(prmpt(Lc,Lb,Lam),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(Lb,Lc,
	   compTerm(Lam,Lc,
		    bothCont(asmCont(iPrompt,Stk),Cont),TCont,Opts),TCont,
	   Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(nth(Lc,Exp,Off),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compTerm(Exp,Lc,bothCont(idxCont(Off),Cont),TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(setix(Lc,Exp,Off,Vl),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compTerm(Exp,Lc,
	   compTerm(Vl,Lc,
		    bothCont(sxCont(Off),Cont),TCont,Opts),TCont,
	   Opts,D,Dx,End,C0,Cx,Stk,Stk0),
  mergeStkLvl(Stk,Stk0,Stkx,"set ix").
compTerm(case(Lc,T,Cases,Deflt),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),!,
  compCase(T,Lc,Cases,Deflt,Cont,TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(unpack(Lc,T,Cases),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(D,Nxt,D1),
  compTerm(T,Lc,contCont(Nxt),TCont,Opts,D1,D2,End,C0,[iLbl(Nxt)|C1],Stk,_Stk),
  compCnsCases(Cases,Lc,Cont,TCont,Opts,D2,Dx,C1,Cx,Stk,Stkx).
compTerm(varNames(Lc,Vrs,T),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  populateVarNames(Vrs,Lc,D,C0,C1),!,
  compTerm(T,Lc,Cont,TCont,Opts,D,Dx,End,C1,Cx,Stk,Stkx).
compTerm(whr(Lc,T,Cnd),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCond(Cnd,Lc,compTerm(T,Lc,Cont,TCont,Opts),
	   abortCont(Lc,strg("where fail"),Opts),TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(ltt(Lc,idnt(Nm),Val,Exp),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Lb,D0),
  chLine(Opts,OLc,Lc,C,C0),!,
  defineLclVar(Nm,Lb,End,D0,D1,Off,C0,[iStV(Off)|C1]),
  compTerm(Val,Lc,bothCont(stoCont(Off,Lb),
			   compTerm(Exp,Lc,Cont,TCont,Opts)),TCont,Opts,D1,Dx,End,C1,Cx,Stk,Stkx).
compTerm(error(Lc,Msg),_OLc,_Cont,_TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  abortCont(Lc,Msg,Opts,D,Dx,End,C,Cx,Stk,Stkx). % no continuation after an error
compTerm(rais(Lc,E),_,_,TCont,Opts,D,Dx,End,C,Cx,Stk,none) :-
  compTerm(E,Lc,TCont,trapCont,Opts,D,Dx,End,C,Cx,Stk,_).
compTerm(cnd(Lc,dsj(TLc,TL,TR),L,R),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  compTerm(cnd(Lc,TL,L,cnd(TLc,TR,L,R)),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx).
compTerm(cnd(Lc,T,L,R),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Cont,OC),!,
  compCond(T,Lc,compTerm(L,Lc,OC,TCont,Opts),compTerm(R,Lc,OC,TCont,Opts),TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(seq(Lc,L,R),OLc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  chLine(Opts,OLc,Lc,C,C0),!,
  compTerm(L,Lc,bothCont(resetCont(Stk),compTerm(R,Lc,Cont,TCont,Opts)),TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(Cond,Lc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  isCond(Cond),!,
  genLbl(D,Nx,D0),
  compCond(Cond,Lc,bothCont(succCont,contCont(Nx)),bothCont(failCont,contCont(Nx)),TCont,
      Opts,D0,D1,End,C,[iLbl(Nx)|C1],Stk,Stk1),
  call(Cont,D1,Dx,End,C1,Cx,Stk1,Stkx).
compTerm(doAct(Lc,Act),OLc,Cont,_TCont,Opts,D,Dx,_End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(D,End,D0),!,
  splitCont(Lc,Cont,RCont),
  compAction(Act,Lc,RCont,propagateCont(RCont),RCont,RCont,Opts,D0,Dx,End,C0,[iLbl(End)|Cx],Stk,Stkx).
compTerm(T,Lc,_,_,_,Dx,Dx,_,C,C,Stk,Stk) :-
  reportError("cannot compile %s",[T],Lc),
  abort.

compVar(a(A),Cont,D,Dx,End,[iLdA(A)|C0],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stk1),
  call(Cont,D,Dx,End,C0,Cx,Stk1,Stkx).
compVar(l(X),Cont,D,Dx,End,[iLdL(X)|C0],Cx,Stk,Stkx) :- !,
  bumpStk(Stk,Stk1),
  call(Cont,D,Dx,End,C0,Cx,Stk1,Stkx).
compVar(g(GlbNm),Cont,D,Dx,End,[iLdG(GlbNm)|C0],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stk1),
  call(Cont,D,Dx,End,C0,Cx,Stk1,Stkx).

/* Terms are generated in reverse order*/
compTerms([],_,Cont,_,_,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Cont,D,Dx,End,C,Cx,Stk,Stkx).
compTerms([T|Ts],Lc,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerms(Ts,Lc,contCont(Nxt),TCont,Opts,D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk0),
  compTerm(T,Lc,Cont,TCont,Opts,D1,Dx,End,C0,Cx,Stk0,Stkx).

/* Compile actions as sequences with several possible continuations */

compAction(seq(Lc,A,B),_,Cont,PCont,RCont,ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  propagatingAction(A),!,
  compAction(A,Lc,combineCont(PCont,
			      compAction(B,Lc,Cont,PCont,RCont,ECont,Opts)),
	     PCont,RCont,ECont,
	     Opts,D,Dx,End,C,Cx,Stk,Stkx).
compAction(seq(Lc,A,B),_,Cont,PCont,RCont,ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  compAction(A,Lc,bothCont(resetCont(Stk),
			   compAction(B,Lc,Cont,PCont,RCont,ECont,Opts)),
	     PCont,RCont,ECont,
	     Opts,D,Dx,End,C,Cx,Stk,Stkx).
compAction(perfDo(Lc,Exp),OLc,Cont,_PCont,_RCont,ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(Exp,Lc,Cont,ECont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(justDo(Lc,Exp),_,Cont,_PCont,_,ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(Exp,Lc,Cont,ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx).
compAction(varD(Lc,Ptn,E),OLc,Cont,_,_,ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(E,Lc,compPtn(Ptn,Lc,Cont,ECont,ECont,Opts),ECont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(assignD(Lc,L,E),OLc,Cont,_,_RCont,_ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  bumpStk(Stk,Stk0),
  compTerm(E,Lc,
	   compTerm(L,Lc,
		    bothCont(asmCont(iAssign,Stk0),Cont),ECont,Opts),ECont,
	   Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(cnd(Lc,T,L,R),OLc,Cont,PCont,RCont,ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Cont,SCont),
  compCond(T,Lc,compAction(L,Lc,SCont,PCont,RCont,ECont,Opts),
	   compAction(R,Lc,SCont,PCont,RCont,ECont,Opts),ECont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compAction(rtnDo(Lc,E),_Lc,_Cont,_,RCont,ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(E,Lc,RCont,ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx).
compAction(raisDo(Lc,E),_Lc,_Cont,_PCont,_RCont,ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(E,Lc,ECont,trapCont,Opts,D,Dx,End,C,Cx,Stk,Stkx).
compAction(whle(Lc,Cond,Body),_,Cont,PCont,RCont,ECont,Opts,D,Dx,End,
	   [iJmp(Tst),iLbl(Nxt)|C],Cx,Stk,Stk) :-
  genLbl(D,Nxt,D0),
  genLbl(D0,Tst,D1),
  compCond(Cond,Lc,jmpCont(Nxt),Cont,ECont,Opts,D1,D2,End,C2,Cx,Stk,Stk1),
  compAction(Body,Lc,bothCont(resetCont(Stk),contCont(Tst)),PCont,RCont,ECont,Opts,D2,Dx,End,C,
	     [iLbl(Tst)|C2],Stk,Stk2),
  verify(gencode:sameStk(Stk,Stk1),"while test stack"),
  verify(gencode:sameStk(Stk,Stk2),"while body stack").
compAction(untl(Lc,Cond,Body),_,Cont,PCont,RCont,ECont,Opts,D,Dx,End,
	   [iLbl(Nxt)|C],Cx,Stk,Stk) :-
  genLbl(D,Nxt,D0),
  genLbl(D0,Tst,D1),
  compAction(Body,Lc,bothCont(resetCont(Stk),contCont(Tst)),PCont,RCont,ECont,
	     Opts,D1,D2,End,C,[iLbl(Tst)|C1],Stk,Stk1),
  compCond(Cond,Lc,Cont,contCont(Nxt),ECont,Opts,D2,Dx,End,C1,Cx,Stk1,Stkx),
  verify(gencode:sameStk(Stk,Stk1),"until test stack"),
  verify(gencode:sameStk(Stk,Stkx),"until body stack").
compAction(tryDo(Lc,A,H),_,Cont,_PCont,RCont,ECont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  genLbl(D0,ThLbl,D1),
  stkLvl(Stk,Lvl),
  compAction(A,Lc,Cont,throwCont(jmpCont(ThLbl)),RCont,
	     jmpCont(ThLbl),Opts,D1,D2,End,C,[iLbl(ThLbl),iDropTo(Lvl)|C1],Stk,Stk1),
  bumpStk(Stk,Stk0),
  compTerm(H,Lc,contCont(Nxt),ECont,Opts,D2,D3,End,C1,[iLbl(Nxt)|C2],Stk0,Stk2),
  oclCont(1,Cont,Opts,D3,Dx,End,C2,Cx,Stk2,Stk3),
  mergeStkLvl(Stk1,Stk3,Stkx,"catch").

propagatingAction(perfDo(_,_)) :-!.
propagatingAction(justDo(_,_)) :-!.
propagatingAction(seq(_,_,R)) :- propagatingAction(R).

propagateCont(ECont,Cont,D,Dx,End,[iCLbl(lbl("star.action#ok",1),Nxt)|C],Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D1),
  dropStk(Stk,1,Stk0),
  call(ECont,D1,D2,End,C,[iLbl(Nxt),iDrop|C0],Stk0,Stk1),
  call(Cont,D2,Dx,End,C0,Cx,Stk0,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"propagate").

throwCont(ThCont,Cont,D,Dx,End,[iUnpack(lbl("star.action#err",1),Nxt)|C],Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  call(ThCont,D0,D1,End,C,[iLbl(Nxt)|C1],Stk,_Stk0),
  call(Cont,D1,Dx,End,C1,Cx,Stk,Stkx).

combineCont(ACont,BCont,D,Dx,End,C,Cx,S,Sx) :-
  call(ACont,BCont,D,Dx,End,C,Cx,S,Sx).

contCont(Lbl,D,D,_,C,Cx,Stk,Stk) :-
  (nonvar(Cx),Cx=[iLbl(Lbl)|_]) ->
  C=Cx ;
  C=[iJmp(Lbl)|Cx].			% some special logic here

combCont([],Dx,Dx,_,Cx,Cx,Stk,Stk).
combCont([Cnt],D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Cnt,D,Dx,End,C,Cx,Stk,Stkx).
combCont([Cnt|Cs],D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Cnt,D,D0,End,C,C0,Stk,Stk0),
  combCont(Cs,D0,Dx,End,C0,Cx,Stk0,Stkx).

bothCont(L,R,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(L,D,D0,End,C,C0,Stk,Stk0),
  call(R,D0,Dx,End,C0,Cx,Stk0,Stkx).

frameSig(Stk,strg(Sig)) :-
  mkTplTipe(Stk,FrTp),
  encLtp(FrTp,Sig).

frameIns(some(Stk),[iFrame(Sig)|Cx],Cx) :-
  frameSig(Stk,Sig).
frameIns(none,Cx,Cx).

allocCont(Str,D,D,_,[iAlloc(Str)|C],Cx,Stk,Stkx) :-
  frameIns(Stk,C,Cx),
  popStack(Str,Stk,Stkx).

popStack(lbl(_,Ar),St,Stx) :-
  dropStk(St,Ar-1,Stx).

stkLvl(some(Lvl),Lvl).

resetCont(Lvl,D,D,_,Cx,Cx,Lvl,Lvl) :-!.
resetCont(Lvl,D,D,_,[iDrop|Cx],Cx,Stk0,Lvl) :-
  dropStk(Stk0,1,Lvl).
resetCont(some(Lvl),D,D,_,[iRst(Lvl)|Cx],Cx,_,some(Lvl)).

stoCont(Off,Lb,Dx,Dx,_End,[iStL(Off),iLbl(Lb)|Cx],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stkx).

releaseCont(Nm,D,Dx,_,Cx,Cx,Stk,Stk) :-
  clearLclVar(Nm,D,Dx).

asmCont(IOp,Stkx,D,D,_,[IOp|Cx],Cx,_,Stkx).

escCont(Nm,Stk0,D,D,_,[iEscape(Nm)|C],Cx,_Stk,Stkx) :-
  bumpStk(Stk0,Stkx),
  frameIns(Stkx,C,Cx).

cllCont(Nm,_Ar,retCont(_),Opts,Dx,Dx,_,C,Cx,_Stk,none) :-!,
  genDbg(Opts,C,[iTail(Nm)|Cx]).
cllCont(Nm,Ar,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genDbg(Opts,C,[iCall(Nm)|C0]),
  dropStk(Stk,Ar,Stk0),
  bumpStk(Stk0,Stk1),
  frameIns(Stk1,C0,C1),
  call(Cont,D,Dx,End,C1,Cx,Stk1,Stkx).

oclCont(Arity,retCont(_),Opts,Dx,Dx,_End,C,Cx,_,none) :-!,
  genDbg(Opts,C,[iOTail(Arity)|Cx]).
oclCont(Arity,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genDbg(Opts,C,[iOCall(Arity)|C0]),
  dropStk(Stk,Arity,Stk0),
  bumpStk(Stk0,Stk1),
  frameIns(Stk1,C0,C1),
  call(Cont,D,Dx,End,C1,Cx,Stk1,Stkx).

jmpCont(Lbl,D,D,_End,[iJmp(Lbl)|Cx],Cx,Stk,Stk).

isJmpCont(jmpCont(Lbl),Lbl).

isBrkCont(jmpCont(_)).
isBrkCont(retCont(_)).

isSimpleCont(retCont(_)).
isSimpleCont(jmpCont(_)).
isSimpleCont(contCont(_)).
isSimpleCont(bothCont(L,R)) :-
  isSimpleCont(L),
  isSimpleCont(R).
isSimpleCont(resetCont(_)).
isSimpleCont(succCont).
isSimpleCont(failCont).

splitCont(_,Cont,Cont) :- isSimpleCont(Cont),!.
splitCont(Lc,Cont,onceCont(Lc,_,Cont)).

onceCont(_,L,Cont,D,Dx,End,[iLbl(Lb)|C],Cx,Stk,Stkx) :-
  var(L),!,
  genLbl(D,Lb,D0),
  call(Cont,D0,Dx,End,C,Cx,Stk,Stkx),
  L=(Lb,Stk,Stkx).
onceCont(_,(Lb,Stkin,Stkout),_,D,D,_,C,Cx,Stk,Stkout) :-
  reconcileStack(Stk,Stkin,Stkout,C,[iJmp(Lb)|Cx]).
onceCont(Lc,(_,Stkin,Stkout),_,D,D,_,Cx,Cx,_Stk,Stkout) :-
  reportError("cannot reconcile stacks [%w,%w]",[Stkin,Stkout],Lc),
  abort.

unitCont(Stk,D,D,_,[iLdC(U)|Cx],Cx,Stk,Stkx) :-!,
  bumpStack(Stk,Stkx),
  mkTpl([],U).
unitCont(Stk,D,Dx,End,C,Cx,Stk0,Stkx) :-
  resetCont(Stk,D,D0,End,C,C0,Stk0,Stk1),
  unitCont(Stk1,D0,Dx,End,C0,Cx,Stk1,Stkx).

reconcileStack(_,_,none,C,C) :-!.
reconcileStack(Stk,Stk,_,C,C) :-!.
reconcileStack(some(Stki),some(Stk),_,[iRst(Stk)|C],C) :-
  Stki>Stk,!.

succCont(D,D,_,[iLdC(enum("star.core#true"))|Cx],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stkx).

failCont(D,D,_,[iLdC(enum("star.core#false"))|Cx],Cx,Stk,Stkx) :-
  bumpStk(Stk,Stkx).

nullCont(D,D,_,C,C,Stk,Stk).

insCont(Ins,D,D,_,[Ins|C],C,Stk,Stk).

abortCont(Lc,Msg,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  locTerm(Lc,LT),
  compTerms([LT,Msg],Lc,asmCont(iAbort,none),trapCont(Lc),Opts,D,Dx,End,C,Cx,Stk,Stkx).

indexCont(Ix,D,D,_,Stk,Stk1,[iDup,iNth(Ix)|Cx],Cx) :-
  bumpStk(Stk,Stk1).
stkArgCont(_,D,D,_,Stk,Stk1,C,C) :-
  bumpStk(Stk,Stk1).

argCont(_Ix,D,D,_,Stk,Stk,Cx,Cx).

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

compPtn(voyd,_,Succ,_Fail,_TCont,_Opts,D,Dx,End,[iDrop|C],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stk1),
  call(Succ,D,Dx,End,C,Cx,Stk1,Stkx).
compPtn(Lit,_,Succ,Fail,_TCont,_Opts,D,Dx,End,[iLdC(Lit),iCmp(Fl)|C],Cx,Stk,Stkx) :-
  isLiteral(Lit),!,
  dropStk(Stk,1,Stk1),
  ptnTest(Succ,Fail,Fl,D,Dx,End,C,Cx,Stk1,Stkx).
compPtn(idnt(Nm),_,Succ,_,_,_Opts,D,Dx,End,[iStL(Off),iLbl(Lb)|C],Cx,Stk,Stkx) :-
  genLbl(D,Lb,D0),
  defineLclVar(Nm,Lb,End,D0,D1,Off,C,C0),
  dropStk(Stk,1,Stk1),
  call(Succ,D1,Dx,End,C0,Cx,Stk1,Stkx).
compPtn(anon,_,Succ,_,_,_Opts,D,Dx,End,[iDrop|C],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stk1),
  call(Succ,D,Dx,End,C,Cx,Stk1,Stkx).
compPtn(ctpl(St,Args),Lc,Succ,Fail,TCont,Opts,D,Dx,End,[iUnpack(St,Fl)|C],Cx,Stk,Stkx) :-
  genLbl(D,Fl,D0),
  dropStk(Stk,1,Stka),
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,Succ,contCont(Fl),
	      Opts,D0,D1,End,C,[iLbl(Fl)|C1],Stka,Stkx),
  call(Fail,D1,Dx,End,C1,Cx,Stk,_).
compPtn(whr(Lc,P,Cnd),OLc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  chLine(Opts,OLc,Lc,C,C0),
  compPtn(P,Lc,contCont(Nxt),Fail,TCont,Opts,D0,D1,End,C0,[iLbl(Nxt)|C1],Stk,Stk1),
  verify(gencode:dropStk(Stk,1,Stk1),"where stack"),
  compCond(Cnd,Lc,Succ,Fail,TCont,Opts,D1,Dx,End,C1,Cx,Stk1,Stkx).
compPtn(T,Lc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(T,Lc,contCont(Nxt),TCont,Opts,D0,D1,End,C,[iLbl(Nxt),iCmp(Fl)|C0],Stk,Stk1),
  dropStk(Stk1,2,Stk2),
  verify(gencode:dropStk(Stk,1,Stk2),"Stk2 off"),
  ptnTest(Succ,Fail,Fl,D1,Dx,End,C0,Cx,Stk2,Stkx).

ptnTest(Succ,Fail,Fl,D,Dx,End,C,Cx,Stk,Stkx) :-
  isJmpCont(Fail,Fl),!,
  call(Succ,D,Dx,End,C,Cx,Stk,Stkx).
ptnTest(Succ,Fail,Fl,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Fl,D0),
  call(Succ,D0,D1,End,C,[iLbl(Fl)|C1],Stk,Stk1),
  call(Fail,D1,Dx,End,C1,Cx,Stk,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"ptn test").

bumpStk(some(Stk),some(Stk1)) :- Stk1 is Stk+1.
dropStk(some(Stk),Cnt,some(Stk1)) :- Stk1 is Stk-Cnt.

sameStk(none,none) :-!.
sameStk(some(S),some(S)).

mergeStkLvl(none,Stk,Stk,_) :-!.
mergeStkLvl(Stk,none,Stk,_) :-!.
mergeStkLvl(Stk1,Stk2,Stk1,Msg) :-
  verify(gencode:sameStk(Stk1,Stk2),Msg).

compPtnArgs([],_,_,_,_,Succ,_,_,D,Dx,End,C,Cx,Stk,Stkx)			  :- 
  call(Succ,D,Dx,End,C,Cx,Stk,Stkx).
compPtnArgs([voyd|R],Lc,ArgCont,TCont,Ix,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stkx) :- 
  call(ArgCont,Ix,D,_D0,End,Stk,_Stk0,C,[iDrop|C0]),
  Ix1 is Ix+1,
  compPtnArgs(R,Lc,ArgCont,TCont,Ix1,Succ,Fail,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compPtnArgs([A|R],Lc,ArgCont,TCont,Ix,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stkx)	  :- 
  call(ArgCont,Ix,D,D0,End,Stk,Stk0,C,C0),
  genLbl(D0,Nxt,D1),
  compPtnArg(A,Ix,Lc,
	     contCont(Nxt),Fail,TCont,Opts,D1,D2,End,C0,[iLbl(Nxt)|C1],Stk0,Stk1),
  Ix1 is Ix+1,
  verify(gencode:sameStk(Stk,Stk1),"argument pattern"),
  compPtnArgs(R,Lc,ArgCont,TCont,Ix1,Succ,Fail,Opts,D2,Dx,End,C1,Cx,Stk1,Stkx).

compPtnArg(idnt(V),Ix,_,Succ,_Fail,_TCont,_,D,Dx,End,C,Cx,Stk,Stkx) :-
  lclVar(V,a(Ix),D),!,
  call(Succ,D,Dx,End,C,Cx,Stk,Stkx).
compPtnArg(P,_Ix,Lc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  compPtn(P,Lc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx).

isCond(cnj(_,_,_)).
isCond(cnd(_,_,_,_)).
isCond(dsj(_,_,_)).
isCond(ng(_,_)).
isCond(mtch(_,_,_)).

isTrueSymb(enum("star.core#true")).
isFalseSymb(enum("star.core#false")).

compCond(enum(Sy),_Lc,Succ,_Fail,_TCont,_Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  isTrueSymb(Sy),!,
  call(Succ,D,Dx,End,C,Cx,Stk,Stkx).
compCond(enum(Sy),_,_,Fail,_T,_,D,Dx,End,C,Cx,Stk,Stkx) :-
  isFalseSymb(Sy),!,
  call(Fail,D,Dx,End,C,Cx,Stk,Stkx).
compCond(cnj(Lc,dsj(LLc,LL,LR),R),_,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  compCond(dsj(LLc,cnj(Lc,LL,R),cnj(Lc,LR,R)),Lc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx).
compCond(cnj(Lc,L,R),OLc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Lc,Fail,OF),
  compCond(L,Lc,compCond(R,Lc,Succ,OF,TCont,Opts),OF,TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(dsj(Lc,L,R),OLc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  splitCont(Lc,Succ,OSc),
  chLine(Opts,OLc,Lc,C,C0),
  compCond(L,Lc,OSc,compCond(R,Lc,OSc,Fail,TCont,Opts),TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(ng(Lc,Cn),OLc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCond(Cn,Lc,Fail,Succ,TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(cnd(Lc,dsj(LLc,TL,TR),L,R),OLc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-!,
  compCond(dsj(LLc,cnd(Lc,TL,L,R),cnd(Lc,TR,L,R)),OLc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx).
compCond(cnd(Lc,T,L,R),OLc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  splitCont(Lc,Succ,OSc),
  splitCont(Lc,Fail,OFl),
  chLine(Opts,OLc,Lc,C,C0),
  compCond(T,Lc,compCond(L,Lc,OSc,OFl,TCont,Opts),compCond(R,Lc,OSc,OFl,TCont,Opts),TCont,
	   Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(mtch(Lc,P,E),OLc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(E,Lc,compPtn(P,Lc,Succ,bothCont(resetCont(Stk),Fail),TCont,Opts),TCont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(E,Lc,Succ,Fail,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(E,Lc,testCont(Succ,Fail),TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx).

testCont(Succ,Fail,D,Dx,End,[iIfNot(Fl)|C],Cx,Stk,Stkx) :-
  dropStk(Stk,1,Stk0),
  genLbl(D,Fl,D0),
  call(Succ,D0,D1,End,C,[iLbl(Fl)|C0],Stk0,Stk1),
  call(Fail,D1,Dx,End,C0,Cx,Stk0,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"test exp").

compCase(T,Lc,Cases,Deflt,Cont,TCont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D1),
  splitCont(Lc,Cont,OC),
  compTerm(T,Lc,contCont(Nxt),TCont,Opts,D1,D2,End,C,[iLbl(Nxt),iCase(Mx)|T0],Stk,Stk0),
  genLbl(D2,Dflt,D3),
  genCaseTable(Cases,Mx,Table),
  stkLvl(Stk,Lvl),
  compCases(Table,0,Mx,OC,contCont(Dflt),TCont,Dflt,
	    Opts,D3,D4,End,T0,Tx,Tx,[iLbl(Dflt),iRst(Lvl)|C1],Stk0),
  compTerm(Deflt,Lc,OC,TCont,Opts,D4,Dx,End,C1,Cx,Stk,Stkx).

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

compCases([],Mx,Mx,_Succ,_,_TCont,_,_Opts,D,D,_End,Tc,Tc,C,C,_Stk).
compCases([],Ix,Mx,Succ,Fail,TCont,Dflt,Opts,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk) :-
  Ix1 is Ix+1,
  compCases([],Ix1,Mx,Succ,Fail,TCont,Dflt,Opts,D,Dx,End,Tc,Tx,C,Cx,Stk).
compCases([(Ix,Case)|Cs],Ix,Mx,Succ,Fail,TCont,Dflt,Opts,D,Dx,End,[iJmp(Lbl)|Tc],Tx,C,Cx,Stk) :-!,
  genLbl(D,Lbl,D0),
  compCaseBranch(Case,Lbl,Succ,Fail,TCont,Opts,D0,D1,End,C,C1,Stk,_),
  H1 is Ix+1,
  compCases(Cs,H1,Mx,Succ,Fail,TCont,Dflt,Opts,D1,Dx,End,Tc,Tx,C1,Cx,Stk).
compCases(Cs,Ix,Mx,Succ,Fail,TCont,Dflt,Opts,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk) :-
  H1 is Ix+1,
  compCases(Cs,H1,Mx,Succ,Fail,TCont,Dflt,Opts,D,Dx,End,Tc,Tx,C,Cx,Stk).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E,Lc)],Lbl,Succ,Fail,TCont,Opts,D,Dx,End,[iLbl(Lbl)|C],Cx,Stk,Stkx) :-!,
  genLbl(D,Nxt,D1),
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,contCont(Nxt),Fail,TCont,Opts,D1,D2,End,C0,[iLbl(Nxt)|C1],Stk,Stk1),
  compTerm(E,Lc,Succ,TCont,Opts,D2,Dx,End,C1,Cx,Stk1,Stkx).
compCaseBranch([(P,E,Lc)|SC],Lbl,Succ,Fail,TCont,Opts,D,Dx,End,
	       [iLbl(Lbl),iTL(Off),iLbl(VLb)|C],Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D1),
  genLbl(D1,Fl,D2),
  genLbl(D2,VLb,D3),
  defineLclVar("__",VLb,End,D3,D4,Off,C,C0),
  genLine(Opts,Lc,C0,C1),
  compPtn(P,Lc,contCont(Nxt),contCont(Fl),TCont,Opts,D4,D5,End,C1,[iLbl(Nxt)|C2],Stk,Stk1),
  verify(gencode:dropStk(Stk1,1,Stk),"case branch"),
  stkLvl(Stk1,Lvl1),
  compTerm(E,Lc,Succ,TCont,Opts,D5,D6,End,C2,[iLbl(Fl),iRst(Lvl1)|C3],Stk1,Stk2),
  compMoreCase(SC,Off,Succ,Fail,TCont,Opts,D6,Dx,End,C3,Cx,Stk1,Stk3),
  mergeStkLvl(Stk2,Stk3,Stkx,"case branch stack").

compMoreCase([],_,_Succ,Fail,_TCont,_Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Fail,D,Dx,End,C,Cx,Stk,Stkx).
compMoreCase([(P,E,Lc)|SC],VLb,Succ,Fail,TCont,Opts,D,Dx,End,[iLdL(VLb)|C],Cx,Stk,Stkx) :-
  genLbl(D,Fl,D0),
  genLbl(D0,Nxt,D1),
  bumpStk(Stk,Stk0),
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,contCont(Nxt),contCont(Fl),TCont,Opts,D1,D2,End,C0,[iLbl(Nxt)|C1],Stk0,Stk1),
  verify(gencode:sameStk(Stk,Stk1),"more case branch"),
  stkLvl(Stk,Lvl),
  compTerm(E,Lc,Succ,TCont,Opts,D2,D3,End,C1,[iLbl(Fl),iRst(Lvl)|C2],Stk,Stk2),
  compMoreCase(SC,VLb,Succ,Fail,TCont,Opts,D3,Dx,End,C2,Cx,Stk,Stk3),
  mergeStkLvl(Stk2,Stk3,Stkx,"more case branch stack").

compCnsCases([(ctpl(Cn,Args),E,Lc)],_,Cont,TCont,Opts,D,Dx,
	     [iUnpack(Cn,Fl)|C],Cx,Stk,Stkx) :-!,
  genLbl(D,Nxt,D1),
  genLbl(D1,Fl,D2),
  abortCont(Lc,strg("failed"),Opts,D2,D2b,Fl,Cy,Cx,Stk,_),
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,contCont(Nxt),contCont(Fl),
	      Opts,D2b,D3,Fl,C,[iLbl(Nxt)|A1],Stk,Stk1),
  compTerm(E,Lc,Cont,TCont,Opts,D3,Dx,Fl,A1,[iLbl(Fl)|Cy],Stk1,Stkx).
compCnsCases(Cases,Lc,Cont,TCont,Opts,D,Dx,[iIndxJmp(Mx)|T0],Cx,Stk,Stkx) :-
  splitCont(Lc,Cont,OC),
  length(Cases,Mx),
  compArms(Cases,Lc,OC,TCont,Opts,D,Dx,T0,Tx,Tx,Cx,Stk,Stkx).

compArms([],_Lc,_Cont,_TCont,_Opts,Dx,Dx,Tc,Tc,Cx,Cx,Stk,Stk).
compArms([(ctpl(Cn,Args),E,Lc)|Cases],_Lc,Cont,TCont,Opts,D,Dx,
	 [iJmp(Lbl)|T1],Tx,
	 [iLbl(Lbl),iUnpack(Cn,End)|C],Cx,Stk,Stkx) :-
  genLbl(D,Lbl,D0),
  genLbl(D0,Nxt,D1),
  genLbl(D1,End,D2),
  abortCont(Lc,strg("failed"),Opts,D2,D2b,End,Cy,Cz,Stk,_),
  compPtnArgs(Args,Lc,stkArgCont,TCont,0,contCont(Nxt),contCont(End),
	      Opts,D2b,D3,End,C,[iLbl(Nxt)|A1],Stk,Stk1),
  compTerm(E,Lc,Cont,TCont,Opts,D3,D4,End,A1,[iLbl(End)|Cy],Stk1,Stk2),
  compArms(Cases,Lc,Cont,TCont,Opts,D4,Dx,T1,Tx,Cz,Cx,Stk,Stk3),
  mergeStkLvl(Stk2,Stk3,Stkx,"bad unpack cases").

removeExtraLines([],[]).
removeExtraLines([iLine(_),iLine(Lc)|Ins], Out) :-
  removeExtraLines([iLine(Lc)|Ins],Out).
removeExtraLines([I|Ins],[I|Out]) :-
  removeExtraLines(Ins,Out).

mkFields(Fields,Out) :-
  sort(Fields,gencode:compField,Flds),
  genFields(Flds,0,Out).

compField((Nm1,_),(Nm2,_)) :-
  str_lt(Nm1,Nm2),!.

genFields([],_,[]).
genFields([(Nm,Tp)|Fields],Ix,[Fld|Out]) :-
  encType(Tp,Sig),
  mkTpl([lbl(Nm,0),strg(Sig)],Fld),
  Ix1 is Ix+1,
  genFields(Fields,Ix1,Out).

genBoot(Defs,pkg(Pkg,_),[BootCde]) :-
  packageVarName(Pkg,"",PkgVr),
  is_member(glbDef(_,PkgVr,faceType(ExpFlds,_),_),Defs),
  is_member(("_main",_),ExpFlds),!,
  localName(Pkg,value,"_main",MainNm),
  localName(Pkg,value,"_boot",BootNm),
  MainLbl = lbl(MainNm,1),
  BootLbl = lbl(BootNm,0),
  is_member(fnDef(_,MainLbl,_,_,_),Defs),!,
  encType(funType(tplType([]),tplType([])),Sig),
  BootMtd = func(BootLbl,hard,Sig,0,
		   [iEscape("_command_line"),
		    iCall(MainLbl)|Cx]),
  frameIns(some(0),Cx,[iHalt]),
  assem(BootMtd,BootCde).
genBoot(_,_,[]).

