:- module(resolve,[overload/3]).

:- use_module(dict).
:- use_module(declmgt).
:- use_module(misc).
:- use_module(errors).
:- use_module(types).
:- use_module(freshen).
:- use_module(canon).
:- use_module(unify).
:- use_module(location).

overload(Defs,Dict,RDefs) :-
  map(Defs,resolve:overloadDef(Dict),RDefs).

overloadDef(Dict,funDef(Lc,Nm,ExtNm,H,Tp,Cx,Eqns),RF) :-!,
  overloadFunction(Lc,Nm,ExtNm,H,Tp,Cx,Eqns,Dict,RF).
overloadDef(Dict,varDef(Lc,Nm,ExtNm,Cx,Tp,Value),RD) :-!,
  overloadDefn(Lc,Nm,ExtNm,Cx,Tp,Value,Dict,RD).
overloadDef(_,Def,Def).

overloadFunction(Lc,Nm,ExtNm,H,Tp,[],Eqns,Dict,funDef(Lc,Nm,ExtNm,H,Tp,[],REqns)) :-
  overloadEquations(Eqns,Dict,[],REqns),!.
overloadFunction(Lc,Nm,ExtNm,H,Tp,Cx,Eqns,Dict,funDef(Lc,Nm,ExtNm,H,Tp,[],REqns)) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  overloadEquations(Eqns,FDict,CVars,REqns),!.

overloadEquations(Eqns,Dict,Extra,REqns) :-
  overloadList(Eqns,overloadEquation(Extra),Dict,REqns).

overloadEquation(Extra,rule(Lc,Args,G,Exp),Dict,
		 rule(Lc,RArgs,RG,RExp)) :-
  defineArgVars(Args,Dict,RDict),
  resolveTerm(Args,RDict,RA),
  addExtra(Extra,RA,RArgs),
  resolveGuard(G,RDict,RG),
  resolveTerm(Exp,RDict,RExp).

resolveGuard(none,_,none) :-!.
resolveGuard(some(G),Dict,some(RG)) :-
  resolveTerm(G,Dict,RG).

% These are used when resolving lambdas only. A lambda cannot introduce any dictionary variables
overloadRule(Over,rule(Lc,Args,G,Exp),Dict,St,Stx,rule(Lc,RArgs,RG,RExp)) :-
  defineArgVars(Args,Dict,RDict),
  overloadTerm(Args,RDict,St,St0,RArgs),
  overloadGuard(G,RDict,St0,St1,RG),
  call(Over,Exp,RDict,St1,Stx,RExp).

overloadDefn(Lc,Nm,ExtNm,[],Tp,Exp,Dict,varDef(Lc,Nm,ExtNm,[],Tp,RExp)) :-
  resolveTerm(Exp,Dict,RExp).
overloadDefn(Lc,Nm,ExtNm,Cx,Tp,Exp,Dict,varDef(Lc,Nm,ExtNm,[],Tp,
					       lambda(Lc,Lbl,[],rule(Lc,tple(Lc,CVars),none,RExp),OTp))) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  contractTypes(Cx,Tps),
  makeContractFunType(Tp,Tps,OTp),
  resolveTerm(Exp,FDict,RExp),
  lambdaLbl(Nm,"over",Lbl).

makeContractFunType(allType(V,T),Cx,allType(V,CT)) :-
  makeContractFunType(T,Cx,CT).
makeContractFunType(constrained(T,_C),Cx,CT) :-
  makeContractFunType(T,Cx,CT).
makeContractFunType(T,Cx,funType(tplType(Cx),T)).

defineCVars(_,[],Dict,[],Dict).
defineCVars(Lc,[implementsFace(X,faceType(Flds,_))|Cx],Dict,CVars,FDict) :-
  fieldVars(Lc,X,Flds,CVars,CVrs,Dict,CDict),
  defineCVars(Lc,Cx,CDict,CVrs,FDict).
defineCVars(Lc,[implicit(Nm,Tp)|Cx],Dict,[v(Lc,Nm,Tp)|CVars],FDict) :-
  declareVr(Lc,Nm,Tp,none,Dict,DDict),
  defineCVars(Lc,Cx,DDict,CVars,FDict).
defineCVars(Lc,[raises(Tp)|Cx],Dict,[v(Lc,TpBlkNm,Tp)|CVars],FDict) :-
  tpName(Tp,TpNm),
  mangleName("$R",conTract,TpNm,TpBlkNm),
  declareTryScope(Lc,Tp,TpBlkNm,Dict,Dict1),
  defineCVars(Lc,Cx,Dict1,CVars,FDict).
defineCVars(Lc,[Con|Cx],Dict,[v(Lc,CVarNm,ConTp)|CVars],FDict) :-
  implementationName(Con,ImplNm),
  mangleName("_",value,ImplNm,CVarNm),
  contractType(Con,ConTp),
  declareImplementation(ImplNm,CVarNm,ConTp,Dict,D0),
  declareVr(Lc,CVarNm,ConTp,none,D0,D1),
  defineCVars(Lc,Cx,D1,CVars,FDict).

fieldVars(_,_,[],CVars,CVars,Dict,Dict).
fieldVars(Lc,Tp,[(FldNm,FldTp)|Flds],[NV|CVrs],XVrs,Dict,CDict) :-
  genVar(FldNm,Lc,funType(tplType([Tp]),FldTp),NV),
  fieldVars(Lc,Tp,Flds,CVrs,XVrs,[access(FldNm,NV)|Dict],CDict).

defineArgVars(tple(_,Args),Dict,RDict) :-
  rfold(Args,resolve:defineArg,Dict,RDict).
defineArgVars(_,Dict,Dict).

defineArg(v(Lc,Nm,Tp),Dict,RDict) :-!,
  declareVr(Lc,Nm,Tp,none,Dict,RDict).
defineArg(capply(_,_,Args,_),Dict,RDict) :-!,
  defineArgVars(Args,Dict,RDict).
defineArg(_,Dict,Dict).
  
resolveTerm(Term,Dict,Resolved) :-
%  locOfCanon(Term,Lc),
%  reportMsg("resolving %s",[Term],Lc),
  overloadTerm(Term,Dict,inactive,St,RTerm),!,
  resolveAgain(inactive,St,Term,RTerm,Dict,Resolved).

% Somewhat complex logic to allow multiple iterations unless it will not help
resolveAgain(_,resolved,Term,T,Dict,R) :- !,
%  reportMsg("resolving again %s",[can(T)]),
  overloadTerm(T,Dict,inactive,St,T0),
  resolveAgain(inactive,St,Term,T0,Dict,R).
resolveAgain(_,inactive,_,T,_,T) :- !.
resolveAgain(active(_,Msg),active(Lc,Msg1),Term,_,_,Term) :-
  similarStrings(Msg,Msg1),
  reportError("cannot resolve %s because %s",[can(Term),ss(Msg)],Lc).
resolveAgain(_,active(Lc,Msg),Orig,_,Dict,R) :-
  overloadTerm(Orig,Dict,inactive,St,T0),!,
  resolveAgain(active(Lc,Msg),St,Orig,T0,Dict,R).

markActive(_,Lc,Msg,active(Lc,Msg)).

markResolved(inactive,resolved).
markResolved(St,St).

overloadTerm(void,_,St,St,void).
overloadTerm(v(Lc,Nm,Tp),_,St,St,v(Lc,Nm,Tp)).
overloadTerm(anon(Lc,Tp),_,St,St,anon(Lc,Tp)).
overloadTerm(intLit(Lc,Ix),_,St,St,intLit(Lc,Ix)).
overloadTerm(bigLit(Lc,Ix),_,St,St,bigLit(Lc,Ix)).
overloadTerm(floatLit(Lc,Dx),_,St,St,floatLit(Lc,Dx)).
overloadTerm(charLit(Lx,Sx),_,St,St,charLit(Lx,Sx)).
overloadTerm(stringLit(Lx,Sx),_,St,St,stringLit(Lx,Sx)).
overloadTerm(dot(Lc,Rc,Fld,Tp),Dict,St,Stx,Dot) :-
  overloadTerm(Rc,Dict,St,St0,RRc),
  resolveDot(Lc,RRc,Fld,Tp,Dict,St0,Stx,Dot).
overloadTerm(update(Lc,Rc,Fld,Vl),Dict,St,Stx,Update) :-
  overloadTerm(Rc,Dict,St,St0,Rx),
  overloadTerm(Vl,Dict,St0,St1,Vx),
  resolveUpdate(Lc,Rx,Fld,Vx,Dict,St1,Stx,Update).
overloadTerm(tdot(Lc,Rc,Fld,Tp),Dict,St,Stx,Dot) :-
  overloadTerm(Rc,Dict,St,St0,RRc),
  resolveTDot(Lc,RRc,Fld,Tp,Dict,St0,Stx,Dot).
overloadTerm(enm(Lc,Rf,Tp),_,St,St,enm(Lc,Rf,Tp)).
overloadTerm(tple(Lc,Args),Dict,St,Stx,tple(Lc,RArgs)) :-!,
  overloadLst(Args,resolve:overloadTerm,Dict,St,Stx,RArgs).
overloadTerm(open(Lc,Er,Tp),Dict,St,Stx,open(Lc,Err,Tp)) :-
  overloadTerm(Er,Dict,St,Stx,Err).
overloadTerm(cell(Lc,Inn),Dict,St,Stx,cell(Lc,Inn1)) :-
  overloadTerm(Inn,Dict,St,Stx,Inn1).
overloadTerm(deref(Lc,Inn),Dict,St,Stx,deref(Lc,Inn1)) :-
  overloadTerm(Inn,Dict,St,Stx,Inn1).
overloadTerm(letExp(Lc,Decls,Defs,Bound),Dict,St,Stx,letExp(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:overloadTerm,Dict,St,Stx,RDefs,RBound).
overloadTerm(letRec(Lc,Decls,Defs,Bound),Dict,St,Stx,letRec(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:overloadTerm,Dict,St,Stx,RDefs,RBound).
overloadTerm(where(Lc,Trm,Cond),Dict,St,Stx,where(Lc,RTrm,RCond)) :-
  overloadTerm(Trm,Dict,St,St0,RTrm),
  overloadTerm(Cond,Dict,St0,Stx,RCond).
overloadTerm(conj(Lc,L,R),Dict,St,Stx,conj(Lc,RL,RR)) :-
  overloadTerm(L,Dict,St,St0,RL),
  overloadTerm(R,Dict,St0,Stx,RR).
overloadTerm(disj(Lc,L,R),Dict,St,Stx,disj(Lc,RL,RR)) :-
  overloadTerm(L,Dict,St,St0,RL),
  overloadTerm(R,Dict,St0,Stx,RR).
overloadTerm(cond(Lc,T,L,R,Tp),Dict,St,Stx,cond(Lc,RT,RL,RR,Tp)) :-
  overloadTerm(T,Dict,St,St0,RT),
  overloadTerm(L,Dict,St0,St1,RL),
  overloadTerm(R,Dict,St1,Stx,RR).
overloadTerm(implies(Lc,G,T),Dict,St,Stx,implies(Lc,RG,RT)) :-
  overloadTerm(G,Dict,St,St0,RG),
  overloadTerm(T,Dict,St0,Stx,RT).
overloadTerm(neg(Lc,T),Dict,St,Stx,neg(Lc,RT)) :-
  overloadTerm(T,Dict,St,Stx,RT).
overloadTerm(match(Lc,L,R),Dict,St,Stx,match(Lc,RL,RR)) :-
  overloadTerm(L,Dict,St,St0,RL),
  overloadTerm(R,Dict,St0,Stx,RR).
overloadTerm(case(Lc,B,C,Tp),Dict,St,Stx,case(Lc,RB,RC,Tp)) :-
  overloadTerm(B,Dict,St,St0,RB),
  overloadCases(C,resolve:overloadTerm,Dict,St0,Stx,RC).
overloadTerm(apply(ALc,over(Lc,T,Cx),Args,Tp),Dict,St,Stx,Term) :-
  overloadMethod(ALc,Lc,T,Cx,Args,Tp,Dict,St,Stx,Term).
overloadTerm(apply(ALc,overaccess(Lc,T,RcTp,Fld,FTp),Args,ATp),Dict,St,Stx,Term) :-
  overloadAccess(ALc,Lc,T,RcTp,Fld,FTp,Args,ATp,Dict,St,Stx,Term).
overloadTerm(apply(Lc,Op,Args,Tp),Dict,St,Stx,apply(Lc,ROp,RArgs,Tp)) :-
  overloadTerm(Op,Dict,St,St0,ROp),
  overloadTerm(Args,Dict,St0,Stx,RArgs).
overloadTerm(capply(Lc,Op,Args,Tp),Dict,St,Stx,capply(Lc,ROp,RArgs,Tp)) :-
  overloadTerm(Op,Dict,St,St0,ROp),
  overloadTerm(Args,Dict,St0,Stx,RArgs).
overloadTerm(over(Lc,raise(RLc,void,ErExp,ETp),[Cx]),Dict,St,Stx,Over) :-
  ( resolveConstraint(Lc,Cx,Dict,St,St0,Trw) ->
    overloadTerm(Trw,Dict,St0,St1,RTrw),
    overloadTerm(ErExp,Dict,St1,Stx,RExp),
    Over = raise(RLc,RTrw,RExp,ETp);
    genMsg("cannot find exception scope for %s",[Cx],Msg),
    markActive(St,Lc,Msg,Stx),
    Over = over(Lc,raise(RLc,void,ErExp,ETp),[Cx])).
overloadTerm(over(Lc,T,Cx),Dict,St,Stx,Over) :-
  ( resolveConstraints(Lc,Cx,Dict,St,St0,DTerms) ->
    resolveRef(T,DTerms,[],OverOp,Dict,St0,St1,NArgs),
    typeOfCanon(T,TTp),
    overApply(Lc,OverOp,NArgs,TTp,Over),
    markResolved(St1,Stx);
    genMsg("cannot find implementation for contracts %s",[Cx],Msg),
    markActive(St,Lc,Msg,Stx),
    Over = over(Lc,T,Cx)).
overloadTerm(overaccess(Lc,T,RcTp,Fld,FTp),Dict,St,Stx,Over) :-
  resolveAccess(Lc,RcTp,Fld,FTp,Dict,St,St1,AccessOp),
  resolveRef(T,[AccessOp],[],OverOp,Dict,St1,St2,NArgs),
  curryOver(Lc,OverOp,NArgs,funType(tplType([RcTp]),FTp),Over),
  markResolved(St2,Stx);
  genMsg("cannot find accessor for %s of type %s",[ss(Fld),tpe(RcTp)],Msg),
  markActive(St,Lc,Msg,Stx),
  Over = overaccess(Lc,T,RcTp,Fld,FTp).
overloadTerm(mtd(Lc,Nm,Tp),_,St,Stx,mtd(Lc,Nm,Tp)) :-
  genMsg("cannot find implementation for %s",[Nm],Msg),
  markActive(St,Lc,Msg,Stx).
overloadTerm(lambda(Lc,Lbl,Cx,Eqn,Tp),Dict,St,Stx,Lam) :-!,
  overloadLambda(Lc,Lbl,Cx,Eqn,Tp,Dict,St,Stx,Lam).
overloadTerm(valof(Lc,A,Tp),Dict,St,Stx,valof(Lc,AA,Tp)) :-!,
  overloadAction(A,Dict,St,Stx,AA).
overloadTerm(task(Lc,A,Tp),Dict,St,Stx,task(Lc,AA,Tp)) :-!,
  overloadTerm(A,Dict,St,Stx,AA).
overloadTerm(suspend(Lc,T,M,Tp),Dict,St,Stx,suspend(Lc,TT,MM,Tp)) :-!,
  overloadTerm(T,Dict,St,St0,TT),
  overloadTerm(M,Dict,St0,Stx,MM).
overloadTerm(retire(Lc,T,M,Tp),Dict,St,Stx,retire(Lc,TT,MM,Tp)) :-!,
  overloadTerm(T,Dict,St,St0,TT),
  overloadTerm(M,Dict,St0,Stx,MM).
overloadTerm(resume(Lc,T,M,Tp),Dict,St,Stx,resume(Lc,TT,MM,Tp)) :-!,
  overloadTerm(T,Dict,St,St0,TT),
  overloadTerm(M,Dict,St0,Stx,MM).
overloadTerm(thnkRef(Lc,A,Tp),Dict,St,Stx,thnkRef(Lc,AA,Tp)) :-!,
  overloadTerm(A,Dict,St,Stx,AA).
overloadTerm(newSV(Lc,Tp),_Dict,Stx,Stx,newSV(Lc,Tp)) :-!.
overloadTerm(svGet(Lc,A,T),Dict,St,Stx,svGet(Lc,AA,T)) :-!,
  overloadTerm(A,Dict,St,Stx,AA).
overloadTerm(svSet(Lc,Th,Vl),Dict,St,Stx,svSet(Lc,TT,VV)) :-!,
  overloadTerm(Th,Dict,St,St0,TT),
  overloadTerm(Vl,Dict,St0,Stx,VV).
overloadTerm(tryCatch(Lc,E,V,H),Dict,St,Stx,tryCatch(Lc,EE,V,HH)) :-
  overloadTryCatch(E,V,H,EE,HH,Dict,St,Stx,resolve:overloadTerm).
overloadTerm(raise(Lc,T,E,Tp),Dict,St,Stx,raise(Lc,TT,EE,Tp)) :-
  overloadTerm(T,Dict,St,St0,TT),
  overloadTerm(E,Dict,St0,Stx,EE).
overloadTerm(T,_,St,St,T) :-
  locOfCanon(T,Lc),
  reportError("invalid term to resolve %s",[can(T)],Lc).

overloadGuard(none,_,Stx,Stx,none) :-!.
overloadGuard(some(G),Dict,St,Stx,some(RG)) :-
  overloadTerm(G,Dict,St,Stx,RG).

overloadTryCatch(E,v(Lc,ErNm,ErTp),H,EE,HH,Dict,St,Stx,Over) :-
  declareTryScope(Lc,ErTp,ErNm,Dict,Dict2),
%  reportMsg("resolving try %s",[can(E)]),
  call(Over,E,Dict2,St,St0,EE),
%  reportMsg("try %s resolved to %s",[can(E),can(EE)]),
  overloadCases(H,Over,Dict,St0,Stx,HH).

overloadLambda(Lc,Lbl,[],rule(Lc,Args,G,Exp),Tp,Dict,St,Stx,
	       lambda(Lc,Lbl,[],rule(Lc,RArgs,RG,RExp),Tp)) :-!,
  defineArgVars(Args,Dict,RDict),
  overloadTerm(Args,RDict,St,St0,RArgs),
  overloadGuard(G,RDict,St0,St1,RG),
  overloadTerm(Exp,RDict,St1,Stx,RExp).
overloadLambda(Lc,Lbl,Cx,Eqn,Tp,Dict,Stx,Stx,lambda(Lc,Lbl,[],OEqn,Tp)) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  overloadEquation(CVars,Eqn,FDict,OEqn).
					      
overloadLet(Lc,Decls,Defs,Bound,RR,Dict,St,Stx,RDefs,RBound) :-
  declareAllDecls(Decls,Lc,Dict,RDict),
  call(RR,Bound,RDict,St,Stx,RBound),
  overload(Defs,RDict,RDefs).

overloadAction(doNop(Lc),_,St,St,doNop(Lc)) :-!.
overloadAction(doSeq(Lc,A,B),Dict,St,Stx,doSeq(Lc,AA,BB)) :-
  overloadAction(A,Dict,St,St1,AA),
  overloadAction(B,Dict,St1,Stx,BB).
overloadAction(doLbld(Lc,Lb,A),Dict,St,Stx,doLbld(Lc,Lb,AA)) :-!,
  overloadAction(A,Dict,St,Stx,AA).
overloadAction(doBrk(Lc,Lb),_,St,St,doBrk(Lc,Lb)) :-!.
overloadAction(doValis(Lc,A),Dict,St,Stx,doValis(Lc,AA)) :-
  overloadTerm(A,Dict,St,Stx,AA).
overloadAction(doDefn(Lc,V,E),Dict,St,Stx,doDefn(Lc,V,EE)) :-
  overloadTerm(E,Dict,St,Stx,EE).
overloadAction(doMatch(Lc,P,A),Dict,St,Stx,doMatch(Lc,PP,AA)) :-
  overloadTerm(P,Dict,St,St1,PP),
  overloadTerm(A,Dict,St1,Stx,AA).
overloadAction(doAssign(Lc,P,A),Dict,St,Stx,doAssign(Lc,PP,AA)) :-
  overloadTerm(P,Dict,St,St1,PP),
  overloadTerm(A,Dict,St1,Stx,AA).
overloadAction(doTryCatch(Lc,A,V,H),Dict,St,Stx,doTryCatch(Lc,AA,V,HH)) :-
  overloadTryCatch(A,V,H,AA,HH,Dict,St,Stx,resolve:overloadAction).
overloadAction(doIfThenElse(Lc,T,A,B),Dict,St,Stx,doIfThenElse(Lc,TT,AA,BB)) :-
  overloadTerm(T,Dict,St,St1,TT),
  overloadAction(A,Dict,St1,St2,AA),
  overloadAction(B,Dict,St2,Stx,BB).
overloadAction(doCase(Lc,B,C,Tp),Dict,St,Stx,doCase(Lc,RB,RC,Tp)) :-
  overloadTerm(B,Dict,St,St0,RB),
  overloadCases(C,resolve:overloadAction,Dict,St0,Stx,RC).
overloadAction(doWhile(Lc,T,A),Dict,St,Stx,doWhile(Lc,TT,AA)) :-
  overloadTerm(T,Dict,St,St1,TT),
  overloadAction(A,Dict,St1,Stx,AA).
overloadAction(doLet(Lc,Decls,Defs,Bound),Dict,St,Stx,doLet(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:overloadAction,Dict,St,Stx,RDefs,RBound).
overloadAction(doLetRec(Lc,Decls,Defs,Bound),Dict,St,Stx,doLetRec(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:overloadAction,Dict,St,Stx,RDefs,RBound).
overloadAction(case(Lc,G,C,Tp),Dict,St,Stx,case(Lc,GG,CC,Tp)) :-
  overloadTerm(G,Dict,St,St1,GG),
  overloadCases(C,resolve:overloadAction,Dict,St1,Stx,CC).
overloadAction(doExp(Lc,T),Dict,St,Stx,doExp(Lc,TT)) :-
  overloadTerm(T,Dict,St,Stx,TT).
overloadAction(A,_,St,St,A) :-
  locOfCanon(A,Lc),
  reportError("cannot resolve action %s",[cnact(A)],Lc).

overloadMethod(ALc,Lc,T,Cx,Args,Tp,Dict,St,Stx,apply(ALc,OverOp,tple(LcA,NArgs),Tp)) :-
  resolveConstraints(Lc,Cx,Dict,St,St0,DTerms),
  markResolved(St0,St1),
  overloadTerm(Args,Dict,St1,St2,tple(LcA,RArgs)),
  resolveRef(T,DTerms,RArgs,OverOp,Dict,St2,Stx,NArgs).

overloadAccess(ALc,Lc,T,RcTp,Fld,Tp,Args,ATp,Dict,St,Stx,
	       apply(ALc,Op,tple(LcA,NArgs),ATp)) :-
  resolveAccess(Lc,RcTp,Fld,Tp,Dict,St,St1,AccessOp),
  overloadTerm(Args,Dict,St1,St2,tple(LcA,RArgs)),
  resolveRef(T,[AccessOp],RArgs,Op,Dict,St2,Stx,NArgs).
  
overloadCases(Cses,Resolver,Dict,St,Stx,RCases) :-
  overloadLst(Cses,resolve:overloadRule(Resolver),Dict,St,Stx,RCases).

overApply(_,OverOp,[],_,OverOp) :-!.
overApply(Lc,OverOp,Args,Tp,apply(Lc,OverOp,tple(Lc,Args),Tp)) :- \+isProgramType(Tp),!.
overApply(Lc,OverOp,Args,Tp,Lam) :-
  curryOver(Lc,OverOp,Args,Tp,Lam).

curryOver(Lc,OverOp,Cx,Tp,
    lambda(Lc,Lbl,[],rule(Lc,tple(Lc,Args),none,
          apply(Lc,OverOp,tple(Lc,NArgs),Tp)),funType(tplType(ArTps),Tp))) :-
  progArgTypes(Tp,ArTps),
  genVrs(ArTps,Lc,Args),
  concat(Cx,Args,NArgs),
  lcPk(Lc,Path),
  lambdaLbl(Path,"curry",Lbl).

genVrs([],_,[]).
genVrs([Tp|ArTps],Lc,[v(Lc,Id,Tp)|Vrs]) :-
  genstr("V",Id),
  genVrs(ArTps,Lc,Vrs).

overloadLst([],_,_,St,St,[]):-!.
overloadLst([T|L],C,D,St,Stx,[RT|RL]) :-
  call(C,T,D,St,St0,RT),
  overloadLst(L,C,D,St0,Stx,RL).

overloadList([],_,_,[]):-!.
overloadList([T|L],C,D,[RT|RL]) :-
  call(C,T,D,RT),
  overloadList(L,C,D,RL).

resolveRef(mtd(Lc,Nm,Tp),[DT|Ds],RArgs,MtdCall,Dict,St,Stx,Args) :-
  concat(Ds,RArgs,Args),
  resolveDot(Lc,DT,Nm,Tp,Dict,St,Stx,MtdCall),!.
resolveRef(v(Lc,Nm,Tp),DT,RArgs,v(Lc,Nm,Tp),_,Stx,Stx,Args) :- !,
  concat(DT,RArgs,Args).
resolveRef(C,DT,RArgs,C,_,Stx,Stx,Args) :-
  concat(DT,RArgs,Args).

resolveDot(Lc,Rc,Fld,Tp,Dict,St,Stx,Reslvd) :-
  typeOfCanon(Rc,RcTp),
  findAccess(RcTp,Fld,Dict,AccTp,FunNm),
  freshen(AccTp,Dict,_,FAccTp),
  newTypeVar("FF",RTp),
  (sameType(funType(tplType([RcTp]),RTp),FAccTp,Lc,Dict),
   freshen(RTp,Dict,_,CFTp),
   getConstraints(CFTp,Cx,FTp), % constraints here are dropped!
   sameType(FTp,Tp,Lc,Dict),
   V = v(Lc,FunNm,funType(tplType([RcTp]),FTp)),
%   Reslvd = apply(Lc,V,tple(Lc,[Rc]),Tp),
   manageConstraints(Cx,Lc,apply(Lc,V,tple(Lc,[Rc]),Tp),Reslvd),
%   reportMsg("dot resolved %s",[can(Reslvd)]),
   markResolved(St,Stx);
   genMsg("accessor defined for %s:%s in %s\nnot consistent with\n%s",
	  [Fld,tpe(FAccTp),can(dot(Lc,Rc,Fld,Tp)),tpe(Tp)],Msg),
   markActive(St,Lc,Msg,Stx),
   Reslvd = dot(Lc,Rc,Fld,Tp)).
resolveDot(Lc,Rc,Fld,Tp,_Dict,St,Stx,dot(Lc,Rc,Fld,Tp)) :-
  typeOfCanon(Rc,RcTp),
  genMsg("no accessor defined for %s for type %s in %s",
	 [Fld,tpe(RcTp),can(dot(Lc,Rc,Fld,Tp))],Msg),
  markActive(St,Lc,Msg,Stx).


resolveTDot(Lc,Rc,Ix,Tp,Dict,St,Stx,tdot(Lc,Rc,Ix,Tp)) :-
  typeOfCanon(Rc,RcTp),
  deRef(RcTp,tplType(Els)),!,
  (length(Els,Ar), Ar>Ix ->
   nth_of(Els,Ix,ElTp),
   (sameType(ElTp,Tp,Lc,Dict) -> St=Stx ;
    genMsg("%sth type of %s:%s not consistent with expected type %s",
	   [ix(Ix),tpe(ElTp),tpe(Tp)],Msg),
    markActive(St,Lc,Msg,Stx));
   genMsg("%type of %s:%s not a tuple of at least %d",
	   [can(Rc),tpe(RcTp),ix(Ix)],Msg),
   markActive(St,Lc,Msg,Stx)).
resolveTDot(Lc,Rc,Ix,Tp,_Dict,St,Stx,tdot(Lc,Rc,Ix,Tp)) :-
  typeOfCanon(Rc,RcTp),
  genMsg("%type of %s:%s not a tuple of at least %d",
	 [can(Rc),tpe(RcTp),ix(Ix)],Msg),
  markActive(St,Lc,Msg,Stx).

resolveAccess(Lc,RcTp,Fld,Tp,Dict,St,Stx,Reslvd) :-
  findAccess(RcTp,Fld,Dict,AccTp,FunNm),
  freshen(AccTp,Dict,_,FAccTp),
  newTypeVar("FF",RTp),
  (sameType(funType(tplType([RcTp]),RTp),FAccTp,Lc,Dict),
   freshen(RTp,Dict,_,CFTp),
   getConstraints(CFTp,Cx,FTp),
   sameType(FTp,Tp,Lc,Dict),
   V = v(Lc,FunNm,funType(tplType([RcTp]),FTp)),
   manageConstraints(Cx,Lc,V,Reslvd),
   markResolved(St,Stx);
   genMsg("no accessor for %s defined for type %s",[ss(Fld),tpe(RcTp)],Msg),
   Reslvd=void,
   markActive(St,Lc,Msg,Stx)).
   
resolveUpdate(Lc,Rc,Fld,Vl,Dict,St,Stx,Reslvd) :-
  typeOfCanon(Rc,RcTp),
  typeOfCanon(Vl,VlTp),
  findUpdate(RcTp,Fld,Dict,AccTp,FunNm),
  freshen(AccTp,Dict,_,FAccTp),
  newTypeVar("FF",VTp),
  (sameType(funType(tplType([RcTp,VTp]),RcTp),FAccTp,Lc,Dict),
   freshen(VTp,Dict,_,CVTp),
   getConstraints(CVTp,Cx,FTp),
   sameType(FTp,VlTp,Lc,Dict),
   V = v(Lc,FunNm,funType(tplType([RcTp,VlTp]),RcTp)),
   Acc = apply(Lc,V,tple(Lc,[Rc,Vl]),RcTp),
   resolveConstraints(Lc,Cx,Dict,St,St0,DTerms),
   resolveRef(Acc,DTerms,[],OverOp,Dict,St0,St1,NArgs),
   overApply(Lc,OverOp,NArgs,RcTp,Reslvd),
   markResolved(St1,Stx);
   genMsg("updater defined for %s:%s in %s\nnot consistent with\n%s",
	  [Fld,tpe(FAccTp),can(update(Lc,Rc,Fld,Vl)),tpe(RcTp)],Msg),
   markActive(St,Lc,Msg,Stx),
   Reslvd = update(Lc,Rc,Fld,Vl)).
resolveUpdate(Lc,Rc,Fld,Vl,_Dict,St,Stx,update(Lc,Rc,Fld,Vl)) :-
  typeOfCanon(Rc,RcTp),
  genMsg("no updater defined for %s for type %s in %s",
	 [Fld,tpe(RcTp),can(upate(Lc,Rc,Fld,Vl))],Msg),
  markActive(St,Lc,Msg,Stx).

resolveConstraints(_,[],_,St,St,[]).
resolveConstraints(Lc,[Con|C],Dict,St,Stx,[CV|Vs]) :-
  resolveConstraint(Lc,Con,Dict,St,St0,CV),!,
  resolveConstraints(Lc,C,Dict,St0,Stx,Vs).

resolveConstraint(Lc,implicit(Nm,Tp),Dict,St,Stx,Over) :-
  (getVar(Lc,Nm,Dict,Over,ITp),
   (sameType(ITp,Tp,Lc,Dict),
    markResolved(St,Stx);
    genMsg("implicit %s:%s not consistent with %s",[Nm,tpe(ITp),tpe(Tp)],Msg),
    markActive(St,Lc,Msg,Stx),
    Over=void) ;
   genMsg("implicit %s:%s not defined",[Nm,tpe(Tp)],Msg),
   markActive(St,Lc,Msg,Stx),
   Over=void).
resolveConstraint(Lc,raises(Tp),Dict,St,Stx,Over) :-
  resolveRaises(Lc,Tp,Dict,St,Stx,Over).
resolveConstraint(Lc,C,Dict,St,Stx,Over) :-
  implementationName(C,ImpNm),
  getImplementation(Dict,ImpNm,ImplVrNm,_ImplTp),
  getVar(Lc,ImplVrNm,Dict,Impl,ITp),
  contractType(C,CTp),
  sameType(ITp,CTp,Lc,Dict),
  markResolved(St,St1),
  overloadTerm(Impl,Dict,St1,Stx,Over).

resolveRaises(Lc,Tp,Dict,St,Stx,Over) :-
  tpName(Tp,TpNm),
  getTryScope(TpNm,Dict,_TLc,VrNm,ErTp),!,
  resolveRaises(Lc,Tp,VrNm,ErTp,Dict,St,Stx,Over).
resolveRaises(Lc,Tp,Dict,St,Stx,Over) :-
  isUnbound(Tp),
  topTryScope(Dict,_,VrNm,ErTp),!,
  resolveRaises(Lc,Tp,VrNm,ErTp,Dict,St,Stx,Over).
resolveRaises(Lc,Tp,_Dict,St,Stx,void) :-
  genMsg("exception context for %s not defined",[tpe(Tp)],Msg),
  markActive(St,Lc,Msg,Stx).

resolveRaises(Lc,Tp,VrNm,ErTp,Dict,St,Stx,Over) :-
  sameType(ErTp,Tp,Lc,Dict),
  markResolved(St,Stx),
  Over=v(Lc,VrNm,ErTp),!.
resolveRaises(Lc,Tp,_,ErTp,_Dict,St,Stx,void) :-
  genMsg("raises %s not consistent with %s",[tpe(ErTp),tpe(Tp)],Msg),
  markActive(St,Lc,Msg,Stx).

resolveImpl(v(Lc,Nm,Tp),_,_,_,_,St,St,v(Lc,Nm,Tp)) :-!.
resolveImpl(I,C,ImpNm,Lc,Dict,St,Stx,Over) :-
  freshen(I,[],_,Con),
  getConstraints(Con,Cx,CT),
  contractType(C,Tp),
  sameType(CT,Tp,Lc,[]),
  resolveDependents(Cx,Lc,Dict,St,St0,Args,[]),
  (St0\=active(_,_) ->
    formOver(v(Lc,ImpNm,Tp),Args,Lc,Tp,Over),
    markResolved(St0,Stx) ;
    Stx=St0, Over = I).
resolveImpl(T,C,_,Lc,_,St,Stx,T) :-
  genMsg("cannot resolve contract %s",[C],Msg),
  markActive(St,Lc,Msg,Stx).

resolveDependents([],_,_,St,St,Args,Args).
resolveDependents([C|L],Lc,Dict,St,Stx,[A|As],Args) :-
  resolveConstraint(Lc,C,Dict,St,St0,A),
  resolveDependents(L,Lc,Dict,St0,Stx,As,Args).

formOver(V,[],_,_,V).
formOver(V,Args,Lc,Tp,apply(Lc,V,tple(Lc,Args),Tp)).

genVar(Nm,Lc,Tp,v(Lc,NV,Tp)) :-
  genstr(Nm,NV).

findAccess(Tp,FldNm,Dict,AccTp,FunNm) :-
  getFieldAccess(Tp,FldNm,FunNm,AccTp,Dict).
findAccess(_Tp,FldNm,Dict,AccTp,FunNm) :-
  is_member(access(FldNm,v(_,FunNm,AccTp)),Dict),!.

findUpdate(Tp,FldNm,Dict,AccTp,FunNm) :-
  getFieldUpdater(Tp,FldNm,FunNm,AccTp,Dict).
findUpdate(_Tp,FldNm,Dict,AccTp,FunNm) :-
  is_member(update(FldNm,v(_,FunNm,AccTp)),Dict),!.

resolveHead(Hd,[],Hd).
resolveHead(enm(Lc,Nm,Tp),CVars,apply(Lc,v(Lc,Nm,Tp),CVars)).
resolveHead(apply(Lc,v(ALc,Nm,Tp),Args,Tpx),CVars,apply(Lc,v(ALc,Nm,Tp),OArgs,Tpx)) :-
  addExtra(CVars,Args,OArgs).

addExtra(Extra,tple(Lc,Els),tple(Lc,EEls)) :-
  concat(Extra,Els,EEls).

addExtraDefs([],Els,Els).
addExtraDefs([v(Lc,Nm,Tp)|Ex],Els,REls) :-
  addExtraDefs(Ex,[varDef(Lc,Nm,Nm,[],Tp,v(Lc,Nm,Tp))|Els],REls).
