:- module(resolve,[overload/3,overloadOthers/3]).

:- use_module(dict).
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
%  dispDef(funDef(Lc,Nm,ExtNm,H,Tp,Cx,Eqns)),
  overloadFunction(Lc,Nm,ExtNm,H,Tp,Cx,Eqns,Dict,RF).
overloadDef(Dict,varDef(Lc,Nm,ExtNm,Cx,Tp,Value),RD) :-!,
  overloadDefn(Lc,Nm,ExtNm,Cx,Tp,Value,Dict,RD).
overloadDef(_,Def,Def).

overloadFunction(Lc,Nm,ExtNm,H,Tp,[],Eqns,Dict,funDef(Lc,Nm,ExtNm,H,Tp,[],REqns)) :-
  overloadEquations(Eqns,Dict,[],REqns),!.
overloadFunction(Lc,Nm,ExtNm,H,Tp,Cx,Eqns,Dict,funDef(Lc,Nm,ExtNm,H,Tp,[],REqns)) :-
%  reportMsg("overloading %s",[canDef(funDef(Lc,Nm,ExtNm,H,Tp,[],Eqns))],Lc),
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  overloadEquations(Eqns,FDict,CVars,REqns),!.

overloadEquations(Eqns,Dict,Extra,REqns) :-
  overloadList(Eqns,overloadEquation(Extra),Dict,REqns).

overloadEquation(Extra,rule(Lc,Args,G,Exp),Dict,
		 rule(Lc,RArgs,RG,RExp)) :-
  overloadTerm(Args,Dict,RA),
  addExtra(Extra,RA,RArgs),
  resolveGuard(G,Dict,RG),
  overloadTerm(Exp,Dict,RExp).

resolveGuard(none,_,none) :-!.
resolveGuard(some(G),Dict,some(RG)) :-
  overloadTerm(G,Dict,RG).

% These are used when resolving lambdas only. A lambda cannot introduce any dictionary variables
overloadRule(Over,rule(Lc,Args,G,Exp),Dict,St,Stx,rule(Lc,RArgs,RG,RExp)) :-
  resolveTerm(Args,Dict,St,St0,RArgs),
  overloadGuard(G,Dict,St0,St1,RG),
  call(Over,Exp,Dict,St1,Stx,RExp).

overloadDefn(Lc,Nm,ExtNm,[],Tp,Exp,Dict,varDef(Lc,Nm,ExtNm,[],Tp,RExp)) :-
  overloadTerm(Exp,Dict,RExp).
overloadDefn(Lc,Nm,ExtNm,Cx,Tp,Exp,Dict,varDef(Lc,Nm,ExtNm,[],Tp,
    lambda(Lc,Lbl,rule(Lc,tple(Lc,CVars),none,RExp),OTp))) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  contractTypes(Cx,Tps),
  makeContractFunType(Tp,Tps,OTp),
  overloadTerm(Exp,FDict,RExp),
  lambdaLbl(Nm,"over",Lbl).

makeContractFunType(allType(V,T),Cx,allType(V,CT)) :-
  makeContractFunType(T,Cx,CT).
makeContractFunType(constrained(T,_C),Cx,CT) :-
  makeContractFunType(T,Cx,CT).
makeContractFunType(T,Cx,funType(tplType(Cx),T)).

defineCVars(_,[],Dict,[],Dict).
defineCVars(Lc,[Con|Cx],Dict,[v(Lc,CVarNm,ConTp)|CVars],FDict) :-
  implementationName(Con,ImplNm),
  mangleName("_",value,ImplNm,CVarNm),
  contractType(Con,ConTp),
  declareImplementation(ImplNm,CVarNm,ConTp,Dict,D0),
  declareVr(Lc,CVarNm,ConTp,none,D0,D1),
  defineCVars(Lc,Cx,D1,CVars,FDict).
defineCVars(Lc,[implementsFace(X,faceType(Flds,_))|Cx],Dict,CVars,FDict) :-
  fieldVars(Lc,X,Flds,CVars,CVrs,Dict,CDict),
  defineCVars(Lc,Cx,CDict,CVrs,FDict).

fieldVars(_,_,[],CVars,CVars,Dict,Dict).
fieldVars(Lc,Tp,[(FldNm,FldTp)|Flds],[NV|CVrs],XVrs,Dict,CDict) :-
  genVar(FldNm,Lc,funType(tplType([Tp]),FldTp),NV),
  fieldVars(Lc,Tp,Flds,CVrs,XVrs,[access(FldNm,NV)|Dict],CDict).

overloadTerm(Term,Dict,Resolved) :-
%  locOfCanon(Term,Lc),
%  reportMsg("resolving %s",[Term],Lc),
  resolveTerm(Term,Dict,inactive,St,RTerm),!,
%  reportMsg("%s resolved %s",[Term,St]),
  resolveAgain(inactive,St,Term,RTerm,Dict,Resolved).

% Somewhat complex logic to allow multiple iterations unless it will not help
resolveAgain(_,resolved,Term,T,Dict,R) :- !,
  resolveTerm(T,Dict,inactive,St,T0),
  resolveAgain(inactive,St,Term,T0,Dict,R).
resolveAgain(_,inactive,_,T,_,T) :- !.
resolveAgain(active(_,_),active(Lc,Msg),Term,_,_,Term) :-
  reportError("cannot resolve %s because %s",[can(Term),ss(Msg)],Lc).
resolveAgain(_,active(Lc,Msg),Orig,_,Dict,R) :-
  resolveTerm(Orig,Dict,inactive,St,T0),
  resolveAgain(active(Lc,Msg),St,Orig,T0,Dict,R).

markActive(_,Lc,Msg,active(Lc,Msg)).

markResolved(inactive,resolved).
markResolved(St,St).

resolveTerm(void,_,St,St,void).
resolveTerm(v(Lc,Nm,Tp),_,St,St,v(Lc,Nm,Tp)).
resolveTerm(anon(Lc,Tp),_,St,St,anon(Lc,Tp)).
resolveTerm(intLit(Lc,Ix),_,St,St,intLit(Lc,Ix)).
resolveTerm(bigLit(Lc,Ix),_,St,St,bigLit(Lc,Ix)).
resolveTerm(floatLit(Lc,Dx),_,St,St,floatLit(Lc,Dx)).
resolveTerm(charLit(Lx,Sx),_,St,St,charLit(Lx,Sx)).
resolveTerm(stringLit(Lx,Sx),_,St,St,stringLit(Lx,Sx)).
resolveTerm(dot(Lc,Rc,Fld,Tp),Dict,St,Stx,Dot) :-
  resolveTerm(Rc,Dict,St,St0,RRc),
  resolveDot(Lc,RRc,Fld,Tp,Dict,St0,Stx,Dot).
resolveTerm(update(Lc,Rc,Fld,Vl),Dict,St,Stx,Update) :-
  resolveTerm(Rc,Dict,St,St0,Rx),
  resolveTerm(Vl,Dict,St0,St1,Vx),
  resolveUpdate(Lc,Rx,Fld,Vx,Dict,St1,Stx,Update).
resolveTerm(enm(Lc,Rf,Tp),_,St,St,enm(Lc,Rf,Tp)).
resolveTerm(cons(Lc,Rf,Tp),_,St,St,cons(Lc,Rf,Tp)).
resolveTerm(tple(Lc,Args),Dict,St,Stx,tple(Lc,RArgs)) :-!,
  overloadLst(Args,resolve:resolveTerm,Dict,St,Stx,RArgs).
resolveTerm(open(Lc,Er,Tp),Dict,St,Stx,open(Lc,Err,Tp)) :-
  resolveTerm(Er,Dict,St,Stx,Err).
resolveTerm(cell(Lc,Inn),Dict,St,Stx,cell(Lc,Inn1)) :-
  resolveTerm(Inn,Dict,St,Stx,Inn1).
resolveTerm(deref(Lc,Inn),Dict,St,Stx,deref(Lc,Inn1)) :-
  resolveTerm(Inn,Dict,St,Stx,Inn1).
resolveTerm(letExp(Lc,Decls,Defs,Bound),Dict,St,Stx,letExp(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:resolveTerm,Dict,St,Stx,RDefs,RBound).
resolveTerm(letRec(Lc,Decls,Defs,Bound),Dict,St,Stx,letRec(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:resolveTerm,Dict,St,Stx,RDefs,RBound).
resolveTerm(where(Lc,Trm,Cond),Dict,St,Stx,where(Lc,RTrm,RCond)) :-
  resolveTerm(Trm,Dict,St,St0,RTrm),
  resolveTerm(Cond,Dict,St0,Stx,RCond).
resolveTerm(conj(Lc,L,R),Dict,St,Stx,conj(Lc,RL,RR)) :-
  resolveTerm(L,Dict,St,St0,RL),
  resolveTerm(R,Dict,St0,Stx,RR).
resolveTerm(disj(Lc,L,R),Dict,St,Stx,disj(Lc,RL,RR)) :-
  resolveTerm(L,Dict,St,St0,RL),
  resolveTerm(R,Dict,St0,Stx,RR).
resolveTerm(cond(Lc,T,L,R,Tp),Dict,St,Stx,cond(Lc,RT,RL,RR,Tp)) :-
  resolveTerm(T,Dict,St,St0,RT),
  resolveTerm(L,Dict,St0,St1,RL),
  resolveTerm(R,Dict,St1,Stx,RR).
resolveTerm(implies(Lc,G,T),Dict,St,Stx,implies(Lc,RG,RT)) :-
  resolveTerm(G,Dict,St,St0,RG),
  resolveTerm(T,Dict,St0,Stx,RT).
resolveTerm(neg(Lc,T),Dict,St,Stx,neg(Lc,RT)) :-
  resolveTerm(T,Dict,St,Stx,RT).
resolveTerm(match(Lc,L,R),Dict,St,Stx,match(Lc,RL,RR)) :-
  resolveTerm(L,Dict,St,St0,RL),
  resolveTerm(R,Dict,St0,Stx,RR).
resolveTerm(case(Lc,B,C,Tp),Dict,St,Stx,case(Lc,RB,RC,Tp)) :-
  resolveTerm(B,Dict,St,St0,RB),
  overloadCases(C,resolve:resolveTerm,Dict,St0,Stx,RC).
resolveTerm(apply(ALc,over(Lc,T,Cx),Args,Tp,ErTp),Dict,St,Stx,Term) :-
  overloadMethod(ALc,Lc,T,Cx,Args,Tp,ErTp,Dict,St,Stx,Term).
resolveTerm(apply(ALc,overaccess(Lc,T,RcTp,Fld,FTp),Args,ATp,ErTp),Dict,St,Stx,Term) :-
  overloadAccess(ALc,Lc,T,RcTp,Fld,FTp,Args,ATp,ErTp,Dict,St,Stx,Term).
resolveTerm(apply(Lc,Op,Args,Tp,ErTp),Dict,St,Stx,apply(Lc,ROp,RArgs,Tp,ErTp)) :-
  resolveTerm(Op,Dict,St,St0,ROp),
  resolveTerm(Args,Dict,St0,Stx,RArgs).
resolveTerm(over(Lc,T,Cx),Dict,St,Stx,Over) :-
  ( resolveContracts(Lc,Cx,Dict,St,St0,DTerms) ->
    resolveRef(T,DTerms,[],OverOp,Dict,St0,St1,NArgs),
    typeOfCanon(T,TTp),
    overApply(Lc,OverOp,NArgs,TTp,Over),
    markResolved(St1,Stx);
    genMsg("cannot find implementation for contracts %s",[Cx],Msg),
    markActive(St,Lc,Msg,Stx),
    Over = over(Lc,T,Cx)).
resolveTerm(overaccess(Lc,T,RcTp,Fld,FTp),Dict,St,Stx,Over) :-
  resolveAccess(Lc,RcTp,Fld,FTp,Dict,St,St1,AccessOp),
  resolveRef(T,[AccessOp],[],OverOp,Dict,St1,St2,NArgs),
  curryOver(Lc,OverOp,NArgs,funType(tplType([RcTp]),FTp),Over),
  markResolved(St2,Stx);
  genMsg("cannot find accessor for %s of type %s",[ss(Fld),tpe(RcTp)],Msg),
  markActive(St,Lc,Msg,Stx),
  Over = overaccess(Lc,T,RcTp,Fld,FTp).
resolveTerm(mtd(Lc,Nm,Tp),_,St,Stx,mtd(Lc,Nm,Tp)) :-
  genMsg("cannot find implementation for %s",[Nm],Msg),
  markActive(St,Lc,Msg,Stx).

resolveTerm(lambda(Lc,Lbl,Eqn,Tp),Dict,St,Stx,lambda(Lc,Lbl,OEqn,Tp)) :-
  overloadRule(resolve:resolveTerm,Eqn,Dict,St,Stx,OEqn).
resolveTerm(valof(Lc,A,Tp),Dict,St,Stx,valof(Lc,AA,Tp)) :-!,
  overloadAction(A,Dict,St,Stx,AA).
resolveTerm(task(Lc,A,Tp),Dict,St,Stx,task(Lc,AA,Tp)) :-!,
  resolveTerm(A,Dict,St,Stx,AA).
resolveTerm(tryCatch(Lc,E,H),Dict,St,Stx,tryCatch(Lc,EE,HH)) :-
  resolveTerm(E,Dict,St,St0,EE),
  overloadCases(H,resolve:resolveTerm,Dict,St0,Stx,HH).
resolveTerm(throw(Lc,E),Dict,St,Stx,throw(Lc,EE)) :-
  resolveTerm(E,Dict,St,Stx,EE).
resolveTerm(T,_,St,St,T) :-
  locOfCanon(T,Lc),
  reportError("invalid term to resolve %s",[can(T)],Lc).

overloadGuard(none,_,Stx,Stx,none) :-!.
overloadGuard(some(G),Dict,St,Stx,some(RG)) :-
  resolveTerm(G,Dict,St,Stx,RG).

overloadLet(Lc,Decls,Defs,Bound,RR,Dict,St,Stx,RDefs,RBound) :-
  declareAccessors(Lc,Decls,Dict,RDict),
  overload(Defs,RDict,RDefs),
  call(RR,Bound,RDict,St,Stx,RBound).

overloadAction(doNop(Lc),_,St,St,doNop(Lc)) :-!.
overloadAction(doSeq(Lc,A,B),Dict,St,Stx,doSeq(Lc,AA,BB)) :-
  overloadAction(A,Dict,St,St1,AA),
  overloadAction(B,Dict,St1,Stx,BB).
overloadAction(doLbld(Lc,Lb,A),Dict,St,Stx,doLbld(Lc,Lb,AA)) :-!,
  overloadAction(A,Dict,St,Stx,AA).
overloadAction(doBrk(Lc,Lb),_,St,St,doBrk(Lc,Lb)) :-!.
overloadAction(doValis(Lc,A),Dict,St,Stx,doValis(Lc,AA)) :-
  resolveTerm(A,Dict,St,Stx,AA).
overloadAction(doThrow(Lc,A),Dict,St,Stx,doThrow(Lc,AA)) :-
  resolveTerm(A,Dict,St,Stx,AA).
overloadAction(doDefn(Lc,V,E),Dict,St,Stx,doDefn(Lc,V,EE)) :-
  resolveTerm(E,Dict,St,Stx,EE).
overloadAction(doMatch(Lc,P,A),Dict,St,Stx,doMatch(Lc,PP,AA)) :-
  resolveTerm(P,Dict,St,St1,PP),
  resolveTerm(A,Dict,St1,Stx,AA).
overloadAction(doAssign(Lc,P,A),Dict,St,Stx,doAssign(Lc,PP,AA)) :-
  resolveTerm(P,Dict,St,St1,PP),
  resolveTerm(A,Dict,St1,Stx,AA).
overloadAction(doTryCatch(Lc,A,H),Dict,St,Stx,doTryCatch(Lc,AA,HH)) :-
  overloadAction(A,Dict,St,St1,AA),
  overloadCases(H,resolve:overloadAction,Dict,St1,Stx,HH).
overloadAction(doIfThenElse(Lc,T,A,B),Dict,St,Stx,doIfThenElse(Lc,TT,AA,BB)) :-
  resolveTerm(T,Dict,St,St1,TT),
  overloadAction(A,Dict,St1,St2,AA),
  overloadAction(B,Dict,St2,Stx,BB).
overloadAction(doCase(Lc,B,C,Tp),Dict,St,Stx,doCase(Lc,RB,RC,Tp)) :-
  resolveTerm(B,Dict,St,St0,RB),
  overloadCases(C,resolve:overloadAction,Dict,St0,Stx,RC).
overloadAction(doWhile(Lc,T,A),Dict,St,Stx,doWhile(Lc,TT,AA)) :-
  resolveTerm(T,Dict,St,St1,TT),
  overloadAction(A,Dict,St1,Stx,AA).
overloadAction(doLet(Lc,Decls,Defs,Bound),Dict,St,Stx,doLet(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:overloadAction,Dict,St,Stx,RDefs,RBound).
overloadAction(doLetRec(Lc,Decls,Defs,Bound),Dict,St,Stx,doLetRec(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:overloadAction,Dict,St,Stx,RDefs,RBound).
overloadAction(case(Lc,G,C,Tp),Dict,St,Stx,case(Lc,GG,CC,Tp)) :-
  resolveTerm(G,Dict,St,St1,GG),
  overloadCases(C,resolve:overloadAction,Dict,St1,Stx,CC).
overloadAction(doSuspend(Lc,T,E,C),Dict,St,Stx,doSuspend(Lc,TT,EE,CC)) :-
  resolveTerm(T,Dict,St,St1,TT),
  resolveTerm(E,Dict,St1,St2,EE),
  overloadCases(C,resolve:overloadAction,Dict,St2,Stx,CC).
overloadAction(doResume(Lc,T,E,C),Dict,St,Stx,doResume(Lc,TT,EE,CC)) :-
  resolveTerm(T,Dict,St,St1,TT),
  resolveTerm(E,Dict,St1,St2,EE),
  overloadCases(C,resolve:overloadAction,Dict,St2,Stx,CC).
overloadAction(doRetire(Lc,T,E),Dict,St,Stx,doRetire(Lc,TT,EE)) :-
  resolveTerm(T,Dict,St,St1,TT),
  resolveTerm(E,Dict,St1,Stx,EE).
overloadAction(doCall(Lc,T,ErTp),Dict,St,Stx,doCall(Lc,TT,ErTp)) :-
  resolveTerm(T,Dict,St,Stx,TT).
overloadAction(A,_,St,St,A) :-
  locOfCanon(A,Lc),
  reportError("cannot resolve action %s",[cnact(A)],Lc).

overloadMethod(ALc,Lc,T,Cx,Args,Tp,ErTp,Dict,St,Stx,apply(ALc,OverOp,tple(LcA,NArgs),Tp,ErTp)) :-
  resolveContracts(Lc,Cx,Dict,St,St0,DTerms),
  markResolved(St0,St1),
  resolveTerm(Args,Dict,St1,St2,tple(LcA,RArgs)),
  resolveRef(T,DTerms,RArgs,OverOp,Dict,St2,Stx,NArgs).

overloadAccess(ALc,Lc,T,RcTp,Fld,Tp,Args,ATp,ErTp,Dict,St,Stx,
	       apply(ALc,Op,tple(LcA,NArgs),ATp,ErTp)) :-
  resolveAccess(Lc,RcTp,Fld,Tp,Dict,St,St1,AccessOp),
  resolveTerm(Args,Dict,St1,St2,tple(LcA,RArgs)),
  resolveRef(T,[AccessOp],RArgs,Op,Dict,St2,Stx,NArgs).
  
overloadCases(Cses,Resolver,Dict,St,Stx,RCases) :-
  overloadLst(Cses,resolve:overloadRule(Resolver),Dict,St,Stx,RCases).

overApply(_,OverOp,[],_,OverOp) :-!.
overApply(Lc,OverOp,Args,Tp,apply(Lc,OverOp,tple(Lc,Args),Tp,none)) :- \+isProgramType(Tp),!.
overApply(Lc,OverOp,Args,Tp,Lam) :-
  curryOver(Lc,OverOp,Args,Tp,Lam).

curryOver(Lc,OverOp,Cx,Tp,
    lambda(Lc,Lbl,rule(Lc,tple(Lc,Args),none,
          apply(Lc,OverOp,tple(Lc,NArgs),Tp,none)),funType(tplType(ArTps),Tp))) :-
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
   getConstraints(CFTp,Cx,FTp),
   sameType(FTp,Tp,Lc,Dict),
   V = v(Lc,FunNm,funType(tplType([RcTp]),FTp)),
   Acc = apply(Lc,V,tple(Lc,[Rc]),Tp,none),
   resolveContracts(Lc,Cx,Dict,St,St0,DTerms),
   resolveRef(Acc,DTerms,[],OverOp,Dict,St0,St1,NArgs),
   overApply(Lc,OverOp,NArgs,Tp,Reslvd),
   markResolved(St1,Stx);
   genMsg("accessor defined for %s:%s in %s\nnot consistent with\n%s",
	  [Fld,tpe(FAccTp),can(dot(Lc,Rc,Fld,Tp)),tpe(Tp)],Msg),
   markActive(St,Lc,Msg,Stx),
   Reslvd = dot(Lc,Rc,Fld,Tp)).
resolveDot(Lc,Rc,Fld,Tp,_Dict,St,Stx,dot(Lc,Rc,Fld,Tp)) :-
  typeOfCanon(Rc,RcTp),
  genMsg("no accessor defined for %s for type %s in %s",
	 [Fld,tpe(RcTp),can(dot(Lc,Rc,Fld,Tp))],Msg),
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
%   reportMsg("updater defined %s:%s",[Fld,tpe(FAccTp)],Lc),
   /* freshen value type again */
   freshen(VTp,Dict,_,CVTp),

   getConstraints(CVTp,Cx,FTp),
%   reportMsg("updater type %s",[tpe(FTp)],Lc),
%   reportMsg("expected type %s",[tpe(VlTp)],Lc),
   sameType(FTp,VlTp,Lc,Dict),
   V = v(Lc,FunNm,funType(tplType([RcTp,VlTp]),RcTp)),
   Acc = apply(Lc,V,tple(Lc,[Rc,Vl]),RcTp,none),

%   reportMsg("base update expression %s",[can(Acc)],Lc),

   resolveContracts(Lc,Cx,Dict,St,St0,DTerms),

%   reportMsg("resolved contracts %s",[can(tple(Lc,DTerms))],Lc),
   
   resolveRef(Acc,DTerms,[],OverOp,Dict,St0,St1,NArgs),
   overApply(Lc,OverOp,NArgs,RcTp,Reslvd),
   
%   reportMsg("access expression %s",[can(Reslvd)],Lc),
   markResolved(St1,Stx);

%   reportMsg("updater defined for %s:%s",[Fld,tpe(FAccTp)],Lc),
%   reportMsg("not consistent with %s",[tpe(RcTp)],Lc),
   
   genMsg("updater defined for %s:%s in %s\nnot consistent with\n%s",
	  [Fld,tpe(FAccTp),can(update(Lc,Rc,Fld,Vl)),tpe(RcTp)],Msg),
   markActive(St,Lc,Msg,Stx),
   Reslvd = update(Lc,Rc,Fld,Vl)).
resolveUpdate(Lc,Rc,Fld,Vl,_Dict,St,Stx,update(Lc,Rc,Fld,Vl)) :-
  typeOfCanon(Rc,RcTp),
  reportMsg("no updater defined for %s for type %s in %s",
	 [Fld,tpe(RcTp),can(update(Lc,Rc,Fld,Vl))],Lc),
  genMsg("no updater defined for %s for type %s in %s",
	 [Fld,tpe(RcTp),can(upate(Lc,Rc,Fld,Vl))],Msg),
  markActive(St,Lc,Msg,Stx).

resolveContracts(_,[],_,St,St,[]).
resolveContracts(Lc,[Con|C],Dict,St,Stx,[CV|Vs]) :-
  resolveContract(Lc,Con,Dict,St,St0,CV),
  resolveContracts(Lc,C,Dict,St0,Stx,Vs).

resolveContract(Lc,C,Dict,St,Stx,Over) :-
  implementationName(C,ImpNm),
  getImplementation(Dict,ImpNm,ImplVrNm,_ImplTp),
  getVar(Lc,ImplVrNm,Dict,Impl,ITp),
  contractType(C,CTp),
  sameType(ITp,CTp,Lc,Dict),
  markResolved(St,St1),
  resolveTerm(Impl,Dict,St1,Stx,Over).

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
  resolveContract(Lc,C,Dict,St,St0,A),
  resolveDependents(L,Lc,Dict,St0,Stx,As,Args).

formOver(V,[],_,_,V).
formOver(V,Args,Lc,Tp,apply(Lc,V,tple(Lc,Args),Tp,none)).

genVar(Nm,Lc,Tp,v(Lc,NV,Tp)) :-
  genstr(Nm,NV).

declareAccessors(_,[],Dict,Dict) :-!.
declareAccessors(Lc,[accDec(Tp,FldNm,FunNm,AcTp)|Defs],Dict,Dx) :-!,
  declareFieldAccess(Tp,FldNm,FunNm,AcTp,Dict,D0),
  declareAccessors(Lc,Defs,D0,Dx).
declareAccessors(Lc,[impDec(ImplNm,FullNm,ImplTp)|Defs],Dict,Dx) :-!,
  declareImplementation(ImplNm,FullNm,ImplTp,Dict,D0),
  declareVr(Lc,FullNm,ImplTp,none,D0,D1),
  declareAccessors(Lc,Defs,D1,Dx).
declareAccessors(Lc,[_|Defs],Dict,Dx) :-
  declareAccessors(Lc,Defs,Dict,Dx).

findAccess(Tp,FldNm,Dict,AccTp,FunNm) :-
  getFieldAccess(Tp,FldNm,FunNm,AccTp,Dict).
findAccess(_Tp,FldNm,Dict,AccTp,FunNm) :-
  is_member(access(FldNm,v(_,FunNm,AccTp)),Dict),!.

findUpdate(Tp,FldNm,Dict,AccTp,FunNm) :-
  getFieldUpdater(Tp,FldNm,FunNm,AccTp,Dict).
findUpdate(_Tp,FldNm,Dict,AccTp,FunNm) :-
  is_member(update(FldNm,v(_,FunNm,AccTp)),Dict),!.

overloadOthers(Other,Dict,OOthers) :-
  overloadList(Other,resolve:overloadOther,Dict,OOthers).

overloadOther(assertion(Lc,Cond),Dict,assertion(Lc,RCond)) :-
  overloadTerm(Cond,Dict,RCond).
overloadOther(show(Lc,Exp),Dict,show(Lc,RExp)) :-
  overloadTerm(Exp,Dict,RExp).

overloadEnum(Lc,Nm,Tp,[],Rules,Dict,enum(Lc,Nm,Tp,[],ORules)) :-
  overloadClassRules(Rules,[],Dict,ORules).
overloadEnum(Lc,Nm,Tp,Cx,Rules,Dict,class(Lc,Nm,Tp,[],ORules)) :-
  defineCVars(Lc,Cx,Dict,EVars,EDict),
  overloadClassRules(Rules,EVars,EDict,ORules).

overloadClass(Lc,Nm,Tp,Cx,Rules,Dict,class(Lc,Nm,Tp,[],ORules)) :-
  defineCVars(Lc,Cx,Dict,EVars,EDict),
  overloadClassRules(Rules,EVars,EDict,ORules).

overloadClassRules(Rules,V,D,ORules) :-
  overloadList(Rules,resolve:overloadClassRule(V),D,ORules).

overloadClassRule(CVars,labelRule(Lc,Nm,Hd,St),Dict,labelRule(Lc,Nm,OHd,OSt)) :-
  resolveHead(Hd,CVars,OHd),
  overloadOthers(St,Dict,OSt).

resolveHead(Hd,[],Hd).
resolveHead(enm(Lc,Nm,Tp),CVars,apply(Lc,v(Lc,Nm,Tp,none),CVars)).
resolveHead(apply(Lc,v(ALc,Nm,Tp),Args,Tpx,ErTp),CVars,apply(Lc,v(ALc,Nm,Tp,ErTp),OArgs,Tpx)) :-
  addExtra(CVars,Args,OArgs).

addExtra(Extra,tple(Lc,Els),tple(Lc,EEls)) :-
  concat(Extra,Els,EEls).

addExtraDefs([],Els,Els).
addExtraDefs([v(Lc,Nm,Tp)|Ex],Els,REls) :-
  addExtraDefs(Ex,[varDef(Lc,Nm,Nm,[],Tp,v(Lc,Nm,Tp))|Els],REls).
