:- module(resolve,[overload/5,overloadOthers/3]).

:- use_module(dict).
:- use_module(misc).
:- use_module(errors).
:- use_module(types).
:- use_module(freshen).
:- use_module(canon).
:- use_module(unify).
:- use_module(vartypes).

overload(Lc,Defs,Dict,RDict,RDefs) :-
  declareAccessors(Lc,Defs,Dict,RDict),
  map(Defs,resolve:overloadDef(RDict),RDefs).

overloadDef(Dict,funDef(Lc,Nm,ExtNm,Tp,Cx,Eqns),RF) :-!,
  overloadFunction(Lc,Nm,ExtNm,Tp,Cx,Eqns,Dict,RF).
overloadDef(Dict,varDef(Lc,Nm,ExtNm,Cx,Tp,Value),RD) :-!,
  overloadDefn(Lc,Nm,ExtNm,Cx,Tp,Value,Dict,RD).
overloadDef(_,Def,Def).

overloadFunction(Lc,Nm,ExtNm,Tp,[],Eqns,Dict,funDef(Lc,Nm,ExtNm,Tp,[],REqns)) :-
  overloadEquations(Eqns,Dict,[],REqns),!.
overloadFunction(Lc,Nm,ExtNm,Tp,Cx,Eqns,Dict,funDef(Lc,Nm,ExtNm,Tp,[],REqns)) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  overloadEquations(Eqns,FDict,CVars,REqns),!.

overloadEquations(Eqns,Dict,Extra,REqns) :-
  overloadList(Eqns,overloadEquation(Extra),Dict,REqns).

overloadEquation(Extra,equation(Lc,Args,G,Exp),Dict,
		 equation(Lc,RArgs,RG,RExp)) :-
  resolveTerm(Args,Dict,RA),
  addExtra(Extra,RA,RArgs),
  resolveGuard(G,Dict,RG),
  resolveTerm(Exp,Dict,RExp).

resolveGuard(none,_,none) :-!.
resolveGuard(some(G),Dict,some(RG)) :-
  resolveTerm(G,Dict,RG).

% These are used when resolving lambdas only. A lambda cannot introduce any dictionary variables
overloadRule(equation(Lc,Args,G,Exp),Dict,St,Stx,equation(Lc,RArgs,RG,RExp)) :-
  overloadTerm(Args,Dict,St,St0,RArgs),
  overloadGuard(G,Dict,St0,St1,RG),
  overloadTerm(Exp,Dict,St1,Stx,RExp).

overloadDefn(Lc,Nm,ExtNm,[],Tp,Exp,Dict,varDef(Lc,Nm,ExtNm,[],Tp,RExp)) :-
  resolveTerm(Exp,Dict,RExp).
overloadDefn(Lc,Nm,ExtNm,Cx,Tp,Exp,Dict,varDef(Lc,Nm,ExtNm,[],Tp,
    lambda(Lc,Lbl,equation(Lc,tple(Lc,CVars),none,RExp),OTp))) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  contractTypes(Cx,Tps),
  makeContractFunType(Tp,Tps,OTp),
  resolveTerm(Exp,FDict,RExp),
  lambdaLbl(Nm,"over",Lbl).

makeContractFunType(allType(V,T),Cx,allType(V,CT)) :-
  makeContractFunType(T,Cx,CT).
makeContractFunType(constrained(T,_C),Cx,CT) :-
  makeContractFunType(T,Cx,CT).
makeContractFunType(T,Cx,funType(tupleType(Cx),T)).

defineCVars(_,[],Dict,[],Dict).
defineCVars(Lc,[Con|Cx],Dict,[v(Lc,CVarNm,ConTp)|CVars],FDict) :-
  implementationName(Con,ImplNm),
  localName("_",value,ImplNm,CVarNm),
  contractType(Con,ConTp),
  declareImplementation(ImplNm,CVarNm,ConTp,Dict,D0),
  declareVr(Lc,CVarNm,ConTp,D0,D1),
  defineCVars(Lc,Cx,D1,CVars,FDict).
defineCVars(Lc,[implementsFace(X,faceType(Flds,_))|Cx],Dict,CVars,FDict) :-
  fieldVars(Lc,X,Flds,CVars,CVrs,Dict,CDict),
  defineCVars(Lc,Cx,CDict,CVrs,FDict).

fieldVars(_,_,[],CVars,CVars,Dict,Dict).
fieldVars(Lc,Tp,[(FldNm,FldTp)|Flds],[NV|CVrs],XVrs,Dict,CDict) :-
  genVar(FldNm,Lc,funType(tupleType([Tp]),FldTp),NV),
  fieldVars(Lc,Tp,Flds,CVrs,XVrs,[access(FldNm,NV)|Dict],CDict).

resolveTerm(Term,Dict,Resolved) :-
%  locOfCanon(Term,Lc),
%  reportMsg("resolving %s",[Term],Lc),
  overloadTerm(Term,Dict,inactive,St,RTerm),!,
%  reportMsg("%s resolved %s",[Term,St]),
  resolveAgain(inactive,St,Term,RTerm,Dict,Resolved).

% Somewhat complex logic to allow multiple iterations unless it will not help
resolveAgain(_,resolved,Term,T,Dict,R) :- !,
  overloadTerm(T,Dict,inactive,St,T0),
  resolveAgain(inactive,St,Term,T0,Dict,R).
resolveAgain(_,inactive,_,T,_,T) :- !.
resolveAgain(active(_,_),active(Lc,Msg),Term,_,_,Term) :-
  reportError(Msg,[],Lc).
resolveAgain(_,active(Lc,Msg),Orig,_,Dict,R) :-
  overloadTerm(Orig,Dict,inactive,St,T0),
  resolveAgain(active(Lc,Msg),St,Orig,T0,Dict,R).

markActive(_,Lc,Msg,active(Lc,Msg)).

markResolved(inactive,resolved).
markResolved(St,St).

overloadTerm(void,_,St,St,void).
overloadTerm(v(Lc,Nm,Tp),_,St,St,v(Lc,Nm,Tp)).
overloadTerm(intLit(Ix,Tp),_,St,St,intLit(Ix,Tp)).
overloadTerm(floatLit(Ix,Tp),_,St,St,floatLit(Ix,Tp)).
overloadTerm(stringLit(Sx,Tp),_,St,St,stringLit(Sx,Tp)).
overloadTerm(dot(Lc,Rc,Fld,Tp),Dict,St,Stx,Dot) :-
  overloadTerm(Rc,Dict,St,St0,RRc),
  resolveAccess(Lc,RRc,Fld,Tp,Dict,St0,Stx,Dot).
overloadTerm(enm(Lc,Rf,Tp),_,St,St,enm(Lc,Rf,Tp)).
overloadTerm(cons(Lc,Rf,Tp),_,St,St,cons(Lc,Rf,Tp)).
overloadTerm(tple(Lc,Args),Dict,St,Stx,tple(Lc,RArgs)) :-
  overloadLst(Args,resolve:overloadTerm,Dict,St,Stx,RArgs).
overloadTerm(cell(Lc,Inn),Dict,St,Stx,cell(Lc,Inn1)) :-
  overloadTerm(Inn,Dict,St,Stx,Inn1).
overloadTerm(assign(Lc,Lhs,Rhs),Dict,St,Stx,assign(Lc,L1,R1)) :-
  overloadTerm(Lhs,Dict,St,St1,L1),
  overloadTerm(Rhs,Dict,St1,Stx,R1).
overloadTerm(letExp(Lc,Decls,Defs,Bound),Dict,St,Stx,letExp(Lc,Decls,RDefs,RBound)) :-
  overload(Lc,Defs,Dict,RDict,RDefs),
  overloadTerm(Bound,RDict,St,Stx,RBound).
overloadTerm(letRec(Lc,Decls,Defs,Bound),Dict,St,Stx,letRec(Lc,Decls,RDefs,RBound)) :-
  overload(Lc,Defs,Dict,RDict,RDefs),
  overloadTerm(Bound,RDict,St,Stx,RBound).
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
overloadTerm(search(Lc,P,S,I),Dict,St,Stx,search(Lc,RP,RS,RI)) :-
  overloadTerm(P,Dict,St,St0,RP),
  overloadTerm(S,Dict,St0,St1,RS),
  overloadTerm(I,Dict,St1,Stx,RI).
overloadTerm(case(Lc,B,C,Tp),Dict,St,Stx,case(Lc,RB,RC,Tp)) :-
  overloadTerm(B,Dict,St,St0,RB),
  overloadCases(C,Dict,St0,Stx,RC).
overloadTerm(abstraction(Lc,B,C,Zed,Gen,Tp),
	     Dict,St,Stx,abstraction(Lc,RB,RC,RZed,RGen,Tp)) :-
  overloadTerm(B,Dict,St,St0,RB),
  overloadTerm(C,Dict,St0,St1,RC),
  overloadTerm(Zed,Dict,St1,St2,RZed),
  overloadTerm(Gen,Dict,St2,Stx,RGen).
overloadTerm(apply(ALc,over(Lc,T,_,Cx),Args,Tp),Dict,St,Stx,Term) :-
  overloadMethod(ALc,Lc,T,Cx,Args,Tp,Dict,St,Stx,Term).
overloadTerm(apply(Lc,overaccess(T,V,Face),Args,Tp),
	     Dict,St,Stx,Term) :-
  overloadAccess(Lc,T,V,Face,Args,Tp,Dict,St,Stx,Term).
overloadTerm(apply(Lc,Op,Args,Tp),Dict,St,Stx,apply(Lc,ROp,RArgs,Tp)) :-
  overloadTerm(Op,Dict,St,St0,ROp),
  overloadTerm(Args,Dict,St0,Stx,RArgs).
overloadTerm(over(Lc,T,IsFn,Cx),Dict,St,Stx,Over) :-
  ( resolveContracts(Lc,Cx,Dict,St,St0,DTerms) ->
    overloadRef(Lc,T,DTerms,[],OverOp,Dict,St0,St1,NArgs),
    overApply(Lc,OverOp,NArgs,IsFn,Over),
    markResolved(St1,Stx);
      genMsg("cannot find implementation for contracts %s",[Cx],Msg),
      markActive(St,Lc,Msg,Stx),
      Over = over(Lc,T,IsFn,Cx)).
overloadTerm(mtd(Lc,Nm,Tp),_,St,Stx,mtd(Lc,Nm,Tp)) :-
  genMsg("cannot find implementation for %s",[Nm],Msg),
  markActive(St,Lc,Msg,Stx).
overloadTerm(lambda(Lc,Lbl,Eqn,Tp),Dict,St,Stx,lambda(Lc,Lbl,OEqn,Tp)) :-
  overloadRule(Eqn,Dict,St,Stx,OEqn).
overloadTerm(valof(Lc,T,Tp),Dict,St,Stx,valof(Lc,RT,Tp)) :-
  overloadTerm(T,Dict,St,Stx,RT).
overloadTerm(doTerm(Lc,Body,Tp),Dict,St,Stx,doTerm(Lc,RBody,Tp)) :-!,
  overloadAction(Body,Dict,St,Stx,RBody).
overloadTerm(taskTerm(Lc,Body,Tp),Dict,St,Stx,taskTerm(Lc,RBody,Tp)) :-
  overloadAction(Body,Dict,St,Stx,RBody).
overloadTerm(T,_,St,St,T) :-
  locOfCanon(T,Lc),
  reportError("invalid term to resolve %s",[can(T)],Lc).

overloadGuard(none,_,Stx,Stx,none) :-!.
overloadGuard(some(G),Dict,St,Stx,some(RG)) :-
  overloadTerm(G,Dict,St,Stx,RG).

overloadMethod(ALc,Lc,T,Cx,Args,Tp,Dict,St,Stx,apply(ALc,OverOp,tple(LcA,NArgs),Tp)) :-
  resolveContracts(Lc,Cx,Dict,St,St0,DTerms),
  markResolved(St0,St1),
  overloadTerm(Args,Dict,St1,St2,tple(LcA,RArgs)),
  overloadRef(Lc,T,DTerms,RArgs,OverOp,Dict,St2,Stx,NArgs).

overloadAccess(Lc,T,V,Face,Args,Tp,Dict,St,Stx,
	       apply(Lc,RT,tple(LcA,[v(Lc,FunNm,
				       funType(tupleType([V]),FldTp))|RArgs]),Tp)) :-
  overloadTerm(T,Dict,St,St0,RT),
  deRef(Face,faceType([(Fld,FldTp)],[])),
  markResolved(St0,St1),
  overloadTerm(Args,Dict,St1,Stx,tple(LcA,RArgs)),
  findAccess(V,Fld,Dict,FunNm).

overloadCases(Cses,Dict,St,Stx,RCases) :-
  overloadLst(Cses,resolve:overloadRule,Dict,St,Stx,RCases).

overloadAction(seqDo(Lc,A,B),Dict,St,Stx,seqDo(Lc,RA,RB)) :-
  overloadAction(A,Dict,St,St1,RA),
  overloadAction(B,Dict,St1,Stx,RB).
overloadAction(varDo(Lc,Ptn,Exp),Dict,St,Stx,varDo(Lc,RPtn,RExp)) :-
  overloadTerm(Ptn,Dict,St,St1,RPtn),
  overloadTerm(Exp,Dict,St1,Stx,RExp).
overloadAction(ifthenDo(Lc,Tst,Th,El),Dict,St,Stx,
	       ifthenDo(Lc,RTst,RTh,REl)) :-
  overloadTerm(Tst,Dict,St,St1,RTst),
  overloadAction(Th,Dict,St1,St2,RTh),
  overloadAction(El,Dict,St2,Stx,REl).
overloadAction(whileDo(Lc,Tst,Body),Dict,St,Stx,whileDo(Lc,RTst,RBody)) :-
  overloadTerm(Tst,Dict,St,St1,RTst),
  overloadAction(Body,Dict,St1,Stx,RBody).
overloadAction(untilDo(Lc,Tst,Body),Dict,St,Stx,untilDo(Lc,RTst,RBody)) :-
  overloadTerm(Tst,Dict,St,St1,RTst),
  overloadAction(Body,Dict,St1,Stx,RBody).
overloadAction(forDo(Lc,Tst,Body),Dict,St,Stx,forDo(Lc,RTst,RBody)) :-
  overloadTerm(Tst,Dict,St,St1,RTst),
  overloadAction(Body,Dict,St1,Stx,RBody).
overloadAction(tryCatchDo(Lc,Body,Hndlr),Dict,St,Stx,tryCatchDo(Lc,RBody,RHndlr)) :-
  overloadAction(Body,Dict,St,St1,RBody),
  overloadTerm(Hndlr,Dict,St1,Stx,RHndlr).
overloadAction(valisDo(Lc,Exp),Dict,St,Stx,valisDo(Lc,RExp)) :-
  overloadTerm(Exp,Dict,St,Stx,RExp).
overloadAction(throwDo(Lc,Exp),Dict,St,Stx,throwDo(Lc,RExp)) :-
  overloadTerm(Exp,Dict,St,Stx,RExp).
overloadAction(performDo(Lc,Exp),Dict,St,Stx,performDo(Lc,RExp)) :-
  overloadAction(Exp,Dict,St,Stx,RExp).
overloadAction(simpleDo(Lc,Exp),Dict,St,Stx,simpleDo(Lc,RExp)) :-
  overloadTerm(Exp,Dict,St,Stx,RExp).
overloadAction(caseDo(Lc,Exp,Cses),Dict,St,Stx,caseDo(Lc,RExp,RCases)) :-
  overloadTerm(Exp,Dict,St,St0,RExp),
  overloadCases(Cses,Dict,St0,Stx,RCases).

overloadActions([],_,St,St,[]).
overloadActions([A|As],Dict,St,Stx,[RA|RAs]) :-
  overloadAction(A,Dict,St,St1,RA),
  overloadActions(As,Dict,St1,Stx,RAs).

overApply(_,OverOp,[],_,OverOp) :-!.
overApply(Lc,OverOp,Args,Tp,apply(Lc,OverOp,tple(Lc,Args),Tp)) :- \+isProgramType(Tp),!.
overApply(Lc,OverOp,Args,Tp,Lam) :-
  curryOver(Lc,OverOp,Args,Tp,Lam).

curryOver(Lc,OverOp,Cx,Tp,
    lambda(Lc,Lbl,equation(Lc,tple(Lc,Args),none,
          apply(Lc,OverOp,tple(Lc,NArgs),Tp)),funType(tupleType(ArTps),Tp))) :-
  progArgTypes(Tp,ArTps),
  genVrs(ArTps,Lc,Args),
  concat(Cx,Args,NArgs),
  lambdaLbl("","curry",Lbl).

genVrs([],_,[]).
genVrs([Tp|ArTps],Lc,[v(Lc,Id,Tp)|Vrs]) :-
  genstr("_",Id),
  genVrs(ArTps,Lc,Vrs).

overloadLst([],_,_,St,St,[]):-!.
overloadLst([T|L],C,D,St,Stx,[RT|RL]) :-
  call(C,T,D,St,St0,RT),
  overloadLst(L,C,D,St0,Stx,RL).

overloadList([],_,_,[]):-!.
overloadList([T|L],C,D,[RT|RL]) :-
  call(C,T,D,RT),
  overloadList(L,C,D,RL).

overloadRef(_,mtd(Lc,Nm,Tp),[DT|Ds],RArgs,MtdCall,Dict,St,Stx,Args) :- !,
  concat(Ds,RArgs,Args),
  resolveAccess(Lc,DT,Nm,Tp,Dict,St,Stx,MtdCall).
overloadRef(_,v(Lc,Nm,Tp),DT,RArgs,v(Lc,Nm,Tp),_,Stx,Stx,Args) :- !,
  concat(DT,RArgs,Args).
overloadRef(_,C,DT,RArgs,C,_,Stx,Stx,Args) :-
  concat(DT,RArgs,Args).

resolveAccess(Lc,Rc,Fld,Tp,Dict,Stx,Stx,apply(Lc,V,tple(Lc,[Rc]),Tp)) :-
  typeOfCanon(Rc,RcTp),
  findAccess(RcTp,Fld,Dict,FunNm),
  V = v(Lc,FunNm,funType(tupleType([RcTp]),Tp)).
resolveAccess(Lc,Rc,Fld,Tp,_Dict,St,Stx,dot(Lc,Rc,Fld,Tp)) :-
  typeOfCanon(Rc,RcTp),
  genMsg("no accessor defined for %s for type %s in %s",
	 [Fld,tpe(RcTp),can(dot(Lc,Rc,Fld,Tp))],Msg),
  markActive(St,Lc,Msg,Stx).

resolveContracts(_,[],_,St,St,[]).
resolveContracts(Lc,[Con|C],Dict,St,Stx,[CV|Vs]) :-
  resolveContract(Lc,Con,Dict,St,St0,CV),
  resolveContracts(Lc,C,Dict,St0,Stx,Vs).

resolveContract(Lc,C,Dict,St,Stx,Over) :-
  implementationName(C,ImpNm),
  getImplementation(Dict,ImpNm,ImplVrNm,_ImplTp),
  getVar(Lc,ImplVrNm,Dict,D0,Impl),
  typeOfCanon(Impl,ITp), % ITp=freshened(ImplTp)
  contractType(C,CTp),
  sameType(ITp,CTp,Dict),
  markResolved(St,St1),
  overloadTerm(Impl,D0,St1,Stx,Over).
resolveContract(Lc,C,_,St,Stx,C) :-
  genMsg("no implementation known for %s",[C],Msg),
  markActive(St,Lc,Msg,Stx).

resolveImpl(v(Lc,Nm,Tp),_,_,_,_,St,St,v(Lc,Nm,Tp)) :-!.
resolveImpl(I,C,ImpNm,Lc,Dict,St,Stx,Over) :-
  freshen(I,[],_,Con),
  getConstraints(Con,Cx,CT),
  contractType(C,Tp),
  sameType(CT,Tp,[]),
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
formOver(V,Args,Lc,Tp,apply(Lc,V,tple(Lc,Args),Tp)).

genVar(Nm,Lc,Tp,v(Lc,NV,Tp)) :-
  genstr(Nm,NV).

declareAccessors(_,[],Dict,Dict).
/*declareAccessors(Lc,[implDef(_,ImplNm,ImplVrNm,ImplTp)|Defs],Dict,Dx) :-
  funResType(ImplTp,ResTp),
  contractType(ResTp,ConTp),
  declareVr(Lc,ImplNm,ImplTp,Dict,D0),
  declareImplementation(Lc,ConTp,ImplNm,ImplTp,D0,D1),
  declareAccessors(Lc,Defs,D1,Dx).
  */
declareAccessors(Lc,[accDef(Tp,FldNm,FunNm,AcTp)|Defs],Dict,Dx) :-
  declareFieldAccess(Tp,FldNm,FunNm,AcTp,Dict,D0),
  declareAccessors(Lc,Defs,D0,Dx).
declareAccessors(Lc,[_|Defs],Dict,Dx) :-
  declareAccessors(Lc,Defs,Dict,Dx).

findAccess(Tp,FldNm,Dict,FunNm) :-
  getFieldAccess(Tp,FldNm,FunNm,_,Dict).
findAccess(_Tp,FldNm,Dict,FunNm) :-
  is_member(access(FldNm,v(_,FunNm,_)),Dict),!.

overloadOthers(Other,Dict,OOthers) :-
  overloadList(Other,resolve:overloadOther,Dict,OOthers).

overloadOther(assertion(Lc,Cond),Dict,assertion(Lc,RCond)) :-
  resolveTerm(Cond,Dict,RCond).
overloadOther(show(Lc,Exp),Dict,show(Lc,RExp)) :-
  resolveTerm(Exp,Dict,RExp).

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
resolveHead(enm(Lc,Nm,Tp),CVars,apply(Lc,v(Lc,Nm,Tp),CVars)).
resolveHead(apply(Lc,v(ALc,Nm,Tp),Args,Tpx),CVars,apply(Lc,v(ALc,Nm,Tp),OArgs,Tpx)) :-
  addExtra(CVars,Args,OArgs).

addExtra(Extra,tple(Lc,Els),tple(Lc,EEls)) :-
  concat(Extra,Els,EEls).

addExtraDefs([],Els,Els).
addExtraDefs([v(Lc,Nm,Tp)|Ex],Els,REls) :-
  addExtraDefs(Ex,[varDef(Lc,Nm,Nm,[],Tp,v(Lc,Nm,Tp))|Els],REls).
