:- module(resolve,[overload/4,overloadOthers/3]).

:- use_module(misc).
:- use_module(errors).
:- use_module(types).
:- use_module(freshen).
:- use_module(canon).
:- use_module(unify).

overload(Defs,Dict,RDict,RDefs) :-
  declareImplementations(Defs,Dict,RDict),
  overloadDefs(Defs,RDict,RDefs).

overloadDefs(D,Dict,RD) :-
  overloadList(D,overloadDef,Dict,RD).

overloadDef(funDef(Lc,Nm,ExtNm,Tp,Cx,Eqns),Dict,RF) :-
  overloadFunction(Lc,Nm,ExtNm,Tp,Cx,Eqns,Dict,RF).
overloadDef(varDef(Lc,Nm,ExtNm,Cx,Tp,Value),Dict,RD) :-
  overloadDefn(Lc,Nm,ExtNm,Cx,Tp,Value,Dict,RD).
overloadDef(T,_,T) :-
  T = typeDef(_,_,_,_).
overloadDef(C,_,C) :-
  C = cnsDef(_,_,_,_).
overloadDef(C,_,C) :-
  C = conDef(_,_,_).
overloadDef(C,_,C) :-
  C = implDef(_,_,_,_).

overloadFunction(Lc,Nm,ExtNm,Tp,[],Eqns,Dict,funDef(Lc,Nm,ExtNm,Tp,[],REqns)) :-
  overloadEquations(Eqns,Dict,[],REqns).
overloadFunction(Lc,Nm,ExtNm,Tp,Cx,Eqns,Dict,funDef(Lc,Nm,ExtNm,Tp,[],REqns)) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  overloadEquations(Eqns,FDict,CVars,REqns).

overloadEquations(Eqns,Dict,Extra,REqns) :-
  overloadList(Eqns,overloadEquation(Extra),Dict,REqns).

overloadEquation(Extra,equation(Lc,Args,Cond,Exp),Dict,equation(Lc,RArgs,RCond,RExp)) :-
  resolveTerm(Args,Dict,RA),
  addExtra(Extra,RA,RArgs),
  resolveTerm(Cond,Dict,RCond),
  resolveTerm(Exp,Dict,RExp).

% These are used when resolving lambdas only. A lambda cannot introduce any dictionary variables
overloadRule(equation(Lc,Args,Cond,Exp),Dict,St,Stx,equation(Lc,RArgs,RCond,RExp)) :-
  overloadTerm(Args,Dict,St,St0,RArgs),
  overloadTerm(Cond,Dict,St0,St1,RCond),
  overloadTerm(Exp,Dict,St1,Stx,RExp).

overloadDefn(Lc,Nm,ExtNm,[],Tp,Exp,Dict,varDef(Lc,Nm,ExtNm,[],Tp,RExp)) :-
  resolveTerm(Exp,Dict,RExp).
overloadDefn(Lc,Nm,ExtNm,Cx,Tp,Exp,Dict,varDef(Lc,Nm,ExtNm,[],Tp,
    lambda(Lc,[equation(Lc,tple(Lc,CVars),enm(Lc,"true"),RExp)],OTp))) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  contractTypes(Cx,Tps),
  makeContractFunType(Tp,Tps,OTp),
  resolveTerm(Exp,FDict,RExp).

makeContractFunType(allType(V,T),Cx,allType(V,CT)) :-
  makeContractFunType(T,Cx,CT).
makeContractFunType(constrained(T,_C),Cx,CT) :-
  makeContractFunType(T,Cx,CT).
makeContractFunType(T,Cx,funType(tupleType(Cx),T)).

defineCVars(_,[],Dict,[],Dict).
defineCVars(Lc,[Con|Cx],Dict,[NV|CVars],FDict) :-
  implementationName(Con,ImplNm),
  genVar(ImplNm,Lc,NV),
  defineCVars(Lc,Cx,[(ImplNm,NV)|Dict],CVars,FDict).
defineCVars(Lc,[implementsFace(_,_)|Cx],Dict,CVars,FDict) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict).

resolveTerm(Term,Dict,Resolved) :-
  overloadTerm(Term,Dict,inactive,St,RTerm),!,
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
overloadTerm(v(Lc,Nm),_,St,St,v(Lc,Nm)).
overloadTerm(intLit(Ix),_,St,St,intLit(Ix)).
overloadTerm(floatLit(Ix),_,St,St,floatLit(Ix)).
overloadTerm(stringLit(Sx),_,St,St,stringLit(Sx)).
overloadTerm(dot(Lc,Rc,Fld),Dict,St,Stx,dot(Lc,RRc,Fld)) :- overloadTerm(Rc,Dict,St,Stx,RRc).
overloadTerm(enm(Lc,Rf),_,St,St,enm(Lc,Rf)).
overloadTerm(cns(Lc,Rf),_,St,St,cns(Lc,Rf)).
overloadTerm(tple(Lc,Args),Dict,St,Stx,tple(Lc,RArgs)) :-
  overloadLst(Args,resolve:overloadTerm,Dict,St,Stx,RArgs).
overloadTerm(theta(Lc,Path,Defs,Others,Types,Sig),Dict,St,St,theta(Lc,Path,RDefs,ROthers,Types,Sig)) :-
  overload(Defs,Dict,RDict,RDefs),
  overloadOthers(Others,RDict,ROthers).
overloadTerm(record(Lc,Path,Defs,Others,Types,Sig),Dict,St,St,record(Lc,Path,RDefs,ROthers,Types,Sig)) :-
  overload(Defs,Dict,RDict,RDefs),
  overloadOthers(Others,RDict,ROthers).
overloadTerm(letExp(Lc,Env,Bound),Dict,St,Stx,letExp(Lc,REnv,RBound)) :-
  overloadTerm(Env,Dict,St,St0,REnv),
  overloadTerm(Bound,Dict,St0,Stx,RBound).
overloadTerm(where(Lc,Trm,Cond),Dict,St,Stx,where(Lc,RTrm,RCond)) :-
  overloadTerm(Trm,Dict,St,St0,RTrm),
  overloadTerm(Cond,Dict,St0,Stx,RCond).
overloadTerm(conj(Lc,L,R),Dict,St,Stx,conj(Lc,RL,RR)) :-
  overloadTerm(L,Dict,St,St0,RL),
  overloadTerm(R,Dict,St0,Stx,RR).
overloadTerm(disj(Lc,L,R),Dict,St,Stx,disj(Lc,RL,RR)) :-
  overloadTerm(L,Dict,St,St0,RL),
  overloadTerm(R,Dict,St0,Stx,RR).
overloadTerm(cond(Lc,T,L,R),Dict,St,Stx,cond(Lc,RT,RL,RR)) :-
  overloadTerm(T,Dict,St,St0,RT),
  overloadTerm(L,Dict,St0,St1,RL),
  overloadTerm(R,Dict,St1,Stx,RR).
overloadTerm(neg(Lc,T),Dict,St,Stx,neg(Lc,RT)) :-
  overloadTerm(T,Dict,St,Stx,RT).
overloadTerm(match(Lc,L,R),Dict,St,Stx,match(Lc,RL,RR)) :-
  overloadTerm(L,Dict,St,St0,RL),
  overloadTerm(R,Dict,St0,Stx,RR).
overloadTerm(apply(ALc,over(Lc,T,IsFn,Cx),Args),Dict,St,Stx,apply(ALc,OverOp,tple(LcA,NArgs))) :-
  resolveContracts(Lc,Cx,Dict,St,St0,DTerms),
  (St0\=active(_,_) ->
    markResolved(St0,St1),
    overloadTerm(Args,Dict,St1,Stx,tple(LcA,RArgs)),
    overloadRef(Lc,T,DTerms,RArgs,OverOp,NArgs) ;
    Stx=St0,
    OverOp = over(Lc,T,IsFn,Cx),
    NArgs = Args).
overloadTerm(apply(Lc,Op,Args),Dict,St,Stx,apply(Lc,ROp,RArgs)) :-
  overloadTerm(Op,Dict,St,St0,ROp),
  overloadTerm(Args,Dict,St0,Stx,RArgs).
overloadTerm(over(Lc,T,IsFn,Cx),Dict,St,Stx,Over) :-
  ( resolveContracts(Lc,Cx,Dict,St,St0,DTerms) ->
      (St0\=active(_,_) ->
        overloadRef(Lc,T,DTerms,[],OverOp,NArgs),
        overApply(Lc,OverOp,NArgs,IsFn,Over),
        markResolved(St0,Stx) ;
      Stx=St0,
      Over=over(Lc,T,IsFn,Cx));
      genMsg("cannot find implementation for contracts %s",[Cx],Msg),
      markActive(St,Lc,Msg,Stx),
      Over = over(Lc,T,IsFn,Cx)).
overloadTerm(mtd(Lc,Nm),_,St,Stx,mtd(Lc,Nm)) :-
  genMsg("cannot find implementation for %s",[Nm],Msg),
  markActive(St,Lc,Msg,Stx).
overloadTerm(lambda(Lc,Rls,Tp),Dict,St,Stx,lambda(Lc,ORls,Tp)) :-
  overloadLst(Rls,resolve:overloadRule,Dict,St,Stx,ORls).

overApply(_,OverOp,[],_,OverOp) :-!.
overApply(Lc,OverOp,Args,Tp,apply(Lc,OverOp,tple(Lc,Args))) :- \+isProgramType(Tp),!.
overApply(Lc,OverOp,Args,Tp,Lam) :-
  curryOver(Lc,OverOp,Args,Tp,Lam).

curryOver(Lc,OverOp,Cx,Tp,lambda(Lc,[equation(Lc,tple(Lc,Args),enm(Lc,"true"),apply(Lc,OverOp,tple(Lc,NArgs)))],Tp)) :-
  progTypeArity(Tp,Ar),
  genVrs(Ar,Lc,Args),
  concat(Cx,Args,NArgs).

genVrs(0,_,[]).
genVrs(Ix,Lc,[v(Lc,Id)|Vrs]) :-
  genstr("_",Id),
  Ix1 is Ix-1,
  genVrs(Ix1,Lc,Vrs).


overloadLst([],_,_,St,St,[]):-!.
overloadLst([T|L],C,D,St,Stx,[RT|RL]) :-
  call(C,T,D,St,St0,RT),
  overloadLst(L,C,D,St0,Stx,RL).

overloadList([],_,_,[]):-!.
overloadList([T|L],C,D,[RT|RL]) :-
  call(C,T,D,RT),
  overloadList(L,C,D,RL).

overloadRef(_,mtd(Lc,Nm,_),[DT],RArgs,dot(Lc,DT,Nm),RArgs) :- !.
overloadRef(_,v(Lc,Nm),DT,RArgs,v(Lc,Nm),Args) :- !, concat(DT,RArgs,Args).
overloadRef(_,C,DT,RArgs,C,Args) :- concat(DT,RArgs,Args).

resolveContracts(_,[],_,St,St,[]).
resolveContracts(Lc,[Con|C],Dict,St,Stx,[CV|Vs]) :-
  resolveContract(Lc,Con,Dict,St,St0,CV),
  resolveContracts(Lc,C,Dict,St0,Stx,Vs).

resolveContract(Lc,C,Dict,St,Stx,Over) :-
  implementationName(C,ImpNm),
  findImplementation(ImpNm,Dict,Impl),!,
  resolve(Impl,C,ImpNm,Lc,Dict,St,St0,Over),!,
  markResolved(St0,Stx).
resolveContract(Lc,C,_,St,Stx,C) :-
  genMsg("no implementation known for %s",[C],Msg),
  markActive(St,Lc,Msg,Stx).

resolve(v(Lc,Nm),_,_,_,_,St,St,v(Lc,Nm)) :-!.
resolve(I,C,ImpNm,Lc,Dict,St,Stx,Over) :-
  freshen(I,[],_,Con),
  moveConstraints(Con,Cx,contractExists(CT,_)),
  sameContract(CT,C,[]),
  resolveDependents(Cx,Lc,Dict,St,St0,Args,[]),
  (St0\=active(_,_) ->
    formOver(v(Lc,ImpNm),Args,Lc,Over),
    markResolved(St0,Stx) ;
    Stx=St0, Over = I).
resolve(T,C,_,Lc,_,St,Stx,T) :-
  genMsg("cannot resolve contract %s",[C],Msg),
  markActive(St,Lc,Msg,Stx).

resolveDependents([],_,_,St,St,Args,Args).
resolveDependents([C|L],Lc,Dict,St,Stx,[A|As],Args) :-
  resolveContract(Lc,C,Dict,St,St0,A),
  resolveDependents(L,Lc,Dict,St0,Stx,As,Args).

formOver(V,[],_,V).
formOver(V,Args,Lc,apply(Lc,V,tple(Lc,Args))).

genVar(Nm,Lc,v(Lc,NV)) :-
  genstr(Nm,NV).

declareImplementations([],Dict,Dict).
declareImplementations([implDef(_,_,ImplName,Spec)|Defs],Dict,RDict) :-
  declareImplementations(Defs,[(ImplName,Spec)|Dict],RDict).
declareImplementations([_|Defs],Dict,RDict) :-
  declareImplementations(Defs,Dict,RDict).

findImplementation(ImplName,Dict,Spec) :-
  is_member((ImplName,Spec),Dict).

inheritImplementations([],_,[]).
inheritImplementations([Impl|L],Hd,[rule(Hd,Impl)|M]) :-
  inheritImplementations(L,Hd,M).

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
resolveHead(enm(Lc,Nm),CVars,apply(Lc,v(Lc,Nm),CVars)).
resolveHead(apply(Lc,v(ALc,Nm),Args),CVars,apply(Lc,v(ALc,Nm),OArgs)) :-
  addExtra(CVars,Args,OArgs).
resolveHead(record(Lc,Lbl,Defs,[],[],Sig),CVars,record(Lc,Lbl,RDefs,[],[],Sig)) :-
  addExtraDefs(CVars,Defs,RDefs).
resolveHead(theta(Lc,Lbl,Defs,[],[],Sig),CVars,theta(Lc,Lbl,RDefs,[],[],Sig)) :-
  addExtraDefs(CVars,Defs,RDefs).

addExtra(Extra,tple(Lc,Els),tple(Lc,EEls)) :-
  concat(Extra,Els,EEls).

addExtraDefs([],Els,Els).
addExtraDefs([v(Lc,Nm)|Ex],Els,REls) :-
  addExtraDefs(Ex,[varDef(Lc,Nm,Nm,[],voidType,v(Lc,Nm))|Els],REls).
