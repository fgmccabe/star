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

overloadRules(Eqns,Dict,St,Stx,OEqns) :-
  overloadLst(Eqns,overloadRule,Dict,St,Stx,OEqns).

overloadDefn(Lc,Nm,ExtNm,[],Tp,Exp,Dict,varDef(Lc,Nm,ExtNm,[],Tp,RExp)) :-
  resolveTerm(Exp,Dict,RExp).
overloadDefn(Lc,Nm,ExtNm,Cx,Tp,Exp,Dict,varDef(Lc,Nm,ExtNm,[],Tp,
    lambda(Lc,equation(Lc,tple(Lc,CVars),enm(Lc,"true",type("star.core*boolean")),RExp),OTp))) :-
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
  contractType(Con,Tp),
  genVar(ImplNm,Lc,Tp,NV),
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
overloadTerm(v(Lc,Nm,Tp),_,St,St,v(Lc,Nm,Tp)).
overloadTerm(intLit(Ix,Tp),_,St,St,intLit(Ix,Tp)).
overloadTerm(floatLit(Ix,Tp),_,St,St,floatLit(Ix,Tp)).
overloadTerm(stringLit(Sx,Tp),_,St,St,stringLit(Sx,Tp)).
overloadTerm(dot(Lc,Rc,Fld,Tp),Dict,St,Stx,dot(Lc,RRc,Fld,Tp)) :- overloadTerm(Rc,Dict,St,Stx,RRc).
overloadTerm(enm(Lc,Rf,Tp),_,St,St,enm(Lc,Rf,Tp)).
overloadTerm(cons(Lc,Rf,Tp),_,St,St,cons(Lc,Rf,Tp)).
overloadTerm(tple(Lc,Args),Dict,St,Stx,tple(Lc,RArgs)) :-
  overloadLst(Args,resolve:overloadTerm,Dict,St,Stx,RArgs).
overloadTerm(theta(Lc,Path,Anon,Defs,Others,Types,Sig),Dict,St,St,theta(Lc,Path,Anon,RDefs,ROthers,Types,Sig)) :-
  overload(Defs,Dict,RDict,RDefs),
  overloadOthers(Others,RDict,ROthers).
overloadTerm(record(Lc,Path,Anon,Defs,Others,Types,Sig),Dict,St,St,record(Lc,Path,Anon,RDefs,ROthers,Types,Sig)) :-
  overload(Defs,Dict,RDict,RDefs),
  overloadOthers(Others,RDict,ROthers).
overloadTerm(varRef(Lc,Inn),Dict,St,Stx,varRef(Lc,Inn1)) :-
  overloadTerm(Inn,Dict,St,Stx,Inn1).
overloadTerm(cell(Lc,Inn),Dict,St,Stx,cell(Lc,Inn1)) :-
  overloadTerm(Inn,Dict,St,Stx,Inn1).
overloadTerm(assign(Lc,Lhs,Rhs),Dict,St,Stx,assign(Lc,L1,R1)) :-
  overloadTerm(Lhs,Dict,St,St1,L1),
  overloadTerm(Rhs,Dict,St1,Stx,R1).
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
overloadTerm(abstraction(Lc,B,C,Zed,Gen,Tp),
	     Dict,St,Stx,abstraction(Lc,RB,RC,RZed,RGen,Tp)) :-
  overloadTerm(B,Dict,St,St0,RB),
  overloadTerm(C,Dict,St0,St1,RC),
  overloadTerm(Zed,Dict,St1,St2,RZed),
  overloadTerm(Gen,Dict,St2,Stx,RGen).
overloadTerm(apply(ALc,over(Lc,T,_,Cx),Args,Tp),Dict,St,Stx,apply(ALc,OverOp,tple(LcA,NArgs),Tp)) :-
  resolveContracts(Lc,Cx,Dict,St,St0,DTerms),
  markResolved(St0,St1),
  overloadTerm(Args,Dict,St1,Stx,tple(LcA,RArgs)),
  overloadRef(Lc,T,DTerms,RArgs,OverOp,NArgs).
overloadTerm(apply(Lc,Op,Args,Tp),Dict,St,Stx,apply(Lc,ROp,RArgs,Tp)) :-
  overloadTerm(Op,Dict,St,St0,ROp),
  overloadTerm(Args,Dict,St0,Stx,RArgs).
overloadTerm(over(Lc,T,IsFn,Cx),Dict,St,Stx,Over) :-
  ( resolveContracts(Lc,Cx,Dict,St,St0,DTerms) ->
      overloadRef(Lc,T,DTerms,[],OverOp,NArgs),
      overApply(Lc,OverOp,NArgs,IsFn,Over),
      markResolved(St0,Stx);
      genMsg("cannot find implementation for contracts %s",[Cx],Msg),
      markActive(St,Lc,Msg,Stx),
      Over = over(Lc,T,IsFn,Cx)).
overloadTerm(mtd(Lc,Nm,Tp),_,St,Stx,mtd(Lc,Nm,Tp)) :-
  genMsg("cannot find implementation for %s",[Nm],Msg),
  markActive(St,Lc,Msg,Stx).
overloadTerm(lambda(Lc,Eqn,Tp),Dict,St,Stx,lambda(Lc,OEqn,Tp)) :-
  overloadRule(Eqn,Dict,St,Stx,OEqn).
overloadTerm(doTerm(Lc,Body,ElTp,ErTp,Con),Dict,St,Stx,doTerm(Lc,RBody,ElTp,ErTp,Con)) :-
  overloadAction(Body,Dict,St,Stx,RBody).

overloadAction(seqDo(Lc,A,B),Dict,St,Stx,seqDo(Lc,RA,RB)) :-
  overloadAction(A,Dict,St,St1,RA),
  overloadAction(B,Dict,St1,Stx,RB).
overloadAction(bindDo(Lc,Ptn,Exp,PT,ErTp),Dict,St,Stx,bindDo(Lc,RPtn,RExp,PT,ErTp)) :-
  overloadTerm(Ptn,Dict,St,St1,RPtn),
  overloadTerm(Exp,Dict,St1,Stx,RExp).
overloadAction(varDo(Lc,Ptn,Exp),Dict,St,Stx,varDo(Lc,RPtn,RExp)) :-
  overloadTerm(Ptn,Dict,St,St1,RPtn),
  overloadTerm(Exp,Dict,St1,Stx,RExp).
overloadAction(ifthenDo(Lc,Tst,Th,El,StTp,ErTp),Dict,St,Stx,ifthenDo(Lc,RTst,RTh,REl,StTp,ErTp)) :-
  overloadTerm(Tst,Dict,St,St1,RTst),
  overloadAction(Th,Dict,St1,St2,RTh),
  overloadAction(El,Dict,St2,Stx,REl).
overloadAction(whileDo(Lc,Tst,Body,StTp,ErTp),Dict,St,Stx,whileDo(Lc,RTst,RBody,StTp,ErTp)) :-
  overloadTerm(Tst,Dict,St,St1,RTst),
  overloadAction(Body,Dict,St1,Stx,RBody).
overloadAction(forDo(Lc,Tst,Body,StTp,ErTp),Dict,St,Stx,forDo(Lc,RTst,RBody,StTp,ErTp)) :-
  overloadTerm(Tst,Dict,St,St1,RTst),
  overloadAction(Body,Dict,St1,Stx,RBody).
overloadAction(tryCatchDo(Lc,Body,Hndlr,StTp,ErTp),Dict,St,Stx,tryCatchDo(Lc,RBody,RHndlr,StTp,ErTp)) :-
  overloadAction(Body,Dict,St,St1,RBody),
  overloadTerm(Hndlr,Dict,St1,Stx,RHndlr).
overloadAction(returnDo(Lc,Exp,ElTp),Dict,St,Stx,returnDo(Lc,RExp,ElTp)) :-
  overloadTerm(Exp,Dict,St,Stx,RExp).
overloadAction(throwDo(Lc,Exp,ErTp),Dict,St,Stx,throwDo(Lc,RExp,ErTp)) :-
  overloadTerm(Exp,Dict,St,Stx,RExp).
overloadAction(performDo(Lc,Exp,ErTp),Dict,St,Stx,performDo(Lc,RExp,ErTp)) :-
  overloadTerm(Exp,Dict,St,Stx,RExp).

overloadActions([],_,St,St,[]).
overloadActions([A|As],Dict,St,Stx,[RA|RAs]) :-
  overloadAction(A,Dict,St,St1,RA),
  overloadActions(As,Dict,St1,Stx,RAs).

overApply(_,OverOp,[],_,OverOp) :-!.
overApply(Lc,OverOp,Args,Tp,apply(Lc,OverOp,tple(Lc,Args),Tp)) :- \+isProgramType(Tp),!.
overApply(Lc,OverOp,Args,Tp,Lam) :-
  curryOver(Lc,OverOp,Args,Tp,Lam).

curryOver(Lc,OverOp,Cx,Tp,
    lambda(Lc,equation(Lc,tple(Lc,Args),enm(Lc,"true",type("star.core*boolean")),
          apply(Lc,OverOp,tple(Lc,NArgs),Tp)),funType(tupleType(ArTps),Tp))) :-
  progArgTypes(Tp,ArTps),
  genVrs(ArTps,Lc,Args),
  concat(Cx,Args,NArgs).

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

overloadRef(_,mtd(Lc,Nm,Tp),[DT|Ds],RArgs,dot(Lc,DT,Nm,Tp),Args) :- !,
  concat(Ds,RArgs,Args).
overloadRef(_,v(Lc,Nm,Tp),DT,RArgs,v(Lc,Nm,Tp),Args) :- !,
  concat(DT,RArgs,Args).
overloadRef(_,C,DT,RArgs,C,Args) :-
  concat(DT,RArgs,Args).

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

resolve(v(Lc,Nm,Tp),_,_,_,_,St,St,v(Lc,Nm,Tp)) :-!.
resolve(I,C,ImpNm,Lc,Dict,St,Stx,Over) :-
  freshen(I,[],_,Con),
  moveConstraints(Con,Cx,contractExists(CT,_)),
  sameContract(CT,C,[]),
  resolveDependents(Cx,Lc,Dict,St,St0,Args,[]),
  contractType(C,Tp),
  (St0\=active(_,_) ->
    formOver(v(Lc,ImpNm,Tp),Args,Lc,Tp,Over),
    markResolved(St0,Stx) ;
    Stx=St0, Over = I).
resolve(T,C,_,Lc,_,St,Stx,T) :-
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
resolveHead(enm(Lc,Nm,Tp),CVars,apply(Lc,v(Lc,Nm,Tp),CVars)).
resolveHead(apply(Lc,v(ALc,Nm,Tp),Args,Tpx),CVars,apply(Lc,v(ALc,Nm,Tp),OArgs,Tpx)) :-
  addExtra(CVars,Args,OArgs).
resolveHead(record(Lc,Lbl,Anon,Defs,[],[],Sig),CVars,record(Lc,Lbl,Anon,RDefs,[],[],Sig)) :-
  addExtraDefs(CVars,Defs,RDefs).
resolveHead(theta(Lc,Lbl,Anon,Defs,[],[],Sig),CVars,theta(Lc,Lbl,Anon,RDefs,[],[],Sig)) :-
  addExtraDefs(CVars,Defs,RDefs).

addExtra(Extra,tple(Lc,Els),tple(Lc,EEls)) :-
  concat(Extra,Els,EEls).

addExtraDefs([],Els,Els).
addExtraDefs([v(Lc,Nm,Tp)|Ex],Els,REls) :-
  addExtraDefs(Ex,[varDef(Lc,Nm,Nm,[],Tp,v(Lc,Nm,Tp))|Els],REls).
