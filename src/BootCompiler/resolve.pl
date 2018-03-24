:- module(resolve,[overload/4,overloadOthers/3,resolveContract/4]).

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

overloadDef(funDef(Lc,Nm,Tp,Cx,Eqns),Dict,RF) :-
  overloadFunction(Lc,Nm,Tp,Cx,Eqns,Dict,RF).
overloadDef(varDef(Lc,Nm,Cx,Tp,Value),Dict,RD) :-
  overloadDefn(Lc,Nm,Cx,Tp,Value,Dict,RD).
overloadDef(T,_,T) :-
  T = typeDef(_,_,_,_).
overloadDef(C,_,C) :-
  C = cnsDef(_,_,_,_).
overloadDef(C,_,C) :-
  C = conDef(_,_,_).
overloadDef(C,_,C) :-
  C = implDef(_,_,_,_).

overloadFunction(Lc,Nm,Tp,[],Eqns,Dict,funDef(Lc,Nm,Tp,[],REqns)) :-
  overloadEquations(Eqns,Dict,[],REqns).
overloadFunction(Lc,Nm,Tp,Cx,Eqns,Dict,funDef(Lc,Nm,Tp,[],REqns)) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  overloadEquations(Eqns,FDict,CVars,REqns).

overloadEquations(Eqns,Dict,Extra,REqns) :-
  overloadList(Eqns,overloadEquation(Extra),Dict,REqns).

overloadEquation(Extra,equation(Lc,Nm,Args,Cond,Exp),Dict,equation(Lc,Nm,RArgs,RCond,RExp)) :-
  resolveTerm(Args,Dict,RA),
  addExtra(Extra,RA,RArgs),
  resolveTerm(Cond,Dict,RCond),
  resolveTerm(Exp,Dict,RExp).

% These are used when resolving lambdas only. A lambda cannot introduce any dictionary variables
overloadRule(equation(Lc,Nm,Args,Cond,Exp),Dict,equation(Lc,Nm,RArgs,RCond,RExp)) :-
  resolveTerm(Args,Dict,RArgs),
  resolveTerm(Cond,Dict,RCond),
  resolveTerm(Exp,Dict,RExp).

overloadDefn(Lc,Nm,[],Tp,Exp,Dict,varDef(Lc,Nm,[],Tp,RExp)) :-
  resolveTerm(Exp,Dict,RExp).
overloadDefn(Lc,Nm,Cx,Tp,Exp,Dict,
    funDef(Lc,Nm,Tp,[],[equation(Lc,Nm,CVars,enm(Lc,"true"),RExp)])) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  resolveTerm(Exp,FDict,RExp).

defineCVars(_,[],Dict,[],Dict).
defineCVars(Lc,[Con|Cx],Dict,[NV|CVars],FDict) :-
  implementationName(Con,ImplNm),
  genVar(ImplNm,Lc,NV),
  defineCVars(Lc,Cx,[(ImplNm,NV)|Dict],CVars,FDict).
defineCVars(Lc,[implementsFace(_,_)|Cx],Dict,CVars,FDict) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict).

resolveTerm(void,_,void).
resolveTerm(v(Lc,Nm),_,v(Lc,Nm)).
resolveTerm(intLit(Ix),_,intLit(Ix)).
resolveTerm(floatLit(Ix),_,floatLit(Ix)).
resolveTerm(stringLit(Sx),_,stringLit(Sx)).
resolveTerm(dot(Lc,Rc,Fld),Dict,dot(Lc,RRc,Fld)) :- resolveTerm(Rc,Dict,RRc).
resolveTerm(enm(Lc,Rf),_,enm(Lc,Rf)).
resolveTerm(cns(Lc,Rf),_,cns(Lc,Rf)).
resolveTerm(tple(Lc,Args),Dict,tple(Lc,RArgs)) :-
  resolveTerms(Args,Dict,RArgs).
resolveTerm(theta(Lc,Path,Defs,Others,Types,Sig),Dict,theta(Lc,Path,RDefs,ROthers,Types,Sig)) :-
  overload(Defs,Dict,RDict,RDefs),
  overloadOthers(Others,RDict,ROthers).
resolveTerm(record(Lc,Path,Defs,Others,Types,Sig),Dict,record(Lc,Path,RDefs,ROthers,Types,Sig)) :-
  overload(Defs,Dict,RDict,RDefs),
  overloadOthers(Others,RDict,ROthers).
resolveTerm(letExp(Lc,Env,Bound),Dict,letExp(Lc,REnv,RBound)) :-
  resolveTerm(Env,Dict,REnv),
  resolveTerm(Bound,Dict,RBound).
resolveTerm(where(Lc,Trm,Cond),Dict,where(Lc,RTrm,RCond)) :-
  resolveTerm(Trm,Dict,RTrm),
  resolveTerm(Cond,Dict,RCond).
resolveTerm(conj(Lc,L,R),Dict,conj(Lc,RL,RR)) :-
  resolveTerm(L,Dict,RL),
  resolveTerm(R,Dict,RR).
resolveTerm(disj(Lc,L,R),Dict,disj(Lc,RL,RR)) :-
  resolveTerm(L,Dict,RL),
  resolveTerm(R,Dict,RR).
resolveTerm(cond(Lc,T,L,R),Dict,cond(Lc,RT,RL,RR)) :-
  resolveTerm(T,Dict,RT),
  resolveTerm(L,Dict,RL),
  resolveTerm(R,Dict,RR).
resolveTerm(neg(Lc,T),Dict,neg(Lc,RT)) :-
  resolveTerm(T,Dict,RT).
resolveTerm(match(Lc,L,R),Dict,match(Lc,RL,RR)) :-
  resolveTerm(L,Dict,RL),
  resolveTerm(R,Dict,RR).
resolveTerm(apply(ALc,over(Lc,T,Cx),Args),Dict,apply(ALc,OverOp,tple(LcA,NArgs))) :-
  resolveContracts(Lc,Cx,Dict,DTerms),
  resolveTerm(Args,Dict,tple(LcA,RArgs)),
  overloadRef(Lc,T,DTerms,RArgs,OverOp,NArgs).
resolveTerm(apply(Lc,Op,Args),Dict,apply(Lc,ROp,RArgs)) :-
  resolveTerm(Op,Dict,ROp),
  resolveTerm(Args,Dict,RArgs).
resolveTerm(over(Lc,T,Cx),Dict,Over) :-
  ( resolveContracts(Lc,Cx,Dict,DTerms) ->
      overloadRef(Lc,T,DTerms,[],OverOp,NArgs),
      (NArgs=[] -> Over = OverOp ; Over = apply(Lc,OverOp,NArgs)) ;
      reportError("cannot find implementation for contracts %s",[Cx],Lc),
      Over = T).
resolveTerm(mtd(Lc,Nm),_,v(Lc,Nm)) :-
  reportError("cannot find implementation for %s",[Nm],Lc).
resolveTerm(lambda(Lc,Rl,Tp),Dict,lambda(Lc,ORl,Tp)) :-
  overloadRule(Rl,Dict,ORl).

overloadList([],_,_,[]):-!.
overloadList([T|L],C,D,[RT|RL]) :-
  call(C,T,D,RT),
  overloadList(L,C,D,RL).

resolveTerms(L,D,RL) :-
  overloadList(L,resolve:resolveTerm,D,RL).

overloadRef(_,mtd(Lc,Nm,_),[DT],RArgs,dot(Lc,DT,Nm),RArgs).
overloadRef(_,v(Lc,Nm),DT,RArgs,v(Lc,Nm),Args) :- concat(DT,RArgs,Args).

resolveContracts(_,[],_,[]).
resolveContracts(Lc,[Con|C],Dict,[CV|Vs]) :-
  resolveContract(Lc,Con,Dict,CV),
  resolveContracts(Lc,C,Dict,Vs).

resolveContract(Lc,C,Dict,Over) :-
  implementationName(C,ImpNm),
  findImplementation(ImpNm,Dict,Impl),!,
  resolve(Impl,C,ImpNm,Lc,Dict,Over),!.
resolveContract(Lc,C,_,v(Lc,ImpNm)) :-
  implementationName(C,ImpNm),
  reportError("no implementation known for %s",[C],Lc).

resolve(v(Lc,Nm),_,_,_,_,v(Lc,Nm)) :-!.
resolve(I,C,ImpNm,Lc,Dict,Over) :-
  freshen(I,[],_,Con),
  moveConstraints(Con,Cx,contractExists(CT,_)),
  sameContract(CT,C,[]),
  resolveDependents(Cx,Lc,Dict,Args,[]),
  formOver(v(Lc,ImpNm),Args,Lc,Over).
resolve(I,C,_,Lc,_,I) :-
  reportError("cannot resolve contract %s",[C],Lc).

resolveDependents([],_,_,Args,Args).
resolveDependents([C|L],Lc,Dict,[A|As],Args) :-
  resolveContract(Lc,C,Dict,A),
  resolveDependents(L,Lc,Dict,As,Args).

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

overloadOther(ignore(Lc,Show),Dict,ignore(Lc,RShow)) :-
  resolveTerm(Show,Dict,RShow).
overloadOther(assertion(Lc,Cond),Dict,assertion(Lc,RCond)) :-
  resolveTerm(Cond,Dict,RCond).

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
  addExtraDefs(Ex,[varDef(Lc,Nm,[],voidType,v(Lc,Nm))|Els],REls).
