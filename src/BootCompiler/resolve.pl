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
overloadDef(defn(Lc,Nm,Cx,Cond,Tp,Value),Dict,RD) :-
  overloadDefn(Lc,Nm,Cx,Cond,Tp,Value,Dict,RD).
overloadDef(vdefn(Lc,Nm,Cx,Cond,Tp,Value),Dict,RD) :-
  overloadDefn(Lc,Nm,Cx,Cond,Tp,Value,Dict,RD).
overloadDef(grDef(Lc,Nm,Tp,Cx,Rules),Dict,RG) :-
  overloadGrammar(Lc,Nm,Tp,Cx,Rules,Dict,RG).
overloadDef(T,_,T) :-
  T = typeDef(_,_,_,_).
overloadDef(C,_,C) :-
  C = conDef(_,_,_,_).
overloadDef(implDef(Lc,INm,ImplName,Spec,OCx,ThDefs,BodyDefs,Types,Others),Dict,RI) :-
  overloadImplementation(Lc,INm,ImplName,Spec,OCx,ThDefs,BodyDefs,Types,Others,Dict,RI).

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
overloadRule(grRule(Lc,Nm,Args,Cond,Body),Dict,grRule(Lc,Nm,RArgs,RCond,RBody)) :-
  resolveTerm(Args,Dict,RArgs),
  resolveTerm(Cond,Dict,RCond),
  resolveGr(Body,Dict,RBody).

overloadDefn(Lc,Nm,[],Cond,Tp,Exp,Dict,defn(Lc,Nm,[],RCond,Tp,RExp)) :-
  resolveTerm(Cond,Dict,RCond),
  resolveTerm(Exp,Dict,RExp).
overloadDefn(Lc,Nm,Cx,Cond,Tp,Exp,Dict,
    function(Lc,Nm,Tp,[],[equation(Lc,Nm,CVars,RCond,RExp)])) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  resolveTerm(Cond,FDict,RCond),
  resolveTerm(Exp,FDict,RExp).

overloadGrammar(Lc,Nm,Tp,[],Rules,Dict,grDef(Lc,Nm,Tp,[],RRules)) :-
  overloadGrRules(Rules,Dict,[],RRules).
overloadGrammar(Lc,Nm,Tp,Cx,Rules,Dict,grDef(Lc,Nm,Tp,[],RRules)) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  overloadGrRules(Rules,FDict,CVars,RRules).

overloadGrRules(Rules,Dict,Extra,RRules) :-
  overloadList(Rules,overloadGrRule(Extra),Dict,RRules).

overloadGrRule(Extra,grRule(Lc,Nm,Args,Cond,Body),Dict,grRule(Lc,Nm,RArgs,RCond,RBody)) :-
  resolveTerm(Args,Dict,RA),
  addExtra(Extra,RA,RArgs),
  resolveTerm(Cond,Dict,RCond),
  resolveGr(Body,Dict,RBody).

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
resolveTerm(dot(Rc,Fld),Dict,dot(RRc,Fld)) :- resolveTerm(Rc,Dict,RRc).
resolveTerm(enm(Lc,Rf),_,enm(Lc,Rf)).
resolveTerm(cns(Lc,Rf),_,cns(Lc,Rf)).
resolveTerm(tple(Lc,Args),Dict,tple(Lc,RArgs)) :-
  resolveTerms(Args,Dict,RArgs).
resolveTerm(theta(Path,Defs,Others,Types),Dict,theta(Path,RDefs,ROthers,Types)) :-
  overload(Defs,Dict,RDict,RDefs),
  overloadOthers(Others,RDict,ROthers).
resolveTerm(record(Path,Defs,Others,Types),Dict,record(Path,RDefs,ROthers,Types)) :-
  overload(Defs,Dict,RDict,RDefs),
  overloadOthers(Others,RDict,ROthers).
resolveTerm(where(Trm,Cond),Dict,where(RTrm,RCond)) :-
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
resolveTerm(phrase(Lc,T,S,R),Dict,phrase(Lc,RT,RS,RR)) :-
  resolveGr(T,Dict,RT),
  resolveTerm(S,Dict,RS),
  resolveTerm(R,Dict,RR).
resolveTerm(phrase(Lc,T,S),Dict,phrase(Lc,RT,RS)) :-
  resolveGr(T,Dict,RT),
  resolveTerm(S,Dict,RS).
resolveTerm(apply(over(Lc,T,Cx),Args),Dict,apply(OverOp,NArgs)) :-
  resolveContracts(Lc,Cx,Dict,DTerms),
  resolveTerm(Args,Dict,RArgs),
  overloadRef(Lc,T,DTerms,RArgs,OverOp,NArgs).
resolveTerm(apply(Op,Args),Dict,apply(ROp,RArgs)) :-
  resolveTerm(Op,Dict,ROp),
  resolveTerm(Args,Dict,RArgs).
resolveTerm(cons(Lc,O,A),Dict,cons(Lc,OO,OA)) :-
  resolveTerm(O,Dict,OO),
  resolveTerm(A,Dict,OA).
resolveTerm(over(Lc,T,Cx),Dict,Over) :-
  ( resolveContracts(Lc,Cx,Dict,DTerms) ->
      overloadRef(Lc,T,DTerms,[],OverOp,NArgs),
      (NArgs=[] -> Over = OverOp ; Over = apply(OverOp,NArgs)) ;
      reportError("cannot find implementation for contracts %s",[Cx],Lc),
      Over = T).
resolveTerm(mtd(Lc,Nm),_,v(Lc,Nm)) :-
  reportError("cannot find implementation for %s",[Nm],Lc).
resolveTerm(lambda(Rl),Dict,lambda(ORl)) :-
  overloadRule(Rl,Dict,ORl).

overloadList([],_,_,[]):-!.
overloadList([T|L],C,D,[RT|RL]) :-
  call(C,T,D,RT),
  overloadList(L,C,D,RL).

resolveTerms(L,D,RL) :-
  overloadList(L,resolve:resolveTerm,D,RL).

resolveGr(terminals(Lc,Terms),Dict,terminals(Lc,RTerms)) :-
  resolveTerminals(Terms,Dict,RTerms).
resolveGr(eof(Lc,Op),Dict,eof(Lc,ROp)) :-
  resolveTerm(Op,Dict,ROp).
resolveGr(conj(Lc,L,R),Dict,conj(Lc,RL,RR)) :-
  resolveGr(L,Dict,RL),
  resolveGr(R,Dict,RR).
resolveGr(disj(Lc,L,R),Dict,disj(Lc,RL,RR)) :-
  resolveGr(L,Dict,RL),
  resolveGr(R,Dict,RR).
resolveGr(cond(Lc,T,L,R),Dict,cond(Lc,RT,RL,RR)) :-
  resolveGr(T,Dict,RT),
  resolveGr(L,Dict,RL),
  resolveGr(R,Dict,RR).
resolveGr(one(Lc,L),Dict,one(Lc,RL)) :-
  resolveGr(L,Dict,RL).
resolveGr(neg(Lc,L),Dict,neg(Lc,RL)) :-
  resolveGr(L,Dict,RL).
resolveGr(ahead(Lc,L),Dict,ahead(Lc,RL)) :-
  resolveGr(L,Dict,RL).
resolveGr(guard(Lc,L,T),Dict,guard(Lc,RL,RT)) :-
  resolveTerm(T,Dict,RT),
  resolveGr(L,Dict,RL).
resolveGr(goal(Lc,T),Dict,goal(Lc,RT)) :-
  resolveTerm(T,Dict,RT).
resolveGr(call(Lc0,over(Lc,T,Cx),Args),Dict,call(Lc0,OverOp,NArgs)) :-
  resolveTerms(Args,Dict,RArgs),
  resolveContracts(Lc,Cx,Dict,DTerms),
  overloadRef(Lc,T,DTerms,RArgs,OverOp,NArgs).
resolveGr(call(Lc,Op,Args),Dict,call(Lc,ROp,RArgs)) :-
  resolveTerm(Op,Dict,ROp),
  resolveTerms(Args,Dict,RArgs).
resolveGr(call(Lc,P,Args),Dict,call(Lc,RP,RArgs)) :-
  resolveTerm(P,Dict,RP),
  resolveTerms(Args,Dict,RArgs).

resolveTerminals([],_,[]).
resolveTerminals([term(Lc,Op,TT)|L],Dict,[term(Lc,ROp,RTT)|M]) :-
  resolveTerm(Op,Dict,ROp),
  resolveTerm(TT,Dict,RTT),
  resolveTerminals(L,Dict,M).

overloadRef(_,mtd(_,Nm),[DT],RArgs,dot(DT,Nm),RArgs).
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
  freshenConstraint([],_,I,[],Con),
  moveConstraints(Con,Cx,CT),
  sameContract(CT,C,[]),
  resolveDependents(Cx,Lc,Dict,Args,[]),
  formOver(v(Lc,ImpNm),Args,Over).
resolve(I,C,_,Lc,_,I) :-
  reportError("cannot resolve contract %s",[C],Lc).

resolveDependents([],_,_,Args,Args).
resolveDependents([C|L],Lc,Dict,[A|As],Args) :-
  resolveContract(Lc,C,Dict,A),
  resolveDependents(L,Lc,Dict,As,Args).

formOver(V,[],V).
formOver(V,Args,apply(V,Args)).

genVar(Nm,Lc,v(Lc,NV)) :-
  genstr(Nm,NV).

declareImplementations([],Dict,Dict).
declareImplementations([implementation(_,_,ImplName,Spec,_,_,_,_,_)|Defs],Dict,RDict) :-
  declareImplementations(Defs,[(ImplName,Spec)|Dict],RDict).
declareImplementations([_|Defs],Dict,RDict) :-
  declareImplementations(Defs,Dict,RDict).

findImplementation(ImplName,Dict,Spec) :-
  is_member((ImplName,Spec),Dict).

overloadImplementation(Lc,INm,ImplName,Spec,AC,ThDefs,Face,Types,Others,Dict,
    implDef(Lc,INm,ImplName,Arity,Spec,implBody(Lc,Hd,RThDefs,ROthers,Types),faceType(Face,Types))) :-
  defineCVars(Lc,AC,Dict,CVars,FDict),
  overload(ThDefs,FDict,FODict,RThDefs),
  overloadOthers(Others,FODict,ROthers),
  length(CVars,Arity),
  (CVars=[] -> Hd = enum(Lc,ImplName) ; Hd = apply(v(Lc,ImplName),tple(Lc,CVars))).

inheritImplementations([],_,[]).
inheritImplementations([Impl|L],Hd,[rule(Hd,Impl)|M]) :-
  inheritImplementations(L,Hd,M).

overloadOthers(Other,Dict,OOthers) :-
  overloadList(Other,resolve:overloadOther,Dict,OOthers).

overloadOther(show(Lc,Show),Dict,show(Lc,RShow)) :-
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
resolveHead(enum(Lc,Nm),CVars,apply(v(Lc,Nm),CVars)).
resolveHead(apply(v(Lc,Nm),Args),CVars,apply(v(Lc,Nm),OArgs)) :-
  addExtra(CVars,Args,OArgs).
resolveHead(record(Lbl,Defs,[],[]),CVars,record(Lbl,RDefs,[],[])) :-
  addExtraDefs(CVars,Defs,RDefs).
resolveHead(theta(Lbl,Defs,[],[]),CVars,theta(Lbl,RDefs,[],[])) :-
  addExtraDefs(CVars,Defs,RDefs).

addExtra(Extra,tple(Lc,Els),tple(Lc,EEls)) :- concat(Extra,Els,EEls).

addExtraDefs([],Els,Els).
addExtraDefs([v(Lc,Nm)|Ex],Els,REls) :-
  addExtraDefs(Ex,[defn(Lc,Nm,[],v(Lc,"true"),voidType,v(Lc,Nm))|Els],REls).
