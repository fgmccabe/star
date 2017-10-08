:- module(transform,[transformProg/3]).

:- use_module(canon).
:- use_module(transutils).
:- use_module(errors).
:- use_module(types).
:- use_module(debug).
%:- use_module(matcher).
:- use_module(misc).
:- use_module(escapes).
:- use_module(location).
:- use_module(freevars).

transformProg(prog(pkg(Pkg,Vers),Imports,Defs,Others,Fields,Types,Contracts,Impls),
    Opts,export(pkg(Pkg,Vers),Imports,Fields,Types,Classes,Rules,Contracts,Impls)) :-
  makePkgMap(Pkg,Defs,Types,Imports,Classes,Map),
  pushOpt(Opts,pkgName(Pkg),POpts),
  transformModuleDefs(Defs,Pkg,Map,POpts,R1,Rx,Rx,[]),
  transformOthers(Pkg,Map,POpts,Others,Inits,Rules,R0),
  packageInit(Pkg,Map,POpts,Inits,R0,R1).

transformModuleDefs([],_,_,_,Rules,Rules,Ex,Ex).
transformModuleDefs([Def|Defs],Pkg,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformMdlDef(Def,Pkg,Map,Opts,Rules,R0,Ex,Ex1),
  transformModuleDefs(Defs,Pkg,Map,Opts,R0,Rx,Ex1,Exx).

transformMdlDef(function(Lc,Nm,Tp,Cx,Eqns),_,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformFunction(Map,Opts,function(Lc,Nm,Tp,Cx,Eqns),Rules,Rx,Ex,Exx).
transformMdlDef(grammar(Lc,Nm,Tp,Cx,Rls),_,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformGrammar(Map,Opts,grammar(Lc,Nm,Tp,Cx,Rls),_,Rules,Rx,Ex,Exx).
transformMdlDef(defn(Lc,Nm,_,Cond,_,Value),_,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformDefn(Map,Opts,Lc,Nm,Cond,Value,Rules,Rx,Ex,Exx).
transformMdlDef(class(Lc,Nm,Tp,Cx,Defs,Face),_,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformClass(Map,Opts,class(Lc,Nm,Tp,Cx,Defs,Face),Rules,Rx,_,_,Ex,Exx).
transformMdlDef(enum(Lc,Nm,Tp,Cx,Defs,Face),Prefix,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformEnum(Map,Opts,enum(Lc,Nm,Tp,Cx,Defs,Face),Rules,Rx,_,_,Ex,Ex0),
  localName(Prefix,"@",Nm,LclName),
  localName(Prefix,"#",Nm,EnumName),
  Ex0 = [clse([],prg(LclName,1),[enum(EnumName)],[neck])|Exx].
transformMdlDef(typeDef(_,_,_,_),_,_,_,Rules,Rules,Ex,Ex).
transformMdlDef(contract(_,_,_),_,_,_,Rules,Rules,Ex,Ex).
transformMdlDef(impl(Lc,_,ImplName,_,_,_,Body,_,Face),_,Map,Opts,Rules,Rx,Ex,Exx) :-
  transformImplementation(Lc,ImplName,Body,Face,Map,Opts,Rules,Rx,Ex,Exx).

extraArity(Arity,Vars,ExAr) :-
  length(Vars,E),
  ExAr is E+Arity.

transformFunction(Map,Opts,function(Lc,Nm,_,[],Eqns),Rules,Rx,Ex,Exx) :-
  lookupFunName(Map,Nm,Reslt),
  programAccess(Reslt,LclFun,_,_,Arity),
  pushOpt(Opts,inProg(Nm),FOpts),
  extraVars(Map,Extra),
  extraArity(Arity,Extra,Ar),
  LclPrg = prg(LclFun,Ar),
  transformEquations(Map,FOpts,LclPrg,Extra,Eqns,1,_,Rules,R0,Ex,Ex0),
  failSafeEquation(Lc,Nm,LclPrg,Ar,R0,Rx),
  closureEntry(Map,Nm,Ex0,Exx).

transformEquations(_,_,_,_,[],No,No,Rules,Rules,Ex,Ex).
transformEquations(Map,Opts,LclPrg,Extra,[Eqn|Defs],No,Nx,Rules,Rx,Ex,Exx) :-
  transformEqn(Eqn,Map,Opts,LclPrg,Extra,No,Rules,R0,Ex,Ex0),
  N1 is No+1,
  transformEquations(Map,Opts,LclPrg,Extra,Defs,N1,Nx,R0,Rx,Ex0,Exx).

transformEqn(equation(Lc,Nm,A,Cond,Value),Map,Opts,LclPrg,Extra,QNo,[eqn(Lc,Q,LclPrg,Args,Body)|Rx],Rx,Ex,Exx) :-
  debugPreamble(Nm,Extra,Q0,LbLx,FBg,Opts,ClOpts),        % are we debugging?
  trPtns(A,Args,[Rep|Extra],Q0,Q1,PreG,PreGx,PostG,PreV,Map,ClOpts,Ex,Ex0), % head args
  trGoal(Cond,PreGx,[neck|CGx],Q1,Q2,Map,ClOpts,Ex0,Ex1),   % condition goals
  trExp(Value,Rep,Q2,Q3,PreV,PVx,PVx,Px,Map,ClOpts,Ex1,Exx),  % replacement expression
  labelAccess(Q3,Q,Map,Body,LbLx),                        % generate label access goals
  frameDebug(Nm,QNo,FBg,LG,Q,ClOpts),                     % generate frame entry debugging
  lineDebug(Lc,LG,PreG,ClOpts),                           % line debug after setting up frame
  deframeDebug(Nm,QNo,Px,[],ClOpts),                      % generate frame exit debugging
  breakDebug(Nm,CGx,PostG,ClOpts).                         % generate break point debugging

failSafeEquation(Lc,Nm,LclPrg,Arity,[clse([],LclPrg,Anons,G)|Rest],Rest) :-
  genAnons(Arity,Anons),
  genRaise(Lc,strg(Nm),G,[]).

genRaise(Lc,LclName,[raise(cons(strct("error",4),[LclName,intgr(Lno),intgr(Off),intgr(Sz)]))|P],P) :-
  lcLine(Lc,Lno),
  lcColumn(Lc,Off),
  lcSize(Lc,Sz).

transformDefn(Map,Opts,Lc,Nm,Cond,Value,
      [clse(Q,prg(LclName,Arity),[Rep|Extra],Body)|Rx],Rx,Ex,Exx) :-
  lookupVarName(Map,Nm,Reslt),
  programAccess(Reslt,LclName,_,_,_),
  pushOpt(Opts,inProg(Nm),DOpts),
  extraVars(Map,Extra),                                   % extra variables coming from labels
  debugPreamble(Nm,Extra,Q0,G0,G1,DOpts,ClOpts),        % are we debugging?
  trGoal(Cond,G4,[neck|G5],Q0,Q2,Map,ClOpts,Ex,Ex0), % condition goals
  trExp(Value,Rep,Q2,Q3,G5,G6,G6,G7,Map,ClOpts,Ex0,Exx),         % replacement expression
  labelAccess(Q3,Q,Map,Body,G0),                        % generate label access goals
  frameDebug(Nm,0,G1,G2,Q,ClOpts),                       % generate frame entry debugging
  lineDebug(Lc,G2,G3,ClOpts),                         % line debug after setting up frame
  deframeDebug(Nm,0,G7,[],ClOpts),                        % generate frame exit debugging
  length([_|Extra],Arity),
  breakDebug(Nm,G3,G4,ClOpts).                         % generate break point debugging

transformGrammar(Map,Opts,grammar(_,Nm,_,[],Rls),LclFun,Rules,Rx,Ex,Exx) :-
  lookupRelName(Map,Nm,Reslt),
  programAccess(Reslt,LclFun,_,_,Arity),
  extraVars(Map,Extra),
  extraArity(Arity,Extra,Ar),
  LclPrg = prg(LclFun,Ar),
  pushOpt(Opts,inProg(Nm),POpts),
  transformGrammarRules(Map,POpts,LclPrg,Extra,Rls,1,_,Rules,Rx,Ex,Ex0),
  closureEntry(Map,Nm,Ex0,Exx).

transformGrammarRules(_,_,_,_,[],No,No,Rules,Rules,Ex,Ex).
transformGrammarRules(Map,Opts,LclFun,Extra,[Rl|Defs],No,Nx,Rules,Rx,Ex,Exx) :-
  transformGrammarRule(Rl,Map,Opts,LclFun,Extra,No,Rules,R0,Ex,Ex0),
  N1 is No+1,
  transformGrammarRules(Map,Opts,LclFun,Extra,Defs,N1,Nx,R0,Rx,Ex0,Exx).

transformGrammarRule(grammarRule(Lc,Nm,A,PB,Body),Map,Opts,LclFun,Extra,QNo,
      [clse(Q,LclFun,[StIn,StX|Args],Goals)|Rx],Rx,Ex,Exx) :-
  debugPreamble(Nm,Extra,Q0,G0,G1,Opts,ClOpts),        % are we debugging?
  trPtns(A,Args,Extra,Q0,Q1,G4,G5,G6,G7,Map,ClOpts,Ex,Ex0), % head args
  genVar("StIn",StIn),
  dcgBody(Body,G5,G6,StIn,StOut,[StIn|Q1],Q2,Map,ClOpts,Ex0,Ex1), % grammar body
  pushTerminals(PB,G7,G8,StOut,StX,Q2,Q4,Map,ClOpts,Ex1,Exx),                % push back
  labelAccess(Q4,Q,Map,Goals,G0),                       % generate label access goals
  frameDebug(Nm,QNo,G1,G2,Q,ClOpts),                     % generate frame entry debugging
  lineDebug(Lc,G2,G3,ClOpts),                           % line debug after setting up frame
  deframeDebug(Nm,QNo,G8,[],ClOpts),                     % generate frame exit debugging
  breakDebug(Nm,G3,G4,ClOpts).                          % generate break point debugging

dcgBody(terminals(_,Terms),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  pushTerminals(Terms,G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(eof(Lc,StrmVar),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  mkCanon(Lc,Strm,S0),
  trGoal(call(Lc,StrmVar,[S0]),G,G0,Q,Qx,Map,Opts,Ex,Exx),
  joinStream(Strm,Strmx,G0,Gx).
dcgBody(conj(_,Lhs,Rhs),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  dcgBody(Lhs,G,G0,Strm,Strm0,Q,Q0,Map,Opts,Ex,Ex0),
  dcgBody(Rhs,G0,Gx,Strm0,Strmx,Q0,Qx,Map,Opts,Ex0,Exx).
dcgBody(disj(_,Lhs,Rhs),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  dcgDisj(Lhs,Rhs,G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(conditional(_,Tst,Lhs,Rhs),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  dcgConditional(Tst,Lhs,Rhs,G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(one(_,Tst),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  dcgOne(Tst,G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(neg(_,Tst),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  joinStream(Strm,Strmx,G,G0),
  dcgNeg(Tst,G0,Gx,Strm,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(ahead(_,Tst),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  joinStream(Strm,Strmx,G,G0),
  dcgAhead(Tst,G0,Gx,Strm,Q,Qx,Map,Opts,Ex,Exx).
dcgBody(guard(_,Lhs,Rhs),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  dcgBody(Lhs,G,G0,Strm,Strmx,Q,Q0,Map,Opts,Ex,Ex0),
  trGoal(Rhs,G0,Gx,Q0,Qx,Map,Opts,Ex0,Exx).
dcgBody(goal(_,Goal),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  joinStream(Strm,Strmx,G,G0),
  pushStreamVar(Map,Strm,MapG),
  trGoal(Goal,G0,Gx,Q,Qx,MapG,Opts,Ex,Exx).
dcgBody(call(Lc,NT,Args),G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  lineDebug(Lc,G,G0,Opts),
  trExps(Args,AG,[],Q,Q0,G0,Pr,Pr,G3,Map,Opts,Ex,Ex0),
  (var(Strmx) -> genVar("Stx",Strmx) ; true),
  trGoalCall(NT,[Strm,Strmx|AG],G3,Gx,Q0,Qx,Map,Opts,Ex0,Exx).

dcgDisj(Lhs,Rhs,[call(DisProg,[Strm,Strmx|DQ])|G],G,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  (var(Strmx) -> genVar("DjOut",Strmx) ; true),
  genVar("DjStrm",DjStrm),
  dcgBody(Lhs,LG,[],DjStrm,DjStrmx,[],LQ,Map,Opts,Ex,Ex0),
  dcgBody(Rhs,RG,[],DjStrm,DjStrmy,LQ,DQ,Map,Opts,Ex0,Ex1),
  length(DQ,Ar),
  Arity is Ar+2,
  genNewName(Map,"Disj",Arity,DisProg),
  C1 = clse([DjStrm|DQ],DisProg,[DjStrm,DjStrmx|DQ],LG),
  C2 = clse([DjStrm|DQ],DisProg,[DjStrm,DjStrmy|DQ],RG),
  Ex1 = [C1,C2|Exx],
  merge(DQ,Q,Qx).

dcgOne(Lhs,[call(OneProg,[Strm,Strmx|DQ])|G],G,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  genVar("OneStm",OneStm),
  (var(Strmx) -> genVar("DjOut",Strmx) ; true),
  dcgBody(Lhs,LG,[neck],OneStm,OneStmx,[],DQ,Map,Opts,Ex,[C1|Exx]),
  length(DQ,Ar),
  Arity is Ar+2,
  genNewName(Map,"One",Arity,OneProg),
  C1 = clse([OneStm|DQ],OneProg,[OneStm,OneStmx|DQ],LG),
  merge(DQ,Q,Qx).

dcgNeg(Tst,[call(NegProg,[Strm|TQ])|G],G,Strm,Q,Qx,Map,Opts,Ex,Exx) :-
  genVar("NegStrm",NegStrm),
  dcgBody(Tst,TG,[neck,fail],NegStrm,_,[],TQ,Map,Opts,Ex,[C1,C2|Exx]),
  length(TQ,Ar),
  Arity is Ar+1,
  genNewName(Map,"Neg",Arity,NegProg),
  C1 = clse([NegStrm|TQ],NegProg,[NegStrm|TQ],TG),
  C2 = clse([NegStrm|TQ],NegProg,[NegStrm|TQ],[]),
  merge(TQ,Q,Qx).

dcgConditional(Tst,Lhs,Rhs,[call(CondProg,[Strm,Strmx|DQ])|G],G,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  (var(Strmx) -> genVar("CndOut",Strmx) ; true),
  genVar("CondStrm",CndStrm),
  dcgBody(Tst,TG,[neck|LG],CndStrm,CndStrm0,[],TQ,Map,Opts,Ex,Ex0),
  dcgBody(Lhs,LG,[],CndStrm0,CndStrmx,TQ,LQ,Map,Opts,Ex0,Ex1),
  dcgBody(Rhs,RG,[],CndStrm,CndStrmy,LQ,DQ,Map,Opts,Ex1,[C1,C2|Exx]),
  length(DQ,Ar),
  Arity is Ar+2,
  genNewName(Map,"Cond",Arity,CondProg),
  C1 = clse([CndStrm|DQ],CondProg,[CndStrm,CndStrmx|DQ],TG),
  C2 = clse([CndStrm|DQ],CondProg,[CndStrm,CndStrmy|DQ],RG),
  merge(DQ,Q,Qx).

dcgAhead(Tst,[call(HdProg,[Strm|TQ])|G],G,Strm,Q,Qx,Map,Opts,Ex,Exx) :-
  genVar("HedStrm",HedStrm),
  dcgBody(Tst,TG,[],HedStrm,_,[],TQ,Map,Opts,Ex,[C1|Exx]),
  length(TQ,Ar),
  Arity is Ar+1,
  genNewName(Map,"Hed",Arity,HdProg),
  C1 = clse([HedStrm|TQ],HdProg,[HedStrm|TQ],TG),
  merge(TQ,Q,Qx).

pushTerminals([],G,Gx,Strm,Strmx,Q,Q,_,_,Ex,Ex) :-
  joinStream(Strm,Strmx,G,Gx).
pushTerminals([term(Lc,SV,T)|More],G,Gx,Strm,Strmx,Q,Qx,Map,Opts,Ex,Exx) :-
  genVar("NStrm",NStrm),
  mkCanon(Lc,Strm,S0),
  mkCanon(Lc,NStrm,S1),
  trGoal(call(Lc,SV,[S0,T,S1]),G,G0,Q,Q0,Map,Opts,Ex,Ex0),
  pushTerminals(More,G0,Gx,NStrm,Strmx,[NStrm|Q0],Qx,Map,Opts,Ex0,Exx).

mkCanon(Lc,idnt(Nm),v(Lc,Nm)).

joinStream(X,X,G,G).
joinStream(Strm,Strmx,[unify(Strm,Strmx)|Gx],Gx).

transformOthers(_,_,_,[],[neck],Rx,Rx).
transformOthers(Pkg,Map,Opts,[assertion(Lc,G)|Others],[call(AssertName,[])|Inits],Rules,Rx) :-
  collect(Others,canon:isAssertion,Asserts,Rest),
  transformAssertions(Pkg,Map,Opts,Lc,[assertion(Lc,G)|Asserts],AssertName,Rules,R0),
  transformOthers(Pkg,Map,Opts,Rest,Inits,R0,Rx).
transformOthers(Pkg,Map,Opts,[show(Lc,E)|Others],[call(ShowName,[])|Inits],Rules,Rx) :-
  collect(Others,canon:isShow,Shows,Rest),
  transformShows(Pkg,Map,Opts,Lc,[show(Lc,E)|Shows],ShowName,Rules,R0),
  transformOthers(Pkg,Map,Opts,Rest,Inits,R0,Rx).

transformAssertions(Pkg,Map,Opts,Lc,Asserts,prg(LclName,0),Rules,Rx) :-
  rfold(Asserts,transform:collectGoal,true(_),G),
  localName(Pkg,"@","assert",LclName),
  extraVars(Map,Extra),
  transformClause(Map,Opts,prg(LclName,0),Extra,1,clause(Lc,"assert",[],true(''),G),Rules,R0,R0,Rx).

collectGoal(assertion(_,G),true(_),G) :-!.
collectGoal(assertion(_,G),O,conj(O,G)).

transformShows(Pkg,Map,Opts,Lc,Asserts,prg(LclName,0),Rules,Rx) :-
  rfold(Asserts,transform:collectShow,true(_),G),
  localName(Pkg,"@","show",LclName),
  extraVars(Map,Extra),
  transformClause(Map,Opts,prg(LclName,0),Extra,1,clause(Lc,"show",[],true(''),G),Rules,R0,R0,Rx).

collectShow(show(Lc,G),true(_),show(Lc,G)) :-!.
collectShow(show(Lc,G),O,conj(O,show(Lc,G))).

packageInit(Pkg,_,_,Inits,[clse([],prg(InitNm,0),[],Inits)|R],R) :-
  localName(Pkg,"@","init",InitNm).

transformClass(Map,Opts,class(Lc,Nm,_,_,Defs,Face),Rules,Rx,Entry,Entry,Ex,Exx) :-
  labelDefn(Map,Opts,Lc,Nm,LclName,Rules,R0),
  genClassMap(Map,Opts,Lc,LclName,Defs,Face,CMap,R0,En0,Ex,Ex1),!,
  transformClassBody(Defs,CMap,Opts,En1,Rx,En0,En1,Ex1,Exx).

transformEnum(Map,Opts,enum(Lc,Nm,_,_,Defs,Face),Rules,Rx,Entry,Enx,Ex,Exx) :-
  labelDefn(Map,Opts,Lc,Nm,LclName,Entry,Enx),
  genClassMap(Map,Opts,Lc,LclName,Defs,Face,CMap,Rules,En0,Ex,Ex1),!,
  transformClassBody(Defs,CMap,Opts,En1,Rx,En0,En1,Ex1,Exx).

labelDefn(Map,Opts,Lc,Nm,LclName,[clse(Q,prg(Access,ArA),[cons(Con,[LblTerm])|Extra],Body)|Rx],Rx) :-
  lookupVarName(Map,Nm,Spec),
  trCons(Nm,1,Con),
  makeLabelTerm(Spec,Access,LblTerm,LclName),
  pushOpt(Opts,inProg(Nm),DOpts),
  extraVars(Map,Extra),                                   % extra variables coming from labels
  extraArity(1,Extra,ArA),
  debugPreamble(Nm,Extra,Q0,LbLx,FBg,DOpts,ClOpts),       % are we debugging?
  labelAccess(Q0,Q,Map,Body,LbLx),                        % generate label access goals
  frameDebug(Nm,0,FBg,LG,Q,ClOpts),                       % generate frame entry debugging
  lineDebug(Lc,LG,[neck|CGx],ClOpts),                     % line debug after setting up frame
  deframeDebug(Nm,0,Px,[],ClOpts),                        % generate frame exit debugging
  breakDebug(Nm,CGx,Px,ClOpts).                           % generate break point debugging

makeLabelTerm(localClass(LclName,Strct,LblPrg,LblVr,ThVr),prg(LclName,3),cons(Strct,[LblVr,ThVr]),LblPrg).
makeLabelTerm(moduleClass(Access,Strct,0),Access,enum(Strct),Access).
makeLabelTerm(moduleClass(Access,Strct,Ar),Access,cons(strct(Strct,Ar),[]),Access).
makeLabelTerm(moduleImpl(Access,Strct),Access,Strct,Strct).

findClassBody(Defs,Stmts) :-
  is_member(classBody(_,_,_,Stmts,_,_),Defs),!.
findClassBody(Defs,Stmts) :-
  is_member(implBody(_,_,Stmts,_,_),Defs),!.

/* A class body of the form
lbl(A1,..,Ak){
  prog1 :- ...
}
is mapped to

pkg#lbl(prog1(X1,..,Xm),Lb,Th) :- pkg#lbl@prog(X1,..,Xm,Lb,Th)

together with the specific translations of prog1
*/
transformClassBody(Defs,Map,Opts,Rules,Rx,Entry,Enx,Ex,Exx) :-
  findClassBody(Defs,Stmts),!,
  transformClassDefs(Map,Opts,Stmts,Rules,Rx,Entry,Enx,Ex,Exx).
transformClassBody(_,_,_,Rules,Rules,Entry,Entry,Ex,Ex).

transformClassDefs(_,_,[],Rules,Rules,Entry,Entry,Extra,Extra).
transformClassDefs(Map,Opts,[Def|Defs],Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformClassDef(Map,Opts,Def,Rules,R0,Entry,En0,Ex,Ex1),
  transformClassDefs(Map,Opts,Defs,R0,Rx,En0,Enx,Ex1,Exx).

transformClassDef(Map,Opts,function(Lc,Nm,Tp,Cx,Eqns),Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformFunction(Map,Opts,function(Lc,Nm,Tp,Cx,Eqns),Rules,Rx,Ex,Ex0),
  entryClause(Map,Nm,Entry,En0),
  closureAccess(Map,Nm,En0,Enx),
  closureEntry(Map,Nm,Ex0,Exx).
transformClassDef(Map,Opts,predicate(Lc,Nm,Tp,Cx,Clses),Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformPredicate(Map,Opts,predicate(Lc,Nm,Tp,Cx,Clses),Rules,Rx,Ex,Ex0),
  entryClause(Map,Nm,Entry,En0),
  closureAccess(Map,Nm,En0,Enx),
  closureEntry(Map,Nm,Ex0,Exx).
transformClassDef(Map,Opts,defn(Lc,Nm,_,Cond,_,Value),Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformDefn(Map,Opts,Lc,Nm,Cond,Value,Rules,Rx,Ex,Exx),
  entryClause(Map,Nm,Entry,Enx).
transformClassDef(Map,Opts,class(Lc,Nm,Tp,Cx,Defs,Face),Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformClass(Map,Opts,class(Lc,Nm,Tp,Cx,Defs,Face),Rules,Rx,Entry,E0,Ex,Exx),
  entryClause(Map,Nm,E0,Enx).
transformClassDef(Map,Opts,enum(Lc,Nm,Tp,Cx,Defs,Face),Rules,Rx,Entry,Enx,Ex,Exx) :-
  transformEnum(Map,Opts,enum(Lc,Nm,Tp,Cx,Defs,Face),Rules,Rx,Entry,E0,Ex,Exx),
  entryClause(Map,Nm,E0,Enx).

entryClause(Map,Name,[clse(Q,prg(Prefix,3),
      [cons(Con,Args),LblVr,ThVr],[neck,call(prg(Prog,Ar2),Q)])|Rx],Rx) :-
  layerName(Map,Prefix),
  lookupVarName(Map,Name,Reslt),
  programAccess(Reslt,Prog,_,_,Arity),
  genVars(Arity,Args),
  genVar("This",ThVr),
  genVar("Lbl",LblVr),
  concat(Args,[LblVr,ThVr],Q),
  trCons(Name,Arity,Con),
  Ar2 is Arity+2.

closureAccess(Map,Name,[clse([LblVr,ThVr],prg(Prefix,3),[LamCons,LblVr,ThVr],[])|Rx],Rx) :-
  layerName(Map,Prefix),
  lookupVarName(Map,Name,Reslt),
  programAccess(Reslt,_,_,Closure,_),
  genVar("This",ThVr),
  genVar("Lbl",LblVr),
  trCons(Name,1,ClAcc),
  LamCons = cons(ClAcc,[cons(strct(Closure,2),[LblVr,ThVr])]).

closureEntry(Map,Name,[clse(Q,prg(Closure,3),[CallStrct,ClosureCons,anon],[call(prg(Prog,ArX),Q)])|L],L) :-
  lookupVarName(Map,Name,Reslt),
  programAccess(Reslt,Prog,_,Closure,Arity),
  extraVars(Map,Extra),
  genVars(Arity,Args),
  concat(Args,Extra,Q),
  trCons("_call",Arity,Con),
  CallStrct = cons(Con,Args),
  length(Extra,ExAr),
  (Extra=[] -> ClosureCons = enum(Closure) | ClosureCons = cons(strct(Closure,ExAr),Extra)),
  length(Q,ArX).

transformImplementation(Lc,ImplName,Def,Face,Map,Opts,Rules,Rx,Ex,Exx) :-
  labelDefn(Map,Opts,Lc,ImplName,_,Rules,R0),
  genClassMap(Map,Opts,Lc,ImplName,[Def],Face,CMap,R0,En0,Ex,Ex1),!,
  transformClassBody([Def],CMap,Opts,En1,Rx,En0,En1,Ex1,Exx).

trPtns([],Args,Args,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtns([P|More],[A|Args],Ax,Q,Qx,Pre,Prx,Post,Psx,Map,Opts,Ex,Exx) :-
  trPtn(P,A,Q,Q0,Pre,Pre0,Post,Pst0,Map,Opts,Ex,Ex0),
  trPtns(More,Args,Ax,Q0,Qx,Pre0,Prx,Pst0,Psx,Map,Opts,Ex0,Exx).

trPtn(v(_,"this"),ThVr,Q,Qx,Pre,Pre,Post,Post,Map,_,Ex,Ex) :-
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
trPtn(v(Lc,"this"),idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :- !,
  reportError("'this' not defined here",[],Lc).
trPtn(v(Lc,Nm),A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts).
trPtn(enum(Lc,Nm),A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts,Ex,Ex) :- !,
  trVarPtn(Lc,Nm,A,Q,Qx,Pre,Prx,Post,Pstx,Map,Opts).
trPtn(intLit(Ix),intgr(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtn(floatLit(Ix),float(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtn(stringLit(Ix),strg(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trPtn(dot(Rc,Fld),Exp,Q,Qx,Pre,Px,Post,Post,Map,Opts,Ex,Exx) :-
  genVar("XV",X),
  trCons(Fld,[X],S),
  C = cons(S,[X]),
  trDotExp(Rc,C,X,Exp,Q,Qx,Pre,Pi,Pi,Px,Map,Opts,Ex,Exx).
trPtn(tuple(_,Ptns),tpl(P),Q,Qx,Pre,Px,Post,Postx,Map,Opts,Ex,Exx) :-
  trPtns(Ptns,P,[],Q,Qx,Pre,Px,Post,Postx,Map,Opts,Ex,Exx).
trPtn(apply(v(_,Nm),A),Ptn,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trPtns(A,Args,[],Q,Q0,APre,AP0,APost,APs0,Map,Opts,Ex,Ex0),
  trPtnCallOp(Nm,Args,Ptn,Q0,Qx,APre,AP0,APost,APs0,Pre,Px,Post,Pstx,Map,Opts,Ex0,Exx).
trPtn(where(P,C),Ptn,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trPtn(P,Ptn,Q,Q0,Pre,P0,Post,Pstx,Map,Opts,Ex,Ex0),
  trGoal(C,P0,Px,Q0,Qx,Map,Opts,Ex0,Exx).
trPtn(XX,Exp,Q,Qx,Pre,Pre,Post,Postx,Map,Opts,Ex,Exx) :-
  trExp(XX,Exp,Q,Qx,Post,Pi,Pi,Postx,Map,Opts,Ex,Exx).

trVarPtn(_,"_",idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_).
trVarPtn(_,Nm,A,Q,Qx,Pre,Prx,Post,Pstx,Map,_) :-
  lookupVarName(Map,Nm,V),!,
  genVar("X",X),
  implementVarPtn(V,Nm,X,A,Q,Qx,Pre,Prx,Post,Pstx).
trVarPtn(Lc,Nm,idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_) :-
  reportError("'%s' not defined",[Nm],Lc).

implementVarPtn(localVar(Vn,_,ClVr,TVr),_,X,X,Q,Qx,[call(Vn,[X,ClVr,TVr])|Pre],Pre,Post,Post) :- !, % instance var
  merge([X,ClVr,TVr],Q,Qx).
implementVarPtn(moduleVar(_,Vn,_),_,X,X,Q,[X|Q],[call(prg(Vn,1),[X])|Pre],Pre,Post,Post) :- !. % module variable
implementVarPtn(labelArg(N,ClVr,TVr),_,_,N,Q,Qx,Pre,Pre,Post,Post) :- !,    % argument from label
  merge([N,ClVr,TVr],Q,Qx).
implementVarPtn(moduleClass(Enum,_,0),_,_,enum(Enum),Q,Q,Pre,Pre,Post,Post).
implementVarPtn(localClass(Enum,_,_,LbVr,ThVr),_,_,cons(Enum,[LbVr,ThVr]),Q,Qx,Pre,Pre,Post,Post) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarPtn(inherit(Nm,LbVr,ThVr),_,_,cons(strct(Nm,2),[LbVr,ThVr]),Q,Qx,Pre,Pre,Post,Post) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarPtn(notInMap,Nm,_,idnt(Nm),Q,Qx,Pre,Pre,Post,Post) :-                 % variable local to rule
  merge([idnt(Nm)],Q,Qx).

trPtnCallOp(Nm,Args,X,Q,Qx,Pre,Px,Tail,[ecall(Nm,XArgs)|Tailx],Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  isEscape(Nm,_),!,
  genVar("X",X),
  concat(Args,[X],XArgs),
  merge([X],Q,Qx).
trPtnCallOp(Nm,Args,Ptn,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,_,Ex,Ex) :-
  lookupFunName(Map,Nm,Reslt),
  genVar("X",X),
  implementPtnCall(Reslt,Nm,X,Args,Ptn,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx).

implementPtnCall(localFun(Fn,_,_,Ar,LblVr,ThVr),_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(prg(Fn,A2),XArgs)|Tailx],Pre,Px,Tail,Tailx) :-
  concat(Args,[X,LblVr,ThVr],XArgs),
  merge([X,LblVr,ThVr],Q,Qx),
  A2 is Ar+2.
implementPtnCall(moduleFun(_,Fn,_,_,Ar),_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(prg(Fn,Ar),XArgs)|Tailx],Pre,Px,Tail,Tailx) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Qx).
implementPtnCall(inheritField(Super,LblVr,ThVr),Nm,X,Args,X,Q,Qx,Pre,Px,Tail,
      [call(Super,[cons(Op,XArgs),LblVr,ThVr])|Tailx],Pre,Px,Tail,Tailx):-
  concat(Args,[X],XArgs),
  trCons(Nm,XArgs,Op),
  merge([X,LblVr,ThVr],Q,Qx).
implementPtnCall(moduleClass(Mdl,_,Ar),_,_,Args,cons(strct(Mdl,Ar),Args),Q,Q,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx).
implementPtnCall(localClass(Mdl,_,_,LbVr,ThVr),_,_,Args,
      cons(Mdl,XArgs),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx) :-
  concat(Args,[LbVr,ThVr],XArgs),
  merge([LbVr,ThVr],Q,Qx).
implementPtnCall(inherit(Nm,_,LbVr,ThVr),_,_,Args,
      cons(strct(Nm,Ar),Args),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx) :-
  merge([LbVr,ThVr],Q,Qx),
  length(Args,Ar).
implementPtnCall(moduleImpl(_,Mdl),_,_,Args,cons(Mdl,Args),Q,Q,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx).

trEntryPtrns([],_,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex).
trEntryPtrns([(Ky,Vl)|R],Xi,Q,Qx,Pre,Px,Post,Postx,Map,Opts,Ex,Exx):-
  trExp(Ky,KyExp,Q,Q0,Pre,Pre0,Post,Pst0,Map,Opts,Ex,Ex0),
  trPtn(Vl,VlPtn,Q0,Q1,Pre0,Pre1,Pst0,[ocall(cons(Op,XArgs),Xi,Xi)|Pst1],Map,Opts,Ex0,Ex1),
  XArgs = [KyExp,VlPtn],
  trCons("present",XArgs,Op),
  trEntryPtrns(R,Xi,Q1,Qx,Pre1,Px,Pst1,Postx,Map,Opts,Ex1,Exx).

trExps([],Args,Args,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trExps([P|More],[A|Args],Extra,Q,Qx,Pre,Prx,Post,Psx,Map,Opts,Ex,Exx) :-
  trExp(P,A,Q,Q0,Pre,Pre0,Post,Pst0,Map,Opts,Ex,Ex0),
  trExps(More,Args,Extra,Q0,Qx,Pre0,Prx,Pst0,Psx,Map,Opts,Ex0,Exx).

trExp(v(_,"this"),ThVr,Q,Qx,Pre,Pre,Post,Post,Map,_,Ex,Ex) :-
  thisVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
trExp(v(_,"stream"),ThVr,Q,Qx,Pre,Pre,Post,Post,Map,_,Ex,Ex) :-
  streamVar(Map,ThVr),!,
  merge([ThVr],Q,Qx).
trExp(v(Lc,Nm),Vr,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Ex) :-
  trVarExp(Lc,Nm,Vr,Q,Qx,Pre,Px,Post,Pstx,Map,Opts).
trExp(intLit(Ix),intgr(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trExp(floatLit(Ix),float(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trExp(stringLit(Ix),strg(Ix),Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-!.
trExp(tuple(_,A),tpl(TA),Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trExps(A,TA,[],Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx).
trExp(apply(Op,A),Exp,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trExps(A,Args,[],Q,Q0,APre,APx,APost,APostx,Map,Opts,Ex,Ex0),
  genVar("X",X),
  trExpCallOp(Op,X,Args,Exp,Q0,Qx,APre,APx,APost,APostx,Pre,Px,Post,Pstx,Map,Opts,Ex0,Exx).
trExp(dot(Rec,Fld),Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  genVar("XV",X),
  trCons(Fld,[X],S),
  C = cons(S,[X]),
  trDotExp(Rec,C,X,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).
trExp(where(P,C),Ptn,Q,Qx,Pre,Px,Post,Pstx,Map,Opts,Ex,Exx) :-
  trExp(P,Ptn,Q,Q0,Pre,P0,Post,Pstx,Map,Opts,Ex,Ex0),
  trGoal(C,P0,Px,Q0,Qx,Map,Opts,Ex0,Exx).
trExp(conditional(Lc,T,L,R),Rslt,Q,Qx,Pre,Prx,Post,Post,Map,Opts,Ex,Exx) :- !,
  genVar("CndV",Rslt),
  lineDebug(Lc,Pre,[call(CondPr,[Rslt|LQ])|Prx],Opts),
  trGoal(T,TG,[neck|LG],[],Q0,Map,Opts,Ex,Ex0),
  trExp(L,LRslt,Q0,Q1,LG,Lx,Lx,[],Map,Opts,Ex0,Ex1),
  trExp(R,RRslt,Q1,LQ,RG,Rx,Rx,[],Map,Opts,Ex1,Ex2),
  length(LQ,QAr),
  genNewName(Map,"condExp",QAr,CondPr),
  Cl1 = clse(LQ,CondPr,[LRslt|LQ],TG),
  Cl2 = clse(LQ,CondPr,[RRslt|LQ],RG),
  Ex2 = [Cl1,Cl2|Exx],
  merge([Rslt|LQ],Q,Qx).
trExp(lambda(Rl),Rslt,Q,Q,Pr,Pr,Post,Post,Map,Opts,Ex,Exx) :-
  trLambdaRule(Rl,Rslt,Q,Map,Opts,Ex,Exx).
trExp(XX,void,Q,Q,Pre,Pre,Post,Post,_,_,Ex,Ex) :-
  reportMsg("internal: cannot transform %s as expression",[XX]).

trDotExp(v(Lc,Nm),C,X,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  lookupVarName(Map,Nm,Reslt),
  implementDotExp(Reslt,v(Lc,Nm),C,X,Exp,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).
trDotExp(R,C,X,X,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(R,Rc,Q,Q0,Pre,Px,Tail,[ocall(C,Rc,Rc)|Tailx],Map,Opts,Ex,Exx),
  merge([X],Q0,Qx).

implementDotExp(inherit(_,Super,ClVr,ThVr),_,C,X,X,Q,Qx,Pre,Pre,[call(Super,[C,ClVr,ThVr])|Tail],Tail,_,_,Ex,Ex) :-
  merge([X,ClVr,ThVr],Q,Qx).
implementDotExp(moduleClass(_,Lbl,_),R,C,X,X,Q,Qx,Pre,Prx,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(R,Rc,Q,Q0,Pre,Prx,Tail,[call(prg(Lbl,3),[C,Rc,Rc])|Tailx],Map,Opts,Ex,Exx),
  merge([X],Q0,Qx).
implementDotExp(_,R,C,X,X,Q,Qx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(R,Rc,Q,Q0,Pre,Px,Tail,[ocall(C,Rc,Rc)|Tailx],Map,Opts,Ex,Exx),
  merge([X],Q0,Qx).

trVarExp(_,"_",idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_).
trVarExp(Lc,Nm,Exp,Q,Qx,Pre,Prx,Post,Pstx,Map,_) :-
  lookupVarName(Map,Nm,V),!,
  genVar("X",X),
  implementVarExp(V,Lc,Nm,X,Exp,Q,Qx,Pre,Prx,Post,Pstx).
trVarExp(Lc,Nm,idnt("_"),Q,Q,Pre,Pre,Post,Post,_,_) :-
  reportError("'%s' not defined",[Nm],Lc).

implementVarExp(localVar(Vn,_,LblVr,ThVr),_,_,X,X,Q,Qx,[call(prg(Vn,3),[X,LblVr,ThVr])|Pre],Pre,Tail,Tail) :-
  merge([X,LblVr,ThVr],Q,Qx).
implementVarExp(moduleVar(_,V,_),_,_,X,X,Q,Qx,[call(prg(V,1),[X])|Pre],Pre,Tail,Tail) :-
  merge([X],Q,Qx).
implementVarExp(labelArg(N,LblVr,ThVar),_,_,_,N,Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([N,LblVr,ThVar],Q,Qx).
implementVarExp(inheritField(Super,LblVr,ThVr),_,Nm,X,X,Q,Qx,
      [call(Super,[cons(V,[X]),LblVr,ThVr])|Pre],Pre,Tail,Tail) :-
  trCons(Nm,1,V),
  merge([X,LblVr,ThVr],Q,Qx).
implementVarExp(moduleClass(Enum,_,0),_,_,_,enum(Enum),Q,Q,Pre,Pre,Tail,Tail).
implementVarExp(moduleImpl(_,enum(Enum)),_,_,_,enum(Enum),Q,Q,Pre,Pre,Tail,Tail).
implementVarExp(localClass(Enum,_,_,LbVr,ThVr),_,_,_,cons(Enum,[LbVr,ThVr]),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarExp(inherit(Nm,_,LbVr,ThVr),_,_,_,cons(strct(Nm,2),[LbVr,ThVr]),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([LbVr,ThVr],Q,Qx).
implementVarExp(notInMap,_,Nm,_,idnt(Nm),Q,Qx,Pre,Pre,Tail,Tail) :-
  merge([idnt(Nm)],Q,Qx).
implementVarExp(moduleFun(_,_,_,Acc,_),_,_,_,enum(Acc),Q,Q,Pre,Pre,Tail,Tail).
implementVarExp(moduleRel(_,_,_,Acc,_),_,_,_,enum(Acc),Q,Q,Pre,Pre,Tail,Tail).
implementVarExp(localFun(_,_,Closure,_,LblVr,ThVr),_,_,_,cons(strct(Closure,2),[LblVr,ThVr]),Q,Q,Pre,Pre,Tail,Tail).
implementVarExp(_Other,Lc,Nm,_,idnt(Nm),Q,Q,Pre,Pre,Tail,Tail) :-
  reportError("cannot handle %s in expression",[Nm],Lc).

trExpCallOp(v(_,Nm),X,Args,X,Q,Qx,Pre,Px,Tail,[ecall(Nm,XArgs)|Tailx],Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Qx),
  isEscape(Nm,_),!.
trExpCallOp(v(Lc,Nm),X,Args,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  lookupFunName(Map,Nm,Reslt),
  implementFunCall(Reslt,Lc,Nm,X,Args,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).
trExpCallOp(dot(Rec,Fld),X,Args,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Q1),
  trCons(Fld,XArgs,Op),
  C = cons(Op,XArgs),
  trExpCallDot(Rec,Rec,C,X,Exp,Q1,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).

trExpCallDot(v(_,Nm),Rec,C,X,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  lookupFunName(Map,Nm,Reslt),
  implementDotFunCall(Reslt,Rec,C,X,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).
trExpCallDot(_,Rec,C,X,X,Q,Qx,Pre,APx,Tail,[ocall(C,Rc,Rc)|ATlx],Pre,Px,Tail,ATlx,Map,Opts,Ex,Exx) :-
  trExp(Rec,Rc,Q,Qx,APx,Rx,Rx,Px,Map,Opts,Ex,Exx).

implementDotFunCall(inherit(_,Super,LblVr,ThVr),_,C,X,X,Q,Qx,Pre,Px,Tail,
      [call(Super,[C,LblVr,ThVr])|Tailx],Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  merge([X,LblVr,ThVr],Q,Qx).
implementDotFunCall(_,Rec,C,X,X,Q,Qx,Pre,APx,Tail,[ocall(C,Rc,Rc)|Tailx],Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExp(Rec,Rc,Q,Q0,APx,Rx,Rx,Px,Map,Opts,Ex,Exx),
  merge([X],Q0,Qx).

implementFunCall(localFun(Fn,_,_,Ar,LblVr,ThVr),_,_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(prg(Fn,Ar2),XArgs)|Tailx],Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  concat(Args,[X,LblVr,ThVr],XArgs),
  merge([X,LblVr,ThVr],Q,Qx),
  Ar2 is Ar+2.
implementFunCall(moduleFun(_,Fn,_,Ar,_),_,_,X,Args,X,Q,Qx,Pre,Px,Tail,[call(prg(Fn,Ar),XArgs)|Tailx],Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  concat(Args,[X],XArgs),
  merge([X],Q,Qx).
implementFunCall(inheritField(Super,LblVr,ThVr),_,Nm,X,Args,X,Q,Qx,Pre,Px,Tail,
      [call(Super,[cons(Op,XArgs),LblVr,ThVr])|Tailx],Pre,Px,Tail,Tailx,_,_,Ex,Ex):-
  concat(Args,[X],XArgs),
  trCons(Nm,XArgs,Op),
  merge([X,LblVr,ThVr],Q,Qx).
implementFunCall(moduleClass(Mdl,_,Ar),_,_,_,Args,cons(strct(Mdl,Ar),Args),Q,Q,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx,_,_,Ex,Ex).
implementFunCall(localClass(Mdl,_,_,Ar,LbVr,ThVr),_,_,_,Args,cons(strct(Mdl,Ar2),XArgs),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  concat(Args,[LbVr,ThVr],XArgs),
  merge([LbVr,ThVr],Q,Qx),
  Ar2 is Ar+2.
implementFunCall(inherit(Mdl,_,LbVr,ThVr),_,_,_,Args,
      cons(strct(Mdl,Ar),Args),Q,Qx,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx,_,_,Ex,Ex) :-
  merge([LbVr,ThVr],Q,Qx),
  length(Args,Ar).
implementFunCall(moduleImpl(_,Mdl),_,_,_,Args,cons(Mdl,Args),Q,Q,Pre,Px,Tail,Tailx,Pre,Px,Tail,Tailx,_,_,Ex,Ex).
implementFunCall(notInMap,Lc,Nm,X,Args,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx) :-
  trExpCallOp(dot(v(Lc,Nm),"_call"),X,Args,Exp,Q,Qx,APre,APx,APost,APstx,Pre,Px,Tail,Tailx,Map,Opts,Ex,Exx).

% We build $$(_call(Args<>Rep),$$(Free),_) :- Cond, !, replacement
trLambdaRule(equation(Lc,Nm,A,Cond,Exp),Closure,Q,Map,Opts,Ex,Exx) :-
  freeVarsInRule(equation(Lc,Nm,A,Cond,Exp),Q,[],FreeVars),
  trPtns(A,Args,[Rep],[],Q1,Goals,PreGx,PostG,[],Map,Opts,Ex,Ex0), % head args
  trGoal(Cond,PreGx,[neck|PostGx],Q1,Q2,Map,Opts,Ex0,Ex1),   % condition goals
  trExp(Exp,Rep,Q2,Q3,PostGx,PVx,PVx,PostG,Map,Opts,Ex1,Ex2),  % replacement expression
  length(Args,Ar),
  trCons("_call",Ar,Con),
  CallStrct = cons(Con,Args),
  genNewName(Map,Nm,Ar,prg(Lam,_)),
  mkClosure(Lam,FreeVars,Closure),
  merge(FreeVars,Q3,QQ),
  LclPrg = prg(Lam,3),
  Ex2 = [clse(QQ,LclPrg,[CallStrct,Closure,anon],Goals)|Ex3],
  failSafeEquation(Lc,"lambda",LclPrg,3,Ex3,Exx).
trLambdaRule(clause(Lc,Nm,A,Cond,Body),Closure,Q,Map,Opts,Ex,Exx) :-
  freeVarsInRule(clause(Lc,Nm,A,Cond,Body),Q,[],FreeVars),
  trPtns(A,Args,[],[],Q1,Goals,PreGx,PostG,[],Map,Opts,Ex,Ex0), % head args
  trGoal(Cond,PreGx,PostC,Q1,Q2,Map,Opts,Ex0,Ex1),   % condition goals
  trGoal(Body,PostC,PostG,Q2,Q3,Map,Opts,Ex1,Ex2),
  length(Args,Ar),
  trCons("_call",Ar,Con),
  CallStrct = cons(Con,Args),
  genNewName(Map,Nm,Ar,prg(Lam,_)),
  mkClosure(Lam,FreeVars,Closure),
  merge(FreeVars,Q3,QQ),
  LclPrg = prg(Lam,3),
  Ex2 = [clse(QQ,LclPrg,[CallStrct,Closure,anon],Goals)|Exx].


mkClosure(Lam,FreeVars,Closure) :-
  length(FreeVars,Ar),
  (Ar = 0 ->
    Closure=enum(Lam) |
    Closure=cons(strct(Lam,Ar),FreeVars)).

trGoal(true(_),Goals,Goals,Q,Q,_,_,Ex,Ex) :-!.
trGoal(false(_),[fail|Rest],Rest,Q,Q,_,_,Ex,Ex) :- !.
trGoal(conj(L,R),Goals,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  trGoal(L,Goals,G0,Q,Q0,Map,Opts,Ex,Ex0),
  trGoal(R,G0,Gx,Q0,Qx,Map,Opts,Ex0,Exx).
trGoal(disj(Lc,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G,[call(DisjPr,LQ)|Gx],Opts),
  trGoal(L,LG,[],[],Q0,Map,Opts,Ex,Ex0),
  trGoal(R,RG,[],Q0,LQ,Map,Opts,Ex0,Ex1),
  length(LQ,QAr),
  genNewName(Map,"or",QAr,DisjPr),
  Cl1 = clse(LQ,DisjPr,LQ,LG),
  Cl2 = clse(LQ,DisjPr,LQ,RG),
  Ex1 = [Cl1,Cl2|Exx],
  merge(LQ,Q,Qx).
trGoal(conditional(Lc,T,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G,[call(CondPr,LQ)|Gx],Opts),
  trGoal(T,TG,[neck|LG],[],Q0,Map,Opts,Ex,Ex0),
  trGoal(L,LG,[],Q0,Q1,Map,Opts,Ex0,Ex1),
  trGoal(R,RG,[],Q1,LQ,Map,Opts,Ex1,Ex2),
  length(LQ,QAr),
  genNewName(Map,"cond",QAr,CondPr),
  Cl1 = clse(LQ,CondPr,LQ,TG),
  Cl2 = clse(LQ,CondPr,LQ,RG),
  Ex2 = [Cl1,Cl2|Exx],
  merge(LQ,Q,Qx).
trGoal(one(Lc,T),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G,[call(OnePr,LQ)|Gx],Opts),
  trGoal(T,TG,[neck],[],LQ,Map,Opts,Ex,Ex0),
  length(LQ,QAr),
  genNewName(Map,"one",QAr,OnePr),
  Cl1 = clse(LQ,OnePr,LQ,TG),
  Ex0 = [Cl1|Exx],
  merge(LQ,Q,Qx).
trGoal(neg(Lc,T),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G,[call(NegPr,LQ)|Gx],Opts),
  trGoal(T,TG,[neck,fail],[],LQ,Map,Opts,Ex,Ex0),
  length(LQ,QAr),
  genNewName(Map,"neg",QAr,NegPr),
  Cl1 = clse(LQ,NegPr,LQ,TG),
  Cl2 = clse(LQ,NegPr,LQ,[]),
  Ex0 = [Cl1,Cl2|Exx],
  merge(LQ,Q,Qx).
trGoal(forall(Lc,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G,[call(APr,LQ)|Gx],Opts),
  trGoal(L,LG,[call(BPr,LQ),neck,fail],[],Q0,Map,Opts,Ex,Ex0),
  trGoal(R,RG,[neck,fail],Q0,LQ,Map,Opts,Ex0,Ex1),
  length(LQ,QAr),
  genNewName(Map,"forallA",QAr,APr),
  genNewName(Map,"forallB",QAr,BPr),
  ACl1 = clse(LQ,APr,LQ,LG),
  ACl2 = clse(LQ,APr,LQ,[]),
  BCl1 = clse(LQ,BPr,LQ,RG),
  BCl2 = clse(LQ,BPr,LQ,[]),
  Ex1 = [ACl1,ACl2,BCl1,BCl2|Exx],
  merge(LQ,Q,Qx).
trGoal(match(Lc,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G3,[match(Lx,Rx)|Gx],Opts),
  trPtn(L,Lx,Q,Q0,G,G0,G0,G1,Map,Opts,Ex,Ex0),
  trExp(R,Rx,Q0,Qx,G1,G2,G2,G3,Map,Opts,Ex0,Exx).
trGoal(unify(Lc,L,R),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :- !,
  lineDebug(Lc,G3,[unify(Lx,Rx)|Gx],Opts),
  trExp(L,Lx,Q,Q0,G,G0,G0,G1,Map,Opts,Ex,Ex0),
  trExp(R,Rx,Q0,Qx,G1,G2,G2,G3,Map,Opts,Ex0,Exx).
trGoal(phrase(_,NT,Strm,Rem),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trExp(Strm,StIn,Q,Q0,G,G0,G0,G1,Map,Opts,Ex,Ex0),
  dcgBody(NT,G1,G2,StIn,StOut,[StIn|Q0],Q2,Map,Opts,Ex0,Ex1), % grammar body
  trExp(Rem,Out,Q2,Qx,G2,G3,G3,G4,Map,Opts,Ex1,Exx),
  joinStream(Out,StOut,G4,Gx).
trGoal(phrase(_,NT,Strm),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trExp(Strm,StIn,Q,Q0,G,G0,G0,G1,Map,Opts,Ex,Ex0),
  dcgBody(NT,G1,Gx,StIn,_,[StIn|Q0],Qx,Map,Opts,Ex0,Exx).
trGoal(show(Lc,Exp),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trLocation(Lc,Loc,G,G0,Q,Q0,Map,Opts,Ex,Ex0),
  trExp(Exp,Trm,Q0,Qx,G0,G1,G1,[ecall("_display",[Loc,Trm])|Gx],Map,Opts,Ex0,Exx).
trGoal(call(Lc,Pred,Args),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  lineDebug(Lc,G,G0,Opts),
  trExps(Args,AG,[],Q,Q0,G0,Pr,Pr,G3,Map,Opts,Ex,Ex0),
  trGoalCall(Pred,AG,G3,Gx,Q0,Qx,Map,Opts,Ex0,Exx).
trGoal(isTrue(Lc,E),G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  lineDebug(Lc,G1,[isTru(Exp)|Gx],Opts),
  trExp(E,Exp,Q,Qx,G,G0,G0,G1,Map,Opts,Ex,Exx).

trLocation(loc(Ln,Col,_,Sz),tpl([intgr(Ln),intgr(Col),intgr(Sz)]),G,G,Q,Q,_,_,Ex,Ex).

trGoalCall(v(_,Nm),Args,[ecall(Nm,Args)|Tail],Tail,Q,Q,_,_,Ex,Ex) :-
  isEscape(Nm,_),!.
trGoalCall(v(Lc,Nm),Args,G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  lookupRelName(Map,Nm,RSpec),
  implementGoalCall(RSpec,Lc,Nm,Args,G,Gx,Q,Qx,Map,Opts,Ex,Exx).
trGoalCall(dot(Rec,Pred),Args,G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trCons(Pred,Args,Op),
  trGoalDot(Rec,cons(Op,Args),G,Gx,Q,Qx,Map,Opts,Ex,Exx).

implementGoalCall(localRel(Fn,_,_,_,LblVr,ThVr),_,_,Args,[call(Fn,XArgs)|Tail],Tail,Q,Qx,_,_,Ex,Ex) :-
  concat(Args,[LblVr,ThVr],XArgs),
  merge([LblVr,ThVr],Q,Qx).
implementGoalCall(moduleRel(_,Fn,_,_,Ar),_,_,Args,[call(prg(Fn,Ar),Args)|Tail],Tail,Q,Q,_,_,Ex,Ex).
implementGoalCall(inheritField(Super,LblVr,ThVr),_,Pred,Args,
      [call(Super,[cons(Op,Args),LblVr,ThVr])|Tail],Tail,Q,Qx,_,_,Ex,Ex) :-
  trCons(Pred,Args,Op),
  merge([LblVr,ThVr],Q,Qx).
implementGoalCall(notInMap,Lc,Pred,Args,G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trCons("_call",Args,Op),
  trGoalDot(v(Lc,Pred),cons(Op,Args),G,Gx,Q,Qx,Map,Opts,Ex,Exx).
implementGoalCall(_,_,Pred,_,G,G,Q,Q,_,_,Ex,Ex) :-
  reportMsg("cannot handle source for %s",[Pred]).

trGoalDot(v(_,Nm),C,[call(Super,[C,LbVr,ThVr])|Gx],Gx,Q,Qx,Map,_,Ex,Ex) :-
  lookupVarName(Map,Nm,inherit(_,Super,LbVr,ThVr)),!,
  merge([LbVr,ThVr],Q,Qx).
trGoalDot(Rec,C,G,Gx,Q,Qx,Map,Opts,Ex,Exx) :-
  trExp(Rec,NR,Q,Qx,G,G0,G0,G1,Map,Opts,Ex,Exx),
  G1 = [ocall(C,NR,NR)|Gx].

genClassMap(Map,Opts,Lc,LclName,Defs,Face,[lyr(LclName,List,Lc,LblGl,LbVr,ThVr,void)|Map],Entry,En,Ex,Exx) :-
  genVar("LbV",LbVr),
  genVar("ThV",ThVr),
  pickAllFieldsFromFace(Face,Fields),
  makeClassMtdMap(Defs,LclName,LbVr,ThVr,LblGl,[],L0,Fields,Map,Opts,Ex,Ex0),
  makeInheritanceMap(Defs,LclName,LbVr,ThVr,Map,Opts,L0,List,Fields,Entry,En,Ex0,Exx).

pickAllFieldsFromFace(Tp,Fields) :-
  moveQuants(Tp,_,QTp),
  moveConstraints(QTp,_,faceType(Fields)).

makeClassMtdMap([],_,_,_,void,List,List,_,_,_,Ex,Ex).
makeClassMtdMap([classBody(_,_,enum(_,_),Stmts,_,_)|Rules],LclName,LbVr,ThVr,LblGl,List,Lx,Fields,Map,Opts,Ex,Exx) :-
  collectMtds(Stmts,LclName,LbVr,ThVr,List,L0,Fields),
  collectLabelVars([],LbVr,ThVr,L0,L1),
  extraVars(Map,Extra),
  makeLblTerm(enum(LclName),Extra,LblTerm),
  (Extra =[] -> LblGl = [] ; LblGl = [unify(LbVr,LblTerm)]),
  makeClassMtdMap(Rules,LclName,LbVr,ThVr,_,L1,Lx,Fields,Map,Opts,Ex,Exx).
makeClassMtdMap([classBody(_,_,Hd,Stmts,_,_)|Rules],LclName,LbVr,ThVr,LblGl,List,Lx,Fields,Map,Opts,Ex,Exx) :-
  collectMtds(Stmts,LclName,LbVr,ThVr,List,L0,Fields),
  trPtn(Hd,Lbl,[],Vs,LblGl,Px,Px,[unify(LbVr,LblTerm)],Map,Opts,Ex,Ex0),
  collectLabelVars(Vs,LbVr,ThVr,L0,L1),
  extraVars(Map,Extra),
  makeLblTerm(Lbl,Extra,LblTerm),
  makeClassMtdMap(Rules,LclName,LbVr,ThVr,_,L1,Lx,Fields,Map,Opts,Ex0,Exx).
makeClassMtdMap([labelRule(_,_,_,_,_)|Rules],LclName,LbVr,ThVr,LblGl,List,L0,Fields,Map,Opts,Ex,Exx) :-
  makeClassMtdMap(Rules,LclName,LbVr,ThVr,LblGl,List,L0,Fields,Map,Opts,Ex,Exx).
makeClassMtdMap([implBody(_,enum(_,_),Stmts,_,_)|Rules],LclName,LbVr,ThVr,LblGl,List,Lx,Fields,Map,Opts,Ex,Exx) :-
  collectMtds(Stmts,LclName,LbVr,ThVr,List,L0,Fields),
  collectLabelVars([],LbVr,ThVr,L0,L1),
  extraVars(Map,Extra),
  makeLblTerm(enum(LclName),Extra,LblTerm),
  (Extra =[] -> LblGl = [] ; LblGl = [unify(LbVr,LblTerm)]),
  makeClassMtdMap(Rules,LclName,LbVr,ThVr,_,L1,Lx,Fields,Map,Opts,Ex,Exx).
makeClassMtdMap([implBody(_,Hd,Stmts,_,_)|Rules],LclName,LbVr,ThVr,LblGl,List,Lx,Fields,Map,Opts,Ex,Exx) :-
  collectMtds(Stmts,LclName,LbVr,ThVr,List,L0,Fields),
  trPtn(Hd,Lbl,[],Vs,LblGl,Px,Px,[unify(LbVr,LblTerm)],Map,Opts,Ex,Ex0),
  collectLabelVars(Vs,LbVr,ThVr,L0,L1),
  extraVars(Map,Extra),
  makeLblTerm(Lbl,Extra,LblTerm),
  makeClassMtdMap(Rules,LclName,LbVr,ThVr,_,L1,Lx,Fields,Map,Opts,Ex0,Exx).

makeLblTerm(enum(Nm),[],enum(Nm)) :- !.
makeLblTerm(enum(Nm),Extra,cons(strct(Nm,Ar),Extra)) :- !, length(Extra,Ar).
makeLblTerm(cons(strct(Nm,_),Args),Extra,cons(strct(Nm,Arity),As)) :- !,
  concat(Args,Extra,As),
  length(As,Arity).

makeInheritanceMap([],_,_,_,_,_,List,List,_,En,En,Ex,Ex).
makeInheritanceMap([classBody(_,_,_,_,_,_)|Defs],LclName,LbVr,ThVr,Map,Opts,List,Lx,Fields,Entry,En,Ex,Exx) :-
  makeInheritanceMap(Defs,LclName,LbVr,ThVr,Map,Opts,List,Lx,Fields,Entry,En,Ex,Exx).
makeInheritanceMap([implBody(_,_,_,_,_)|Defs],LclName,LbVr,ThVr,Map,Opts,List,Lx,Fields,Entry,En,Ex,Exx) :-
  makeInheritanceMap(Defs,LclName,LbVr,ThVr,Map,Opts,List,Lx,Fields,Entry,En,Ex,Exx).
makeInheritanceMap([labelRule(_,_,P,R,FaceTp)|Defs],LclName,LbVr,ThVr,Map,Opts,List,Lx,Fields,Entry,En,Ex,Exx) :-
  pickAllFieldsFromFace(FaceTp,InhFields),
  extraVars(Map,Extra),
  genVar("CV",CV),
  trPtn(P,Ptn,Extra,Q0,Body,Pre0,Pre0,Prx,Map,Opts,Ex,Ex0),
  trExp(R,Repl,Q0,Q1,Prx,Px,Px,[ocall(CV,Repl,ThVr)],Map,Opts,Ex0,Ex1),
  genNewName(Map,"^",3,Super),
  merge(Q1,[CV,ThVr],Q),
  Ex1 =  [clse(Q,Super,[CV,Ptn,ThVr],Body)|Ex2],
  makeInheritFields(InhFields,types:isPredType,LclName,Super,Fields,LbVr,ThVr,Entry,En0,List,L1),
  makeInheritanceMap(Defs,LclName,LbVr,ThVr,Map,Opts,L1,Lx,Fields,En0,En,Ex2,Exx).

makeInheritFields([],_,_,_,_,_,_,Entry,Entry,List,List).
makeInheritFields([(Nm,Tp)|InhFields],Test,LclName,Super,Fields,LbVr,ThVr,Entry,En,List,Lx) :-
  is_member((Nm,_),List),
  \+ call(Test,Tp),!,
  makeInheritFields(InhFields,Test,LclName,Super,Fields,LbVr,ThVr,Entry,En,List,Lx).
makeInheritFields([(Nm,Tp)|InhFields],Test,LclName,Super,Fields,LbVr,ThVr,Entry,En,List,Lx) :-
  inheritClause(Nm,Tp,LclName,Super,Entry,En0),
  makeInheritFields(InhFields,Test,LclName,Super,Fields,LbVr,ThVr,En0,En,[(Nm,inheritField(Super,LbVr,ThVr))|List],Lx).

inheritClause(Name,Tp,Prefix,Super,[clse(Q,prg(Prefix,3),[cons(Con,Args),LbVr,ThVr],
      [neck,call(Super,[cons(Con,Args),LbVr,ThVr])])|En],En) :-
  fieldArity(Tp,Arity),
  genVars(Arity,Args),
  genVar("This",ThVr),
  genVar("Lbl",LbVr),
  concat(Args,[LbVr,ThVr],Q),
  trCons(Name,Arity,Con).

fieldArity(Tp,Arity) :- isFunctionType(Tp,Ar), !, Arity is Ar+1.
fieldArity(Tp,Arity) :- isPredType(Tp,Arity),!.
fieldArity(Tp,Arity) :- isClassType(Tp,Ar), !, Arity is Ar+1.
fieldArity(_,1).

collectMtds([],_,_,_,List,List,_).
collectMtds([Entry|Defs],OuterNm,LbVr,ThVr,List,Lx,Fields) :-
  collectMtd(Entry,OuterNm,LbVr,ThVr,List,L0),
  collectMtds(Defs,OuterNm,LbVr,ThVr,L0,Lx,Fields).

collectMtd(function(_,Nm,Tp,_,_),OuterNm,Lbl,ThV,List,
      [(Nm,localFun(LclName,AccessName,ClosureName,ArA,Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName),
  localName(OuterNm,"%",Nm,AccessName),
  localName(OuterNm,"^",Nm,ClosureName),
  typeArity(Tp,Ar),
  ArA is Ar+1.
collectMtd(predicate(_,Nm,Tp,_,_),OuterNm,Lbl,ThV,List,
      [(Nm,localRel(LclName,AccessName,ClosureName,Arity,Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName),
  localName(OuterNm,"%",Nm,AccessName),
  localName(OuterNm,"^",Nm,ClosureName),
  typeArity(Tp,Arity).
collectMtd(grammar(_,Nm,Tp,_,_),OuterNm,Lbl,ThV,List,
      [(Nm,localRel(LclName,AccessName,ClosureName,Arity,Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName),
  localName(OuterNm,"%",Nm,AccessName),
  localName(OuterNm,"^",Nm,ClosureName),
  typeArity(Tp,Ar),
  Arity is Ar+2.
collectMtd(defn(_,Nm,_,_,_,_),OuterNm,Lbl,ThV,List,[(Nm,localVar(LclName,AccessName,Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName),
  localName(OuterNm,"%",Nm,AccessName).
collectMtd(enum(_,Nm,_,_,_,_),OuterNm,Lbl,ThV,List,
      [(Nm,localClass(OuterNm,LclName,AccessName,0,Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName),
  localName(OuterNm,"%",Nm,AccessName).
collectMtd(class(_,Nm,Tp,_,_,_),OuterNm,Lbl,ThV,List,
      [(Nm,localClass(OuterNm,LclName,AccessName,Arity,Lbl,ThV))|List]) :-
  localName(OuterNm,"@",Nm,LclName),
  localName(OuterNm,"%",Nm,AccessName),
  typeArity(Tp,Arity).

collectLabelVars([],_,_,List,List).
collectLabelVars([V|Args],LbVr,ThVr,List,Lx) :-
  V=idnt(Nm),
  collectLabelVars(Args,LbVr,ThVr,[(Nm,labelArg(V,LbVr,ThVr))|List],Lx).
