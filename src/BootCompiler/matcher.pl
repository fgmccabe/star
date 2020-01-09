:- module(matcher,[functionMatcher/6,caseMatcher/4]).

:- use_module(canon).
:- use_module(errors).
:- use_module(types).
:- use_module(misc).
:- use_module(location).
:- use_module(lterms).
:- use_module(transutils).

functionMatcher(Lc,Ar,Nm,Tp,Eqns,fnDef(Lc,Nm,Tp,NVrs,Reslt)) :-
  genVars(Ar,NVrs),
  makeTriples(Eqns,0,Tpls),
  genRaise(Lc,Error),
  matchTriples(Lc,NVrs,Tpls,Error,Reslt),!.
functionMatcher(Lc,_Ar,Nm,Tp,_Eqns,fnDef(Lc,Nm,Tp,[],enum("void"))) :-
  reportError("(internal) failed to construct function for %s",[Nm],Lc).

caseMatcher(Lc,Bnd,Cases,Result) :-
  makeTriples(Cases,0,Tpls),
  genRaise(Lc,Error),
  matchTriples(Lc,[Bnd],Tpls,Error,Result).

genRaise(Lc,error(Lc,"no matches")).

matchTriples(_,[],Tps,Deflt,Reslt) :-
  conditionalize(Tps,Deflt,Reslt).
matchTriples(Lc,Vrs,Tpls,Deflt,Reslt) :-
  partitionTriples(Tpls,Segments),
  matchSegments(Segments,Vrs,Lc,Deflt,Reslt).

matchSegments([],_,_,Deflt,Deflt).
matchSegments([Seg|M],Vrs,Lc,Deflt,Reslt) :-
  matchSegments(M,Vrs,Lc,Deflt,Partial),
  matchSegment(Seg,Vrs,Lc,Partial,Reslt).

matchSegment(Seg,Vrs,Lc,Deflt,Reslt) :-
  segmentMode(Seg,Mode),
  compileMatch(Mode,Seg,Vrs,Lc,Deflt,Reslt).

segmentMode([Tr|_],Mode) :-
  tripleArgMode(Tr,Mode).

compileMatch(inScalars,Tpls,Vrs,Lc,Deflt,Reslt) :-
  matchScalars(Tpls,Vrs,Lc,Deflt,Reslt).
compileMatch(inConstructors,Tpls,Vrs,Lc,Deflt,Reslt) :-
  matchConstructors(Lc,Tpls,Vrs,Deflt,Reslt).
compileMatch(inVars,Tpls,Vrs,Lc,Deflt,Reslt) :-
  matchVars(Lc,Vrs,Tpls,Deflt,Reslt).

conditionalize([],Deflt,Deflt).
conditionalize([(_,(Lc,Bnds,Test,Val),_)|M],Deflt,Repl) :-!,
  pullWhere(Val,enum("star.core#true"),Vl,C0),
  mergeGoal(Test,C0,Lc,TT),
  (TT=enum("star.core#true") ->
    applyBindings(Bnds,Lc,Vl,Repl);
    conditionalize(M,Deflt,Other),
    applyBindings(Bnds,Lc,Vl,TVl),
    Repl = cnd(Lc,TT,TVl,Other)
  ).

applyBindings(Bnds,Lc,Val,DVal) :-
  filter(Bnds,matcher:filterBndVar(Val),VBnds),!,
  (VBnds=[] -> DVal=Val ; DVal = varNames(Lc,VBnds,Val)).

filterBndVar(Val,(Nm,X)) :-
  \+ string_concat("_",_,Nm),
  idInTerm(idnt(X),Val).

argMode(idnt(_),inVars).
argMode(voyd,inScalars).
argMode(intgr(_),inScalars).
argMode(float(_),inScalars).
argMode(strg(_),inScalars).
argMode(lbl(_,_),inScalars).
argMode(whr(_,T,_),M) :- argMode(T,M).
argMode(enum(_),inConstructors).
argMode(ctpl(_,_),inConstructors).

makeTriples([],_,[]).
makeTriples([Rl|L],Ix,[Tr|LL]) :-
  makeEqnTriple(Rl,Ix,Tr),
  Ix1 is Ix+1,
  makeTriples(L,Ix1,LL).

makeEqnTriple((Lc,Args,Cnd,Val),Ix,(Args,(Lc,[],enum("star.core#true"),whr(Lc,Val,Cnd)),Ix)).

partitionTriples([Tr|L],[[Tr|LL]|Tx]) :-
  tripleArgMode(Tr,M),
  partTriples(L,L0,M,LL),
  partitionTriples(L0,Tx).
partitionTriples([],[]).

partTriples([],[],_,[]).
partTriples([Tr|L],Lx,M,[Tr|LL]) :-
  tripleArgMode(Tr,M),
  partTriples(L,Lx,M,LL).
partTriples(L,L,_,[]).

tripleArgMode(([A|_],_,_),Mode) :-
  argMode(A,Mode),!.

newVars([],V,V).
newVars([_|L],V,[idnt(NN)|VV]) :-
  genstr("_",NN),
  newVars(L,V,VV).

matchScalars(Tpls,[V|Vrs],Lc,Deflt,CaseExp) :-
  sort(Tpls,matcher:compareScalarTriple,ST),
  formCases(ST,matcher:sameScalarTriple,Lc,Vrs,Deflt,Cases),
  mkCase(Lc,V,Cases,Deflt,CaseExp).

mkCase(Lc,V,[(Lbl,Exp,Lc)],Deflt,cnd(Lc,mtch(Lc,Lbl,V),Exp,Deflt)) :-!.
mkCase(Lc,V,Cases,Deflt,case(Lc,V,Cases,Deflt)).

matchConstructors(Lc,Tpls,[V|Vrs],Deflt,CaseExp) :-
  sort(Tpls,matcher:compareConstructorTriple,ST),
  formCases(ST,matcher:sameConstructorTriple,Lc,Vrs,Deflt,Cases),
  mkCase(Lc,V,Cases,Deflt,CaseExp).

formCases([],_,_,[],_,[]) :- !.
formCases([],_,_,_,_,[]).
formCases([Tr|Trpls],Cmp,Lc,Vrs,Deflt,[(Lbl,Case,Lc)|Cses]) :-
  pickMoreCases(Tr,Trpls,Tx,Cmp,More),
  formCase(Tr,Lbl,[Tr|Tx],Lc,Vrs,Deflt,Case),
  formCases(More,Cmp,Lc,Vrs,Deflt,Cses).

formCase(([Lbl|_],_,_),Lbl,Tpls,Lc,Vrs,Deflt,Case) :-
  isScalar(Lbl),!,
  subTriples(Tpls,STpls),
  matchTriples(Lc,Vrs,STpls,Deflt,Case).
formCase(([enum(Lb)|_],_,_),enum(Lb),Tpls,Lc,Vrs,Deflt,Case) :-
  subTriples(Tpls,STpls),
  matchTriples(Lc,Vrs,STpls,Deflt,Case).
formCase(([ctpl(Op,Args)|_],_,_),ctpl(Op,NVrs),Tpls,Lc,Vrs,Deflt,Case) :-
  length(Args,Ar),
  genVars(Ar,NVrs),
  concat(NVrs,Vrs,NArgs),
  subTriples(Tpls,NTpls),
  matchTriples(Lc,NArgs,NTpls,Deflt,Case).

pickMoreCases(_,[],[],_,[]).
pickMoreCases(Tr,[A|Trpls],[A|Tx],Cmp,More) :-
  call(Cmp,Tr,A),!,
  pickMoreCases(Tr,Trpls,Tx,Cmp,More).
pickMoreCases(_,Trpls,[],_,Trpls).

isScalar(S) :- argMode(S,inScalars),!.

mergeTriples(L1,L2,L3) :-
  sortedMerge(L1,L2,matcher:earlierIndex,L3).

subTriples([],[]).
subTriples([T1|Tr1],[ST1|STr1]) :-
  subTriple(T1,ST1),
  subTriples(Tr1,STr1).

subTriple(([ctpl(_,CArgs)|Args],V,X),(NArgs,V,X)) :-!,
  concat(CArgs,Args,NArgs).
subTriple(([_|Args],V,X),(Args,V,X)).

earlierIndex((_,_,Ix1),(_,_,Ix2)) :-
  Ix1<Ix2.

compareConstructorTriple(([A|_],_,_),([B|_],_,_)) :-
  compareConstructor(A,B).

compareConstructor(A,B) :-
  constructorName(A,ANm),
  constructorName(B,BNm),
  str_lt(ANm,BNm).

sameConstructorTriple(([A|_],_,_),([B|_],_,_)) :-
  sameConstructor(A,B).

sameConstructor(A,B) :-
  constructorName(A,Nm),
  constructorName(B,Nm).

constructorName(enum(Nm),Nm).
constructorName(lbl(Nm,_),Nm).
constructorName(ctpl(C,_),Nm) :-
  constructorName(C,Nm).

compareScalarTriple(([A|_],_,_),([B|_],_,_)) :-
  compareScalar(A,B).

compareScalar(intgr(A),intgr(B)) :-!,
  A<B.
compareScalar(float(A),float(B)) :-!,
  A<B.
compareScalar(strg(A),strg(B)) :-!,
  str_lt(A,B).
compareScalar(lbl(L1,_A1),lbl(L2,_A2)) :-
  str_lt(L1,L2),!.
compareScalar(lbl(L,A1),lbl(L,A2)) :-
  A1<A2.

sameScalarTriple(([A|_],_,_),([A|_],_,_)).

matchVars(Lc,[V|Vrs],Triples,Deflt,Reslt) :-
  applyVar(V,Triples,NTriples),
  matchTriples(Lc,Vrs,NTriples,Deflt,Reslt).

applyVar(_,[],[]).
applyVar(V,[([idnt(XV)|Args],(Lc,Bnd,Cond,Vl),Ix)|Tpls],[(NArgs,(Lc,[(XV,V)|Bnd],NCond,NVl),Ix)|NTpls]) :-
  Vrs = [(XV,V)],
  substTerm(Vrs,Vl,NVl),
  substTerms(Vrs,Args,NArgs),
  substTerm(Vrs,Cond,NCond),
  applyVar(V,Tpls,NTpls).
applyVar(V,[([whr(Lcw,idnt(XV),Cond)|Args],(Lc,Bnd,Test,Vl),Ix)|Tpls],
      [(NArgs,(Lc,[(XV,V)|Bnd],NCnd,NVl),Ix)|NTpls]) :-
  Vrs = [(XV,V)],
  substTerm(Vrs,Vl,NVl),
  substTerm(Vrs,Cond,NCond),
  substTerm(Vrs,Test,NTest),
  mergeGoal(NTest,NCond,Lcw,NCnd),
  substTerms(Vrs,Args,NArgs),
  applyVar(V,Tpls,NTpls).
