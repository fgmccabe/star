:- module(matcher,[functionMatcher/6]).

:- use_module(canon).
:- use_module(errors).
:- use_module(types).
:- use_module(misc).
:- use_module(location).
:- use_module(freevars).
:- use_module(terms).

functionMatcher(Lc,Ar,Nm,Tp,Eqns,fnDef(Lc,Nm,Tp,[eqn(Lc,NVrs,Reslt)])) :-
  genVars(Ar,NVrs),
  makeTriples(Eqns,0,Tpls),
  genRaise(Lc,Error),
  matchTriples(Lc,NVrs,Tpls,Error,Reslt).

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
conditionalize([(_,(_,Bnds,whr(Lc,Val,Cond)),_)|M],Deflt,cnd(Lc,Cnd,Value,Other)) :-
  pullWheres(Val,Lc,Cond,Cnd,Vl),
  conditionalize(M,Deflt,Other),
  applyBindings(Bnds,Lc,Vl,Value).
conditionalize([(_,(Lc,Bnds,Val),_)|_],_,Value) :-
  applyBindings(Bnds,Lc,Val,Value).

pullWheres(whr(Lc,Val,Cond),Lc0,Cnd,Cndx,Value) :-
  pullWheres(Val,Lc0,cnj(Lc,Cnd,Cond),Cndx,Value).
pullWheres(Val,_Lc,Cnd,Cnd,Val).

applyBindings([],_,Val,Val).
applyBindings(Bnds,Lc,Val,varNames(Lc,Bnds,Val)).

argMode(idnt(_),inVars).
argMode(intgr(_),inScalars).
argMode(float(_),inScalars).
argMode(strg(_),inScalars).
argMode(whr(_,T,_),M) :- argMode(T,M).
argMode(enum(_),inConstructors).
argMode(ctpl(_,_),inConstructors).

makeEqnTriple(eqn(Lc,Args,Value),Ix,(Args,(Lc,[],Value),Ix)).

makeTriples([],_,[]).
makeTriples([Rl|L],Ix,[Tr|LL]) :-
  makeEqnTriple(Rl,Ix,Tr),
  Ix1 is Ix+1,
  makeTriples(L,Ix1,LL).

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

genVars(0,[]).
genVars(Ar,[idnt(NN)|LL]) :-
  Ar>0,
  genstr("_",NN),
  Ar1 is Ar-1,
  genVars(Ar1,LL).

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

isScalar(intgr(_)).
isScalar(float(_)).
isScalar(strg(_)).
isScalar(enum(_)).

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
  scalarTerm(A,As),
  scalarTerm(B,Bs),
  compareScalar(As,Bs).

compareScalar(intgr(A),intgr(B)) :-
  A<B.
compareScalar(float(A),float(B)) :-
  A<B.
compareScalar(strg(A),strg(B)) :-
  str_lt(A,B).

sameScalarTriple(([A|_],_,_),([A|_],_,_)).

matchVars(Lc,[V|Vrs],Triples,Deflt,Reslt) :-
  applyVar(V,Triples,NTriples),
  matchTriples(Lc,Vrs,NTriples,Deflt,Reslt).

applyVar(_,[],[]).
applyVar(V,[([idnt(XV)|Args],(Lc,Bnd,Vl),Ix)|Tpls],[(Args,(Lc,[(XV,V)|Bnd],NVl),Ix)|NTpls]) :-
  substTerm([(XV,V)],Vl,NVl),
  applyVar(V,Tpls,NTpls).
applyVar(V,[([whr(Lcw,idnt(XV),Cond)|Args],(Lc,Bnd,Vl),Ix)|Tpls],
      [(Args,(Lc,[(XV,V)|Bnd],whr(Lcw,NVl,NCond)),Ix)|NTpls]) :-
  substTerm([(XV,V)],Vl,NVl),
  substTerm([(XV,V)],Cond,NCond),
  applyVar(V,Tpls,NTpls).
