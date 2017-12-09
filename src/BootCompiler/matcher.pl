:- module(matcher,[functionMatcher/5]).

:- use_module(canon).
:- use_module(errors).
:- use_module(types).
:- use_module(misc).
:- use_module(location).
:- use_module(freevars).
:- use_module(terms).
:- use_module(polyfill).

functionMatcher(Lc,Ar,Nm,Eqns,fnDef(Lc,Nm,[eqn(Lc,NVrs,Reslt)])) :-
  genVars(Ar,NVrs),
  makeTriples(Eqns,0,Tpls),
  genRaise(Lc,Error),
  matchTriples(Lc,NVrs,Tpls,Error,Reslt).

matchTriples(_,[],Tps,Deflt,Reslt) :-
  conditionalize(Tps,Deflt,Reslt).
matchTriples(Lc,Vrs,Tpls,Deflt,Reslt) :-
  partitionTriples(Tpls,Segments),
  matchSegments(Segments,Vrs,Lc,Deflt,Reslt).

matchSegments([],_,Lc,Deflt,Deflt).
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
compileMatch(inTuples,Tpls,Vrs,Lc,Deflt,Reslt) :-
  matchTuples(Lc,Tpls,Vrs,Deflt,Reslt.
compileMatch(inVars,Tpls,Vrs,Lc,Deflt,Reslt) :-
  matchVars(Lc,Tpls,Vrs,Deflt,Reslt).

conditionalize([(_,(_,[],Value),_)|_],Value).
conditionalize([(_,(Lc,Bnds,Value),_)|_],varNames(Lc,Bnds,Value)).

argMode(idnt(_),inVars).
argMode(intgr(_),inScalars).
argMode(float(_),inScalars).
argMode(strg(_),inScalars).
argMode(whr(_,T,_),M) :- argMode(T,M).
argMode(enu(_),inConstructors).
argMode(ctpl(_,_),inConstructors).
argMode(tpl(_),inTuples).

makeEqnTriple(eqn(Lc,Args,Value),Ix,(Args,(Lc,[],Value),Ix)).

makeTriples([],_,[]).
makeTriples([Rl|L],Ix,[Tr|LL]) :-
  makeEqnTriple(Rl,Ix,Tr),
  Ix1 is Ix+1,
  makeTriples(L,Ix1,LL).

partitionTriples([Tr|L],[[Tr|LL]|Tx]) :-
  argMode(Tr,M),
  partTriples(L,L0,M,LL),
  partitionTriples(L0,Tx).
partitionTriples([],[]).

partTriples([],[],_,[]).
partTriples([Tr|L],Lx,M,[Tr|LL]) :-
  tripleArgMode(Tr,M),
  partTriples(L,M,LL).
partTriples(L,L,_,[]).

tripleArgMode(([A|_],_,_),Mode) :-
  argMode(A,Mode),!.

triplesMode([Tr|_],M) :-
  tripleArgMode(Tr,M).

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

matchScalars(Tpls,[V|Vrs],Lc,Deflt,case(Lc,V,Cases,Deflt)) :-
  quickSort(Tpls,matcher:compareScalarTriple,ST),
  formCases(ST,matcher:sameScalarTriple,Lc,Vrs,Deflt,Cases).

matchConstructors(Lc,Tpls,[V|Vrs],Deflt,case(Lc,V,Cases,Deflt)) :-
  quickSort(CT,matcher:compareConstructorTriple,ST),
  formCases(ST,matcher:sameConstructorTriple,Lc,Vrs,Deflt,Cases).

formCases([],_,_,[],_,_) :- !.
formCases([],_,Lc,Vrs,Deflt,[]).
formCases([Tr|Trpls],Cmp,Lc,Vrs,Deflt,[(Lbl,Case)|Cses]) :-
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
isScalar(enu(_)).

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
  '_str_lt'(ANm,BNm).

sameConstructorTriple(([A|_],_,_),([B|_],_,_)) :-
  sameConstructor(A,B).

sameConstructor(A,B) :-
  constructorName(A,Nm),
  constructorName(B,Nm).

constructorName(enu(Nm),Nm).
constructorName(strct(Nm,_),Nm).
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
  '_str_lt'(A,B).

sameScalarTriple(([A|_],_,_),([A|_],_,_)).

matchVars(Lc,[V|Vrs],Triples,Deflt,Reslt) :-
  applyVar(V,Triples,NTriples),
  matchTriples(Lc,Vrs,NTriples,Deflt,Reslt).

applyVar(_,[],[]).
applyVar(V,[([idnt(XV)|Args],(Lc,Bnd,Vl),Ix)|Tpls],[(Args,(Lc,[(XV,V)|Bnd],NVl),Ix)|NTpls]) :-
  substTerm([(XV,V)],Vl,NVl),
  applyVar(V,Tpls,NTpls).
