:- module(matcher,[functionMatcher/8,caseMatcher/5,actionCaseMatcher/5]).

:- use_module(canon).
:- use_module(errors).
:- use_module(types).
:- use_module(misc).
:- use_module(location).
:- use_module(lterms).
:- use_module(transutils).

functionMatcher(Lc,Ar,Nm,H,Tp,Eqns,Map,fnDef(Lc,Nm,H,Tp,NVrs,Reslt)) :-
  genVars(Ar,NVrs),
  makeTriples(Eqns,0,Tpls),
  getLocalLblName(Nm,LclNm),
  genRaise(Lc,LclNm,Error),
  matchTriples(Lc,lterms:substTerm,matcher:mkCond,NVrs,Tpls,Error,Map,0,Reslt),!.
functionMatcher(Lc,_Ar,Nm,H,Tp,_Eqns,_,fnDef(Lc,Nm,H,Tp,[],enum("void"))) :-
  reportError("(internal) failed to construct function for %s",[Nm],Lc).

getLocalLblName(lbl(Nm,_),LclNm) :-
  getLocalName(Nm,LclNm).

caseMatcher(Lc,Bnd,Cases,Map,Result) :-
  makeTriples(Cases,0,Tpls),
  genRaise(Lc,"case",Error),
  matchTriples(Lc,lterms:substTerm,matcher:mkCond,[Bnd],Tpls,Error,Map,0,Result).

actionCaseMatcher(Lc,Bnd,Cases,Map,Result) :-
  makeTriples(Cases,0,Tpls),
  genRaise(Lc,"case",Error),
  matchTriples(Lc,lterms:substAction,matcher:mkIfThenElse,[Bnd],Tpls,Error,Map,0,Result).

genRaise(Lc,Nm,error(Lc,strg(Msg))) :-
  string_concat("no matches in ",Nm,Msg).

genVrs([],[]) :-!.
genVrs([Tp|Tps],[(V,Tp)|Vrs]) :-
  genVar("_V",V),
  genVrs(Tps,Vrs).

matchTriples(_,_,Conder,[],Tps,Deflt,_,_,Reslt) :-
  conditionalize(Tps,Conder,Deflt,Reslt).
matchTriples(Lc,Subber,Conder,Vrs,Tpls,Deflt,Map,Dp,Reslt) :-
  partitionTriples(Tpls,Segments),
  matchSegments(Segments,Subber,Conder,Vrs,Lc,Deflt,Map,Dp,Reslt).

matchSegments([],_,_,_,_,Deflt,_,_,Deflt).
matchSegments([Seg|M],Subber,Conder,Vrs,Lc,Deflt,Map,Dp,Reslt) :-
  matchSegments(M,Subber,Conder,Vrs,Lc,Deflt,Map,Dp,Partial),
  matchSegment(Seg,Subber,Conder,Vrs,Lc,Partial,Map,Dp,Reslt).

matchSegment(Seg,Subber,Conder,Vrs,Lc,Deflt,Map,Dp,Reslt) :-
  segmentMode(Seg,Mode),
  compileMatch(Mode,Subber,Conder,Seg,Vrs,Lc,Deflt,Map,Dp,Reslt).

segmentMode([Tr|_],Mode) :-
  tripleArgMode(Tr,Mode).

compileMatch(inScalars,_Subber,Conder,Tpls,Vrs,_Lc,Deflt,_Map,Dp,Reslt) :-
  tooDeep(Dp),!,
  conditionalize(Vrs,Conder,Tpls,Deflt,Reslt).
compileMatch(inScalars,Subber,Conder,Tpls,Vrs,Lc,Deflt,Map,Dp,Reslt) :-
  matchScalars(Subber,Conder,Tpls,Vrs,Lc,Deflt,Map,Dp,Reslt).
compileMatch(inConstructors,_Subber,Conder,Tpls,Vrs,_Lc,Deflt,_Map,Dp,Reslt) :-
  tooDeep(Dp),!,
  conditionalize(Vrs,Conder,Tpls,Deflt,Reslt).
compileMatch(inConstructors,Subber,Conder,Tpls,Vrs,Lc,Deflt,Map,Dp,Reslt) :-
  matchConstructors(Lc,Subber,Conder,Tpls,Vrs,Deflt,Map,Dp,Reslt).
compileMatch(inVars,Subber,Conder,Tpls,Vrs,Lc,Deflt,Map,Dp,Reslt) :-
  matchVars(Lc,Subber,Conder,Vrs,Tpls,Deflt,Map,Dp,Reslt).

conditionalize([],_,Deflt,Deflt).
conditionalize([(_,(Lc,Bnds,Guard,Test,Val),_)|M],Conder,Deflt,Repl) :-!,
  pullWhere(Val,none,Vl,C0),
  mergeGl(Guard,Test,Lc,G0),
  mergeGl(G0,C0,Lc,TT),
  (mustSucceed(TT) ->
   applyBindings(Bnds,Lc,Vl,Repl);
   conditionalize(M,Conder,Deflt,Other),
   applyBindings(Bnds,Lc,Vl,TVl),
   call(Conder,Lc,TT,TVl,Other,Repl)
  ).

conditionalize(_Vrs,_,[],Deflt,Deflt).
conditionalize(Vrs,Conder,[(Args,(Lc,Bnds,Guard,Test,Val),_)|M],Deflt,Repl) :-
  mkMatchCond(Vrs,Args,Lc,BaseCond),
  pullWhere(Val,BaseCond,Vl,C0),
  mergeGl(Guard,Test,Lc,G0),
  mergeGl(C0,G0,Lc,TT),
  (mustSucceed(TT) ->
   applyBindings(Bnds,Lc,Vl,Repl);
   conditionalize(Vrs,Conder,M,Deflt,Other),
   applyBindings(Bnds,Lc,Vl,TVl),
   call(Conder,Lc,TT,TVl,Other,Repl)
  ).

mkMatchCond([_|Vrs],[anon|Args],Lc,Cnd) :-
  mkMatchCond(Vrs,Args,Lc,Cnd).
mkMatchCond([],[],_,none).
mkMatchCond([V|Vrs],[A|Args],Lc,Cond) :-
  mkMatchCond(Vrs,Args,Lc,C0),
  mergeGl(some(mtch(Lc,A,V)),C0,Lc,Cond).
/*

mkMatchCond(Vrs,Args,Lc,Cnd) :-
  mkMtchEls(Vrs,Args,Vs,As),
  (Vs=[] ->
   Cnd=none;
   mkTpl(Vs,VrTpl),
   mkTpl(As,AgTpl),
   Cnd = some(mtch(Lc,AgTpl,VrTpl))).
  
mkMtchEls([],[],[],[]) :-!.
mkMtchEls([_V|Vs],[anon|As],Vrs,Args) :-!,
  mkMtchEls(Vs,As,Vrs,Args).
mkMtchEls([V|Vs],[A|As],[V|Vrs],[A|Args]) :-
  mkMtchEls(Vs,As,Vrs,Args).
  */
  
mustSucceed(none).

applyBindings(Bnds,Lc,Val,DVal) :-
  filter(Bnds,matcher:filterBndVar(Val),VBnds),!,
  (VBnds=[] -> DVal=Val ; DVal = varNames(Lc,VBnds,Val)).

filterBndVar(Val,(Nm,X)) :-
  \+ string_concat("_",_,Nm),
  idInTerm(idnt(X),Val).

argMode(idnt(_),inVars).
argMode(anon,inVars).
argMode(voyd,inScalars).
argMode(intgr(_),inScalars).
argMode(bigx(_),inScalars).
argMode(float(_),inScalars).
argMode(chr(_),inScalars).
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

makeEqnTriple((Lc,Args,Test,Val),Ix,(Args,(Lc,[],none,Test,Val),Ix)).

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

matchScalars(Subber,Conder,Tpls,[V|Vrs],Lc,Deflt,Map,Dp,CaseExp) :-
  sort(Tpls,matcher:compareScalarTriple,ST),
  Dp1 is Dp+1,
  formCases(ST,Subber,Conder,matcher:sameScalarTriple,Lc,Vrs,Deflt,Map,Dp1,Cases),
  mkCase(Lc,V,Conder,Cases,Deflt,CaseExp).

mkCond(Lc,Tst,cnd(_,T,Th,El),El,cnd(Lc,T1,Th,El)) :-!,
  mergeGl(Tst,some(T),Lc,some(T1)).
mkCond(Lc,some(Tst),Th,El,cnd(Lc,Tst,Th,El)).

mkIfThenElse(Lc,Tst,iftte(_,T,Th,El),El,iftte(Lc,T1,Th,El)) :-!,
  mergeGl(Tst,some(T),Lc,some(T1)).
mkIfThenElse(Lc,some(Tst),Th,El,iftte(Lc,Tst,Th,El)).

mkCase(Lc,V,Conder,[(Lbl,Exp,Lc)],Deflt,Cnd) :-!,
  call(Conder,Lc,some(mtch(Lc,Lbl,V)),Exp,Deflt,Cnd).
mkCase(Lc,V,Cases,Deflt,case(Lc,V,Cases,Deflt)).

mkUnpack(_Lc,V,Conder,[(enum(Nm),Exp,Lc)],_Index,Deflt,_Map,Reslt) :-!,
  call(Conder,Lc,some(mtch(Lc,ctpl(lbl(Nm,0),[]),V)),Exp,Deflt,Reslt).
mkUnpack(_Lc,V,Conder,[(Ptn,Exp,Lc)],_Index,Deflt,_Map,Reslt) :-!,
  call(Conder,Lc,some(mtch(Lc,Ptn,V)),Exp,Deflt,Reslt).
mkUnpack(Lc,V,_,Cases,Index,Deflt,Map,unpack(Lc,V,Arms)) :-
  populateArms(Index,Cases,Map,Lc,Deflt,Arms).

populateArms([],_,_,_,_,[]).
populateArms([(lbl(FullNm,_),_Ix)|Index],Cases,Map,DLc,Deflt,[Arm|Arms]) :-
  (armPresent(FullNm,Cases,Lbl,Exp,Lc) ->
   Arm=(Lbl,Exp,Lc);
   lookupVar(Map,FullNm,moduleCons(_,CnsTp,_)),
   emptyCase(CnsTp,FullNm,EmptyLbl),
   Arm=(EmptyLbl,Deflt,DLc)),
  populateArms(Index,Cases,Map,DLc,Deflt,Arms).

armPresent(Nm,[(enum(Nm),Exp,Lc)|_],ctpl(lbl(Nm,0),[]),Exp,Lc) :-!.
armPresent(Nm,[(ctpl(lbl(Nm,Ar),As),Exp,Lc)|_],ctpl(lbl(Nm,Ar),As),Exp,Lc) :-!.
armPresent(Nm,[_|Cases],Lbl,Exp,Lc) :-
  armPresent(Nm,Cases,Lbl,Exp,Lc).

emptyCase(ConsTp,CnsNm,ctpl(lbl(CnsNm,Ar),Args)) :-
  isConType(ConsTp,Ar),
  genAnons(Ar,Args).

showCase((Lbl,Exp,Lc),C,Cx) :-
  appStr("case ",C,C0),
  showLocation(Lc,C0,C1),
  showTerm(Lbl,0,C1,C2),
  appStr("=>",C2,C3),
  showTerm(Exp,0,C3,C4),
  appStr("\n",C4,Cx).

showCases([],Cx,Cx) :-!.
showCases([Cs|Css],C,Cx) :-
  showCase(Cs,C,C0),
  showCases(Css,C0,Cx).

dispCases(Css) :-
  showCases(Css,Chrs,[]),
  string_chars(Res,Chrs), write(Res).  

matchConstructors(Lc,Subber,Conder,Tpls,[V|Vrs],Deflt,Map,Dp,CaseExp) :-
%  reportMsg("constructor case around %s,\ndefault %s",[V,Deflt],Lc),
  sort(Tpls,matcher:compareConstructorTriple,ST),
  Dp1 is Dp+1,
  formCases(ST,Subber,Conder,matcher:sameConstructorTriple,Lc,Vrs,Deflt,Map,Dp1,Cases),
					%  dispCases(Cases),
  findIndexMap(Tpls,Map,Index),
  mkUnpack(Lc,V,Conder,Cases,Index,Deflt,Map,CaseExp).

findIndexMap([([A|_],_,_)|_],Map,Index) :-
  cnsName(A,ANm),
  findConsType(Map,ANm,Tp),
  consTpName(Tp,TpNm),
  lookupTypeIndex(Map,TpNm,Index).
findIndexMap([([ctpl(lbl(Nm,Ar),_)|_],_,_)|_],_,[(lbl(Nm,Ar),0)]) :-
  isTplLbl(Nm,Ar),!.
  
formCases([],_,_,_,_,[],_,_,_,[]) :- !.
formCases([],_,_,_,_,_,_,_,_,[]).
formCases([Tr|Trpls],Subber,Conder,Cmp,Lc,Vrs,Deflt,Map,Dp,[(Lbl,Case,Lc)|Cses]) :-
  pickMoreCases(Tr,Trpls,Tx,Cmp,More),
  formCase(Tr,Subber,Conder,Lbl,[Tr|Tx],Lc,Vrs,Deflt,Map,Dp,Case),
  formCases(More,Subber,Conder,Cmp,Lc,Vrs,Deflt,Map,Dp,Cses).

formCase(([Lbl|_],_,_),Subber,Conder,Lbl,Tpls,Lc,Vrs,Deflt,Map,Dp,Case) :-
  isScalar(Lbl),!,
  subTriples(Tpls,STpls),
  matchTriples(Lc,Subber,Conder,Vrs,STpls,Deflt,Map,Dp,Case).
formCase(([enum(Lb)|_],_,_),Subber,Conder,enum(Lb),Tpls,Lc,Vrs,Deflt,Map,Dp,Case) :-
  subTriples(Tpls,STpls),
  matchTriples(Lc,Subber,Conder,Vrs,STpls,Deflt,Map,Dp,Case).
formCase(([ctpl(Op,Args)|_],_,_),Subber,Conder,ctpl(Op,NVrs),[Tpl],Lc,Vrs,Deflt,Map,Dp,Case) :-!,
  genTplVars(Args,NVrs),
  concat(NVrs,Vrs,NArgs),
  subTriples([Tpl],NTpls),
  matchTriples(Lc,Subber,Conder,NArgs,NTpls,Deflt,Map,Dp,Case).
formCase(([ctpl(Op,Args)|_],_,_),Subber,Conder,ctpl(Op,NVrs),Tpls,Lc,Vrs,Deflt,Map,Dp,Case) :-
  length(Args,Ar),
  genVars(Ar,NVrs),
  concat(NVrs,Vrs,NArgs),
  subTriples(Tpls,NTpls),
  matchTriples(Lc,Subber,Conder,NArgs,NTpls,Deflt,Map,Dp,Case).

genTplVars([],[]).
genTplVars([anon|Vrs],[anon|Rest]) :-
  genTplVars(Vrs,Rest).
genTplVars([idnt(Nm)|Vrs],[idnt(NNm)|Rest]) :-
  genstr(Nm,NNm),
  genTplVars(Vrs,Rest).
genTplVars([_|Vrs],[idnt(Nm)|Rest]) :-
  genstr("V",Nm),
  genTplVars(Vrs,Rest).

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
  cnsName(A,ANm),
  cnsName(B,BNm),
  str_lt(ANm,BNm).

sameConstructorTriple(([A|_],_,_),([B|_],_,_)) :-
  sameConstructor(A,B).

sameConstructor(A,B) :-
  cnsName(A,Nm),
  cnsName(B,Nm).

cnsName(enum(Nm),Nm).
cnsName(lbl(Nm,_),Nm).
cnsName(ctpl(C,_),Nm) :-
  cnsName(C,Nm).

compareScalarTriple(([A|_],_,_),([B|_],_,_)) :-
  compareScalar(A,B).

compareScalar(intgr(A),intgr(B)) :-!,
  A<B.
compareScalar(bigx(A),bigx(B)) :-!,
  number_string(Ax,A),
  number_string(Bx,B),
  Ax<Bx.
compareScalar(float(A),float(B)) :-!,
  A<B.
compareScalar(chr(A),chr(B)) :-!,
  chr_lt(A,B).
compareScalar(strg(A),strg(B)) :-!,
  str_lt(A,B).
compareScalar(lbl(L1,_A1),lbl(L2,_A2)) :-
  str_lt(L1,L2),!.
compareScalar(lbl(L,A1),lbl(L,A2)) :-
  A1<A2.

sameScalarTriple(([A|_],_,_),([A|_],_,_)).

matchVars(Lc,Subber,Conder,[V|Vrs],Triples,Deflt,Map,Dp,Reslt) :-
  applyVar(V,Subber,Triples,NTriples),
  matchTriples(Lc,Subber,Conder,Vrs,NTriples,Deflt,Map,Dp,Reslt).

applyVar(_,_,[],[]).
applyVar(idnt(V),Subber,[([idnt(XV)|Args],(Lc,Bnd,Guard,Cond,Vl),Ix)|Tpls],
	 [(NArgs,(Lc,[(XV,idnt(V))|Bnd],NGuard,NCond,NVl),Ix)|NTpls]) :-
  Vrs = [(XV,idnt(V))],
  call(Subber,Vrs,Vl,NVl),
  substTerms(Vrs,Args,NArgs),
  substGoal(Vrs,Cond,NCond),
  substGoal(Vrs,Guard,NGuard),
  applyVar(idnt(V),Subber,Tpls,NTpls).
applyVar(idnt(V),Subber,[([anon|Args],(Lc,Bnd,Guard,Cond,Vl),Ix)|Tpls],
	 [(Args,(Lc,Bnd,Guard,Cond,Vl),Ix)|NTpls]) :-
  applyVar(idnt(V),Subber,Tpls,NTpls).
applyVar(idnt(V),Subber,
	 [([whr(Lcw,idnt(XV),WCond)|Args],(Lc,Bnd,Guard,Cond,Vl),Ix)|Tpls],
	 [(NArgs,(Lc,[(XV,idnt(V))|Bnd],MGuard,NCond,NVl),Ix)|NTpls]) :-
  Vrs = [(XV,idnt(V))],
  call(Subber,Vrs,Vl,NVl),
  substTerm(Vrs,WCond,NWC),
  substGoal(Vrs,Guard,NGuard),
  substGoal(Vrs,Cond,NCond),
  mergeGl(NGuard,some(NWC),Lcw,MGuard),
  substTerms(Vrs,Args,NArgs),
  applyVar(idnt(V),Subber,Tpls,NTpls).
applyVar(anon,Subber,[([anon|Args],(Lc,Bnd,Guard,Cond,Vl),Ix)|Tpls],
	 [(Args,(Lc,Bnd,Guard,Cond,Vl),Ix)|NTpls]) :-
  applyVar(anon,Subber,Tpls,NTpls).

tooDeep(D) :-
  D > 0.