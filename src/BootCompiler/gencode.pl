:- module(gencode,[genCode/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(terms).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).
:- use_module(assem).
:- use_module(errors).
:- use_module(gensig).

genCode(mdule(Pkg,Imports,Face,_Enums,Defs,_Contracts,_Impls),Text) :-
  encPkg(Pkg,PT),
  encType(Face,Sig),
  genImports(Imports,ImpTpl),
  genDefs(Defs,C,[]),
  mkTpl(C,Cdes),
  mkTpl([PT,strg(Sig),ImpTpl,Cdes],Tp),
  encode(Tp,Txt),
  encode(strg(Txt),Text).

genImports(Imps,ImpTpl) :-
  map(Imps,gencode:genImport,Els),
  mkTpl(Els,ImpTpl).

genImport(Pkg,PkgTrm) :-
  encPkg(Pkg,PkgTrm).

genDefs(Defs,O,Ox) :-
  rfold(Defs,gencode:genDef,Ox,O).

genDef(fnDef(Lc,Nm,Tp,[eqn(_,Args,Value)]),O,[CdTrm|O]) :-
  encType(Tp,Sig),
  initDict(D),
  genLbl(D,Ex,D0),
  genLbl(D0,End,D1),
  buildArgs(Args,1,D1,D1a),
  compPtnArgs(Args,gencode:argCont,1,gencode:contCont(Ex),gencode:errorCont(Lc,"failed"),D1a,D2,End,C1,[iLbl(Ex)|C2],0,Stk0),
  compTerm(Value,gencode:retCont(Lc),D2,Dx,End,C2,[iLbl(End)],Stk0,_Stk),
  genEnter(Dx,C0,C1),
  dispIns(Nm,Sig,C0),
  assem([method(Nm,Sig)|C0],CdTrm).

retCont(_,[iRet|Cx],Cx,Stk,Stk1) :-
  Stk1 is Stk-1.

initDict(scope([],[],0)).

buildArgs([],_,D,D).
buildArgs([A|R],Ix,D,Dx) :-
  buildArg(A,Ix,D,D0),
  Ix1 is Ix+1,
  buildArgs(R,Ix1,D0,Dx).

buildArg(idnt(Nm),Ix,scope(D,Lbs,InUse),scope([(Nm,a(Ix),void,void)|D],Lbs,InUse)).
buildArg(_,_,D,D).

isVar(Nm,Wh,scope(Vrs,_,_)) :-
  is_member((Nm,Wh,_,_),Vrs),!.

varLabels(scope(Vrs,_,_),Vrs).

defineLclVar(Nm,Lbl,End,scope(Vrs,Lbs,InUse),
      scope([(Nm,l(Off),Lbl,End)|Vrs],Lbs,NInUse),Off,[iLocal(Nm,Lbl,End,Off)|Cx],Cx) :-
  nextFreeOff(InUse,Off,NInUse).

populateVarNames([],_,C,C).
populateVarNames([(Nw,idnt(Ex))|Vs],D,C,Cx) :-
  populateVarNm(Nw,Ex,D,C,C0),
  populateVarNames(Vs,D,C0,Cx).

populateVarNm(Nw,Ex,scope(Vrs,_,_),[iLocal(Nw,Frm,End,Off)|Cx],Cx) :-
  is_member((Ex,l(Off),Frm,End),Vrs),!.
populateVarNm(_,Ex,scope(Vrs,_,_),Cx,Cx) :-
  is_member((Ex,a(_),_,_),Vrs),!.
populateVarNm(_,Ex,_,C,C) :-
  reportError("variable %s not known",[Ex]).

nextFreeOff(InUse,Nxt,Nxt) :-
  Nxt is InUse+1.

genLbl(scope(Nms,Lbs,IU),Lb,scope(Nms,[Lb|Lbs],IU)) :-
  length(Lbs,N),
  swritef(Lb,"_L%d",[N]).

genEnter(D,[iEnter(L)|Cx],Cx) :-
  findMaxLocal(D,L).

findMaxLocal(scope(Nms,_,_),Mx) :-
  rfold(Nms,gencode:localMx,0,Mx).

localMx((_,l(Off),_),M,Mx) :- !, Mx is max(Off,M).
localMx(_,M,M).

compTerm(Lit,Cont,D,D,_End,[iLdC(Lit)|C0],Cx,Stk,Stkx) :-
  isGround(Lit),!,
  Stk1 is Stk+1,
  call(Cont,C0,Cx,Stk1,Stkx).
compTerm(idnt(Nm),Cont,D,D,_End,[iLdA(A)|C0],Cx,Stk,Stkx) :-
  isVar(Nm,a(A),D),!,
  Stk1 is Stk+1,
  call(Cont,C0,Cx,Stk1,Stkx).
compTerm(idnt(Nm),Cont,D,D,_End,[iLdL(X)|C0],Cx,Stk,Stkx) :-
  isVar(Nm,l(X),D),!,
  Stk1 is Stk+1,
  call(Cont,C0,Cx,Stk1,Stkx).
compTerm(ctpl(St,A),Cont,D,Dx,End,C,Cx,Stk,Stk2) :-
  compTerms(A,gencode:combCont([gencode:allocCont(St),Cont]),D,Dx,End,C,Cx,Stk,Stk2).
compTerm(ecll(Lc,Nm,A),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  lineCont(Lc,C,C0,Stk,Stk0),
  MegaCont = gencode:combCont([gencode:escCont(Nm,Stk),Cont]),
  compTerms(A,MegaCont,D,Dx,End,C0,Cx,Stk0,Stkx).
compTerm(cll(Lc,Nm,A),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  MegaCont = gencode:combCont([gencode:lineCont(Lc),gencode:cllCont(Nm,Stk),Cont]),
  compTerms(A,MegaCont,D,Dx,End,C,Cx,Stk,Stkx).
compTerm(ocall(Lc,Cl,Rc),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(Rc,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk0),
  compTerm(Cl,gencode:combCont([gencode:lineCont(Lc),oclCont(Stk),Cont]),D1,Dx,End,C0,Cx,Stk0,Stkx).
compTerm(case(Lc,T,Cases,Deflt),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  lineCont(Lc,C,C0,Stk,Stk0),
  compCase(T,Cases,Deflt,Cont,D,Dx,End,C0,Cx,Stk0,Stkx).
compTerm(varNames(Lc,Vrs,T),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  lineCont(Lc,C,C0,Stk,Stk0),
  populateVarNames(Vrs,D,C0,C1),
  compTerm(T,Cont,D,Dx,End,C1,Cx,Stk0,Stkx).
compTerm(whr(Lc,T,Cnd),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  lineCont(Lc,C,C0,Stk,Stk0),
  compCond(Cnd,gencode:contCont(Nxt),gencode:errorCont(Lc,"where fail"),D0,D1,End,C0,[iLbl(Nxt)|C1],Stk0,Stk1),
  compTerm(T,Cont,D1,Dx,End,C1,Cx,Stk1,Stkx).
compTerm(error(Lc,Msg),Cont,Dx,Dx,_End,C,Cx,Stk,Stkx) :-
  errorCont(Lc,Msg,C,C0,Stk,Stk1),
  call(Cont,C0,Cx,Stk1,Stkx).
compTerm(Cond,Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  isCond(Cond),!,
  compCond(Cond,gencode:combCont([gencode:succCont(),Cont]),
      gencode:combCont([gencode:failCont(),Cont]),
      D,Dx,End,C,Cx,Stk,Stkx).
compTerm(T,_,Dx,Dx,_,C,C,Stk,Stk) :-
  dispTerm(T,Tx),
  reportError("cannot compile %s",[Tx]).

% Terms are generated in reverse order
compTerms([],Cont,D,D,_,C,Cx,Stk,Stkx) :-
  call(Cont,C,Cx,Stk,Stkx).
compTerms([T|Ts],Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerms(Ts,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk0),
  compTerm(T,Cont,D1,Dx,End,C0,Cx,Stk0,Stkx).

isGround(T) :- isLiteral(T),!.
isGround(ctpl(S,A)) :-
  isGround(S),
  forall(gencode:is_member(E,A), gencode:isGround(E)).

contCont(Lbl,C,Cx,Stk,Stk) :-
  (nonvar(Cx),Cx=[iLbl(Lbl)|_]) -> C=Cx ; C=[iJmp(Lbl)|Cx]. % some special logic here

combCont([],Cx,Cx,Stk,Stk).
combCont([Cnt|Cs],C,Cx,Stk,Stkx) :-
  call(Cnt,C,C0,Stk,Stk0),
  combCont(Cs,C0,Cx,Stk0,Stkx).

allocCont(Str,[iAlloc(Str),iFrame(Stk)|Cx],Cx,Stk,Stkx) :-
  popStack(Str,Stk,Stkx).

verifyCont(Lvl,Msg,C,C,Stk,Stk) :-
  verify(Stk=Lvl,Msg).

resetCont(Lvl,[iRst(Lvl)|Cx],Cx,_,Lvl).

frameCont([iFrame(Stk)|Cx],Cx,Stk,Stk).

escCont(Nm,Stk0,[iEscape(Nm),iFrame(Stk)|Cx],Cx,Stk,Stkx) :-
  Stkx is Stk0+1.

cllCont(Nm,Stk0,[iCall(Nm),iFrame(Stk)|Cx],Cx,Stk,Stkx) :-
  Stkx is Stk0+1.

oclCont(Stk0,[iOCall,iFrame(Stk)|Cx],Cx,Stk,Stkx) :-
  Stkx is Stk0+1.

jmpCont(Lbl,[iJmp(Lbl)|Cx],Cx,Stk,Stk).

isJmpCont(gencode:jmpCont(Lbl),Lbl).

succCont([iLdC(enum("star.core#true"))|Cx],Cx,Stk,Stk1) :-
  Stk1 is Stk+1.

failCont([iLdC(enum("star.core#true"))|Cx],Cx,Stk,Stk1) :-
  Stk1 is Stk+1.

errorCont(Lc,Msg,[iRais(ctpl(lbl("error",2),[LT,strg(Msg)]))|Cx],Cx,Stk,Stk1) :-
  locTerm(Lc,LT),
  Stk1 is Stk+1.

lineCont(Lc,[iLine(Lt)|Cx],Cx,Stk,Stk) :-
  locTerm(Lc,Lt).

indexCont(Ix,[iDup,iNth(Ix)|Cx],Cx).

argCont(Ix,[iLdA(Ix)|Cx],Cx).

popStack(lbl(_,Ar),St,Stx) :-
  Stx is St-Ar+1.

compPtn(Lit,Succ,Fail,D,Dx,_,[iLdC(Lit),iCmp(Fl)|C],Cx,Stk,Stkx) :-
  isLiteral(Lit),!,
  Stk1 is Stk-1,
  ptnTest(Succ,Fail,Fl,D,Dx,C,Cx,Stk1,Stkx).

compPtn(idnt(Nm),Succ,_,D,Dx,End,[iStL(Off),iLbl(Lb)|C],Cx,Stk,Stkx) :-
  genLbl(D,Lb,D0),
  defineLclVar(Nm,Lb,End,D0,Dx,Off,C,C0),
  Stk1 is Stk-1,
  call(Succ,C0,Cx,Stk1,Stkx),
  verify(Stk-1=:=Stkx,"ident stack").
compPtn(ctpl(St,A),Succ,Fail,D,Dx,End,[iDup,iLdC(St),iCLbl(Fl)|C],Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  ptnTest(gencode:contCont(Nxt),Fail,Fl,D0,D1,C,[iLbl(Nxt)|C0],Stk,Stk1),
  Stk0 is Stk-1,
  compPtnArgs(A,gencode:indexCont,0,gencode:combCont([gencode:resetCont(Stk0),Succ]),Fail,D1,Dx,End,C0,Cx,Stk1,Stkx),
  verify(Stkx=:=Stk0,"cpt stack").
compPtn(whr(_,P,Cnd),Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compPtn(P,gencode:contCont(Nxt),Fail,D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk1),
  verify(Stk1=:=Stk-1,"where stack"),
  compCond(Cnd,Succ,Fail,D1,Dx,C0,Cx,Stk1,Stkx),
  verify(Stkx=Stk1,"cond stack").
compPtn(T,Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(T,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt),iCmp(Fl)|C0],Stk,Stk1),
  Stk2 is Stk1-2,
  verify(Stk2=:=Stk-1,"Stk2 off"),
  ptnTest(Succ,Fail,Fl,D1,Dx,C0,Cx,Stk2,Stkx).

ptnTest(Succ,Fail,Fl,D,D,C,Cx,Stk,Stkx) :-
  isJmpCont(Fail,Fl),!,
  call(Succ,C,Cx,Stk,Stkx).
ptnTest(Succ,Fail,Fl,D,Dx,C,Cx,Stk,Stkx) :-
  genLbl(D,Fl,Dx),
  call(Succ,C,[iLbl(Fl)|C1],Stk,Stkx),
  call(Fail,C1,Cx,Stk,_).

compPtnArgs([],_,_,Succ,_,D,D,_,C,Cx,Stk,Stkx) :-
  call(Succ,C,Cx,Stk,Stkx).
compPtnArgs([A|R],ArgCont,Ix,Succ,Fail,D,Dx,End,C,Cx,Stk,Stk1) :-
  genLbl(D,Nxt,D0),
  compPtnArg(A,Ix,ArgCont,gencode:contCont(Nxt),Fail,D0,D1,End,C,[iLbl(Nxt)|C1],Stk,Stk0),
  Ix1 is Ix+1,
  compPtnArgs(R,ArgCont,Ix1,Succ,Fail,D1,Dx,End,C1,Cx,Stk0,Stk1).

compPtnArg(idnt(V),Ix,_,Succ,_Fail,D,D,_End,C,Cx,Stk,Stkx) :-
  isVar(V,a(Ix),D),!,
  call(Succ,C,Cx,Stk,Stkx).
compPtnArg(P,Ix,ArgCont,Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(ArgCont,Ix,C,C0),
  Stki is Stk+1,
  compPtn(P,Succ,Fail,D,Dx,End,C0,Cx,Stki,Stkx).

isCond(cnj(_,_,_)).
isCond(dsj(_,_,_)).
isCond(ng(_,_)).
isCond(mtch(_,_,_)).

compCond(enum("star.core#true"),Succ,_,D,D,_End,C,Cx,Stk,Stkx) :-
  call(Succ,C,Cx,Stk,Stkx).
compCond(enum("star.core#false"),_,Fail,D,D,_End,C,Cx,Stk,Stkx) :-
  call(Fail,C,Cx,Stk,Stkx).
compCond(cnj(_,enum("star.core#true"),R),Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  compCond(R,Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx).
compCond(cnj(_,L,R),Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compCond(L,gencode:contCont(Nxt),Fail,D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk1),
  compCond(R,Succ,Fail,D1,Dx,End,C0,Cx,Stk1,Stkx).
compCond(dsj(_,L,R),Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Fl,D0),
  compCond(L,Succ,gencode:jmpCont(Fl),D0,D1,End,C,[iLbl(Fl),iRst(Stk)|C0],Stk,Stk0),
  compCond(R,Succ,Fail,D1,Dx,End,C0,Cx,Stk0,Stkx).
compCond(ng(_,Cn),Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  compCond(Cn,Fail,Succ,D,Dx,End,C,Cx,Stk,Stkx).
compCond(mtch(_,P,E),Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(E,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk0),
  compPtn(P,gencode:combCont([resetCont(Stk),Succ]),gencode:combCont([resetCont(Stk),Fail]),D1,Dx,End,C0,Cx,Stk0,Stkx).
compCond(E,Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(E,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt),iBf(Fl)|C0],Stk,Stk1),
  verify(Stk1=:=Stk+1,"general cond"),
  ptnTest(Succ,Fail,Fl,D1,Dx,C0,Cx,Stk,Stkx).

compCase(T,Cases,Deflt,Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(T,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt),iCase(Mx),iJmp(Dflt)|T0],Stk,Stk0),
  genLbl(D1,Dflt,D2),
  genCaseTable(Cases,Mx,Table),
  compCases(Table,0,Mx,Cont,gencode:jmpCont(Dflt),Dflt,D2,D3,End,T0,Tx,Tx,[iLbl(Dflt),iRst(Stk)|C1],Stk0),
  compTerm(Deflt,Cont,D3,Dx,End,C1,Cx,Stk,Stkx).

genCaseTable(Cases,P,Table) :-
  length(Cases,L),
  nextPrime(L,P),
  caseHashes(Cases,P,Hs),
  sortCases(Hs,Table).

caseHashes([],_,[]).
caseHashes([(P,E)|Cases],Mx,[(P,Hx,E)|C]) :-
  caseHash(P,Mx,Hx),
  caseHashes(Cases,Mx,C).

caseHash(T,Mx,Hx) :-
  isLiteral(T),
  termHash(T,Hs),
  Hx is Hs mod Mx.
caseHash(ctpl(O,_),Mx,Hx) :-
  caseHash(O,Mx,Hx).
caseHash(whr(_,P,_),Mx,Hx) :-
  caseHash(P,Mx,Hx).

sortCases(Cases,Sorted) :-
  sort(Cases,gencode:compareHash,SS),
  mergeDuplicates(SS,Sorted).

compareHash((_,H1,_),(_,H2,_)) :- H1<H2.

mergeDuplicates([],[]).
mergeDuplicates([(P,H,E)|M],[(H,[(P,E)|D])|Mr]) :-
  mergeDuplicate(M,H,D,Rs),
  mergeDuplicates(Rs,Mr).

mergeDuplicate([(P,H,E)|M],H,[(P,E)|Ds],Rs) :-!,
  mergeDuplicate(M,H,Ds,Rs).
mergeDuplicate(M,_,[],M).

compCases([],Mx,Mx,_Succ,_,_Dflt,D,D,_End,Tc,Tc,C,C,_Stk).
compCases([],Ix,Mx,Succ,Fail,Dflt,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk) :-
  Ix1 is Ix+1,
  compCases([],Ix1,Mx,Succ,Fail,Dflt,D,Dx,End,Tc,Tx,C,Cx,Stk).
compCases([(Ix,Case)|Cs],Ix,Mx,Succ,Fail,Dflt,D,Dx,End,[iJmp(Lbl)|Tc],Tx,C,Cx,Stk) :-
  genLbl(D,Lbl,D0),
  compCaseBranch(Case,Lbl,Succ,Fail,D0,D1,End,C,C1,Stk,_),
  H1 is Ix+1,
  compCases(Cs,H1,Mx,Succ,Fail,Dflt,D1,Dx,End,Tc,Tx,C1,Cx,Stk).
compCases(Cs,Ix,Mx,Succ,Fail,Dflt,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk) :-
  H1 is Ix+1,
  compCases(Cs,H1,Mx,Succ,Fail,Dflt,D,Dx,End,Tc,Tx,C,Cx,Stk).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E)],Lbl,Succ,Fail,D,Dx,End,[iLbl(Lbl)|C],Cx,Stk,Stkx) :-!,
  genLbl(D,Nxt,D1),
  compPtn(P,gencode:contCont(Nxt),Fail,D1,D2,End,C,[iLbl(Nxt)|C1],Stk,Stk1),
  compTerm(E,Succ,D2,Dx,End,C1,Cx,Stk1,Stkx).
compCaseBranch([(P,E)|SC],Lbl,Succ,Fail,D,Dx,End,[iLbl(Lbl),iTL(Off),iLbl(VLb)|C],Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D1),
  genLbl(D1,Fl,D2),
  genLbl(D2,VLb,D3),
  defineLclVar("__",VLb,End,D3,D4,Off,C,C0),
  compPtn(P,gencode:contCont(Nxt),gencode:jmpCont(Fl),D4,D5,End,C0,[iLbl(Nxt)|C1],Stk,Stk1),
  verify(Stk-1=:=Stk1,"case branch"),
  compTerm(E,Succ,D5,D6,End,C1,[iLbl(Fl),iRst(Stk)|C2],Stk1,Stkx),
  compMoreCase(SC,Off,Succ,Fail,D6,Dx,End,C2,Cx,Stk,Stky),
  verify(Stky=Stky,"mismatch in case stack").

compMoreCase([],_,_Succ,Fail,Dx,Dx,_End,C,Cx,Stk,Stkx) :-
  call(Fail,C,Cx,Stk,Stkx).
compMoreCase([(P,E)|SC],Off,Succ,Fail,D,Dx,End,[iLdL(Off)|C],Cx,Stk,Stkx) :-
  genLbl(D,Fl,D0),
  genLbl(D0,Nxt,D1),
  compPtn(P,gencode:contCont(Nxt),gencode:jmpCont(Fl),D1,D2,End,C,[iLbl(Nxt)|C1],Stk,Stk1),
  verify(Stk-1=:=Stk1,"more case branch"),
  compTerm(E,Succ,D2,D3,End,C1,[iLbl(Fl),iRst(Stk)|C2],Stk1,Stkx),
  compMoreCase(SC,Off,Succ,Fail,D3,Dx,End,C2,Cx,Stk,_).

dispIns(Nm,_Sig,I) :-
  showTerm(Nm,C,C0),
  appStr(":\n",C0,C1),
  rfold(I,gencode:showIns,C1,[]),
  string_chars(Txt,C),
  writeln(Txt).

showIns(iLbl(L),C,Cx) :-
  appStr(L,C,C0),
  appStr(":\n",C0,Cx).
showIns(A,C,C0) :-
  swritef(I,"%w\n",[A]),
  appStr(I,C,C0).
