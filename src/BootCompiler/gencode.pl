:- module(gencode,[genCode/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(terms).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

genCode(mdule(Pkg,Imports,Face,_Enums,Defs,_Contracts,_Impls),Text) :-
  genPkgHdr(Pkg,Face,Code,C0),
  genImports(Imports,C0,C1),
  genDefs(Defs,C1,[]),
  assemble(Code,Text).

genPkgHdr(Pkg,Face,[pkg(Pkg,Sig)|Cx],Cx) :-
  encType(Face,Sig).

genImports([],C,C).
genImports([Pkg|I],[import(Pkg)|C],Cx) :-
  genImports(I,C,Cx).

genDefs(Defs,O,Ox) :-
  rfold(Defs,gencode:genDef,O,Ox).

genDef(fnDef(Lc,Nm,Tp,[eqn(_,Args,Value)]),O,[Txt|O]) :-
  encType(Tp,Sig),
  initDict(D),
  genLbl(D,Ex,D0),
  genLbl(D0,End,D1),
  compPtnArgs(Args,0,gencode:contCont(Ex),gencode:errorCont(Lc,"failed"),D1,D2,End,C1,[iLbl(Ex)|C2],0,Stk0),
  compTerm(Value,gencode:succCont(Lc),D2,Dx,End,C2,[iLbl(End)],Stk0,_Stk),
  genEnter(Dx,C0,C1),
  dispIns(C0),
  assem([method(Nm,Sig)|C0],CdTrm),
  encode(CdTrm,Txt).

succCont(_,[iRet|Cx],Cx,Stk,Stk1) :-
  Stk1 is Stk-1.

buildArgDict([],_,D,D).
buildArgDict([A|L],C,D,Dx) :-
  buildArg(A,C,C0,D,D0),
  buildArgDict(L,C0,D0,Dx).

initDict(scope([],[],0)).
buildArg(idnt(Nm),C,Cx,scope(D,Lbs,InUse),scope([(Nm,a(C),void,void)|D],Lbs,InUse)) :-
   Cx is C+1.

isVar(Nm,Wh,scope(Vrs,_,_)) :-
  is_member((Nm,Wh,_,_),Vrs),!.

defineLclVar(Nm,Lbl,End,scope(Vrs,Lbs,InUse),scope([(Nm,l(Off),Lbl,End)|Vrs],Lbs,NInUse),Off) :-
  nextFreeOff(InUse,Off,NInUse).

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
  compTerms(A,gencode:combCont(gencode:allocCont(St),Cont),D,Dx,End,C,Cx,Stk,Stk2).
compTerm(ecll(_,Nm,A),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  MegaCont = gencode:combCont(gencode:escCont(Nm,Stk),Cont),
  compTerms(A,MegaCont,D,Dx,End,C,Cx,Stk,Stkx).
compTerm(cll(_,Nm,A),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  MegaCont = gencode:combCont(gencode:cllCont(Nm,Stk),Cont),
  compTerms(A,MegaCont,D,Dx,End,C,Cx,Stk,Stkx).
compTerm(ocall(_,Cl,Rc),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(Cl,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk0),
  compTerm(Rc,gencode:combCont(oclCont(Stk),Cont),D1,Dx,End,C0,Cx,Stk0,Stkx).
compTerm(case(_,T,Cases,Deflt),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  compCase(T,Cases,Deflt,Cont,D,Dx,End,C,Cx,Stk,Stkx).
compTerm(varNames(_,_,T),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(T,Cont,D,Dx,End,C,Cx,Stk,Stkx).
compTerm(whr(Lc,T,Cnd),Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(T,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt)|C1],Stk,Stkx),
  compCond(Cnd,Cont,gencode:errorCont(Lc,"where fail"),D1,Dx,End,C1,Cx,Stk,_).
compTerm(error(Lc,Msg),Cont,Dx,Dx,_End,C,Cx,Stk,Stkx) :-
  errorCont(Lc,Msg,C,C0,Stk,Stk1),
  call(Cont,C0,Cx,Stk1,Stkx).

compTerms([],Cont,D,D,_,C,Cx,Stk,Stkx) :-
  call(Cont,C,Cx,Stk,Stkx).
compTerms([T|Ts],Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(T,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk0),
  compTerms(Ts,Cont,D1,Dx,End,C0,Cx,Stk0,Stkx).

isGround(T) :- isLiteral(T),!.
isGround(ctpl(S,A)) :-
  isGround(S),
  (gencode:is_member(E,A) *> gencode:isGround(E)).

contCont(Lbl,C,Cx,Stk,Stk) :-
  (nonvar(Cx),Cx=[iLbl(Lbl)|_]) -> C=Cx ; C=[iJmp(Lbl)|Cx]. % some special logic here

combCont(Cnt1,Cnt2,C,Cx,Stk,Stkx) :-
  call(Cnt1,C,C0,Stk,Stk0),
  call(Cnt2,C0,Cx,Stk0,Stkx).

allocCont(Str,[iAlloc(Str),iFrame(Stk)|Cx],Cx,Stk,Stkx) :-
  popStack(Str,Stk,Stkx).

frameCont([iFrame(Stk)|Cx],Cx,Stk,Stk).

escCont(Nm,Stk0,[iEscape(Nm),iFrame(Stk)|Cx],Cx,Stk,Stkx) :-
  Stkx is Stk0+1.

cllCont(Nm,Stk0,[iCall(Nm),iFrame(Stk)|Cx],Cx,Stk,Stkx) :-
  Stkx is Stk0+1.

oclCont(Stk0,[iOCall,iFrame(Stk)|Cx],Cx,Stk,Stkx) :-
  Stkx is Stk0+1.

jmpCont(Lbl,[iJmp(Lbl)|Cx],Cx,Stk,Stk).

isJmpCont(gencode:jmpCont(Lbl),Lbl).

errorCont(Lc,Msg,[iRais(ctpl(strct("error",2),[LT,strg(Msg)]))|Cx],Cx,Stk,Stk1) :-
  locTerm(Lc,LT),
  Stk1 is Stk+1.

popStack(strct(_,Ar),St,Stx) :-
  Stx is St-Ar+1.

compPtn(Lit,Succ,Fail,D,Dx,_,[iLdC(Lit),iCmp(Fl)|C],Cx,Stk,Stkx) :-
  isLiteral(Lit),!,
  Stk1 is Stk-1,
  ptnTest(Succ,Fail,Fl,D,Dx,C,Cx,Stk1,Stkx).
compPtn(idnt(Nm),Succ,_,D,Dx,End,[iStL(Off),iLbl(Lb)|C],Cx,Stk,Stkx) :-
  genLbl(D,Lb,D0),
  defineLclVar(Nm,Lb,End,D0,Dx,Off),
  Stk1 is Stk-1,
  call(Succ,C,Cx,Stk1,Stkx).
compPtn(ctpl(St,A),Succ,Fail,D,Dx,End,[iLdC(St),iPull(1),iCLbl(Fl)|C],Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  ptnTest(gencode:contCont(Nxt),Fail,Fl,D0,D1,C,[iLbl(Nxt)|C0],Stk,Stk1),
  compPtnArgs(A,0,Succ,Fail,D1,Dx,End,C0,Cx,Stk1,Stkx).
compPtn(whr(_,P,Cnd),Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compPtn(P,gencode:contCont(Nxt),Fail,D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk1),
  compCond(Cnd,Succ,Fail,D1,Dx,C0,Cx,Stk1,Stkx).

ptnTest(Succ,Fail,Fl,D,D,C,Cx,Stk,Stkx) :-
  isJmpCont(Fail,Fl),!,
  call(Succ,C,Cx,Stk,Stkx).
ptnTest(Succ,Fail,Fl,D,Dx,C,Cx,Stk,Stkx) :-
  genLbl(D,Fl,Dx),
  call(Succ,C,[iLbl(Fl)|C1],Stk,Stkx),
  call(Fail,C1,Cx,Stk,_).

compPtnArgs([],_,Succ,_,D,D,_,C,Cx,Stk,Stkx) :-
  call(Succ,C,Cx,Stk,Stkx).
compPtnArgs([A|R],Ix,Succ,Fail,D,Dx,End,[iDup,iNth(Ix)|C],Cx,Stk,Stk1) :-
  Stki is Stk+1,
  genLbl(D,Nxt,D0),
  compPtn(A,gencode:contCont(Nxt),Fail,D0,D1,End,C,[iLbl(Nxt)|C0],Stki,Stk0),
  Ix1 is Ix+1,
  compPtnArgs(R,Ix1,Succ,Fail,D1,Dx,End,C0,Cx,Stk0,Stk1).

compCond(enum("star.core#true"),Succ,_,D,D,_End,C,Cx,Stk,Stkx) :-
  call(Succ,C,Cx,Stk,Stkx).
compCond(enum("star.core#false"),_,Fail,D,D,_End,C,Cx,Stk,Stkx) :-
  call(Fail,C,Cx,Stk,Stkx).
compCond(cnj(_,L,R),Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compCond(L,gencode:contCont(Nxt),Fail,D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk1),
  compCond(R,Succ,Fail,D1,Dx,End,C0,Cx,Stk1,Stkx).
compCond(dsj(_,L,R),Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Fl,D0),
  compCond(L,Succ,gencode:jmpCont(Fl),D0,D1,End,C,[iLbl(Fl)|C0],Stk,Stk0),
  compCond(R,Succ,Fail,D1,Dx,End,C0,Cx,Stk0,Stkx).
compCond(ng(_,Cn),Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  compCond(Cn,Fail,Succ,D,Dx,End,C,Cx,Stk,Stkx).
compCond(mtch(_,P,E),Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(E,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk0),
  compPtn(P,Succ,Fail,D1,Dx,End,C0,Cx,Stk0,Stkx).
compCond(E,Succ,Fail,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(E,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt),iBf(Fl)|C0],Stk,Stk1),
  ptnTest(Succ,Fail,Fl,D1,Dx,C0,Cx,Stk1,Stkx).

compCase(T,Cases,Deflt,Cont,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(T,gencode:contCont(Nxt),D0,D1,End,C,[iLbl(Nxt),iCase(Mx),iJmp(Dflt)|T0],Stk,Stk0),
  genLbl(D1,Dflt,D2),
  genCaseTable(Cases,Mx,Table),
  compCases(Table,0,Cont,gencode:jmpCont(Dflt),D2,D3,End,T0,Tx,Tx,[iLbl(Dflt)|C1],Stk0),
  compTerm(Deflt,Cont,D3,Dx,End,C1,Cx,Stk0,Stkx).

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

compCases([],_Ix,_Succ,_Dflt,D,D,_End,Tc,Tc,C,C,_Stk).
compCases([(Ix,Case)|Cs],Ix,Succ,Fail,D,Dx,End,[iJmp(Lbl)|Tc],Tx,C,Cx,Stk) :-
  genLbl(D,Lbl,D0),
  compCaseBranch(Case,Lbl,Succ,Fail,D0,D1,End,C,C1,Stk,_),
  H1 is Ix+1,
  compCases(Cs,H1,Succ,Fail,D1,Dx,End,Tc,Tx,C1,Cx,Stk).
compCases(Cs,Ix,Succ,Fail,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk) :-
  H1 is Ix+1,
  compCases(Cs,H1,Succ,Fail,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E)],Lbl,Succ,Fail,D,Dx,End,[iLbl(Lbl)|C],Cx,Stk,Stkx) :-!,
  genLbl(D,Nxt,D1),
  compPtn(P,gencode:contCont(Nxt),Fail,D1,D2,End,C,[iLbl(Nxt)|C1],Stk,Stk1),
  compTerm(E,Succ,D2,Dx,End,C1,Cx,Stk1,Stkx).
compCaseBranch([(P,E)|SC],Lbl,Succ,Fail,D,Dx,End,[iLbl(Lbl),iTL(Off)|C],Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D1),
  genLbl(D1,Fl,D2),
  defineLclVar("__",Lbl,End,D2,D3,Off),
  compPtn(P,gencode:contCont(Nxt),gencode:jmpCont(Fl),D3,D4,End,C,[iLbl(Nxt)|C1],Stk,Stk1),
  compTerm(E,Succ,D4,D5,End,C1,[iLbl(Fl)|C2],Stk1,Stkx),
  compMoreCase(SC,Off,Succ,Fail,D5,Dx,End,C2,Cx,Stk,_).

compMoreCase([],_,_Succ,Fail,Dx,Dx,_End,C,Cx,Stk,Stkx) :-
  call(Fail,C,Cx,Stk,Stkx).
compMoreCase([(P,E)|SC],Off,Succ,Fail,D,Dx,End,[iLdL(Off)|C],Cx,Stk,Stkx) :-
  genLbl(D,Fl,D0),
  genLbl(D0,Nxt,D1),
  compPtn(P,gencode:contCont(Nxt),gencode:jmpCont(Fl),D1,D2,End,C,[iLbl(Nxt)|C1],Stk,Stk1),
  compTerm(E,Succ,D2,D3,End,C1,[iLbl(Fl)|C2],Stk1,Stkx),
  compMoreCase(SC,Off,Succ,Fail,D3,Dx,End,C2,Cx,Stk,_).

dispIns(I) :-
  rfold(I,gencode:showIns,Chrs,[]),
  string_chars(Txt,Chrs),
  writeln(Txt).

showIns(iLbl(L),C,Cx) :-
  appStr(L,C,C0),
  appStr(":\n",C0,Cx).
showIns(A,C,C0) :-
  swritef(I,"%w\n",[A]),
  appStr(I,C,C0).
