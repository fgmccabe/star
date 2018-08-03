:- module(gencode,[genCode/3]).

:- use_module(misc).
:- use_module(types).
:- use_module(terms).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).
:- use_module(assem).
:- use_module(errors).
:- use_module(gensig).

genCode(mdule(Pkg,Imports,Face,_Enums,Defs,_Contracts,_Impls),Opts,Text) :-
  encPkg(Pkg,PT),
  encType(Face,Sig),
  initDict(D),
  genImports(Imports,ImpTpl,D,D0),
  defineGlobals(Defs,D0,D1),
  genDefs(Defs,Opts,D1,C,[]),
  mkTpl(C,Cdes),
  mkTpl([PT,strg(Sig),ImpTpl,Cdes],Tp),
  encode(Tp,Txt),
  encode(strg(Txt),Text).

genImports(Imps,ImpTpl,D,Dx) :-
  map(Imps,gencode:genImport,Els),
  mkTpl(Els,ImpTpl),
  rfold(Imps,gencode:importGlbVar,D,Dx).

genImport(import(_,Pkg,_,_,_,_),PkgTrm) :-
  encPkg(Pkg,PkgTrm).

importGlbVar(import(_,pkg(Pkg,_),faceType(Fields,_),_,_,Impls),D,Dx) :-
  rfold(Fields,gencode:importPkgField(Pkg),D,D0),
  rfold(Impls,gencode:importImpl,D0,Dx).

importPkgField(_Pkg,(_,Tp),Dx,Dx) :-
  isProgramType(Tp),!.
importPkgField(Pkg,(N,_),D,Dx) :-
  packageVarName(Pkg,N,Vn),
  defineGlbVar(Vn,D,Dx).

importImpl(imp(ImpNm,_),D,Dx) :-
  defineGlbVar(ImpNm,D,Dx).

defineGlobals(Defs,D,Dx) :-
  rfold(Defs,gencode:defGlbl,D,Dx).

defGlbl(vrDef(_,Nm,_,_),D,Dx) :-!,
  defineGlbVar(Nm,D,Dx).
defGlbl(_,D,D).

genDefs(Defs,Opts,D,O,Ox) :-
  rfold(Defs,gencode:genDef(D,Opts),Ox,O).

genDef(D,Opts,fnDef(Lc,Nm,Tp,Args,Value),O,[CdTrm|O]) :-
  encType(Tp,Sig),
  genLbl(D,Ex,D0),
  genLbl(D0,End,D1),
  buildArgs(Args,1,D1,D1a),
  genLine(Opts,Lc,C0,C1),
  compPtnArgs(Args,Lc,argCont,1,contCont(Ex),raiseCont(Lc,"failed",Opts),Opts,D1a,D2,End,C1,[iLbl(Ex)|C2],0,Stk0),
  compTerm(Value,Lc,retCont(Opts),Opts,D2,Dx,End,C2,[iLbl(End)],Stk0,_Stk),
  (is_member(showGenCode,Opts) -> dispIns([method(Nm,Sig,_Lx)|C0]);true ),
  findMaxLocal(Dx,Lx),
  assem([method(Nm,Sig,Lx)|C0],CdTrm).
genDef(D,Opts,vrDef(Lc,Nm,Tp,Value),O,[Cd|O]) :-
  encType(funType(tupleType([]),Tp),Sig),
  genLbl(D,End,D1),
  genLine(Opts,Lc,C0,C1),
  compTerm(Value,Lc,bothCont(glbCont(Nm),retCont(Opts)),Opts,D1,Dx,End,C1,[iLbl(End)],0,_Stk),
  (is_member(showGenCode,Opts) -> dispIns([method(lbl(Nm,0),Sig,Lx)|C0]);true ),
  findMaxLocal(Dx,Lx),
  assem([method(lbl(Nm,0),Sig,Lx)|C0],Cd).

glbCont(Nm,D,D,_,[iDup,iStG(Nm)|Cx],Cx,Stk,Stk).

retCont(Opts,D,D,_,C,Cx,_Stk,none) :-
  genDRtn(Opts,C,[iRet|Cx]).

dropCont(D,D,_,[iDrop|Cx],Cx,Stk,Stk1) :-
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

%defineLclVar(Nm,Lbl,End,scope(Vrs,Lbs,InUse),scope(Vrs,Lbs,InUse),Off,Cx,Cx) :-
%  is_member((Nm,l(Off),_,_),Vrs),!.
defineLclVar(Nm,Lbl,End,scope(Vrs,Lbs,InUse),
      scope([(Nm,l(Off),Lbl,End)|Vrs],Lbs,NInUse),Off,[iLocal(Nm,Lbl,End,Off)|Cx],Cx) :-
  nextFreeOff(InUse,Off,NInUse).

clearLclVar(Nm,scope(Vrs,Lbs,InUse),scope(NVrs,Lbs,NInUse)) :-
  subtract((Nm,l(Off),_,_),Vrs,NVrs),
  addToFree(Off,InUse,NInUse).

defineGlbVar(Nm,scope(Vrs,Lbs,InUse),
      scope([(Nm,g(Nm),none,none)|Vrs],Lbs,InUse)).

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

findMaxLocal(scope(Nms,_,_),Mx) :-
  rfold(Nms,gencode:localMx,0,Mx).

localMx((_,l(Off),_),M,Mx) :- !, Mx is max(Off,M).
localMx(_,M,M).

compTerm(Lit,_,Cont,_,D,Dx,End,[iLdC(Lit)|C0],Cx,Stk,Stkx) :-
  isGround(Lit),!,
  Stk1 is Stk+1,
  call(Cont,D,Dx,End,C0,Cx,Stk1,Stkx).
compTerm(idnt(Nm),_,Cont,_,D,Dx,End,C,Cx,Stk,Stkx) :-
  isVar(Nm,V,D),!,
  compVar(V,Cont,D,Dx,End,C,Cx,Stk,Stkx).
compTerm(ctpl(St,A),Lc,Cont,Opts,D,Dx,End,C,Cx,Stk,Stk2) :-
  compTerms(A,Lc,bothCont(allocCont(St),Cont),Opts,D,Dx,End,C,Cx,Stk,Stk2).
compTerm(ecll(Lc,Nm,A),OLc,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerms(A,Lc,bothCont(escCont(Nm,Stk),Cont),Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(cll(Lc,Nm,A),OLc,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerms(A,Lc,cllCont(Nm,Stk,Cont,Opts),Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(ocall(Lc,Fn,A),OLc,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerms(A,Lc,compTerm(Fn,Lc,oclCont(Stk,Cont,Opts),Opts),Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(case(Lc,T,Cases,Deflt),OLc,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCase(T,Lc,Cases,Deflt,Cont,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(varNames(Lc,Vrs,T),OLc,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  populateVarNames(Vrs,D,C0,C1),
  compTerm(T,Lc,Cont,Opts,D,Dx,End,C1,Cx,Stk,Stkx).
compTerm(whr(Lc,T,Cnd),OLc,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  genLbl(D,Nxt,D0),
  compCond(Cnd,Lc,contCont(Nxt),raiseCont(Lc,"where fail",Opts),Opts,D0,D2,End,C0,[iLbl(Nxt)|C1],Stk,Stk1),
  compTerm(T,Lc,Cont,Opts,D2,Dx,End,C1,Cx,Stk1,Stkx).
compTerm(error(Lc,Msg),_OLc,_Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  raiseCont(Lc,Msg,Opts,D,Dx,End,C,Cx,Stk,Stkx). % no continuation after an error
compTerm(cnd(Lc,T,L,R),OLc,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Cont,OC),
  compCond(T,Lc,compTerm(L,Lc,OC,Opts),compTerm(R,Lc,OC,Opts),Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(seq(Lc,L,R),OLc,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(L,Lc,bothCont(dropCont,compTerm(R,Lc,Cont,Opts)),Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compTerm(Cond,Lc,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  isCond(Cond),!,
  genLbl(D,Nx,D0),
  compCond(Cond,Lc,bothCont(succCont(),contCont(Nx)),bothCont(failCont(),contCont(Nx)),
      Opts,D0,D1,End,C,[iLbl(Nx)|C1],Stk,Stk1),
  call(Cont,D1,Dx,End,C1,Cx,Stk1,Stkx).
compTerm(T,Lc,_,_,Dx,Dx,_,C,C,Stk,Stk) :-
  reportError("cannot compile %s",[T],Lc),
  abort.

compVar(a(A),Cont,D,Dx,End,[iLdA(A)|C0],Cx,Stk,Stkx) :- !,
  Stk1 is Stk+1,
  call(Cont,D,Dx,End,C0,Cx,Stk1,Stkx).
compVar(l(X),Cont,D,Dx,End,[iLdL(X)|C0],Cx,Stk,Stkx) :- !,
  Stk1 is Stk+1,
  call(Cont,D,Dx,End,C0,Cx,Stk1,Stkx).
compVar(g(GlbNm),Cont,D,Dx,End,[iLdG(GlbNm)|C0],Cx,Stk,Stkx) :-
  Stk1 is Stk+1,
  call(Cont,D,Dx,End,C0,Cx,Stk1,Stkx).

% Terms are generated in reverse order
compTerms([],_,Cont,_,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Cont,D,Dx,End,C,Cx,Stk,Stkx).
compTerms([T|Ts],Lc,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerms(Ts,Lc,contCont(Nxt),Opts,D0,D1,End,C,[iLbl(Nxt)|C0],Stk,Stk0),
  compTerm(T,Lc,Cont,Opts,D1,Dx,End,C0,Cx,Stk0,Stkx).

contCont(Lbl,D,D,_,C,Cx,Stk,Stk) :-
  (nonvar(Cx),Cx=[iLbl(Lbl)|_]) -> C=Cx ; C=[iJmp(Lbl)|Cx]. % some special logic here

combCont([],Dx,Dx,_,Cx,Cx,Stk,Stk).
combCont([Cnt],D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Cnt,D,Dx,End,C,Cx,Stk,Stkx).
combCont([Cnt|Cs],D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Cnt,D,D0,End,C,C0,Stk,Stk0),
  combCont(Cs,D0,Dx,End,C0,Cx,Stk0,Stkx).

bothCont(L,R,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(L,D,D0,End,C,C0,Stk,Stk0),
  call(R,D0,Dx,End,C0,Cx,Stk0,Stkx).

allocCont(Str,D,D,_,[iAlloc(Str),iFrame(Stk)|Cx],Cx,Stk,Stkx) :-
  popStack(Str,Stk,Stkx).

resetCont(Lvl,D,D,_,Cx,Cx,Lvl,Lvl) :-!.
resetCont(Lvl,D,D,_,[iRst(Lvl)|Cx],Cx,_,Lvl).

escCont(Nm,Stk0,D,D,_,[iEscape(Nm),iFrame(Stkx)|Cx],Cx,_Stk,Stkx) :-
  Stkx is Stk0+1.

cllCont(Nm,_Stk0,retCont(_),Opts,Dx,Dx,_,C,Cx,_Stk,none) :-!,
  genDTail(Opts,Nm,C,[iTail(Nm)|Cx]).
cllCont(Nm,Stk0,Cont,Opts,D,Dx,End,C,Cx,_Stk,Stkx) :-
  genDCall(Opts,Nm,C,[iCall(Nm),iFrame(Stk1)|C0]),
  Stk1 is Stk0+1,
  call(Cont,D,Dx,End,C0,Cx,Stk1,Stkx).

oclCont(Stk0,retCont(_),Opts,Dx,Dx,_End,C,Cx,Stk,none) :-!,
  genDOTail(Opts,Arity,C,[iOTail(Arity)|Cx]),
  Arity is Stk-Stk0.
oclCont(Stk0,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genDOCall(Opts,Arity,C,[iOCall(Arity),iFrame(Stk1)|C0]),
  Stk1 is Stk0+1,
  Arity is Stk-Stk0,
  call(Cont,D,Dx,End,C0,Cx,Stk1,Stkx).

jmpCont(Lbl,D,D,_End,[iJmp(Lbl)|Cx],Cx,Stk,Stk).

isJmpCont(jmpCont(Lbl),Lbl).

lblCont(Lb,Cont,D,Dx,End,[iLbl(Lb)|C],Cx,Stk,Stkx) :-
  call(Cont,D,Dx,End,C,Cx,Stk,Stkx).

isBrkCont(jmpCont(_)).
isBrkCont(retCont(_)).

isSimpleCont(retCont(_)).
isSimpleCont(jmpCont(_)).
isSimpleCont(contCont(_)).
isSimpleCont(bothCont(L,R)) :-
  isSimpleCont(L),
  isSimpleCont(R).
isSimpleCont(resetCont(_)).
isSimpleCont(succCont).
isSimpleCont(failCont).

splitCont(Cont,Cont) :- isSimpleCont(Cont),!.
splitCont(Cont,onceCont(_,Cont)).

onceCont(L,Cont,D,Dx,End,[iLbl(Lb)|C],Cx,Stk,Stkx) :-
  var(L),!,
  genLbl(D,Lb,D0),
  call(Cont,D0,Dx,End,C,Cx,Stk,Stkx),
  L=(Lb,Stk,Stkx).
onceCont((Lb,Stkin,Stkout),_,D,D,_,C,Cx,Stk,Stkout) :-
  reconcileStack(Stk,Stkin,Stkout,C,[iJmp(Lb)|Cx]).

reconcileStack(_,_,none,C,C) :-!.
reconcileStack(Stk,Stk,_,C,C) :-!.
reconcileStack(Stki,Stk,_,[iRst(Stk)|C],C) :-
  Stki>Stk,!.
reconcileStack(Stki,Stk,_,C,C) :-
  reportError("cannot reconcile stacks [%w,%w]",[Stki,Stk]),
  abort.

succCont(D,D,_,[iLdC(enum("star.core#true"))|Cx],Cx,Stk,Stk1) :-
  Stk1 is Stk+1.

failCont(D,D,_,[iLdC(enum("star.core#false"))|Cx],Cx,Stk,Stk1) :-
  Stk1 is Stk+1.

nullCont(D,D,_,C,C,Stk,Stk).

insCont(Ins,D,D,_,[Ins|C],C,Stk,Stk).

raiseCont(Lc,Msg,Opts,D,Dx,End,C,Cx,Stk,none) :-
  locTerm(Lc,LT),
  mkTpl([LT,strg(Msg)],ATrm),
  compTerm(ATrm,Lc,insCont(iAbort),Opts,D,Dx,End,C,Cx,Stk,_).

indexCont(Ix,D,D,_,[iDup,iNth(Ix)|Cx],Cx).

argCont(Ix,D,D,_,[iLdA(Ix)|Cx],Cx).

chLine(_,Lc,Lc,C,C) :- !.
chLine(Opts,_,Lc,C,Cx) :-
  genLine(Opts,Lc,C,Cx).

genLine(Opts,Lc,[iDLine(Lt)|Cx],Cx) :-
  is_member(debugging,Opts),!,
  locTerm(Lc,Lt).
genLine(_,_,Cx,Cx).

genDCall(Opts,Lt,[iDCall(Lt)|Cx],Cx) :-
  is_member(debugging,Opts),!.
genDCall(_,_,Cx,Cx).

genDOCall(Opts,Arity,[iDOCall(Arity)|Cx],Cx) :-
  is_member(debugging,Opts),!.
genDOCall(_,_,Cx,Cx).

genDTail(Opts,Lt,[iDTail(Lt)|Cx],Cx) :-
  is_member(debugging,Opts),!.
genDTail(_,_,Cx,Cx).

genDOTail(Opts,Arity,[iDOTail(Arity)|Cx],Cx) :-
  is_member(debugging,Opts),!.
genDOTail(_,_,Cx,Cx).

genDRtn(Opts,[iDRet|Cx],Cx) :-
  is_member(debugging,Opts),!.
genDRtn(_,Cx,Cx).

popStack(lbl(_,Ar),St,Stx) :-
  Stx is St-Ar+1.

compPtn(Lit,_,Succ,Fail,_Opts,D,Dx,End,[iLdC(Lit),iCmp(Fl)|C],Cx,Stk,Stkx) :-
  isLiteral(Lit),!,
  Stk1 is Stk-1,
  ptnTest(Succ,Fail,Fl,D,Dx,End,C,Cx,Stk1,Stkx).
compPtn(idnt(Nm),_,Succ,_,_Opts,D,Dx,End,[iStL(Off),iLbl(Lb)|C],Cx,Stk,Stkx) :-
  genLbl(D,Lb,D0),
  defineLclVar(Nm,Lb,End,D0,D1,Off,C,C0),
  Stk1 is Stk-1,
  call(Succ,D1,Dx,End,C0,Cx,Stk1,Stkx).
compPtn(ctpl(St,A),Lc,Succ,Fail,Opts,D,Dx,End,[iDup,iLdC(St),iCLbl(Nxt),iLbl(FLb),iRst(Stk0)|C],Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  Stk0 is Stk-1,
  genLbl(D0,FLb,D1),
  call(Fail,D1,D2,End,C,[iLbl(Nxt)|C1],Stk0,_Stkx),
  compPtnArgs(A,Lc,indexCont,0,bothCont(resetCont(Stk0),Succ),jmpCont(FLb),Opts,D2,Dx,End,C1,Cx,Stk,Stkx).
compPtn(whr(Lc,P,Cnd),OLc,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  chLine(Opts,OLc,Lc,C,C0),
  compPtn(P,Lc,contCont(Nxt),Fail,Opts,D0,D1,End,C0,[iLbl(Nxt)|C1],Stk,Stk1),
  verify(Stk1=:=Stk-1,"where stack"),
  compCond(Cnd,Lc,Succ,Fail,Opts,D1,Dx,End,C1,Cx,Stk1,Stkx).
compPtn(T,Lc,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D0),
  compTerm(T,Lc,contCont(Nxt),Opts,D0,D1,End,C,[iLbl(Nxt),iCmp(Fl)|C0],Stk,Stk1),
  Stk2 is Stk1-2,
  verify(Stk2=:=Stk-1,"Stk2 off"),
  ptnTest(Succ,Fail,Fl,D1,Dx,End,C0,Cx,Stk2,Stkx).

ptnTest(Succ,Fail,Fl,D,Dx,End,C,Cx,Stk,Stkx) :-
  isJmpCont(Fail,Fl),!,
  call(Succ,D,Dx,End,C,Cx,Stk,Stkx).
ptnTest(Succ,Fail,Fl,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Fl,D0),
  call(Succ,D0,D1,End,C,[iLbl(Fl)|C1],Stk,Stk1),
  call(Fail,D1,Dx,End,C1,Cx,Stk,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"ptn test").

testCont(Succ,Fail,D,Dx,End,[iBf(Fl)|C],Cx,Stk,Stkx) :-
  genLbl(D,Fl,D0),
  Stk0 is Stk-1,
  call(Succ,D0,D1,End,C,[iLbl(Fl)|C0],Stk0,Stk1),
  call(Fail,D1,Dx,End,C0,Cx,Stk0,Stk2),
  mergeStkLvl(Stk1,Stk2,Stkx,"test exp").

mergeStkLvl(none,Stk,Stk,_).
mergeStkLvl(Stk,none,Stk,_).
mergeStkLvl(Stk1,Stk2,Stk1,Msg) :-
  verify(Stk1=:=Stk2,Msg).

compPtnArgs([],_,_,_,Succ,_,_,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Succ,D,Dx,End,C,Cx,Stk,Stkx).
compPtnArgs([A|R],Lc,ArgCont,Ix,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stk1) :-
  genLbl(D,Nxt,D0),
  compPtnArg(A,Ix,Lc,ArgCont,contCont(Nxt),Fail,Opts,D0,D1,End,C,[iLbl(Nxt)|C1],Stk,Stk0),
  Ix1 is Ix+1,
  verify(Stk=:=Stk0,"argument pattern"),
  compPtnArgs(R,Lc,ArgCont,Ix1,Succ,Fail,Opts,D1,Dx,End,C1,Cx,Stk0,Stk1).

compPtnArg(idnt(V),Ix,_,_,Succ,_Fail,_,D,Dx,End,C,Cx,Stk,Stkx) :-
  isVar(V,a(Ix),D),!,
  call(Succ,D,Dx,End,C,Cx,Stk,Stkx).
compPtnArg(P,Ix,Lc,ArgCont,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(ArgCont,Ix,D,D0,End,C,C0),
  Stki is Stk+1,
  compPtn(P,Lc,Succ,Fail,Opts,D0,Dx,End,C0,Cx,Stki,Stkx).

isCond(cnj(_,_,_)).
isCond(cnd(_,_,_,_)).
isCond(dsj(_,_,_)).
isCond(ng(_,_)).
isCond(mtch(_,_,_)).

compCond(enum("star.core#true"),_Lc,Succ,_Fail,_Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Succ,D,Dx,End,C,Cx,Stk,Stkx).
compCond(enum("star.core#false"),_,_,Fail,_,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Fail,D,Dx,End,C,Cx,Stk,Stkx).
compCond(cnj(Lc,L,R),OLc,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  splitCont(Fail,OF),
  compCond(L,Lc,compCond(R,Lc,Succ,OF,Opts),OF,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(dsj(Lc,L,R),OLc,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  splitCont(Succ,OSc),
  chLine(Opts,OLc,Lc,C,C0),
  compCond(L,Lc,OSc,compCond(R,Lc,OSc,Fail,Opts),Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(ng(Lc,Cn),OLc,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compCond(Cn,Lc,Fail,Succ,Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(cnd(Lc,T,L,R),OLc,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  splitCont(Succ,OSc),
  splitCont(Fail,OFl),
  chLine(Opts,OLc,Lc,C,C0),
  compCond(T,Lc,compCond(L,Lc,OSc,OFl,Opts),compCond(R,Lc,OSc,OFl,Opts),Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(mtch(Lc,P,E),OLc,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  chLine(Opts,OLc,Lc,C,C0),
  compTerm(E,Lc,compPtn(P,Lc,Succ,Fail,Opts),Opts,D,Dx,End,C0,Cx,Stk,Stkx).
compCond(E,Lc,Succ,Fail,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  compTerm(E,Lc,testCont(Succ,Fail),Opts,D,Dx,End,C,Cx,Stk,Stkx).

compCase(T,Lc,Cases,Deflt,Cont,Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D1),
  splitCont(Cont,OC),
  compTerm(T,Lc,contCont(Nxt),Opts,D1,D2,End,C,[iLbl(Nxt),iCase(Mx),iJmp(Dflt)|T0],Stk,Stk0),
  genLbl(D2,Dflt,D3),
  genCaseTable(Cases,Mx,Table),
  compCases(Table,0,Mx,OC,contCont(Dflt),Dflt,Opts,D3,D4,End,T0,Tx,Tx,[iLbl(Dflt),iRst(Stk)|C1],Stk0),
  compTerm(Deflt,Lc,OC,Opts,D4,Dx,End,C1,Cx,Stk,Stkx).

genCaseTable(Cases,P,Table) :-
  length(Cases,L),
  nextPrime(L,P),
  caseHashes(Cases,P,Hs),
  sortCases(Hs,Table).

caseHashes([],_,[]).
caseHashes([(P,E,Lc)|Cases],Mx,[(P,Hx,E,Lc)|C]) :-
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

compCases([],Mx,Mx,_Succ,_,_Dflt,_Opts,D,D,_End,Tc,Tc,C,C,_Stk).
compCases([],Ix,Mx,Succ,Fail,Dflt,Opts,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk) :-
  Ix1 is Ix+1,
  compCases([],Ix1,Mx,Succ,Fail,Dflt,Opts,D,Dx,End,Tc,Tx,C,Cx,Stk).
compCases([(Ix,Case)|Cs],Ix,Mx,Succ,Fail,Dflt,Opts,D,Dx,End,[iJmp(Lbl)|Tc],Tx,C,Cx,Stk) :-!,
  genLbl(D,Lbl,D0),
  compCaseBranch(Case,Lbl,Succ,Fail,Opts,D0,D1,End,C,C1,Stk,_),
  H1 is Ix+1,
  compCases(Cs,H1,Mx,Succ,Fail,Dflt,Opts,D1,Dx,End,Tc,Tx,C1,Cx,Stk).
compCases(Cs,Ix,Mx,Succ,Fail,Dflt,Opts,D,Dx,End,[iJmp(Dflt)|Tc],Tx,C,Cx,Stk) :-
  H1 is Ix+1,
  compCases(Cs,H1,Mx,Succ,Fail,Dflt,Opts,D,Dx,End,Tc,Tx,C,Cx,Stk).

% two cases to consider: hash collision or no hash collision
compCaseBranch([(P,E,Lc)],Lbl,Succ,Fail,Opts,D,Dx,End,[iLbl(Lbl)|C],Cx,Stk,Stkx) :-!,
  genLbl(D,Nxt,D1),
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,contCont(Nxt),Fail,Opts,D1,D2,End,C0,[iLbl(Nxt)|C1],Stk,Stk1),
  compTerm(E,Lc,Succ,Opts,D2,Dx,End,C1,Cx,Stk1,Stkx).
compCaseBranch([(P,E,Lc)|SC],Lbl,Succ,Fail,Opts,D,Dx,End,[iLbl(Lbl),iTL(Off),iLbl(VLb)|C],Cx,Stk,Stkx) :-
  genLbl(D,Nxt,D1),
  genLbl(D1,Fl,D2),
  genLbl(D2,VLb,D3),
  defineLclVar("__",VLb,End,D3,D4,Off,C,C0),
  genLine(Opts,Lc,C0,C1),
  compPtn(P,Lc,contCont(Nxt),contCont(Fl),Opts,D4,D5,End,C1,[iLbl(Nxt)|C2],Stk,Stk1),
  verify(Stk-1=:=Stk1,"case branch"),
  compTerm(E,Lc,Succ,Opts,D5,D6,End,C2,[iLbl(Fl),iRst(Stk1)|C3],Stk1,Stk2),
  compMoreCase(SC,Off,Succ,Fail,Opts,D6,Dx,End,C3,Cx,Stk1,Stk3),
  mergeStkLvl(Stk2,Stk3,Stkx,"case branch stack").

compMoreCase([],_,_Succ,Fail,_Opts,D,Dx,End,C,Cx,Stk,Stkx) :-
  call(Fail,D,Dx,End,C,Cx,Stk,Stkx).
compMoreCase([(P,E,Lc)|SC],VLb,Succ,Fail,Opts,D,Dx,End,[iLdL(VLb)|C],Cx,Stk,Stkx) :-
  genLbl(D,Fl,D0),
  genLbl(D0,Nxt,D1),
  Stk0 is Stk+1,
  genLine(Opts,Lc,C,C0),
  compPtn(P,Lc,contCont(Nxt),contCont(Fl),Opts,D1,D2,End,C0,[iLbl(Nxt)|C1],Stk0,Stk1),
  verify(Stk=:=Stk1,"more case branch"),
  compTerm(E,Lc,Succ,Opts,D2,D3,End,C1,[iLbl(Fl),iRst(Stk)|C2],Stk,Stk2),
  compMoreCase(SC,VLb,Succ,Fail,Opts,D3,Dx,End,C2,Cx,Stk,Stk3),
  mergeStkLvl(Stk2,Stk3,Stkx,"more case branch stack").

/*
dispIns(Nm,_Sig,I) :-
  showTerm(Nm,0,C,C0),
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
*/
