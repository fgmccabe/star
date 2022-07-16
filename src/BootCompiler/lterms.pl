:- module(lterms,[ssTransformed/2,
		  dispRuleSet/1,dispProg/1,dispEquations/1,
		  substTerm/3,substGoal/3,substAction/3,
		  substTerms/3,rewriteTerm/3,
		  rewriteAction/3,
		  genTplStruct/2,isLiteral/1,isCnd/1,mkTpl/2,
		  isTplLbl/2,mkCons/3,
		  isUnit/1,
		  termHash/2,
		  ssTrm/3,dispTerm/1,showTerm/4,locTerm/2,dispAct/1,
		  idInTerm/2, 
		  mergeGl/4,
		  validLProg/2]).

:- use_module(display).
:- use_module(misc).
:- use_module(canon).
:- use_module(operators).
:- use_module(location).
:- use_module(types).
:- use_module(escapes).
:- use_module(intrinsics).
:- use_module(errors).

mergeGl(none,G,_,G).
mergeGl(some(G),none,_,some(G)).
mergeGl(some(G1),some(G2),Lc,some(cnj(Lc,G1,G2))).

ssTransformed(mdule(Pkg,_Imports,_,_,Defs),
	      sq([ss("Package "),canon:ssPkg(Pkg),nl(0),iv(nl(0),Rs)])):-
  map(Defs,lterms:ssRuleSet,Rs).

ssRuleSet(fnDef(_Lc,Nm,H,_Tp,Args,Value),sq([ss(HH),ss("Fn: "),NN,lp,AA,rp,ss(" => "),VV])) :-
  ssTrm(Nm,0,NN),
  showArgs(Args,0,AA),
  ssTrm(Value,0,VV),
  (H=soft -> HH="soft ";HH="").
ssRuleSet(glbDef(_Lc,Nm,_Tp,Value),sq([ss("Gl: "),id(Nm),ss(" = "),VV])) :-
  ssTrm(Value,0,VV).
ssRuleSet(typDef(_Lc,Tp,_Rl,IxMap),
	  sq([ss("Tp: "),TT,ss(" : "),MM])) :-
  ssType(Tp,false,0,TT),
  ssConsMap(IxMap,MM).
ssRuleSet(lblDef(_Lc,Lbl,_Tp,Ix),
	  sq([ss("Cn: "),LL,ss(" @ "),ix(Ix)])) :-
  ssTrm(Lbl,0,LL).

dispEquations(Eqs) :-
  map(Eqs,lterms:ssEqn(lterms:ssTrm),EE),
  displayln(iv(nl(0),EE)).

ssEqn(Dsp,(_Lc,Args,Grd,Val),sq([ss("("),AA,ss(")=>"),GG,VV])) :-
  showArgs(Args,0,AA),
  ssGuard(Grd,GG),
  call(Dsp,ssTrm,Val,0,VV).

ssGuard(none,ss("")).
ssGuard(some(G),sq([ss(" where "),GG])) :-
  ssTrm(G,0,GG).

dispProg(M) :-
  ssTransformed(M,S),
  validSS(S),!,
  displayln(S).

dispRuleSet(RS) :-
  display:displayln(lterms:ssRuleSet(RS)).

ssConsMap(Els,iv(ss(", "),EE)) :-
  map(Els,lterms:ssConsEntry,EE).

ssConsEntry((Lb,Ix),sq([LL,ss(":"),ix(Ix)])) :-
  ssTrm(Lb,0,LL).
  
showArgs(Args,Dp,iv(ss(","),AA)) :-
  map(Args,lterms:swTrm(Dp),AA).

swTrm(Dp,T,S) :- ssTrm(T,Dp,S).

showTerm(Trm,Dp,O,Ox) :-
  ss_to_chrs(lterms:ssTrm(Trm,Dp),O,Ox).

ssTrm(voyd,_,ss("void")) :-!.
ssTrm(idnt(Nm),_,id(Nm)) :-!.
ssTrm(anon,_,ss("_")) :-!.
ssTrm(intgr(Ix),_,ix(Ix)) :-!.
ssTrm(bigx(Ix),_,ss(Ix)) :-!.
ssTrm(float(Dx),_,fx(Dx)) :-!.
ssTrm(chr(Cp),_,sq([ss("`"),cp(Cp),ss("`")])) :-!.
ssTrm(strg(Str),_,sq([ss(""""),ss(Str),ss("""")])) :-!.
ssTrm(thrw(_,E),Dp,sq([ss("throw "),EE])) :-
  ssTrm(E,Dp,EE).
ssTrm(cll(_,Op,Args),Dp,sq([OO,lp,AA,rp])) :- !,
  ssTrm(Op,Dp,OO),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(ocall(_,Op,Args),Dp,sq([OO,ss("°"),lp,AA,rp])) :-!,
  ssTrm(Op,Dp,OO),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(ecll(_,Es,Args),Dp,sq([ss("ε"),ss(Es),ss("("),AA,ss(")")])) :-!,
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(intrinsic(_,Op,Args),Dp,sq([id(OpNm),ss("<"),AA,ss(">")])) :-!,
  atom_string(Op,OpNm),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(ctpl(Op,A),Dp,sq([ss("."),OO,lp,AA,rp])) :-!,
  ssCOnOp(Op,OO),
  Dp1 is Dp+2,
  showArgs(A,Dp1,AA).
ssTrm(enum(Nm),_,sq([ss("."),id(Nm)])) :-!.
ssTrm(nth(_,Rc,Off),Dp,sq([OO,ss("."),ix(Off)])) :-!,
  ssTrm(Rc,Dp,OO).
ssTrm(setix(_,Rc,Off,Vl),Dp,sq([OO,ss("."),ix(Off),ss(":="),VV])) :-!,
  ssTrm(Rc,Dp,OO),
  ssTrm(Vl,Dp,VV).
ssTrm(cel(_,C),Dp,sq([ss("ref "),CC])) :-!,
  ssTrm(C,Dp,CC).
ssTrm(get(_,C),Dp,sq([CC,ss("!")])) :-!,
  ssTrm(C,Dp,CC).
ssTrm(set(_,C,V),Dp,sq([CC,ss(":="),VV])) :-!,
  ssTrm(C,Dp,CC),
  ssTrm(V,Dp,VV).
ssTrm(lbl(Nm,Ar),_,sq([id(Nm),ss("/"),ix(Ar)])) :-!.
ssTrm(whr(_,Ptn,Cond),Dp,sq([PP,ss(" whr "),CC])) :-!,
  ssTrm(Ptn,Dp,PP),
  Dp1 is Dp+2,
  ssTrm(Cond,Dp1,CC).
ssTrm(ltt(_,Vr,Bnd,Exp),Dp,sq([ss("let "),VV,ss("="),BB,ss(" in "),EE])) :-!,
  Dp1 is Dp+2,
  ssTrm(Vr,Dp1,VV),
  ssTrm(Bnd,Dp1,BB),
  ssTrm(Exp,Dp1,EE).
ssTrm(varNames(_,Vars,Value),Dp,sq([ss("vars:"),VV,ss("->"),EE])) :-!,
  ssVarNames(Vars,Dp,VV),
  ssTrm(Value,Dp,EE).
ssTrm(case(_,G,Cases,Deflt),Dp,
      sq([ss("case "),GG,ss("in"),lb,CC,rb,ss(" else "),DD])) :-!,
  ssTrm(G,Dp,GG),
  ssCases(Cases,Dp,lterms:ssTrm,CC),
  ssTrm(Deflt,Dp,DD).
ssTrm(unpack(_,G,Cases),Dp, sq([ss("unpack "),GG,ss(" in "),CC])) :-!,
  ssTrm(G,Dp,GG),
  ssCases(Cases,Dp,lterms:ssTrm,CC).
ssTrm(seqD(_,L,R),Dp,sq([LL,ss(";"),RR])) :-!,
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(cnj(_,L,R),Dp,sq([LL,ss("&&"),RR])) :-!,
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(dsj(_,L,R),Dp,sq([lp,LL,ss("||"),RR,rp])) :-!,
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(cnd(_,T,L,R),Dp,sq([lp,TT,ss(" ? "),nl(Dp1),LL,ss("||"),nl(Dp1),RR,rp])) :-!,
  Dp1 is Dp+2,
  ssTrm(T,Dp,TT),
  ssTrm(L,Dp1,LL),
  ssCnd(R,Dp,RR).
ssTrm(mtch(_,L,R),Dp,sq([lp,LL,ss(".="),RR,rp])) :-!,
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(ng(_,R),Dp,sq([lp,ss("~"),RR,rp])) :-!,
  ssTrm(R,Dp,RR).
ssTrm(error(Lc,M),Dp,sq([lp,ss("error "),MM,rp,ss("@"),LL])) :-!,
  ssTrm(M,Dp,MM),
  ssLoc(Lc,LL).
ssTrm(tsk(_,A),Dp,sq([ss("task "),AA])) :-
  ssTrm(A,Dp,AA).
ssTrm(susp(_,T,E),Dp,sq([TT,ss(" suspend "),EE])) :-
  ssTrm(T,Dp,TT),
  ssTrm(E,Dp,EE).
ssTrm(resme(_,T,E),Dp,sq([TT,ss(" resume "),EE])) :-
  ssTrm(T,Dp,TT),
  ssTrm(E,Dp,EE).
ssTrm(vlof(_,A),Dp,sq([ss("valof "),AA])) :-
  ssAct(A,Dp,AA).
ssTrm(perf(_,A),Dp,sq([ss("perform "),AA])) :-
  ssAct(A,Dp,AA).
ssTrm(try(_,B,E,H),Dp,sq([ss("try "),BB,ss(" catch "),EE,ss(" in "),HH])) :-
  Dp2 is Dp+2,
  ssTrm(E,Dp,EE),
  ssTrm(B,Dp2,BB),
  ssTrm(H,Dp,HH).

dispAct(A) :-
  display:display(lterms:ssAct(A,0)).

ssAct(nop(_),_,ss("{}")) :-!.
ssAct(seq(Lc,A,B),Dp,sq([ss("{"),iv(sq([ss(";"),nl(Dp2)]),AA),ss("}")])) :-!,
  Dp2 is Dp+2,
  ssActSeq(seq(Lc,A,B),Dp2,AA).
ssAct(lbld(_,Lb,A),Dp,sq([ss(Lb),ss(":"),AA])) :-!,
  ssAct(A,Dp,AA).
ssAct(brk(_,Lb),_,sq([ss("break "),ss(Lb)])) :-!.
ssAct(vls(_,E),Dp,sq([ss("valis "),EE])) :-!,
  ssTrm(E,Dp,EE).
ssAct(thrw(_,E),Dp,sq([ss("throw "),EE])) :-!,
  ssTrm(E,Dp,EE).
ssAct(mtch(_,P,E),Dp,sq([PP,ss(" .= "),EE])) :-!,
  ssTrm(P,Dp,PP),
  ssTrm(E,Dp,EE).
ssAct(defn(_,P,E),Dp,sq([PP,ss(" = "),EE])) :-!,
  ssTrm(P,Dp,PP),
  ssTrm(E,Dp,EE).
ssAct(asgn(_,P,E),Dp,sq([PP,ss(" := "),EE])) :-!,
  ssTrm(P,Dp,PP),
  ssTrm(E,Dp,EE).
ssAct(case(_,G,Cases,Deflt),Dp,
      sq([ss("case "),GG,ss("in"),lb,CC,rb,ss(" else "),DD])) :-!,
  ssTrm(G,Dp,GG),
  ssCases(Cases,Dp,lterms:ssAct,CC),
  ssTrm(Deflt,Dp,DD).
ssAct(unpack(_,G,Cases),Dp,
      sq([ss("unpack "),GG,ss(" in "),CC])) :-!,
  ssTrm(G,Dp,GG),
  ssCases(Cases,Dp,lterms:ssAct,CC).
ssAct(iftte(_,G,T,E),Dp,
      sq([ss("if "),GG,ss(" then "),nl(Dp2),TT,nl(Dp2),ss(" else "),EE])) :-!,
  Dp2 is Dp+2,
  ssTrm(G,Dp,GG),
  ssAct(T,Dp2,TT),
  ssAct(E,Dp2,EE).
ssAct(iftt(_,G,T),Dp,
      sq([ss("if "),GG,ss(" then "),nl(Dp2),TT])) :-!,
  Dp2 is Dp+2,
  ssTrm(G,Dp,GG),
  ssAct(T,Dp2,TT).
ssAct(whle(_,G,B),Dp,
      sq([ss("while "),GG,ss(" do"),nl(Dp2),BB])) :-!,
  Dp2 is Dp+2,
  ssTrm(G,Dp,GG),
  ssAct(B,Dp2,BB).
ssAct(ffor(_,P,S,B),Dp,
      sq([ss("for "),PP,ss(" in "),SS,dd("do"),nl(Dp2),BB])) :-!,
  Dp2 is Dp+2,
  ssTrm(P,Dp,PP),
  ssTrm(S,Dp,SS),
  ssAct(B,Dp2,BB).
ssAct(ltt(_,Vr,B,A),Dp,sq([ss("let "),VV,ss("="),BB,ss(" in "),AA])) :-!,
  Dp1 is Dp+2,
  ssTrm(Vr,Dp1,VV),
  ssTrm(B,Dp1,BB),
  ssAct(A,Dp1,AA).
ssAct(rtire(_,T,E),Dp,sq([TT,ss(" retire "),EE])) :-
  ssTrm(T,Dp,TT),
  ssTrm(E,Dp,EE).
ssAct(error(_,M),Dp,sq([ss(" error "),MM])) :-
  ssTrm(M,Dp,MM).
ssAct(try(_,B,E,H),Dp,sq([ss("try "),BB,ss(" catch "),EE,ss(" in "),HH])) :-
  Dp2 is Dp+2,
  ssTrm(E,Dp,EE),
  ssAct(B,Dp2,BB),
  ssAct(H,Dp2,HH).
ssAct(perf(_,E),Dp,sq([ss("call "),EE])) :-
  ssTrm(E,Dp,EE).

ssActSeq(seq(_,A,B),Dp,[AA|BB]) :-!,
  ssAct(A,Dp,AA),
  ssActSeq(B,Dp,BB).
ssActSeq(A,Dp,[AA]) :-
  ssAct(A,Dp,AA).

ssCnd(cnd(_,T,L,R),Dp,sq([TT,ss(" ? "),nl(Dp),LL,ss("||"),nl(Dp),RR])) :-!,
  Dp1 is Dp+2,
  ssTrm(T,Dp,TT),
  ssTrm(L,Dp1,LL),
  ssCnd(R,Dp,RR).
ssCnd(Exp,Dp,XX) :- ssTrm(Exp,Dp,XX).

ssCases(Cases,Dp,Leaf,sq([lb,nl(Dp2),iv(nl(Dp2),CC),nl(Dp),rb])) :-
  Dp2 is Dp+2,
  map(Cases,lterms:ssCase(Dp2,Leaf),CC).

ssCase(Dp,Leaf,(Ptn,Val,_),sq([PP,ss("=>"),VV])) :-
  ssTrm(Ptn,Dp,PP),
  call(Leaf,Val,Dp,VV),!.

ssCOnOp(lbl(L,Ar),sq([])) :-
  isTplLbl(L,Ar).
ssCOnOp(lbl(Nm,_),id(Nm)).

ssTrmGuard(none,_,sq([])) :- !.
ssTrmGuard(some(C),Dp,sq([ss("where "),CC])) :-
  ssTrm(C,Dp,CC).

ssVarNames(Vrs,Dp,iv(ss(","),VV)) :-
  map(Vrs,lterms:ssVarBinding(Dp),VV).

ssVarBinding(Dp,(V,Vx),ss([id(V),ss("/"),VV])) :-
  ssTrm(Vx,Dp,VV).

dispTerm(T) :-
  display:display(lterms:ssTrm(T,0)).

substTerm(Q,In,Out) :-
  rewriteTerm(lterms:applyQ(Q),In,Out),!.

substGoal(_,none,none) :-!.
substGoal(Q,some(In),some(Out)) :-
  substTerm(Q,In,Out).

substAction(Q,In,Out) :-
  rewriteAction(lterms:applyQ(Q),In,Out),!.

applyQ(Q,idnt(Nm),Trm) :- is_member((Nm,Trm),Q),!.

substTerms(Q,Els,NEls):-
  map(Els,lterms:substTerm(Q),NEls).

rewriteTerm(QTst,T,T1) :-
  call(QTst,T,T1),!.
rewriteTerm(_,voyd,voyd).
rewriteTerm(_,intgr(Ix),intgr(Ix)).
rewriteTerm(_,bigx(Ix),bigx(Ix)).
rewriteTerm(_,idnt(Nm),idnt(Nm)).
rewriteTerm(_,anon,anon).
rewriteTerm(_,float(Dx),float(Dx)).
rewriteTerm(_,chr(Cp),chr(Cp)).
rewriteTerm(_,strg(Sx),strg(Sx)).
rewriteTerm(_,enum(Nm),enum(Nm)).
rewriteTerm(_,lbl(Nm,Ar),lbl(Nm,Ar)).
rewriteTerm(QTest,ltt(Lc,V,Val,Exp),ltt(Lc,V,Val1,Exp1)) :-
  rewriteTerm(lterms:checkV(V,QTest),Val,Val1),
  rewriteTerm(lterms:checkV(V,QTest),Exp,Exp1).
rewriteTerm(QTest,thrw(Lc,E),thrw(Lc,EE)) :-!,
  rewriteTerm(QTest,E,EE).
rewriteTerm(QTest,cll(Lc,Op,Args),cll(Lc,NOp,NArgs)) :-
  rewriteTerm(QTest,Op,NOp),
  rewriteTerms(QTest,Args,NArgs).
rewriteTerm(QTest,ocall(Lc,Op,Args),ocall(Lc,NOp,NArgs)) :-
  rewriteTerm(QTest,Op,NOp),
  rewriteTerms(QTest,Args,NArgs).
rewriteTerm(QTest,nth(Lc,Op,Off),nth(Lc,NOp,Off)) :-
  rewriteTerm(QTest,Op,NOp).
rewriteTerm(QTest,cel(Lc,T),cel(Lc,NT)) :-
  rewriteTerm(QTest,T,NT).
rewriteTerm(QTest,get(Lc,T),get(Lc,NT)) :-
  rewriteTerm(QTest,T,NT).
rewriteTerm(QTest,set(Lc,T,V),set(Lc,NT,NV)) :-
  rewriteTerm(QTest,T,NT),
  rewriteTerm(QTest,V,NV).
rewriteTerm(QTest,setix(Lc,Op,Off,Vl),setix(Lc,NOp,Off,NVl)) :-
  rewriteTerm(QTest,Op,NOp),
  rewriteTerm(QTest,Vl,NVl).
rewriteTerm(QTest,ctpl(Op,Args),ctpl(NOp,NArgs)) :-
  rewriteTerm(QTest,Op,NOp),
  rewriteTerms(QTest,Args,NArgs).
rewriteTerm(QTest,intrinsic(Lc,Op,Args),intrinsic(Lc,Op,NArgs)) :-
  rewriteTerms(QTest,Args,NArgs).
rewriteTerm(QTest,ecll(Lc,Call,Args),ecll(Lc,Call,NArgs)) :-
  rewriteTerms(QTest,Args,NArgs).
rewriteTerm(QTest,whr(Lc,T,C),whr(Lc,NT,NC)) :-
  rewriteTerm(QTest,T,NT),
  rewriteTerm(QTest,C,NC).
rewriteTerm(QTest,varNames(Lc,V,T),varNames(Lc,NV,NT)) :-
  map(V,lterms:rewriteVN(QTest),NV),
  rewriteTerm(QTest,T,NT).
rewriteTerm(QTest,case(Lc,T,C,D),case(Lc,NT,NC,ND)) :-
  rewriteTerm(QTest,T,NT),
  map(C,lterms:rewriteCase(QTest,lterms:rewriteTerm),NC),
  rewriteTerm(QTest,D,ND).
rewriteTerm(QTest,unpack(Lc,T,C),unpack(Lc,NT,NC)) :-
  rewriteTerm(QTest,T,NT),
  map(C,lterms:rewriteCase(QTest,lterms:rewriteTerm),NC).
rewriteTerm(QTest,seqD(Lc,L,R),seqD(Lc,NL,NR)) :-
  rewriteTerm(QTest,L,NL),
  rewriteTerm(QTest,R,NR).
rewriteTerm(QTest,cnj(Lc,L,R),cnj(Lc,NL,NR)) :-
  rewriteTerm(QTest,L,NL),
  rewriteTerm(QTest,R,NR).
rewriteTerm(QTest,dsj(Lc,L,R),dsj(Lc,NL,NR)) :-
  rewriteTerm(QTest,L,NL),
  rewriteTerm(QTest,R,NR).
rewriteTerm(QTest,cnd(Lc,T,L,R),cnd(Lc,NT,NL,NR)) :-
  rewriteTerm(QTest,T,NT),
  rewriteTerm(QTest,L,NL),
  rewriteTerm(QTest,R,NR).
rewriteTerm(QTest,mtch(Lc,L,R),mtch(Lc,NL,NR)) :-
  rewriteTerm(QTest,L,NL),
  rewriteTerm(QTest,R,NR).
rewriteTerm(QTest,ng(Lc,R),ng(Lc,NR)) :-
  rewriteTerm(QTest,R,NR).
rewriteTerm(QTest,vlof(Lc,A),vlof(Lc,AA)) :-
  rewriteAction(QTest,A,AA).
rewriteTerm(QTest,tsk(Lc,F),tsk(Lc,FF)) :-
  rewriteTerm(QTest,F,FF).
rewriteTerm(QTest,susp(Lc,T,E),susp(Lc,TT,EE)) :-
  rewriteTerm(QTest,T,TT),
  rewriteTerm(QTest,E,EE).
rewriteTerm(QTest,resme(Lc,T,E),resme(Lc,TT,EE)) :-
  rewriteTerm(QTest,T,TT),
  rewriteTerm(QTest,E,EE).
rewriteTerm(QTest,error(Lc,M),error(Lc,MM)) :-!,
  rewriteTerm(QTest,M,MM).
rewriteTerm(QTest,try(Lc,B,E,H),try(Lc,BB,EE,HH)) :-!,
  rewriteTerm(QTest,E,EE),
  rewriteTerm(QTest,B,BB),
  rewriteTerm(QTest,H,HH).

rewriteTerms(QTest,Els,NEls):-
  map(Els,lterms:rewriteTerm(QTest),NEls).

rewriteGoal(_,none,none).
rewriteGoal(QTest,some(T),some(NT)) :-
  rewriteTerm(QTest,T,NT).

rewriteVN(QTest,(T,E),(T,NE)) :-
  rewriteTerm(QTest,E,NE).

rewriteAction(_QTest,nop(Lc),nop(Lc)) :- !.
rewriteAction(QTest,seq(Lc,L,R),seq(Lc,LL,RR)) :-!,
  rewriteAction(QTest,L,LL),
  rewriteAction(QTest,R,RR).
rewriteAction(QTest,lbld(Lc,Lb,A),lbld(Lc,Lb,AA)) :- !,
  rewriteAction(QTest,A,AA).
rewriteAction(_,brk(Lc,Lb),brk(Lc,Lb)) :-!.
rewriteAction(QTest,vls(Lc,E),vls(Lc,EE)) :- !,
  rewriteTerm(QTest,E,EE).
rewriteAction(QTest,thrw(Lc,E),thrw(Lc,EE)) :- !,
  rewriteTerm(QTest,E,EE).
rewriteAction(QTest,perf(Lc,E),perf(Lc,EE)) :- !,
  rewriteTerm(QTest,E,EE).
rewriteAction(QTest,mtch(Lc,P,E),mtch(Lc,PP,EE)) :- !,
  rewriteTerm(QTest,P,PP),
  rewriteTerm(QTest,E,EE).
rewriteAction(QTest,defn(Lc,P,E),defn(Lc,PP,EE)) :- !,
  rewriteTerm(QTest,P,PP),
  rewriteTerm(QTest,E,EE).
rewriteAction(QTest,asgn(Lc,P,E),asgn(Lc,PP,EE)) :- !,
  rewriteTerm(QTest,P,PP),
  rewriteTerm(QTest,E,EE).
rewriteAction(QTest,case(Lc,G,C),case(Lc,GG,CC)) :-
  rewriteTerm(QTest,G,GG),
  map(C,lterms:rewriteCase(QTest,lterms:rewriteAction),CC).
rewriteAction(QTest,unpack(Lc,G,C),unpack(Lc,GG,CC)) :-
  rewriteTerm(QTest,G,GG),
  map(C,lterms:rewriteCase(QTest,lterms:rewriteAction),CC).
rewriteAction(QTest,iftte(Lc,G,L,R),iftte(Lc,GG,LL,RR)) :-!,
  rewriteTerm(QTest,G,GG),
  rewriteAction(QTest,L,LL),
  rewriteAction(QTest,R,RR).
rewriteAction(QTest,iftt(Lc,G,L),iftt(Lc,GG,LL)) :-!,
  rewriteTerm(QTest,G,GG),
  rewriteAction(QTest,L,LL).
rewriteAction(QTest,whle(Lc,G,L),whle(Lc,GG,LL)) :-!,
  rewriteTerm(QTest,G,GG),
  rewriteAction(QTest,L,LL).
rewriteAction(QTest,ffor(Lc,P,S,B),ffor(Lc,PP,SS,BB)) :-!,
  rewriteTerm(QTest,P,PP),
  rewriteTerm(QTest,S,SS),
  rewriteAction(QTest,B,BB).
rewriteAction(QTest,ltt(Lc,V,E,B),ltt(Lc,V,EE,BB)) :-!,
  rewriteTerm(lterms:checkV(V,QTest),E,EE),
  rewriteAction(lterms:checkV(V,QTest),B,BB).
rewriteAction(QTest,rtire(Lc,T,E),rtire(Lc,TT,EE)) :-!,
  rewriteTerm(QTest,T,TT),
  rewriteTerm(QTest,E,EE).
rewriteAction(QTest,error(Lc,M),error(Lc,MM)) :-!,
  rewriteTerm(QTest,M,MM).
rewriteAction(QTest,try(Lc,B,E,H),try(Lc,BB,EE,HH)) :-!,
  rewriteTerm(QTest,E,EE),
  rewriteAction(QTest,B,BB),
  rewriteAction(QTest,H,HH).
  
rewriteCase(QTest,BCall,(T,E,Lbl),(NT,NE,Lbl)) :-
  rewriteTerm(QTest,T,NT),
  call(BCall,QTest,E,NE).

checkV(Vr,Other,T,T1) :-
  T\=Vr,
  call(Other,T,T1).

genTplStruct(Cnt,lbl(Nm,Cnt)) :-
  swritef(Nm,"()%d",[Cnt]).

isTplLbl(Nm,Ar) :- string_concat("()",A,Nm),number_string(Ar,A).

mkTpl(Els,ctpl(C,Els)) :-
  length(Els,Cnt),
  genTplStruct(Cnt,C).

mkCons(Nm,Els,ctpl(lbl(Nm,Arity),Els)) :-
  length(Els,Arity).

isUnit(ctpl(lbl("()0",0),[])).

isLiteral(intgr(_)).
isLiteral(bigx(_)).
isLiteral(float(_)).
isLiteral(chr(_)).
isLiteral(strg(_)).
isLiteral(enum(_)).
isLiteral(lbl(_,_)).
isLiteral(ctpl(Lbl,Args)) :-
  isLiteral(Lbl),
  check_implies(misc:is_member(A,Args),lterms:isLiteral(A)),!.

termHash(voyd,0).
termHash(intgr(Ix),Ix).
termHash(bigx(Bx),Hx) :- bigHash(Bx,Hx).
termHash(float(Dx),Ix) :- Ix is round(Dx).
termHash(chr(Cp),Ix) :- charHash(0,Cp,Ix).
termHash(strg(Sx),Ix) :- stringHash(0,Sx,Ix).
termHash(enum(Sx),Ix) :- termHash(lbl(Sx,0),Ix).
termHash(lbl(Nm,Ar),Hx) :-
  stringHash(0,Nm,Lx),
  Ix is Ar*37+Lx,
  hashSixtyFour(Ix,Hx).

locTerm(loc(Pk,Ln,Col,Pos,Len),Tpl) :-
  mkTpl([strg(Pk),intgr(Ln),intgr(Col),intgr(Pos),intgr(Len)],Tpl).

idInTerm(idnt(Nm),Term) :-
  inTerm(Term,Nm),!.

inTerm(idnt(Nm),Nm).
inTerm(thrw(E),Nm) :-
  inTerm(E,Nm).
inTerm(cll(_,_,Args),Nm) :-
  is_member(Arg,Args),
  inTerm(Arg,Nm).
inTerm(ocall(_,Op,_),Nm) :-
  inTerm(Op,Nm).
inTerm(ocall(_,_Op,Args),Nm) :-
  is_member(Arg,Args), inTerm(Arg,Nm),!.
inTerm(ecll(_,_,Args),Nm) :-
  is_member(Arg,Args), inTerm(Arg,Nm),!.
inTerm(intrinsic(_,_Op,Args),Nm) :-
  is_member(Arg,Args), inTerm(Arg,Nm),!.
inTerm(nth(_,Op,_),Nm) :-
  inTerm(Op,Nm).
inTerm(cel(_,C),Nm) :-
  inTerm(C,Nm).
inTerm(get(_,C),Nm) :-
  inTerm(C,Nm).
inTerm(set(_,C,_),Nm) :-
  inTerm(C,Nm).
inTerm(set(_,_,V),Nm) :-
  inTerm(V,Nm).
inTerm(setix(_,Op,_,_),Nm) :-
  inTerm(Op,Nm).
inTerm(setix(_,_,_,Vl),Nm) :-
  inTerm(Vl,Nm).
inTerm(ctpl(_,Args),Nm) :-
  is_member(Arg,Args), inTerm(Arg,Nm),!.
inTerm(shft(_,L,_A),Nm) :-
  is_member(L,Nm).
inTerm(shft(_,_L,A),Nm) :-
  is_member(A,Nm).
inTerm(resme(_,L,_A),Nm) :-
  is_member(L,Nm).
inTerm(resme(_,_L,Arg),Nm) :-
  inTerm(Arg,Nm),!.
inTerm(whr(_,T,_),Nm) :-
  inTerm(T,Nm),!.
inTerm(whr(_,_,C),Nm) :-
  inTerm(C,Nm),!.
inTerm(varNames(_,V,_),Nm) :-
  is_member((Nm,_),V),!.
inTerm(varNames(_,_,T),Nm) :-
  inTerm(T,Nm),!.
inTerm(case(_,T,_C),Nm) :-
  inTerm(T,Nm),!.
inTerm(case(_,_T,C),Nm) :-
  is_member((P,V),C), (inTerm(P,Nm);inTerm(V,Nm)),!.
inTerm(unpack(_,T,_C),Nm) :-
  inTerm(T,Nm),!.
inTerm(unpack(_,_T,C),Nm) :-
  is_member((P,V),C), (inTerm(P,Nm);inTerm(V,Nm)),!.
inTerm(seqD(_,L,R),Nm) :-
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(cnj(_,L,R),Nm) :-
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(dsj(_,L,R),Nm) :-
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(cnd(_,T,L,R),Nm) :-
  inTerm(T,Nm) ; inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(mtch(_,L,R),Nm) :-
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(ng(_,R),Nm) :-
  inTerm(R,Nm).
inTerm(ltt(_,_,B,_E),Nm) :-
  inTerm(B,Nm).
inTerm(ltt(_,_,_B,E),Nm) :-
  inTerm(E,Nm).
inTerm(perf(_,A),Nm) :-
  inAction(A,Nm).

inAction(nop(_),_) :- !,fail.
inAction(seq(_,L,R),Nm) :-!,
  (inAction(L,Nm) ; inAction(R,Nm)).
inAction(lbld(_,_,E),Nm) :- !,
  inAction(E,Nm).
inAction(vls(_,E),Nm) :- !,
  inTerm(E,Nm).
inAction(thrw(_,E),Nm) :- !,
  inTerm(E,Nm).
inAction(perf(_,E),Nm) :- !,
  inTerm(E,Nm).
inAction(mtch(_,P,E),Nm) :- !,
  (inTerm(P,Nm) ; inTerm(E,Nm)).
inAction(defn(_,P,E),Nm) :- !,
  (inTerm(P,Nm) ; inTerm(E,Nm)).
inAction(asgn(_,P,E),Nm) :- !,
  (inTerm(P,Nm) ; inTerm(E,Nm)).
inAction(case(_,T,_C),Nm) :-
  inTerm(T,Nm),!.
inAction(case(_,_T,C),Nm) :-
  is_member((P,V,_),C), (inTerm(P,Nm);inAction(V,Nm)),!.
inAction(unpack(_,T,_C),Nm) :-
  inTerm(T,Nm),!.
inAction(unpack(_,_T,C),Nm) :-
  is_member((P,V,_),C), (inTerm(P,Nm);inAction(V,Nm)),!.
inAction(iftte(_,G,L,R),Nm) :-!,
  (inTerm(G,Nm) ; inTerm(L,Nm) ; inTerm(R,Nm)).
inAction(iftt(_,G,L),Nm) :-!,
  (inTerm(G,Nm) ; inTerm(L,Nm)).
inAction(whle(_,G,L),Nm) :-!,
  (inTerm(G,Nm) ; inAction(L,Nm)).
inAction(ltt(_,_,E,B),Nm) :-!,
  (inTerm(E,Nm) ; inAction(B,Nm)).
inAction(rtire(_,L,G),Nm) :-!,
  (inTerm(L,Nm) ; inTerm(G,Nm)).
inAction(error(_,M),Nm) :-!,
  inTerm(M,Nm).
inAction(try(_,B,E,H),Nm) :-!,
  (inAction(B,Nm) ; inTerm(E,Nm) ; inAction(H,Nm)).

isCnd(cnj(_,_,_)).
isCnd(dsj(_,_,_)).
isCnd(mtch(_,_,_)).
isCnd(ng(_,_)).

validLProg(PkgDecls,mdule(_,_,_,_,Defs)) :-
  declareNms(PkgDecls,[],Dct),
  validDfs(Defs,Dct),!.

validDfs([],_).
validDfs([D|Dfs],Dct) :-
  validDf(D,Dct),!,
  validDfs(Dfs,Dct).

validDf(fnDef(Lc,_,_,_Tp,Args,Value),Dct) :-!,
  declareArgs(Args,Dct,D0),
  validTerm(Value,Lc,D0),!.
validDf(glbDef(Lc,_Nm,_Tp,Value),Dct) :-
  validTerm(Value,Lc,Dct).
validDf(typDef(_Lc,_Tp,_Rl,_IxMap),_).

declareNms(Defs,Dct,Dx) :-
  rfold(Defs,lterms:declareDef,Dct,Dx).

declareDef(funDec(_,Nm,_Tp),Dct,Dx) :-!,
  add_mem(Nm,Dct,Dx).
declareDef(varDec(_,Nm,_Tp),Dct,Dx) :-!,
  add_mem(Nm,Dct,Dx).
declareDef(typeDec(_,_,_),Dx,Dx).
declareDef(cnsDec(_,_,_),Dx,Dx).
declareDef(contractDec(_,_,_),Dx,Dx).
declareDef(accDec(_,_,Nm,_),Dct,Dx) :-
  add_mem(Nm,Dct,Dx).
declareDef(updDec(_,_,Nm,_),Dct,Dx) :-
  add_mem(Nm,Dct,Dx).
declareDef(impDec(_,Nm,_),Dct,Dx) :-
  add_mem(Nm,Dct,Dx).
       
declareArgs(Args,Dct,Dx) :-
  rfold(Args,lterms:declareArg,Dct,Dx).

declareArg(idnt(Nm),D,Dx) :-
  add_mem(Nm,D,Dx).

validTerm(idnt(Nm),Lc,D) :-
  (is_member(Nm,D) -> true ; 
   reportError("(validate) Variable %s not in scope",[id(Nm)],Lc)).
validTerm(anon,_,_).
validTerm(voyd,_,_).
validTerm(intgr(_),_,_).
validTerm(bigx(_),_,_).
validTerm(float(_),_,_).
validTerm(chr(_),_,_).
validTerm(strg(_),_,_).
validTerm(lbl(_,_),_,_).
validTerm(cll(Lc,lbl(_,_),Args),_,D) :-
  validTerms(Args,Lc,D).
validTerm(ocall(Lc,Op,Args),_,D) :-
  validTerm(Op,Lc,D),
  validTerms(Args,Lc,D).
validTerm(ecll(Lc,Es,Args),_,D) :-
  isEscape(Es,_),
  validTerms(Args,Lc,D).
validTerm(intrinsic(Lc,Is,Args),_,D) :-
  isIntrinsic(_,_,Is),
  validTerms(Args,Lc,D).
validTerm(thrw(Lc,E),_,D) :-
  validTerm(E,Lc,D).
validTerm(ctpl(lbl(_,_),Args),Lc,D) :-
  validTerms(Args,Lc,D).
validTerm(enum(_),_,_).
validTerm(nth(Lc,Rc,Off),_,D) :-
  integer(Off),
  validTerm(Rc,Lc,D).
validTerm(cel(Lc,C),_,D) :-
  validTerm(C,Lc,D).
validTerm(get(Lc,C),_,D) :-
  validTerm(C,Lc,D).
validTerm(set(Lc,C,V),_,D) :-
  validTerm(C,Lc,D),
  validTerm(V,Lc,D).
validTerm(setix(Lc,Rc,Off,Vl),_,D) :-
  integer(Off),
  validTerm(Rc,Lc,D),
  validTerm(Vl,Lc,D).
validTerm(whr(Lc,Exp,Cond),_,D) :-
  glVars(Cond,D,D1),
  validTerm(Exp,Lc,D1),
  validTerm(Cond,Lc,D1).
validTerm(ltt(Lc,Vr,Bnd,Exp),_,D) :-
  validTerm(Bnd,Lc,D),
  ptnVars(Vr,D,D1),
  validTerm(Vr,Lc,D1),
  validTerm(Exp,Lc,D1).
validTerm(varNames(Lc,Vars,Value),_,D) :-
  rfold(Vars,lterms:validVr,D,D1),
  validTerm(Value,Lc,D1).
validTerm(case(Lc,G,Cases,Deflt),_,D) :-
  validTerm(G,Lc,D),
  validCases(Cases,lterms:validTerm,D),
  validTerm(Deflt,Lc,D).
validTerm(unpack(Lc,G,Cases),_,D) :-
  validTerm(G,Lc,D),
  validCases(Cases,lterms:validTerm,D).
validTerm(try(Lc,B,E,H),_,D) :-
  validTerm(B,Lc,D),
  ptnVars(E,D,D0),
  validTerm(H,Lc,D0).
validTerm(seqD(Lc,L,R),_,D) :-
  validTerm(L,Lc,D),
  validTerm(R,Lc,D).
validTerm(cnj(Lc,L,R),_,D) :-
  glVars(L,D,D0),
  validTerm(L,Lc,D0),
  validTerm(R,Lc,D0).
validTerm(dsj(Lc,L,R),_,D) :-
  validTerm(L,Lc,D),
  validTerm(R,Lc,D).
validTerm(cnd(Lc,T,L,R),_,D) :-
  glVars(T,D,D0),
  validTerm(T,Lc,D0),
  validTerm(L,Lc,D0),
  validTerm(R,Lc,D).
validTerm(mtch(Lc,L,R),_,D) :-
  validPtn(L,Lc,D,D0),
  validTerm(R,Lc,D0).
validTerm(ng(Lc,R),_,D) :-
  validTerm(R,Lc,D).
validTerm(vlof(Lc,A),_,D) :-
  validAction(A,Lc,D,_).
validTerm(perf(Lc,A),_,D) :-
  validAction(A,Lc,D,_).
validTerm(error(Lc,R),_,D) :-
  validTerm(R,Lc,D).
validTerm(tsk(Lc,F),_,D) :-
  validTerm(F,Lc,D).
validTerm(susp(Lc,T,E),_,D) :-
  validTerm(T,Lc,D),
  validTerm(E,Lc,D).
validTerm(resme(Lc,T,E),_,D) :-
  validTerm(T,Lc,D),
  validTerm(E,Lc,D).
validTerm(error(Lc,M),_,D) :-
  validTerm(M,Lc,D).
validTerm(T,Lc,_) :-
  reportError("(internal) Invalid term %s in scope",[ltrm(T)],Lc).

validTerms([],_,_) :-!.
validTerms([A|Args],Lc,D) :-
  validTerm(A,Lc,D),
  validTerms(Args,Lc,D).

validVr(Id,D,[Id|D]).

validPtn(idnt(Nm),_,D,Dx) :-
  add_mem(Nm,D,Dx).
validPtn(anon,_,Dx,Dx).
validPtn(voyd,_,Dx,Dx).
validPtn(intgr(_),_,Dx,Dx).
validPtn(bigx(_),_,Dx,Dx).
validPtn(float(_),_,Dx,Dx).
validPtn(chr(_),_,Dx,Dx).
validPtn(strg(_),_,Dx,Dx).
validPtn(lbl(_,_),_,Dx,Dx).
validPtn(ctpl(lbl(_,_),Args),Lc,D,Dx) :-
  validPtns(Args,Lc,D,Dx).
validPtn(enum(_),_,Dx,Dx).
validPtn(nth(Lc,Rc,_),_,D,Dx) :-
  validPtn(Rc,Lc,D,Dx).
validPtn(whr(Lc,Ptn,Cond),_,D,Dx) :-
  validPtn(Ptn,Lc,D,D0),
  glVars(Cond,D0,Dx),
  validTerm(Cond,Lc,Dx).
validPtn(varNames(Lc,_,Value),_,D,Dx) :-
  validPtn(Value,Lc,D,Dx).
validPtn(T,Lc,D,D) :-
  reportError("(internal) Invalid pattern %s",[ltrm(T)],Lc).

validPtns([],_,Dx,Dx) :-!.
validPtns([P|Ps],Lc,D,Dx) :-
  validPtn(P,Lc,D,D0),
  validPtns(Ps,Lc,D0,Dx).

validCases(Cases,Leaf,D) :-
  check_implies(is_member((Ptn,Val,Lc),Cases),lterms:validCase(Lc,Ptn,Val,Leaf,D)).

validCase(Lc,Ptn,Val,Leaf,D) :-
  validPtn(Ptn,Lc,D,D1),
  call(Leaf,Val,Lc,D1).

validAct(A,Lc,D) :-
  validAction(A,Lc,D,_).

validAction(nop(_),_,D,D) :- !.
validAction(seq(Lc,L,R),_,D,Dx) :-!,
  validAction(L,Lc,D,D0),
  validAction(R,Lc,D0,Dx).
validAction(lbld(Lc,_,A),_,D,Dx) :-!,
  validAction(A,Lc,D,Dx).
validAction(brk(_,_),_,D,D) :- !.
validAction(vls(Lc,E),_,D,D) :- !,
  validTerm(E,Lc,D).
validAction(rse(Lc,E),_,D,D) :- !,
  validTerm(E,Lc,D).
validAction(thrw(Lc,E),_,D,D) :- !,
  validTerm(E,Lc,D).
validAction(perf(Lc,E),_,D,D) :- !,
  validTerm(E,Lc,D).
validAction(defn(Lc,P,E),_,D,Dx) :- !,
  validPtn(P,Lc,D,Dx),
  validTerm(E,Lc,Dx).
validAction(mtch(Lc,P,E),_,D,Dx) :- !,
  validPtn(P,Lc,D,Dx),
  validTerm(E,Lc,Dx).
validAction(defn(Lc,P,E),_,D,D) :- !,
  validTerm(P,Lc,D),
  validTerm(E,Lc,D).
validAction(asgn(Lc,P,E),_,D,D) :- !,
  validTerm(P,Lc,D),
  validTerm(E,Lc,D).
validAction(case(Lc,G,C),_,D,D) :-
  validTerm(G,Lc,D),
  validCases(C,lterms:validAct,D).
validAction(unpack(Lc,G,C),_,D,D) :-
  validTerm(G,Lc,D),
  validCases(C,lterms:validAct,D).
validAction(iftte(Lc,G,L,R),_,D,Dx) :-!,
  glVars(G,D,D0),
  validTerm(G,Lc,D0),
  validAction(L,Lc,D0,D1),
  validAction(R,Lc,D,D2),
  merge(D1,D2,Dx).
validAction(iftt(Lc,G,L),_,D,D) :-!,
  glVars(G,D,D0),
  validTerm(G,Lc,D0),
  validAction(L,Lc,D0,_).
validAction(whle(Lc,G,L),_,D,D) :-!,
  glVars(G,D,D0),
  validTerm(G,Lc,D0),
  validAction(L,Lc,D0,_).
validAction(ffor(Lc,P,S,B),_,D,D) :-!,
  validTerm(S,Lc,D),
  validPtn(P,Lc,D,D0),
  validAction(B,Lc,D0,_).
validAction(ltt(Lc,V,E,B),_,D,D) :-!,
  validTerm(E,Lc,D),
  ptnVars(V,D,D1),
  validTerm(V,Lc,D1),
  validAction(B,Lc,D1,_).
validAction(rtire(Lc,L,G),_,D,D) :-!,
  validTerm(L,Lc,D),
  validTerm(G,Lc,D).
validAction(error(Lc,M),_,D,D) :-
  validTerm(M,Lc,D).
validAction(try(Lc,B,E,H),_,D,D) :-
  validAction(B,Lc,D,_),
  ptnVars(E,D,D0),
  validAction(H,Lc,D0,_).

validAction(T,Lc,D,D) :-
  reportError("(internal) Invalid action %s",[lact(T)],Lc).

ptnVars(idnt(Nm),D,Dx) :-
  add_mem(Nm,D,Dx).
ptnVars(anon,Dx,Dx).
ptnVars(voyd,Dx,Dx).
ptnVars(intgr(_),Dx,Dx).
ptnVars(bigx(_),Dx,Dx).
ptnVars(float(_),Dx,Dx).
ptnVars(chr(_),Dx,Dx).
ptnVars(strg(_),Dx,Dx).
ptnVars(lbl(_,_),Dx,Dx).
ptnVars(cll(_,_,Args),D,Dx) :-
  rfold(Args,lterms:ptnVars,D,Dx).
ptnVars(ocall(_,_,Args),D,Dx) :-
  rfold(Args,lterms:ptnVars,D,Dx).
ptnVars(ecll(_,_,Args),D,Dx) :-
  rfold(Args,lterms:ptnVars,D,Dx).
ptnVars(intrinsic(_,_,Args),D,Dx) :-
  rfold(Args,lterms:ptnVars,D,Dx).
ptnVars(ctpl(_,Args),D,Dx) :-
  rfold(Args,lterms:ptnVars,D,Dx).
ptnVars(enum(_),Dx,Dx).
ptnVars(nth(_,Rc,_),D,Dx) :-
  ptnVars(Rc,D,Dx).
ptnVars(whr(_,Ptn,Cond),D,Dx) :-
  ptnVars(Ptn,D,D0),
  glVars(Cond,D0,Dx).

glVars(cnj(_,L,R),D,Dx) :-
  glVars(L,D,D0),
  glVars(R,D0,Dx).
glVars(dsj(_,L,R),D,Dx) :-
  glVars(L,D,D0),
  glVars(R,D0,Dx).
glVars(cnd(_,T,L,R),D,Dx) :-
  glVars(T,[],D0),
  glVars(L,D0,D1),
  glVars(R,D,D2),
  intersect(D1,D2,Di),
  merge(Di,D,Dx).
glVars(mtch(_,L,_),D,Dx) :-
  ptnVars(L,D,Dx).
glVars(_,Dx,Dx).

