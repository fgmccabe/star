:- module(lterms,[ssTransformed/2,
		  ssRuleSet/3,dispRuleSet/1,dispProg/1,dispEquations/1,dispRules/1,
		  substTerm/3,substGoal/3,substAction/3,
		  substTerms/3,
		  genTplStruct/2,isLiteral/1,isCnd/1,mkTpl/2,isTpl/1,
		  isTplLbl/2,mkCons/3,
		  isUnit/1,
		  termHash/2,
		  ssTrm/3,dispTerm/1,showTerm/4,locTerm/2,dispAct/1,ssConsMap/2,
		  idInTerm/2, 
		  mergeGl/4,
		  validLProg/2,
		  uniqify/2,
		  tipeOf/2]).

:- use_module(display).
:- use_module(misc).
:- use_module(canon).
:- use_module(operators).
:- use_module(location).
:- use_module(types).
:- use_module(escapes).
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
ssRuleSet(prDef(_Lc,Nm,_Tp,Args,Act),sq([ss("Pr: "),NN,lp,AA,rp,ss("{"),AS,ss("}")])) :-
  ssTrm(Nm,0,NN),
  showArgs(Args,0,AA),
  ssAct(Act,2,AS).
ssRuleSet(glbDef(_Lc,Nm,_Tp,Value),sq([ss("Gl: "),id(Nm),ss(" = "),VV])) :-
  ssTrm(Value,0,VV).
ssRuleSet(typDef(_Lc,Tp,_Rl,IxMap),
	  sq([ss("Tp: "),TT,ss(" : "),MM])) :-
  ssType(Tp,false,0,TT),
  ssConsMap(IxMap,MM).
ssRuleSet(lblDef(_Lc,Lbl,_Tp,Ix),
	  sq([ss("Cn: "),LL,ss(" @ "),ix(Ix)])) :-
  ssTrm(Lbl,0,LL).

ssRuleSet(Df,_,DD) :-
  ssRuleSet(Df,DD).

dispEquations(Eqs) :-
  map(Eqs,lterms:ssEqn(lterms:ssTrm),EE),
  displayln(iv(nl(0),EE)).

ssEqn(Dsp,(_Lc,Args,Grd,Val),sq([ss("("),AA,ss(")=>"),GG,VV])) :-
  showArgs(Args,0,AA),
  ssGuard(Grd,GG),
  call(Dsp,Val,0,VV).

dispRules(Eqs) :-
  map(Eqs,lterms:ssRle,EE),
  displayln(iv(nl(0),EE)).

ssRle((_Lc,Args,Grd,A),sq([ss("("),RR,ss(") do "),GG,AA])) :-
  showArgs(Args,0,RR),
  ssGuard(Grd,GG),
  ssAct(A,2,AA).

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

ssConsEntry((Lb,Ix),sq([LL,ss("@"),ix(Ix)])) :-
  ssTrm(Lb,0,LL).
  
showArgs(Args,Dp,iv(ss(","),AA)) :-
  map(Args,lterms:swTrm(Dp),AA).

swTrm(Dp,T,S) :- ssTrm(T,Dp,S).

showTerm(Trm,Dp,O,Ox) :-
  ss_to_chrs(lterms:ssTrm(Trm,Dp),O,Ox).

ssTrm(voyd,_,ss("void")) :-!.
ssTrm(unreach(_,_),_,ss("unreachable")) :-!.
ssTrm(idnt(Nm,_T),_Dp,sq([id(Nm)])) :-!.
%% ssTrm(idnt(Nm,T),Dp,sq([id(Nm),ss(":"),STp])) :-!,
%%       ssType(T,false,Dp,STp).
ssTrm(enum(Nm),_,sq([ss("."),id(Nm)])).
ssTrm(ann(_),_,ss("_")) :-!.
ssTrm(intgr(Ix),_,ix(Ix)) :-!.
ssTrm(bigx(Ix),_,ss(Ix)) :-!.
ssTrm(flot(Dx),_,fx(Dx)) :-!.
ssTrm(chr(Cp),_,sq([ss("`"),cp(Cp),ss("`")])) :-!.
ssTrm(strg(Str),_,qt(Str,'\"')) :-!.
ssTrm(thrw(_,E),Dp,sq([ss("throw "),EE])) :-
  ssTrm(E,Dp,EE).
ssTrm(cll(_,Op,Args,_Tp),Dp,sq([OO,lp,AA,rp])) :- !,
  ssTrm(Op,Dp,OO),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(xcll(_,Op,Args,_Tp,ErTp),Dp,sq([OO,lp,AA,rp,ss(" throws "),EE])) :- !,
  ssType(ErTp,false,Dp,EE),
  ssTrm(Op,Dp,OO),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(ocall(_,Op,Args,_),Dp,sq([OO,ss("°"),lp,AA,rp])) :-!,
  ssTrm(Op,Dp,OO),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(xocall(_,Op,Args,_,ErTp),Dp,sq([OO,ss("°°"),lp,AA,rp, ss(" throws "),EE])) :-!,
  ssType(ErTp,false,Dp,EE),
  ssTrm(Op,Dp,OO),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(ecll(_,Es,Args,_),Dp,sq([ss("ε"),ss(Es),ss("("),AA,ss(")")])) :-!,
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(xecll(_,Es,Args,_,ErTp),Dp,sq([ss("ε"),ss(Es),ss("("),AA,ss(")"),ss(" throws "),EE])) :-!,
  ssType(ErTp,false,Dp,EE),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(ctpl(Op,A),Dp,sq([ss("."),OO,lp,AA,rp])) :-!,
  ssCOnOp(Op,OO),
  Dp1 is Dp+2,
  showArgs(A,Dp1,AA).
ssTrm(clos(Nm,Ar,Free,Tp),Dp,sq([ss("<"),id(Nm),ss("/"),ix(Ar),ss("|"),FF,ss(":"),TT,ss(">")])) :-
  ssTrm(Free,Dp,FF),
  ssType(Tp,false,Dp,TT).
ssTrm(enum(Nm,_Tp),_,sq([ss("."),id(Nm)])) :-!.
ssTrm(nth(_,Rc,Off,_),Dp,sq([OO,ss("."),ix(Off)])) :-!,
  ssTrm(Rc,Dp,OO).
ssTrm(setix(_,Rc,Off,Vl),Dp,sq([OO,ss("."),ix(Off),ss(":="),VV])) :-!,
  ssTrm(Rc,Dp,OO),
  ssTrm(Vl,Dp,VV).
ssTrm(cel(_,C),Dp,sq([ss("ref "),CC])) :-!,
  ssTrm(C,Dp,CC).
ssTrm(get(_,C,_),Dp,sq([CC,ss("!")])) :-!,
  ssTrm(C,Dp,CC).
ssTrm(sav(_,_),_Dp,sq([ss("sav"),lp,rp])) :-!.
ssTrm(savIsSet(_,S),Dp,sq([SS,ss("?")])) :-!,
  ssTrm(S,Dp,SS).
ssTrm(savGet(_,Rf,_),Dp,sq([lp,RR,ss("^"),rp])) :-!,
  ssTrm(Rf,Dp,RR).
ssTrm(savSet(_,Rf,Vl),Dp,sq([lp,RR,ss("?:=?"),VV,rp])) :-!,
  ssTrm(Rf,Dp,RR),
  ssTrm(Vl,Dp,VV).
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
ssTrm(cse(_,G,Cases,Deflt,_),Dp,
      sq([ss("case "),GG,ss(" in "),CC,ss(" else "),DD])) :-!,
  ssTrm(G,Dp,GG),
  ssCases(Cases,Dp,lterms:ssTrm,CC),
  ssTrm(Deflt,Dp,DD).
ssTrm(unpack(_,G,Cases,Deflt,_),Dp,
      sq([ss("unpack "),GG,ss(" in "),CC,ss(" else "),DD])) :-!,
  ssTrm(G,Dp,GG),
  ssCases(Cases,Dp,lterms:ssTrm,CC),
  ssTrm(Deflt,Dp,DD).
ssTrm(seqD(_,L,R),Dp,sq([LL,ss(";"),RR])) :-!,
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(cnj(_,L,R),Dp,sq([lp,LL,ss("&&"),RR,rp])) :-!,
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(dsj(_,L,R),Dp,sq([lp,LL,ss("||"),RR,rp])) :-!,
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(cnd(_,T,L,R),Dp,sq([lp,TT,ss(" ?? "),nl(Dp1),LL,ss("||"),nl(Dp1),RR,rp])) :-!,
  Dp1 is Dp+2,
  ssTrm(T,Dp,TT),
  ssTrm(L,Dp1,LL),
  ssCnd(R,Dp,RR).
ssTrm(mtch(_,L,R),Dp,sq([lp,LL,ss(".="),RR,rp])) :-!,
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(ng(_,R),Dp,sq([ss("~"),lp,RR,rp])) :-!,
  ssTrm(R,Dp,RR).
ssTrm(error(Lc,M),Dp,sq([lp,ss("error "),MM,rp,ss("@"),LL])) :-!,
  ssTrm(M,Dp,MM),
  ssLoc(Lc,LL).
ssTrm(tsk(_,A),Dp,sq([ss("task "),AA])) :-
  ssTrm(A,Dp,AA).
ssTrm(vlof(_,A,_),Dp,sq([ss("valof "),AA])) :-
  ssAct(A,Dp,AA).
ssTrm(tryX(_,B,_E,H),Dp,sq([ss("try "),BB,ss(" catch "),HH])) :-
  Dp2 is Dp+2,
  ssTrm(B,Dp2,BB),
  ssTrm(H,Dp,HH).
ssTrm(susp(_,T,E,_),Dp,sq([TT,ss(" suspend "),EE])) :-
  ssTrm(T,Dp,TT),
  ssTrm(E,Dp,EE).
ssTrm(rtire(_,T,E,_),Dp,sq([TT,ss(" retire "),EE])) :-
  ssTrm(T,Dp,TT),
  ssTrm(E,Dp,EE).
ssTrm(resme(_,T,E,_),Dp,sq([TT,ss(" resume "),EE])) :-
  ssTrm(T,Dp,TT),
  ssTrm(E,Dp,EE).

dispAct(A) :-
  display:display(lterms:ssAct(A,0)).

ssAct(nop(_),_,ss("{}")) :-!.
ssAct(seq(Lc,A,B),Dp,sq([ss("{"),nl(Dp2),iv(sq([ss(";"),nl(Dp2)]),AA),nl(Dp),ss("}")])) :-!,
  Dp2 is Dp+2,
  ssActSeq(seq(Lc,A,B),Dp2,AA).
ssAct(lbld(_,Lb,A),Dp,sq([ss(Lb),ss(":"),AA])) :-!,
  ssAct(A,Dp,AA).
ssAct(brk(_,Lb),_,sq([ss("break "),ss(Lb)])) :-!.
ssAct(vls(_,E),Dp,sq([ss("valis "),EE])) :-!,
  ssTrm(E,Dp,EE).
ssAct(aThrow(_,E),Dp,sq([ss("throw "),EE])) :-!,
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
ssAct(setix(_,Rc,Off,Vl),Dp,sq([OO,ss("."),ix(Off),ss(":="),VV])) :-!,
  ssTrm(Rc,Dp,OO),
  ssTrm(Vl,Dp,VV).
ssAct(case(_,G,Cases,Deflt),Dp,
      sq([ss("case "),GG,ss("in"),CC,ss(" else "),DD])) :-!,
  ssTrm(G,Dp,GG),
  ssCases(Cases,Dp,lterms:ssAct,CC),
  ssAct(Deflt,Dp,DD).
ssAct(unpack(_,G,Cases,Deflt),Dp,
      sq([ss("case "),GG,ss("in"),CC,ss(" else "),DD])) :-!,
  ssTrm(G,Dp,GG),
  ssCases(Cases,Dp,lterms:ssAct,CC),
  ssAct(Deflt,Dp,DD).
ssAct(iftte(_,G,T,nop(_)),Dp,
      sq([ss("if "),GG,ss(" then "),nl(Dp2),TT])) :-!,
  Dp2 is Dp+2,
  ssTrm(G,Dp,GG),
  ssAct(T,Dp2,TT).
ssAct(iftte(_,G,T,E),Dp,
      sq([ss("if "),GG,ss(" then "),nl(Dp2),TT,nl(Dp2),ss(" else "),EE])) :-!,
  Dp2 is Dp+2,
  ssTrm(G,Dp,GG),
  ssAct(T,Dp2,TT),
  ssAct(E,Dp2,EE).
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
ssAct(error(_,M),Dp,sq([ss(" error "),MM])) :-
  ssTrm(M,Dp,MM).
ssAct(aTry(_,B,_E,H),Dp,sq([ss("try "),BB,ss(" catch "),HH])) :-
  Dp2 is Dp+2,
  ssAct(B,Dp2,BB),
  ssAct(H,Dp2,HH).
ssAct(perf(_,E),Dp,sq([ss("do "),EE])) :-
  ssTrm(E,Dp,EE).

ssActSeq(seq(_,A,B),Dp,[AA|BB]) :-!,
  ssAct(A,Dp,AA),
  ssActSeq(B,Dp,BB).
ssActSeq(A,Dp,[AA]) :-
  ssAct(A,Dp,AA).

ssCnd(cnd(_,T,L,R),Dp,sq([TT,ss(" ?? "),nl(Dp),LL,ss("||"),nl(Dp),RR])) :-!,
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

dispTerm(T) :-
  display:display(lterms:ssTrm(T,0)).

makeSubstDict(Vrs,D) :-
  rfold(Vrs,lterms:decVr,qq{},D).

extendSubstDict(Vrs,D,Dx) :-
  rfold(Vrs,lterms:decVr,D,Dx).

decVr((Nm,SNm),Q,QQ):-
  makeKey(Nm,Key),
  put_dict(Key,Q,SNm,QQ).

substTerm(Q,In,Out) :-
  makeSubstDict(Q,QD),
  rewriteTerm(lterms:applyQ(QD),lterms:extendQ,In,Out),
  !.

substGoal(_,none,none) :-!.
substGoal(Q,some(In),some(Out)) :-
  substTerm(Q,In,Out).

substAction(Q,In,Out) :-
  makeSubstDict(Q,QD),
  rewriteAction(lterms:applyQ(QD),lterms:extendQ,In,Out),!.

applyQ(QD,Nm,Trm) :-
  makeKey(Nm,Key),
  get_dict(Key,QD,Trm),!.

extendQ(AQ,ltt,Vr,lterms:checkV(Vr,AQ)).
extendQ(AQ,gl,_,AQ).
extendQ(AQ,ptn,_,AQ).
extendQ(AQ,seq,_,AQ).

substTerms(Q,Els,NEls):-
  makeSubstDict(Q,QD),
  map(Els,lterms:rewriteTerm(lterms:applyQ(QD),lterms:extendQ),NEls).

rewriteTerm(AQ,_,idnt(Nm,_),Trm) :-
  call(AQ,Nm,Trm),!.
rewriteTerm(_,_,idnt(Nm,Tp),idnt(Nm,Tp)).
rewriteTerm(_,_,voyd,voyd).
rewriteTerm(_,_,unreach(Lc,Tp),unreach(Lc,Tp)).
rewriteTerm(_,_,intgr(Ix),intgr(Ix)).
rewriteTerm(_,_,bigx(Ix),bigx(Ix)).
rewriteTerm(_,_,ann(T),ann(T)).
rewriteTerm(_,_,flot(Dx),flot(Dx)).
rewriteTerm(_,_,chr(Cp),chr(Cp)).
rewriteTerm(_,_,strg(Sx),strg(Sx)).
rewriteTerm(_,_,enum(Nm),enum(Nm)).
rewriteTerm(_,_,lbl(Nm,Ar),lbl(Nm,Ar)).
rewriteTerm(AQ,EX,ltt(Lc,V,Val,Exp),ltt(Lc,NV,Val1,Exp1)) :-
  call(EX,AQ,ltt,V,NQ),
  rewriteTerm(NQ,EX,V,NV),
  rewriteTerm(NQ,EX,Val,Val1),
  rewriteTerm(NQ,EX,Exp,Exp1).
rewriteTerm(AQ,EX,thrw(Lc,E),thrw(Lc,EE)) :-!,
  rewriteTerm(AQ,EX,E,EE).
rewriteTerm(AQ,EX,ecll(Lc,Call,Args,Tp),ecll(Lc,Call,NArgs,Tp)) :-
  rewriteTerms(AQ,EX,Args,NArgs).
rewriteTerm(AQ,EX,xecll(Lc,Call,Args,Tp,ErTp),xecll(Lc,Call,NArgs,Tp,ErTp)) :-
  rewriteTerms(AQ,EX,Args,NArgs).
rewriteTerm(AQ,EX,cll(Lc,Op,Args,Tp),cll(Lc,NOp,NArgs,Tp)) :-
  rewriteTerm(AQ,EX,Op,NOp),
  rewriteTerms(AQ,EX,Args,NArgs).
rewriteTerm(AQ,EX,xcll(Lc,Op,Args,Tp,ErTp),xcll(Lc,NOp,NArgs,Tp,ErTp)) :-
  rewriteTerm(AQ,EX,Op,NOp),
  rewriteTerms(AQ,EX,Args,NArgs).
rewriteTerm(AQ,EX,ocall(Lc,Op,Args,Tp),ocall(Lc,NOp,NArgs,Tp)) :-
  rewriteTerm(AQ,EX,Op,NOp),
  rewriteTerms(AQ,EX,Args,NArgs).
rewriteTerm(AQ,EX,xocall(Lc,Op,Args,Tp,ErTp),xocall(Lc,NOp,NArgs,Tp,ErTp)) :-
  rewriteTerm(AQ,EX,Op,NOp),
  rewriteTerms(AQ,EX,Args,NArgs).
rewriteTerm(AQ,EX,nth(Lc,Op,Off,Tp),nth(Lc,NOp,Off,Tp)) :-
  rewriteTerm(AQ,EX,Op,NOp).
rewriteTerm(AQ,EX,cel(Lc,T),cel(Lc,NT)) :-
  rewriteTerm(AQ,EX,T,NT).
rewriteTerm(AQ,EX,get(Lc,T,Tp),get(Lc,NT,Tp)) :-
  rewriteTerm(AQ,EX,T,NT).
rewriteTerm(AQ,EX,setix(Lc,Op,Off,Vl),setix(Lc,NOp,Off,NVl)) :-
  rewriteTerm(AQ,EX,Op,NOp),
  rewriteTerm(AQ,EX,Vl,NVl).
rewriteTerm(_AQ,_EX,sav(Lc,Tp),sav(Lc,Tp)) :-!.
rewriteTerm(AQ,EX,savIsSet(Lc,S),savIsSet(Lc,SS)) :-!,
  rewriteTerm(AQ,EX,S,SS).
rewriteTerm(AQ,EX,savGet(Lc,L,Tp),savGet(Lc,LL,Tp)) :-
  rewriteTerm(AQ,EX,L,LL).
rewriteTerm(AQ,EX,savSet(Lc,Lam,Vl),savSet(Lc,LLam,VV)) :-
  rewriteTerm(AQ,EX,Lam,LLam),
  rewriteTerm(AQ,EX,Vl,VV).
rewriteTerm(AQ,EX,clos(Nm,Ar,Free,Tp),clos(Nm,Ar,NFree,Tp)) :-
  rewriteTerm(AQ,EX,Free,NFree).
rewriteTerm(AQ,EX,ctpl(Op,Args),ctpl(NOp,NArgs)) :-
  rewriteTerm(AQ,EX,Op,NOp),
  rewriteTerms(AQ,EX,Args,NArgs).
rewriteTerm(AQ,EX,whr(Lc,T,C),whr(Lc,NT,NC)) :-
  rewriteGoal(AQ,EX,T,NT,NQ),
  rewriteTerm(NQ,EX,C,NC).
rewriteTerm(AQ,EX,cse(Lc,G,C,D,T),cse(Lc,NG,NC,ND,T)) :-
  rewriteTerm(AQ,EX,G,NG),
  map(C,lterms:rewriteCase(AQ,EX,lterms:rewriteTerm),NC),
  rewriteTerm(AQ,EX,D,ND).
rewriteTerm(AQ,EX,unpack(Lc,T,C,D,Tp),unpack(Lc,NT,NC,ND,Tp)) :-
  rewriteTerm(AQ,EX,T,NT),
  map(C,lterms:rewriteCase(AQ,EX,lterms:rewriteTerm),NC),
  rewriteTerm(AQ,EX,D,ND).
rewriteTerm(AQ,EX,seqD(Lc,L,R),seqD(Lc,NL,NR)) :-
  rewriteTerm(AQ,EX,L,NL),
  rewriteTerm(AQ,EX,R,NR).
rewriteTerm(AQ,EX,G,Gx) :-
  isCnd(G),!,
  rewriteGoal(AQ,EX,G,Gx,_).
rewriteTerm(AQ,EX,cnd(Lc,T,L,R),cnd(Lc,NT,NL,NR)) :-
  rewriteGoal(AQ,EX,T,NT,NQ),
  rewriteTerm(NQ,EX,L,NL),
  rewriteTerm(NQ,EX,R,NR).
rewriteTerm(AQ,EX,ng(Lc,R),ng(Lc,NR)) :-
  rewriteTerm(AQ,EX,R,NR).
rewriteTerm(AQ,EX,vlof(Lc,A,Tp),vlof(Lc,AA,Tp)) :-
  rewriteAction(AQ,EX,A,AA).
rewriteTerm(AQ,EX,tsk(Lc,F),tsk(Lc,FF)) :-
  rewriteTerm(AQ,EX,F,FF).
rewriteTerm(AQ,EX,resme(Lc,T,E,Tp),resme(Lc,TT,EE,Tp)) :-
  rewriteTerm(AQ,EX,T,TT),
  rewriteTerm(AQ,EX,E,EE).
rewriteTerm(AQ,EX,susp(Lc,T,E,Tp),susp(Lc,TT,EE,Tp)) :-
  rewriteTerm(AQ,EX,T,TT),
  rewriteTerm(AQ,EX,E,EE).
rewriteTerm(AQ,EX,rtire(Lc,T,E,Tp),rtire(Lc,TT,EE,Tp)) :-
  rewriteTerm(AQ,EX,T,TT),
  rewriteTerm(AQ,EX,E,EE).
rewriteTerm(AQ,EX,error(Lc,M),error(Lc,MM)) :-!,
  rewriteTerm(AQ,EX,M,MM).
rewriteTerm(AQ,EX,tryX(Lc,B,E,H),tryX(Lc,BB,EE,HH)) :-!,
  rewriteTerm(AQ,EX,E,EE),
  rewriteTerm(AQ,EX,B,BB),
  rewriteTerm(AQ,EX,H,HH).

rewriteTerms(AQ,EX,Els,NEls):-
  map(Els,lterms:rewriteTerm(AQ,EX),NEls).

rewritePtn(AQ,_,idnt(Nm,_),Trm) :-
  call(AQ,Nm,Trm),!.
rewritePtn(_,_,idnt(Nm,Tp),idnt(Nm,Tp)).
rewritePtn(_,_,voyd,voyd).
rewritePtn(_,_,unreach(Lc,Tp),unreach(Lc,Tp)).
rewritePtn(_,_,intgr(Ix),intgr(Ix)).
rewritePtn(_,_,bigx(Ix),bigx(Ix)).
rewritePtn(_,_,ann(T),ann(T)).
rewritePtn(_,_,flot(Dx),flot(Dx)).
rewritePtn(_,_,chr(Cp),chr(Cp)).
rewritePtn(_,_,strg(Sx),strg(Sx)).
rewritePtn(_,_,enum(Nm),enum(Nm)).
rewritePtn(_,_,lbl(Nm,Ar),lbl(Nm,Ar)).
rewritePtn(AQ,EX,savGet(Lc,L,Tp),savGet(Lc,LL,Tp)) :-
  rewritePtn(AQ,EX,L,LL).
rewritePtn(AQ,EX,ctpl(Op,Args),ctpl(NOp,NArgs)) :-
  rewritePtn(AQ,EX,Op,NOp),
  rewritePtns(AQ,EX,Args,NArgs).
rewritePtn(AQ,EX,whr(Lc,T,C),whr(Lc,NT,NC)) :-
  rewritePtn(AQ,EX,T,NT), % variables already extracted
  rewriteGl(AQ,EX,C,NC).

rewritePtns(AQ,EX,Els,NEls):-
  map(Els,lterms:rewritePtn(AQ,EX),NEls).

rewriteGoal(AQ,EX,Gl,RGl,NQ) :-
  call(EX,AQ,gl,Gl,NQ),
  rewriteGl(NQ,EX,Gl,RGl).
  
newVar((Nm,Tp),(Nm,idnt(NNm,Tp))) :-
  genstr(Nm,NNm).

idVar(idnt(Nm,Tp),(Nm,idnt(Nm,Tp))).

rewriteGl(AQ,EX,cnj(Lc,L,R),cnj(Lc,NL,NR)) :-
  rewriteGl(AQ,EX,L,NL),
  rewriteGl(AQ,EX,R,NR).
rewriteGl(AQ,EX,dsj(Lc,L,R),dsj(Lc,NL,NR)) :-
  rewriteGl(AQ,EX,L,NL),
  rewriteGl(AQ,EX,R,NR).
rewriteGl(AQ,EX,cnd(Lc,T,L,R),cnd(Lc,NT,NL,NR)) :-
  rewriteGl(AQ,EX,T,NT),
  rewriteGl(AQ,EX,L,NL),
  rewriteGl(AQ,EX,R,NR).
rewriteGl(AQ,EX,mtch(Lc,L,R),mtch(Lc,NL,NR)) :-
  rewritePtn(AQ,EX,L,NL),
  rewriteTerm(AQ,EX,R,NR).
rewriteGl(AQ,EX,ng(Lc,R),ng(Lc,NR)) :-
  rewriteGl(AQ,EX,R,NR).
rewriteGl(AQ,EX,T,NT) :-
  rewriteTerm(AQ,EX,T,NT).

rewriteAction(_,_,nop(Lc),nop(Lc)) :- !.
rewriteAction(AQ,EX,seq(Lc,L,R),seq(Lc,LL,RR)) :-!,
  call(EX,AQ,seq,L,NQ),
  rewriteAction(NQ,EX,L,LL),
  rewriteAction(NQ,EX,R,RR).
rewriteAction(AQ,EX,lbld(Lc,Lb,A),lbld(Lc,Lb,AA)) :- !,
  rewriteAction(AQ,EX,A,AA).
rewriteAction(_,_,brk(Lc,Lb),brk(Lc,Lb)) :-!.
rewriteAction(AQ,EX,vls(Lc,E),vls(Lc,EE)) :- !,
  rewriteTerm(AQ,EX,E,EE).
rewriteAction(AQ,EX,aThrow(Lc,E),aThrow(Lc,EE)) :- !,
  rewriteTerm(AQ,EX,E,EE).
rewriteAction(AQ,EX,perf(Lc,E),perf(Lc,EE)) :- !,
  rewriteTerm(AQ,EX,E,EE).
rewriteAction(AQ,EX,mtch(Lc,P,E),mtch(Lc,PP,EE)) :- !,
  rewriteTerm(AQ,EX,P,PP),
  rewriteTerm(AQ,EX,E,EE).
rewriteAction(AQ,EX,defn(Lc,P,E),defn(Lc,PP,EE)) :- !,
  rewriteTerm(AQ,EX,P,PP),
  rewriteTerm(AQ,EX,E,EE).
rewriteAction(AQ,EX,asgn(Lc,P,E),asgn(Lc,PP,EE)) :- !,
  rewriteTerm(AQ,EX,P,PP),
  rewriteTerm(AQ,EX,E,EE).
rewriteAction(AQ,EX,setix(Lc,P,Ix,E),setix(Lc,PP,Ix,EE)) :- !,
  rewriteTerm(AQ,EX,P,PP),
  rewriteTerm(AQ,EX,E,EE).
rewriteAction(AQ,EX,case(Lc,G,C,D),case(Lc,GG,CC,ND)) :-
  rewriteTerm(AQ,EX,G,GG),
  rewriteAction(AQ,EX,D,ND),
  map(C,lterms:rewriteCase(AQ,EX,lterms:rewriteAction),CC).
rewriteAction(AQ,EX,unpack(Lc,G,C,D),unpack(Lc,GG,CC,ND)) :-
  rewriteTerm(AQ,EX,G,GG),
  rewriteAction(AQ,EX,D,ND),
  map(C,lterms:rewriteCase(AQ,EX,lterms:rewriteAction),CC).
rewriteAction(AQ,EX,iftte(Lc,G,L,R),iftte(Lc,GG,LL,RR)) :-!,
  rewriteGoal(AQ,EX,G,GG,NQ),
  rewriteAction(NQ,EX,L,LL),
  rewriteAction(AQ,EX,R,RR).
rewriteAction(AQ,EX,whle(Lc,G,B),whle(Lc,GG,BB)) :-!,
  rewriteGoal(AQ,EX,G,GG,Q1),
  rewriteAction(Q1,EX,B,BB).
rewriteAction(AQ,EX,ffor(Lc,P,S,B),ffor(Lc,PP,SS,BB)) :-!,
  rewriteTerm(AQ,EX,P,PP),
  rewriteTerm(AQ,EX,S,SS),
  rewriteAction(AQ,EX,B,BB).
rewriteAction(AQ,EX,ltt(Lc,V,E,B),ltt(Lc,VV,EE,BB)) :-!,
  call(EX,AQ,ltt,V,NQ),
  rewriteTerm(NQ,EX,V,VV),
  rewriteTerm(NQ,EX,E,EE),
  rewriteAction(NQ,EX,B,BB).
rewriteAction(AQ,EX,error(Lc,M),error(Lc,MM)) :-!,
  rewriteTerm(AQ,EX,M,MM).
rewriteAction(AQ,EX,aTry(Lc,B,E,H),aTry(Lc,BB,EE,HH)) :-!,
  rewriteTerm(AQ,EX,E,EE),
  rewriteAction(AQ,EX,B,BB),
  rewriteAction(AQ,EX,H,HH).
  
rewriteCase(AQ,EX,BCall,(P,E,Lbl),(NP,NE,Lbl)) :-
  call(EX,AQ,ptn,P,NQ),
  rewriteTerm(NQ,EX,P,NP),
  call(BCall,NQ,EX,E,NE).

checkV(Vr,Other,T,T1) :-
  T\=Vr,
  call(Other,T,T1).

genTplStruct(Cx,lbl(Nm,Cx)) :-
  swritef(Nm,"()%d",[Cx]).

isTplLbl(Nm,Ar) :- string_concat("()",A,Nm),number_string(Ar,A).

mkTpl(Els,ctpl(C,Els)) :-
  length(Els,Cx),
  genTplStruct(Cx,C).

isTpl(ctpl(lbl(Lb,Ar),_)) :- isTplLbl(Lb,Ar).

mkCons(Nm,Els,ctpl(lbl(Nm,Arity),Els)) :-
  length(Els,Arity).

isUnit(ctpl(lbl("()0",0),[])).

isLiteral(intgr(_)).
isLiteral(bigx(_)).
isLiteral(flot(_)).
isLiteral(chr(_)).
isLiteral(strg(_)).
isLiteral(enum(_)).
isLiteral(lbl(_,_)).
isLiteral(ctpl(Lbl,Args)) :-
  isLiteral(Lbl),
  check_implies(misc:is_member(A,Args),lterms:isLiteral(A)),!.
isLiteral(clos(_,_,F,_)) :-
  isLiteral(F).

termHash(voyd,0).
termHash(unreach,0).
termHash(intgr(Ix),Ix).
termHash(bigx(Bx),Hx) :- bigHash(Bx,Hx).
termHash(flot(Dx),Hx) :- Ix is round(Dx), hashSixtyOne(Ix,Hx).
termHash(chr(Cp),Ix) :- charHash(0,Cp,Ix).
termHash(strg(Sx),Ix) :- stringHash(0,Sx,Ix).
termHash(enum(Sx),Ix) :- termHash(lbl(Sx,0),Ix).
termHash(lbl(Nm,Ar),Hx) :-
  stringHash(0,Nm,Lx),
  Ix is Ar*37+Lx,
  hashSixtyOne(Ix,Hx).

locTerm(loc(Pk,Ln,Col,Pos,Len),Tpl) :-
  mkTpl([strg(Pk),intgr(Ln),intgr(Col),intgr(Pos),intgr(Len)],Tpl).

idInTerm(idnt(Nm,_),Term) :-
  inTerm(Term,Nm),!.

inTerm(idnt(Nm,_),Nm).
inTerm(cll(_,_,Args,_),Nm) :-
  is_member(Arg,Args),
  inTerm(Arg,Nm).
inTerm(xcll(_,_,Args,_,_),Nm) :-
  is_member(Arg,Args),
  inTerm(Arg,Nm).
inTerm(ocall(_,Op,Args,_),Nm) :-
  (is_member(Arg,Args), inTerm(Arg,Nm) ; inTerm(Op,Nm)),!.
inTerm(xocall(_,Op,Args,_,_),Nm) :-
  (is_member(Arg,Args), inTerm(Arg,Nm) ; inTerm(Op,Nm)),!.
inTerm(ecll(_,_,Args,_),Nm) :-
  is_member(Arg,Args),
  inTerm(Arg,Nm),!.
inTerm(xecll(_,_,Args,_,_),Nm) :-
  is_member(Arg,Args),
  inTerm(Arg,Nm),!.
inTerm(clos(_,_,Fr,_),Nm) :-
  inTerm(Fr,Nm).
inTerm(nth(_,Op,_,_),Nm) :-
  inTerm(Op,Nm).
inTerm(cel(_,C),Nm) :-
  inTerm(C,Nm).
inTerm(get(_,C,_),Nm) :-
  inTerm(C,Nm).
inTerm(setix(_,Op,_,_),Nm) :-
  inTerm(Op,Nm).
inTerm(setix(_,_,_,Vl),Nm) :-
  inTerm(Vl,Nm).
inTerm(ctpl(_,Args),Nm) :-
  is_member(Arg,Args), inTerm(Arg,Nm),!.
inTerm(resme(_,T,_M,_),Nm) :-
  is_member(T,Nm).
inTerm(resme(_,_T,M,_),Nm) :-
  inTerm(M,Nm),!.
inTerm(susp(_,T,_M,_),Nm) :-
  is_member(T,Nm).
inTerm(susp(_,_T,M,_),Nm) :-
  inTerm(M,Nm),!.
inTerm(rtire(_,T,_M,_),Nm) :-
  is_member(T,Nm),!.
inTerm(rtire(_,_T,M,_),Nm) :-
  inTerm(M,Nm),!.
inTerm(whr(_,T,_),Nm) :-
  inTerm(T,Nm),!.
inTerm(whr(_,_,C),Nm) :-
  inTerm(C,Nm),!.
inTerm(cse(_,T,_C,_),Nm) :-
  inTerm(T,Nm),!.
inTerm(cse(_,_T,C,_),Nm) :-
  is_member((P,V),C), (inTerm(P,Nm);inTerm(V,Nm)),!.
inTerm(unpack(_,T,_C,D,_),Nm) :-
  (inTerm(T,Nm);inTerm(D,Nm)),!.
inTerm(unpack(_,_T,C,_,_),Nm) :-
  is_member((P,V),C), (inTerm(P,Nm);inTerm(V,Nm)),!.
inTerm(seqD(_,L,R),Nm) :-!,
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(cnj(_,L,R),Nm) :-!,
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(dsj(_,L,R),Nm) :-!,
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(cnd(_,T,L,R),Nm) :-!,
  inTerm(T,Nm) ; inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(mtch(_,L,R),Nm) :-!,
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(ng(_,R),Nm) :-!,
  inTerm(R,Nm).
inTerm(ltt(_,_,B,E),Nm) :-!,
  (inTerm(B,Nm);inTerm(E,Nm)).
inTerm(vlof(_,A,_),Nm) :-!,
  inAction(A,Nm).
inTerm(sav(_,_),_) :-!,false.
inTerm(savIsSet(_,S),Nm) :-!,
  inTerm(S,Nm).
inTerm(savGet(_,Th,_),Nm) :-!,
  inTerm(Th,Nm).
inTerm(savSet(_,Th,Vl),Nm) :-!,
  (inTerm(Th,Nm) ; inTerm(Vl,Nm)).
inTerm(tryX(_,B,E,H),Nm) :-
  (inTerm(B,Nm) ; inTerm(E,Nm); inTerm(H,Nm)),!.
inTerm(thrw(_,E),Nm) :-
  inTerm(E,Nm),!.

inAction(nop(_),_) :- !,fail.
inAction(seq(_,L,R),Nm) :-!,
  (inAction(L,Nm) ; inAction(R,Nm)).
inAction(lbld(_,_,E),Nm) :- !,
  inAction(E,Nm).
inAction(vls(_,E),Nm) :- !,
  inTerm(E,Nm).
inAction(aThrow(_,E),Nm) :- !,
  inTerm(E,Nm),!.
inAction(perf(_,E),Nm) :- !,
  inTerm(E,Nm).
inAction(mtch(_,P,E),Nm) :- !,
  (inTerm(P,Nm) ; inTerm(E,Nm)).
inAction(defn(_,P,E),Nm) :- !,
  (inTerm(P,Nm) ; inTerm(E,Nm)).
inAction(setix(_,Op,_,_),Nm) :-!,
  inTerm(Op,Nm).
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
inAction(whle(_,G,L),Nm) :-!,
  (inTerm(G,Nm) ; inAction(L,Nm)).
inAction(ltt(_,_,E,B),Nm) :-!,
  (inTerm(E,Nm) ; inAction(B,Nm)).
inAction(error(_,M),Nm) :-!,
  inTerm(M,Nm).
inAction(aTry(_,B,E,H),Nm) :-!,
  (inAction(B,Nm) ;  inTerm(E,Nm) ; inAction(H,Nm)).

isCnd(cnj(_,_,_)).
isCnd(dsj(_,_,_)).
isCnd(mtch(_,_,_)).
isCnd(ng(_,R)) :- isCnd(R).
isCnd(cnd(_,L,R)) :- isCnd(L);isCnd(R).

tipeOf(voyd,voidType).
tipeOf(unreach(_,Tp),Tp).
tipeOf(ann(Tp),Tp).
tipeOf(idnt(_,T),T).
tipeOf(chr(_),type("char")).
tipeOf(intgr(_),type("integer")).
tipeOf(bigx(_),type("bigint")).
tipeOf(strg(_),type("string")).
tipeOf(flot(_),type("float")).
tipeOf(thrw(_,_),voidType).
tipeOf(cll(_,_,_,T),T).
tipeOf(xcll(_,_,_,T,_),T).
tipeOf(ocall(_,_,_,T),T).
tipeOf(xocall(_,_,_,T,_),T).
tipeOf(ecll(_,_,_,T),T).
tipeOf(xecll(_,_,_,T,_),T).
tipeOf(nth(_,_,_,T),T).
tipeOf(cel(_,V),T) :- tipeOf(V,VT), mkRefTp(VT,T).
tipeOf(get(_,_,T),T).
tipeOf(setix(_,_,_,_),voidType).
tipeOf(enum(_),voidType).
tipeOf(ctpl(_,Args),tplType(AA)) :-
  map(Args,lterms:tipeOf,AA).
tipeOf(resme(_,_,_,T),T).
tipeOf(susp(_,_,_,T),T).
tipeOf(rtire(_,_,_,T),T).
tipeOf(whr(_,E,_),T) :-
  tipeOf(E,T).
tipeOf(cse(_,_G,_C,_D,T),T).
tipeOf(unpack(_,_G,_C,_D,T),T).
tipeOf(seqD(_,_,R),T) :- tipeOf(R,T).
tipeOf(cnj(_,_,_),type("boolean")).
tipeOf(dsj(_,_,_),type("boolean")).
tipeOf(cnd(_,_,L,_),T) :- tipeOf(L,T).
tipeOf(mtch(_,_,_),type("boolean")).
tipeOf(ng(_,_),type("boolean")).
tipeOf(ltt(_,_,_,E),T) :- tipeOf(E,T).
tipeOf(tryX(_,B,_,_),T) :- tipeOf(B,T).
tipeOf(thrw(_,_),voidType).
tipeOf(sav(_,T),T).
tipeOf(savIsSet(_,_),type("boolean")).
tipeOf(savGet(_,_,Tp),Tp).
tipeOf(savSet(_,S,T),Nm) :-!,
  inTerm(S,Nm);inTerm(T,Nm).
tipeOf(clos(_,_,_,Tp),Tp).
tipeOf(vlof(_,_,T),T).
tipeOf(error(_,_),voidType).
tipeOf(T,_) :-
  reportFatal("Cannot determine type of %s",[ltrm(T)]).

validLProg(PkgDecls,mdule(_,_,_,_,Defs)) :-
  declareStd(Base),
  declareNms(PkgDecls,Base,Dct),
  validDfs(Defs,Dct),!.

validDfs([],_).
validDfs([D|Dfs],Dct) :-
  errorCount(E),
  validDf(D,Dct),!,
  (noNewErrors(E) ->
     validDfs(Dfs,Dct);
   reportMsg("(validate) Defn %s not valid",[ldef(D)])).

validDf(fnDef(Lc,_,_,_Tp,Args,Value),Dct) :-!,
  declareArgs(Args,Dct,D0),
  validTerm(Value,Lc,D0),!.
validDf(prDef(Lc,_,_Tp,Args,Act),Dct) :-!,
  declareArgs(Args,Dct,D0),
  validAction(Act,Lc,D0,_),!.
validDf(glbDef(Lc,_Nm,_Tp,Value),Dct) :-
  validTerm(Value,Lc,Dct).
validDf(typDef(_Lc,_Tp,_Rl,_IxMap),_) :-!.
validDf(lblDef(_,_,_,_),_).

declareStd(Base) :-
  stdDecl(Decls),
  rfold(Decls,lterms:declareDef,[],Base).

declareNms(Defs,Dct,Dx) :-
  rfold(Defs,lterms:declareDef,Dct,Dx).

declareDef(funDec(_,Nm,Tp),Dct,Dx) :-!,
  add_mem((Nm,Tp),Dct,Dx).
declareDef(varDec(_,Nm,Tp),Dct,Dx) :-!,
  add_mem((Nm,Tp),Dct,Dx).
declareDef(typeDec(_,_,_,_),Dx,Dx).
declareDef(contractDec(_,_,_),Dx,Dx).
declareDef(accDec(_,_,Nm,Tp),Dct,Dx) :-
  add_mem((Nm,Tp),Dct,Dx).
declareDef(updDec(_,_,Nm,Tp),Dct,Dx) :-
  add_mem((Nm,Tp),Dct,Dx).
declareDef(impDec(_,Nm,Tp),Dct,Dx) :-
  add_mem((Nm,Tp),Dct,Dx).
declareDef(cnsDec(_,Nm,Tp),Dct,Dx) :-
  add_mem((Nm,Tp),Dct,Dx).
       
declareArgs(Args,Dct,Dx) :-
  rfold(Args,lterms:declareArg,Dct,Dx).

declareArg(idnt(Nm,Tp),D,Dx) :-
  add_mem((Nm,Tp),D,Dx).

validTerm(idnt(Nm,_),Lc,D) :-
  (is_member((Nm,_),D) -> true ;
   reportError("(validate) Variable %s not in scope",[id(Nm)],Lc)).
validTerm(ann(_),_,_).
validTerm(voyd,_,_).
validTerm(unreach(_,_),_,_).
validTerm(intgr(_),_,_).
validTerm(bigx(_),_,_).
validTerm(flot(_),_,_).
validTerm(chr(_),_,_).
validTerm(strg(_),_,_).
validTerm(lbl(_,_),_,_).
validTerm(ecll(Lc,Es,Args,_),_,D) :-
  isEscape(Es),!,
  validTerms(Args,Lc,D).
validTerm(xecll(Lc,Es,Args,_,_),_,D) :-
  isEscape(Es),!,
  validTerms(Args,Lc,D).
validTerm(cll(Lc,lbl(_,_),Args,_),_,D) :-
  validTerms(Args,Lc,D).
validTerm(xcll(Lc,lbl(_,_),Args,_,_),_,D) :-
  validTerms(Args,Lc,D).
validTerm(ocall(Lc,Op,Args,_),_,D) :-
  validTerm(Op,Lc,D),
  validTerms(Args,Lc,D).
validTerm(xocall(Lc,Op,Args,_,_),_,D) :-
  validTerm(Op,Lc,D),
  validTerms(Args,Lc,D).
validTerm(clos(_,_,Free,_Tp),Lc,D) :-
  validTerm(Free,Lc,D).
validTerm(thrw(Lc,E),_,D) :-
  validTerm(E,Lc,D).
validTerm(ctpl(lbl(_,_),Args),Lc,D) :-
  validTerms(Args,Lc,D).
validTerm(enum(_),_,_).
validTerm(nth(Lc,Rc,Off,_),_,D) :-
  integer(Off),
  validTerm(Rc,Lc,D).
validTerm(cel(Lc,C),_,D) :-
  validTerm(C,Lc,D).
validTerm(get(Lc,C,_),_,D) :-
  validTerm(C,Lc,D).
validTerm(setix(Lc,Rc,Off,Vl),_,D) :-
  integer(Off),
  validTerm(Rc,Lc,D),
  validTerm(Vl,Lc,D).
validTerm(sav(_Lc,_),_,_D) :-!.
validTerm(savIsSet(Lc,S),_,D) :-
  validTerm(S,Lc,D).
validTerm(savGet(Lc,Th,_),_,D) :-
  validTerm(Th,Lc,D).
validTerm(savSet(Lc,Th,Vl),_,D) :-
  validTerm(Th,Lc,D),
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
validTerm(cse(Lc,G,Cases,Deflt,_),_,D) :-
  validTerm(G,Lc,D),
  validCases(Cases,lterms:validTerm,D),
  validTerm(Deflt,Lc,D).
validTerm(unpack(Lc,G,Cases,Deflt,_),_,D) :-
  validTerm(G,Lc,D),
  validCases(Cases,lterms:validTerm,D),
  validTerm(Deflt,Lc,D).
validTerm(tryX(Lc,B,E,H),_,D) :-
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
validTerm(vlof(Lc,A,_),_,D) :-
  validAction(A,Lc,D,_).
validTerm(error(Lc,R),_,D) :-
  validTerm(R,Lc,D).
validTerm(tsk(Lc,F),_,D) :-
  validTerm(F,Lc,D).
validTerm(resme(Lc,T,E,_),_,D) :-
  validTerm(T,Lc,D),
  validTerm(E,Lc,D).
validTerm(susp(Lc,T,E,_),_,D) :-
  validTerm(T,Lc,D),
  validTerm(E,Lc,D).
validTerm(rtire(Lc,T,E,_),_,D) :-
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

validPtn(idnt(Nm,Tp),_,D,Dx) :-
  add_mem((Nm,Tp),D,Dx).
validPtn(ann(_),_,Dx,Dx).
validPtn(voyd,_,Dx,Dx).
validPtn(intgr(_),_,Dx,Dx).
validPtn(bigx(_),_,Dx,Dx).
validPtn(flot(_),_,Dx,Dx).
validPtn(chr(_),_,Dx,Dx).
validPtn(strg(_),_,Dx,Dx).
validPtn(lbl(_,_),_,Dx,Dx).
validPtn(ctpl(lbl(_,_),Args),Lc,D,Dx) :-
  validPtns(Args,Lc,D,Dx).
validPtn(enum(_),_,Dx,Dx).
validPtn(nth(Lc,Rc,_,_),_,D,Dx) :-
  validPtn(Rc,Lc,D,Dx).
validPtn(savGet(Lc,V,_),_,D,Dx) :-
  validPtn(V,Lc,D,Dx).
validPtn(whr(Lc,Ptn,Cond),_,D,Dx) :-
  validPtn(Ptn,Lc,D,D0),
  glVars(Cond,D0,Dx),
  validTerm(Cond,Lc,Dx).
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
validAction(aThrow(Lc,E),_,D,D) :- !,
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
validAction(setix(Lc,Rc,Off,Vl),_,D,D) :-
  integer(Off),
  validTerm(Rc,Lc,D),
  validTerm(Vl,Lc,D).
validAction(asgn(Lc,P,E),_,D,D) :- !,
  validTerm(P,Lc,D),
  validTerm(E,Lc,D).
validAction(case(Lc,G,C,Df),_,D,Dx) :-
  validTerm(G,Lc,D),
  validCases(C,lterms:validAct,D),
  validAction(Df,Lc,D,Dx).
validAction(unpack(Lc,G,C,Df),_,D,Dx) :-
  validTerm(G,Lc,D),
  validCases(C,lterms:validAct,D),
  validAction(Df,Lc,D,Dx).
validAction(iftte(Lc,G,L,R),_,D,Dx) :-!,
  glVars(G,D,D0),
  validTerm(G,Lc,D0),
  validAction(L,Lc,D0,D1),
  validAction(R,Lc,D,D2),
  merge(D1,D2,Dx).
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
validAction(error(Lc,M),_,D,D) :-
  validTerm(M,Lc,D).
validAction(aTry(Lc,B,E,H),_,D,D) :-
  validAction(B,Lc,D,_),
  ptnVars(E,D,D0),
  validAction(H,Lc,D0,_).
validAction(T,Lc,D,D) :-
  reportError("(internal) Invalid action %s",[lact(T)],Lc).

ptnVars(idnt(Nm,Tp),D,Dx) :-
  add_mem((Nm,Tp),D,Dx).
ptnVars(ann(_),Dx,Dx).
ptnVars(voyd,Dx,Dx).
ptnVars(intgr(_),Dx,Dx).
ptnVars(bigx(_),Dx,Dx).
ptnVars(flot(_),Dx,Dx).
ptnVars(chr(_),Dx,Dx).
ptnVars(strg(_),Dx,Dx).
ptnVars(lbl(_,_),Dx,Dx).
ptnVars(cll(_,_,Args,_),D,Dx) :-
  ptnVarsList(Args,D,Dx).
ptnVars(ocall(_,_,Args,_),D,Dx) :-
  ptnVarsList(Args,D,Dx).
ptnVars(ecll(_,_,Args,_),D,Dx) :-
  ptnVarsList(Args,D,Dx).
ptnVars(ctpl(_,Args),D,Dx) :-
  ptnVarsList(Args,D,Dx).
ptnVars(enum(_),Dx,Dx).
ptnVars(nth(_,Rc,_,_),D,Dx) :-
  ptnVars(Rc,D,Dx).
ptnVars(savGet(_,Sv,_),D,Dx) :-
  ptnVars(Sv,D,Dx).
ptnVars(whr(_,Ptn,Cond),D,Dx) :-
  ptnVars(Ptn,D,D0),
  glVars(Cond,D0,Dx).

ptnVarsList(Args,D,Dx) :-
  rfold(Args,lterms:ptnVars,D,Dx).

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

uniQ(QD,Nm,Trm) :-
  makeKey(Nm,Key),
  get_dict(Key,QD,Trm),!.

extendU(lterms:uniQ(Q),ltt,idnt(Nm,Tp),lterms:uniQ(QQ)) :-
  newVar((Nm,Tp),NV),
  decVr(NV,Q,QQ).
extendU(lterms:uniQ(Q),gl,Gl,lterms:uniQ(QQ)) :-
  glVars(Gl,[],GVrs),
  map(GVrs,lterms:newVar,GV),
  extendSubstDict(GV,Q,QQ).
extendU(lterms:uniQ(Q),ptn,Gl,lterms:uniQ(QQ)) :-
  ptnVars(Gl,[],GVrs),
  map(GVrs,lterms:newVar,GV),
  extendSubstDict(GV,Q,QQ).
extendU(lterms:uniQ(Q),seq,A,lterms:uniQ(QQ)) :-
  extractActVrs(A,Vrs),
  map(Vrs,lterms:newVar,GV),
  extendSubstDict(GV,Q,QQ).

extractActVrs(defn(_,P,_),Vrs) :-!,
  ptnVars(P,[],Vrs).
extractActVrs(mtch(_,P,_),Vrs) :- !,
  ptnVars(P,[],Vrs).
extractActVrs(_,[]).

uniqify(fnDef(Lc,Nm,H,Tp,Args,Value),fnDef(Lc,Nm,H,Tp,Args,UValue)) :-
  map(Args,lterms:idVar,Vrs),
  makeSubstDict(Vrs,QD),
  rewriteTerm(lterms:uniQ(QD),lterms:extendU,Value,UValue),!.
uniqify(prDef(Lc,Nm,H,Tp,Args,Act),prDef(Lc,Nm,H,Tp,Args,UAct)) :-
  map(Args,lterms:idVar,Vrs),
  makeSubstDict(Vrs,QD),
  rewriteAction(lterms:uniQ(QD),lterms:extendU,Act,UAct),!.
uniqify(glbDef(Lc,Nm,Tp,Value),glbDef(Lc,Nm,Tp,UValue)) :-
  makeSubstDict([],QD),
  rewriteTerm(lterms:uniQ(QD),lterms:extendU,Value,UValue),!.
uniqify(D,D).
