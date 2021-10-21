:- module(inline,[inlineProg/4]).

:- use_module(canon).
:- use_module(misc).
:- use_module(types).
:- use_module(errors).
:- use_module(freevars).

inlineProg(Prg,_Env,_Max,Prg).

inlineExp(v(_,Nm,_),Env,Max,Rpl,_,Trg) :-
  varInEnv(Nm,Env,Rep),!,
  inlineEx(Rep,Env,Max,Rpl,triggered,Trg).
inlineExp(v(Lc,Nm,Tp),Env,Max,v(Lc,Nm,Tp),T,T) :- !.
inlineExp(anon(Lc,Tp),_,_,anon(Lc,Tp),T,T) :-!.
inlineExp(void,_Env,_Max,void,Trg,Trg) :-!.
inlineExp(intLit(Lc,Ix),_Env,_Max,intLit(Lc,Ix),Trg,Trg) :-!.
inlineExp(floatLit(Lc,Dx),_Env,_Max,floatLit(Lc,Dx),Trg,Trg) :-!.
inlineExp(charLit(Lc,Sx),_Env,_Max,charLit(Lc,Sx),Trg,Trg) :-!.
inlineExp(stringLit(Lc,Sx),_Env,_Max,stringLit(Lc,Sx),Trg,Trg) :-!.
inlineExp(enm(Lc,Nm,Tp),_Env,_Max,enm(Lc,Nm,Tp),Trg,Trg) :-!.
inlineExp(cons(Lc,Nm,Tp),_Env,_Max,cons(Lc,Nm,Tp),Trg,Trg) :-!.
inlineExp(apply(Lc,Op,Args,Tp),Env,Max,Rep,Trg,Tx) :-!,
  inlineExp(Op,Env,Max,ROp,Trg,T0),
  inlineExp(Args,Env,Max,RArgs,T0,T1),
  applyOp(Lc,ROp,RArgs,Env,Max,Rep0,T1,Tx).
inlineExp(dot(Lc,Rc,Fld,Tp),Env,Max,Rep,Trg,Tx) :-!,
  inlineExp(Rc,Env,Max,RRc,Trg,T0),
  applyDot(Lc,RRc,Fld,Tp,Env,Rep,T0,Tx).
inlineExp(case(Lc,Bound,Cases,Tp),Env,Max,Rep,Trg,Tx) :- !,
  inlineExp(Bound,Env,Max,RBnd,Trg,T0),
  applyCases(Lc,RRBnd,Cases,Tp,Env,Max,Rep,T0,Tx).
inlineExp(tple(Lc,Els),Env,Max,tple(Lc,REls),T0,Tx) :- !,
  inlineExps(Els,Env,Max,REls,T0,Tx).
inlineExp(cell(Lc,Vl),Env,Max,cell(Lc,RVl),Trg,Tx) :-!,
  inlineExp(Vl,Env,Max,RVl,Trg,Tx).
inlineExp(deref(Lc,Vl),Env,Max,deref(Lc,RVl),Trg,Tx) :-!,
  inlineExp(Vl,Env,Max,RVl,Trg,Tx).
inlineExp(mtd(Lc,Nm,Tp),Env,Max,mtd(Lc,Nm,Tp),T,T) :-!,
  reportWarning("unresolved method %s",[Nm],Lc).
inlineExp(over(Lc,Nm,Cx,Tp),Env,Max,over(Lc,Nm,Cx,Tp),T,T) :-!,
  reportWarning("unresolved expression %s",[Nm],Lc).
inlineExp(where(Lc,T,C),Env,Max,Rep,Trg,Tx) :- !,
  inlineExp(T,Env,Max,RT,Trg,T0),
  inlineGuard(C,Env,Max,RC,T0,T1),
  applyWhere(Lc,RT,RC,Env,Max,Rep,T1,Tx).
inlineExp(conj(Lc,L,R),Env,Max,Rep,Trg,Tx) :-!,
  inlineExp(L,Env,Max,RL,Trg,T0),
  inlineExp(R,Env,Max,RR,T0,T1),
  applyConj(Lc,RL,RR,Env,Max,Reg,T1,Tx).
inlineExp(disj(Lc,L,R),Env,Max,Rep,Trg,Tx) :-!,
  inlineExp(L,Env,Max,RL,Trg,T0),
  inlineExp(R,Env,Max,RR,T0,T1),
  applyDisj(Lc,RL,RR,Env,Max,Reg,T1,Tx).
inlineExp(implies(Lc,L,R),Env,Max,Rep,Trg,Tx) :-!,
  inlineExp(L,Env,Max,RL,Trg,T0),
  inlineExp(R,Env,Max,RR,T0,T1),
  applyImplies(Lc,RL,RR,Env,Max,Reg,T1,Tx).
inlineExp(neg(Lc,R),Env,Max,Rep,Trg,Tx) :-!,
  inlineExp(R,Env,Max,RR,Trg,T1),
  applyNeg(Lc,RR,Env,Max,Reg,T1,Tx).
inlineExp(cond(Lc,T,L,R),Env,Max,Rep,Trg,Tx) :-!,
  inlineExp(T,Env,Max,RT,Trg,T0),
  inlineExp(L,Env,Max,RL,T0,T1),
  inlineExp(R,Env,Max,RR,T1,T2),
  applyCons(Lc,RT,RL,RR,Env,Max,Reg,T2,Tx).
inlineExp(match(Lc,L,R),Env,Max,Rep,Trg,Tx) :-!,
  inlinePtn(L,Env,Max,RL,Trg,T0),
  inlineExp(R,Env,Max,RR,T0,T1),
  applyMatch(Lc,RL,RR,Env,Max,Reg,T1,Tx).
inlineExp(search(Lc,L,R),Env,Max,Rep,Trg,Tx) :-!,
  inlinePtn(L,Env,Max,RL,Trg,T0),
  inlineExp(R,Env,Max,RR,T0,T1),
  applySearch(Lc,RL,RR,Env,Max,Reg,T1,Tx).
inlineExp(lambda(Lc,Lbl,Rle,Tp),Env,Max,lambda(Lc,Lbl,RRle,Tp),Trg,Tx) :- !,
  inlineRule(Rle,Env,Max,RRle,Trg,Tx).
inlineExp(theta(Lc,Path,Defs,Tp),Env,Max,theta(Lc,Path,RDefs,Tp),Trg,Tx) :-
  inlineDefs(Defs,Env,Max,RDefs,Trg,Tx).
inlineExp(record(Lc,Path,Defs,Tp),Env,Max,record(Lc,Path,RDefs,Tp),Trg,Tx) :-
  inlineDefs(Defs,Env,Max,RDefs,Trg,Tx).
inlineExp(letExp(Lc,Defs,Bound),Env,Max,letExp(Lc,RDefs,RBound),Trg,Tx) :-
  inlineExp(Defs,Env,Max,RDefs,Trg,T0),
  extendEnv(RDefs,Env,REnv),
  inlineExp(Bound,REnv,Max,RBound,T0,Tx).
inlineExp(T,_,_,T,Trg,Trg) :-
  locOf(T,Lc),
  reportError("cannot inline handle %s",[T],Lc).

inlineEx(Rep,Env,Max,Rpl,triggered,Trg) :-
  Max > 0,!,
  D1 is Max-1,
  inlineExp(Rep,Env,D1,Rpl,triggered,Trg).
inlineEx(Rep,_,_,Rep,Trg,Trg).

inlineExps([],_,_,[],T,T) :-!.
inlineExps([E|Es],Env,Max,[RE|REs],T,Tx) :-
  inlineExp(R,Env,Max,RE,T,T0),
  inlineExps(Es,Env,Max,REs,T0,Tx).

matchTerm(v(_,Nm,_),Oth,Env,REnv) :-
  varInEnv(Nm,Env,Bnd),!,
  matchTerm(Bnd,Oth,Env,REnv).
matchTerm(Oth,v(_,Nm,_),Env,REnv) :-
  varInEnv(Nm,Env,Bnd),!,
  matchTerm(Oth,Bnd,Env,REnv).
matchTerm(v(Lc,Nm,Tp),Oth,Env,REnv) :- !,
  addToEnv(Nm,Oth,Env,REnv).
matchTerm(Oth,v(Lc,Nm,Tp),Env,REnv) :- !,
  addToEnv(Nm,Oth,Env,REnv).
matchTerm(void,void,Env,Env) :- !.
matchTerm(intLit(Ix,Tp),intLit(Ix,Tp),Env,Env) :-!.
matchTerm(floatLit(Lc,Dx),floatLit(Lc,Dx),Env,Env) :-!.
matchTerm(charLit(Lc,Sx),charLit(Lc,Sx),Env,Env) :-!.
matchTerm(stringLit(Lc,Sx),stringLit(Lc,Sx),Env,Env) :-!.
matchTerm(enm(_,Nm,Tp),enm(_,Nm,Tp),Env,Env) :-!.
matchTerm(cons(_,Nm,Tp),cons(_,Nm,Tp),Env,Env) :-!.
matchTerm(apply(_,LOp,LArgs,_),apply(_,ROp,RArgs,_),Env,REnv) :-!,
  matchTerm(LOp,ROp,Env,E0),
  matchTerm(LArgs,RArgs,E0,REnv).
matchTerm(tple(_,Ls),tple(_,Rs),Env,REnv) :-!,
  matchTerms(Ls,Rs,Env,REnv).
matchTerm(where(_,L,C),R,Env,REnv) :-!,
  matchTerm(L,R,Env,E1),
  sameCond(C,enm(_,"true",_),E1,REnv).
matchTerm(L,where(_,R,C),Env,REnv) :-!,
  matchTerm(L,R,Env,E0),
  sameCond(C,enm(_,"true",_),E0,REnv).

matchTerms([],[],Env,Env) :- !.
matchTerms([L|Ls],[R|Rs],Env,REnv) :-
  matchTerm(L,R,Env,E0),
  matchTerms(Ls,Rs,E0,REnv).

rewriteTerm(void,_,void).
rewriteTerm(v(_,Nm,_),Env,Trm) :-
  varInEnv(Nm,Env,Vl),
  rewriteTerm(Vl,Env,Trm).
rewriteTerm(v(Lc,Nm,Tp),_,v(Lc,Nm,Tp)).
rewriteTerm(intLit(Lc,Ix),_,intLit(Lc,Ix)).
rewriteTerm(floatLit(Lc,Dx),_,floatLit(Lc,Dx)).
rewriteTerm(charLit(Lc,Sx),_,charLit(Lc,Sx)).
rewriteTerm(stringLit(Lc,Sx),_,stringLit(Lc,Sx)).
rewriteTerm(dot(Lc,Rc,Fld,Tp),Env,dot(Lc,RRc,Fld,Tp)) :-
  rewriteTerm(Rc,Env,RRc).
rewriteTerm(enm(Lc,Rf,Tp),_,enm(Lc,Rf,Tp)).
rewriteTerm(cons(Lc,Rf,Tp),_,cons(Lc,Rf,Tp)).
rewriteTerm(tple(Lc,Args),Env,tple(Lc,RArgs)) :-
  rewriteTerms(Args,Env,RArgs).
rewriteTerm(theta(Lc,Path,Defs,Sig),Env,theta(Lc,Path,RDefs,Sig)) :-
  rewriteDefs(Defs,Env,RDefs).
rewriteTerm(record(Lc,Path,Anon,Defs,Sig),Env,record(Lc,Path,Anon,RDefs,Sig)) :-
  rewriteDefs(Defs,Env,RDefs).
rewriteTerm(cell(Lc,Inn),Env,cell(Lc,Inn1)) :-
  rewriteTerm(Inn,Env,Inn1).
rewriteTerm(deref(Lc,Inn),Env,deref(Lc,Inn1)) :-
  rewriteTerm(Inn,Env,Inn1).
rewriteTerm(letExp(Lc,Env,Bound),Env,letExp(Lc,REnv,RBound)) :-
  rewriteTerm(Env,Env,REnv),
  rewriteTerm(Bound,Env,RBound).
rewriteTerm(prompt(Lc,Trm,Tp),Env,prompt(Lc,RTrm,Tp)) :-
  rewriteTerm(Trm,Env,RTrm).
rewriteTerm(shift(Lc,V,F),Env,shift(Lc,V,RF)) :-
  rewriteTerm(F,Env,RF).
rewriteTerm(where(Lc,Trm,Cond),Env,where(Lc,RTrm,RCond)) :-
  rewriteTerm(Trm,Env,RTrm),
  rewriteGuard(Cond,Env,RCond).
rewriteTerm(conj(Lc,L,R),Env,conj(Lc,RL,RR)) :-
  rewriteTerm(L,Env,RL),
  rewriteTerm(R,Env,RR).
rewriteTerm(disj(Lc,L,R),Env,disj(Lc,RL,RR)) :-
  rewriteTerm(L,Env,RL),
  rewriteTerm(R,Env,RR).
rewriteTerm(cond(Lc,T,L,R,Tp),Env,cond(Lc,RT,RL,RR,Tp)) :-
  rewriteTerm(T,Env,RT),
  rewriteTerm(L,Env,St0,St1,RL),
  rewriteTerm(R,Env,St1,Stx,RR).
rewriteTerm(implies(Lc,G,T),Env,implies(Lc,RG,RT)) :-
  rewriteTerm(G,Env,RG),
  rewriteTerm(T,Env,RT).
rewriteTerm(neg(Lc,T),Env,neg(Lc,RT)) :-
  rewriteTerm(T,Env,RT).
rewriteTerm(match(Lc,L,R),Env,match(Lc,RL,RR)) :-
  rewriteTerm(L,Env,RL),
  rewriteTerm(R,Env,RR).
rewriteTerm(search(Lc,P,S,I),Env,search(Lc,RP,RS,RI)) :-
  rewriteTerm(P,Env,RP),
  rewriteTerm(S,Env,RS),
  rewriteTerm(I,Env,RI).
rewriteTerm(case(Lc,B,C,Tp),Env,case(Lc,RB,RC,Tp)) :-
  rewriteTerm(B,Env,RB),
  rewriteCases(C,Env,RC).
rewriteTerm(abstraction(Lc,B,C,Zed,Gen,Tp),
	     Env,abstraction(Lc,RB,RC,RZed,RGen,Tp)) :-
  rewriteTerm(B,Env,RB),
  rewriteTerm(C,Env,RC),
  rewriteTerm(Zed,Env,RZed),
  rewriteTerm(Gen,Env,RGen).
rewriteTerm(apply(Lc,Op,Args,Tp),Env,apply(Lc,ROp,RArgs,Tp)) :-
  rewriteTerm(Op,Env,ROp),
  rewriteTerm(Args,Env,RArgs).
rewriteTerm(lambda(Lc,Lbl,Eqn,Tp),Env,lambda(Lc,Lbl,OEqn,Tp)) :-
  rewriteRule(Eqn,Env,OEqn).
rewriteTerm(doTerm(Lc,Body,Tp),Env,doTerm(Lc,RBody,Tp)) :-
  rewriteAction(Body,Env,RBody).
rewriteTerm(taskTerm(Lc,Lbl,Body,Tp),Env,taskTerm(Lc,Lbl,RBody,Tp)) :-
  rewriteAction(Body,Env,RBody).

rewriteTerms([],Env,[]) :- !.
rewriteTerms([L|Ls],Env,[R|Rs]) :-
  rewriteTerm(L,Env,R),
  rewriteTerms(Ls,Env,Rs).

rewriteDefs([],_Env,[]).
rewriteDefs([D|Ds],Env,[RD|RDs]) :-
  rewriteDef(D,Env,RD),
  rewriteDefs(Ds,Env,RDs).

rewriteDef(funDef(Lc,Nm,FullNm,H,Tp,Cx,Eqns),Env,
	   funDef(Lc,Nm,FullNm,H,Tp,Cx,REqns)) :-
  rewriteRules(Eqns,Env,REqns).
rewriteDef(varDef(Lc,Nm,FullNm,Cx,Tp,Val),Env,
	   varDef(Lc,Nm,FullNm,Tp,Cx,RVal)) :-
  rewriteTerm(Val,Env,RVal).
rewriteDef(cnsDef(Lc,Nm,V),_,cnsDef(Lc,Nm,V)).
rewriteDef(typeDef(Lc,Nm,Tp,Rl),_,typeDef(Lc,Nm,Tp,Rl)).
rewriteDef(conDef(Lc,Nm,Tp),_,conDef(Lc,Nm,Tp)).
rewriteDef(implDef(Nm,Con,Impl,Spec),_,implDef(Nm,Con,Impl,Spec)).

applyOp(Lc,v(VLc,Nm,Tp),Args,Env,Max,Rep0,_,Tx) :-
  defInEnv(Nm,Env,Eqns),
  findCandidate(Args,Env,Eqns,(_,ECond,Eval)),
  inlineEx(where(Lc,Eval,ECond),Env,Max,triggered,Tx).
applyOp(Lc,V,Args,Env,Max,Rep,T1,Tx) :-
  inlineEx(apply(Lc,V,Args),Env,Max,Rep,T1,Tx).

findCandidate(Args,Env,Eqns,Cand) :-
  is_member(Eqn,Eqns),
  isCandidate(Args,Env,Eqn,Cand),!.

isCandidate(Args,Env,Eqn,(E1,SCond,SVal)) :-
  exclusions(Env,Excl),
  freeVars(lambda(_,_,Eqn,_),[],Excl,[],FV),
  genVars(FV,Q),
  pushBindings(Q,Env,E0),
  rewriteTerm(lamdba(_,Eqn,G,_),E0,
	      lamdba(_,rule(Lc,EArgs,EG,ECond,EVal),_)),
  matchTerm(EArgs,Args,E0,E1),
  rewriteGuard(ECond,E1,SCond),
  rewriteTerm(EVal,E1,SVal).

varInEnv(Nm,inlines(Vars,_),Val) :-
  makeKey(Nm,Key),
  get_dict(Key,Vars,Val),!.

defInEnv(Nm,inlines(_,Defs),Eqns) :-
  makeKey(Nm,Key),
  get_dict(Key,Defs,funDef(_,_,_,_,_,Eqns)).

extendEnv([],Env,Env).
extendEnv([D|Ds],Env,Ev) :-
  extendDef(D,Env,E0),
  extendEnv(Ds,E0,Ev).

extendDef(varDef(_,Nm,_,[],_,Val),Env,Ev) :-
  addVarToEnv(Nm,Val,Env,Ev).
extendDef(funDef(Lc,Nm,FullNm,H,Tp,[],Eqns),inlines(Vars,Defs),inlines(Vars,EDefs)) :-
  make_key(Nm,Key),
  put_dict(Key,Defs,funDef(Lc,Nm,FullNm,H,Tp,[],Eqns),EDefs).
extendDef(_,Env,Env).

addVarToEnv(Nm,Vr,inlines(Vars,Defs),inlines(EVars,Defs)) :-
  make_key(Nm,Key),
  put_dict(Key,Vars,Vr,EVars).