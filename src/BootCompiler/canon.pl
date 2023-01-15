:- module(canon,[dispFunction/3,dispDef/1,dispCanon/1,dispAction/1,dispCanonProg/1,
		 ssCanonProg/2,ssTerm/3,ssPkg/2,ssContract/3,
		 dispDecls/1,
		 typeOfCanon/2,locOfCanon/2,
		 constructorName/2,constructorType/2,
		 isCanonDef/1,isCanon/1,isSimpleCanon/1,
		 isPkg/1,isGoal/1,isIterableGoal/1,
		 anonVar/3]).

:- use_module(misc).
:- use_module(display).
:- use_module(operators).
:- use_module(types).
:- use_module(location).

isCanonDef(funDef(_,_,_,_,_,_,_)).
isCanonDef(varDef(_,_,_,_,_,_)).
isCanonDef(cnsDef(_,_,_)).
isCanonDef(typeDef(_,_,_,_)).
isCanonDef(conDef(_,_,_)).
isCanonDef(implDef(_,_,_,_)).
isCanonDef(accDec(_,_,_,_)).
isCanonDef(updDec(_,_,_,_)).

isCanon(prog(_,_,_,_,_)).
isCanon(v(_,_,_)).
isCanon(anon(_,_)).
isCanon(deref(_,_)).
isCanon(cell(_,_)).
isCanon(over(_,_,_)).
isCanon(overaccess(_,_,_,_)).
isCanon(mtd(_,_,_)).
isCanon(intLit(_,_)).
isCanon(bigLit(_,_)).
isCanon(floatLit(_,_)).
isCanon(charLit(_,_)).
isCanon(stringLit(_,_)).
isCanon(apply(_,_,_,_,_)).
isCanon(invoke(_,_,_,_)).
isCanon(resume(_,_,_,_)).
isCanon(suspend(_,_,_,_)).
isCanon(dot(_,_,_,_)).
isCanon(update(_,_,_,_)).
isCanon(enm(_,_,_)).
isCanon(cons(_,_,_)).
isCanon(tple(_,_)).
isCanon(where(_,_,_)).
isCanon(conj(_,_,_)).
isCanon(disj(_,_,_)).
isCanon(implies(_,_,_)).
isCanon(cond(_,_,_,_,_)).
isCanon(match(_,_,_)).
isCanon(open(_,_,_)).
isCanon(neg(_,_)).
isCanon(lambda(_,_,_,_)).
isCanon(fiber(_,_,_)).
isCanon(tryCatch(_,_,_,_)).

isSimpleCanon(v(_,_,_)).
isSimpleCanon(anon(_,_)).
isSimpleCanon(intLit(_,_)).
isSimpleCanon(bigLit(_,_)).
isSimpleCanon(floatLit(_,_)).
isSimpleCanon(charLit(_,_)).
isSimpleCanon(stringLit(_,_)).
isSimpleCanon(enm(_,_,_)).

isGoal(match(_,_,_)) :-!.
isGoal(conj(_,_,_)) :- !.
isGoal(implies(_,_,_)) :- !.
isGoal(disj(_,_,_)) :- !.
isGoal(neg(_,_)) :- !.
isGoal(cond(_,_,L,R,_)) :- !, isGoal(L),isGoal(R).

isIterableGoal(conj(_,L,R)) :- !, (isIterableGoal(L) ; isIterableGoal(R)).
isIterableGoal(implies(_,L,R)) :- !, (isIterableGoal(L) ; isIterableGoal(R)).
isIterableGoal(disj(_,L,R)) :- !,  (isIterableGoal(L) ; isIterableGoal(R)).
isIterableGoal(neg(_,R)) :- !, isIterableGoal(R).

isPkg(pkg(_,_)).

typeOfCanon(v(_,_,Tp),Tp) :- !.
typeOfCanon(anon(_,Tp),Tp) :- !.
typeOfCanon(dot(_,_,_,Tp),Tp) :- !.
typeOfCanon(update(_,Rc,_,_),Tp) :- !, typeOfCanon(Rc,Tp).
typeOfCanon(intLit(_,_),type("star.core*integer")) :- !.
typeOfCanon(bigLit(_,_),type("star.core*bigint")) :- !.
typeOfCanon(floatLit(_,_),type("star.core*float")) :- !.
typeOfCanon(charLit(_,_),type("star.core*char")) :- !.
typeOfCanon(stringLit(_,_),type("star.core*string")) :- !.
typeOfCanon(enm(_,_,Tp),Tp) :- !.
typeOfCanon(cons(_,_,Tp),Tp) :- !.
typeOfCanon(where(_,T,_),Tp) :- !, typeOfCanon(T,Tp).
typeOfCanon(open(_,_,Tp),Tp) :-!.
typeOfCanon(match(_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(conj(_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(disj(_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(implies(_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(cond(_,_,_,_,Tp),Tp) :-!.
typeOfCanon(letExp(_,_,_,Bnd),Tp) :- !,typeOfCanon(Bnd,Tp).
typeOfCanon(letRec(_,_,_,Bnd),Tp) :- !,typeOfCanon(Bnd,Tp).
typeOfCanon(apply(_,_,_,Tp,_),Tp) :-!.
typeOfCanon(invoke(_,_,_,Tp),Tp) :-!.
typeOfCanon(resume(_,_,_,Tp),Tp) :-!.
typeOfCanon(suspend(_,_,_,Tp),Tp) :-!.
typeOfCanon(tple(_,Els),tplType(Tps)) :-!,
  map(Els,canon:typeOfCanon,Tps).
typeOfCanon(cell(_,Vl),refType(Tp)) :-
  typeOfCanon(Vl,Tp).
typeOfCanon(deref(_,Vl),Tp) :-
  typeOfCanon(Vl,refType(Tp)).
typeOfCanon(lambda(_,_,_,Tp),Tp) :-!.
typeOfCanon(over(_,T,_),Tp) :- typeOfCanon(T,Tp).
typeOfCanon(overaccess(_,_,_,Tp),Tp) :- !.
typeOfCanon(mtd(_,_,Tp),Tp) :-!.
typeOfCanon(case(_,_,_,Tp),Tp) :- !.
typeOfCanon(raise(_,_,Tp),Tp) :-!.
typeOfCanon(fiber(_,_,Tp),Tp) :-!.
typeOfCanon(valof(_,_,Tp),Tp) :-!.
typeOfCanon(tryCatch(_,E,_T,_),Tp) :- !,
  typeOfCanon(E,Tp).

locOfCanon(v(Lc,_,_),Lc) :- !.
locOfCanon(anon(Lc,_),Lc) :- !.
locOfCanon(dot(Lc,_,_,_),Lc) :- !.
locOfCanon(update(Lc,_,_,_),Lc) :- !.
locOfCanon(intLit(Lc,_),Lc) :- !.
locOfCanon(bigLit(Lc,_),Lc) :- !.
locOfCanon(floatLit(Lc,_),Lc) :- !.
locOfCanon(charLit(Lc,_),Lc) :- !.
locOfCanon(stringLit(Lc,_),Lc) :- !.
locOfCanon(enm(Lc,_,_),Lc) :- !.
locOfCanon(where(Lc,_,_),Lc) :- !.
locOfCanon(open(Lc,_,_),Lc) :-!.
locOfCanon(match(Lc,_,_),Lc) :-!.
locOfCanon(conj(Lc,_,_),Lc) :-!.
locOfCanon(disj(Lc,_,_),Lc) :-!.
locOfCanon(neg(Lc,_),Lc) :-!.
locOfCanon(implies(Lc,_,_),Lc) :-!.
locOfCanon(cond(Lc,_,_,_,_),Lc) :-!.
locOfCanon(letExp(Lc,_,_,_),Lc) :- !.
locOfCanon(letRec(Lc,_,_,_),Lc) :- !.
locOfCanon(case(Lc,_,_,_),Lc) :- !.
locOfCanon(apply(Lc,_,_,_,_),Lc) :-!.
locOfCanon(invoke(Lc,_,_,_),Lc) :-!.
locOfCanon(resume(Lc,_,_,_),Lc) :-!.
locOfCanon(suspend(Lc,_,_,_),Lc) :-!.
locOfCanon(tple(Lc,_),Lc) :-!.
locOfCanon(lambda(Lc,_,_,_),Lc) :-!.
locOfCanon(assign(Lc,_,_),Lc) :-!.
locOfCanon(tryCatch(Lc,_,_,_),Lc) :-!.
locOfCanon(whileDo(Lc,_,_),Lc) :-!.
locOfCanon(forDo(Lc,_,_,_),Lc) :-!.
locOfCanon(valis(Lc,_),Lc) :-!.
locOfCanon(raise(Lc,_,_),Lc) :-!.
locOfCanon(fiber(Lc,_,_),Lc) :-!.
locOfCanon(valof(Lc,_,_),Lc) :-!.

locOfCanon(doNop(Lc),Lc) :-!.
locOfCanon(doSeq(Lc,_,_),Lc) :-!.
locOfCanon(doLbld(Lc,_,_),Lc) :-!.
locOfCanon(doBrk(Lc,_),Lc) :-!.
locOfCanon(doValis(Lc,_),Lc) :-!.
locOfCanon(doThrow(Lc,_,_),Lc) :-!.
locOfCanon(doMatch(Lc,_,_),Lc) :-!.
locOfCanon(doDefn(Lc,_,_),Lc) :-!.
locOfCanon(doAssign(Lc,_,_),Lc) :-!.
locOfCanon(doTryCatch(Lc,_,_,_),Lc) :-!.
locOfCanon(doIfThenElse(Lc,_,_,_),Lc) :-!.
locOfCanon(doWhile(Lc,_,_),Lc) :-!.
locOfCanon(doLet(Lc,_,_,_),Lc) :-!.
locOfCanon(doLetRec(Lc,_,_,_),Lc) :-!.
locOfCanon(doCall(Lc,_,_),Lc) :-!.
locOfCanon(doCase(Lc,_,_,_),Lc) :-!.
locOfCanon(doRetire(Lc,_,_),Lc) :-!.

constructorName(enm(_,Nm,_),Nm) :-!.
constructorName(cons(_,Nm,_),Nm).

constructorType(enm(_,_,Tp),Tp) :-!.
constructorType(cons(_,_,Tp),Tp).

dispCanonProg(P) :-
  displayln(canon:ssCanonProg(P)).  

ssCanonProg(prog(Pkg,Imports,XDecls,_Decls,Defs),sq([PP,nl(2),iv(nl(2),XX),rb])) :-
  ssPkg(Pkg,PP),
  map(Imports,canon:ssImport,II),
  map(XDecls,canon:ssDecl(2,ss("export ")),EE),
%  map(Decls,canon:ssDecl(2,ss("local ")),DD),
  map(Defs,canon:ssDf(2),FF),
  flatten([EE,/*DD,*/II,FF],XX).

ssPkg(pkg(Nm,V),sq([ss(Nm)|Vs])) :-
  ssVersion(V,Vs).

ssVersion(defltVersion,[]).
ssVersion(ver(V),[ss(":"),ss(V)]).

dispCanon(T) :-
  displayln(canon:ssTerm(T,0)).

ssTerm(v(_,Nm,_),_,id(Nm)).
ssTerm(anon(_,_),_,ss("_")).
ssTerm(void,_,ss("void")).
ssTerm(intLit(_,Ix),_,ix(Ix)).
ssTerm(bigLit(_,Bx),_,ss(Bx)).
ssTerm(floatLit(_,Dx),_,fx(Dx)).
ssTerm(charLit(_,Cp),_,sq([ss("`"),cp(Cp),ss("`")])).
ssTerm(stringLit(_,Str),_,sq([ss(""""),ss(Str),ss("""")])).
ssTerm(apply(_,Op,Args,_,none),Dp,sq([O,A])) :-
  ssTerm(Op,Dp,O),
  ssTerm(Args,Dp,A).
ssTerm(apply(_,Op,Args,_,some(ErTp)),Dp,sq([O,A,ss(" raises "),TT])) :-
  ssTerm(Op,Dp,O),
  ssTerm(Args,Dp,A),
  ssType(ErTp,false,Dp,TT).
ssTerm(invoke(_,Op,Args,_),Dp,sq([O,ss("."),A])) :-
  ssTerm(Op,Dp,O),
  ssTerm(Args,Dp,A).
ssTerm(resume(_,F,E,_),Dp,sq([FF,ss(" resume "),EE])) :-
  ssTerm(F,Dp,FF),
  ssTerm(E,Dp,EE).
ssTerm(suspend(_,F,E,_),Dp,sq([FF,ss(" suspend "),EE])) :-
  ssTerm(F,Dp,FF),
  ssTerm(E,Dp,EE).
ssTerm(dot(_,Rc,Fld,_),Dp,sq([R,ss("."),id(Fld)])) :-
  ssTerm(Rc,Dp,R).
ssTerm(update(_,Rc,Fld,Vl),Dp,sq([RR,ss("."),id(Fld),ss("="),VV])) :-
  ssTerm(Rc,Dp,RR),
  ssTerm(Vl,Dp,VV).
ssTerm(enm(_,Nm,_),_,sq([ss("."),id(Nm)])).
ssTerm(cons(_,Nm,_),_,sq([ss("."),id(Nm)])).
ssTerm(open(_,E,_),Dp,sq([ss("open "),EE])) :- ssTerm(E,Dp,EE).
ssTerm(case(_,Bound,Cases,_),Dp,
	    sq([ss("case "),B,ss(" in {"),Rs,ss("}")])) :-
  ssTerm(Bound,Dp,B),
  ssRls("",Cases,Dp,canon:ssTerm,Rs).
ssTerm(cell(_,Vr),Dp,sq([ss("ref "),V])) :-
  ssTerm(Vr,Dp,V).
ssTerm(deref(_,Vr),Dp,sq([V,ss("!")])) :-
  ssTerm(Vr,Dp,V).
ssTerm(letExp(_,_Decls,Defs,Ex),Dp,
	    sq([ss("let {"),nl(Dp2),iv(nl(Dp2),DS),nl(Dp),ss("} in "),B])) :-
  Dp2 is Dp+2,
  map(Defs,canon:ssDf(Dp2),DS),
  ssTerm(Ex,Dp,B).
ssTerm(letRec(_,Decls,Defs,Ex),Dp,
	    sq([ss("let {."),nl(Dp2),iv(nl(Dp2),Ds),nl(Dp),ss(".} in "),B])) :-
  Dp2 is Dp+2,
  map(Decls,canon:ssDecl(Dp2,ss("rec ")),DD),
  map(Defs,canon:ssDf(Dp2),XX),
  flatten([DD,XX],Ds),
  ssTerm(Ex,Dp,B).
ssTerm(lambda(_,Lbl,Rle,_),Dp,sq([lp,Rl,rp])) :-
  ssRule(Lbl,Dp,Rle,Rl).
ssTerm(tple(_,Els),Dp,sq([lp,iv(ss(", "),SEls),rp])) :-
  ssTerms(Els,Dp,SEls).
ssTerm(mtd(_,Nm,_),_,sq([ss("°"),id(Nm)])).
ssTerm(over(_,V,Cons),Dp,sq([iv(ss(","),CCs),ss("|:"),VV])) :-
  map(Cons,types:ssConstraint(false,Dp),CCs),
  ssTerm(V,Dp,VV).
ssTerm(overaccess(_,R,Fld,F),Dp,sq([RR,ss("<~{"),ss(Fld),ss(":"),FF,ss("}")])) :-
  ssTerm(R,Dp,RR),
  ssType(F,false,Dp,FF).
ssTerm(where(_,Ptn,Cond),Dp,sq([PP,GG])) :-
  ssTerm(Ptn,Dp,PP),
  ssGuard(some(Cond),Dp,GG).
ssTerm(conj(_,L,R),Dp,sq([LL,ss(" && "),RR])) :-
  ssTerm(L,Dp,LL),
  ssTerm(R,Dp,RR).
ssTerm(disj(_,L,R),Dp,sq([lp,LL,ss(" || "),RR,rp])) :-
  ssTerm(L,Dp,LL),
  ssTerm(R,Dp,RR).
ssTerm(implies(_,L,R),Dp,sq([lp,LL,ss(" *> "),RR,rp])) :-
  ssTerm(L,Dp,LL),
  ssTerm(R,Dp,RR).
ssTerm(cond(_,Test,Either,Or,_),Dp,sq([lp,TT,ss("??"),LL,ss(" || "),RR,rp])) :-
  ssTerm(Test,Dp,TT),
  ssTerm(Either,Dp,LL),
  ssTerm(Or,Dp,RR).
ssTerm(match(_,P,E),Dp,sq([lp,LL,ss(" .= "),RR,rp])) :-
  ssTerm(P,Dp,LL),
  ssTerm(E,Dp,RR).
ssTerm(neg(_,R),Dp,sq([lp,ss(" ~ "),RR,rp])) :-
  ssTerm(R,Dp,RR).
ssTerm(raise(_,T,A),Dp,sq([TT,ss(" raise "),AA])) :-!,
  typeOfCanon(T,ETp),
  ssType(ETp,true,Dp,TT),
  ssTerm(A,Dp,AA).
ssTerm(valof(_,A,_),Dp,sq([ss("valof "),AA])) :-!,
  ssAction(A,Dp,AA).
ssTerm(fiber(_,A,_),Dp,sq([ss("fiber "),AA])) :-!,
  ssTerm(A,Dp,AA).
ssTerm(tryCatch(_,A,T,Hs),Dp,sq([ss("try "),TT,ss(" in "),AA,ss(" catch "),TT,lb,HH,nl(Dp),rb])) :-!,
  Dp2 is Dp+2,
  ssTerm(T,Dp,TT),
  ssTerm(A,Dp2,AA),
  ssRls("",Hs,Dp2,canon:ssTerm,HH).

ssTerms([],_,[]).
ssTerms([T|More],Dp,[TT|TTs]) :-
  ssTerm(T,Dp,TT),
  ssTerms(More,Dp,TTs).

dispAction(A) :-
  displayln(canon:ssAction(A,0)).

ssAction(doNop(_),_,ss("{}")) :-!.
ssAction(doSeq(Lc,L,R),Dp,sq([ss("{"),nl(Dp2),Sq,nl(Dp),ss("}")])) :-!,
  Dp2 is Dp+2,
  ssActSeq(doSeq(Lc,L,R),Dp2,Sq).
ssAction(doLbld(_,Lb,A),Dp,sq([ss(Lb),ss(":"),AA])) :-!,
  ssAction(A,Dp,AA).
ssAction(doBrk(_,Lb),_,sq([ss("break "),ss(Lb)])) :-!.
ssAction(doValis(_,E),Dp,sq([ss("valis "),EE])) :-!,
  ssTerm(E,Dp,EE).
ssAction(doRaise(_,T,E),Dp,sq([TT,ss(" raise "),EE])) :-!,
  typeOfCanon(T,ETp),
  ssType(ETp,true,Dp,TT),
  ssTerm(E,Dp,EE).
ssAction(doCall(_,E,_),Dp,sq([ss("call "),EE])) :-!,
  ssTerm(E,Dp,EE).
ssAction(doMatch(_,P,E),Dp,sq([PP,ss(" .= "),EE])) :-!,
  ssTerm(P,Dp,PP),
  ssTerm(E,Dp,EE).
ssAction(doDefn(_,P,E),Dp,sq([PP,ss(" = "),EE])) :-!,
  ssTerm(P,Dp,PP),
  ssTerm(E,Dp,EE).
ssAction(doAssign(_,P,E),Dp,sq([PP,ss(" := "),EE])) :-!,
  ssTerm(P,Dp,PP),
  ssTerm(E,Dp,EE).
ssAction(doTryCatch(_,A,T,Hs),Dp,sq([ss("try "),TT,ss(" in "),AA,ss(" catch "),lb,HH,nl(Dp),rb])) :-!,
  Dp2 is Dp+2,
  ssTerm(T,Dp,TT),
  ssAction(A,Dp2,AA),
  ssRls("",Hs,Dp2,canon:ssAction,HH).
ssAction(doIfThenElse(_,T,A,doNop(_)),Dp,sq([ss("if "),TT,ss(" then "),nl(Dp2),AA])) :-!,
  Dp2 is Dp+2,
  ssTerm(T,Dp,TT),
  ssAction(A,Dp2,AA).
ssAction(doIfThenElse(_,T,A,B),Dp,sq([ss("if "),TT,ss(" then "),nl(Dp2),AA,nl(Dp),ss("else "),BB])) :-!,
  Dp2 is Dp+2,
  ssTerm(T,Dp,TT),
  ssAction(A,Dp2,AA),
  ssAction(B,Dp2,BB).
ssAction(doWhile(_,T,B),Dp,sq([ss("while "),TT,ss(" do"),nl(Dp2),BB])) :-!,
  Dp2 is Dp+2,
  ssTerm(T,Dp,TT),
  ssAction(B,Dp2,BB).
ssAction(doLet(_,_,D,A),Dp,sq([ss("let{"),nl(Dp2),iv(nl(Dp2),DS),nl(Dp),ss("} in "),AA])) :-!,
  Dp2 is Dp+2,
  map(D,canon:ssDf(Dp2),DS),
  ssAction(A,Dp2,AA).
ssAction(doLetRec(_,_,D,A),Dp,sq([ss("let{."),nl(Dp2),iv(nl(Dp2),DS),nl(Dp),ss(".} in "),AA])) :-!,
  Dp2 is Dp+2,
  map(D,canon:ssDf(Dp2),DS),
  ssAction(A,Dp2,AA).
ssAction(doCase(_,Bound,Cases,_),Dp,
	 sq([ss("case "),B,ss(" in {"),Rs,ss("}")])) :-
  ssTerm(Bound,Dp,B),
  ssRls("",Cases,Dp,canon:ssAction,Rs).
ssAction(doRetire(_,T,E),Dp,sq([TT,ss(" retire "),EE])) :-
  ssTerm(T,Dp,TT),
  ssTerm(E,Dp,EE).
				    
ssAction(A,Dp,AA) :- ssTerm(A,Dp,AA).

ssActSeq(doSeq(_,L,R),Dp,sq([LL,ss(";"),nl(Dp),RR])) :-!,
  ssAction(L,Dp,LL),
  ssActSeq(R,Dp,RR).
ssActSeq(A,Dp,S) :-
  ssAction(A,Dp,S).

ssConstraints([],_,[]).
ssConstraints([T|More],Dp,[TT|TTs]) :-
  ssConstraint(false,Dp,T,TT),
  ssConstraints(More,Dp,TTs).

ssRule(Nm,Dp,rule(_,Args,Guard,Value),sq([id(Nm),A,G,ss(" => "),V])) :-
  ssTerm(Args,Dp,A),
  ssGuard(Guard,Dp,G),
  ssTerm(Value,Dp,V).

ssGuard(none,_,sq([])) :- !.
ssGuard(some(C),Dp,sq([ss(" where "),CC])) :-
  ssTerm(C,Dp,CC).

ssDefs(L,Dp,iv(nl(Dp),LL)) :-
  map(L,canon:ssDf(Dp),LL).

ssDf(Dp,Df,XX) :-
  ssDef(Dp,Df,XX),
  (validSS(XX) ; errors:reportMsg("%s not valid display",[XX]),abort).

ssDef(Dp,funDef(Lc,Nm,ExtNm,Sft,_Type,_Cx,Eqns),
      sq([ss(SS),ss("fun "),id(Nm),ss("@"),Lcs,nl(Dp),Rs])) :-
  ssRls(ExtNm,Eqns,Dp,canon:ssTerm,Rs),
  ssLoc(Lc,Lcs),
  (Sft=soft -> SS="soft " ; SS="").
ssDef(Dp,varDef(_Lc,Nm,_ExtNm,_Cx,_Tp,Value),
      sq([ss("var "),id(Nm),ss(" = "),V])) :-
  ssTerm(Value,Dp,V).
ssDef(Dp,cnsDef(Lc,Nm,C),
      sq([ss("con "),id(Nm),ss(" : "),Ts,ss("@"),Lcs])) :-
  typeOfCanon(C,Tp),
  ssType(Tp,true,Dp,Ts),
  ssLoc(Lc,Lcs).
ssDef(Dp,typeDef(Lc,Nm,_Tp,Rl),
      sq([ss("type "),id(Nm),ss(":"),Ts,ss("@"),Lcs])) :-
  ssLoc(Lc,Lcs),
  ssType(Rl,true,Dp,Ts).
ssDef(Dp,conDef(Nm,_ConNm,Rl),
      sq([ss("contract "),id(Nm),ss(" : "),Ts])) :-
  ssType(Rl,true,Dp,Ts).
ssDef(Dp,implDef(Nm,_ConNm,ImplNm,ImplTp),
      sq([ss("impl "),id(Nm),ss(" = "),id(ImplNm),ss(":"),Ts])) :-
  ssType(ImplTp,true,Dp,Ts).
ssDef(Dp,accDec(Tp,FldNm,FunNm,_),
      sq([ss("acc "),Ts,ss("."),id(FldNm),ss(" = "),id(FunNm)])) :-
  ssType(Tp,false,Dp,Ts).
ssDef(Dp,updDec(Tp,FldNm,FunNm,_),
      sq([ss("upd "),Ts,ss("."),id(FldNm),ss(" = "),id(FunNm)])) :-
  ssType(Tp,false,Dp,Ts).

ssFunction(Dp,Nm,Type,Eqns,
	   sq([ss("fun "),id(Nm),ss(" : "),Ts,nl(Dp),Rs])) :-
  ssType(Type,true,Dp,Ts),
  ssRls(Nm,Eqns,Dp,canon:ssTerm,Rs).

ssRls(Nm,Eqns,Dp,Dsp,iv(nl(Dp),EE)) :-
  map(Eqns,canon:ssEqn(Nm,Dp,Dsp),EE).

ssEqn("",Dp,Dsp,rule(_,Args,Guard,Value),
      sq([canon:ssTerm(Args,Dp),canon:ssGuard(Guard,Dp),ss(" => "),
	  VV])) :-
  call(Dsp,Value,Dp,VV).
ssEqn(Nm,Dp,Dsp,rule(_,Args,Guard,Value),
      sq([id(Nm),
	  canon:ssTerm(Args,Dp),canon:ssGuard(Guard,Dp),ss(" => "),
	  VV])) :-
  call(Dsp,Value,Dp,VV).

dispDecls(Decls) :-
  map(Decls,canon:ssDecl(0,ss("")),DD),
  displayln(iv(nl(0),DD)).

ssDecl(Dp,X,funDec(Nm,Nm,Type),sq([X,ss("fun "),id(Nm),ss(" :: "),TT])) :-!,
  ssType(Type,true,Dp,TT).
ssDecl(Dp,X,funDec(Nm,LclNme,Type),sq([X,ss("fun "),id(Nm),ss("~"),id(LclNme),ss(" :: "),TT])) :-
  ssType(Type,true,Dp,TT).
ssDecl(Dp,X,varDec(Nm,LclNme,Tp),
      sq([X,ss("var "),id(Nm),ss("~"),id(LclNme),ss(" :: "),Ts])) :-
  ssType(Tp,true,Dp,Ts).
ssDecl(Dp,X,cnsDec(Nm,LclNme,Tp),
      sq([X,ss("cons "),id(Nm),ss("~"),id(LclNme),ss(" :: "),Ts])) :-
  ssType(Tp,true,Dp,Ts).
ssDecl(Dp,X,typeDec(Nm,Tp,_Rl),
       sq([X,ss("type "),id(Nm),ss("::"),Ts])) :-
  ssType(Tp,true,Dp,Ts).
ssDecl(Dp,X,contractDec(Nm,_ConNm,Rl),
       sq([X,ss("contract "),id(Nm),ss(" :: "),Ts])) :-
  ssType(Rl,true,Dp,Ts).
ssDecl(Dp,X,impDec(Nm,ImplNm,ImplTp),
       sq([X,ss("impl "),id(Nm),ss(" ~ "),id(ImplNm),ss("::"),Ts])) :-
  ssType(ImplTp,true,Dp,Ts).
ssDecl(Dp,X,accDec(Tp,FldNm,FunNm,FunTp),
       sq([X,ss("acc "),Ts,ss("."),id(FldNm),
	   ss(" using "),id(FunNm),ss(" :: "),TT])) :-
  ssType(Tp,false,Dp,Ts),
  ssType(FunTp,false,Dp,TT).
ssDecl(Dp,X,updDec(Tp,FldNm,FunNm,FunTp),
       sq([X,ss("upd "),Ts,ss("."),id(FldNm),
	   ss(" using "),id(FunNm),ss(" :: "),TT])) :-
  ssType(Tp,false,Dp,Ts),
  ssType(FunTp,false,Dp,TT).

ssImpl(Dp,imp(ImplName,FullName,Spec),
       sq([ss("implementation "),
	   id(ImplName),
	   ss("="),
	   id(FullName),
	   ss(":"),
	   SS])) :-
  ssType(Spec,true,Dp,SS).
ssImpl(Dp,acc(Tp,Fld,Fn,AccTp),
       sq([ss("access "),
	   SS,
	   ss("."),
	   id(Fld),
	   ss(" with "),
	   id(Fn),
	   ss(":"),
	   TT])) :-
  ssType(Tp,true,Dp,SS),
  ssType(AccTp,true,Dp,TT).

ssContract(Dp,conDef(Nm,ConNm,ConRule),
	   sq([ss("contract "),
	       id(Nm),
	       ss("«"),
	       id(ConNm),
	       ss("» "),
	       TT])):-
  ssType(ConRule,false,Dp,TT).
	    
ssImport(importPk(_,Viz,Pkg),
	 sq([VV,ss(" import "),PP])) :-
  ssVisibility(Viz,VV),
  ssPkg(Pkg,PP).

ssVisibility(private,ss("private ")).
ssVisibility(public,ss("public ")).
ssVisibility(transitive,ss("transitive ")).

dispFunction(Nm,Type,Eqns) :-
  displayln(canon:ssFunction(0,Nm,Type,Eqns)).

dispDef(Def) :-
  displayln(canon:ssDef(0,Def)).

anonVar(Lc,anon(Lc,Tp),Tp) :-
  newTypeVar("_",Tp).

