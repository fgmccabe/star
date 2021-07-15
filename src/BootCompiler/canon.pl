:- module(canon,[dispFunction/3,
		 ssCanonProg/2,ssTerm/3,ssAction/3,ssPkg/2,ssContract/3,
		 dispDecls/1,
		 typeOfCanon/2,splitPtn/3,locOfCanon/2,
		 constructorName/2,constructorType/2,
		 isCanonDef/1,isCanon/1,isSimpleCanon/1,isAssertion/1,isShow/1,
		 isPkg/1,isGoal/1,isIterableGoal/1,isAction/1,
		 anonVar/3]).

:- use_module(misc).
:- use_module(display).
:- use_module(operators).
:- use_module(types).
:- use_module(location).

isCanonDef(funDef(_,_,_,_,_,_)).
isCanonDef(varDef(_,_,_,_,_,_)).
isCanonDef(cnsDef(_,_,_)).
isCanonDef(typeDef(_,_,_,_)).
isCanonDef(conDef(_,_,_,_)).
isCanonDef(implDef(_,_,_,_)).
isCanonDef(accDef(_,_,_,_)).

isCanon(prog(_,_,_,_,_)).
isCanon(v(_,_,_)).
isCanon(over(_,_,_,_)).
isCanon(overaccess(_,_,_)).
isCanon(mtd(_,_,_)).
isCanon(intLit(_,_)).
isCanon(floatLit(_,_)).
isCanon(stringLit(_,_)).
isCanon(apply(_,_,_,_)).
isCanon(dot(_,_,_,_)).
isCanon(enm(_,_,_)).
isCanon(cons(_,_,_)).
isCanon(tple(_,_)).
isCanon(where(_,_,_)).
isCanon(conj(_,_,_)).
isCanon(disj(_,_,_)).
isCanon(implies(_,_,_)).
isCanon(cond(_,_,_,_,_)).
isCanon(match(_,_,_)).
isCanon(neg(_,_)).
isCanon(assign(_,_,_)).
isCanon(cell(_,_)).
isCanon(lambda(_,_,_,_)).
isCanon(valof(_,_)).
isCanon(doTerm(_,_,_)).
isCanon(taskTerm(_,_,_)).
isCanon(noDo(_)).
isCanon(seqDo(_,_,_)).
isCanon(ifThenDo(_,_,_,_)).
isCanon(whileDo(_,_,_)).
isCanon(untilDo(_,_,_)).
isCanon(forDo(_,_,_)).
isCanon(tryCatchDo(_,_,_)).
isCanon(caseDo(_,_,_,_,_)).
isCanon(varDo(_,_,_)).
isCanon(valisDo(_,_)).
isCanon(throwDo(_,_)).
isCanon(performDo(_,_)).
isCanon(simpleDo(_,_)).
isCanon(assertDo(_,_)).
isCanon(search(_,_,_,_)).

isSimpleCanon(v(_,_,_)).
isSimpleCanon(intLit(_,_)).
isSimpleCanon(floatLit(_,_)).
isSimpleCanon(stringLit(_,_)).
isSimpleCanon(enm(_,_,_)).

isAssertion(assertion(_,_)).
isShow(show(_,_)).

isGoal(match(_,_,_)) :-!.
isGoal(conj(_,_,_)) :- !.
isGoal(implies(_,_,_)) :- !.
isGoal(disj(_,_,_)) :- !.
isGoal(neg(_,_)) :- !.
isGoal(search(_,_,_,_)) :- !.
isGoal(cond(_,_,L,R,_)) :- !, isGoal(L),isGoal(R).

isAction(seqDo(_,_,_)).
isAction(ifThenDo(_,_,_,_)).
isAction(whileDo(_,_,_)).
isAction(forDo(_,_,_,_)).
isAction(caseDo(_,_,_)).
isAction(tryCatchDo(_,_,_)).
isAction(assign(_,_,_)).
isAction(apply(_,_,_,_)).
isAction(varDo(_,_,_)).
isAction(valisDo(_,_)).
isAction(throwDo(_,_)).
isAction(performDo(_,_)).
isAction(noDo(_)).

isIterableGoal(conj(_,L,R)) :- !, (isIterableGoal(L) ; isIterableGoal(R)).
isIterableGoal(implies(_,L,R)) :- !, (isIterableGoal(L) ; isIterableGoal(R)).
isIterableGoal(disj(_,L,R)) :- !,  (isIterableGoal(L) ; isIterableGoal(R)).
isIterableGoal(neg(_,R)) :- !, isIterableGoal(R).
isIterableGoal(search(_,_,_,_)) :- !.

isPkg(pkg(_,_)).

typeOfCanon(v(_,_,Tp),Tp) :- !.
typeOfCanon(dot(_,_,_,Tp),Tp) :- !.
typeOfCanon(intLit(_,Tp),Tp) :- !.
typeOfCanon(floatLit(_,Tp),Tp) :- !.
typeOfCanon(stringLit(_,Tp),Tp) :- !.
typeOfCanon(enm(_,_,Tp),Tp) :- !.
typeOfCanon(cons(_,_,Tp),Tp) :- !.
typeOfCanon(where(_,T,_),Tp) :- !, typeOfCanon(T,Tp).
typeOfCanon(abstraction(_,_,_,_,_,Tp),Tp) :- !.
typeOfCanon(search(_,_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(match(_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(conj(_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(disj(_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(implies(_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(cond(_,_,_,_,Tp),Tp) :-!.
typeOfCanon(letExp(_,_,_,Bnd),Tp) :- !,typeOfCanon(Bnd,Tp).
typeOfCanon(letRec(_,_,_,Bnd),Tp) :- !,typeOfCanon(Bnd,Tp).
typeOfCanon(apply(_,_,_,Tp),Tp) :-!.
typeOfCanon(tple(_,Els),tupleType(Tps)) :-!,
  map(Els,canon:typeOfCanon,Tps).
typeOfCanon(assign(_,_,Vl),Tp) :-
  typeOfCanon(Vl,Tp).
typeOfCanon(cell(_,Vl),refType(Tp)) :-
  typeOfCanon(Vl,Tp).
typeOfCanon(lambda(_,_,_,Tp),Tp) :-!.
typeOfCanon(over(_,T,_,_),Tp) :- typeOfCanon(T,Tp).
typeOfCanon(overaccess(V,_,_),Tp) :- typeOfCanon(V,Tp).
typeOfCanon(mtd(_,_,Tp),Tp) :-!.
typeOfCanon(case(_,_,_,Tp),Tp) :- !.
typeOfCanon(valof(_,_,Tp),Tp).

locOfCanon(v(Lc,_,_),Lc) :- !.
locOfCanon(dot(Lc,_,_,_),Lc) :- !.
locOfCanon(intLit(Lc,_),Lc) :- !.
locOfCanon(floatLit(Lc,_),Lc) :- !.
locOfCanon(stringLit(Lc,_),Lc) :- !.
locOfCanon(enm(Lc,_,_),Lc) :- !.
locOfCanon(where(Lc,_,_),Lc) :- !.
locOfCanon(abstraction(Lc,_,_,_,_,_),Lc) :- !.
locOfCanon(search(Lc,_,_,_),Lc) :-!.
locOfCanon(match(Lc,_,_),Lc) :-!.
locOfCanon(conj(Lc,_,_),Lc) :-!.
locOfCanon(disj(Lc,_,_),Lc) :-!.
locOfCanon(neg(Lc,_),Lc) :-!.
locOfCanon(implies(Lc,_,_),Lc) :-!.
locOfCanon(cond(Lc,_,_,_,_),Lc) :-!.
locOfCanon(letExp(Lc,_,_,_),Lc) :- !.
locOfCanon(letRec(Lc,_,_,_),Lc) :- !.
locOfCanon(case(Lc,_,_,_),Lc) :- !.
locOfCanon(apply(Lc,_,_,_),Lc) :-!.
locOfCanon(tple(Lc,_),Lc) :-!.
locOfCanon(lambda(Lc,_,_,_),Lc) :-!.
locOfCanon(valof(Lc,_,_),Lc) :-!.
locOfCanon(doTerm(Lc,_,_),Lc) :-!.
locOfCanon(taskTerm(Lc,_,_),Lc) :-!.
locOfCanon(seqDo(Lc,_,_),Lc) :-!.
locOfCanon(ifThenDo(Lc,_,_,_,_,_,_),Lc) :-!.
locOfCanon(whileDo(Lc,_,_),Lc) :-!.
locOfCanon(untilDo(Lc,_,_),Lc) :-!.
locOfCanon(forDo(Lc,_,_),Lc) :-!.
locOfCanon(caseDo(Lc,_,_),Lc) :-!.
locOfCanon(tryCatchDo(Lc,_,_),Lc) :-!.
locOfCanon(assign(Lc,_,_,_,_),Lc) :-!.
locOfCanon(apply(Lc,_,_,_),Lc) :-!.
locOfCanon(varDo(Lc,_,_,_,_,_),Lc) :-!.
locOfCanon(valisDo(Lc,_,_,_,_),Lc) :-!.
locOfCanon(throwDo(Lc,_,_,_,_),Lc) :-!.
locOfCanon(performDo(Lc,_,_,_,_),Lc) :-!.
locOfCanon(simpleDo(Lc,_,_,_),Lc) :-!.
locOfCanon(noDo(Lc),Lc) :-!.

constructorName(enm(_,Nm,_),Nm) :-!.
constructorName(cons(_,Nm,_),Nm).

constructorType(enm(_,_,Tp),Tp) :-!.
constructorType(cons(_,_,Tp),Tp).

ssCanonProg(prog(Pkg,Imports,XDecls,Decls,Defs),sq([PP,nl(2),iv(nl(2),XX),rb])) :-
  ssPkg(Pkg,PP),
  map(Imports,canon:ssImport,II),
  map(XDecls,canon:ssDecl(2,ss("export ")),EE),
  map(Decls,canon:ssDecl(2,ss("local ")),DD),
  map(Defs,canon:ssDf(2),FF),
  flatten([EE,DD,II,FF],XX).

ssPkg(pkg(Nm,V),sq([ss(Nm)|Vs])) :-
  ssVersion(V,Vs).

ssVersion(defltVersion,[]).
ssVersion(ver(V),[ss(":"),ss(V)]).

ssTerm(v(_,Nm,_),_,id(Nm)).
ssTerm(void,_,ss("void")).
ssTerm(intLit(Ix,_),_,ix(Ix)).
ssTerm(floatLit(Dx,_),_,fx(Dx)).
ssTerm(stringLit(Str,_),_,sq([ss(""""),ss(Str),ss("""")])).
ssTerm(apply(_,Op,Args,_),Dp,sq([O,A])) :-
  ssTerm(Op,Dp,O),
  ssTerm(Args,Dp,A).
ssTerm(dot(_,Rc,Fld,_),Dp,sq([R,ss("."),id(Fld)])) :-
  ssTerm(Rc,Dp,R).
ssTerm(enm(_,Nm,_),_,sq([ss("."),id(Nm)])).
ssTerm(cons(_,Nm,_),_,sq([ss("."),id(Nm)])).
ssTerm(case(_,Bound,Cases,_),Dp,
	    sq([ss("case "),B,ss(" in {"),Rs,ss("}")])) :-
  ssTerm(Bound,Dp,B),
  ssRls("",Cases,Dp,Rs).
ssTerm(assign(_,Vr,Vl),Dp,sq([L,ss(" := "),R])) :-
  ssTerm(Vr,Dp,L),
  ssTerm(Vl,Dp,R).
ssTerm(cell(_,Vr),Dp,sq([ss("!!"),V])) :-
  ssTerm(Vr,Dp,V).
ssTerm(letExp(_,Decls,Defs,Ex),Dp,
	    sq([ss("let {."),nl(Dp2),iv(nl(Dp2),Ds),nl(Dp),ss(".} in "),B])) :-
  Dp2 is Dp+2,
  map(Decls,canon:ssDecl(Dp2,ss("let ")),DD),
  map(Defs,canon:ssDf(Dp2),XX),
  flatten([DD,XX],Ds),
  ssTerm(Ex,Dp,B).
ssTerm(letRec(_,Decls,Defs,Ex),Dp,
	    sq([ss("let {"),nl(Dp2),iv(nl(Dp2),Ds),nl(Dp),ss("} in "),B])) :-
  Dp2 is Dp+2,
  map(Decls,canon:ssDecl(Dp2,ss("rec ")),DD),
  map(Defs,canon:ssDf(Dp2),XX),
  flatten([DD,XX],Ds),
  ssTerm(Ex,Dp,B).
ssTerm(lambda(_,_,Rle,_),Dp,sq([lp,Rl,rp])) :-
  ssRule("",Dp,Rle,Rl).
ssTerm(tple(_,Els),Dp,sq([lp,iv(ss(", "),SEls),rp])) :-
  ssTerms(Els,Dp,SEls).
ssTerm(mtd(_,Nm,_),_,sq([ss("°"),id(Nm)])).
ssTerm(over(_,V,_,Cons),Dp,sq([iv(ss(","),CCs),ss("|:"),VV])) :-
  map(Cons,types:ssConstraint(false,Dp),CCs),
  ssTerm(V,Dp,VV).
ssTerm(overaccess(V,TV,F),Dp,sq([TT,ss("<~"),FF,ss("|:"),VV])) :-
  ssType(TV,false,Dp,TT),
  ssType(F,false,Dp,FF),
  ssTerm(V,Dp,VV).
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
ssTerm(cond(_,Test,Either,Or,_),Dp,sq([lp,TT,ss("?"),LL,ss(" || "),RR,rp])) :-
  ssTerm(Test,Dp,TT),
  ssTerm(Either,Dp,LL),
  ssTerm(Or,Dp,RR).
ssTerm(match(_,P,E),Dp,sq([lp,LL,ss(" .= "),RR,rp])) :-
  ssTerm(P,Dp,LL),
  ssTerm(E,Dp,RR).
ssTerm(search(_,P,S,M),Dp,sq([lp,LL,ss(" in "),RR,ss(" using "),II,rp])) :-
  ssTerm(P,Dp,LL),
  ssTerm(S,Dp,RR),
  ssTerm(M,Dp,II).
ssTerm(abstraction(_,Bound,Guard,G,_,_),Dp,
	    sq([ss("{"),BB,ss(" | "),CC, ss(" using "),II,ss("}")])) :-
  ssTerm(Bound,Dp,BB),
  ssTerm(Guard,Dp,CC),
  ssTerm(G,Dp,II).
ssTerm(neg(_,R),Dp,sq([lp,ss(" ~ "),RR,rp])) :-
  ssTerm(R,Dp,RR).
ssTerm(doTerm(_,Body,_),Dp,sq([ss(" do "),AA])) :-
  ssAction(Body,Dp,AA).
ssTerm(taskTerm(_,Body,_),Dp,sq([ss(" task "),AA])) :-
  ssAction(Body,Dp,AA).
ssTerm(valof(_,E,_),Dp,sq([ss("valof "),EE])) :-
  ssTerm(E,Dp,EE).
ssTerm(noDo(_),_,ss(" nothing ")).

ssTerms([],_,[]).
ssTerms([T|More],Dp,[TT|TTs]) :-
  ssTerm(T,Dp,TT),
  ssTerms(More,Dp,TTs).

ssConstraints([],_,[]).
ssConstraints([T|More],Dp,[TT|TTs]) :-
  ssConstraint(false,Dp,T,TT),
  ssConstraints(More,Dp,TTs).

ssAction(seqDo(Lc,A,B),Dp,sq([lb,iv(sq([ss(";"),nl(Dp2)]),AA),rb])) :-
  Dp2 is Dp+2,
  ssActions(seqDo(Lc,A,B),Dp2,AA).
ssAction(varDo(_,Ptn,Exp),Dp,sq([PP,ss(" .= "),VV])) :-
  ssTerm(Ptn,Dp,PP),
  ssTerm(Exp,Dp,VV).
ssAction(ifThenDo(_,Tst,Th,El),Dp,
	 sq([ss("if "),CC,ss("then"),nl(Dp2),TT,nl(Dp2),ss("else"),EE])) :-
  Dp2 is Dp+2,
  ssTerm(Tst,Dp,CC),
  ssAction(Th,Dp2,TT),
  ssAction(El,Dp2,EE).
ssAction(caseDo(_,Gov,Cses,_,_),Dp,
	 sq([ss("case "),GG,ss("in"),lb,iv(nl(Dp2),CC),rb])) :-
  Dp2 is Dp+2,
  ssTerm(Gov,Dp,GG),
  map(Cses,canon:ssActionCase(Dp2),CC).
ssAction(whileDo(_,Tst,Bdy),Dp,
	 sq([ss("while "),CC,ss("do"),nl(Dp2),BB])) :-
  Dp2 is Dp+2,
  sTerm(Tst,Dp,CC),
  ssAction(Bdy,Dp2,BB).
ssAction(untilDo(_,Tst,Bdy),Dp,
	 sq([ss("do"),BB,ss("until"),CC])) :-
  Dp2 is Dp+2,
  sTerm(Tst,Dp,CC),
  ssAction(Bdy,Dp2,BB).
ssAction(forDo(_,Tst,Bdy),Dp,
	 sq([ss("for "),CC,ss("do"),nl(Dp2),BB])) :-
  Dp2 is Dp+2,
  sTerm(Tst,Dp,CC),
  ssAction(Bdy,Dp2,BB).
ssAction(tryCatchDo(_,Bdy,Hndlr),Dp,
	 sq([ss("try "),BB,ss("catch"),HH])) :-
  Dp2 is Dp+2,
  sTerm(Hndlr,Dp2,HH),
  ssAction(Bdy,Dp2,BB).
ssAction(valisDo(_,Exp),Dp,sq([ss("valis "),EE])) :-
  ssTerm(Exp,Dp,EE).
ssAction(throwDo(_,Exp),Dp,sq([ss("throw "),EE])) :-
  ssTerm(Exp,Dp,EE).
ssAction(performDo(_,Exp),Dp,sq([ss("perform "),EE])) :-
  ssTerm(Exp,Dp,EE).
ssAction(simpleDo(_,Exp),Dp,sq([ss("just "),EE])) :-
  ssTerm(Exp,Dp,EE).

ssActions(seqDo(_,A,B),Dp,[AA|BB]) :-
  ssAction(A,Dp,AA),
  ssActions(B,Dp,BB).
ssActions(A,Dp,[AA]) :-
  ssAction(A,Dp,AA).

ssActionCase(Dp,equation(_,Args,Guard,Value),sq([AA,GG,ss(" => "),VV])) :-
  ssTerm(Args,Dp,AA),
  ssGuard(Guard,Dp,GG),
  ssAction(Value,Dp,VV).

ssRule(Nm,Dp,equation(_,Args,Guard,Value),sq([id(Nm),A,G,ss(" => "),V])) :-
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

ssDef(Dp,funDef(Lc,Nm,ExtNm,_Type,_Cx,Eqns),
      sq([ss("fun "),id(Nm),ss("@"),Lcs,nl(Dp),Rs])) :-
  ssRls(ExtNm,Eqns,Dp,Rs),
  ssLoc(Lc,Lcs).
ssDef(Dp,varDef(Lc,Nm,ExtNm,_Cx,Tp,Value),
      sq([ss("var "),id(Nm),ss(" : "),Ts,ss("@"),Lcs,nl(Dp),
	  id(ExtNm),ss(" = "),V])) :-
  ssType(Tp,true,Dp,Ts),
  ssTerm(Value,Dp,V),
  ssLoc(Lc,Lcs).
ssDef(Dp,cnsDef(Lc,Nm,C),
      sq([ss("con "),id(Nm),ss(" : "),Ts,ss("@"),Lcs])) :-
  typeOfCanon(C,Tp),
  ssType(Tp,true,Dp,Ts),
  ssLoc(Lc,Lcs).
ssDef(Dp,typeDef(Lc,Nm,_Tp,Rl),
      sq([ss("type "),id(Nm),ss(":"),Ts,ss("@"),Lcs])) :-
  ssLoc(Lc,Lcs),
  ssType(Rl,true,Dp,Ts).
ssDef(Dp,conDef(Nm,_ConNm,_ConTp,Rl),
      sq([ss("contract "),id(Nm),ss(" : "),Ts])) :-
  ssType(Rl,true,Dp,Ts).
ssDef(Dp,implDef(Nm,_ConNm,ImplNm,ImplTp),
      sq([ss("impl "),id(Nm),ss(" = "),id(ImplNm),ss(":"),Ts])) :-
  ssType(ImplTp,true,Dp,Ts).
ssDef(Dp,accDef(Tp,FldNm,FunNm,_),
      sq([ss("acc "),Ts,ss("."),id(FldNm),ss(" = "),id(FunNm)])) :-
  ssType(Tp,false,Dp,Ts).

ssFunction(Dp,Nm,Type,Eqns,
	   sq([ss("fun "),id(Nm),ss(" : "),Ts,nl(Dp),Rs])) :-
  ssType(Type,true,Dp,Ts),
  ssRls(Nm,Eqns,Dp,Rs).

ssRls(Nm,Eqns,Dp,iv(nl(Dp),EE)) :-
  map(Eqns,canon:ssEqn(Nm,Dp),EE).

ssEqn("",Dp,equation(_,Args,Guard,Value),
      sq([canon:ssTerm(Args,Dp),canon:ssGuard(Guard,Dp),ss("=>"),
	  canon:ssTerm(Value,Dp)])).
ssEqn(Nm,Dp,equation(_,Args,Guard,Value),
      sq([id(Nm),
	  canon:ssTerm(Args,Dp),canon:ssGuard(Guard,Dp),ss("=>"),
	  canon:ssTerm(Value,Dp)])).

dispDecls(Decls) :-
  map(Decls,canon:ssDecl(0,ss("")),DD),
  displayln(iv(nl(0),DD)).

ssDecl(Dp,X,funDec(Nm,LclNme,Type),
       sq([X,ss("fun "),id(Nm),ss("~"),id(LclNme),ss(" :: "),TT])) :-
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
ssDecl(Dp,X,contractDec(Nm,_ConNm,_ConTp,Rl),
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

ssContract(Dp,conDef(Nm,ConNm,_ConTp,ConRule),
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

splitPtn(P,Px,Cnd) :-
  locOfCanon(P,Lc),
  splitPttrn(P,Px,none,C),
  deoptional(C,Lc,Cnd).

deoptional(none,Lc,enm(Lc,"true",type("star.core*boolean"))).
deoptional(some(C),_,C).

splitPttrn(apply(Lc,Op,Arg),apply(Lc,NOp,NArg),Cond,Cx) :-
  splitPttrn(Op,NOp,Cond,C0),
  splitPttrn(Arg,NArg,C0,Cx).
splitPttrn(tple(Lc,Els),tple(Lc,NEls),Cond,Cx) :-
  splitPttrns(Els,NEls,Cond,Cx).
splitPttrn(where(Lc,Ptn,Cond),P1,C,Cx) :-
  splitPttrn(Ptn,P1,C,C0),
  mergeGl(C0,some(Cond),Lc,Cx).
splitPttrn(P,P,C,C).

mergeGl(none,G,_,G) :-!.
mergeGl(G,none,_,G) :-!.
mergeGl(some(G1),some(G2),Lc,some(conj(Lc,G1,G2))).

splitPttrns([],[],C,C).
splitPttrns([P|Ps],[Px|Pxs],C,Cx) :-
  splitPttrn(P,Px,C,C0),
  splitPttrns(Ps,Pxs,C0,Cx).

anonVar(Lc,v(Lc,N,Tp),Tp) :-
  genstr("_",N),
  newTypeVar(N,Tp).

