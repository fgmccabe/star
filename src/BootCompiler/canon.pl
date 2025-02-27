:- module(canon,[dispFunction/3,dispDef/1,dispCanon/1,dispAction/1,dispCanonProg/1,
		 ssCanonProg/2,ssTerm/3,ssPkg/2,ssContract/3,ssRule/3,ssDecl/2,
		 dispDecls/1,
		 typeOfCanon/2,typesOf/2,locOfCanon/2,
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
isCanon(apply(_,_,_,_)).
isCanon(capply(_,_,_,_)).
isCanon(dot(_,_,_,_)).
isCanon(update(_,_,_,_)).
isCanon(tdot(_,_,_,_)).
isCanon(enm(_,_,_)).
isCanon(thnkRef(_,_,_)).
isCanon(newSV(_,_)).
isCanon(svGet(_,_,_)).
isCanon(svSet(_,_,_)).
isCanon(tple(_,_)).
isCanon(where(_,_,_)).
isCanon(conj(_,_,_)).
isCanon(disj(_,_,_)).
isCanon(implies(_,_,_)).
isCanon(cond(_,_,_,_,_)).
isCanon(match(_,_,_)).
isCanon(open(_,_,_)).
isCanon(neg(_,_)).
isCanon(lambda(_,_,_,_,_)).
isCanon(fiber(_,_,_)).
isCanon(tryCatch(_,_,_,_)).
isCanon(try(_,_,_)).
isCanon(check(_,_,_)).
isCanon(fail(_,_,_)).
isCanon(result(_,_,_)).

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
typeOfCanon(tdot(_,_,_,Tp),Tp) :- !.
typeOfCanon(update(_,Rc,_,_),Tp) :- !, typeOfCanon(Rc,Tp).
typeOfCanon(intLit(_,_),type("integer")) :- !.
typeOfCanon(bigLit(_,_),type("bigint")) :- !.
typeOfCanon(floatLit(_,_),type("float")) :- !.
typeOfCanon(charLit(_,_),type("char")) :- !.
typeOfCanon(stringLit(_,_),type("string")) :- !.
typeOfCanon(enm(_,_,Tp),Tp) :- !.
typeOfCanon(where(_,T,_),Tp) :- !, typeOfCanon(T,Tp).
typeOfCanon(open(_,_,Tp),Tp) :-!.
typeOfCanon(match(_,_,_),type("boolean")) :-!.
typeOfCanon(conj(_,_,_),type("boolean")) :-!.
typeOfCanon(disj(_,_,_),type("boolean")) :-!.
typeOfCanon(implies(_,_,_),type("boolean")) :-!.
typeOfCanon(cond(_,_,_,_,Tp),Tp) :-!.
typeOfCanon(letExp(_,_,_,Bnd),Tp) :- !,typeOfCanon(Bnd,Tp).
typeOfCanon(letRec(_,_,_,Bnd),Tp) :- !,typeOfCanon(Bnd,Tp).
typeOfCanon(apply(_,_,_,Tp),Tp) :-!.
typeOfCanon(capply(_,_,_,Tp),Tp) :-!.
typeOfCanon(tple(_,Els),tplType(Tps)) :-!,
  map(Els,canon:typeOfCanon,Tps).
typeOfCanon(nth(_,_,_,Tp),Tp) :-!.
typeOfCanon(cell(_,Vl),tpExp(tpFun("ref",1),Tp)) :-
  typeOfCanon(Vl,Tp).
typeOfCanon(deref(_,Vl),Tp) :-
  typeOfCanon(Vl,tpExp(tpFun("ref",1),Tp)).
typeOfCanon(lambda(_,_,_,_,Tp),Tp) :-!.
typeOfCanon(over(_,T,_),Tp) :- typeOfCanon(T,Tp).
typeOfCanon(overaccess(_,_,_,Tp),Tp) :- !.
typeOfCanon(mtd(_,_,Tp),Tp) :-!.
typeOfCanon(case(_,_,_,Tp),Tp) :- !.
typeOfCanon(raise(_,_,_,Tp),Tp) :-!.
typeOfCanon(fiber(_,_,Tp),Tp) :-!.
typeOfCanon(thnkRef(_,_,Tp),Tp) :- !.
typeOfCanon(newSV(_,Tp),Tp) :- !.
typeOfCanon(svGet(_,_,Tp),Tp) :- !.
typeOfCanon(svSet(_,_,Vl),Tp) :- !,
  typeOfCanon(Vl,Tp).
typeOfCanon(valof(_,_,Tp),Tp) :-!.
typeOfCanon(tryCatch(_,E,_T,_),Tp) :- !,
  typeOfCanon(E,Tp).
typeOfCanon(try(_,E,_),Tp) :- !, typeOfCanon(E,Tp).
typeOfCanon(check(_,_,Tp),Tp) :-!.
typeOfCanon(fail(_,_,Tp),Tp) :-!.
typeOfCanon(result(_,_,Tp),Tp) :-!.

typesOf([],[]).
typesOf([C|Cs],[Tp|Tps]) :-
  typeOfCanon(C,Tp),
  typesOf(Cs,Tps).

locOfCanon(v(Lc,_,_),Lc) :- !.
locOfCanon(anon(Lc,_),Lc) :- !.
locOfCanon(dot(Lc,_,_,_),Lc) :- !.
locOfCanon(update(Lc,_,_,_),Lc) :- !.
locOfCanon(tdot(Lc,_,_,_),Lc) :- !.
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
locOfCanon(apply(Lc,_,_,_),Lc) :-!.
locOfCanon(capply(Lc,_,_,_),Lc) :-!.
locOfCanon(tple(Lc,_),Lc) :-!.
locOfCanon(lambda(Lc,_,_,_,_),Lc) :-!.
locOfCanon(assign(Lc,_,_),Lc) :-!.
locOfCanon(tryCatch(Lc,_,_,_),Lc) :-!.
locOfCanon(try(Lc,_,_),Lc) :-!.
locOfCanon(check(Lc,_,_),Lc) :-!.
locOfCanon(result(Lc,_,_),Lc) :-!.
locOfCanon(fail(Lc,_,_),Lc) :-!.
locOfCanon(whileDo(Lc,_,_),Lc) :-!.
locOfCanon(forDo(Lc,_,_,_),Lc) :-!.
locOfCanon(valis(Lc,_),Lc) :-!.
locOfCanon(raise(Lc,_,_,_),Lc) :-!.
locOfCanon(fiber(Lc,_,_),Lc) :-!.
locOfCanon(thnkRef(Lc,_,_),Lc) :-!.
locOfCanon(newSV(Lc,_),Lc) :-!.
locOfCanon(svGet(Lc,_,_),Lc) :-!.
locOfCanon(svSet(Lc,_,_),Lc) :-!.
locOfCanon(valof(Lc,_,_),Lc) :-!.
locOfCanon(doNop(Lc),Lc) :-!.
locOfCanon(doSeq(Lc,_,_),Lc) :-!.
locOfCanon(doLbld(Lc,_,_),Lc) :-!.
locOfCanon(doBrk(Lc,_),Lc) :-!.
locOfCanon(doValis(Lc,_),Lc) :-!.
locOfCanon(doSpawn(Lc,_),Lc) :-!.
locOfCanon(doMatch(Lc,_,_),Lc) :-!.
locOfCanon(doDefn(Lc,_,_),Lc) :-!.
locOfCanon(doAssign(Lc,_,_),Lc) :-!.
locOfCanon(doTryCatch(Lc,_,_,_),Lc) :- !.
locOfCanon(doTry(Lc,_,_),Lc) :-!.
locOfCanon(doResult(Lc,_),Lc) :-!.
locOfCanon(doFail(Lc,_),Lc) :-!.
locOfCanon(doIfThenElse(Lc,_,_,_),Lc) :-!.
locOfCanon(doWhile(Lc,_,_),Lc) :-!.
locOfCanon(doLet(Lc,_,_,_),Lc) :-!.
locOfCanon(doLetRec(Lc,_,_,_),Lc) :-!.
locOfCanon(doCall(Lc,_),Lc) :-!.
locOfCanon(doCase(Lc,_,_,_),Lc) :-!.

constructorName(enm(_,Nm,_),Nm) :-!.

constructorType(enm(_,_,Tp),Tp) :-!.

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
ssTerm(apply(_,Op,Args,_),Dp,sq([O,A])) :-
  ssTerm(Op,Dp,O),
  ssTerm(Args,Dp,A).
ssTerm(capply(_,Op,Args,_),Dp,sq([O,A])) :-
  ssTerm(Op,Dp,O),
  ssTerm(Args,Dp,A).
ssTerm(dot(_,Rc,Fld,_),Dp,sq([R,ss("."),id(Fld)])) :-
  ssTerm(Rc,Dp,R).
ssTerm(update(_,Rc,Fld,Vl),Dp,sq([RR,ss("."),id(Fld),ss("="),VV])) :-
  ssTerm(Rc,Dp,RR),
  ssTerm(Vl,Dp,VV).
ssTerm(tdot(_,Rc,Fld,_),Dp,sq([R,ss("."),ix(Fld)])) :-
  ssTerm(Rc,Dp,R).
ssTerm(enm(_,Nm,_),_,sq([ss("."),id(Nm)])).
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
ssTerm(lambda(_,Lbl,_,Rle,_),Dp,sq([lp,Rl,rp])) :-
  ssRule(Lbl,Dp,Rle,Rl).
ssTerm(tple(_,Els),Dp,sq([lp,iv(ss(", "),SEls),rp])) :-
  ssTerms(Els,Dp,SEls).
ssTerm(mtd(_,Nm,_),_,sq([ss("°"),id(Nm)])).
ssTerm(over(_,V,Cx),Dp,sq([ss("<"),iv(ss(","),CCs),ss("|:"),VV,ss(">")])) :-
  map(Cx,types:ssConstraint(false,Dp),CCs),
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
ssTerm(raise(_,_,E,_),Dp,sq([ss(" raise "),EE])) :-!,
  ssTerm(E,Dp,EE).
ssTerm(valof(_,A,_),Dp,sq([ss("valof "),AA])) :-!,
  ssAction(A,Dp,AA).
ssTerm(fiber(_,A,_),Dp,sq([ss("fiber "),AA])) :-!,
  ssTerm(A,Dp,AA).
ssTerm(thnkRef(_,A,_),Dp,sq([AA,ss("!!")])) :-!,
  ssTerm(A,Dp,AA).
ssTerm(newSV(_,T),Dp,sq([ss("#"),lp,TT,rp])) :-!,
  ssType(T,false,Dp,TT).
ssTerm(svGet(_,A,_),Dp,sq([AA,ss("^^")])) :-!,
  ssTerm(A,Dp,AA).
ssTerm(svSet(_,Th,Vl),Dp,sq([TT,ss("^:=^"),VV])) :-!,
  ssTerm(Th,Dp,TT),
  ssTerm(Vl,Dp,VV).
ssTerm(tryCatch(_,A,T,Hs),Dp,sq([ss("try "),AA,ss(" catch "),TT,ss(" in "),lb,HH,nl(Dp),rb])) :-!,
  Dp2 is Dp+2,
  ssTerm(T,Dp,TT),
  ssTerm(A,Dp2,AA),
  ssRls("",Hs,Dp2,canon:ssTerm,HH).
ssTerm(try(_,A,Hs),Dp,sq([ss("try "),AA,ss(" catch "),lb,HH,nl(Dp),rb])) :-!,
  Dp2 is Dp+2,
  ssTerm(A,Dp2,AA),
  ssRls("",Hs,Dp2,canon:ssTerm,HH).
ssTerm(check(_,T,_),Dp,sq([ss("?"),TT])) :-
  Dp2 is Dp+2,
  ssTerm(T,Dp2,TT).
ssTerm(result(_,T,_),Dp,sq([ss("^"),TT])) :-
  Dp2 is Dp+2,
  ssTerm(T,Dp2,TT).
ssTerm(fail(_,T,_),Dp,sq([ss("fail "),TT])) :-
  Dp2 is Dp+2,
  ssTerm(T,Dp2,TT),

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
ssAction(doCall(_,E),Dp,sq([ss("call "),EE])) :-!,
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
ssAction(doTry(_,A,Hs),Dp,sq([ss("try "),AA,ss(" catch "),lb,HH,nl(Dp),rb])) :-!,
  Dp2 is Dp+2,
  ssAction(A,Dp2,AA),
  ssRls("",Hs,Dp2,canon:ssAction,HH).
ssAction(doResult(_,E),Dp,sq([ss("result "),EE])) :-!,
  ssTerm(E,Dp,EE).
ssAction(doFail(_,E),Dp,sq([ss("fail "),EE])) :-!,
  ssTerm(E,Dp,EE).
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

ssRule(Rl,Dp,Ds) :-
  ssRule("",Dp,Rl,Ds).
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

ssDef(Dp,funDef(Lc,Nm,ExtNm,Sft,Tp,_Cx,Eqns),
      sq([ss(SS),ss("fun "),id(Nm),ss(":"),TT,ss(" @"),Lcs,nl(Dp),Rs])) :-
  ssType(Tp,true,Dp,TT),
  ssRls(ExtNm,Eqns,Dp,canon:ssTerm,Rs),
  ssLoc(Lc,Lcs),
  (Sft=soft -> SS="soft " ; SS="").
ssDef(Dp,varDef(_Lc,Nm,_ExtNm,_Cx,Tp,Value),
      sq([ss("var "),id(Nm),ss(":"),TT,ss(" = "),V])) :-
  ssType(Tp,true,Dp,TT),
  ssTerm(Value,Dp,V).
ssDef(Dp,cnsDef(Lc,Nm,C),
      sq([ss("con "),id(Nm),ss(" : "),Ts,ss("@"),Lcs])) :-
  typeOfCanon(C,Tp),
  ssType(Tp,true,Dp,Ts),
  ssLoc(Lc,Lcs).
ssDef(Dp,typeDef(Lc,_Nm,_Tp,Rl),sq([ss("type "),Ts,ss("@"),Lcs])) :-
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

ssDecl(Decl,SS) :-
  ssDecl(0,ss(""),Decl,SS).

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
ssDecl(Dp,X,effectDec(Nm,_ConNm,Rl),
       sq([X,ss("effect "),id(Nm),ss(" :: "),Ts])) :-
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

