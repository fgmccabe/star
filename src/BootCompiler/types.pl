:- module(types,[isType/1,isConstraint/1,newTypeVar/2,newTypeVar/3,readOnlyTypeVar/2,skolemFun/3,deRef/2,
      typeArity/2,isFunctionType/2,isGrammarType/2,isPtnType/1,isPtnType/2,isClassType/2,
      dispType/1,showType/3,showConstraint/3,
      occursIn/2,isUnbound/1,isBound/1, constraints/2, isIdenticalVar/2,
      bind/2, moveQuants/3,
      moveConstraints/3,moveConstraints/4,setConstraint/2,setConstraints/2, implementationName/2]).
:- use_module(misc).

isType(anonType).
isType(voidType).
isType(thisType).
isType(kVar(_,_)).
isType(tVar(_,_,_,_)).
isType(type(_)).
isType(typeExp(_,_)).
isType(refType(_)).
isType(tupleType(_)).
isType(funType(_,_)).
isType(ptnType(_,_)).
isType(grammarType(_,_)).
isType(consType(_,_)).
isType(univType(_,_)).
isType(existType(_,_)).
isType(faceType(_)).
isType(constrained(_,_)).

isConstraint(conTract(_,_,_)).
isConstraint(implementsFace(_,_)).

% the _ in unb(_) is to work around issues with SWI-Prolog's assignment.
newTypeVar(Nm,tVar(_,_,Nm,Id)) :- gensym("_#",Id).
newTypeVar(Nm,Constraints,tVar(_,C,Nm,Id)) :- gensym("_#",Id), concat(Constraints,_,C).

readOnlyTypeVar(Nm,kVar(Id)) :- genstr(Nm,Id).

skolemFun(Nm,Ar,kFun(Id,Ar)) :- genstr(Nm,Id).

deRef(tVar(Curr,_,_,_),Tp) :- nonvar(Curr), !, deRef(Curr,Tp),!.
deRef(T,T).

isIdenticalVar(tVar(_,_,_,Id),tVar(_,_,_,Id)).

isUnbound(T) :- deRef(T,tVar(Curr,_,_,_)), var(Curr).

isBound(T) :- deRef(T,TV), TV\=tVar(_,_,_,_).

constraints(Tp,Cons) :- deRef(Tp,tVar(_,Cons,_,_)).

setConstraints(constrained(Tp,Con),XTp) :-
  setConstraint(Con),
  setConstraints(Tp,XTp).
setConstraints(Tp,Tp).

setConstraint(conTract(Nm,Args,Deps)) :-
  constrainArgs(Args,conTract(Nm,Args,Deps)).
setConstraint(implementsFace(Tp,Face)) :-
  setConstraint(Tp,implementsFace(Tp,Face)).

constrainArgs([],_).
constrainArgs([V|L],C) :-
  setConstraint(V,C),
  constrainArgs(L,C).

setConstraint(tVar(_,Cons,_,_),C) :-
  setCon(Cons,C).
setConstraint(_,_).

setCon([Cons|_],C) :- var(Cons), !, Cons = C.
setCon([Cons|L],C) :- notSameConstraint(C,Cons),!, setCon(L,C).
setCon([Cons|_],C) :- sameConstraint(Cons,C),!.

bind(T,Tp) :- \+occursIn(T,Tp), T=tVar(Tp,_,_,_).

occursIn(TV,Tp) :- deRef(Tp,DTp), \+ isIdenticalVar(TV,DTp), TV = tVar(_,_,_,Id), occIn(Id,DTp),!.

occIn(Id,tVar(_,_,_,Id)) :-!.
occIn(Id,tVar(Curr,_,_,_)) :- nonvar(Curr), !, occIn(Id,Curr).
occIn(Id,typeExp(O,_)) :- occIn(Id,O),!.
occIn(Id,typeExp(_,L)) :- is_member(A,L), occIn(Id,A).
occIn(Id,refType(I)) :- occIn(Id,I).
occIn(Id,tupleType(L)) :- is_member(A,L), occIn(Id,A).
occIn(Id,funType(L,_)) :- is_member(A,L), occIn(Id,A).
occIn(Id,funType(_,R)) :- is_member(A,R), occIn(Id,A).
occIn(Id,ptnType(L,_)) :- is_member(A,L), occIn(Id,A).
occIn(Id,ptnType(_,R)) :- is_member(A,R), occIn(Id,A).
occIn(Id,grammarType(L,_)) :- is_member(A,L), occIn(Id,A).
occIn(Id,grammarType(_,R)) :- is_member(A,R), occIn(Id,A).
occIn(Id,consType(L,_)) :- is_member(A,L), occIn(Id,A).
occIn(Id,consType(_,R)) :- occIn(Id,R).
occIn(Id,constrained(Tp,Con)) :- occIn(Id,Con) ; occIn(Id,Tp).
occIn(Id,univType(_,Tp)) :- occIn(Id,Tp).
occIn(Id,faceType(L)) :- is_member((_,A),L), occIn(Id,A).

moveQuants(univType(B,Tp),[B|Q],Tmpl) :- !,
  moveQuants(Tp,Q,Tmpl).
moveQuants(Tp,[],Tp).

moveConstraints(constrained(Tp,Con),[Con|C],Tmp) :-!,
  moveConstraints(Tp,C,Tmp).
moveConstraints(Tp,[],Tp).

moveConstraints(constrained(Tp,Con),C,Cx,Inner) :-
  moveConstraints(Tp,[Con|C],Cx,Inner).
moveConstraints(Tp,C,C,Tp).

showType(anonType,O,Ox) :- appStr("_",O,Ox).
showType(voidType,O,Ox) :- appStr("void",O,Ox).
showType(thisType,O,Ox) :- appStr("this",O,Ox).
showType(kVar(Nm),O,Ox) :- appStr(Nm,O,Ox).
showType(kFun(Nm,Ar),O,Ox) :- appStr(Nm,O,O1),appStr("/",O1,O2),appInt(Ar,O2,Ox).
showType(tVar(Curr,_,_,_),O,Ox) :- nonvar(Curr),!,showType(Curr,O,Ox).
showType(tVar(_,_,Nm,Id),O,Ox) :- appStr("%",O,O1),appStr(Nm,O1,O2),appSym(Id,O2,Ox).
showType(type(Nm),O,Ox) :- appStr(Nm,O,Ox).
showType(tpFun(Nm,Ar),O,Ox) :- appStr(Nm,O,O1),appStr("%",O1,O2),appInt(Ar,O2,Ox).
showType(typeExp(Nm,A),O,Ox) :- showType(Nm,O,O1), appStr("[",O1,O2),showTypeEls(A,O2,O3),appStr("]",O3,Ox).
showType(tupleType(A),O,Ox) :- appStr("(",O,O1), showTypeEls(A,O1,O2), appStr(")",O2,Ox).
showType(funType(A,R),O,Ox) :- appStr("(",O,O1), showTypeEls(A,O1,O2), appStr(")",O2,O3), appStr("=>",O3,O4), showType(R,O4,Ox).
showType(ptnType(A,R),O,Ox) :- appStr("(",O,O1), showTypeEls(A,O1,O2), appStr(")",O2,O3), appStr("<=",O3,O4), showType(R,O4,Ox).
showType(grammarType(A,R),O,Ox) :- appStr("(",O,O1), showTypeEls(A,O1,O2), appStr(")",O2,O3), appStr("-->",O3,O4), showType(R,O4,Ox).
showType(consType(A,R),O,Ox) :- showTypeEls(A,O,O1), appStr("<=>",O1,O2), showType(R,O2,Ox).
showType(refType(R),O,Ox) :- appStr("ref",O,O4), showType(R,O4,Ox).
showType(univType(V,Tp),O,Ox) :- appStr("all ",O,O1), showBound(V,O1,O2), showMoreQuantified(Tp,showType,O2,Ox).
showType(existType(V,Tp),O,Ox) :- appStr("exist ",O,O1), showBound(V,O1,O2), showMoreQuantified(Tp,showType,O2,Ox).
showType(faceType(Els),O,Ox) :- appStr("{ ",O,O1), showTypeFields(Els,O1,O2), appStr("}",O2,Ox).
showType(typeExists(Hd,Bd),O,Ox) :- showType(Hd,O,O1), appStr("<~",O1,O2),showType(Bd,O2,Ox).
showType(constrained(Tp,Con),O,Ox) :- showConstraint(Con,O,O1), showMoreConstraints(Tp,O1,Ox).

showMoreConstraints(constrained(Tp,Con),O,Ox) :- appStr(",",O,O1), showConstraint(Con,O1,O2), showMoreConstraints(Tp,O2,Ox).
showMoreConstraints(Tp,O,Ox) :- appStr("|:",O,O1),showType(Tp,O1,Ox).

showConstraint(univType(V,B),O,Ox) :-
  appStr("all ",O,O1),
  showBound(V,O1,O2),
  showMoreQuantified(B,showConstraint,O2,Ox).
showConstraint(conTract(Nm,Els,[]),O,Ox) :-!,
  appStr(Nm,O,O1), appStr("[",O1,O2),showTypeEls(Els,O2,O3),appStr("]",O3,Ox).
showConstraint(conTract(Nm,Els,Deps),O,Ox) :-
  appStr(Nm,O,O1),
  appStr("[",O1,O2),
  showTypeEls(Els,O2,O3),
  appStr("->>",O3,O4),
  showTypeEls(Deps,O4,O5),
  appStr("]",O5,Ox).
showConstraint(implementsFace(Tp,Els),O,Ox) :-
  showType(Tp,O,O1),
  appStr("<~",O1,O2),
  showType(faceType(Els),O2,Ox).
showConstraint(constrained(Con,Extra),O,Ox) :-
  showConstraint(Extra,O,O1),
  appStr("|:",O1,O2),
  showConstraint(Con,O2,Ox).

showBound(Nm,O,Ox) :- showType(Nm,O,Ox).

showTypeEls([],O,O).
showTypeEls([Tp|More],O,E) :- showType(Tp,O,O1), showMoreTypeEls(More,O1,E).

showMoreTypeEls([],O,O).
showMoreTypeEls([Tp|More],O,E) :- appStr(", ",O,O1),showType(Tp,O1,O2), showMoreTypeEls(More,O2,E).

showMoreQuantified(univType(Nm,Tp),P,O,E) :- appStr(", ",O,O1), showType(Nm,O1,O2), showMoreQuantified(Tp,P,O2,E).
showMoreQuantified(Tp,P,O,E) :- appStr(" ~~ ",O,O1), call(P,Tp,O1,E).

showTypeFields([],O,O).
showTypeFields([F|More],O,E) :- showTypeField(F,O,O1), showMoreTypeFields(More,O1,E).

showMoreTypeFields([],O,O).
showMoreTypeFields([Fld|More],O,E) :- appStr(". ",O,O1), showTypeField(Fld,O1,O2), showMoreTypeFields(More,O2,E).

showTypeField((Nm,Tp),O,E) :- appStr(Nm,O,O1), appStr(" : ",O1,O2), showType(Tp,O2,E).

dispType(Tp) :-
  showType(Tp,Chrs,[]),
  string_chars(Text,Chrs),
  writeln(Text).

typeArity(univType(_,Tp),Ar) :- typeArity(Tp,Ar).
typeArity(existType(_,Tp),Ar) :- typeArity(Tp,Ar).
typeArity(constrained(Tp,_),Ar) :- typeArity(Tp,Ar).
typeArity(funType(A,_),Ar) :- length(A,Ar).
typeArity(ptnType(A,_),Ar) :- length(A,Ar).
typeArity(grammarType(A,_),Ar) :- length(A,Ar).
typeArity(consType(A,_),Ar) :- length(A,Ar).
typeArity(refType(A),Ar) :- typeArity(A,Ar).
typeArity(_,0).

isFunctionType(univType(_,Tp),Ar) :- isFunctionType(Tp,Ar).
isFunctionType(funType(A,_),Ar) :- length(A,Ar).

isPtnType(Tp) :- isPtnType(Tp,_).

isPtnType(univType(_,Tp),Ar) :- isPtnType(Tp,Ar).
isPtnType(ptnType(A),Ar) :- length(A,Ar).

isClassType(univType(_,Tp),Ar) :- isClassType(Tp,Ar).
isClassType(consType(A,_),Ar) :- length(A,Ar).

isGrammarType(univType(_,Tp),Ar) :- isGrammarType(Tp,Ar).
isGrammarType(grammarType(A,_),Ar) :- length(A,Ar).

implementationName(conTract(Nm,Args,_),INm) :-
  appStr(Nm,S0,S1),
  marker(conTract,M),
  surfaceNames(Args,M,S1,[]),
  string_chars(INm,S0).

surfaceNames([],_,S,S).
surfaceNames([T|L],Sep,S0,Sx) :-
  deRef(T,TT),
  surfaceName(TT,SN),
  appStr(Sep,S0,S1),
  appStr(SN,S1,S2),
  surfaceNames(L,Sep,S2,Sx).

surfaceName(type(Nm),Nm).
surfaceName(typeExp(Op,_),Nm) :- deRef(Op,OO), surfaceName(OO,Nm).
surfaceName(kVar(Nm),Nm).
surfaceName(kFun(Nm,_),Nm).
surfaceName(tpFun(Nm,_),Nm).
surfaceName(tupleType(Els),Nm) :-
  length(Els,Ar),
  swritef(Nm,"()%d",[Ar]).
