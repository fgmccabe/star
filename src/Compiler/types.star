star.compiler.types{
  import star.
  import star.iterable.

  import star.compiler.location.
  import star.compiler.misc.

  public tipe ::=
    voidType |
    thisType |
    kVar(string) |
    kFun(string,integer) |
    tVar(tv,string) |
    tFun(tv,integer,string) |
    tipe(string) |
    tpFun(string,integer) |
    tpExp(tipe,tipe) |
    refType(tipe) |
    tupleType(list[tipe]) |
    allType(tipe,tipe) |
    existType(tipe,tipe) |
    faceType(list[(string,tipe)],list[(string,tipe)]) |
    typeLambda(tipe,tipe) |
    typeExists(tipe,tipe) |
    constrainedType(tipe,constraint).

  public constraint ::=
    conConstraint(string,list[tipe],list[tipe]) |
    implConstraint(tipe,tipe).

  tv ::= tv{
    binding : ref option[tipe].
    constraints : ref list[constraint].
  }

  public deRef:(tipe) => tipe.
  deRef(tVar(B,_)) where T^=B.binding! => deRef(T).
  deRef(tFun(B,_,_)) where T^=B.binding! => deRef(T).
  deRef(Tp) default => Tp.

  public newTypeVar:(string) => tipe.
  newTypeVar(Pre) => tVar(tv{binding := none. constraints := []. },genSym(Pre)).

  public newTypeFun:(string,integer) => tipe.
  newTypeFun(Pre,Ax) => tFun(tv{binding := none. constraints := []. },Ax,genSym(Pre)).

  public implementation display[tipe] => {.
    disp(T) => showType(T,false,10000)
  .}

  public showType:(tipe,boolean,integer) => ss.
  showType(T,Sh,Dp) => shTipe(deRef(T),Sh,Dp).

  shType:(tipe,boolean,integer) => ss.
  shTipe(voidType,_,_) => ss("void").
  shTipe(thisType,_,_) => ss("this").
  shTipe(kVar(Nm),_,_) => disp(Nm).
  shTipe(kFun(Nm,Ar),_,_) => ssSeq([ss(Nm),ss("/"),disp(Ar)]).
  shTipe(tVar(V,Nm),Sh,Dp) => ssSeq([showAllConstraints(V.constraints!,Dp),ss("%"),ss(Nm)]).
  shTipe(tFun(_,Ar,Nm),_,_) => ssSeq([ss("%"),ss(Nm),ss("/"),disp(Ar)]).
  shTipe(tipe(Nm),_,_) => ss(Nm).
  shTipe(tpFun(Nm,Ar),_,_) => ssSeq([ss(Nm),ss("/"),disp(Ar)]).
  shTipe(tpExp(F,A),Sh,Dp) => showTypeExp(F,A,Sh,Dp).
  shTipe(refType(T),Sh,Dp) => ssSeq([ss("ref "),showType(T,Sh,Dp)]).
  shTipe(tupleType(A),Sh,Dp) => ssSeq([ss("("),showTypes(A,Sh,Dp),ss(")")]).
  shTipe(allType(A,T),Sh,Dp) => ssSeq([ss("all "),showBound(A,Dp),..showMoreQuantified(T,Sh,Dp)]).
  shTipe(existType(A,T),Sh,Dp) => ssSeq([ss("exist "),showBound(A,Dp),..showMoreQuantified(T,Sh,Dp)]).
  shTipe(faceType(Els,Tps),Sh,Dp) => ssSeq([ss("{"),showTypeEls(Els,Tps,Sh,Dp),ss("}")]).
  shTipe(typeLambda(A,T),Sh,Dp) => ssSeq([showType(A,Sh,Dp),ss("~>"),showType(T,Sh,Dp)]).
  shTipe(typeExists(A,T),Sh,Dp) => ssSeq([showType(A,Sh,Dp),ss("<~"),showType(T,Sh,Dp)]).
  shTipe(constrainedType(T,C),Sh,Dp) => ssSeq([showConstraint(C,Dp),ss("|:"),showType(T,Sh,Dp)]).

  showTypes(_,_,0) => ss("...").
  showTypes(E,Sh,Dp) => ssSeq(showEls(E,Sh,Dp-1,"")).

  showEls([],_,_,_) => [].
  showEls([T,..Tps],Sh,Dp,Sep) => [ss(Sep),showType(T,Sh,Dp),..showEls(Tps,Sh,Dp,", ")].

  showTypeEls:(list[(string,tipe)],list[(string,tipe)],boolean,integer) => ss.
  showTypeEls(Els,Tps,Sh,Dp) =>
    ssSeq(interleave([ssSeq([ss(Nm),ss(":"),showType(Tp,Sh,Dp)]) | (Nm,Tp) in Els] ++
               [ssSeq([ss(Nm),ss("~>"),showType(Tp,Sh,Dp)]) | (Nm,Tp) in Tps],ss(". "))).

  showTypeExp:(tipe,tipe,boolean,integer) => ss.
  showTypeExp(tpExp(T,A),B,Sh,Dp) => ssSeq([showTypeExp(deRef(T),A,Sh,Dp),ss(","),showType(B,Sh,Dp)]).
  showTypeExp(tpFun(Nm,_),A,Sh,Dp) => ssSeq([disp(Nm),ss("["),showType(A,Sh,Dp)]).
  showTypeExp(kFun(Nm,_),A,Sh,Dp) => ssSeq([disp(Nm),ss("["),showType(A,Sh,Dp)]).
  showTypeExp(tFun(_,_,Nm),A,Sh,Dp) => ssSeq([disp(Nm),ss("["),showType(A,Sh,Dp)]).
  showTypeExp(T,A,Sh,Dp) => ssSeq([showType(T,Sh,Dp),ss("["),showType(A,Sh,Dp)]).

  showAllConstraints([],Dp) => ss("").
  showAllConstraints([C,..Cs],Dp) => ssSeq([showConstraint(C,Dp),..showMoreConstraints(Cs,Dp)]).

  showMoreConstraints([],_) => [ss("|:")].
  showMoreConstraints([C,..Cs],Dp) => [ss(", "),showConstraint(C,Dp),..showMoreConstraints(Cs,Dp)].

  showMoreQuantified(allType(V,T),Sh,Dp) => [ss(","),showBound(V,Dp),..showMoreQuantified(T,Sh,Dp)].
  showMoreQuantified(T,Sh,Dp) => [showType(T,Sh,Dp)].

  showBound(V,Dp) => showType(V,false,Dp).

  showConstraint(conConstraint(Nm,Args,[]),Dp) => ssSeq([disp(Nm),ss("["),showTypes(Args,false,Dp),ss("]")]).
  showConstraint(conConstraint(Nm,Args,Deps),Dp) => ssSeq([disp(Nm),ss("["),showTypes(Args,false,Dp),ss("->>"),showTypes(Deps,false,Dp),ss("]")]).
  showConstraint(implConstraint(Tp,Fc),Dp) => ssSeq([showType(Tp,false,Dp),ss("<~"),showType(Fc,false,Dp)]).

  public contract all c ~~ hasType[c] ::= {
    typeOf:(c)=>tipe.
  }

  public implementationName:(constraint) => string.
  implementationName(conConstraint(Nm,Args,_)) => Nm++surfaceNames(Args).

  surfaceNames(Tps) => _str_multicat(flatten(Tps//((T)=>surfaceName(deRef(T))))).

  surfaceName(tipe(Nm)) => ["!",Nm].
  surfaceName(tpExp(O,_)) => surfaceName(deRef(O)).
  surfaceName(kVar(Nm)) => ["!",Nm].
  surfaceName(kFun(Nm,_)) => ["!",Nm].
  surfaceName(tpFun(Nm,_)) => ["!",Nm].
  surfaceName(kVar(Nm)) => ["!",Nm].
  surfaceName(tVar(_,_)) => ["!_"].
  surfaceName(tFun(_,_,_)) => ["!_"].
  surfaceName(allType(_,T)) => surfaceName(deRef(T)).
  surfaceName(existType(_,T)) => surfaceName(deRef(T)).
  surfaceName(constrainedType(T,_)) => surfaceName(deRef(T)).
  surfaceName(typeLambda(_,T)) => surfaceName(deRef(T)).
  surfaceName(tupleType(A)) => ["!()\(size(A))"].

}
