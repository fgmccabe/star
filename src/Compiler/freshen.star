star.compiler.freshen{
  import star.
  import star.iterable.

  import star.compiler.misc.
  import star.compiler.types.

  freshen:(tipe,(string)=>boolean,(string)=>option[tipe]) => (list[(string,tipe)],tipe).
  freshen(Tp,Ex,Env) where (T,Q) .= freshQuants(deRef(Tp),[]) =>
    (Q,frshn(T,Q,Ex,Env)).
  freshen(Tp,_,_) default => ([],Tp).

  freshQuants(allType(kVar(V),T),B) =>
    freshQuants(deRef(T),[(V,newTypeVar(V)),..B]).
  freshQuants(allType(kFun(V,Ar),T),B) =>
    freshQuants(deRef(T),[(V,newTypeFun(V,Ar)),..B]).
  freshQuants(existType(kVar(V),T),B) =>
    freshQuants(deRef(T),[(V,genSkolemFun(V,B)),..B]).
  freshQuants(T,B) default => (T,B).

  genSkolemFun(Nm,[]) => skolemVar(Nm).
  genSkolemFun(Nm,Q) => foldLeft((S,(_,V))=>tpExp(S,V),kFun(genSym(Nm),size(Q)),Q).

  skolemVar(Nm) => kVar(genSym(Nm)).

  frshn(voidType,_,_,_) => voidType.
  frshn(kVar(Nm),Q,Ex,Env) where Ex(Nm) => kVar(Nm).
  frshn(kVar(Nm),_,_,Env) where T^=Env(Nm) => T.
  frshn(kVar(Nm),Q,_,_) where (Nm,T) in Q => T.
  frshn(kVar(Nm),_,_,_) => kVar(Nm).

  frshn(kFun(Nm,Ar),Q,Ex,Env) where Ex(Nm) => kFun(Nm,Ar).
  frshn(kFun(Nm,Ar),_,_,Env) where T^=Env(Nm) => T.
  frshn(kFun(Nm,Ar),Q,_,_) where (Nm,T) in Q => T.
  frshn(kFun(Nm,Ar),_,_,_) => kFun(Nm,Ar).

  frshn(refType(T),Q,Ex,Env) => refType(frshn(deRef(T),Q,Ex,Env)).



}
