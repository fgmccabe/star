star.compiler.typeparse{
  import star.

  import star.compiler.ast.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.location.
  import star.compiler.types.
  import star.compiler.unify.
  import star.compiler.wff.

  public parseType:(ast,dict,reports) => either[reports,tipe].

  parseType(Tp,Env,Rp) => let{
    parseT(Tp,Q,C) where (Lc,V,BT) ^= isQuantified(Tp) => do{
      BV <- parseBoundTpVars(V);
      In <- parseT(BT,Q++BV,C);
      lift reQuant(BV,In)
    }. 
    parseT(Tp,Q,C) where (Lc,Nm) ^= isName(Tp) =>
      parseTypeName(Lc,Nm,C).
    parseT(Tp,Q,C) where (Lc,O,Args) ^= isSquareTerm(Tp) => do{
      (Op,C1) <- parseT(O,C);
      (ArgTps,C2) <- parseTps(Args,C1);
      if (Qx,OOp) .= freshen(Op,[],Env) then {
        return rebind(Qx,wrapConstraints(C2,applyTypeFun(OOp,ArgTps,Env)))
      } else
        throw reportError(Rp,"Could not freshen $(Op)",Lc)
    }

    parseBoundTpVars:(cons[ast])=>either[reports,cons[(string,tipe)]].
    parseBoundTpVars([]) => either(nil).
    parseBoundTpVars([V,..R]) =>
      parseBoundTpVars(R) >>= (L) =>
      parseBoundTpVar(V) >>= (Vr) => return cons(Vr,L).
    
    parseBoundTpVar(Nm) where (_,Id) ^= isName(Nm) => either((Id,newTypeVar(Id))).
    parseBoundTpVar(FNm) where
	(_,Lhs,Rhs) ^= isBinary(FNm,"/") &&
    (_,Id) ^= isName(Lhs) &&
    (_,Ar) ^= isInt(Rhs) => either((Id,newTypeFun(Id,Ar))).
    parseBoundTpVar(O) default =>
      other(reportError(Rp,"invalid bound type variable $(O)",locOf(O))).
  } in parseT(Tp,[],Rp).

  reQuant:(cons[(string,tipe)],tipe) => tipe.
  reQuant(nil,Tp) => Tp.
  reQuant(cons((_,KV),T),Tp) => reQuant(T,allType(KV,Tp)).


}
