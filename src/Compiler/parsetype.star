star.compiler.typeparse{
  import star.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.types.
  import star.compiler.unify.
  import star.compiler.wff.

  public parseType:(ast,dict,reports) => either[reports,tipe].

  parseType(Tp,Env,Rp) => let{
    parseT(Tp,C) where (Lc,V,BT) ^= isQuantified(Tp) => .
    parseT(Tp,C) where (Lc,Nm) ^= isName(Tp) =>
      parseTypeName(Lc,Nm,C).
    parseT(Tp,C) where (Lc,O,Args) ^= isSquareTerm(Tp) => do{
      (Op,C1) <- parseT(O,C);
      (ArgTps,C2) <- parseTps(Args,C1);
      if (Qx,OOp) .= freshen(Op,[],Env) then {
        return rebind(Qx,wrapConstraints(C2,applyTypeFun(OOp,ArgTps,Env)))
      } else
        raise reportError(Rp,"Could not freshen $(Op)",Lc)
    }

  } in
}


}
