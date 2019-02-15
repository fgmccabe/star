star.compiler.wff{
  import star.

  import star.compiler.ast.
  import star.compiler.misc.

  public isQuantified:(ast)=>option[(locn,list[ast],ast)].
  isQuantified(T) where (Lc,Lh,B)^=isBinary(T,"~~") &&
                        (_,V)^=isUnary(Lh,"all") => some((Lc,deComma(V),B)).


  deComma:(ast) => list[ast].
  deComma(Trm) => let{
    deC(T,SoF) where (_,Lh,Rh)^=isBinary(T,",") =>
      deC(Rh,deC(Lh,SoF))
    deC(T,SoF) => [T,..SoF].
  } in deC(Trm,[]).

}
