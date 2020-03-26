star.compiler.peephole{
  import star.
  import star.pkg.

  import star.compiler.assem.
  import star.compiler.errors.
  import star.compiler.misc.

  import star.compiler.location.
  import star.compiler.terms.

  implementation equality[assemLbl] => {.
    al(L1) == al(L2) => L1==L2.
  .}

  public peep:(cons[assemOp])=>cons[assemOp].
  peep([]) => [].
  peep([iCase(Cx),..Code]) => [iCase(Cx),..copyPeep(Cx,Code)].
  peep([iJmp(Lb),iLbl(Lb),..Code]) => peep([iLbl(Lb),..Code]).
  peep([Ins,..Code]) => [Ins,..peep(Code)].

  copyPeep(0,Code) => peep(Code).
  copyPeep(Cx,[iJmp(Lb),..Code]) => [iJmp(Lb),..copyPeep(Cx-1,Code)].
}
