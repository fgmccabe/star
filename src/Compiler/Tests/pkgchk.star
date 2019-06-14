test.comp.pkgchk{
  import star.
  import star.pkg.
  import star.repo.

  import test.comp.nullrepo.
  import test.comp.strrepo.

  import star.compiler.location.
  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.checker.
  import star.compiler.dict.
  import star.compiler.dependencies.
  import star.compiler.errors.
  import star.compiler.meta.
  import star.compiler.parser.
  import star.compiler.types.

  R0 = reports([]).
  Pk = pkg("test",defltVersion).
  lc = pkgLoc(Pk).

  getAst((some(A),_)) => A.

  getBrEls(A) where (_,Els) ^= isBrTuple(A) => Els.

  S0 = """
star.core{
    -- special core to use in emergency	-- 
    public boolean ::= true | false.

    public contract equality[t] ::= {
      (==):(t,t)=>boolean.
    }
  }
""".

  A0 = getAst(parseText(lc,S0,R0)).

  show disp(A0).

  P0 = checkPkg(nullRepo,A0,stdDict,R0).

  show disp(P0).

  S1 = """
test{
  import star.core.
  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>_int_times(N,fact(_int_minus(N,1))).
}
  """.

  SR0 = addSigToRep(strRepo([]),
    pkg("star.core",defltVersion),
    """n5o5'()5'n2o2'pkg's'star.core's'1.0.0'n0o0'()0's"I{'true'C()t'star.core*boolean''false'C()t'star.core*boolean'}{'boolean'Yt'star.core*boolean'I{}{}}"n0o0'()0'n0o0'()0'""").

  A1 = getAst(parseText(lc,S1,R0)).

  show disp(A1).

  P1 = checkPkg(SR0,A1,stdDict,R0).

  show disp(P1).
  
}
