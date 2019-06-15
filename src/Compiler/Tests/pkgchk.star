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
  import star.compiler.impawt.
  import star.compiler.meta.
  import star.compiler.parser.
  import star.compiler.types.
  import star.compiler.terms.

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

  public implementation all t ~~ equality[boolean] => {
    true == true => true.
    false == false => true.
    _ == _ default => false.
  }
  }
  """.

  addSpec:(pkgSpec,strRepo) => strRepo.
  addSpec(Spec,R) where pkgSpec(Pkg,_,_,_,_) .= Spec => addSigToRep(R,Pkg,(Spec::term)::string).

  A0 = getAst(parseText(lc,S0,R0)).

  show disp(A0).

  
  SR1 = valof do{
    RP0 = strRepo([]);
    (Sp,_,_) <- checkPkg(RP0,A0,stdDict,R0);
  logMsg("spec = $(Sp::term)");
    valis addSpec(Sp,RP0)
  }

  show disp(SR1).

  S1 = """
test{
  import star.core.
  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>_int_times(N,fact(_int_minus(N,1))).
}
  """.

  A1 = getAst(parseText(lc,S1,R0)).

  show disp(A1).

  P1 = checkPkg(SR1,A1,stdDict,R0).

  show disp(P1).
}
