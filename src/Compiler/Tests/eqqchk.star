test.comp.eqqchk{
  import star.
  import star.pkg.
  import star.repo.

  import test.comp.strrepo.

  import star.compiler.location.
  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.checker.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.impawt.
  import star.compiler.parser.
  import star.compiler.types.
  import star.compiler.terms.

  -- deeper check of resolver

  R0 = reports([]).
  lc = startLoc("test").

  getAst((some(A),_)) => A.

  S0 = """
star.core{
    -- special core to use in emergency	-- 
    public boolean ::= true | false.

    public option[e] ::= none | some(e).

    public contract equality[t] ::= {
      (==):(t,t)=>boolean.
    }

    public implementation equality[integer] => {
      I1 == I2 => _int_eq(I1,I2)
    }
  }
  """.

  addSpec:(pkgSpec,strRepo) => strRepo.
  addSpec(Spec,R) where pkgSpec(Pkg,_,_,_,_) .= Spec => addSigToRep(R,Pkg,(Spec::term)::string).

  A0 = getAst(parseText(lc,S0,R0)).

  SR1 = valof{
    RP0 = strRepo([]);
    (Sp,_,_) = checkPkg(RP0,A0,stdDict,R0);
    showMsg("spec = $(Sp::term)");
    valis addSpec(Sp,RP0)
  }

  show disp(SR1).

  S1 = """
test{
  import star.core.

  public cons[e] ::= nil | cons(e,cons[e]).

  public implementation all e ~~ equality[e] |: equality[cons[e]] => let{.
    public consEq:(cons[e],cons[e])=>boolean.
    consEq(nil,nil) => true.
    consEq(cons(E1,L1),cons(E2,L2)) => E1==E2 && consEq(L1,L2).
  } in {
    L1==L2 => consEq(L1,L2)
  }

  assert cons(1,cons(3,nil)) == cons(1,cons(3,nil))
}
  """.

  A1 = getAst(parseText(lc,S1,R0)).

  P1 = checkPkg(SR1,A1,stdDict,R0).

  show disp(P1).
}
