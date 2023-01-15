test.comp.pkgchk{
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

  R0 = reports([]).
  Pk = pkg("test",defltVersion).
  lc = pkgLoc(Pk).

  getAst((some(A),_)) => A.

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

    public contract arith[t] ::= {
      (+):(t,t)=>t.
      (-):(t,t)=>t.
      (*):(t,t)=>t.
    }
 
    public implementation arith[integer] => {
      X+Y => _int_plus(X,Y).
      X-Y => _int_minus(X,Y).
      X*Y => _int_times(X,Y).
    }    
  }
  """.

  addSpec:(pkgSpec,strRepo) => strRepo.
  addSpec(Spec,R) where pkgSpec(Pkg,_,_,_,_) .= Spec => addSigToRep(R,Pkg,(Spec::term)::string).

  A0 = getAst(parseText(lc,S0,R0)).

  SR1 = valof{
    RP0 = strRepo([]);
    (Sp,_,_) = checkPkg(RP0,A0,stdDict,R0);
  logMsg("spec = $(Sp::term)");
    valis addSpec(Sp,RP0)
  }

  show disp(SR1).

  S1 = """
test{
  import star.core.
  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>N*fact(N-1).

  double:all x ~~ arith[x] |: (x)=>x.
  double(X) => X+X.
}
  """.

  A1 = getAst(parseText(lc,S1,R0)).

  P1 = checkPkg(SR1,A1,stdDict,R0).

  show disp(P1).
}
