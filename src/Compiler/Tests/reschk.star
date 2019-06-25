test.comp.reschk{
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
  Pk = pkg("test",defltVersion).
  lc = pkgLoc(Pk).

  getAst((some(A),_)) => A.

  S0 = """
star.core{
    -- special core to use in emergency	-- 
    public boolean ::= true | false.

    public option[e] ::= none | some(e).

    public contract equality[t] ::= {
      (==):(t,t)=>boolean.
    }

    public contract all S,E ~~ stream[S ->> E] ::= {
      _eof:(S) => boolean.
      _hdtl:(S) => option[(E,S)].
    }

    public contract all S,E ~~ sequence[S->>E] ::= {
      _cons:(E,S) => S.
      _nil:S.
    }
  }
  """.

  addSpec:(pkgSpec,strRepo) => strRepo.
  addSpec(Spec,R) where pkgSpec(Pkg,_,_,_,_) .= Spec => addSigToRep(R,Pkg,(Spec::term)::string).

  A0 = getAst(parseText(lc,S0,R0)).

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

  public cons[e] ::= nil | cons(e,cons[e]).

  implementation all e ~~ stream[cons[e]->>e] => {
    _eof(nil) => true.
    _eof(_) default => false.

    _hdtl(cons(H,T)) => some((H,T)).
  }

  implementation all x ~~ sequence[cons[x] ->> x] => {
    _cons(E,S) => cons(E,S).
    _nil = nil.
  }

  public conc:all e ~~ stream[e] |: (e,e)=>e.
  conc([],T) => T.
  conc([E,..Es],T) => [E,..conc(Es,T)].
}
  """.

  A1 = getAst(parseText(lc,S1,R0)).

  P1 = checkPkg(SR1,A1,stdDict,R0).

  show disp(P1).
}
