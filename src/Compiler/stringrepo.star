test.comp.strrepo{
  import star.
  import star.pkg.
  import star.repo.

  public strRepo ::= strRepo(map[string,strEntry]).

  strEntry ::= sigEntry(string).

  public implementation repo[strRepo] => {
    hasSignature(strRepo(M),pkg(P,_)) where sigEntry(Sig)^=M[P] => some(Sig).
    hasSignature(_,_) default => none.
    hasCode(_,_) => none.
  }

  public implementation display[strRepo] => let{
    dispRepo(strRepo(M)) => ssSeq([ss("strRepo"),disp(M)]).
  } in {
    disp = dispRepo
  }

  implementation display[strEntry] => {
    disp(sigEntry(Sig)) => ssSeq([ss("signature: "),ss(Sig)])
  }

  public implementation coercion[map[string,string],strRepo] => {
    _coerce(M) => strRepo(M///(Ky,Sg)=>sigEntry(Sg))
  }

  public addSigToRep:(strRepo,pkg,string) => strRepo.
  addSigToRep(strRepo(M),pkg(Pk,_),Sig) =>
    strRepo(M[Pk->sigEntry(Sig)]).
}
