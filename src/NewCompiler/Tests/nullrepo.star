test.comp.nullrepo{
  import star.
  import star.repo.

  public nullRepo ::= nullRepo.

  public implementation repo[nullRepo] => {
    hasSignature(_,_) => none.
    hasCode(_,_) => none.
  }

  public implementation display[nullRepo] => {
    disp(nullRepo) => ss("nullRepo").
  }
}
