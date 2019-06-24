star.compiler.query{
  import star.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.freshen.
  import star.compiler.unify.

  -- Implement various source transformations that implement queries

  lifter ~> (liftState[canon])=>liftState[canon].

  liftState ::= lifted(canon) | unlifted(canon).
  
  genCondition:(canon.string,lifter,lifter,lifter,canon,reports) =>
    either[reports,canon].
  genCondition(search(Lc,Ptn,Src,Iterator),Path,Lift,_Seq,Succ,Initial) =>
