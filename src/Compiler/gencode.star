star.compiler.gencode{
  import star.
  import star.pkg.

  import star.compiler.assem.
  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.terms.

  label ::= lbl(string) | ulbl(integer).

  srcLoc ::= lclVar(integer,label,label).

  codeCtx ::= codeCtx(map[string,srcLoc]).

  contin ~> (list[assemOp],codeCtx,reports,compilerOptions)=>either[reports,(list[assemOp],codeCtx)].

  genExp:(crExp,list[assemOp],codeCtx,contin,reports,compilerOptions)=> either[reports,(list[assemOp],codeCtx)].
  genExp(

  

}
