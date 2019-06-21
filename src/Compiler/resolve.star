star.compiler.resolve{
  import star.

  import star.compiler.canon.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.freshen.
  import star.compiler.unify.

  public overload:(list[implSpec],list[canonDef],reports) => either[reports,list[canonDef]].
  overload(Specs,Defs,Rp) => overloadDefs(Specs,Defs,Rp).

  overloadDefs:(list[implSpec],list[canonDef],reports) => either[reports,list[canonDef]].
  overloadDefs(Impls,Defs,Rp) => either([]).
}
  
