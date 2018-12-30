star.compiler.dict{
  import star.

  import star.compiler.canon.
  import star.compiler.escapes.
  import star.compiler.location.
  import star.compiler.types.

  tpDef ::= tpDef(locn,tipe,tipe) |
    tpVar(locn,tipe).

  public vrEntry ::= vrEntry(option[locn],(locn,tipe)=>canon,tipe,()=>tipe).

  public scope ::= scope(map[string,tpDef],map[string,vrEntry],cons[constraint],map[string,map[string,constraint]]).
  public dict ~> cons[scope].

  
}
