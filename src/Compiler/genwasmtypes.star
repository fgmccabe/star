star.compiler.wasm.gentypes{
  import star.

  import star.compiler.meta.
  import star.compiler.types.
  import star.compiler.wasm.types.

  public buildWasmTypeMap:(cons[cDefn]) => map[string,wasmTypeDefn].
  buildWasmTypeMap(Defs) => bildMap(Defs,[]).

  bildMap([],Map) => Map.
  bildMap([Def,..Defs],Map) =>
    bildMap(Defs,bildEntry(Def,Map)).

  bildEntry(.tpDef(Lc,Tp,Rl,Cons),Map) => valof{
    tpNm = dlrName(tpName(Tp)); -- $tpName is the text format name of the type
    if _ ?= Map[tpNm] && isEmpty(Cons) then
      valis Map
    else{

    }
  }


}
    
    

  
