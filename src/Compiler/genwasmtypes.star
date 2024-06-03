star.compiler.wasm.gentypes{
  import star.
  import star.topsort.

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

    Defined = foldRight((D,S)=>S\+definedName(D),[],Defs);
    Q = foldRight(pickVar,[],Defined);
    AllRefs = foldRight((D,A) => [findRefs(D,D,Q,Defined),..A],([]:cons[defSpec]),Defs);
  

  implementation depends[defSpec->>defnSp] => {
    references(.defSpec(_,Refs,_)) => Refs.
    defined(.defSpec(Sp,_,_),Rf) => Sp==Rf.
  }

  


}
    
    

  
