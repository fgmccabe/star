star.compiler.wasm.type{
  import star.
  import star.compiler.types.
  import star.compiler.wasm.types.

  -- Project wasm types from Star types
  wasmTp:(tipe,map[string,wasmTypeDefn])=>(value_type,map[string,wasmTypeDefn]).
  wasmTp(Tp,Mp) => case deRef(Tp) in {
    .voidType => (.RefType("void"),Mp).
    .kFun(_,_) => (.RefType("any"),Mp).
    .tVar(_,_) => (.RefType("any"),Mp).
    .tFun(_,_,_) => (.RefType("any"),Mp).
    .nomnal(Nm) => 
    .tpFun(string,integer) |
    .tpExp(tipe,tipe) |
    .tupleType(cons[tipe]) |
    .allType(tipe,tipe) |
    .existType(tipe,tipe) |
    .faceType(cons[(string,tipe)],cons[(string,typeRule)]) |
    .constrainedType(Tp,_) => 
    
  }
  
}
