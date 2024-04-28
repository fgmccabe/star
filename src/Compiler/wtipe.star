star.compiler.wasm.type{
  import star.
  import star.compiler.meta.
  import star.compiler.types.
  import star.compiler.wasm.types.

  -- Project wasm types from Star types
  projectTp:(tipe,map[string,wasmTypeDefn])=>(value_type,map[string,wasmTypeDefn]).
  projectTp(Tp,Mp) => case deRef(Tp) in {
    | .voidType => (.RefType("void"),Mp)
    | .kFun(_,_) => (.RefType("any"),Mp)
    | .tVar(_,_) => (.RefType("any"),Mp)
    | .tFun(_,_,_) => (.RefType("any"),Mp)
    | .nomnal(Nm) => (.RefType(Nm),Mp)
    | .tpFun(Nm,_) => (.RefType(Nm),Mp)
    | .tpExp(Op,_) => projectTp(deRef(Op),Mp)
    | .tupleType(Els) => findTupleType(Els,Mp)
    | .allType(_,B) => projectTp(deRef(B),Mp)
    | .existType(_,B) => projectTp(deRef(B),Mp)
    | .faceType(Els,_) => findTupleType(Els//snd,Mp)
    | .constrainedType(T,_) => projectTp(deRef(T),Mp)
  }

  collectConstructors:(cons[decl],map[string,wasmTypeDefn]) => map[string,wasmTypeDefn].
  collectConstructors(Decls,Mp) => foldLeft(collectConstructor,Mp,Decls).

  collectConstructor(tpeDec(Lc,Nm,Tp,Rl),Mp) 
  
}


















