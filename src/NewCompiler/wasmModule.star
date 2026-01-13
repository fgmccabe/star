star.compiler.wasm.module{
  import star.
  import star.compiler.wasm.types.
  import star.compiler.wasm.instr.

  public wasmDefn ::=
    .wasmGlob(string,value_type) |
    .wasmFunc(string,heap_type,map[string,value_type],wOp) |
    .wasmTypeDef(string,value_type).


}  
