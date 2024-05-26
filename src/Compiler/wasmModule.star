star.compiler.wasm.module{
  import star.
  import star.compiler.wasm.types.
  import star.compiler.wasm.instr.

  public wasmDefn ::=
    .wasmGlobDef(string,value_type) |
    .wasmFuncDef(string,heap_type,map[string,value_type],wOp) |
    .wasmTypeDef(string,value_type).


}  
