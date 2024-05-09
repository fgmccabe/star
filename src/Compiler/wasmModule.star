star.compiler.wasm.module{
  import star.
  import star.compiler.wasm.types.
  import star.compiler.wasm.instr.

  public wasmDefn ::=
    .wasmGlobal(string,value_type) |
    .wasmFunction(string,heap_type,map[string,value_type],wOp) |
    .wasmType(string,value_type).

  public typeIndex ~> integer.

}  
