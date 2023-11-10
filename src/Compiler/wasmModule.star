star.compiler.wasm.module{
  import star.
  import star.compiler.wasm.types.
  import star.compiler.wasm.instr.

  public wasmDefn ::=
    .wasmGlobal(string,value_type) |
    .wasmFunction(string,block_type) |
    .wasmType(string,value_type).

}  
