star.compiler.wasm.types{
  import star.

  public wasmTypeDefn ::= .subTp(integer,boolean,cons[string],heap_type).

  public value_type ::=
    .NumType(num_type)
    | .VecType(vec_type)
    | .RefType(string)
    | .RefNulType(string).

  public heap_type ::=
    .FuncTp(cons[value_type],cons[value_type]) |
    .NoFuncTp |
    .ContTp(cons[value_type],cons[value_type]) |
    .NoContTp |
    .StructTp(cons[(string,field_type)]) |
    .ArrayTp(field_type) |
    .ExternTp |
    .NoExternTp |
    .i31RefTp |
    .EqTp |
    .AnyTp |
    .NoneTp.

  public field_type ::= .fldTp(mutability,value_type) |
  .pkdFld(mutability,packed_type).

  public packed_type ::= .i8 | .i16.

  public mutability ::= .fixed | .mutable.

  public num_type ::= .I32Type | .I64Type | .F32Type | .F64Type.

  public vec_type ::= .V128Type.

  public implementation display[value_type] => {
    disp(.NumType(IT)) => disp(IT).
    disp(.VecType(IT)) => disp(IT).
    disp(.RefType(RT)) => "(ref $(RT))".
    disp(.RefNulType(RT)) => "(ref null $(RT))".
  }

  public implementation display[num_type] => {
    disp(.I32Type) => "i32".
    disp(.I64Type) => "i64".
    disp(.F32Type) => "f32".
    disp(.F64Type) => "f64".
  }

  public implementation display[vec_type] => {
    disp(.V128Type) => "i128".
  }

  public implementation display[heap_type] => {
    disp(.FuncTp(Args,Res)) => "(func (param #((Args//(A)=>disp(A))*)) (result #((Res//(A)=>disp(A))*)))".
    disp(.ContTp(Args,Res)) => "(cont (param #((Args//(A)=>disp(A))*)) (result #((Res//(A)=>disp(A))*)))".
    disp(.StructTp(Flds)) => "(struct #((Flds//((Nm,Tp))=> "(field \$#(Nm) $(Tp))")*))".
    disp(.ArrayTp(ATp)) => "(array $(ATp))".
    disp(.ExternTp) => "extern".
    disp(.i31RefTp) => "i31ref".
    disp(.AnyTp) => "any"
  }

  public implementation display[field_type] => {
    disp(.fldTp(M,T)) => "(field #(M==.fixed??""||"mutable") $(T))".
    disp(.pkdFld(M,T)) => "(field #(M==.fixed??""||"mutable") $(T))".
  }
  
  public implementation display[packed_type] => {
    disp(.i8) => "i8".
    disp(.i16) => "i16".
  }

  public implementation equality[heap_type] => {
    .FuncTp(A1,R1) == .FuncTp(A2,R2) => A1==A2 && R1==R2.
    .NoFuncTp == .NoFuncTp => .true.
    .ContTp(A1,R1) == .ContTp(A2,R2) => A1==A2 && R1==R2.
    .NoContTp == .NoContTp => .true.
    .StructTp(F1) == .StructTp(F2) => F1==F2.
    .ArrayTp(A1) == .ArrayTp(A2) => A1==A2.
    .ExternTp == .ExternTp => .true.
    .NoExternTp == .NoExternTp => .true.
    .i31RefTp == .i31RefTp => .true.
    .EqTp == .EqTp => .true.
    .AnyTp == .AnyTp => .true.
    .NoneTp == .NoneTp => .true.
    _ == _ default => .false.
  }

  public implementation equality[value_type] => {
    .NumType(N1) == .NumType(N2) => N1==N2.
    .VecType(V1) == .VecType(V2) => V1==V2.
    .RefType(R1) == .RefType(R2) => R1==R2.
    .RefNulType(R1) == .RefNulType(R2) => R1==R2.
    _ == _ default => .false.
  }

  public implementation equality[num_type] => {
    .I32Type == .I32Type => .true.
    .I64Type == .I64Type => .true.
    .F32Type == .F32Type => .true.
    .F64Type == .F64Type => .true.
    _ == _ default => .false.
  }

  public implementation equality[vec_type] => {
    .V128Type == .V128Type => .true.
    _ == _ default => .false.
  }

  public implementation equality[packed_type] => {
    .i8 == .i8 => .true.
    .i16 == .i16 => .true.
    _ == _ default => .false
  }

  public implementation equality[mutability] => {
    .fixed == .fixed => .true.
    .mutable == .mutable => .true.
    _ == _ default => .false
  }
  
  public implementation equality[field_type] => {
    .fldTp(M1,V1) == .fldTp(M2,V2) => M1==M2 && V1==V2.
    .pkdFld(M1,V1) == .pkdFld(M2,V2) => M1==M2 && V1==V2.
    _ == _ default => .false.
  }    
  
  public implementation equality[mutability] => {
    .fixed == .fixed => .true.
    .mutable == .mutable => .true.
    _ == _ default => .false
  }
}
