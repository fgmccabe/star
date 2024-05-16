star.compiler.wasm.types{
  import star.

  public typeIndex ~> string.

  public wasmTypeDefn ::= .subTp(integer,boolean,cons[string],heap_type).

  public value_type ::=
    .NumType(num_type)
    | .VecType(vec_type)
    | .RefType(typeIndex)
    | .RefNulType(typeIndex).

  public heap_type ::=
    .FuncTp(cons[value_type],cons[value_type]) |
    .NoFuncTp |
    .StackTp(cons[value_type],value_type) |
    .NoStackTp |
    .StructTp(cons[(string,field_type)]) |
    .ArrayTp(field_type) |
    .ExternTp |
    .NoExternTp |
    .i31RefTp |
    .EqTp |
    .AnyTp |
    .NoneTp.

  public ref_type ~> (nullability,heap_type).

  public field_type ::= .fldTp(mutability,value_type) |
  .pkdFld(mutability,packed_type).

  public packed_type ::= .i8 | .i16.

  public mutability ::= .const | .mutable.

  public nullability ::= .nullable | .nonnull.

  public num_type ::= .IntType(int_type) | .FltType(flt_type).

  public int_type ::= .I32Type | .I64Type.
  public flt_type ::= .F32Type | .F64Type.

  public vec_type ::= .V128Type.

  public implementation display[value_type] => {
    disp(.NumType(IT)) => disp(IT).
    disp(.VecType(IT)) => disp(IT).
    disp(.RefType(RT)) => "(ref $(RT))".
    disp(.RefNulType(RT)) => "(ref null $(RT))".
  }

  public implementation display[num_type] => {
    disp(.IntType(Tp)) => disp(Tp).
    disp(.FltType(Tp)) => disp(Tp).
  }

  public implementation display[int_type] => {
    disp(.I32Type) => "i32".
    disp(.I64Type) => "i64".
  }

  public implementation display[flt_type] => {
    disp(.F32Type) => "f32".
    disp(.F64Type) => "f64".
  }

  public implementation display[vec_type] => {
    disp(.V128Type) => "i128".
  }

  public implementation display[heap_type] => {
    disp(.FuncTp(Args,Res)) => "(func (param #((Args//(A)=>disp(A))*)) (result #((Res//(A)=>disp(A))*)))".
    disp(.StackTp(Args,Res)) => "(stack (param #((Args//(A)=>disp(A))*)) $(Res))".
    disp(.StructTp(Flds)) => "(struct #((Flds//((Nm,Tp))=> "(field \$#(Nm) $(Tp))")*))".
    disp(.ArrayTp(ATp)) => "(array $(ATp))".
    disp(.ExternTp) => "extern".
    disp(.i31RefTp) => "i31ref".
    disp(.AnyTp) => "any"
  }

  public implementation display[field_type] => {
    disp(.fldTp(M,T)) => "(field #(M==.const??""||"mutable") $(T))".
    disp(.pkdFld(M,T)) => "(field #(M==.const??""||"mutable") $(T))".
  }
  
  public implementation display[packed_type] => {
    disp(.i8) => "i8".
    disp(.i16) => "i16".
  }

  public implementation display[nullability] => {
    disp(.nullable) => "null?".
    disp(.nonnull) => "".
  }

  public implementation equality[heap_type] => {
    .FuncTp(A1,R1) == .FuncTp(A2,R2) => A1==A2 && R1==R2.
    .NoFuncTp == .NoFuncTp => .true.
    .StackTp(A1,R1) == .StackTp(A2,R2) => A1==A2 && R1==R2.
    .NoStackTp == .NoStackTp => .true.
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
    .IntType(T1) == .IntType(T2) => T1==T2.
    .FltType(T1) == .FltType(T2) => T1==T2.
    _ == _ default => .false.
  }

  public implementation equality[int_type] => {
    .I32Type == .I32Type => .true.
    .I64Type == .I64Type => .true.
    _ == _ default => .false.
  }

  public implementation equality[flt_type] => {
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
    .const == .const => .true.
    .mutable == .mutable => .true.
    _ == _ default => .false
  }
  
  public implementation equality[field_type] => {
    .fldTp(M1,V1) == .fldTp(M2,V2) => M1==M2 && V1==V2.
    .pkdFld(M1,V1) == .pkdFld(M2,V2) => M1==M2 && V1==V2.
    _ == _ default => .false.
  }    
  
  public implementation equality[mutability] => {
    .const == .const => .true.
    .mutable == .mutable => .true.
    _ == _ default => .false
  }
}
