star.compiler.wasm.types{
  import star.

  public typeIndex ~> string.

  public recType ::= .recType(cons[wTypeDef]).

  public wTypeDef ::= .wTypeDef(typeIndex,finality,cons[heap_type],compType).

  public finality ::= .final | .notFinal.

  public compType ::=
  | .Func(cons[value_type],cons[value_type])
  | .Stack(cons[value_type],value_type)
  | .Struct(cons[field_type])
  | .Array(field_type).
  
  public value_type ::=
  | .NumType(num_type)
  | .VecType(vec_type)
  | .RefType(ref_type).

  public field_type ::=
  | .fldTp(mutability,value_type)
  | .pkdFld(mutability,packed_type).

  public mutability ::= .const | .mutable.
  
  public packed_type ::= .i8 | .i16.
  
  public ref_type ~> (nullability,heap_type).

  public heap_type ::=
    .FuncTp |
    .NoFuncTp |
    .StackTp |
    .NoStackTp |
    .StructTp |
    .ArrayTp |
    .ExternTp |
    .NoExternTp |
    .i31RefTp |
    .EqTp |
    .AnyTp |
    .NoneTp |
    .NamedTp(typeIndex).

  public nullability ::= .nullable | .nonnull.

  public num_type ::= .IntType(int_type) | .FltType(flt_type).

  public int_type ::= .I32Type | .I64Type.
  public flt_type ::= .F32Type | .F64Type.

  public vec_type ::= .V128Type.

  -- Standard abbreviations

  public eqref = .RefType((.nullable,.EqTp)).
  public neqref = .RefType((.nonnull,.EqTp)).
  public i31ref = .RefType((.nullable,.i31RefTp)).
  public nullref = .RefType((.nullable,.NoneTp)).
  public funcref = .RefType((.nullable,.FuncTp)).
  public stackref = .RefType((.nullable,.StackTp)).

  public i32Type = .NumType(.IntType(.I32Type)).
  public i64Type = .NumType(.IntType(.I64Type)).

  public eqRefIndex = "eqref".

  public implementation display[recType] => {
    disp(.recType([])) => "".
    disp(.recType([D])) => disp(D).
    disp(.recType(L)) => "(rec #(interleave(L//disp,"\n  ")*) )".
  }

  public implementation display[wTypeDef] => {
    disp(.wTypeDef(Nm,Fi,Sups,Def)) => "(type \$$(Nm) $(Fi) #(showSups(Sups)) $(Def))"
  }

  public implementation display[finality] => {
    disp(.final) => "final".
    disp(.notFinal) => ""
  }

  showSups([]) => "".
  showSups(L) => "(sub #(interleave(L//disp," ")*))".

  public implementation display[value_type] => {
    disp(.NumType(IT)) => disp(IT).
    disp(.VecType(IT)) => disp(IT).
    disp(.RefType((.nullable,RT))) => "(ref null $(RT))".
    disp(.RefType((.nonnull,RT))) => "(ref $(RT))".
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
    disp(.FuncTp) => "func".
    disp(.NoFuncTp) => "nofunc".
    disp(.StackTp) => "stack".
    disp(.NoStackTp) => "nostack".
    disp(.StructTp) => "struct".
    disp(.ArrayTp) => "array".
    disp(.ExternTp) => "extern".
    disp(.NoExternTp) => "noextern".
    disp(.i31RefTp) => "i31ref".
    disp(.EqTp) => "eq".
    disp(.AnyTp) => "any".
    disp(.NoneTp) => "none".
    disp(.NamedTp(Nm)) => "\$$(Nm)".
  }

  public implementation display[compType] => {
    disp(.Func(A,R)) => "(func #((A//(a)=>"(param $(a))")*) (result $(R)))".
    disp(.Stack(A,R)) => "(stack #((A//(a)=>"(param $(a))")*) (result $(R)))".
    disp(.Struct(A)) => "(struct #((A//disp)*))".
    disp(.Array(A)) => "(array $(A))".
  }

  public implementation display[field_type] => {
    disp(.fldTp(M,T)) => "(field #(M==.const??""||"mutable") $(T))".
    disp(.pkdFld(M,T)) => "(packed field #(M==.const??""||"mutable") $(T))".
  }
  
  public implementation display[packed_type] => {
    disp(.i8) => "i8".
    disp(.i16) => "i16".
  }

  public implementation display[nullability] => {
    disp(.nullable) => "null?".
    disp(.nonnull) => "".
  }

  public implementation equality[nullability] => {
    .nullable==.nullable => .true.
    .nonnull==.nonnull => .true.
    _ == _ default => .true
  }

  public implementation equality[heap_type] => {
    .FuncTp == .FuncTp => .true.
    .NoFuncTp == .NoFuncTp => .true.
    .StackTp == .StackTp => .true.
    .NoStackTp == .NoStackTp => .true.
    .StructTp == .StructTp => .true.
    .ArrayTp == .ArrayTp => .true.
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
}
