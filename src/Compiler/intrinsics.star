star.compiler.intrinsics{
  -- Automatically Generated Listing of intrinsics -- Do NOT Edit
  import star.

  import star.compiler.terms.
  import star.compiler.types.
  import star.compiler.assem.

  public intrinsic:(string) => option[(tipe,assemOp)].
  intrinsic("_int_eq") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),.iIEq)).  -- integer equality
  intrinsic("_int_lt") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),.iILt)).  -- integer less than
  intrinsic("_int_ge") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),.iIGe)).  -- integer greater or equal
  intrinsic("_flt_eq") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType,fltType])),boolType),.iFEq)).  -- float equality
  intrinsic("_flt_lt") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),boolType),.iFLt)).  -- float less than
  intrinsic("_flt_ge") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),boolType),.iFGe)).  -- float greater or equal

  intrinsic(_) default => .none.
}
