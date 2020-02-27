star.compiler.intrinsics{
  -- Automatically Generated Listing of intrinsics -- Do NOT Edit
  import star.

  import star.compiler.terms.
  import star.compiler.types.
  import star.compiler.assem.

  public intrinsic:(string) => option[(tipe,assemOp)].
  intrinsic("_int_plus") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),iIAdd)).  -- add two integers
  intrinsic("_int_minus") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),iISub)).  -- subtract two integers
  intrinsic("_int_times") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),iIMul)).  -- multiply two integers
  intrinsic("_int_div") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),iIDiv)).  -- divide two integers
  intrinsic("_int_mod") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),iIMod)).  -- modulo remainder
  intrinsic("_flt_plus") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),iFAdd)).  -- add two floats
  intrinsic("_flt_minus") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),iFSub)).  -- subtract two floats
  intrinsic("_flt_times") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),iFMul)).  -- multiply two floats
  intrinsic("_flt_div") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),iFDiv)).  -- divide two floats
  intrinsic("_flt_mod") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),iFMod)).  -- modulo remainder
  intrinsic("_int_abs") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType])),intType),iIAbs)).  -- integer absolute value
  intrinsic("_flt_abs") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType])),fltType),iFAbs)).  -- float absolute value
  intrinsic("_int_eq") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),iIEq)).  -- integer equality
  intrinsic("_int_lt") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),iILt)).  -- integer less than
  intrinsic("_int_ge") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),iIGe)).  -- integer greater or equal
  intrinsic("_flt_eq") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType,fltType])),boolType),iFEq)).  -- float equality
  intrinsic("_flt_lt") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),boolType),iFLt)).  -- float less than
  intrinsic("_flt_ge") => some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),boolType),iFGe)).  -- float greater or equal
  intrinsic("_band") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),iBAnd)).  -- bitwise and two integers
  intrinsic("_bor") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),iBOr)).  -- bitwise or two integers
  intrinsic("_bxor") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),iBXor)).  -- bitwise xor two integers
  intrinsic("_blsl") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),iBLsl)).  -- logical left shift
  intrinsic("_blsr") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),iBLsr)).  -- logical right shift
  intrinsic("_basr") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),iBAsr)).  -- arithmetic right shift
  intrinsic("_bnot") => some((tpExp(tpExp(tpFun("=>",2),tupleType([intType])),intType),iBNot)).  -- bitwise negate number

  intrinsic(_) default => none.
}
