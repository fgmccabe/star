star.compiler.intrinsics{
  -- Automatically Generated Listing of intrinsics -- Do NOT Edit
  import star.

  import star.compiler.data.
  import star.compiler.types.
  import star.compiler.assem.

  public intrinsic:(string) => option[(tipe,assemOp)].
  intrinsic(Es) => case Es in {
    "_int_plus" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iIAdd)).  -- add two integers
    "_int_minus" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iISub)).  -- subtract two integers
    "_int_times" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iIMul)).  -- multiply two integers
    "_int_div" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iIDiv)).  -- divide two integers
    "_int_mod" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iIMod)).  -- modulo remainder
    "_flt_plus" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),.iFAdd)).  -- add two floats
    "_flt_minus" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),.iFSub)).  -- subtract two floats
    "_flt_times" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),.iFMul)).  -- multiply two floats
    "_flt_div" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),.iFDiv)).  -- divide two floats
    "_flt_mod" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),.iFMod)).  -- modulo remainder
    "_int_abs" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType])),intType),.iIAbs)).  -- integer absolute value
    "_flt_abs" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType])),fltType),.iFAbs)).  -- float absolute value
    "_int_eq" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),.iIEq)).  -- integer equality
    "_int_lt" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),.iILt)).  -- integer less than
    "_int_ge" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),.iIGe)).  -- integer greater or equal
    "_flt_eq" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType,fltType])),boolType),.iFEq)).  -- float equality
    "_flt_lt" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),boolType),.iFLt)).  -- float less than
    "_flt_ge" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),boolType),.iFGe)).  -- float greater or equal
    "_band" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBAnd)).  -- bitwise and two integers
    "_bor" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBOr)).  -- bitwise or two integers
    "_bxor" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBXor)).  -- bitwise xor two integers
    "_blsl" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBLsl)).  -- logical left shift
    "_blsr" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBLsr)).  -- logical right shift
    "_basr" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBAsr)).  -- arithmetic right shift
    "_bnot" => .some((tpExp(tpExp(tpFun("=>",2),tupleType([intType])),intType),.iBNot)).  -- bitwise negate number
    "_fiber_eq" => .some((allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("star.core*fiber",2),nomnal("s")),nomnal("r"),tpExp(tpFun("star.core*fiber",2),nomnal("s")),nomnal("r")])),boolType))),.iTEq)).  -- compare two fiber identifiers
    "_new_fiber" => .some((allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("star.core*fiber",2),nomnal("s")),nomnal("r")])),nomnal("s"))])),tpExp(tpFun("star.core*fiber",2),nomnal("s"))))),.iFiber)).  -- create a new fiber
    "_suspend_fiber" => .some((allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("star.core*fiber",2),nomnal("s")),nomnal("r"),nomnal("s")])),nomnal("r")))),.iSuspend)).  -- suspend a fiber
    "_retire_fiber" => .some((allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("star.core*fiber",2),nomnal("s")),nomnal("r"),nomnal("s")])),tupleType([])))),.iRetire)).  -- retire a fiber
    "_resume_fiber" => .some((allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("star.core*fiber",2),nomnal("s")),nomnal("r"),nomnal("r")])),nomnal("s")))),.iResume)).  -- resume a fiber
    "_cell" => .some((allType(nomnal("t"),tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("t")])),tpExp(tpFun("ref",1),nomnal("t")))),.iCell)).  -- create a reference cell
    "_get" => .some((allType(nomnal("t"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("ref",1),nomnal("t"))])),nomnal("t"))),.iGet)).  -- access contents of reference cell
    "_assign" => .some((allType(nomnal("t"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("ref",1),nomnal("t")),nomnal("t")])),tupleType([]))),.iAssign)).  -- update contents of reference cell

    _ default => .none.
  }
}
