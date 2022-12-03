star.compiler.intrinsics{
  -- Automatically Generated Listing of intrinsics -- Do NOT Edit
  import star.

  import star.compiler.types.
  import star.compiler.assem.

  public intrinsic:(string) => option[(tipe,assemOp,boolean)].
  intrinsic(Es) => case Es in {
    "_int_plus" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iIAdd, .true).  -- add two integers
    "_int_minus" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iISub, .true).  -- subtract two integers
    "_int_times" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iIMul, .true).  -- multiply two integers
    "_int_div" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iIDiv, .true).  -- divide two integers
    "_int_mod" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iIMod, .true).  -- modulo remainder
    "_flt_plus" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),.iFAdd, .true).  -- add two floats
    "_flt_minus" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),.iFSub, .true).  -- subtract two floats
    "_flt_times" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),.iFMul, .true).  -- multiply two floats
    "_flt_div" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),.iFDiv, .true).  -- divide two floats
    "_flt_mod" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),fltType),.iFMod, .true).  -- modulo remainder
    "_int_abs" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType])),intType),.iIAbs, .true).  -- integer absolute value
    "_flt_abs" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([fltType])),fltType),.iFAbs, .true).  -- float absolute value
    "_int_eq" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),.iIEq, .false).  -- integer equality
    "_int_lt" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),.iILt, .false).  -- integer less than
    "_int_ge" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),boolType),.iIGe, .false).  -- integer greater or equal
    "_flt_eq" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType,fltType])),boolType),.iFEq, .false).  -- float equality
    "_flt_lt" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),boolType),.iFLt, .false).  -- float less than
    "_flt_ge" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([fltType,fltType])),boolType),.iFGe, .false).  -- float greater or equal
    "_band" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBAnd, .true).  -- bitwise and two integers
    "_bor" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBOr, .true).  -- bitwise or two integers
    "_bxor" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBXor, .true).  -- bitwise xor two integers
    "_blsl" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBLsl, .true).  -- logical left shift
    "_blsr" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBLsr, .true).  -- logical right shift
    "_basr" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType,intType])),intType),.iBAsr, .true).  -- arithmetic right shift
    "_bnot" => ? (tpExp(tpExp(tpFun("=>",2),tupleType([intType])),intType),.iBNot, .true).  -- bitwise negate number
    "_fiber_eq" => ? (allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("s")),nomnal("r")),tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("s")),nomnal("r"))])),boolType))),.iTEq, .true).  -- compare two fiber identifiers
    "_new_fiber" => ? (allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("r")),nomnal("s")),nomnal("r")])),nomnal("s"))])),tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("r")),nomnal("s"))))),.iFiber, .true).  -- create a new fiber
    "_spawn" => ? (allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("r")),nomnal("s"))])),nomnal("s"))])),nomnal("s")))),.iSpawn, .true).  -- spawn a new task
    "_suspend_fiber" => ? (allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("s")),nomnal("r")),nomnal("s")])),nomnal("r")))),.iSuspend, .true).  -- suspend a fiber
    "_retire_fiber" => ? (allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("s")),nomnal("r")),nomnal("s")])),tupleType([])))),.iRetire, .false).  -- retire a fiber
    "_resume_fiber" => ? (allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("s")),nomnal("r")),nomnal("r")])),nomnal("s")))),.iResume, .true).  -- resume a fiber
    "_cell" => ? (allType(nomnal("t"),tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("t")])),tpExp(tpFun("ref",1),nomnal("t")))),.iCell, .true).  -- create a reference cell
    "_get" => ? (allType(nomnal("t"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("ref",1),nomnal("t"))])),nomnal("t"))),.iGet, .false).  -- access contents of reference cell
    "_assign" => ? (allType(nomnal("t"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("ref",1),nomnal("t")),nomnal("t")])),tupleType([]))),.iAssign, .false).  -- update contents of reference cell

    _ default => .none.
  }
}
