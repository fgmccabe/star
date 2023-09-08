star.compiler.intrinsics{
  -- Automatically Generated Listing of intrinsics -- Do NOT Edit
  import star.

  import star.compiler.types.
  import star.compiler.assem.

  public tailMode ::= .noMore | .notLast.

  public intrinsic:(string) => option[(tipe,assemOp,boolean,tailMode)].
  intrinsic(Es) => case Es in {
    "_abort" => .some((.allType(.nomnal("a"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("a"),strType])),.tupleType([]))),.iAbort, .false, .noMore)).  -- abort process
    "_int_plus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iIAdd, .true, .notLast)).  -- add two integers
    "_int_minus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iISub, .true, .notLast)).  -- subtract two integers
    "_int_times" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iIMul, .true, .notLast)).  -- multiply two integers
    "_int_div" => .some((.constrainedType(.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.raisEs(.nomnal("star.core*errorCode"))),.iIDiv, .true, .notLast)).  -- divide two integers
    "_int_mod" => .some((.constrainedType(.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.raisEs(.nomnal("star.core*errorCode"))),.iIMod, .true, .notLast)).  -- modulo remainder
    "_flt_plus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),.iFAdd, .true, .notLast)).  -- add two floats
    "_flt_minus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),.iFSub, .true, .notLast)).  -- subtract two floats
    "_flt_times" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),.iFMul, .true, .notLast)).  -- multiply two floats
    "_flt_div" => .some((.constrainedType(.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),.raisEs(.nomnal("star.core*errorCode"))),.iFDiv, .true, .notLast)).  -- divide two floats
    "_flt_mod" => .some((.constrainedType(.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),.raisEs(.nomnal("star.core*errorCode"))),.iFMod, .true, .notLast)).  -- modulo remainder
    "_int_abs" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType])),intType),.iIAbs, .true, .notLast)).  -- integer absolute value
    "_flt_abs" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType])),fltType),.iFAbs, .true, .notLast)).  -- float absolute value
    "_int_eq" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),.iIEq, .false, .notLast)).  -- integer equality
    "_int_lt" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),.iILt, .false, .notLast)).  -- integer less than
    "_int_ge" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),.iIGe, .false, .notLast)).  -- integer greater or equal
    "_chr_eq" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([chrType,chrType])),boolType),.iCEq, .false, .notLast)).  -- character equality
    "_chr_lt" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([chrType,chrType])),boolType),.iCLt, .false, .notLast)).  -- character less than
    "_chr_ge" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([chrType,chrType])),boolType),.iCGe, .false, .notLast)).  -- character greater or equal
    "_flt_eq" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),boolType),.iFEq, .false, .notLast)).  -- float equality
    "_flt_lt" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),boolType),.iFLt, .false, .notLast)).  -- float less than
    "_flt_ge" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),boolType),.iFGe, .false, .notLast)).  -- float greater or equal
    "_band" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBAnd, .true, .notLast)).  -- bitwise and two integers
    "_bor" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBOr, .true, .notLast)).  -- bitwise or two integers
    "_bxor" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBXor, .true, .notLast)).  -- bitwise xor two integers
    "_blsl" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBLsl, .true, .notLast)).  -- logical left shift
    "_blsr" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBLsr, .true, .notLast)).  -- logical right shift
    "_basr" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBAsr, .true, .notLast)).  -- arithmetic right shift
    "_bnot" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType])),intType),.iBNot, .true, .notLast)).  -- bitwise negate number
    "_fiber_eq" => .some((.allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("=>>",2),.tupleType([.nomnal("r")])),.nomnal("s")),.tpExp(.tpExp(.tpFun("=>>",2),.tupleType([.nomnal("r")])),.nomnal("s"))])),boolType))),.iTEq, .true, .notLast)).  -- compare two fiber identifiers
    "_cell" => .some((.allType(.nomnal("t"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("t")])),.tpExp(.tpFun("ref",1),.nomnal("t")))),.iCell, .true, .notLast)).  -- create a reference cell
    "_get" => .some((.allType(.nomnal("t"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpFun("ref",1),.nomnal("t"))])),.nomnal("t"))),.iGet, .false, .notLast)).  -- access contents of reference cell
    "_assign" => .some((.allType(.nomnal("t"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpFun("ref",1),.nomnal("t")),.nomnal("t")])),.tupleType([]))),.iAssign, .false, .notLast)).  -- update contents of reference cell

    _ default => .none.
  }
}
