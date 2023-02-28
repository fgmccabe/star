star.compiler.intrinsics{
  -- Automatically Generated Listing of intrinsics -- Do NOT Edit
  import star.

  import star.compiler.types.
  import star.compiler.assem.

  public tailMode ::= .noMore | .notLast.

  public intrinsic:(string) => option[(tipe,assemOp,boolean,tailMode)].
  intrinsic(Es) => case Es in {
    "_abort" => ? (.allType(.nomnal("a"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("a"),strType])),.tupleType([]))),.iAbort, .false, .noMore).  -- abort process
    "_int_plus" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iIAdd, .true, .notLast).  -- add two integers
    "_int_minus" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iISub, .true, .notLast).  -- subtract two integers
    "_int_times" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iIMul, .true, .notLast).  -- multiply two integers
    "_int_div" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iIDiv, .true, .notLast).  -- divide two integers
    "_int_mod" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iIMod, .true, .notLast).  -- modulo remainder
    "_flt_plus" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),.iFAdd, .true, .notLast).  -- add two floats
    "_flt_minus" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),.iFSub, .true, .notLast).  -- subtract two floats
    "_flt_times" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),.iFMul, .true, .notLast).  -- multiply two floats
    "_flt_div" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),.iFDiv, .true, .notLast).  -- divide two floats
    "_flt_mod" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),.iFMod, .true, .notLast).  -- modulo remainder
    "_int_abs" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType])),intType),.iIAbs, .true, .notLast).  -- integer absolute value
    "_flt_abs" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType])),fltType),.iFAbs, .true, .notLast).  -- float absolute value
    "_int_eq" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),.iIEq, .false, .notLast).  -- integer equality
    "_int_lt" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),.iILt, .false, .notLast).  -- integer less than
    "_int_ge" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),.iIGe, .false, .notLast).  -- integer greater or equal
    "_flt_eq" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType,fltType])),boolType),.iFEq, .false, .notLast).  -- float equality
    "_flt_lt" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),boolType),.iFLt, .false, .notLast).  -- float less than
    "_flt_ge" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),boolType),.iFGe, .false, .notLast).  -- float greater or equal
    "_band" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBAnd, .true, .notLast).  -- bitwise and two integers
    "_bor" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBOr, .true, .notLast).  -- bitwise or two integers
    "_bxor" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBXor, .true, .notLast).  -- bitwise xor two integers
    "_blsl" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBLsl, .true, .notLast).  -- logical left shift
    "_blsr" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBLsr, .true, .notLast).  -- logical right shift
    "_basr" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),.iBAsr, .true, .notLast).  -- arithmetic right shift
    "_bnot" => ? (.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType])),intType),.iBNot, .true, .notLast).  -- bitwise negate number
    "_fiber_eq" => ? (.allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s")),.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s"))])),boolType))),.iTEq, .true, .notLast).  -- compare two fiber identifiers
    "_new_fiber" => ? (.allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s")),.nomnal("r")])),.nomnal("s"))])),.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s"))))),.iFiber, .true, .notLast).  -- create a new fiber
    "_spawn" => ? (.allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s"))])),.nomnal("s"))])),.nomnal("s")))),.iSpawn, .true, .notLast).  -- spawn a new task
    "_suspend" => ? (.allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s")),.nomnal("s")])),.nomnal("r")))),.iSuspend, .true, .notLast).  -- suspend a fiber
    "_retire" => ? (.allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s")),.nomnal("s")])),.tupleType([])))),.iRetire, .false, .noMore).  -- retire a fiber
    "_resume" => ? (.allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s")),.nomnal("r")])),.nomnal("s")))),.iResume, .true, .notLast).  -- resume a fiber
    "_cell" => ? (.allType(.nomnal("t"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("t")])),.tpExp(.tpFun("ref",1),.nomnal("t")))),.iCell, .true, .notLast).  -- create a reference cell
    "_get" => ? (.allType(.nomnal("t"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpFun("ref",1),.nomnal("t"))])),.nomnal("t"))),.iGet, .false, .notLast).  -- access contents of reference cell
    "_assign" => ? (.allType(.nomnal("t"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpFun("ref",1),.nomnal("t")),.nomnal("t")])),.tupleType([]))),.iAssign, .false, .notLast).  -- update contents of reference cell

    _ default => .none.
  }
}
