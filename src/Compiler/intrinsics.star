star.compiler.intrinsics{
  -- Automatically Generated Listing of intrinsics -- Do NOT Edit
  import star.

  import star.compiler.types.
  import star.compiler.assem.

  public intrinsic:(string) => option[(tipe,(assemLbl)=>assemOp,boolean)].
  intrinsic(Es) => case Es in {
    | "_abort" => .some((.allType(.nomnal("a"),.allType(.nomnal("e"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("a"),strType])),.nomnal("e")))),(_)=>.iAbort, .false))  -- abort process
    | "_int_plus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iIAdd, .true))  -- add two integers
    | "_int_minus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iISub, .true))  -- subtract two integers
    | "_int_times" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iIMul, .true))  -- multiply two integers
    | "_int_div" => .some((.tpExp(.tpExp(.tpExp(.tpFun("=>",3),.tupleType([intType,intType])),intType),.nomnal("errorCode")),(Lb)=>.iIDiv(Lb), .true))  -- divide two integers
    | "_int_mod" => .some((.tpExp(.tpExp(.tpExp(.tpFun("=>",3),.tupleType([intType,intType])),intType),.nomnal("errorCode")),(Lb)=>.iIMod(Lb), .true))  -- modulo remainder
    | "_flt_plus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),(_)=>.iFAdd, .true))  -- add two floats
    | "_flt_minus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),(_)=>.iFSub, .true))  -- subtract two floats
    | "_flt_times" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),(_)=>.iFMul, .true))  -- multiply two floats
    | "_flt_div" => .some((.tpExp(.tpExp(.tpExp(.tpFun("=>",3),.tupleType([fltType,fltType])),fltType),.nomnal("errorCode")),(Lb)=>.iFDiv(Lb), .true))  -- divide two floats
    | "_flt_mod" => .some((.tpExp(.tpExp(.tpExp(.tpFun("=>",3),.tupleType([fltType,fltType])),fltType),.nomnal("errorCode")),(Lb)=>.iFMod(Lb), .true))  -- modulo remainder
    | "_int_abs" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType])),intType),(_)=>.iIAbs, .true))  -- integer absolute value
    | "_flt_abs" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType])),fltType),(_)=>.iFAbs, .true))  -- float absolute value
    | "_int_eq" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),(_)=>.iIEq, .false))  -- integer equality
    | "_int_lt" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),(_)=>.iILt, .false))  -- integer less than
    | "_int_ge" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),(_)=>.iIGe, .false))  -- integer greater or equal
    | "_chr_eq" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([chrType,chrType])),boolType),(_)=>.iCEq, .false))  -- character equality
    | "_chr_lt" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([chrType,chrType])),boolType),(_)=>.iCLt, .false))  -- character less than
    | "_chr_ge" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([chrType,chrType])),boolType),(_)=>.iCGe, .false))  -- character greater or equal
    | "_flt_eq" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),boolType),(_)=>.iFEq, .false))  -- float equality
    | "_flt_lt" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),boolType),(_)=>.iFLt, .false))  -- float less than
    | "_flt_ge" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),boolType),(_)=>.iFGe, .false))  -- float greater or equal
    | "_band" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBAnd, .true))  -- bitwise and two integers
    | "_bor" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBOr, .true))  -- bitwise or two integers
    | "_bxor" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBXor, .true))  -- bitwise xor two integers
    | "_blsl" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBLsl, .true))  -- logical left shift
    | "_blsr" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBLsr, .true))  -- logical right shift
    | "_basr" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBAsr, .true))  -- arithmetic right shift
    | "_bnot" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType])),intType),(_)=>.iBNot, .true))  -- bitwise negate number
    | "_fiber" => .some((.allType(.nomnal("r"),.allType(.nomnal("s"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("fiber",2),.nomnal("r")),.nomnal("s"))])),.nomnal("r")),.nomnal("s")])),.tpExp(.tpExp(.tpFun("fiber",2),.nomnal("r")),.nomnal("s"))))),(_)=>.iFiber, .true))  -- create a new fiber

    | _ default => .none
  }
}
