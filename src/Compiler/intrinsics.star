star.compiler.intrinsics{
  -- Automatically Generated Listing of intrinsics -- Do NOT Edit
  import star.

  import star.compiler.types.
  import star.compiler.assem.

  public tailMode ::= .noMore | .notLast.

  public implementation equality[tailMode] => {
    .noMore == .noMore => .true.
    .notLast == .notLast => .true.
    _ == _ default => .false.
  }

  public intrinsic:(string) => option[(tipe,(assemLbl)=>assemOp,boolean,tailMode)].
  intrinsic(Es) => case Es in {
    | "_abort" => .some((.allType(.nomnal("a"),.allType(.nomnal("e"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("a"),strType])),.nomnal("e")))),(_)=>.iAbort, .false, .noMore))  -- abort process
    | "_int_plus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iIAdd, .true, .notLast))  -- add two integers
    | "_int_minus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iISub, .true, .notLast))  -- subtract two integers
    | "_int_times" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iIMul, .true, .notLast))  -- multiply two integers
    | "_int_div" => .some((.tpExp(.tpExp(.tpExp(.tpFun("=>",3),.tupleType([intType,intType])),intType),.nomnal("errorCode")),(Lb)=>.iIDiv(Lb), .true, .notLast))  -- divide two integers
    | "_int_mod" => .some((.tpExp(.tpExp(.tpExp(.tpFun("=>",3),.tupleType([intType,intType])),intType),.nomnal("errorCode")),(Lb)=>.iIMod(Lb), .true, .notLast))  -- modulo remainder
    | "_flt_plus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),(_)=>.iFAdd, .true, .notLast))  -- add two floats
    | "_flt_minus" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),(_)=>.iFSub, .true, .notLast))  -- subtract two floats
    | "_flt_times" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),fltType),(_)=>.iFMul, .true, .notLast))  -- multiply two floats
    | "_flt_div" => .some((.tpExp(.tpExp(.tpExp(.tpFun("=>",3),.tupleType([fltType,fltType])),fltType),.nomnal("errorCode")),(Lb)=>.iFDiv(Lb), .true, .notLast))  -- divide two floats
    | "_flt_mod" => .some((.tpExp(.tpExp(.tpExp(.tpFun("=>",3),.tupleType([fltType,fltType])),fltType),.nomnal("errorCode")),(Lb)=>.iFMod(Lb), .true, .notLast))  -- modulo remainder
    | "_int_abs" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType])),intType),(_)=>.iIAbs, .true, .notLast))  -- integer absolute value
    | "_flt_abs" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType])),fltType),(_)=>.iFAbs, .true, .notLast))  -- float absolute value
    | "_int_eq" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),(_)=>.iIEq, .false, .notLast))  -- integer equality
    | "_int_lt" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),(_)=>.iILt, .false, .notLast))  -- integer less than
    | "_int_ge" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),boolType),(_)=>.iIGe, .false, .notLast))  -- integer greater or equal
    | "_chr_eq" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([chrType,chrType])),boolType),(_)=>.iCEq, .false, .notLast))  -- character equality
    | "_chr_lt" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([chrType,chrType])),boolType),(_)=>.iCLt, .false, .notLast))  -- character less than
    | "_chr_ge" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([chrType,chrType])),boolType),(_)=>.iCGe, .false, .notLast))  -- character greater or equal
    | "_flt_eq" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),boolType),(_)=>.iFEq, .false, .notLast))  -- float equality
    | "_flt_lt" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),boolType),(_)=>.iFLt, .false, .notLast))  -- float less than
    | "_flt_ge" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([fltType,fltType])),boolType),(_)=>.iFGe, .false, .notLast))  -- float greater or equal
    | "_band" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBAnd, .true, .notLast))  -- bitwise and two integers
    | "_bor" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBOr, .true, .notLast))  -- bitwise or two integers
    | "_bxor" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBXor, .true, .notLast))  -- bitwise xor two integers
    | "_blsl" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBLsl, .true, .notLast))  -- logical left shift
    | "_blsr" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBLsr, .true, .notLast))  -- logical right shift
    | "_basr" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType,intType])),intType),(_)=>.iBAsr, .true, .notLast))  -- arithmetic right shift
    | "_bnot" => .some((.tpExp(.tpExp(.tpFun("=>",2),.tupleType([intType])),intType),(_)=>.iBNot, .true, .notLast))  -- bitwise negate number
    | "_fiber" => .some((.allType(.nomnal("r"),.allType(.nomnal("s"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("fiber",2),.nomnal("r")),.nomnal("s"))])),.nomnal("r")),.nomnal("s")])),.tpExp(.tpExp(.tpFun("fiber",2),.nomnal("r")),.nomnal("s"))))),(_)=>.iFiber, .true, .notLast))  -- create a new fiber

    | _ default => .none
  }
}
