star.compiler.intrinsics{
  -- Automatically Generated Listing of intrinsics -- Do NOT Edit
  import star.

  import star.compiler.types.
  import star.compiler.assem.

  public intrinsic:(string) => option[(tipe,(assemLbl)=>assemOp,boolean)].
  intrinsic(Es) => case Es in {
    | "_int_plus" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),(_)=>.iIAdd, .true))  -- add two integers
    | "_int_minus" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),(_)=>.iISub, .true))  -- subtract two integers
    | "_int_times" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),(_)=>.iIMul, .true))  -- multiply two integers
    | "_int_div" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.nomnal("errorCode")),(Lb)=>.iIDiv(Lb), .true))  -- divide two integers
    | "_int_mod" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.nomnal("errorCode")),(Lb)=>.iIMod(Lb), .true))  -- modulo remainder
    | "_flt_plus" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("float"),.voidType),(_)=>.iFAdd, .true))  -- add two floats
    | "_flt_minus" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("float"),.voidType),(_)=>.iFSub, .true))  -- subtract two floats
    | "_flt_times" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("float"),.voidType),(_)=>.iFMul, .true))  -- multiply two floats
    | "_flt_div" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("float"),.nomnal("errorCode")),(Lb)=>.iFDiv(Lb), .true))  -- divide two floats
    | "_flt_mod" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("float"),.nomnal("errorCode")),(Lb)=>.iFMod(Lb), .true))  -- modulo remainder
    | "_int_abs" => .some((.funType(.tupleType([.nomnal("integer")]),.nomnal("integer"),.voidType),(_)=>.iIAbs, .true))  -- integer absolute value
    | "_flt_abs" => .some((.funType(.tupleType([.nomnal("float")]),.nomnal("float"),.voidType),(_)=>.iFAbs, .true))  -- float absolute value
    | "_int_eq" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("boolean"),.voidType),(_)=>.iIEq, .false))  -- integer equality
    | "_int_lt" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("boolean"),.voidType),(_)=>.iILt, .false))  -- integer less than
    | "_int_ge" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("boolean"),.voidType),(_)=>.iIGe, .false))  -- integer greater or equal
    | "_chr_eq" => .some((.funType(.tupleType([.nomnal("char"),.nomnal("char")]),.nomnal("boolean"),.voidType),(_)=>.iCEq, .false))  -- character equality
    | "_chr_lt" => .some((.funType(.tupleType([.nomnal("char"),.nomnal("char")]),.nomnal("boolean"),.voidType),(_)=>.iCLt, .false))  -- character less than
    | "_chr_ge" => .some((.funType(.tupleType([.nomnal("char"),.nomnal("char")]),.nomnal("boolean"),.voidType),(_)=>.iCGe, .false))  -- character greater or equal
    | "_flt_eq" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("boolean"),.voidType),(_)=>.iFEq, .false))  -- float equality
    | "_flt_lt" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("boolean"),.voidType),(_)=>.iFLt, .false))  -- float less than
    | "_flt_ge" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("boolean"),.voidType),(_)=>.iFGe, .false))  -- float greater or equal
    | "_band" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),(_)=>.iBAnd, .true))  -- bitwise and two integers
    | "_bor" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),(_)=>.iBOr, .true))  -- bitwise or two integers
    | "_bxor" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),(_)=>.iBXor, .true))  -- bitwise xor two integers
    | "_blsl" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),(_)=>.iBLsl, .true))  -- logical left shift
    | "_blsr" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),(_)=>.iBLsr, .true))  -- logical right shift
    | "_basr" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),(_)=>.iBAsr, .true))  -- arithmetic right shift
    | "_bnot" => .some((.funType(.tupleType([.nomnal("integer")]),.nomnal("integer"),.voidType),(_)=>.iBNot, .true))  -- bitwise negate number
    | "_fiber" => .some((.allType(.kVar("r"),.allType(.kVar("s"),.funType(.tupleType([.funType(.tupleType([.tpExp(.tpExp(.tpFun("fiber",2),.kVar("r")),.kVar("s"))]),.kVar("r"),.voidType),.kVar("s")]),.tpExp(.tpExp(.tpFun("fiber",2),.kVar("r")),.kVar("s")),.voidType))),(_)=>.iFiber, .true))  -- create a new fiber

    | _ default => .none
  }
}
