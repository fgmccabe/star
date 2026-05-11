star.compiler.intrinsics{
  -- Automatically Generated Listing of intrinsics -- Do NOT Edit
  import star.

  import star.compiler.types.
  import star.compiler.ssa.

  public intrinsic:(string) => option[(tipe,(cons[string],assemLbl)=>insOp)].
  intrinsic(Es) => case Es in {
    | "_int_plus" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),([Rs, V0, V1],Lb) => .iIAdd(Rs, V0, V1))).  -- add two integers
    | "_int_minus" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),([Rs, V0, V1],Lb) => .iISub(Rs, V0, V1))).  -- subtract two integers
    | "_int_times" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),([Rs, V0, V1],Lb) => .iIMul(Rs, V0, V1))).  -- multiply two integers
    | "_int_div" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.nomnal("errorCode")),([Rs, V0, V1],Lb) => .iIDiv(Lb, Rs, V0, V1))).  -- divide two integers
    | "_int_mod" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.nomnal("errorCode")),([Rs, V0, V1],Lb) => .iIMod(Lb, Rs, V0, V1))).  -- modulo remainder
    | "_flt_plus" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("float"),.voidType),([Rs, V0, V1],Lb) => .iFAdd(Rs, V0, V1))).  -- add two floats
    | "_flt_minus" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("float"),.voidType),([Rs, V0, V1],Lb) => .iFSub(Rs, V0, V1))).  -- subtract two floats
    | "_flt_times" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("float"),.voidType),([Rs, V0, V1],Lb) => .iFMul(Rs, V0, V1))).  -- multiply two floats
    | "_flt_div" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("float"),.nomnal("errorCode")),([Rs, V0, V1],Lb) => .iFDiv(Lb, Rs, V0, V1))).  -- divide two floats
    | "_flt_mod" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("float"),.nomnal("errorCode")),([Rs, V0, V1],Lb) => .iFMod(Lb, Rs, V0, V1))).  -- modulo remainder
    | "_int_abs" => .some((.funType(.tupleType([.nomnal("integer")]),.nomnal("integer"),.voidType),([Rs, V0],Lb) => .iIAbs(Rs, V0))).  -- integer absolute value
    | "_flt_abs" => .some((.funType(.tupleType([.nomnal("float")]),.nomnal("float"),.voidType),([Rs, V0],Lb) => .iFAbs(Rs, V0))).  -- float absolute value
    | "_int_eq" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("boolean"),.voidType),([Rs, V0, V1],Lb) => .iIEq(Rs, V0, V1))).  -- integer equality
    | "_int_lt" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("boolean"),.voidType),([Rs, V0, V1],Lb) => .iILt(Rs, V0, V1))).  -- integer less than
    | "_int_ge" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("boolean"),.voidType),([Rs, V0, V1],Lb) => .iIGe(Rs, V0, V1))).  -- integer greater or equal
    | "_chr_eq" => .some((.funType(.tupleType([.nomnal("char"),.nomnal("char")]),.nomnal("boolean"),.voidType),([Rs, V0, V1],Lb) => .iCEq(Rs, V0, V1))).  -- character equality
    | "_chr_lt" => .some((.funType(.tupleType([.nomnal("char"),.nomnal("char")]),.nomnal("boolean"),.voidType),([Rs, V0, V1],Lb) => .iCLt(Rs, V0, V1))).  -- character less than
    | "_chr_ge" => .some((.funType(.tupleType([.nomnal("char"),.nomnal("char")]),.nomnal("boolean"),.voidType),([Rs, V0, V1],Lb) => .iCGe(Rs, V0, V1))).  -- character greater or equal
    | "_flt_eq" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("boolean"),.voidType),([Rs, V0, V1],Lb) => .iFEq(Rs, V0, V1))).  -- float equality
    | "_flt_lt" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("boolean"),.voidType),([Rs, V0, V1],Lb) => .iFLt(Rs, V0, V1))).  -- float less than
    | "_flt_ge" => .some((.funType(.tupleType([.nomnal("float"),.nomnal("float")]),.nomnal("boolean"),.voidType),([Rs, V0, V1],Lb) => .iFGe(Rs, V0, V1))).  -- float greater or equal
    | "_band" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),([Rs, V0, V1],Lb) => .iBAnd(Rs, V0, V1))).  -- bitwise and two integers
    | "_bor" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),([Rs, V0, V1],Lb) => .iBOr(Rs, V0, V1))).  -- bitwise or two integers
    | "_bxor" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),([Rs, V0, V1],Lb) => .iBXor(Rs, V0, V1))).  -- bitwise xor two integers
    | "_blsl" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),([Rs, V0, V1],Lb) => .iBLsl(Rs, V0, V1))).  -- logical left shift
    | "_blsr" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),([Rs, V0, V1],Lb) => .iBLsr(Rs, V0, V1))).  -- logical right shift
    | "_basr" => .some((.funType(.tupleType([.nomnal("integer"),.nomnal("integer")]),.nomnal("integer"),.voidType),([Rs, V0, V1],Lb) => .iBAsr(Rs, V0, V1))).  -- arithmetic right shift
    | "_bnot" => .some((.funType(.tupleType([.nomnal("integer")]),.nomnal("integer"),.voidType),([Rs, V0],Lb) => .iBNot(Rs, V0))).  -- bitwise negate number
    | "_fiber" => .some((.allType(.kVar("r"),.allType(.kVar("s"),.funType(.tupleType([.funType(.tupleType([.tpExp(.tpExp(.tpFun("fiber",2),.kVar("r")),.kVar("s"))]),.kVar("r"),.voidType),.kVar("s")]),.tpExp(.tpExp(.tpFun("fiber",2),.kVar("r")),.kVar("s")),.voidType))),([Rs, V0, V1],Lb) => .iFiber(Rs, V0, V1))).  -- create a new fiber

    | _ default => .none
  }
}
