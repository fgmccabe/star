/* Automatically generated, do not edit */

:-module(intrinsics,[isIntrinsic/3]).

isIntrinsic("_abort",allType(kVar("a"),funType(tplType([kVar("a"),type("star*string")]),tplType([]))),iAbort).  % abort process
isIntrinsic("_int_plus",funType(tplType([type("star*integer"),type("star*integer")]),type("star*integer")),iIAdd).  % add two integers
isIntrinsic("_int_minus",funType(tplType([type("star*integer"),type("star*integer")]),type("star*integer")),iISub).  % subtract two integers
isIntrinsic("_int_times",funType(tplType([type("star*integer"),type("star*integer")]),type("star*integer")),iIMul).  % multiply two integers
isIntrinsic("_int_div",constrained(funType(tplType([type("star*integer"),type("star*integer")]),type("star*integer")),raises(type("star.core*errorCode"))),iIDiv).  % divide two integers
isIntrinsic("_int_mod",constrained(funType(tplType([type("star*integer"),type("star*integer")]),type("star*integer")),raises(type("star.core*errorCode"))),iIMod).  % modulo remainder
isIntrinsic("_flt_plus",funType(tplType([type("star*float"),type("star*float")]),type("star*float")),iFAdd).  % add two floats
isIntrinsic("_flt_minus",funType(tplType([type("star*float"),type("star*float")]),type("star*float")),iFSub).  % subtract two floats
isIntrinsic("_flt_times",funType(tplType([type("star*float"),type("star*float")]),type("star*float")),iFMul).  % multiply two floats
isIntrinsic("_flt_div",constrained(funType(tplType([type("star*float"),type("star*float")]),type("star*float")),raises(type("star.core*errorCode"))),iFDiv).  % divide two floats
isIntrinsic("_flt_mod",constrained(funType(tplType([type("star*float"),type("star*float")]),type("star*float")),raises(type("star.core*errorCode"))),iFMod).  % modulo remainder
isIntrinsic("_int_abs",funType(tplType([type("star*integer")]),type("star*integer")),iIAbs).  % integer absolute value
isIntrinsic("_flt_abs",funType(tplType([type("star*float")]),type("star*float")),iFAbs).  % float absolute value
isIntrinsic("_int_eq",funType(tplType([type("star*integer"),type("star*integer")]),type("star*boolean")),iIEq).  % integer equality
isIntrinsic("_int_lt",funType(tplType([type("star*integer"),type("star*integer")]),type("star*boolean")),iILt).  % integer less than
isIntrinsic("_int_ge",funType(tplType([type("star*integer"),type("star*integer")]),type("star*boolean")),iIGe).  % integer greater or equal
isIntrinsic("_chr_eq",funType(tplType([type("star*char"),type("star*char")]),type("star*boolean")),iCEq).  % character equality
isIntrinsic("_chr_lt",funType(tplType([type("star*char"),type("star*char")]),type("star*boolean")),iCLt).  % character less than
isIntrinsic("_chr_ge",funType(tplType([type("star*char"),type("star*char")]),type("star*boolean")),iCGe).  % character greater or equal
isIntrinsic("_flt_eq",funType(tplType([type("star*float"),type("star*float")]),type("star*boolean")),iFEq).  % float equality
isIntrinsic("_flt_lt",funType(tplType([type("star*float"),type("star*float")]),type("star*boolean")),iFLt).  % float less than
isIntrinsic("_flt_ge",funType(tplType([type("star*float"),type("star*float")]),type("star*boolean")),iFGe).  % float greater or equal
isIntrinsic("_band",funType(tplType([type("star*integer"),type("star*integer")]),type("star*integer")),iBAnd).  % bitwise and two integers
isIntrinsic("_bor",funType(tplType([type("star*integer"),type("star*integer")]),type("star*integer")),iBOr).  % bitwise or two integers
isIntrinsic("_bxor",funType(tplType([type("star*integer"),type("star*integer")]),type("star*integer")),iBXor).  % bitwise xor two integers
isIntrinsic("_blsl",funType(tplType([type("star*integer"),type("star*integer")]),type("star*integer")),iBLsl).  % logical left shift
isIntrinsic("_blsr",funType(tplType([type("star*integer"),type("star*integer")]),type("star*integer")),iBLsr).  % logical right shift
isIntrinsic("_basr",funType(tplType([type("star*integer"),type("star*integer")]),type("star*integer")),iBAsr).  % arithmetic right shift
isIntrinsic("_bnot",funType(tplType([type("star*integer")]),type("star*integer")),iBNot).  % bitwise negate number
isIntrinsic("_fiber_eq",allType(kVar("r"),allType(kVar("s"),funType(tplType([tpExp(tpExp(tpFun("fiber",2),kVar("r")),kVar("s")),tpExp(tpExp(tpFun("fiber",2),kVar("r")),kVar("s"))]),type("star*boolean")))),iTEq).  % compare two fiber identifiers
isIntrinsic("_cell",allType(kVar("t"),funType(tplType([kVar("t")]),tpExp(tpFun("ref",1),kVar("t")))),iCell).  % create a reference cell
isIntrinsic("_get",allType(kVar("t"),funType(tplType([tpExp(tpFun("ref",1),kVar("t"))]),kVar("t"))),iGet).  % access contents of reference cell
isIntrinsic("_assign",allType(kVar("t"),funType(tplType([tpExp(tpFun("ref",1),kVar("t")),kVar("t")]),tplType([]))),iAssign).  % update contents of reference cell

