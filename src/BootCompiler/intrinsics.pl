/* Automatically generated, do not edit */

:-module(intrinsics,[isIntrinsic/3]).

isIntrinsic("_abort",allType(kVar("a"),funType(tplType([kVar("a"),type("string")]),tplType([]))),iAbort).  % abort process
isIntrinsic("_int_plus",funType(tplType([type("integer"),type("integer")]),type("integer")),iIAdd).  % add two integers
isIntrinsic("_int_minus",funType(tplType([type("integer"),type("integer")]),type("integer")),iISub).  % subtract two integers
isIntrinsic("_int_times",funType(tplType([type("integer"),type("integer")]),type("integer")),iIMul).  % multiply two integers
isIntrinsic("_int_div",constrained(funType(tplType([type("integer"),type("integer")]),type("integer")),raises(type("errorCode"))),iIDiv).  % divide two integers
isIntrinsic("_int_mod",constrained(funType(tplType([type("integer"),type("integer")]),type("integer")),raises(type("errorCode"))),iIMod).  % modulo remainder
isIntrinsic("_flt_plus",funType(tplType([type("float"),type("float")]),type("float")),iFAdd).  % add two floats
isIntrinsic("_flt_minus",funType(tplType([type("float"),type("float")]),type("float")),iFSub).  % subtract two floats
isIntrinsic("_flt_times",funType(tplType([type("float"),type("float")]),type("float")),iFMul).  % multiply two floats
isIntrinsic("_flt_div",constrained(funType(tplType([type("float"),type("float")]),type("float")),raises(type("errorCode"))),iFDiv).  % divide two floats
isIntrinsic("_flt_mod",constrained(funType(tplType([type("float"),type("float")]),type("float")),raises(type("errorCode"))),iFMod).  % modulo remainder
isIntrinsic("_int_abs",funType(tplType([type("integer")]),type("integer")),iIAbs).  % integer absolute value
isIntrinsic("_flt_abs",funType(tplType([type("float")]),type("float")),iFAbs).  % float absolute value
isIntrinsic("_int_eq",funType(tplType([type("integer"),type("integer")]),type("boolean")),iIEq).  % integer equality
isIntrinsic("_int_lt",funType(tplType([type("integer"),type("integer")]),type("boolean")),iILt).  % integer less than
isIntrinsic("_int_ge",funType(tplType([type("integer"),type("integer")]),type("boolean")),iIGe).  % integer greater or equal
isIntrinsic("_chr_eq",funType(tplType([type("char"),type("char")]),type("boolean")),iCEq).  % character equality
isIntrinsic("_chr_lt",funType(tplType([type("char"),type("char")]),type("boolean")),iCLt).  % character less than
isIntrinsic("_chr_ge",funType(tplType([type("char"),type("char")]),type("boolean")),iCGe).  % character greater or equal
isIntrinsic("_flt_eq",funType(tplType([type("float"),type("float")]),type("boolean")),iFEq).  % float equality
isIntrinsic("_flt_lt",funType(tplType([type("float"),type("float")]),type("boolean")),iFLt).  % float less than
isIntrinsic("_flt_ge",funType(tplType([type("float"),type("float")]),type("boolean")),iFGe).  % float greater or equal
isIntrinsic("_band",funType(tplType([type("integer"),type("integer")]),type("integer")),iBAnd).  % bitwise and two integers
isIntrinsic("_bor",funType(tplType([type("integer"),type("integer")]),type("integer")),iBOr).  % bitwise or two integers
isIntrinsic("_bxor",funType(tplType([type("integer"),type("integer")]),type("integer")),iBXor).  % bitwise xor two integers
isIntrinsic("_blsl",funType(tplType([type("integer"),type("integer")]),type("integer")),iBLsl).  % logical left shift
isIntrinsic("_blsr",funType(tplType([type("integer"),type("integer")]),type("integer")),iBLsr).  % logical right shift
isIntrinsic("_basr",funType(tplType([type("integer"),type("integer")]),type("integer")),iBAsr).  % arithmetic right shift
isIntrinsic("_bnot",funType(tplType([type("integer")]),type("integer")),iBNot).  % bitwise negate number
isIntrinsic("_fiber_eq",allType(kVar("r"),allType(kVar("s"),funType(tplType([tpExp(tpExp(tpFun("fiber",2),kVar("r")),kVar("s")),tpExp(tpExp(tpFun("fiber",2),kVar("r")),kVar("s"))]),type("boolean")))),iTEq).  % compare two fiber identifiers
isIntrinsic("_fiber",allType(kVar("r"),allType(kVar("s"),funType(tplType([funType(tplType([tpExp(tpExp(tpFun("fiber",2),kVar("r")),kVar("s")),kVar("r")]),kVar("s"))]),tpExp(tpExp(tpFun("fiber",2),kVar("r")),kVar("s"))))),iFiber).  % create a new fiber
isIntrinsic("_suspend",allType(kVar("r"),allType(kVar("s"),funType(tplType([tpExp(tpExp(tpFun("fiber",2),kVar("r")),kVar("s")),kVar("s")]),kVar("r")))),iSuspend).  % suspend fiber
isIntrinsic("_retire",allType(kVar("r"),allType(kVar("s"),allType(kVar("e"),funType(tplType([tpExp(tpExp(tpFun("fiber",2),kVar("r")),kVar("s")),kVar("s")]),kVar("e"))))),iRetire).  % retire fiber
isIntrinsic("_resume",allType(kVar("r"),allType(kVar("s"),funType(tplType([tpExp(tpExp(tpFun("fiber",2),kVar("r")),kVar("s")),kVar("r")]),kVar("s")))),iResume).  % resume fiber
isIntrinsic("_cell",allType(kVar("t"),funType(tplType([kVar("t")]),tpExp(tpFun("ref",1),kVar("t")))),iCell).  % create a reference cell
isIntrinsic("_get",allType(kVar("t"),funType(tplType([tpExp(tpFun("ref",1),kVar("t"))]),kVar("t"))),iGet).  % access contents of reference cell
isIntrinsic("_assign",allType(kVar("t"),funType(tplType([tpExp(tpFun("ref",1),kVar("t")),kVar("t")]),tplType([]))),iAssign).  % update contents of reference cell

