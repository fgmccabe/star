/* Automatically generated, do not edit */

:-module(intrinsics,[isIntrinsic/4]).

isIntrinsic("_int_plus",funType(tplType([type("integer"),type("integer")]),type("integer")),iIAdd,noThrow).  % add two integers
isIntrinsic("_int_minus",funType(tplType([type("integer"),type("integer")]),type("integer")),iISub,noThrow).  % subtract two integers
isIntrinsic("_int_times",funType(tplType([type("integer"),type("integer")]),type("integer")),iIMul,noThrow).  % multiply two integers
isIntrinsic("_int_div",funType(tplType([type("integer"),type("integer")]),type("integer"),type("errorCode")),iIDiv,throwing).  % divide two integers
isIntrinsic("_int_mod",funType(tplType([type("integer"),type("integer")]),type("integer"),type("errorCode")),iIMod,throwing).  % modulo remainder
isIntrinsic("_flt_plus",funType(tplType([type("float"),type("float")]),type("float")),iFAdd,noThrow).  % add two floats
isIntrinsic("_flt_minus",funType(tplType([type("float"),type("float")]),type("float")),iFSub,noThrow).  % subtract two floats
isIntrinsic("_flt_times",funType(tplType([type("float"),type("float")]),type("float")),iFMul,noThrow).  % multiply two floats
isIntrinsic("_flt_div",funType(tplType([type("float"),type("float")]),type("float"),type("errorCode")),iFDiv,throwing).  % divide two floats
isIntrinsic("_flt_mod",funType(tplType([type("float"),type("float")]),type("float"),type("errorCode")),iFMod,throwing).  % modulo remainder
isIntrinsic("_int_abs",funType(tplType([type("integer")]),type("integer")),iIAbs,noThrow).  % integer absolute value
isIntrinsic("_flt_abs",funType(tplType([type("float")]),type("float")),iFAbs,noThrow).  % float absolute value
isIntrinsic("_int_eq",funType(tplType([type("integer"),type("integer")]),type("boolean")),iIEq,noThrow).  % integer equality
isIntrinsic("_int_lt",funType(tplType([type("integer"),type("integer")]),type("boolean")),iILt,noThrow).  % integer less than
isIntrinsic("_int_ge",funType(tplType([type("integer"),type("integer")]),type("boolean")),iIGe,noThrow).  % integer greater or equal
isIntrinsic("_chr_eq",funType(tplType([type("char"),type("char")]),type("boolean")),iCEq,noThrow).  % character equality
isIntrinsic("_chr_lt",funType(tplType([type("char"),type("char")]),type("boolean")),iCLt,noThrow).  % character less than
isIntrinsic("_chr_ge",funType(tplType([type("char"),type("char")]),type("boolean")),iCGe,noThrow).  % character greater or equal
isIntrinsic("_flt_eq",funType(tplType([type("float"),type("float")]),type("boolean")),iFEq,noThrow).  % float equality
isIntrinsic("_flt_lt",funType(tplType([type("float"),type("float")]),type("boolean")),iFLt,noThrow).  % float less than
isIntrinsic("_flt_ge",funType(tplType([type("float"),type("float")]),type("boolean")),iFGe,noThrow).  % float greater or equal
isIntrinsic("_band",funType(tplType([type("integer"),type("integer")]),type("integer")),iBAnd,noThrow).  % bitwise and two integers
isIntrinsic("_bor",funType(tplType([type("integer"),type("integer")]),type("integer")),iBOr,noThrow).  % bitwise or two integers
isIntrinsic("_bxor",funType(tplType([type("integer"),type("integer")]),type("integer")),iBXor,noThrow).  % bitwise xor two integers
isIntrinsic("_blsl",funType(tplType([type("integer"),type("integer")]),type("integer")),iBLsl,noThrow).  % logical left shift
isIntrinsic("_blsr",funType(tplType([type("integer"),type("integer")]),type("integer")),iBLsr,noThrow).  % logical right shift
isIntrinsic("_basr",funType(tplType([type("integer"),type("integer")]),type("integer")),iBAsr,noThrow).  % arithmetic right shift
isIntrinsic("_bnot",funType(tplType([type("integer")]),type("integer")),iBNot,noThrow).  % bitwise negate number
isIntrinsic("_fiber",allType(kVar("r"),allType(kVar("s"),funType(tplType([funType(tplType([tpExp(tpExp(tpFun("fiber",2),kVar("r")),kVar("s"))]),kVar("r")),kVar("s")]),tpExp(tpExp(tpFun("fiber",2),kVar("r")),kVar("s"))))),iFiber,noThrow).  % create a new fiber

