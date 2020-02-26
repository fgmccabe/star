/* Automatically generated, do not edit */

:-module(intrinsics,[isIntrinsic/3]).

isIntrinsic("_exit",funType(tupleType([type("star.core*integer")]),tupleType([])),iHalt).  % terminate engine
isIntrinsic("_int_plus",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iIAdd).  % add two integers
isIntrinsic("_int_minus",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iISub).  % subtract two integers
isIntrinsic("_int_times",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iIMul).  % multiply two integers
isIntrinsic("_int_div",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iIDiv).  % divide two integers
isIntrinsic("_int_mod",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iIMod).  % modulo remainder
isIntrinsic("_flt_plus",funType(tupleType([type("star.core*float"),type("star.core*float")]),type("star.core*float")),iFAdd).  % add two floats
isIntrinsic("_flt_minus",funType(tupleType([type("star.core*float"),type("star.core*float")]),type("star.core*float")),iFSub).  % subtract two floats
isIntrinsic("_flt_times",funType(tupleType([type("star.core*float"),type("star.core*float")]),type("star.core*float")),iFMul).  % multiply two floats
isIntrinsic("_flt_div",funType(tupleType([type("star.core*float"),type("star.core*float")]),type("star.core*float")),iFDiv).  % divide two floats
isIntrinsic("_flt_mod",funType(tupleType([type("star.core*float"),type("star.core*float")]),type("star.core*float")),iFMod).  % modulo remainder
isIntrinsic("_int_abs",funType(tupleType([type("star.core*integer")]),type("star.core*integer")),iIAbs).  % integer absolute value
isIntrinsic("_flt_abs",funType(tupleType([type("star.core*float")]),type("star.core*float")),iFAbs).  % float absolute value
isIntrinsic("_int_eq",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*boolean")),iIEq).  % integer equality
isIntrinsic("_int_lt",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*boolean")),iILt).  % integer less than
isIntrinsic("_int_ge",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*boolean")),iIGe).  % integer greater or equal
isIntrinsic("_flt_eq",funType(tupleType([type("star.core*float"),type("star.core*float"),type("star.core*float")]),type("star.core*boolean")),iFEq).  % float equality
isIntrinsic("_flt_lt",funType(tupleType([type("star.core*float"),type("star.core*float")]),type("star.core*boolean")),iFLt).  % float less than
isIntrinsic("_flt_ge",funType(tupleType([type("star.core*float"),type("star.core*float")]),type("star.core*boolean")),iFGe).  % float greater or equal
isIntrinsic("_band",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBAnd).  % bitwise and two integers
isIntrinsic("_bor",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBOr).  % bitwise or two integers
isIntrinsic("_bxor",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBXor).  % bitwise xor two integers
isIntrinsic("_blsl",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBLsl).  % logical left shift
isIntrinsic("_blsr",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBLsr).  % logical right shift
isIntrinsic("_basr",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBAsr).  % arithmetic right shift
isIntrinsic("_bnot",funType(tupleType([type("star.core*integer")]),type("star.core*integer")),iBNot).  % bitwise negate number

