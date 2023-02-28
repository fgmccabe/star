/* Automatically generated, do not edit */

:-module(intrinsics,[isIntrinsic/3]).

isIntrinsic("_abort",allType(kVar("a"),funType(tplType([kVar("a"),type("star.core*string")]),tplType([]))),iAbort).  % abort process
isIntrinsic("_int_plus",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iIAdd).  % add two integers
isIntrinsic("_int_minus",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iISub).  % subtract two integers
isIntrinsic("_int_times",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iIMul).  % multiply two integers
isIntrinsic("_int_div",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iIDiv).  % divide two integers
isIntrinsic("_int_mod",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iIMod).  % modulo remainder
isIntrinsic("_flt_plus",funType(tplType([type("star.core*float"),type("star.core*float")]),type("star.core*float")),iFAdd).  % add two floats
isIntrinsic("_flt_minus",funType(tplType([type("star.core*float"),type("star.core*float")]),type("star.core*float")),iFSub).  % subtract two floats
isIntrinsic("_flt_times",funType(tplType([type("star.core*float"),type("star.core*float")]),type("star.core*float")),iFMul).  % multiply two floats
isIntrinsic("_flt_div",funType(tplType([type("star.core*float"),type("star.core*float")]),type("star.core*float")),iFDiv).  % divide two floats
isIntrinsic("_flt_mod",funType(tplType([type("star.core*float"),type("star.core*float")]),type("star.core*float")),iFMod).  % modulo remainder
isIntrinsic("_int_abs",funType(tplType([type("star.core*integer")]),type("star.core*integer")),iIAbs).  % integer absolute value
isIntrinsic("_flt_abs",funType(tplType([type("star.core*float")]),type("star.core*float")),iFAbs).  % float absolute value
isIntrinsic("_int_eq",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*boolean")),iIEq).  % integer equality
isIntrinsic("_int_lt",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*boolean")),iILt).  % integer less than
isIntrinsic("_int_ge",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*boolean")),iIGe).  % integer greater or equal
isIntrinsic("_flt_eq",funType(tplType([type("star.core*float"),type("star.core*float"),type("star.core*float")]),type("star.core*boolean")),iFEq).  % float equality
isIntrinsic("_flt_lt",funType(tplType([type("star.core*float"),type("star.core*float")]),type("star.core*boolean")),iFLt).  % float less than
isIntrinsic("_flt_ge",funType(tplType([type("star.core*float"),type("star.core*float")]),type("star.core*boolean")),iFGe).  % float greater or equal
isIntrinsic("_band",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBAnd).  % bitwise and two integers
isIntrinsic("_bor",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBOr).  % bitwise or two integers
isIntrinsic("_bxor",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBXor).  % bitwise xor two integers
isIntrinsic("_blsl",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBLsl).  % logical left shift
isIntrinsic("_blsr",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBLsr).  % logical right shift
isIntrinsic("_basr",funType(tplType([type("star.core*integer"),type("star.core*integer")]),type("star.core*integer")),iBAsr).  % arithmetic right shift
isIntrinsic("_bnot",funType(tplType([type("star.core*integer")]),type("star.core*integer")),iBNot).  % bitwise negate number
isIntrinsic("_fiber_eq",allType(kVar("s"),allType(kVar("r"),funType(tplType([tpExp(tpExp(tpFun("star.fiber*fiber",2),kVar("r")),kVar("s")),tpExp(tpExp(tpFun("star.fiber*fiber",2),kVar("r")),kVar("s"))]),type("star.core*boolean")))),iTEq).  % compare two fiber identifiers
isIntrinsic("_new_fiber",allType(kVar("s"),allType(kVar("r"),funType(tplType([funType(tplType([tpExp(tpExp(tpFun("star.fiber*fiber",2),kVar("r")),kVar("s")),kVar("r")]),kVar("s"))]),tpExp(tpExp(tpFun("star.fiber*fiber",2),kVar("r")),kVar("s"))))),iFiber).  % create a new fiber
isIntrinsic("_spawn",allType(kVar("s"),allType(kVar("r"),funType(tplType([funType(tplType([tpExp(tpExp(tpFun("star.fiber*fiber",2),kVar("r")),kVar("s"))]),kVar("s"))]),kVar("s")))),iSpawn).  % spawn a new task
isIntrinsic("_suspend",allType(kVar("s"),allType(kVar("r"),funType(tplType([tpExp(tpExp(tpFun("star.fiber*fiber",2),kVar("r")),kVar("s")),kVar("s")]),kVar("r")))),iSuspend).  % suspend a fiber
isIntrinsic("_retire",allType(kVar("s"),allType(kVar("r"),funType(tplType([tpExp(tpExp(tpFun("star.fiber*fiber",2),kVar("r")),kVar("s")),kVar("s")]),tplType([])))),iRetire).  % retire a fiber
isIntrinsic("_resume",allType(kVar("s"),allType(kVar("r"),funType(tplType([tpExp(tpExp(tpFun("star.fiber*fiber",2),kVar("r")),kVar("s")),kVar("r")]),kVar("s")))),iResume).  % resume a fiber
isIntrinsic("_cell",allType(kVar("t"),funType(tplType([kVar("t")]),refType(kVar("t")))),iCell).  % create a reference cell
isIntrinsic("_get",allType(kVar("t"),funType(tplType([refType(kVar("t"))]),kVar("t"))),iGet).  % access contents of reference cell
isIntrinsic("_assign",allType(kVar("t"),funType(tplType([refType(kVar("t")),kVar("t")]),tplType([]))),iAssign).  % update contents of reference cell

