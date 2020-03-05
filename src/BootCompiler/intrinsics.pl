/* Automatically generated, do not edit */

:-module(intrinsics,[isIntrinsic/3]).

isIntrinsic("_int_eq",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*boolean")),iIEq).  % integer equality
isIntrinsic("_int_lt",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*boolean")),iILt).  % integer less than
isIntrinsic("_int_ge",funType(tupleType([type("star.core*integer"),type("star.core*integer")]),type("star.core*boolean")),iIGe).  % integer greater or equal
isIntrinsic("_flt_eq",funType(tupleType([type("star.core*float"),type("star.core*float"),type("star.core*float")]),type("star.core*boolean")),iFEq).  % float equality
isIntrinsic("_flt_lt",funType(tupleType([type("star.core*float"),type("star.core*float")]),type("star.core*boolean")),iFLt).  % float less than
isIntrinsic("_flt_ge",funType(tupleType([type("star.core*float"),type("star.core*float")]),type("star.core*boolean")),iFGe).  % float greater or equal

