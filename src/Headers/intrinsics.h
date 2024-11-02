/*
  This is where you define an intrinsic function that the compiler converts to assembler
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
 */

#define continType(R,S) "x" R S
#define fiberType(R,S) "UUz2'fiber'" R S

#define ERRCODE "t'errorCode"


/* Define the standard intrinsics */
intrinsic(_abort,":k'a'F(k'a's)()","Abort",False, Last, "abort process")
intrinsic(_int_plus,"F(ii)i","IAdd",True,NotLast,"add two integers")
intrinsic(_int_minus,"F(ii)i","ISub",True,NotLast,"subtract two integers")
intrinsic(_int_times,"F(ii)i","IMul",True,NotLast,"multiply two integers")
intrinsic(_int_div,"|F(ii)ir"ERRCODE,"IDiv",True,NotLast,"divide two integers")
intrinsic(_int_mod,"|F(ii)ir"ERRCODE,"IMod",True,NotLast,"modulo remainder")

intrinsic(_flt_plus,"F(ff)f","FAdd",True,NotLast,"add two floats")
intrinsic(_flt_minus,"F(ff)f","FSub",True,NotLast,"subtract two floats")
intrinsic(_flt_times,"F(ff)f","FMul",True,NotLast,"multiply two floats")
intrinsic(_flt_div,"|F(ff)fr"ERRCODE,"FDiv",True,NotLast,"divide two floats")
intrinsic(_flt_mod,"|F(ff)fr"ERRCODE,"FMod",True,NotLast,"modulo remainder")

intrinsic(_int_abs,"F(i)i","IAbs",True,NotLast,"integer absolute value")
intrinsic(_flt_abs,"F(f)f","FAbs",True,NotLast,"float absolute value")

intrinsic(_int_eq,"F(ii)l","IEq",False,NotLast,"integer equality")
intrinsic(_int_lt,"F(ii)l","ILt",False,NotLast,"integer less than")
intrinsic(_int_ge,"F(ii)l","IGe",False,NotLast,"integer greater or equal")

intrinsic(_chr_eq,"F(cc)l","CEq",False,NotLast,"character equality")
intrinsic(_chr_lt,"F(cc)l","CLt",False,NotLast,"character less than")
intrinsic(_chr_ge,"F(cc)l","CGe",False,NotLast,"character greater or equal")

intrinsic(_flt_eq,"F(ff)l","FEq",False,NotLast,"float equality")
intrinsic(_flt_lt,"F(ff)l","FLt",False,NotLast,"float less than")
intrinsic(_flt_ge,"F(ff)l","FGe",False,NotLast,"float greater or equal")

intrinsic(_band,"F(ii)i","BAnd",True,NotLast,"bitwise and two integers")
intrinsic(_bor,"F(ii)i","BOr",True,NotLast,"bitwise or two integers")
intrinsic(_bxor,"F(ii)i","BXor",True,NotLast,"bitwise xor two integers")
intrinsic(_blsl,"F(ii)i","BLsl",True,NotLast,"logical left shift")
intrinsic(_blsr,"F(ii)i","BLsr",True,NotLast,"logical right shift")
intrinsic(_basr,"F(ii)i","BAsr",True,NotLast,"arithmetic right shift")
intrinsic(_bnot,"F(i)i","BNot",True,NotLast,"bitwise negate number")

intrinsic(_fiber_eq,":k'r':k's'F("fiberType("k'r'","k's'")fiberType("k'r'","k's'")")l","TEq",True,NotLast,"compare two fiber identifiers")

intrinsic(_cell, ":k't'F(k't')rk't'", "Cell", True, NotLast,"create a reference cell")
intrinsic(_get, ":k't'F(rk't')k't'", "Get", False,NotLast,"access contents of reference cell")
intrinsic(_assign, ":k't'F(rk't'k't')()", "Assign", False, NotLast,"update contents of reference cell")

