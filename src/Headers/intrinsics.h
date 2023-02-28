/*
  This is where you define an intrinsic function that the compiler converts to assembler
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
 */

#define fiberType(R,S) "UUz2'star.fiber*fiber'" R S

/* Define the standard intrinsics */
intrinsic(_abort,":k'a'F(k'a's)()","Abort",False, Last, "abort process")
intrinsic(_int_plus,"F(ii)i","IAdd",True,NotLast,"add two integers")
intrinsic(_int_minus,"F(ii)i","ISub",True,NotLast,"subtract two integers")
intrinsic(_int_times,"F(ii)i","IMul",True,NotLast,"multiply two integers")
intrinsic(_int_div,"F(ii)i","IDiv",True,NotLast,"divide two integers")
intrinsic(_int_mod,"F(ii)i","IMod",True,NotLast,"modulo remainder")

intrinsic(_flt_plus,"F(ff)f","FAdd",True,NotLast,"add two floats")
intrinsic(_flt_minus,"F(ff)f","FSub",True,NotLast,"subtract two floats")
intrinsic(_flt_times,"F(ff)f","FMul",True,NotLast,"multiply two floats")
intrinsic(_flt_div,"F(ff)f","FDiv",True,NotLast,"divide two floats")
intrinsic(_flt_mod,"F(ff)f","FMod",True,NotLast,"modulo remainder")

intrinsic(_int_abs,"F(i)i","IAbs",True,NotLast,"integer absolute value")
intrinsic(_flt_abs,"F(f)f","FAbs",True,NotLast,"float absolute value")

intrinsic(_int_eq,"F(ii)l","IEq",False,NotLast,"integer equality")
intrinsic(_int_lt,"F(ii)l","ILt",False,NotLast,"integer less than")
intrinsic(_int_ge,"F(ii)l","IGe",False,NotLast,"integer greater or equal")

intrinsic(_flt_eq,"F(fff)l","FEq",False,NotLast,"float equality")
intrinsic(_flt_lt,"F(ff)l","FLt",False,NotLast,"float less than")
intrinsic(_flt_ge,"F(ff)l","FGe",False,NotLast,"float greater or equal")

intrinsic(_band,"F(ii)i","BAnd",True,NotLast,"bitwise and two integers")
intrinsic(_bor,"F(ii)i","BOr",True,NotLast,"bitwise or two integers")
intrinsic(_bxor,"F(ii)i","BXor",True,NotLast,"bitwise xor two integers")
intrinsic(_blsl,"F(ii)i","BLsl",True,NotLast,"logical left shift")
intrinsic(_blsr,"F(ii)i","BLsr",True,NotLast,"logical right shift")
intrinsic(_basr,"F(ii)i","BAsr",True,NotLast,"arithmetic right shift")
intrinsic(_bnot,"F(i)i","BNot",True,NotLast,"bitwise negate number")

intrinsic(_fiber_eq,":k's':k'r'F("fiberType("k'r'","k's'")fiberType("k'r'","k's'")")l","TEq",True,NotLast,"compare two fiber identifiers")
intrinsic(_new_fiber,":k's':k'r'F(F("fiberType("k'r'","k's'")"k'r')k's')"fiberType("k'r'","k's'"),"Fiber",True,NotLast,"create a new fiber")
intrinsic(_spawn,":k's':k'r'F(F("fiberType("k'r'","k's'")")k's')k's'","Spawn",True,NotLast,"spawn a new task")
intrinsic(_suspend,":k's':k'r'F("fiberType("k'r'","k's'")"k's')k'r'","Suspend", True,NotLast,"suspend a fiber")
intrinsic(_retire,":k's':k'r'F("fiberType("k'r'","k's'")"k's')()","Retire",False,Last,"retire a fiber")
intrinsic(_resume,":k's':k'r'F("fiberType("k'r'","k's'")"k'r')k's'","Resume" ,True,NotLast,"resume a fiber")

intrinsic(_cell, ":k't'F(k't')rk't'", "Cell", True, NotLast,"create a reference cell")
intrinsic(_get, ":k't'F(rk't')k't'", "Get", False,NotLast,"access contents of reference cell")
intrinsic(_assign, ":k't'F(rk't'k't')()", "Assign", False, NotLast,"update contents of reference cell")

