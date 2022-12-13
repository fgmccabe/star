/*
  This is where you define an intrinsic function that the compiler converts to assembler
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
 */

#define fiberType(R,S) "UUz2'star.fiber*fiber'" R S

/* Define the standard intrinsics */

intrinsic(_int_plus,"F(ii)i","IAdd",True,"add two integers")
intrinsic(_int_minus,"F(ii)i","ISub",True,"subtract two integers")
intrinsic(_int_times,"F(ii)i","IMul",True,"multiply two integers")
intrinsic(_int_div,"F(ii)i","IDiv",True,"divide two integers")
intrinsic(_int_mod,"F(ii)i","IMod",True,"modulo remainder")

intrinsic(_flt_plus,"F(ff)f","FAdd",True,"add two floats")
intrinsic(_flt_minus,"F(ff)f","FSub",True,"subtract two floats")
intrinsic(_flt_times,"F(ff)f","FMul",True,"multiply two floats")
intrinsic(_flt_div,"F(ff)f","FDiv",True,"divide two floats")
intrinsic(_flt_mod,"F(ff)f","FMod",True,"modulo remainder")

intrinsic(_int_abs,"F(i)i","IAbs",True,"integer absolute value")
intrinsic(_flt_abs,"F(f)f","FAbs",True,"float absolute value")

intrinsic(_int_eq,"F(ii)l","IEq",False,"integer equality")
intrinsic(_int_lt,"F(ii)l","ILt",False,"integer less than")
intrinsic(_int_ge,"F(ii)l","IGe",False,"integer greater or equal")

intrinsic(_flt_eq,"F(fff)l","FEq",False,"float equality")
intrinsic(_flt_lt,"F(ff)l","FLt",False,"float less than")
intrinsic(_flt_ge,"F(ff)l","FGe",False,"float greater or equal")

intrinsic(_band,"F(ii)i","BAnd",True,"bitwise and two integers")
intrinsic(_bor,"F(ii)i","BOr",True,"bitwise or two integers")
intrinsic(_bxor,"F(ii)i","BXor",True,"bitwise xor two integers")
intrinsic(_blsl,"F(ii)i","BLsl",True,"logical left shift")
intrinsic(_blsr,"F(ii)i","BLsr",True,"logical right shift")
intrinsic(_basr,"F(ii)i","BAsr",True,"arithmetic right shift")
intrinsic(_bnot,"F(i)i","BNot",True,"bitwise negate number")

intrinsic(_fiber_eq,":k's':k'r'F("fiberType("k's'","k'r'")fiberType("k's'","k'r'")")l","TEq",True,"compare two fiber identifiers")
intrinsic(_new_fiber,":k's':k'r'F(F("fiberType("k'r'","k's'")"k'r')k's')"fiberType("k'r'","k's'"),"Fiber",True,"create a new fiber")
intrinsic(_stack_split,":k's':k'r'F(F("fiberType("k'r'","k's'")")k's')k's'","Spawn",True,"spawn a new task")
intrinsic(_suspend_fiber,":k's':k'r'F("fiberType("k's'","k'r'")"k's')k'r'","Suspend", True,"suspend a fiber")
intrinsic(_retire_fiber,":k's':k'r'F("fiberType("k's'","k'r'")"k's')()","Retire",False,"retire a fiber")
intrinsic(_resume_fiber,":k's':k'r'F("fiberType("k's'","k'r'")"k'r')k's'","Resume" ,True,"resume a fiber")

intrinsic(_cell, ":k't'F(k't')rk't'", "Cell", True, "create a reference cell")
intrinsic(_get, ":k't'F(rk't')k't'", "Get", False,"access contents of reference cell")
intrinsic(_assign, ":k't'F(rk't'k't')()", "Assign", False, "update contents of reference cell")

