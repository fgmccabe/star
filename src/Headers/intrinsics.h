/*
  This is where you define an intrinsic function that the compiler converts to assembler
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
 */

#define processState "t'star.thread*threadState'"
#define thread "t'star.thread*thread'"
#define io "t'ioHandle'"
#define option(T) "Uz1'option'" T
#define either(E,O) "UUz2'star.either*either'" E O
#define future(F,E) "UUz2'future'" F E
#define fiber(R,S) "UUz2'fiber'" R S
#define raises(A,E) "|"A"r"E
#define throws(A,R,E) "T" tpl(A) R E
#define func(A,R) "F" tpl(A) R
#define tpl(E) "(" E ")"
#define vec(E) "V" E
#define ref(E) "r" E
#define bool "l"
#define chr "c"
#define int "i"
#define flt "f"
#define big "b"
#define str "s"
#define lst(T) "l" T
#define all(V,T) ":" V T
#define vr(V) "k'"V"'"
#define e vr("e")
#define a vr("a")
#define b vr("b")
#define r vr("r")
#define s vr("s")
#define unit tpl(/**/)

#define ERR "t'errorCode'"

/* Define the standard intrinsics */
intrinsic(_abort,all(a,all(e,func(a str,e))),"Abort",False, Last, "abort process")
intrinsic(_int_plus, func(int int, int),"IAdd",True,NotLast,"add two integers")
intrinsic(_int_minus, func(int int, int),"ISub",True,NotLast,"subtract two integers")
intrinsic(_int_times, func(int int, int),"IMul",True,NotLast,"multiply two integers")
intrinsic(_int_div,throws(int int, int,ERR),"IDiv",True,NotLast,"divide two integers")
intrinsic(_int_mod,raises(func(int int, int),ERR),"IMod",True,NotLast,"modulo remainder")

intrinsic(_flt_plus,func(flt flt,flt),"FAdd",True,NotLast,"add two floats")
intrinsic(_flt_minus,func(flt flt,flt),"FSub",True,NotLast,"subtract two floats")
intrinsic(_flt_times,func(flt flt,flt),"FMul",True,NotLast,"multiply two floats")
intrinsic(_flt_div, raises(func(flt flt,flt),ERR),"FDiv",True,NotLast,"divide two floats")
intrinsic(_flt_mod, raises(func(flt flt,flt),ERR),"FMod",True,NotLast,"modulo remainder")

intrinsic(_int_abs,func(int, int),"IAbs",True,NotLast,"integer absolute value")
intrinsic(_flt_abs,func(flt,flt),"FAbs",True,NotLast,"float absolute value")

intrinsic(_int_eq,func(int int,bool),"IEq",False,NotLast,"integer equality")
intrinsic(_int_lt,func(int int,bool),"ILt",False,NotLast,"integer less than")
intrinsic(_int_ge,func(int int,bool),"IGe",False,NotLast,"integer greater or equal")

intrinsic(_chr_eq, func(chr chr,bool),"CEq",False,NotLast,"character equality")
intrinsic(_chr_lt, func(chr chr,bool),"CLt",False,NotLast,"character less than")
intrinsic(_chr_ge, func(chr chr,bool),"CGe",False,NotLast,"character greater or equal")

intrinsic(_flt_eq,func(flt flt,bool),"FEq",False,NotLast,"float equality")
intrinsic(_flt_lt,func(flt flt,bool),"FLt",False,NotLast,"float less than")
intrinsic(_flt_ge,func(flt flt,bool),"FGe",False,NotLast,"float greater or equal")

intrinsic(_band,func(int int,int),"BAnd",True,NotLast,"bitwise and two integers")
intrinsic(_bor,func(int int,int),"BOr",True,NotLast,"bitwise or two integers")
intrinsic(_bxor,func(int int,int),"BXor",True,NotLast,"bitwise xor two integers")
intrinsic(_blsl,func(int int,int),"BLsl",True,NotLast,"logical left shift")
intrinsic(_blsr,func(int int,int),"BLsr",True,NotLast,"logical right shift")
intrinsic(_basr,func(int int,int),"BAsr",True,NotLast,"arithmetic right shift")
intrinsic(_bnot,func(int,int),"BNot",True,NotLast,"bitwise negate number")

intrinsic(_fiber,all(r,all(s,func(func(fiber(r,s),r) s,fiber(r,s)))),"Fiber",True,NotLast,"create a new fiber")

#undef processState
#undef thread
#undef io
#undef ERR
#undef option
#undef either
#undef future
#undef fiber
#undef raises
#undef throws
#undef func
#undef tpl
#undef bool
#undef chr
#undef int
#undef flt
#undef big
#undef str
#undef lst
#undef all
#undef vr
#undef e
#undef a
#undef b
#undef r
#undef s

