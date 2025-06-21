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
intrinsic(_int_plus, func(int int, int),"IAdd",True,"add two integers")
intrinsic(_int_minus, func(int int, int),"ISub",True,"subtract two integers")
intrinsic(_int_times, func(int int, int),"IMul",True,"multiply two integers")
intrinsic(_int_div,throws(int int, int,ERR),"IDiv",True,"divide two integers")
intrinsic(_int_mod,throws(int int, int,ERR),"IMod",True,"modulo remainder")

intrinsic(_flt_plus,func(flt flt,flt),"FAdd",True,"add two floats")
intrinsic(_flt_minus,func(flt flt,flt),"FSub",True,"subtract two floats")
intrinsic(_flt_times,func(flt flt,flt),"FMul",True,"multiply two floats")
intrinsic(_flt_div, throws(flt flt,flt,ERR),"FDiv",True,"divide two floats")
intrinsic(_flt_mod, throws(flt flt,flt,ERR),"FMod",True,"modulo remainder")

intrinsic(_int_abs,func(int, int),"IAbs",True,"integer absolute value")
intrinsic(_flt_abs,func(flt,flt),"FAbs",True,"float absolute value")

intrinsic(_int_eq,func(int int,bool),"IEq",False,"integer equality")
intrinsic(_int_lt,func(int int,bool),"ILt",False,"integer less than")
intrinsic(_int_ge,func(int int,bool),"IGe",False,"integer greater or equal")

intrinsic(_chr_eq, func(chr chr,bool),"CEq",False,"character equality")
intrinsic(_chr_lt, func(chr chr,bool),"CLt",False,"character less than")
intrinsic(_chr_ge, func(chr chr,bool),"CGe",False,"character greater or equal")

intrinsic(_flt_eq,func(flt flt,bool),"FEq",False,"float equality")
intrinsic(_flt_lt,func(flt flt,bool),"FLt",False,"float less than")
intrinsic(_flt_ge,func(flt flt,bool),"FGe",False,"float greater or equal")

intrinsic(_band,func(int int,int),"BAnd",True,"bitwise and two integers")
intrinsic(_bor,func(int int,int),"BOr",True,"bitwise or two integers")
intrinsic(_bxor,func(int int,int),"BXor",True,"bitwise xor two integers")
intrinsic(_blsl,func(int int,int),"BLsl",True,"logical left shift")
intrinsic(_blsr,func(int int,int),"BLsr",True,"logical right shift")
intrinsic(_basr,func(int int,int),"BAsr",True,"arithmetic right shift")
intrinsic(_bnot,func(int,int),"BNot",True,"bitwise negate number")

intrinsic(_fiber,all(r,all(s,func(func(fiber(r,s),r) s,fiber(r,s)))),"Fiber",True,"create a new fiber")

#undef processState
#undef thread
#undef io
#undef ERR
#undef option
#undef either
#undef future
#undef fiber
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

