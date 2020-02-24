/*
  This is where you define an intrinsic function that the compiler converts to assembler
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
 */


/* Define the standard intrinsics */
intrinsic(_exit,"F(i)()","Halt","terminate engine")

intrinsic(_int_plus,"F(ii)i","IAdd","add two integers")
intrinsic(_int_minus,"F(ii)i","ISub","subtract two integers")
intrinsic(_int_times,"F(ii)i","IMul","multiply two integers")
intrinsic(_int_div,"F(ii)i","IDiv","divide two integers")
intrinsic(_int_mod,"F(ii)i","IMod","modulo remainder")

intrinsic(_flt_plus,"F(ff)f","FAdd","add two floats")
intrinsic(_flt_minus,"F(ff)f","FSub","subtract two floats")
intrinsic(_flt_times,"F(ff)f","FMul","multiply two floats")
intrinsic(_flt_div,"F(ff)f","FDiv","divide two floats")
intrinsic(_flt_mod,"F(ff)f","FMod","modulo remainder")

intrinsic(_int_abs,"F(i)i","IAbs","integer absolute value")
intrinsic(_flt_abs,"F(f)f","FAbs","float absolute value")

intrinsic(_int_eq,"F(ii)l","IEq","integer equality")
intrinsic(_int_lt,"F(ii)l","ILt","integer less than")
intrinsic(_int_ge,"F(ii)l","IGe","integer greater or equal")

intrinsic(_flt_eq,"F(fff)l","FEq","float equality")
intrinsic(_flt_lt,"F(ff)l","FLt","float less than")
intrinsic(_flt_ge,"F(ff)l","FGe","float greater or equal")

intrinsic(_band,"F(ii)i","IAnd","bitwise and two integers")
intrinsic(_bor,"F(ii)i","IOr","bitwise or two integers")
intrinsic(_bxor,"F(ii)i","IXor","bitwise xor two integers")
intrinsic(_blsl,"F(ii)i","Lsl","logical left shift")
intrinsic(_blsr,"F(ii)i","Lsr","logical right shift")
intrinsic(_basr,"F(ii)i","Asr","arithmetic right shift")
intrinsic(_bnot,"F(i)i","INot","bitwise negate number")
intrinsic(_nthb,"F(ii)l","ITst","is nth bit set?")
intrinsic(_bcount,"F(i)i","ICnt","count the number of bits set")

