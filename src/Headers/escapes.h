/*
  This is where you define a new escape function so that the compiler and
  the run-time system can see it
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
 */

/* Declare standard types used in escapes */

#define processState "t'star.thread*threadState'"
#define threadType "t'star.thread*thread'"
#define ioHandle "t'ioHandle'"
#define optionType(T) "Uz1'star.core*option'" T
#define futureType(F,E) "UUz2'future'" F E
#define ERRCODE "t'star.core*errorCode'"

/* Define the standard escapes */
escape(_exit, "F(i)()", "terminate engine")
escape(_abort, ":k'a'F(k'a's)()", "abort process")

escape(_definedLbl, "F(si)l", "test for defined name")
escape(_callLbl, "F(siLLs)()", "invoke defined name")
escape(_globalIsSet,"F(s)l","test if a global var is set")

escape(_int_plus, "F(ii)i", "add two integers")
escape(_int_minus, "F(ii)i", "subtract two integers")
escape(_int_times, "F(ii)i", "multiply two integers")
escape(_int_div, "|F(ii)ir"ERRCODE, "divide two integers")
escape(_int_mod, "|F(ii)ir"ERRCODE, "modulo remainder")
escape(_int_hash, "F(i)i", "compute hash of integer")
escape(_int_gcd, "|F(ii)ir"ERRCODE, "gcd of two integers")

escape(_int_lg2,"|F(i)ir"ERRCODE,"integer log2")

escape(_flt_plus, "F(ff)f", "add two floats")
escape(_flt_minus, "F(ff)f", "subtract two floats")
escape(_flt_times, "F(ff)f", "multiply two floats")
escape(_flt_div, "|F(ff)fr"ERRCODE, "divide two floats")
escape(_flt_mod, "|F(ff)fr"ERRCODE, "modulo remainder")

escape(_int_abs, "F(i)i", "integer absolute value")
escape(_flt_abs, "F(f)f", "float absolute value")

escape(_int_eq, "F(ii)l", "integer equality")
escape(_int_lt, "F(ii)l", "integer less than")
escape(_int_ge, "F(ii)l", "integer greater or equal")

escape(_flt_eq, "F(ff)l", "float equality")
escape(_flt_lt, "F(ff)l", "float less than")
escape(_flt_ge, "F(ff)l", "float greater or equal")

escape(_int2flt, "F(i)f", "convert integer to float")
escape(_flt2int, "F(f)i", "convert float to integer")

escape(_bits_float, "F(i)f", "convert a bit pattern to a float")
escape(_float_bits, "F(f)i", "convert a float into a bit pattern")

escape(_flt_hash, "F(f)i", "compute hash of float")
escape(_flt_pwr, "F(ff)f", "raise X to the power Y")

escape(_big_plus, "F(bb)b", "add two bigints")
escape(_big_minus, "F(bb)b", "subtract two bigints")
escape(_big_times, "F(bb)b", "multiply two bigints")
escape(_big_div, "|F(bb)(bb)r"ERRCODE, "divide two bigints, return quotient & remainder")
escape(_big_bitand, "F(bb)b", "bitwise and of two bigints")
escape(_big_bitor, "F(bb)b", "bitwise or of two bigints")
escape(_big_bitxor, "F(bb)b", "bitwise exclusive or of two bigints")
escape(_big_bitnot, "F(b)b", "bitwise negation of a bigint")

escape(_big_gcd, "|F(bb)br"ERRCODE, "gcd of two bigints")
escape(_big_hash, "F(b)i", "compute hash of bigint")

escape(_big_eq, "F(bb)l", "bigint equality")
escape(_big_lt, "F(bb)l", "bigint less than")
escape(_big_ge, "F(bb)l", "bigint greater or equal")

escape(_int2big, "F(i)b", "convert integer to bigint")
escape(_big2int, "F(b)" optionType("i"), "convert bigint to integer")
escape(_ints2big, "F(Li)b", "convert list of integers to bigint")
escape(_big2ints, "F(b)Li", "convert bigint to list of integers")

escape(_str2big, "F(s)" optionType("b"), "convert string to bigint")
escape(_big2str, "F(b)s", "convert bigint to string")

escape(_big_format, "|F(bs)sr"ERRCODE, "format a big integer")

escape(_fiber_eq,":k's':k'r'F(x(k's')k'r'x(k's')k'r')l","compare two fiber identifiers")

escape(sqrt, "|F(f)fr"ERRCODE, "square root")
escape(exp, "|F(f)fr"ERRCODE, "exponential")
escape(log, "F(f)f", "logarithm")
escape(log10, "F(f)f", "10-based logarithm")
escape(pi, "F()f", "return PI")
escape(sin, "F(f)f", "sine")
escape(cos, "F(f)f", "cosine")
escape(tan, "F(f)f", "tangent")
escape(asin, "F(f)f", "arc sine")
escape(acos, "F(f)f", "arc cosine")
escape(atan, "F(f)f", "arc tangent")

escape(trunc, "F(f)f", "truncate to nearest integer")
escape(floor, "F(f)f", "truncate to lower integer")
escape(ceil, "F(f)f", "truncate to next integer")
escape(integral, "F(f)l", "test if number is integral")

escape(_irand, "F(i)i", "generate random integer")
escape(_random, "F()f", "generate random float in range [0..1)")
escape(_seed, "F(i)()", "set random number seed")

escape(_ldexp, "F(fi)f", "raise x to 2**y")
escape(_frexp, "F(f)(fi)", "split x into mant and exp")
escape(_modf, "F(f)(fi)", "split x into int and frac")

escape(_band, "F(ii)i", "bitwise and two integers")
escape(_bor, "F(ii)i", "bitwise or two integers")
escape(_bxor, "F(ii)i", "bitwise xor two integers")
escape(_blsl, "F(ii)i", "logical left shift")
escape(_blsr, "F(ii)i", "logical right shift")
escape(_basr, "F(ii)i", "arithmetic right shift")
escape(_bnot, "F(i)i", "bitwise negate number")

escape(_cell, ":k't'F(k't')rk't'", "create a reference cell")
escape(_get, ":k't'F(rk't')k't'", "access contents of reference cell")
escape(_assign, ":k't'F(rk't'k't')()", "update contents of reference cell")

escape(_futureIsResolved, ":k'f':k'e'F("futureType("k'f'","k'e'")")l", "test to see if a future has been set")
escape(_futureIsAccepted, ":k'f':k'e'F("futureType("k'f'","k'e'")")l", "test to see if a future has been accepted")
escape(_futureIsRejected, ":k'f':k'e'F("futureType("k'f'","k'e'")")l", "test to see if a future has been rejected")
escape(_futureVal, ":k'f':k'e'|F("futureType("k'f'","k'e'")")k'f'rk'e'", "get the value of the future")

escape(_tuple_nth, ":k't':k'e'F(k't'i)k'e'", "Access tuple element")
escape(_tuple_set_nth, ":k't':k'e'F(k't'ik'e')k't'", "Update tuple element")

escape(_cwd, "F()s", "return url of current working directory")
escape(_cd, "|F(s)()r"ERRCODE, "change current working directory")
escape(_rm, "|F(s)()r"ERRCODE, "remove file")
escape(_mv, "|F(ss)()r"ERRCODE, "rename file")
escape(_mkdir, "|F(si)()r"ERRCODE, "create directory")
escape(_rmdir, "|F(s)()r"ERRCODE, "delete directory")
escape(_isdir, "F(s)l", "is directory present")
escape(_file_chmod, "|F(si)()r"ERRCODE, "change mode of a file or directory")
escape(_ls, "|F(s)Lsr"ERRCODE, "return a array of files in a directory")
escape(_repo, "F()s", "return the standard repo directory name")

escape(_file_mode, "|F(s)ir"ERRCODE, "report modes of a file")
escape(_file_present, "F(s)l", "check presence of a file")
escape(_file_type, "|F(s)ir"ERRCODE, "report on the type of a file")
escape(_file_size, "|F(s)ir"ERRCODE, "report on the size of a file")
escape(_file_modified, "|F(s)ir"ERRCODE, "report on when a file was last modified")
escape(_file_date, "|F(s)(iii)r"ERRCODE, "report on file access time and modification times")

escape(_openInFile, "|F(si)"ioHandle"r"ERRCODE, "open input file")
escape(_openOutFile, "|F(si)"ioHandle"r"ERRCODE, "open output file")
escape(_openAppendFile, "|F(si)"ioHandle"r"ERRCODE, "open output file")
escape(_openAppendIOFile, "|F(si)"ioHandle"r"ERRCODE, "open output file")

escape(_popen, "|F(sLsL(ss))("ioHandle ioHandle ioHandle")r"ERRCODE, "open a pipe")

escape(_close, "|F("ioHandle")()r"ERRCODE, "close file")
escape(_end_of_file, "F("ioHandle")l", "end of file test")
escape(_inchars, "|F("ioHandle"i)sr"ERRCODE, "read block string")
escape(_inchars_async, "|F("ioHandle"i)"futureType("s",ERRCODE)"r"ERRCODE, "async read block string")
escape(_inchar, "|F("ioHandle")cr"ERRCODE, "read single character")
escape(_inchar_async, "|F("ioHandle")"futureType("c",ERRCODE)"r"ERRCODE, "async read single character")
escape(_inbyte, "|F("ioHandle")ir"ERRCODE, "read single byte")
escape(_inbyte_async, "|F("ioHandle")"futureType("i",ERRCODE)"r"ERRCODE, "async ead single byte")
escape(_inbytes,"|F("ioHandle"i)Vir"ERRCODE,"read block of bytes")
escape(_inbytes_async,"|F("ioHandle"i)"futureType("Vi",ERRCODE)"r"ERRCODE,"async read block of bytes")
escape(_inline, "|F("ioHandle")sr"ERRCODE, "read a line")
escape(_inline_async, "|F("ioHandle")"futureType("s",ERRCODE)"r"ERRCODE, "async read of a line")
escape(_outchar, "|F("ioHandle"c)()r"ERRCODE, "write a single character")
escape(_outchar_async, "|F("ioHandle"c)"futureType("()",ERRCODE)"r"ERRCODE, "async write a single character")
escape(_outbyte, "|F("ioHandle"i)()r"ERRCODE, "write a single byte")
escape(_outbyte_async, "|F("ioHandle"i)"futureType("()",ERRCODE)"r"ERRCODE, "async write a single byte")
escape(_outbytes, "|F("ioHandle"Li)()r"ERRCODE, "write a list of bytes")
escape(_outtext, "|F("ioHandle"s)()r"ERRCODE, "write a string as a block")
escape(_outtext_async, "|F("ioHandle"s)"futureType("()",ERRCODE)"r"ERRCODE, "async write a string as a block")
escape(_stdfile, "F(i)"ioHandle, "standard file descriptor")
escape(_fposition, "F("ioHandle")i", "report current file position")
escape(_fseek, "|F("ioHandle"i)()r"ERRCODE, "seek to new file position")
escape(_flush, "|F("ioHandle")()r"ERRCODE, "flush the I/O buffer")
escape(_flushall, "F()()", "flush all files")
escape(_setfileencoding, "F("ioHandle"i)()", "set file encoding on file")

escape(_get_file, "|F(s)sr"ERRCODE, "file into a char sequence")
escape(_getfile_async, "|F(s)"futureType("s",ERRCODE)"r"ERRCODE, "file into a string")
escape(_put_file, "|F(ss)()r"ERRCODE, "write string into file")
escape(_put_file_async, "|F(ss)"futureType("()",ERRCODE)"r"ERRCODE, "async write string into file")
escape(_show, "F(s)()", "show something on console")

escape(_install_pkg, "F(s)L(ss)", "define package from string contents")
escape(_pkg_is_present, "F(ss)l", "True if an identified package is available")
escape(_in_manifest, "F(sss)l", "True if pkg/version/kind is present in manifest")
escape(_locate_in_manifest, "|F(sss)sr"ERRCODE, "Access manifest resource")

escape(_logmsg, "F(s)()", "log a message in logfile or console")
escape(_display_depth,"F()i", "Current standard display depth")

// Socket handling functions
escape(_connect, "|F(sii)("ioHandle ioHandle")r"ERRCODE, "connect to remote host")
escape(_listen, "|F(i)"ioHandle"r"ERRCODE, "listen on a port")
escape(_accept, "|F("ioHandle")("ioHandle ioHandle "sis)r"ERRCODE, "accept connection")

escape(_hosttoip, "F(s)Ls", "IP address of host")
escape(_iptohost, "|F(s)sr"ERRCODE, "host name from IP")

// Timing and delaying
escape(_delay, "|F(f)()r"ERRCODE, "delay for period of time")
escape(_sleep, "|F(f)()r"ERRCODE, "sleep until a definite time")
escape(_now, "F()f", "current time")
escape(_today, "F()f", "time at midnight")
escape(_ticks, "F()i", "used CPU time")
escape(_time2date, "F(f)(iiiiiifi)", "convert a time to a date")
escape(_time2utc, "F(f)(iiiiiifi)", "convert a time to UTC date")
escape(_date2time, "F(iiiiifi)f", "convert a date to a time")
escape(_utc2time, "F(iiiiifi)f", "convert a UTC date to a time")
escape(_formattime,"|F(fs)sr"ERRCODE,"format a time value")
escape(_parsetime,"F(ss)" optionType("f"),"parse a date expression guided by format string")

// Character class escapes
escape(_uniCodeCategory, "F(c)i", "unicode category")

escape(_isCcChar, "F(c)l", "is Other, control char")
escape(_isCfChar, "F(c)l", "is Other, format char")
escape(_isCnChar, "F(c)l", "is Other, unassigned char")
escape(_isCoChar, "F(c)l", "is Other, private char")
escape(_isCsChar, "F(c)l", "is Other, surrogate char")
escape(_isLlChar, "F(c)l", "is Letter, lowercase char")
escape(_isLmChar, "F(c)l", "is Letter, modifier char")
escape(_isLoChar, "F(c)l", "is Letter, other char")
escape(_isLtChar, "F(c)l", "is Letter, title char")
escape(_isLuChar, "F(c)l", "is Letter, uppercase char")
escape(_isMcChar, "F(c)l", "is Mark, spacing char")
escape(_isMeChar, "F(c)l", "is Mark, enclosing char")
escape(_isMnChar, "F(c)l", "is Mark, nonspacing char")
escape(_isNdChar, "F(c)l", "is Number, decimal digit")
escape(_isNlChar, "F(c)l", "is Number, letter char")
escape(_isNoChar, "F(c)l", "is Number, other char")
escape(_isPcChar, "F(c)l", "is Punctuation, connector")
escape(_isPdChar, "F(c)l", "is Punctuation, dash char")
escape(_isPeChar, "F(c)l", "is Punctuation, close char")
escape(_isPfChar, "F(c)l", "is Punctuation, final quote")
escape(_isPiChar, "F(c)l", "is Punctuation, initial quote")
escape(_isPoChar, "F(c)l", "is Punctuation, other char")
escape(_isPsChar, "F(c)l", "is Punctuation, open char")
escape(_isScChar, "F(c)l", "is Symbol, currency char")
escape(_isSkChar, "F(c)l", "is Symbol, modifier char")
escape(_isSmChar, "F(c)l", "is Symbol, math char")
escape(_isSoChar, "F(c)l", "is Symbol, other char")
escape(_isZlChar, "F(c)l", "is Separator, line char")
escape(_isZpChar, "F(c)l", "is Separator, para char")
escape(_isZsChar, "F(c)l", "is Separator, space char")

escape(_isLetterChar, "F(c)l", "is letter char")
escape(_digitCode, "F(c)i", "convert char to num")

escape(_codePoint, "F(c)i", "convert char to code point integer")
escape(_char, "F(i)c", "convert integer code point to char")

escape(_isIDStart, "F(c)l", "is start char of identifier")
escape(_isIDContinue, "F(c)l", "is continue char of identifier")

// String handling escapes
escape(_int2str, "F(iiii)s", "format an integer as a string")
escape(_flt2str, "F(ficl)s", "format a floating as a string")
escape(_int_format, "|F(is)sr"ERRCODE, "format an integer using picture format")
escape(_flt_format, "F(fs)s", "format a floating point using picture format")

escape(_str2flt, "F(s)" optionType("f"), "parse a string as a float")
escape(_str2int, "F(s)" optionType("i"), "parse a string as an integer")

escape(_chr_eq, "F(cc)l", "Character equality")
escape(_chr_lt, "F(cc)l", "Character 1 is less than character 2")
escape(_chr_ge, "F(cc)l", "Character 1 is greater than or equals to character 2")

escape(_chr_hash, "F(c)i", "Compute hash of a character")
escape(_chr_quote, "F(c)s", "Display string for a character")
escape(_chr_format, "F(cs)s", "format a character using picture format")

escape(_str_eq, "F(ss)l", "String equality")
escape(_str_lt, "F(ss)l", "String 1 is less than string 2")
escape(_str_ge, "F(ss)l", "String 1 is greater than or equals to string 2")

escape(_str_hash, "F(s)i", "Compute hash of string")
escape(_str_len, "F(s)i", "return length of char sequence")

escape(_str_gen, "F(s)s", "Generate a unique string")

escape(_stringOf, ":k't'F(k't'i)s", "Display a general term")

escape(_explode, "F(s)Lc", "convert char sequence to list of chars")
escape(_implode, "F(Lc)s", "convert list of code points to char sequence")

escape(_str_find, "F(ssi)i", "find a substring in string")
escape(_sub_str, "F(sii)s", "extract a substring")
escape(_str_split, "F(si)(ss)", "split a string at a point")
escape(_str_concat, "F(ss)s", "Concatenate two strings")
escape(_str_reverse, "F(s)s", "Unicode reverse of string")
escape(_str_start, "F(ss)l", "True if second string starts with first")
escape(_str_end, "F(ss)l", "True if second string ends with first")
escape(_str_splice, "F(siis)s", "splice a substring into a string")

escape(_str_multicat, "F(Ls)s", "Concatenate a list of strings into one")
escape(_str_hdtl, "F(s)(cs)", "pick up the first character and return remainder")
escape(_str_back, "F(s)(sc)", "pick up the last character and return remainder")
escape(_str_cons, "F(cs)s", "put a char in the front")
escape(_code2str, "F(c)s", "make a 1 char string")
escape(_str_apnd, "F(sc)s", "put a char in the back")
escape(_str_charat, "F(si)"optionType("c"), "index into string")
escape(_str_quote, "F(s)s", "construct a quoted version of a string")
escape(_str_format, "F(ss)s", "apply formatting to a char sequence")

escape(_getenv, "F(ss)s", "get an environment variable")
escape(_setenv, "|F(ss)()r"ERRCODE, "set an environment variable")
escape(_envir, "F()L(ss)", "return entire environment")

// Process manipulation
escape(_fork, "F(F()())"threadType, "fork new process")
escape(_thread, "F()"threadType"", "report thread of current process")
escape(_kill, "|F("threadType")()r"ERRCODE, "kill off a process")
escape(_thread_state, "F("threadType ")" processState, "state of process")
escape(_waitfor, "|F("threadType")()r"ERRCODE, "wait for other thread to terminate")

escape(_shell, "|F(sLsL(ss))ir"ERRCODE, "Run a shell cmd")

escape(_ins_debug, "F()()", "set instruction-level")
escape(_stackTrace, "F()s", "Print a stack trace")

#undef processState
#undef threadType
#undef ioHandle
#undef optionType
#undef futureType
#undef ERRCODE
