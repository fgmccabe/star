/*
  This is where you define a new escape function so that the compiler and
  the run-time system can see it
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
 */

/* Declare standard types used in escapes */

#define processState "t'star.thread*threadState'"
#define thread "t'star.thread*thread'"
#define io "t'ioHandle'"
#define option(T) "Uz1'option'" T
#define either(E,O) "UUz2'star.either*either'" E O
#define future(F,E) "UUz2'future'" F E
#define fiber(R,S) "UUz2'fiber'" R S
#define raises(A,E) "|"A"r"E
#define throws(A,E) "|"A"t"E
#define func(A,R) "F" tpl(A) R
#define tpl(E) "(" E ")"
#define vec(E) "V" E
#define ref(E) "r" E
#define bool "l"
#define chr "c"
#define int "i"
#define flt "f"
#define big "b"
#define strng "s"
#define lst(T) "L" T
#define all(V,T) ":" V T
#define vr(V) "k'"V"'"
#define e vr("e")
#define a vr("a")
#define r vr("r")
#define s vr("s")
#define unit tpl(/**/)

#define ERR "t'errorCode'"

/* Define the standard escapes */
escape(_exit, all(e,func(int,e)), "terminate engine")
escape(_abort, all(a,all(e,func(a strng,e))), "abort process")

escape(_definedLbl, func(strng int,bool), "test for defined name")
escape(_globalIsSet,func(strng, bool),"test if a global var is set")

escape(_int_plus, func(int int, int), "add two integers")
escape(_int_minus, func(int int, int), "subtract two integers")
escape(_int_times, func(int int, int), "multiply two integers")
escape(_int_div, raises(func(int int, int),ERR), "divide two integers")
escape(_int_mod, raises(func(int int, int),ERR), "modulo remainder")
escape(_int_hash, func(int, int), "compute hash of integer")
escape(_int_gcd, raises(func(int int,  int),ERR), "gcd of two integers")

escape(_int_lg2,raises(func(int, int),ERR),"integer log2")

escape(_flt_plus, func(flt flt,flt), "add two floats")
escape(_flt_minus, func(flt flt,flt), "subtract two floats")
escape(_flt_times, func(flt flt,flt), "multiply two floats")
escape(_flt_div, raises(func(flt flt,flt),ERR), "divide two floats")
escape(_flt_mod, raises(func(flt flt,flt),ERR), "modulo remainder")

escape(_int_abs, func(int, int), "integer absolute value")
escape(_flt_abs, func(flt,flt), "float absolute value")

escape(_int_eq, func(int int,bool), "integer equality")
escape(_int_lt, func(int int,bool), "integer less than")
escape(_int_ge, func(int int,bool), "integer greater or equal")

escape(_int_pow, raises(func(int int, int),ERR), "integer exponentiation")

escape(_flt_eq, func(flt flt,bool), "float equality")
escape(_flt_lt, func(flt flt,bool), "float less than")
escape(_flt_ge, func(flt flt,bool), "float greater or equal")

escape(_int2flt, func(int,flt), "convert integer to float")
escape(_flt2int, func(flt,int), "convert float to integer")

escape(_bits_float, func(int,flt), "convert a bit pattern to a float")
escape(_float_bits, func(flt,int), "convert a float into a bit pattern")

escape(_flt_hash, func(flt,int), "compute hash of float")
escape(_flt_pwr, func(flt flt,flt), "raise X to the power Y")

escape(_big_plus, func(big big,big), "add two bigints")
escape(_big_minus, func(big big,big), "subtract two bigints")
escape(_big_times, func(big big,big), "multiply two bigints")
escape(_big_div, raises(func(big big,tpl(big big)),ERR), "divide two bigints, return quotient & remainder")
escape(_big_bitand, func(big big,big), "bitwise and of two bigints")
escape(_big_bitor, func(big big,big), "bitwise or of two bigints")
escape(_big_bitxor, func(big big,big), "bitwise exclusive or of two bigints")
escape(_big_bitnot, func(big,big), "bitwise negation of a bigint")

escape(_big_gcd, raises(func(big big,big),ERR), "gcd of two bigints")
escape(_big_hash, func(big,int), "compute hash of bigint")

escape(_big_eq, func(big big,bool), "bigint equality")
escape(_big_lt, func(big big,bool), "bigint less than")
escape(_big_ge, func(big big,bool), "bigint greater or equal")

escape(_int2big, func(int,big), "convert integer to bigint")
escape(_big2int, func(big,option(int)), "convert bigint to integer")
escape(_ints2big, func(lst(int),big), "convert list of integers to bigint")
escape(_big2ints, func(big,lst(int)), "convert bigint to list of integers")

escape(_str2big, func(strng,option(big)), "convert string to bigint")
escape(_big2str, func(big,strng), "convert bigint to string")

escape(_big_format, raises(func(big strng,strng),ERR), "format a big integer")

escape(_fiber_eq,all(r,all(s,func(fiber(r,s) fiber(r,s),bool))),"compare two fiber identifiers")
escape(_fiber,all(r,all(s,func(func(fiber(r,s) r,s),fiber(r,s)))),"create a new fiber")

escape(sqrt, raises(func(flt,flt),ERR), "square root")
escape(exp, raises(func(flt,flt),ERR), "exponential")
escape(log, func(flt,flt), "logarithm")
escape(log10, func(flt,flt), "10-based logarithm")
escape(pi, func(/**/,flt), "return PI")
escape(sin, func(flt,flt), "sine")
escape(cos, func(flt,flt), "cosine")
escape(tan, func(flt,flt), "tangent")
escape(asin, func(flt,flt), "arc sine")
escape(acos, func(flt,flt), "arc cosine")
escape(atan, func(flt,flt), "arc tangent")

escape(trunc, func(flt,flt), "truncate to nearest integer")
escape(floor, func(flt,flt), "truncate to lower integer")
escape(ceil, func(flt,flt), "truncate to next integer")
escape(integral, func(flt,bool), "test if number is integral")

escape(_irand, func(int,int), "generate random integer")
escape(_random, func(/**/,flt), "generate random float in range [0..1)")
escape(_seed, func(int,unit), "set random number seed")

escape(_ldexp, func(flt int,flt), "raise x to 2**y")
escape(_frexp, func(flt, tpl(flt int)), "split x into mant and exp")
escape(_modf, func(flt, tpl(flt int)), "split x into int and frac")

escape(_band, func(int int,int), "bitwise and two integers")
escape(_bor, func(int int,int), "bitwise or two integers")
escape(_bxor, func(int int,int), "bitwise xor two integers")
escape(_blsl, func(int int,int), "logical left shift")
escape(_blsr, func(int int,int), "logical right shift")
escape(_basr, func(int int,int), "arithmetic right shift")
escape(_bnot, func(int,int), "bitwise negate number")

escape(_cell_future,all(s,all(e,func(ref(either(s,e)),future(s,e)))),"create a user-programmable future")

escape(_futureIsResolved,all(a,all(e,func(future(a,e),bool))), "test to see if a future has been set")
escape(_futureIsAccepted, all(a,all(e,func(future(a,e),bool))), "test to see if a future has been accepted")
escape(_futureIsRejected, all(a,all(e,func(future(a,e),bool))), "test to see if a future has been rejected")
escape(_futureVal, all(a,all(e,raises(func(future(a,e),a),e))), "get the value of the future")

escape(_tuple_nth, all(a,all(e,func(a int,e))), "Access tuple element")
escape(_tuple_set_nth, all(a,all(e,func(a int e,a))), "Update tuple element")

escape(_cwd, func(/**/,strng), "return url of current working directory")
escape(_cd, raises(func(strng,unit),ERR), "change current working directory")
escape(_rm, raises(func(strng,unit),ERR), "remove file")
escape(_mv, raises(func(strng strng,unit),ERR), "rename file")
escape(_mkdir, raises(func(strng int,unit),ERR), "create directory")
escape(_rmdir, raises(func(strng,unit),ERR), "delete directory")
escape(_isdir, func(strng,bool), "is directory present")
escape(_file_chmod, raises(func(strng int,unit),ERR), "change mode of a file or directory")
escape(_ls, raises(func(strng,lst(strng)),ERR), "return a array of files in a directory")
escape(_repo, func(/**/,strng), "return the standard repo directory name")

escape(_file_mode, raises(func(strng,int),ERR), "report modes of a file")
escape(_file_present, func(strng,bool), "check presence of a file")
escape(_file_type, raises(func(strng,int),ERR), "report on the type of a file")
escape(_file_size, raises(func(strng,int),ERR), "report on the size of a file")
escape(_file_modified, raises(func(strng,int),ERR), "report on when a file was last modified")
escape(_file_date, raises(func(strng,tpl(int int int)),ERR), "report on file access time and modification times")

escape(_openInFile, raises(func(strng int,io),ERR), "open input file")
escape(_openOutFile, raises(func(strng int,io),ERR), "open output file")
escape(_openAppendFile, raises(func(strng int,io),ERR), "open output file")
escape(_openAppendIOFile, raises(func(strng int,io),ERR), "open output file")

escape(_popen, raises(func(strng lst(strng) lst(tpl(strng strng)), tpl(io io io)),ERR), "open a pipe")

escape(_close, raises(func(io,unit),ERR), "close file")
escape(_end_of_file, func(io,bool), "end of file test")
escape(_inchars, raises(func(io int,strng),ERR), "read block string")
escape(_inchars_async, raises(func(io int,future(strng,ERR)),ERR), "async read block string")
escape(_inchar, raises(func(io,chr),ERR), "read single character")
escape(_inchar_async, raises(func(io,future(chr,ERR)),ERR), "async read single character")
escape(_inbyte, raises(func(io,int),ERR), "read single byte")
escape(_inbyte_async, raises(func(io,future(int,ERR)),ERR), "async ead single byte")
escape(_inbytes,raises(func(io int,vec(int)),ERR),"read block of bytes")
escape(_inbytes_async,raises(func(io int,future(vec(int),ERR)),ERR),"async read block of bytes")
escape(_inline, raises(func(io,strng),ERR), "read a line")
escape(_inline_async, raises(func(io,future(strng,ERR)),ERR), "async read of a line")
escape(_outchar, raises(func(io chr,unit),ERR), "write a single character")
escape(_outchar_async, raises(func(io chr,future(unit,ERR)),ERR), "async write a single character")
escape(_outbyte, raises(func(io int,unit),ERR), "write a single byte")
escape(_outbyte_async, raises(func(io int,future(unit,ERR)),ERR), "async write a single byte")
escape(_outbytes, raises(func(io lst(int),unit),ERR), "write a list of bytes")
escape(_outtext, raises(func(io strng,unit),ERR), "write a string as a block")
escape(_outtext_async, raises(func(io strng,future(unit,ERR)),ERR), "async write a string as a block")
escape(_stdfile, func(int,io), "standard file descriptor")
escape(_fposition, func(io,int), "report current file position")
escape(_fseek, raises(func(io int,unit),ERR), "seek to new file position")
escape(_flush, raises(func(io,unit),ERR), "flush the I/O buffer")
escape(_flushall, func(/**/,unit), "flush all files")
escape(_fname,func(io,strng),"the channel file name")

escape(_waitIo,all(e,func(lst(tpl(io func(/**/,bool) e)) int,bool)), "Poll for IO ready")
escape(_setfileencoding, func(io int,unit), "set file encoding on file")

escape(_get_file, raises(func(strng,strng),ERR), "file into a char sequence")
escape(_put_file, raises(func(strng strng,unit),ERR), "write string into file")
escape(_show, func(strng,unit), "show something on console")

escape(_install_pkg, raises(func(strng, lst(tpl(strng strng))),ERR), "define package from string contents")
escape(_pkg_is_present, raises(func(strng strng,bool),ERR), "True if an identified package is available")
escape(_in_manifest, raises(func(strng strng strng,bool),ERR), "True if pkg/version/kind is present in manifest")
escape(_locate_in_manifest, raises(func(strng strng strng,strng),ERR), "Access manifest resource")

escape(_logmsg, func(strng,unit), "log a message in logfile or console")
escape(_display_depth,func(/**/,int), "Current standard display depth")

// Socket handling functions
escape(_connect, raises(func(strng int int,tpl(io io)),ERR), "connect to remote host")
escape(_listen, raises(func(int,io),ERR), "listen on a port")
escape(_accept, raises(func(io, tpl(io io strng int strng)),ERR), "accept connection")

escape(_hosttoip, func(strng,lst(strng)), "IP address of host")
escape(_iptohost, raises(func(strng,strng),ERR), "host name from IP")

// Timing and delaying
escape(_delay, raises(func(flt,unit),ERR), "delay for period of time")
escape(_sleep, raises(func(flt,unit),ERR), "sleep until a definite time")
escape(_now, func(/**/,flt), "current time")
escape(_today, func(/**/,flt), "time at midnight")
escape(_ticks, func(/**/,int), "used CPU time")
escape(_time2date, func(flt,tpl(int int int int int int flt int)), "convert a time to a date")
escape(_time2utc, func(flt,tpl(int int int int int int flt int)), "convert a time to UTC date")
escape(_date2time, func(int int int int int flt int,flt), "convert a date to a time")
escape(_utc2time, func(int int int int int flt int,flt), "convert a UTC date to a time")
escape(_formattime,raises(func(flt strng,strng),ERR),"format a time value")
escape(_parsetime,func(strng strng,option(flt)),"parse a date expression guided by format string")

// Character class escapes
escape(_uniCodeCategory, func(chr,int), "unicode category")

escape(_isCcChar, func(chr,bool), "is Other, control char")
escape(_isCfChar, func(chr,bool), "is Other, format char")
escape(_isCnChar, func(chr,bool), "is Other, unassigned char")
escape(_isCoChar, func(chr,bool), "is Other, private char")
escape(_isCsChar, func(chr,bool), "is Other, surrogate char")
escape(_isLlChar, func(chr,bool), "is Letter, lowercase char")
escape(_isLmChar, func(chr,bool), "is Letter, modifier char")
escape(_isLoChar, func(chr,bool), "is Letter, other char")
escape(_isLtChar, func(chr,bool), "is Letter, title char")
escape(_isLuChar, func(chr,bool), "is Letter, uppercase char")
escape(_isMcChar, func(chr,bool), "is Mark, spacing char")
escape(_isMeChar, func(chr,bool), "is Mark, enclosing char")
escape(_isMnChar, func(chr,bool), "is Mark, nonspacing char")
escape(_isNdChar, func(chr,bool), "is Number, decimal digit")
escape(_isNlChar, func(chr,bool), "is Number, letter char")
escape(_isNoChar, func(chr,bool), "is Number, other char")
escape(_isPcChar, func(chr,bool), "is Punctuation, connector")
escape(_isPdChar, func(chr,bool), "is Punctuation, dash char")
escape(_isPeChar, func(chr,bool), "is Punctuation, close char")
escape(_isPfChar, func(chr,bool), "is Punctuation, final quote")
escape(_isPiChar, func(chr,bool), "is Punctuation, initial quote")
escape(_isPoChar, func(chr,bool), "is Punctuation, other char")
escape(_isPsChar, func(chr,bool), "is Punctuation, open char")
escape(_isScChar, func(chr,bool), "is Symbol, currency char")
escape(_isSkChar, func(chr,bool), "is Symbol, modifier char")
escape(_isSmChar, func(chr,bool), "is Symbol, math char")
escape(_isSoChar, func(chr,bool), "is Symbol, other char")
escape(_isZlChar, func(chr,bool), "is Separator, line char")
escape(_isZpChar, func(chr,bool), "is Separator, para char")
escape(_isZsChar, func(chr,bool), "is Separator, space char")

escape(_isLetterChar, func(chr,bool), "is letter char")
escape(_digitCode, raises(func(chr,int),ERR), "convert char to num")

escape(_codePoint, func(chr,int), "convert char to code point integer")
escape(_char, func(int,chr), "convert integer code point to char")

// String handling escapes
escape(_int2str, func(int int int int,strng), "format an integer as a string")
escape(_flt2str, func(flt int chr bool,strng), "format a floating as a string")
escape(_int_format, raises(func(int strng,strng),ERR), "format an integer using picture format")
escape(_flt_format, func(flt strng,strng), "format a floating point using picture format")

escape(_str2flt, func(strng,option(flt)), "parse a string as a float")
escape(_str2int, func(strng,option(int)), "parse a string as an integer")

escape(_chr_eq, func(chr chr,bool), "Character equality")
escape(_chr_lt, func(chr chr,bool), "Character 1 is less than character 2")
escape(_chr_ge, func(chr chr,bool), "Character 1 is greater than or equals to character 2")

escape(_chr_hash, func(chr,int), "Compute hash of a character")
escape(_chr_quote, func(chr,strng), "Display string for a character")
escape(_chr_format, func(chr strng,strng), "format a character using picture format")

escape(_str_eq, func(strng strng,bool), "String equality")
escape(_str_lt, func(strng strng,bool), "String 1 is less than string 2")
escape(_str_ge, func(strng strng,bool), "String 1 is greater than or equals to string 2")

escape(_str_hash, func(strng,int), "Compute hash of string")
escape(_str_len, func(strng,int), "return length of char sequence")

escape(_str_gen, func(strng, strng), "Generate a unique string")

escape(_stringOf, all(r,func(r int,strng)), "Display a general term")

escape(_explode, func(strng,lst(chr)), "convert char sequence to list of chars")
escape(_implode, func(lst(chr),strng), "convert list of code points to char sequence")

escape(_str_find, func(strng strng int,int), "find a substring in string")
escape(_sub_str, func(strng int int,strng), "extract a substring")
escape(_str_split, func(strng int,tpl(strng strng)), "split a string at a point")
escape(_str_concat, func(strng strng,strng), "Concatenate two strings")
escape(_str_reverse, func(strng,strng), "Unicode reverse of string")
escape(_str_start, func(strng strng,bool), "True if second string starts with first")
escape(_str_end, func(strng strng,bool), "True if second string ends with first")
escape(_str_splice, func(strng int int strng,strng), "splice a substring into a string")

escape(_str_multicat, func(lst(strng),strng), "Concatenate a list of strings into one")
escape(_str_hdtl, func(strng,option(tpl(chr strng))), "pick up the first character and return remainder")
escape(_str_back, raises(func(strng,tpl(strng chr)),ERR), "pick up the last character and return remainder")
escape(_str_cons, func(chr strng,strng), "put a char in the front")
escape(_code2str, func(chr,strng), "make a 1 char string")
escape(_str_apnd, func(strng chr,strng), "put a char in the back")
escape(_str_charat, func(strng int,option(chr)), "index into string")
escape(_str_quote, func(strng,strng), "construct a quoted version of a string")
escape(_str_format, func(strng strng,strng), "apply formatting to a char sequence")

escape(_getenv, func(strng,option(strng)), "get an environment variable")
escape(_setenv, raises(func(strng strng,unit),ERR), "set an environment variable")
escape(_envir, func(/**/,lst(tpl(strng strng))), "return entire environment")

// Process manipulation
escape(_fork, func(func(/**/,unit),thread), "fork new process")
escape(_thread, func(/**/,thread), "report thread of current process")
escape(_kill, raises(func(thread,unit),ERR), "kill off a process")
escape(_thread_state, func(thread,processState), "state of process")
escape(_waitfor, raises(func(thread,unit),ERR), "wait for other thread to terminate")

escape(_shell, raises(func(strng lst(strng) lst(tpl(strng strng)),int),ERR), "Run a shell cmd")

escape(_ins_debug, func(/**/,unit), "set instruction-level")
escape(_stackTrace, func(/**/,strng), "Print a stack trace")

escape(_jit_compile,all(a,all(e,raises(func(func(a,e),unit),ERR))),"Jit compile a single arg function")

#undef processState
#undef thread
#undef io
#undef ERR
#undef option
#undef either
#undef future
#undef fiber
#undef func
#undef raises
#undef throws
#undef tpl
#undef bool
#undef chr
#undef int
#undef flt
#undef big
#undef strng
#undef lst
#undef all
#undef vr
#undef e
#undef a
#undef r
#undef s

