/*
  This is where you define a new escape function so that the compiler and
  the run-time system can see it
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
 */

/* Declare standard types used in escapes */

#define sysRet(E) "Uz2'star.core*result'" E "()"
#define processState "t'star.thread*threadState'"
#define threadType "t'star.thread*thread'"
#define lockType "t'star.thread*lock'"
#define fileType "t'star.file*fileHandle'"
#define udpType "t'star.io*udpHandle'"
#define ssType "t'star.core*ss'"
#define optionType(T) "Uz1'star.core*option'" T
#define tagType(F,T) "UUz2'tag'" F T

/* Define the standard escapes */
escape(_exit,"F(i)()","terminate engine")
escape(_abort,":k'a'F(k'a's)()","abort process")

escape(_definedLbl,"F(si)l","test for defined name")
escape(_callLbl,"F(siLLs)()","invoke defined name")
escape(_fun2cont,":k'a':k'b'F("tagType("k'a'", "k'b'")"F(k'a')k'b')D(k'a')k'b'","create a continuation from a function")

escape(_int_plus,"F(ii)i","add two integers")
escape(_int_minus,"F(ii)i","subtract two integers")
escape(_int_times,"F(ii)i","multiply two integers")
escape(_int_div,"F(ii)i","divide two integers")
escape(_int_mod,"F(ii)i","modulo remainder")
escape(_int_hash,"F(i)i","compute hash of integer")

escape(_flt_plus,"F(ff)f","add two floats")
escape(_flt_minus,"F(ff)f","subtract two floats")
escape(_flt_times,"F(ff)f","multiply two floats")
escape(_flt_div,"F(ff)f","divide two floats")
escape(_flt_mod,"F(ff)f","modulo remainder")

escape(_int_abs,"F(i)i","integer absolute value")
escape(_flt_abs,"F(i)i","float absolute value")

escape(_int_eq,"F(ii)l","integer equality")
escape(_int_lt,"F(ii)l","integer less than")
escape(_int_ge,"F(ii)l","integer greater or equal")

escape(_flt_eq,"F(fff)l","float equality")
escape(_flt_lt,"F(ff)l","float less than")
escape(_flt_ge,"F(ff)l","float greater or equal")

escape(_int2flt,"F(i)f","convert integer to float")
escape(_flt2int,"F(f)i","convert float to integer")

escape(_bits_float,"F(i)f","convert a bit pattern to a float")
escape(_float_bits,"F(f)i","convert a float into a bit pattern")

escape(_flt_hash,"F(f)i","compute hash of float")
escape(_flt_pwr,"F(ff)f","raise X to the power Y")

escape(sqrt,"F(f)f","square root")
escape(exp,"F(f)f","exponential")
escape(log,"F(f)f","logarithm")
escape(log10,"F(f)f","10-based logarithm")
escape(pi,"F()f","return PI")
escape(sin,"F(f)f","sine")
escape(cos,"F(f)f","cosine")
escape(tan,"F(f)f","tangent")
escape(asin,"F(f)f","arc sine")
escape(acos,"F(f)f","arc cosine")
escape(atan,"F(f)f","arc tangent")

escape(trunc,"F(f)f","truncate to nearest integer")
escape(floor,"F(f)f","truncate to lower integer")
escape(ceil,"F(f)f","truncate to next integer")
escape(integral,"F(f)l","test if number is integral")

escape(_irand,"F(i)i","generate random integer")
escape(_random,"F()f","generate random float in range [0..1)")
escape(_seed,"F(i)()","set random number seed")

escape(_ldexp,"F(fi)f","raise x to 2**y")
escape(_frexp,"F(f)(fi)","split x into mant and exp")
escape(_modf,"F(f)(fi)","split x into int and frac")

escape(_band,"F(ii)i","bitwise and two integers")
escape(_bor,"F(ii)i","bitwise or two integers")
escape(_bxor,"F(ii)i","bitwise xor two integers")
escape(_blsl,"F(ii)i","logical left shift")
escape(_blsr,"F(ii)i","logical right shift")
escape(_basr,"F(ii)i","arithmetic right shift")
escape(_bnot,"F(i)i","bitwise negate number")

escape(_cell,":k't'F(k't')rk't'","create a reference cell")
escape(_get,":k't'F(rk't')k't'","access contents of reference cell")
escape(_assign,":k't'F(rk't'k't')()","update contents of reference cell")

escape(_overwrite,":k't'F(k't'k't')k't'","overwrite a structure with new structure")
//
//escape(_isDefinedVr,":k't'F(s)l","Check a global variable")
//escape(_definedVr,":k't'F(s)k't'","Access a global variable")
//escape(_defineVr,":k't'F(sk't')l","Define a global variable")

escape(_tuple_nth,":k't':k'e'F(k't'i)k'e'","Access tuple element")
escape(_tuple_set_nth,":k't':k'e'F(k't'ik'e')k't'","Update tuple element")

/*

escape(_suspend,":k'u'P2k'u'P0","suspend handler if variable not bound")

*/

escape(_cwd,"F()s","return url of current working directory")
escape(_cd,"F(s)"sysRet("s"),"change current working directory")
escape(_rm,"F(s)"sysRet("s"),"remove file")
escape(_mv,"F(ss)"sysRet("s"),"rename file")
escape(_mkdir,"F(si)"sysRet("s"),"create directory")
escape(_rmdir,"F(s)"sysRet("s"),"delete directory")
escape(_isdir,"F(s)l","is directory present")
escape(_file_chmod,"F(si)"sysRet("s"),"change mode of a file or directory")
escape(_ls,"F(s)Ls","return a array of files in a directory")
escape(_repo,"F()s","return the standard repo directory name")

escape(_file_mode,"F(s)i","report modes of a file")
escape(_file_present,"F(s)l","check presence of a file")
escape(_file_type,"F(s)i","report on the type of a file")
escape(_file_size,"F(s)i","report on the size of a file")
escape(_file_modified,"F(s)i","report on when a file was last modified")
escape(_file_date,"F(s)(iii)","report on file access time and modification times")

escape(_openInFile,"F(si)"fileType,"open input file")
escape(_openOutFile,"F(si)"fileType,"open output file")
escape(_openAppendFile,"F(si)"fileType,"open output file")
escape(_openAppendIOFile,"F(si)"fileType,"open output file")

escape(_popen,"F(sLsL(ss))("fileType fileType fileType")","open a pipe")

escape(_close,"F("fileType")"sysRet("s"),"close file")
escape(_end_of_file,"F("fileType")l","end of file test")
escape(_ready_to_read,"F("fileType")l","file ready test")
escape(_ready_to_write,"F("fileType")l","file ready test")
escape(_inchars,"F("fileType"i)s","read block string")
// escape(_inbytes,"F("fileType"i)Ai","read block of bytes")
escape(_inchar,"F("fileType")i","read single character")
escape(_inbyte,"F("fileType")i","read single byte")
escape(_inline,"F("fileType")s","read a line")
escape(_intext,"F("fileType"s)s","read until matching character")
escape(_outchar,"F("fileType"i)"sysRet("s"),"write a single character")
escape(_outbyte,"F("fileType"i)"sysRet("s"),"write a single byte")
escape(_outbytes,"F("fileType"Li)"sysRet("s"),"write a list of bytes")
escape(_outtext,"F("fileType"s)"sysRet("s"),"write a string as a block")
escape(_stdfile,"F(i)"fileType,"standard file descriptor")
escape(_fposition,"F("fileType")i","report current file position")
escape(_fseek,"F("fileType"i)"sysRet("s"),"seek to new file position")
escape(_flush,"F("fileType")"sysRet("s"),"flush the I/O buffer")
escape(_flushall,"F()()","flush all files")
escape(_setfileencoding,"F("fileType"i)"sysRet("s"), "set file encoding on file")
escape(_get_file,"F(s)s","file into a char sequence")
escape(_put_file,"F(ss)()","write string into file")
escape(_show,"F(s)()","show something on console")

escape(_install_pkg,"F(s)L(ss)","define package from string contents")
escape(_pkg_is_present,"F(ss)l","True if an identified package is available")
escape(_in_manifest,"F(sss)l","True if pkg/version/kind is present in manifest")
escape(_locate_in_manifest,"F(sss)s","Access manifest resource")

escape(_logmsg,"F(s)()","log a message in logfile or console")

// Socket handling functions
escape(_connect,"F(sii)("fileType fileType")","connect to remote host")
escape(_listen,"F(i)"fileType,"listen on a port")
escape(_accept,"F("fileType")("fileType fileType "sis)","accept connection")

/*
escape(_udpPort,"F(i"udpType")"sysRet,"estabish a UDP port")
escape(_udpGet,"F("udpType")(SSi)","read a UDP datagram")
escape(_udpSend,"F("udpType"SSi)"sysRet,"send a UDP datagram")
escape(_udpClose,"F("udpType")"sysRet,"close the UDP socket")
*/

escape(_hosttoip,"F(s)Ls","IP address of host")
escape(_iptohost,"F(s)s","host name from IP")

// Timing and delaying
escape(_delay,"F(f)()","delay for period of time")
escape(_sleep,"F(f)()","sleep until a definite time")
escape(_now,"F()f","current time")
escape(_today,"F()f","time at midnight")
escape(_ticks,"F()i","used CPU time")
escape(_time2date,"F(f)(iiiiiifii)", "convert a time to a date")
escape(_time2utc,"F(f)(iiiiiifii)", "convert a time to UTC date")
escape(_date2time,"F(iiiiifi)f", "convert a date to a time")
escape(_utc2time,"F(iiiiifi)f", "convert a UTC date to a time")

// Character class escapes

escape(_uniCodeCategory,"F(i)i","unicode category")

escape(_isCcChar,"F(i)l","is Other, control char")
escape(_isCfChar,"F(i)l","is Other, format char")
escape(_isCnChar,"F(i)l","is Other, unassigned char")
escape(_isCoChar,"F(i)l","is Other, private char")
escape(_isCsChar,"F(i)l","is Other, surrogate char")
escape(_isLlChar,"F(i)l","is Letter, lowercase char")
escape(_isLmChar,"F(i)l","is Letter, modifier char")
escape(_isLoChar,"F(i)l","is Letter, other char")
escape(_isLtChar,"F(i)l","is Letter, title char")
escape(_isLuChar,"F(i)l","is Letter, uppercase char")
escape(_isMcChar,"F(i)l","is Mark, spacing char")
escape(_isMeChar,"F(i)l","is Mark, enclosing char")
escape(_isMnChar,"F(i)l","is Mark, nonspacing char")
escape(_isNdChar,"F(i)l","is Number, decimal digit")
escape(_isNlChar,"F(i)l","is Number, letter char")
escape(_isNoChar,"F(i)l","is Number, other char")
escape(_isPcChar,"F(i)l","is Punctuation, connector")
escape(_isPdChar,"F(i)l","is Punctuation, dash char")
escape(_isPeChar,"F(i)l","is Punctuation, close char")
escape(_isPfChar,"F(i)l","is Punctuation, final quote")
escape(_isPiChar,"F(i)l","is Punctuation, initial quote")
escape(_isPoChar,"F(i)l","is Punctuation, other char")
escape(_isPsChar,"F(i)l","is Punctuation, open char")
escape(_isScChar,"F(i)l","is Symbol, currency char")
escape(_isSkChar,"F(i)l","is Symbol, modifier char")
escape(_isSmChar,"F(i)l","is Symbol, math char")
escape(_isSoChar,"F(i)l","is Symbol, other char")
escape(_isZlChar,"F(i)l","is Separator, line char")
escape(_isZpChar,"F(i)l","is Separator, para char")
escape(_isZsChar,"F(i)l","is Separator, space char")

escape(_isLetterChar,"F(i)l","is letter char")
escape(_digitCode,"F(i)i","convert char to num")

escape(_isIDStart,"F(i)l","is start char of identifier")
escape(_isIDContinue,"F(i)l","is continue char of identifier")

// String handling escapes
escape(_int2str,"F(iiii)s","format an integer as a string")
escape(_flt2str,"F(fiiil)s","format a floating as a string")
escape(_int_format,"F(is)s","format an integer using picture format")
escape(_flt_format,"F(fs)s","format a floating point using picture format")

escape(_str2flt,"F(s)" optionType("f"),"parse a string as a float")
escape(_str2int,"F(s)" optionType("i"),"parse a string as an integer")

escape(_str_eq,"F(ss)l","String equality")
escape(_string_eq,"F(SS)l","String equality")
escape(_str_lt,"F(ss)l","String 1 is less than string 2")
escape(_str_ge,"F(ss)l","String 1 is greater than or equals to string 2")

escape(_str_hash,"F(s)i","Compute hash of string")
escape(_string_len,"F(S)i","return length of string")
escape(_str_len,"F(s)i","return length of char sequence")

escape(_str_gen,"F(s)s","Generate a unique string")

escape(_stringOf,":k't'F(k't'i)s","Display a general term")

escape(_explode,"F(s)Li","convert char sequence to list of code points")
escape(_implode,"F(Li)s","convert list of code points to char sequence")

escape(_str_find,"F(ssi)i","find a substring in string")
escape(_sub_str,"F(sii)s","extract a substring")
escape(_str_split,"F(si)(ss)","split a string at a point")
escape(_str_concat,"F(ss)s","Concatenate two strings")
escape(_str_reverse,"F(s)s","Unicode reverse of string")
escape(_str_start,"F(ss)l","True if second string starts with first")
escape(_str_splice,"F(siis)s","splice a substring into a string")

escape(_str_multicat,"F(Ls)s","Concatenate a list of strings into one")
escape(_str_fltn,"F(S)s","Flatten a string into a char sequence")
escape(_str_hdtl,"F(s)(is)","pick up the first character and return remainder")
escape(_str_back,"F(s)(si)","pick up the last character and return remainder")
escape(_str_cons,"F(is)s","put a char in the front")
escape(_code2str,"F(i)s","make a 1 char string")
escape(_str_apnd,"F(si)s","put a char in the back")
escape(_str_quote,"F(s)s","construct a quoted version of a string")

escape(_getenv,"F(ss)s","get an environment variable")
escape(_setenv,"F(ss)()","set an environment variable")
escape(_envir,"F()L(ss)","return entire environment")
escape(_getlogin,"F()s","return user's login")

// Process manipulation
escape(_fork,"F(F()"sysRet("s")")"threadType,"fork new process")
escape(_thread,"F()"threadType"","report thread of current process")
escape(_kill,"F("threadType")"sysRet("s") ,"kill off a process")
escape(_thread_state,"F("threadType ")" processState,"state of process")
escape(_waitfor,"F("threadType")"sysRet("s"),"wait for other thread to terminate")

escape(_shell,"F(sLsL(ss))i","Run a shell cmd")

// Lock management
escape(_newLock,"F()"lockType,"create a new lock")
escape(_acquireLock,"F("lockType"f)"sysRet("s"),"acquire lock")
escape(_waitLock,"F("lockType"f)"sysRet("s"),"release and wait on a lock")
escape(_releaseLock,"F("lockType")"sysRet("s"),"release a lock")

escape(_ins_debug,"F()()","set instruction-level")
escape(_stackTrace,"F()()","Print a stack trace")

#undef processState
#undef threadType
#undef fileType
#undef lockType
#undef udpType
#undef sysRet
