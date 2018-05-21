/*
  This is where you define a new escape function so that the compiler and
  the run-time system can see it
  Copyright (c) 2016, 2017. Francis G. McCabe
 */

/* Declare standard types used in escapes */

#define sysRet "Uz1'star.core*resultType'S"
#define processState "t'star.thread*processState'"
#define threadType "t'star.thread*thread'"
#define lockType "t'star.thread*lock'"
#define fileType "t'star.io*fileHandle'"
#define udpType "t'star.io*udpHandle'"
#define locType "(Siiii)"

/* Define the standard escapes */
escape(_exit,True,False,"F(i)v","terminate engine")
escape(_command_line,False,False,"F()LS","command line arguments")
//escape(_command_opts,False,False,"F()L(SS)","command line options")
escape(_abort,False,False,":k's':k't'F(k's'k't')v","abort process")

escape(_identical,False,False,":k't'F(k't'k't')l","test for identicality")

escape(_defined,True,False,"F(Si)l","test for defined name")

escape(_int_plus,False,False,"F(ii)i","add two integers")
escape(_int_minus,False,False,"F(ii)i","subtract two integers")
escape(_int_times,False,False,"F(ii)i","multiply two integers")
escape(_int_div,False,False,"F(ii)i","divide two integers")
escape(_int_mod,False,False,"F(ii)i","modulo remainder")
escape(_int_hash,False,False,"F(i)i","compute hash of integer")

escape(_flt_plus,False,False,"F(ff)f","add two floats")
escape(_flt_minus,False,False,"F(ff)f","subtract two floats")
escape(_flt_times,False,False,"F(ff)f","multiply two floats")
escape(_flt_div,False,False,"F(ff)f","divide two floats")
escape(_flt_mod,False,False,"F(ff)f","modulo remainder")

escape(_int_abs,False,False,"F(i)i","integer absolute value")
escape(_flt_abs,False,False,"F(i)i","float absolute value")

escape(_int_eq,False,False,"F(ii)l","integer equality")
escape(_int_lt,False,False,"F(ii)l","integer less than")
escape(_int_ge,False,False,"F(ii)l","integer greater or equal")

escape(_flt_eq,False,False,"F(ff)l","float equality")
escape(_flt_lt,False,False,"F(ff)l","float less than")
escape(_flt_ge,False,False,"F(ff)l","float greater or equal")

escape(_int2flt,False,False,"F(i)f","convert integer to float")
escape(_flt2int,False,False,"F(f)i","convert float to integer")

escape(_flt_hash,False,False,"F(f)i","compute hash of float")

escape(_flt_pwr,False,False,"F(ff)f","raise X to the power Y")

escape(sqrt,False,False,"F(f)f","square root")
escape(_exp,False,False,"F(f)f","exponential")
escape(log,False,False,"F(f)f","logarithm")
escape(log10,False,False,"F(f)f","10-based logarithm")
escape(pi,False,False,"F()f","return PI")
escape(sin,False,False,"F(f)f","sine")
escape(cos,False,False,"F(f)f","cosine")
escape(tan,False,False,"F(f)f","tangent")
escape(asin,False,False,"F(f)f","arc sine")
escape(acos,False,False,"F(f)f","arc cosine")
escape(atan,False,False,"F(f)f","arc tangent")

escape(trunc,False,False,"F(f)f","truncate to nearest integer")
escape(floor,False,False,"F(f)f","truncate to lower integer")
escape(ceil,False,False,"F(f)f","truncate to next integer")
escape(integral,False,False,"F(f)l","test if number is integral")

escape(_irand,False,False,"F(i)i","generate random integer")

escape(_ldexp,False,False,"F(fi)f","raise x to 2**y")
escape(_frexp,False,False,"F(f)(fi)","split x into mant and exp")
escape(_modf,False,False,"F(f)(fi)","split x into int and frac")

escape(_band,False,False,"F(ii)i","bitwise and two integers")
escape(_bor,False,False,"F(ii)i","bitwise or two integers")
escape(_bxor,False,False,"F(ii)i","bitwise xor two integers")
escape(_blsl,False,False,"F(ii)i","logical left shift")
escape(_blsr,False,False,"F(ii)i","logical right shift")
escape(_basr,False,False,"F(ii)i","arithmetic right shift")
escape(_bnot,False,False,"F(i)i","bitwise negate number")
escape(_nthb,False,False,"F(ii)l","is nth bit set?")

escape(_cell,False,False,":k't'F(k't')rk't'","create a reference cell")
escape(_get,False,False,":k't'F(rk't')k't'","access contents of reference cell")
escape(_assign,False,False,":k't'F(rk't'k't')rk't'","update contents of reference cell")
escape(_isDefinedVr,True,False,":k't'F(S)l","Check a global variable")
escape(_definedVr,True,False,":k't'F(S)k't'","Access a global variable")
escape(_defineVr,True,False,":k't'F(Sk't')l","Define a global variable")

escape(_list_empty,False,False,":k't'F(Lk't')l","is the list empty")
escape(_list_size,False,False,":k't'F(Lk't')i","compute the size of a list")
escape(_list_nth,False,False,":k't'F(Lk't'i)k't'","access element of a list")
escape(_list_append,False,False,":k't'F(Lk't'k't')Lk't'","append element to a list")
escape(_list_prepend,False,False,":k't'F(Lk't'k't')Lk't'","prepend element to a list")
escape(_list_slice,False,False,":k't'F(Lk't'ii)Lk't'","slice a list")
escape(_list_front,False,False,":k't'F(Lk't'i)Lk't'","front portion of list")
escape(_list_back,False,False,":k't'F(Lk't'i)Lk't'","back portion of list")
escape(_list_nil,False,False,":k't'F(i)Lk't'","create an empty list of min capacity")
escape(_list_concat,False,False,":k't'F(Lk't'Lk't')Lk't'","concatenate two lists")
escape(_list_flatten,False,False,":k't'F(LLk't')Lk't'","flatten a list of lists into a list")
escape(_list_reverse,False,False,":k't'F(Lk't')Lk't'","reverse the elements of a list")

/*

escape(_suspend,False,False,":k'u'P2k'u'P0","suspend handler if variable not bound")

*/

  escape(_get_file,True,False,"F(S)S","Get the contents of a file as a string")
  escape(_cwd,True,False,"F()S","return url of current working directory")
  escape(_cd,False,False,"F(S)"sysRet,"change current working directory")
  escape(_rm,True,False,"F(S)"sysRet,"remove file")
  escape(_mv,True,False,"F(SS)"sysRet,"rename file")
  escape(_mkdir,True,False,"F(Si)"sysRet,"create directory")
  escape(_rmdir,True,False,"F(S)"sysRet,"delete directory")
  escape(_isdir,True,False,"F(S)l","is directory present")
  escape(_file_chmod,True,False,"F(Si)"sysRet,"change mode of a file or directory")
  escape(_ls,True,False,"F(S)LS","return a list of files in a directory")

  escape(_file_mode,True,False,"F(S)i","report modes of a file")
  escape(_file_present,True,False,"F(S)l","check presence of a file")
  escape(_file_type,True,False,"F(S)i","report on the type of a file")
  escape(_file_size,True,False,"F(S)i","report on the size of a file")
  escape(_file_modified,True,False,"F(S)i","report on when a file was last modified")
  escape(_file_date,True,False,"F(S)(iii)","report on file access time and modification times")

  escape(_openInFile,True,False,"F(Si)"fileType,"open input file")
  escape(_openOutFile,True,False,"F(Si)"fileType,"open output file")
  escape(_openAppendFile,True,False,"F(Si)"fileType,"open output file")
  escape(_openAppendIOFile,True,False,"F(Si)"fileType,"open output file")

  escape(_popen,True,False,"F(SLSL(SS))("fileType fileType fileType")","open a pipe")

  escape(_close,True,False,"F("fileType")"sysRet,"close file")
  escape(_end_of_file,True,False,"F("fileType")l","end of file test")
  escape(_ready_to_read,True,False,"F("fileType")l","file ready test")
  escape(_ready_to_write,True,False,"F("fileType")l","file ready test")
  escape(_inchars,True,False,"F("fileType"i)S","read block string")
  escape(_inbytes,True,False,"F("fileType"i)Li","read block of bytes")
  escape(_inchar,True,False,"F("fileType")i","read single character")
  escape(_inbyte,True,False,"F("fileType")i","read single byte")
  escape(_inline,True,False,"F("fileType")S","read a line")
  escape(_intext,True,False,"F("fileType"S)S","read until matching character")
  escape(_outchar,True,False,"F("fileType"i)"sysRet,"write a single character")
  escape(_outbyte,True,False,"F("fileType"i)"sysRet,"write a single byte")
  escape(_outbytes,True,False,"F("fileType"Li)"sysRet,"write a list of bytes")
  escape(_outtext,True,False,"F("fileType"S)"sysRet,"write a string as a block")
  escape(_stdfile,True,False,"F(i)"fileType,"standard file descriptor")
  escape(_fposition,True,False,"F("fileType")i","report current file position")
  escape(_fseek,True,False,"F("fileType"i)"sysRet,"seek to new file position")
  escape(_flush,True,False,"F("fileType")"sysRet,"flush the I/O buffer")
  escape(_flushall,True,False,"F()v","flush all files")
  escape(_setfileencoding,True,False,"F("fileType"i)"sysRet, "set file encoding on file")

  escape(_install_pkg,True,False,"F(S)L(SS)","define package from string contents")
  escape(_pkg_is_present,True,False,"F(SS)l","True if an identified package is available")

  escape(_logmsg,False,False,"F(S)"sysRet,"log a message in logfile or console")

  // Socket handling functions
  escape(_connect,True,False,"F(Sii)("fileType fileType")","connect to remote host")
  escape(_listen,True,False,"F(i)"fileType,"listen on a port")
  escape(_accept,True,False,"F("fileType")("fileType fileType "SiS)","accept connection")

/*
  escape(_udpPort,True,False,"F(i"udpType")"sysRet,"estabish a UDP port")
  escape(_udpGet,True,False,"F("udpType")(SSi)","read a UDP datagram")
  escape(_udpSend,True,False,"F("udpType"SSi)"sysRet,"send a UDP datagram")
  escape(_udpClose,True,False,"F("udpType")"sysRet,"close the UDP socket")
*/

  escape(_hosttoip,False,False,"F(S)LS","IP address of host")
  escape(_iptohost,False,False,"F(S)S","host name from IP")

// Timing and delaying
  escape(_delay,False,False,"F(f)"sysRet,"delay for period of time")
  escape(_sleep,False,False,"F(f)"sysRet,"sleep until a definite time")
  escape(_now,False,False,"F()f","current time")
  escape(_today,False,False,"F()f","time at midnight")
  escape(_ticks,False,False,"F()f","used CPU time")
  escape(_time2date,False,False,"F(f)(iiiiiifii)", "convert a time to a date")
  escape(_time2utc,False,False, "F(f)(iiiiiifii)", "convert a time to UTC date")
  escape(_date2time,False,False,"F(iiiiifi)f", "convert a date to a time")
  escape(_utc2time,False,False,"F(iiiiifi)f", "convert a UTC date to a time")

 // Character class escapes

  escape(_isCcChar,False,False,"F(i)l","is Other, control char")
  escape(_isCfChar,False,False,"F(i)l","is Other, format char")
  escape(_isCnChar,False,False,"F(i)l","is Other, unassigned char")
  escape(_isCoChar,False,False,"F(i)l","is Other, private char")
  escape(_isCsChar,False,False,"F(i)l","is Other, surrogate char")
  escape(_isLlChar,False,False,"F(i)l","is Letter, lowercase char")
  escape(_isLmChar,False,False,"F(i)l","is Letter, modifier char")
  escape(_isLoChar,False,False,"F(i)l","is Letter, other char")
  escape(_isLtChar,False,False,"F(i)l","is Letter, title char")
  escape(_isLuChar,False,False,"F(i)l","is Letter, uppercase char")
  escape(_isMcChar,False,False,"F(i)l","is Mark, spacing char")
  escape(_isMeChar,False,False,"F(i)l","is Mark, enclosing char")
  escape(_isMnChar,False,False,"F(i)l","is Mark, nonspacing char")
  escape(_isNdChar,False,False,"F(i)l","is Number, decimal digit")
  escape(_isNlChar,False,False,"F(i)l","is Number, letter char")
  escape(_isNoChar,False,False,"F(i)l","is Number, other char")
  escape(_isPcChar,False,False,"F(i)l","is Punctuation, connector")
  escape(_isPdChar,False,False,"F(i)l","is Punctuation, dash char")
  escape(_isPeChar,False,False,"F(i)l","is Punctuation, close char")
  escape(_isPfChar,False,False,"F(i)l","is Punctuation, final quote")
  escape(_isPiChar,False,False,"F(i)l","is Punctuation, initial quote")
  escape(_isPoChar,False,False,"F(i)l","is Punctuation, other char")
  escape(_isPsChar,False,False,"F(i)l","is Punctuation, open char")
  escape(_isScChar,False,False,"F(i)l","is Symbol, currency char")
  escape(_isSkChar,False,False,"F(i)l","is Symbol, modifier char")
  escape(_isSmChar,False,False,"F(i)l","is Symbol, math char")
  escape(_isSoChar,False,False,"F(i)l","is Symbol, other char")
  escape(_isZlChar,False,False,"F(i)l","is Separator, line char")
  escape(_isZpChar,False,False,"F(i)l","is Separator, para char")
  escape(_isZsChar,False,False,"F(i)l","is Separator, space char")

  escape(_isLetterChar,False,False,"F(i)l","is letter char")
  escape(_digitCode,False,False,"F(i)i","convert char to num")

// String handling escapes
  escape(_int2str,False,False,"F(iiii)S","format an integer as a string")
  escape(_flt2str,False,False,"F(fiiil)S","format a floating as a string")
  escape(_int_format,False,False,"F(iS)S","format an integer using picture format")
  escape(_flt_format,False,False,"F(fS)S","format a floating point using picture format")

  escape(_str2flt,False,False,"F(S)f","parse a string as a float")
  escape(_str2int,False,False,"F(S)i","parse a string as an integer")

  escape(_str_eq,False,False,"F(SS)l","String equality")
  escape(_str_lt,False,False,"F(SS)l","String 1 is less than string 2")
  escape(_str_ge,False,False,"F(SS)l","String 1 is greater than or equals to string 2")

  escape(_str_hash,False,False,"F(S)i","Compute hash of string")
  escape(_str_len,False,False,"F(S)i","return length of string")

  escape(_str_gen,False,False,"F(S)S","Generate a unique string")

  escape(_stringOf,False,False,":k't'F(k't'i)S","Display a general term")

  escape(_explode,False,False,"F(S)Li","convert string to list of code points")
  escape(_implode,False,False,"F(Li)S","convert list of code points to string")

  escape(_str_find,False,False,"F(SSi)i","find a substring in string")
  escape(_sub_str,False,False,"F(Sii)S","extract a substring")
  escape(_str_split,False,False,"F(Si)(SS)","split a string at a point")
  escape(_str_concat,False,False,"F(SS)S","Concatenate two strings")
  escape(_str_start,False,False,"F(SS)l","True if second string starts with first")
  escape(_str_multicat,False,False,"F(LS)S","Concatenate a list of strings into one")

  escape(_getenv,False,False,"F(SS)S","get an environment variable")
  escape(_setenv,True,False,"F(SS)"sysRet,"set an environment variable")
  escape(_envir,False,False,"F()L(SS)","return entire environment")
  escape(_getlogin,False,False,"F()S","return user's login")

// Process manipulation
  escape(_fork,False,False,"F(F()"sysRet")"threadType,"fork new process")
  escape(_thread,False,False,"F()"threadType"","report thread of current process")
  escape(_kill,True,False,"F("threadType")"sysRet ,"kill off a process")
  escape(_thread_state,False,False,"F("threadType ")" processState,"state of process")
  escape(_waitfor,False,False,"F("threadType")"sysRet,"wait for other thread to terminate")

  escape(_shell,True,False,"F(SLSL(SS))i","Run a shell cmd")

 // Lock management
  escape(_newLock,False,False,"F()"lockType,"create a new lock")
  escape(_acquireLock,False,False,"F("lockType"f)"sysRet,"acquire lock")
  escape(_waitLock,False,False,"F("lockType"f)"sysRet,"release and wait on a lock")
  escape(_releaseLock,False,False,"F("lockType")"sysRet,"release a lock")

  escape(_ins_debug,False,False,"F()v","set instruction-level")
  escape(_stackTrace,False,False,"F()v","Print a stack trace")
  escape(_assert,False,False,"F(l"locType")v","Check an assertion and bolt if false")

#undef processState
#undef threadType
#undef fileType
#undef lockType
#undef udpType
#undef sysRet
