/* Automatically generated, do not edit */

star.compiler.escapes{
  import star.
  import star.compiler.types.

  public escapeType:(string)=>option[tipe].
  escapeType(Es) => case Es in {
    "_exit" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.tupleType([])).
    "_abort" => ? .allType(.nomnal("a"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("a"),.nomnal("star.core*string")])),.tupleType([]))).
    "_definedLbl" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer")])),.nomnal("star.core*boolean")).
    "_callLbl" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer"),.tpExp(.tpFun("star.core*cons",1),.tpExp(.tpFun("star.core*cons",1),.nomnal("star.core*string")))])),.tupleType([])).
    "_int_plus" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_int_minus" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_int_times" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_int_div" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_int_mod" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_int_hash" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_int_gcd" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_flt_plus" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "_flt_minus" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "_flt_times" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "_flt_div" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "_flt_mod" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "_int_abs" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_flt_abs" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "_int_eq" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*boolean")).
    "_int_lt" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*boolean")).
    "_int_ge" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*boolean")).
    "_flt_eq" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*float"),.nomnal("star.core*float")])),.nomnal("star.core*boolean")).
    "_flt_lt" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*float")])),.nomnal("star.core*boolean")).
    "_flt_ge" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*float")])),.nomnal("star.core*boolean")).
    "_int2flt" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.nomnal("star.core*float")).
    "_flt2int" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*integer")).
    "_bits_float" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.nomnal("star.core*float")).
    "_float_bits" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*integer")).
    "_flt_hash" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*integer")).
    "_flt_pwr" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "_big_plus" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])),.nomnal("star.core*bigint")).
    "_big_minus" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])),.nomnal("star.core*bigint")).
    "_big_times" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])),.nomnal("star.core*bigint")).
    "_big_div" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])).
    "_big_bitand" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])),.nomnal("star.core*bigint")).
    "_big_bitor" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])),.nomnal("star.core*bigint")).
    "_big_bitxor" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])),.nomnal("star.core*bigint")).
    "_big_bitnot" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint")])),.nomnal("star.core*bigint")).
    "_big_gcd" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])),.nomnal("star.core*bigint")).
    "_big_hash" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint")])),.nomnal("star.core*integer")).
    "_big_eq" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])),.nomnal("star.core*boolean")).
    "_big_lt" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])),.nomnal("star.core*boolean")).
    "_big_ge" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*bigint")])),.nomnal("star.core*boolean")).
    "_int2big" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.nomnal("star.core*bigint")).
    "_big2int" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint")])),.tpExp(.tpFun("star.core*option",1),.nomnal("star.core*integer"))).
    "_ints2big" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpFun("star.core*cons",1),.nomnal("star.core*integer"))])),.nomnal("star.core*bigint")).
    "_big2ints" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint")])),.tpExp(.tpFun("star.core*cons",1),.nomnal("star.core*integer"))).
    "_str2big" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*option",1),.nomnal("star.core*bigint"))).
    "_big2str" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint")])),.nomnal("star.core*string")).
    "_big_format" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*bigint"),.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_fiber_eq" => ? .allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("s")),.nomnal("r")),.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("s")),.nomnal("r"))])),.nomnal("star.core*boolean")))).
    "_new_fiber" => ? .allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s")),.nomnal("r")])),.nomnal("s"))])),.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s"))))).
    "_stack_split" => ? .allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s"))])),.nomnal("s"))])),.nomnal("s")))).
    "_suspend_fiber" => ? .allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s")),.nomnal("s")])),.nomnal("r")))).
    "_retire_fiber" => ? .allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s")),.nomnal("s")])),.tupleType([])))).
    "_resume_fiber" => ? .allType(.nomnal("s"),.allType(.nomnal("r"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("star.fiber*fiber",2),.nomnal("r")),.nomnal("s")),.nomnal("r")])),.nomnal("s")))).
    "sqrt" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "exp" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "log" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "log10" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "pi" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.nomnal("star.core*float")).
    "sin" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "cos" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "tan" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "asin" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "acos" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "atan" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "trunc" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "floor" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "ceil" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*float")).
    "integral" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.nomnal("star.core*boolean")).
    "_irand" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_random" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.nomnal("star.core*float")).
    "_seed" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.tupleType([])).
    "_ldexp" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*integer")])),.nomnal("star.core*float")).
    "_frexp" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*integer")])).
    "_modf" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*integer")])).
    "_band" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_bor" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_bxor" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_blsl" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_blsr" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_basr" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_bnot" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_cell" => ? .allType(.nomnal("t"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("t")])),.tpExp(.tpFun("star.core*ref",1),.nomnal("t")))).
    "_get" => ? .allType(.nomnal("t"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpFun("star.core*ref",1),.nomnal("t"))])),.nomnal("t"))).
    "_assign" => ? .allType(.nomnal("t"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpFun("star.core*ref",1),.nomnal("t")),.nomnal("t")])),.tupleType([]))).
    "_overwrite" => ? .allType(.nomnal("t"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("t"),.nomnal("t")])),.nomnal("t"))).
    "_tuple_nth" => ? .allType(.nomnal("t"),.allType(.nomnal("e"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("t"),.nomnal("star.core*integer")])),.nomnal("e")))).
    "_tuple_set_nth" => ? .allType(.nomnal("t"),.allType(.nomnal("e"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("t"),.nomnal("star.core*integer"),.nomnal("e")])),.nomnal("t")))).
    "_cwd" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.nomnal("star.core*string")).
    "_cd" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_rm" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_mv" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_mkdir" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_rmdir" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_isdir" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*boolean")).
    "_file_chmod" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_ls" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*cons",1),.nomnal("star.core*string"))).
    "_repo" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.nomnal("star.core*string")).
    "_file_mode" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*integer")).
    "_file_present" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*boolean")).
    "_file_type" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*integer")).
    "_file_size" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*integer")).
    "_file_modified" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*integer")).
    "_file_date" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer")])).
    "_openInFile" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer")])),.nomnal("star.file*fileHandle")).
    "_openOutFile" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer")])),.nomnal("star.file*fileHandle")).
    "_openAppendFile" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer")])),.nomnal("star.file*fileHandle")).
    "_openAppendIOFile" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer")])),.nomnal("star.file*fileHandle")).
    "_popen" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.tpExp(.tpFun("star.core*cons",1),.nomnal("star.core*string")),.tpExp(.tpFun("star.core*cons",1),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")]))])),.tupleType([.nomnal("star.file*fileHandle"),.nomnal("star.file*fileHandle"),.nomnal("star.file*fileHandle")])).
    "_close" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_end_of_file" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle")])),.nomnal("star.core*boolean")).
    "_ready_to_read" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle")])),.nomnal("star.core*boolean")).
    "_ready_to_write" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle")])),.nomnal("star.core*boolean")).
    "_inchars" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle"),.nomnal("star.core*integer")])),.nomnal("star.core*string")).
    "_inchar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle")])),.nomnal("star.core*integer")).
    "_inbyte" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle")])),.nomnal("star.core*integer")).
    "_inline" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle")])),.nomnal("star.core*string")).
    "_inline_async" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle")])),.tpExp(.tpFun("star.core*future",1),.nomnal("star.core*string"))).
    "_intext" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle"),.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_outchar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle"),.nomnal("star.core*integer")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_outbyte" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle"),.nomnal("star.core*integer")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_outbytes" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle"),.tpExp(.tpFun("star.core*cons",1),.nomnal("star.core*integer"))])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_outtext" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle"),.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_stdfile" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.nomnal("star.file*fileHandle")).
    "_fposition" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle")])),.nomnal("star.core*integer")).
    "_fseek" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle"),.nomnal("star.core*integer")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_flush" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_flushall" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.tupleType([])).
    "_setfileencoding" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle"),.nomnal("star.core*integer")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_get_file" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_put_file" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])),.tupleType([])).
    "_show" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tupleType([])).
    "_install_pkg" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*cons",1),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")]))).
    "_pkg_is_present" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])),.nomnal("star.core*boolean")).
    "_in_manifest" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string"),.nomnal("star.core*string")])),.nomnal("star.core*boolean")).
    "_locate_in_manifest" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string"),.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_logmsg" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tupleType([])).
    "_connect" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.tupleType([.nomnal("star.file*fileHandle"),.nomnal("star.file*fileHandle")])).
    "_listen" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.nomnal("star.file*fileHandle")).
    "_accept" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.file*fileHandle")])),.tupleType([.nomnal("star.file*fileHandle"),.nomnal("star.file*fileHandle"),.nomnal("star.core*string"),.nomnal("star.core*integer"),.nomnal("star.core*string")])).
    "_hosttoip" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*cons",1),.nomnal("star.core*string"))).
    "_iptohost" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_delay" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.tupleType([])).
    "_sleep" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.tupleType([])).
    "_now" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.nomnal("star.core*float")).
    "_today" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.nomnal("star.core*float")).
    "_ticks" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.nomnal("star.core*integer")).
    "_time2date" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*float"),.nomnal("star.core*integer"),.nomnal("star.core*integer")])).
    "_time2utc" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float")])),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*float"),.nomnal("star.core*integer"),.nomnal("star.core*integer")])).
    "_date2time" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*float"),.nomnal("star.core*integer")])),.nomnal("star.core*float")).
    "_utc2time" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*float"),.nomnal("star.core*integer")])),.nomnal("star.core*float")).
    "_uniCodeCategory" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*integer")).
    "_isCcChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isCfChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isCnChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isCoChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isCsChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isLlChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isLmChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isLoChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isLtChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isLuChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isMcChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isMeChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isMnChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isNdChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isNlChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isNoChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isPcChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isPdChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isPeChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isPfChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isPiChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isPoChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isPsChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isScChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isSkChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isSmChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isSoChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isZlChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isZpChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isZsChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isLetterChar" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_digitCode" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*integer")).
    "_codePoint" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*integer")).
    "_char" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer")])),.nomnal("star.core*char")).
    "_isIDStart" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_isIDContinue" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_int2str" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*string")).
    "_flt2str" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*integer"),.nomnal("star.core*char"),.nomnal("star.core*boolean")])),.nomnal("star.core*string")).
    "_int_format" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*integer"),.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_flt_format" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*float"),.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_str2flt" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*option",1),.nomnal("star.core*float"))).
    "_str2int" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*option",1),.nomnal("star.core*integer"))).
    "_chr_eq" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char"),.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_chr_lt" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char"),.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_chr_ge" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char"),.nomnal("star.core*char")])),.nomnal("star.core*boolean")).
    "_chr_hash" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*integer")).
    "_chr_quote" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*string")).
    "_chr_format" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char"),.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_str_eq" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])),.nomnal("star.core*boolean")).
    "_str_lt" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])),.nomnal("star.core*boolean")).
    "_str_ge" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])),.nomnal("star.core*boolean")).
    "_str_hash" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*integer")).
    "_str_len" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*integer")).
    "_str_gen" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_stringOf" => ? .allType(.nomnal("t"),.tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("t"),.nomnal("star.core*integer")])),.nomnal("star.core*string"))).
    "_explode" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tpExp(.tpFun("star.core*cons",1),.nomnal("star.core*char"))).
    "_implode" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpFun("star.core*cons",1),.nomnal("star.core*char"))])),.nomnal("star.core*string")).
    "_str_find" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string"),.nomnal("star.core*integer")])),.nomnal("star.core*integer")).
    "_sub_str" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer"),.nomnal("star.core*integer")])),.nomnal("star.core*string")).
    "_str_split" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer")])),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])).
    "_str_concat" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_str_reverse" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_str_start" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])),.nomnal("star.core*boolean")).
    "_str_splice" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*integer"),.nomnal("star.core*integer"),.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_str_multicat" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpFun("star.core*cons",1),.nomnal("star.core*string"))])),.nomnal("star.core*string")).
    "_str_hdtl" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tupleType([.nomnal("star.core*char"),.nomnal("star.core*string")])).
    "_str_back" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*char")])).
    "_str_cons" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char"),.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_code2str" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*char")])),.nomnal("star.core*string")).
    "_str_apnd" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*char")])),.nomnal("star.core*string")).
    "_str_quote" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_str_format" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_getenv" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])),.nomnal("star.core*string")).
    "_setenv" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")])),.tupleType([])).
    "_envir" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.tpExp(.tpFun("star.core*cons",1),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")]))).
    "_getlogin" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.nomnal("star.core*string")).
    "_fork" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))),.tupleType([])])),.nomnal("star.thread*thread")).
    "_thread" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.nomnal("star.thread*thread")).
    "_kill" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.thread*thread")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_thread_state" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.thread*thread")])),.nomnal("star.thread*threadState")).
    "_waitfor" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.thread*thread")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_shell" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.core*string"),.tpExp(.tpFun("star.core*cons",1),.nomnal("star.core*string")),.tpExp(.tpFun("star.core*cons",1),.tupleType([.nomnal("star.core*string"),.nomnal("star.core*string")]))])),.nomnal("star.core*integer")).
    "_newLock" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.nomnal("star.thread*lock")).
    "_acquireLock" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.thread*lock"),.nomnal("star.core*float")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_waitLock" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.thread*lock"),.nomnal("star.core*float")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_releaseLock" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([.nomnal("star.thread*lock")])),.tpExp(.tpFun("star.core*result",2),.nomnal("star.core*string"))).
    "_ins_debug" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.tupleType([])).
    "_stackTrace" => ? .tpExp(.tpExp(.tpFun("=>",2),.tupleType([])),.nomnal("star.core*string")).
    _ default => .none.
  }

  public isEscape:(string)=>option[integer].
  isEscape(Es) => case Es in {
    "_exit" => ?0.
    "_abort" => ?1.
    "_definedLbl" => ?2.
    "_callLbl" => ?3.
    "_int_plus" => ?4.
    "_int_minus" => ?5.
    "_int_times" => ?6.
    "_int_div" => ?7.
    "_int_mod" => ?8.
    "_int_hash" => ?9.
    "_int_gcd" => ?10.
    "_flt_plus" => ?11.
    "_flt_minus" => ?12.
    "_flt_times" => ?13.
    "_flt_div" => ?14.
    "_flt_mod" => ?15.
    "_int_abs" => ?16.
    "_flt_abs" => ?17.
    "_int_eq" => ?18.
    "_int_lt" => ?19.
    "_int_ge" => ?20.
    "_flt_eq" => ?21.
    "_flt_lt" => ?22.
    "_flt_ge" => ?23.
    "_int2flt" => ?24.
    "_flt2int" => ?25.
    "_bits_float" => ?26.
    "_float_bits" => ?27.
    "_flt_hash" => ?28.
    "_flt_pwr" => ?29.
    "_big_plus" => ?30.
    "_big_minus" => ?31.
    "_big_times" => ?32.
    "_big_div" => ?33.
    "_big_bitand" => ?34.
    "_big_bitor" => ?35.
    "_big_bitxor" => ?36.
    "_big_bitnot" => ?37.
    "_big_gcd" => ?38.
    "_big_hash" => ?39.
    "_big_eq" => ?40.
    "_big_lt" => ?41.
    "_big_ge" => ?42.
    "_int2big" => ?43.
    "_big2int" => ?44.
    "_ints2big" => ?45.
    "_big2ints" => ?46.
    "_str2big" => ?47.
    "_big2str" => ?48.
    "_big_format" => ?49.
    "_fiber_eq" => ?50.
    "_new_fiber" => ?51.
    "_stack_split" => ?52.
    "_suspend_fiber" => ?53.
    "_retire_fiber" => ?54.
    "_resume_fiber" => ?55.
    "sqrt" => ?56.
    "exp" => ?57.
    "log" => ?58.
    "log10" => ?59.
    "pi" => ?60.
    "sin" => ?61.
    "cos" => ?62.
    "tan" => ?63.
    "asin" => ?64.
    "acos" => ?65.
    "atan" => ?66.
    "trunc" => ?67.
    "floor" => ?68.
    "ceil" => ?69.
    "integral" => ?70.
    "_irand" => ?71.
    "_random" => ?72.
    "_seed" => ?73.
    "_ldexp" => ?74.
    "_frexp" => ?75.
    "_modf" => ?76.
    "_band" => ?77.
    "_bor" => ?78.
    "_bxor" => ?79.
    "_blsl" => ?80.
    "_blsr" => ?81.
    "_basr" => ?82.
    "_bnot" => ?83.
    "_cell" => ?84.
    "_get" => ?85.
    "_assign" => ?86.
    "_overwrite" => ?87.
    "_tuple_nth" => ?88.
    "_tuple_set_nth" => ?89.
    "_cwd" => ?90.
    "_cd" => ?91.
    "_rm" => ?92.
    "_mv" => ?93.
    "_mkdir" => ?94.
    "_rmdir" => ?95.
    "_isdir" => ?96.
    "_file_chmod" => ?97.
    "_ls" => ?98.
    "_repo" => ?99.
    "_file_mode" => ?100.
    "_file_present" => ?101.
    "_file_type" => ?102.
    "_file_size" => ?103.
    "_file_modified" => ?104.
    "_file_date" => ?105.
    "_openInFile" => ?106.
    "_openOutFile" => ?107.
    "_openAppendFile" => ?108.
    "_openAppendIOFile" => ?109.
    "_popen" => ?110.
    "_close" => ?111.
    "_end_of_file" => ?112.
    "_ready_to_read" => ?113.
    "_ready_to_write" => ?114.
    "_inchars" => ?115.
    "_inchar" => ?116.
    "_inbyte" => ?117.
    "_inline" => ?118.
    "_inline_async" => ?119.
    "_intext" => ?120.
    "_outchar" => ?121.
    "_outbyte" => ?122.
    "_outbytes" => ?123.
    "_outtext" => ?124.
    "_stdfile" => ?125.
    "_fposition" => ?126.
    "_fseek" => ?127.
    "_flush" => ?128.
    "_flushall" => ?129.
    "_setfileencoding" => ?130.
    "_get_file" => ?131.
    "_put_file" => ?132.
    "_show" => ?133.
    "_install_pkg" => ?134.
    "_pkg_is_present" => ?135.
    "_in_manifest" => ?136.
    "_locate_in_manifest" => ?137.
    "_logmsg" => ?138.
    "_connect" => ?139.
    "_listen" => ?140.
    "_accept" => ?141.
    "_hosttoip" => ?142.
    "_iptohost" => ?143.
    "_delay" => ?144.
    "_sleep" => ?145.
    "_now" => ?146.
    "_today" => ?147.
    "_ticks" => ?148.
    "_time2date" => ?149.
    "_time2utc" => ?150.
    "_date2time" => ?151.
    "_utc2time" => ?152.
    "_uniCodeCategory" => ?153.
    "_isCcChar" => ?154.
    "_isCfChar" => ?155.
    "_isCnChar" => ?156.
    "_isCoChar" => ?157.
    "_isCsChar" => ?158.
    "_isLlChar" => ?159.
    "_isLmChar" => ?160.
    "_isLoChar" => ?161.
    "_isLtChar" => ?162.
    "_isLuChar" => ?163.
    "_isMcChar" => ?164.
    "_isMeChar" => ?165.
    "_isMnChar" => ?166.
    "_isNdChar" => ?167.
    "_isNlChar" => ?168.
    "_isNoChar" => ?169.
    "_isPcChar" => ?170.
    "_isPdChar" => ?171.
    "_isPeChar" => ?172.
    "_isPfChar" => ?173.
    "_isPiChar" => ?174.
    "_isPoChar" => ?175.
    "_isPsChar" => ?176.
    "_isScChar" => ?177.
    "_isSkChar" => ?178.
    "_isSmChar" => ?179.
    "_isSoChar" => ?180.
    "_isZlChar" => ?181.
    "_isZpChar" => ?182.
    "_isZsChar" => ?183.
    "_isLetterChar" => ?184.
    "_digitCode" => ?185.
    "_codePoint" => ?186.
    "_char" => ?187.
    "_isIDStart" => ?188.
    "_isIDContinue" => ?189.
    "_int2str" => ?190.
    "_flt2str" => ?191.
    "_int_format" => ?192.
    "_flt_format" => ?193.
    "_str2flt" => ?194.
    "_str2int" => ?195.
    "_chr_eq" => ?196.
    "_chr_lt" => ?197.
    "_chr_ge" => ?198.
    "_chr_hash" => ?199.
    "_chr_quote" => ?200.
    "_chr_format" => ?201.
    "_str_eq" => ?202.
    "_str_lt" => ?203.
    "_str_ge" => ?204.
    "_str_hash" => ?205.
    "_str_len" => ?206.
    "_str_gen" => ?207.
    "_stringOf" => ?208.
    "_explode" => ?209.
    "_implode" => ?210.
    "_str_find" => ?211.
    "_sub_str" => ?212.
    "_str_split" => ?213.
    "_str_concat" => ?214.
    "_str_reverse" => ?215.
    "_str_start" => ?216.
    "_str_splice" => ?217.
    "_str_multicat" => ?218.
    "_str_hdtl" => ?219.
    "_str_back" => ?220.
    "_str_cons" => ?221.
    "_code2str" => ?222.
    "_str_apnd" => ?223.
    "_str_quote" => ?224.
    "_str_format" => ?225.
    "_getenv" => ?226.
    "_setenv" => ?227.
    "_envir" => ?228.
    "_getlogin" => ?229.
    "_fork" => ?230.
    "_thread" => ?231.
    "_kill" => ?232.
    "_thread_state" => ?233.
    "_waitfor" => ?234.
    "_shell" => ?235.
    "_newLock" => ?236.
    "_acquireLock" => ?237.
    "_waitLock" => ?238.
    "_releaseLock" => ?239.
    "_ins_debug" => ?240.
    "_stackTrace" => ?241.
    _ default => .none.
  }
}.
