/* Automatically generated, do not edit */

star.compiler.escapes{
  import star.
  import star.compiler.types.

  public escapeType:(string)=>option[tipe].
  escapeType(Es) => case Es in {
    "_exit" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),tupleType([])).
    "_abort" => ? allType(nomnal("a"),tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("a"),nomnal("star.core*string")])),tupleType([]))).
    "_definedLbl" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer")])),nomnal("star.core*boolean")).
    "_callLbl" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer"),tpExp(tpFun("star.core*cons",1),tpExp(tpFun("star.core*cons",1),nomnal("star.core*string")))])),tupleType([])).
    "_int_plus" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_int_minus" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_int_times" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_int_div" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_int_mod" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_int_hash" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_int_gcd" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_flt_plus" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*float")])),nomnal("star.core*float")).
    "_flt_minus" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*float")])),nomnal("star.core*float")).
    "_flt_times" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*float")])),nomnal("star.core*float")).
    "_flt_div" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*float")])),nomnal("star.core*float")).
    "_flt_mod" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*float")])),nomnal("star.core*float")).
    "_int_abs" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_flt_abs" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "_int_eq" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*boolean")).
    "_int_lt" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*boolean")).
    "_int_ge" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*boolean")).
    "_flt_eq" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*float"),nomnal("star.core*float")])),nomnal("star.core*boolean")).
    "_flt_lt" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*float")])),nomnal("star.core*boolean")).
    "_flt_ge" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*float")])),nomnal("star.core*boolean")).
    "_int2flt" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),nomnal("star.core*float")).
    "_flt2int" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*integer")).
    "_bits_float" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),nomnal("star.core*float")).
    "_float_bits" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*integer")).
    "_flt_hash" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*integer")).
    "_flt_pwr" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*float")])),nomnal("star.core*float")).
    "_big_plus" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint"),nomnal("star.core*bigint")])),nomnal("star.core*bigint")).
    "_big_minus" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint"),nomnal("star.core*bigint")])),nomnal("star.core*bigint")).
    "_big_times" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint"),nomnal("star.core*bigint")])),nomnal("star.core*bigint")).
    "_big_div" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint"),nomnal("star.core*bigint")])),tupleType([nomnal("star.core*bigint"),nomnal("star.core*bigint")])).
    "_big_gcd" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint"),nomnal("star.core*bigint")])),nomnal("star.core*bigint")).
    "_big_hash" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint")])),nomnal("star.core*integer")).
    "_big_eq" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint"),nomnal("star.core*bigint")])),nomnal("star.core*boolean")).
    "_big_lt" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint"),nomnal("star.core*bigint")])),nomnal("star.core*boolean")).
    "_big_ge" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint"),nomnal("star.core*bigint")])),nomnal("star.core*boolean")).
    "_int2big" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),nomnal("star.core*bigint")).
    "_big2int" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint")])),tpExp(tpFun("star.core*option",1),nomnal("star.core*integer"))).
    "_ints2big" => ? tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("star.core*cons",1),nomnal("star.core*integer"))])),nomnal("star.core*bigint")).
    "_big2ints" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint")])),tpExp(tpFun("star.core*cons",1),nomnal("star.core*integer"))).
    "_str2big" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tpExp(tpFun("star.core*option",1),nomnal("star.core*bigint"))).
    "_big2str" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint")])),nomnal("star.core*string")).
    "_big_format" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*bigint"),nomnal("star.core*string")])),nomnal("star.core*string")).
    "_fiber_eq" => ? allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("s")),nomnal("r")),tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("s")),nomnal("r"))])),nomnal("star.core*boolean")))).
    "_new_fiber" => ? allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("r")),nomnal("s")),nomnal("r")])),nomnal("s"))])),tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("r")),nomnal("s"))))).
    "_spawn" => ? allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("r")),nomnal("s"))])),nomnal("s"))])),nomnal("s")))).
    "_suspend_fiber" => ? allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("r")),nomnal("s")),nomnal("s")])),nomnal("r")))).
    "_retire_fiber" => ? allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("r")),nomnal("s")),nomnal("s")])),tupleType([])))).
    "_resume_fiber" => ? allType(nomnal("s"),allType(nomnal("r"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("star.fiber*fiber",2),nomnal("r")),nomnal("s")),nomnal("r")])),nomnal("s")))).
    "sqrt" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "exp" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "log" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "log10" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "pi" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),nomnal("star.core*float")).
    "sin" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "cos" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "tan" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "asin" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "acos" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "atan" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "trunc" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "floor" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "ceil" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*float")).
    "integral" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),nomnal("star.core*boolean")).
    "_irand" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_random" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),nomnal("star.core*float")).
    "_seed" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),tupleType([])).
    "_ldexp" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*integer")])),nomnal("star.core*float")).
    "_frexp" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),tupleType([nomnal("star.core*float"),nomnal("star.core*integer")])).
    "_modf" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),tupleType([nomnal("star.core*float"),nomnal("star.core*integer")])).
    "_band" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_bor" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_bxor" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_blsl" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_blsr" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_basr" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_bnot" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_cell" => ? allType(nomnal("t"),tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("t")])),tpExp(tpFun("star.core*ref",1),nomnal("t")))).
    "_get" => ? allType(nomnal("t"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("star.core*ref",1),nomnal("t"))])),nomnal("t"))).
    "_assign" => ? allType(nomnal("t"),tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("star.core*ref",1),nomnal("t")),nomnal("t")])),tupleType([]))).
    "_overwrite" => ? allType(nomnal("t"),tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("t"),nomnal("t")])),nomnal("t"))).
    "_tuple_nth" => ? allType(nomnal("t"),allType(nomnal("e"),tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("t"),nomnal("star.core*integer")])),nomnal("e")))).
    "_tuple_set_nth" => ? allType(nomnal("t"),allType(nomnal("e"),tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("t"),nomnal("star.core*integer"),nomnal("e")])),nomnal("t")))).
    "_cwd" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),nomnal("star.core*string")).
    "_cd" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_rm" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_mv" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_mkdir" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_rmdir" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_isdir" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*boolean")).
    "_file_chmod" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_ls" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tpExp(tpFun("star.core*cons",1),nomnal("star.core*string"))).
    "_repo" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),nomnal("star.core*string")).
    "_file_mode" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*integer")).
    "_file_present" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*boolean")).
    "_file_type" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*integer")).
    "_file_size" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*integer")).
    "_file_modified" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*integer")).
    "_file_date" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer")])).
    "_openInFile" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer")])),nomnal("star.file*fileHandle")).
    "_openOutFile" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer")])),nomnal("star.file*fileHandle")).
    "_openAppendFile" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer")])),nomnal("star.file*fileHandle")).
    "_openAppendIOFile" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer")])),nomnal("star.file*fileHandle")).
    "_popen" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),tpExp(tpFun("star.core*cons",1),nomnal("star.core*string")),tpExp(tpFun("star.core*cons",1),tupleType([nomnal("star.core*string"),nomnal("star.core*string")]))])),tupleType([nomnal("star.file*fileHandle"),nomnal("star.file*fileHandle"),nomnal("star.file*fileHandle")])).
    "_close" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_end_of_file" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle")])),nomnal("star.core*boolean")).
    "_ready_to_read" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle")])),nomnal("star.core*boolean")).
    "_ready_to_write" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle")])),nomnal("star.core*boolean")).
    "_inchars" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle"),nomnal("star.core*integer")])),nomnal("star.core*string")).
    "_inchar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle")])),nomnal("star.core*integer")).
    "_inbyte" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle")])),nomnal("star.core*integer")).
    "_inline" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle")])),nomnal("star.core*string")).
    "_inline_async" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle")])),tpExp(tpFun("star.core*future",1),nomnal("star.core*string"))).
    "_intext" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle"),nomnal("star.core*string")])),nomnal("star.core*string")).
    "_outchar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle"),nomnal("star.core*integer")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_outbyte" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle"),nomnal("star.core*integer")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_outbytes" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle"),tpExp(tpFun("star.core*cons",1),nomnal("star.core*integer"))])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_outtext" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle"),nomnal("star.core*string")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_stdfile" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),nomnal("star.file*fileHandle")).
    "_fposition" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle")])),nomnal("star.core*integer")).
    "_fseek" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle"),nomnal("star.core*integer")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_flush" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_flushall" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),tupleType([])).
    "_setfileencoding" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle"),nomnal("star.core*integer")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_get_file" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*string")).
    "_put_file" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])),tupleType([])).
    "_show" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tupleType([])).
    "_install_pkg" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tpExp(tpFun("star.core*cons",1),tupleType([nomnal("star.core*string"),nomnal("star.core*string")]))).
    "_pkg_is_present" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])),nomnal("star.core*boolean")).
    "_in_manifest" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string"),nomnal("star.core*string")])),nomnal("star.core*boolean")).
    "_locate_in_manifest" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string"),nomnal("star.core*string")])),nomnal("star.core*string")).
    "_logmsg" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tupleType([])).
    "_connect" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer"),nomnal("star.core*integer")])),tupleType([nomnal("star.file*fileHandle"),nomnal("star.file*fileHandle")])).
    "_listen" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),nomnal("star.file*fileHandle")).
    "_accept" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.file*fileHandle")])),tupleType([nomnal("star.file*fileHandle"),nomnal("star.file*fileHandle"),nomnal("star.core*string"),nomnal("star.core*integer"),nomnal("star.core*string")])).
    "_hosttoip" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tpExp(tpFun("star.core*cons",1),nomnal("star.core*string"))).
    "_iptohost" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*string")).
    "_delay" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),tupleType([])).
    "_sleep" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),tupleType([])).
    "_now" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),nomnal("star.core*float")).
    "_today" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),nomnal("star.core*float")).
    "_ticks" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),nomnal("star.core*integer")).
    "_time2date" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*float"),nomnal("star.core*integer"),nomnal("star.core*integer")])).
    "_time2utc" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float")])),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*float"),nomnal("star.core*integer"),nomnal("star.core*integer")])).
    "_date2time" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*float"),nomnal("star.core*integer")])),nomnal("star.core*float")).
    "_utc2time" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*float"),nomnal("star.core*integer")])),nomnal("star.core*float")).
    "_uniCodeCategory" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*integer")).
    "_isCcChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isCfChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isCnChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isCoChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isCsChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isLlChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isLmChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isLoChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isLtChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isLuChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isMcChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isMeChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isMnChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isNdChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isNlChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isNoChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isPcChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isPdChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isPeChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isPfChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isPiChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isPoChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isPsChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isScChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isSkChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isSmChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isSoChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isZlChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isZpChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isZsChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isLetterChar" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_digitCode" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*integer")).
    "_codePoint" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*integer")).
    "_char" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer")])),nomnal("star.core*char")).
    "_isIDStart" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_isIDContinue" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_int2str" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*string")).
    "_flt2str" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*integer"),nomnal("star.core*char"),nomnal("star.core*boolean")])),nomnal("star.core*string")).
    "_int_format" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*integer"),nomnal("star.core*string")])),nomnal("star.core*string")).
    "_flt_format" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*float"),nomnal("star.core*string")])),nomnal("star.core*string")).
    "_str2flt" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tpExp(tpFun("star.core*option",1),nomnal("star.core*float"))).
    "_str2int" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tpExp(tpFun("star.core*option",1),nomnal("star.core*integer"))).
    "_chr_eq" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char"),nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_chr_lt" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char"),nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_chr_ge" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char"),nomnal("star.core*char")])),nomnal("star.core*boolean")).
    "_chr_hash" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*integer")).
    "_chr_quote" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*string")).
    "_chr_format" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char"),nomnal("star.core*string")])),nomnal("star.core*string")).
    "_str_eq" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])),nomnal("star.core*boolean")).
    "_str_lt" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])),nomnal("star.core*boolean")).
    "_str_ge" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])),nomnal("star.core*boolean")).
    "_str_hash" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*integer")).
    "_str_len" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*integer")).
    "_str_gen" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*string")).
    "_stringOf" => ? allType(nomnal("t"),tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("t"),nomnal("star.core*integer")])),nomnal("star.core*string"))).
    "_explode" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tpExp(tpFun("star.core*cons",1),nomnal("star.core*char"))).
    "_implode" => ? tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("star.core*cons",1),nomnal("star.core*char"))])),nomnal("star.core*string")).
    "_str_find" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string"),nomnal("star.core*integer")])),nomnal("star.core*integer")).
    "_sub_str" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer"),nomnal("star.core*integer")])),nomnal("star.core*string")).
    "_str_split" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer")])),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])).
    "_str_concat" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])),nomnal("star.core*string")).
    "_str_reverse" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*string")).
    "_str_start" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])),nomnal("star.core*boolean")).
    "_str_splice" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*integer"),nomnal("star.core*integer"),nomnal("star.core*string")])),nomnal("star.core*string")).
    "_str_multicat" => ? tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpFun("star.core*cons",1),nomnal("star.core*string"))])),nomnal("star.core*string")).
    "_str_hdtl" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tupleType([nomnal("star.core*char"),nomnal("star.core*string")])).
    "_str_back" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),tupleType([nomnal("star.core*string"),nomnal("star.core*char")])).
    "_str_cons" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char"),nomnal("star.core*string")])),nomnal("star.core*string")).
    "_code2str" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*char")])),nomnal("star.core*string")).
    "_str_apnd" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*char")])),nomnal("star.core*string")).
    "_str_quote" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string")])),nomnal("star.core*string")).
    "_str_format" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])),nomnal("star.core*string")).
    "_getenv" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])),nomnal("star.core*string")).
    "_setenv" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),nomnal("star.core*string")])),tupleType([])).
    "_envir" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),tpExp(tpFun("star.core*cons",1),tupleType([nomnal("star.core*string"),nomnal("star.core*string")]))).
    "_getlogin" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),nomnal("star.core*string")).
    "_fork" => ? tpExp(tpExp(tpFun("=>",2),tupleType([tpExp(tpExp(tpFun("=>",2),tupleType([])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))),tupleType([])])),nomnal("star.thread*thread")).
    "_thread" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),nomnal("star.thread*thread")).
    "_kill" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.thread*thread")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_thread_state" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.thread*thread")])),nomnal("star.thread*threadState")).
    "_waitfor" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.thread*thread")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_shell" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.core*string"),tpExp(tpFun("star.core*cons",1),nomnal("star.core*string")),tpExp(tpFun("star.core*cons",1),tupleType([nomnal("star.core*string"),nomnal("star.core*string")]))])),nomnal("star.core*integer")).
    "_newLock" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),nomnal("star.thread*lock")).
    "_acquireLock" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.thread*lock"),nomnal("star.core*float")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_waitLock" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.thread*lock"),nomnal("star.core*float")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_releaseLock" => ? tpExp(tpExp(tpFun("=>",2),tupleType([nomnal("star.thread*lock")])),tpExp(tpFun("star.core*result",2),nomnal("star.core*string"))).
    "_ins_debug" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),tupleType([])).
    "_stackTrace" => ? tpExp(tpExp(tpFun("=>",2),tupleType([])),nomnal("star.core*string")).
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
    "_big_gcd" => ?34.
    "_big_hash" => ?35.
    "_big_eq" => ?36.
    "_big_lt" => ?37.
    "_big_ge" => ?38.
    "_int2big" => ?39.
    "_big2int" => ?40.
    "_ints2big" => ?41.
    "_big2ints" => ?42.
    "_str2big" => ?43.
    "_big2str" => ?44.
    "_big_format" => ?45.
    "_fiber_eq" => ?46.
    "_new_fiber" => ?47.
    "_spawn" => ?48.
    "_suspend_fiber" => ?49.
    "_retire_fiber" => ?50.
    "_resume_fiber" => ?51.
    "sqrt" => ?52.
    "exp" => ?53.
    "log" => ?54.
    "log10" => ?55.
    "pi" => ?56.
    "sin" => ?57.
    "cos" => ?58.
    "tan" => ?59.
    "asin" => ?60.
    "acos" => ?61.
    "atan" => ?62.
    "trunc" => ?63.
    "floor" => ?64.
    "ceil" => ?65.
    "integral" => ?66.
    "_irand" => ?67.
    "_random" => ?68.
    "_seed" => ?69.
    "_ldexp" => ?70.
    "_frexp" => ?71.
    "_modf" => ?72.
    "_band" => ?73.
    "_bor" => ?74.
    "_bxor" => ?75.
    "_blsl" => ?76.
    "_blsr" => ?77.
    "_basr" => ?78.
    "_bnot" => ?79.
    "_cell" => ?80.
    "_get" => ?81.
    "_assign" => ?82.
    "_overwrite" => ?83.
    "_tuple_nth" => ?84.
    "_tuple_set_nth" => ?85.
    "_cwd" => ?86.
    "_cd" => ?87.
    "_rm" => ?88.
    "_mv" => ?89.
    "_mkdir" => ?90.
    "_rmdir" => ?91.
    "_isdir" => ?92.
    "_file_chmod" => ?93.
    "_ls" => ?94.
    "_repo" => ?95.
    "_file_mode" => ?96.
    "_file_present" => ?97.
    "_file_type" => ?98.
    "_file_size" => ?99.
    "_file_modified" => ?100.
    "_file_date" => ?101.
    "_openInFile" => ?102.
    "_openOutFile" => ?103.
    "_openAppendFile" => ?104.
    "_openAppendIOFile" => ?105.
    "_popen" => ?106.
    "_close" => ?107.
    "_end_of_file" => ?108.
    "_ready_to_read" => ?109.
    "_ready_to_write" => ?110.
    "_inchars" => ?111.
    "_inchar" => ?112.
    "_inbyte" => ?113.
    "_inline" => ?114.
    "_inline_async" => ?115.
    "_intext" => ?116.
    "_outchar" => ?117.
    "_outbyte" => ?118.
    "_outbytes" => ?119.
    "_outtext" => ?120.
    "_stdfile" => ?121.
    "_fposition" => ?122.
    "_fseek" => ?123.
    "_flush" => ?124.
    "_flushall" => ?125.
    "_setfileencoding" => ?126.
    "_get_file" => ?127.
    "_put_file" => ?128.
    "_show" => ?129.
    "_install_pkg" => ?130.
    "_pkg_is_present" => ?131.
    "_in_manifest" => ?132.
    "_locate_in_manifest" => ?133.
    "_logmsg" => ?134.
    "_connect" => ?135.
    "_listen" => ?136.
    "_accept" => ?137.
    "_hosttoip" => ?138.
    "_iptohost" => ?139.
    "_delay" => ?140.
    "_sleep" => ?141.
    "_now" => ?142.
    "_today" => ?143.
    "_ticks" => ?144.
    "_time2date" => ?145.
    "_time2utc" => ?146.
    "_date2time" => ?147.
    "_utc2time" => ?148.
    "_uniCodeCategory" => ?149.
    "_isCcChar" => ?150.
    "_isCfChar" => ?151.
    "_isCnChar" => ?152.
    "_isCoChar" => ?153.
    "_isCsChar" => ?154.
    "_isLlChar" => ?155.
    "_isLmChar" => ?156.
    "_isLoChar" => ?157.
    "_isLtChar" => ?158.
    "_isLuChar" => ?159.
    "_isMcChar" => ?160.
    "_isMeChar" => ?161.
    "_isMnChar" => ?162.
    "_isNdChar" => ?163.
    "_isNlChar" => ?164.
    "_isNoChar" => ?165.
    "_isPcChar" => ?166.
    "_isPdChar" => ?167.
    "_isPeChar" => ?168.
    "_isPfChar" => ?169.
    "_isPiChar" => ?170.
    "_isPoChar" => ?171.
    "_isPsChar" => ?172.
    "_isScChar" => ?173.
    "_isSkChar" => ?174.
    "_isSmChar" => ?175.
    "_isSoChar" => ?176.
    "_isZlChar" => ?177.
    "_isZpChar" => ?178.
    "_isZsChar" => ?179.
    "_isLetterChar" => ?180.
    "_digitCode" => ?181.
    "_codePoint" => ?182.
    "_char" => ?183.
    "_isIDStart" => ?184.
    "_isIDContinue" => ?185.
    "_int2str" => ?186.
    "_flt2str" => ?187.
    "_int_format" => ?188.
    "_flt_format" => ?189.
    "_str2flt" => ?190.
    "_str2int" => ?191.
    "_chr_eq" => ?192.
    "_chr_lt" => ?193.
    "_chr_ge" => ?194.
    "_chr_hash" => ?195.
    "_chr_quote" => ?196.
    "_chr_format" => ?197.
    "_str_eq" => ?198.
    "_str_lt" => ?199.
    "_str_ge" => ?200.
    "_str_hash" => ?201.
    "_str_len" => ?202.
    "_str_gen" => ?203.
    "_stringOf" => ?204.
    "_explode" => ?205.
    "_implode" => ?206.
    "_str_find" => ?207.
    "_sub_str" => ?208.
    "_str_split" => ?209.
    "_str_concat" => ?210.
    "_str_reverse" => ?211.
    "_str_start" => ?212.
    "_str_splice" => ?213.
    "_str_multicat" => ?214.
    "_str_hdtl" => ?215.
    "_str_back" => ?216.
    "_str_cons" => ?217.
    "_code2str" => ?218.
    "_str_apnd" => ?219.
    "_str_quote" => ?220.
    "_str_format" => ?221.
    "_getenv" => ?222.
    "_setenv" => ?223.
    "_envir" => ?224.
    "_getlogin" => ?225.
    "_fork" => ?226.
    "_thread" => ?227.
    "_kill" => ?228.
    "_thread_state" => ?229.
    "_waitfor" => ?230.
    "_shell" => ?231.
    "_newLock" => ?232.
    "_acquireLock" => ?233.
    "_waitLock" => ?234.
    "_releaseLock" => ?235.
    "_ins_debug" => ?236.
    "_stackTrace" => ?237.
    _ default => .none.
  }
}.
