/* Automatically generated, do not edit */

star.compiler.operators{
  import star.

  operator ::= prefixOp(integer,integer)
             | infixOp(integer,integer,integer)
             | postfixOp(integer,integer).

  public bracket ::= bkt(string,string,string,integer).

  public isOperator:(string)=>boolean.
  isOperator(Nm) => size(oper(Nm))>0.

  public isInfixOp:(string) => option[(integer,integer,integer)].
  isInfixOp(Nm) => pickInfix(oper(Nm)).

  pickInfix:(list[operator]) => option[(integer,integer,integer)].
  pickInfix([]) => none.
  pickInfix([infixOp(Lf,Pr,Rg),.._]) => some((Lf,Pr,Rg)).
  pickInfix([_,..L]) => pickInfix(L).

  public isPrefixOp:(string) => option[(integer,integer)].
  isPrefixOp(Nm) => pickPrefix(oper(Nm)).

  pickPrefix:(list[operator]) => option[(integer,integer)].
  pickPrefix([]) => none.
  pickPrefix([prefixOp(Pr,Rg),.._]) => some((Pr,Rg)).
  pickPrefix([_,..L]) => pickPrefix(L).

  public isPostfixOp:(string) => option[(integer,integer)].
  isPostfixOp(Nm) => pickPostfix(oper(Nm)).

  pickPostfix:(list[operator]) => option[(integer,integer)].
  pickPostfix([]) => none.
  pickPostfix([postfixOp(Pr,Rg),.._]) => some((Pr,Rg)).
  pickPostfix([_,..L]) => pickPrefix(L).

  oper:(string)=>list[operator].
  oper("all") => [prefixOp(1010,1009)].
  oper("^=") => [infixOp(899,900,899)].
  oper("&&") => [infixOp(910,910,909)].
  oper("pure") => [prefixOp(300,299)].
  oper("..,") => [infixOp(999,1000,1000)].
  oper("~>") => [infixOp(1230,1231,1230)].
  oper(".|.") => [infixOp(720,720,719)].
  oper("import") => [prefixOp(900,899)].
  oper("of") => [infixOp(399,400,399)].
  oper(",..") => [infixOp(999,1000,999)].
  oper("==>") => [infixOp(949,950,949)].
  oper("**") => [infixOp(600,600,599)].
  oper("->") => [infixOp(899,900,899)].
  oper("raise") => [prefixOp(300,299)].
  oper(". ") => [postfixOp(1899,1900), infixOp(1899,1900,1900)].
  oper("!") => [postfixOp(99,100)].
  oper("->>") => [infixOp(1199,1200,1199)].
  oper("=!=") => [infixOp(899,900,899)].
  oper("default") => [postfixOp(939,940)].
  oper("#") => [prefixOp(1750,1749), infixOp(759,760,759)].
  oper("^^") => [infixOp(999,1000,999)].
  oper("%") => [infixOp(700,700,699)].
  oper("<-") => [infixOp(924,925,924)].
  oper(".>>>.") => [infixOp(600,600,599)].
  oper("\\+") => [prefixOp(905,904)].
  oper("*") => [postfixOp(699,700), infixOp(700,700,699)].
  oper("+") => [postfixOp(699,700), infixOp(720,720,719)].
  oper(".>>.") => [infixOp(600,600,599)].
  oper(",") => [infixOp(999,1000,1000)].
  oper("contract") => [prefixOp(1260,1259)].
  oper("-") => [prefixOp(300,299), infixOp(720,720,719)].
  oper(".") => [infixOp(100,100,99)].
  oper("/") => [infixOp(700,700,699)].
  oper("<*>") => [infixOp(949,950,950)].
  oper("•") => [infixOp(450,450,449)].
  oper("exists") => [prefixOp(1010,1009)].
  oper("<=") => [infixOp(949,950,949)].
  oper(":") => [infixOp(1249,1250,1249)].
  oper("-->") => [infixOp(1199,1200,1199)].
  oper(";") => [infixOp(1899,1900,1900)].
  oper("<") => [infixOp(899,900,899)].
  oper(".=") => [infixOp(899,900,899)].
  oper("=") => [infixOp(974,975,974)].
  oper("|:") => [infixOp(1234,1235,1234)].
  oper("show") => [prefixOp(1260,1259)].
  oper("++") => [infixOp(719,720,720)].
  oper(">") => [infixOp(899,900,899)].
  oper("return") => [prefixOp(300,299)].
  oper("?") => [infixOp(1148,1149,1149)].
  oper("@") => [prefixOp(400,399), infixOp(399,400,400)].
  oper("in") => [infixOp(899,900,899)].
  oper("^|") => [infixOp(1248,1249,1249)].
  oper("open") => [prefixOp(900,899)].
  oper("~~") => [infixOp(1239,1240,1240)].
  oper("assert") => [prefixOp(1260,1259)].
  oper("=.") => [infixOp(899,900,899)].
  oper(".^.") => [infixOp(720,720,719)].
  oper("//") => [infixOp(800,800,799)].
  oper("public") => [prefixOp(1700,1699)].
  oper("ref") => [prefixOp(900,899)].
  oper(".~.") => [prefixOp(650,649)].
  oper("where") => [infixOp(929,930,929)].
  oper("=<") => [infixOp(899,900,899)].
  oper("==") => [infixOp(899,900,899)].
  oper("=>") => [infixOp(949,950,950)].
  oper("^") => [prefixOp(100,99), infixOp(99,100,99)].
  oper("<=>") => [infixOp(949,950,949)].
  oper("private") => [prefixOp(1700,1699)].
  oper(".&.") => [infixOp(700,700,699)].
  oper("///") => [infixOp(800,800,799)].
  oper("::") => [infixOp(399,400,399)].
  oper("+++") => [infixOp(719,720,720)].
  oper(":=") => [infixOp(974,975,974)].
  oper(".<<.") => [infixOp(600,600,599)].
  oper("^+") => [prefixOp(905,904)].
  oper("^.") => [infixOp(450,450,449)].
  oper(">>=") => [infixOp(949,950,950)].
  oper("^/") => [infixOp(800,800,799)].
  oper("<~") => [infixOp(1230,1231,1230)].
  oper("type") => [prefixOp(1251,1250)].
  oper("implementation") => [prefixOp(1260,1259)].
  oper("|") => [infixOp(1248,1249,1249)].
  oper(".~") => [infixOp(499,500,499)].
  oper(".#.") => [infixOp(600,600,599)].
  oper("~") => [infixOp(489,499,489)].
  oper("^//") => [infixOp(800,800,799)].
  oper("||") => [infixOp(1148,1149,1149)].
  oper("::=") => [infixOp(1249,1250,1249)].
  oper(">=") => [infixOp(899,900,899)].
  oper(">>") => [infixOp(949,950,950)].
  oper(_) default => [].

  public isBracket:(string) => option[bracket].
  isBracket("{.") => some(bkt("{.","{..}",".}",2000)).
  isBracket(".}") => some(bkt("{.","{..}",".}",2000)).
  isBracket("{..}") => some(bkt("{.","{..}",".}",2000)).
  isBracket("[") => some(bkt("[","[]","]",2000)).
  isBracket("]") => some(bkt("[","[]","]",2000)).
  isBracket("[]") => some(bkt("[","[]","]",2000)).
  isBracket("(.") => some(bkt("(.","(..)",".)",2000)).
  isBracket(".)") => some(bkt("(.","(..)",".)",2000)).
  isBracket("(..)") => some(bkt("(.","(..)",".)",2000)).
  isBracket("(") => some(bkt("(","()",")",2000)).
  isBracket(")") => some(bkt("(","()",")",2000)).
  isBracket("()") => some(bkt("(","()",")",2000)).
  isBracket("{") => some(bkt("{","{}","}",2000)).
  isBracket("}") => some(bkt("{","{}","}",2000)).
  isBracket("{}") => some(bkt("{","{}","}",2000)).
  isBracket(_) default => none.

  public isLeftBracket:(string) => boolean.
  isLeftBracket(S) => bkt(S,_,_,_) ^= isBracket(S).

  public isRightBracket:(string) => boolean.
  isRightBracket(S) => bkt(_,_,S,_) ^= isBracket(S).

  public follows:(string,integer) => option[string].
    follows("",0c%) => some("%").
  follows("",0c&) => some("&").
  follows("",0c() => some("(").
  follows("",0c)) => some(")").
  follows("",0c*) => some("*").
  follows("",0c+) => some("+").
  follows("",0c,) => some(",").
  follows("",0c-) => some("-").
  follows("",0c.) => some(".").
  follows("",0c/) => some("/").
  follows("",0c{) => some("{").
  follows("",0c|) => some("|").
  follows("",0c}) => some("}").
  follows("",0c~) => some("~").
  follows("",0c[) => some("[").
  follows("",0c\\) => some("\\").
  follows("",0c]) => some("]").
  follows("",0c^) => some("^").
  follows("",0c:) => some(":").
  follows("",0c;) => some(";").
  follows("",0c<) => some("<").
  follows("",0c=) => some("=").
  follows("",0c>) => some(">").
  follows("",0c?) => some("?").
  follows("",0c@) => some("@").
  follows("",0c!) => some("!").
  follows("",0c•) => some("•").
  follows("",0c#) => some("#").
  follows("",0c$) => some("$").
  follows("&",0c&) => some("&&").
  follows("(",0c.) => some("(.").
  follows("*",0c*) => some("**").
  follows("+",0c+) => some("++").
  follows("++",0c+) => some("+++").
  follows(",",0c.) => some(",.").
  follows(",.",0c.) => some(",..").
  follows("-",0c-) => some("--").
  follows("-",0c>) => some("->").
  follows("--",0c>) => some("-->").
  follows("->",0c>) => some("->>").
  follows(".",0c#) => some(".#").
  follows(".",0c&) => some(".&").
  follows(".",0c|) => some(".|").
  follows(".",0c}) => some(".}").
  follows(".",0c)) => some(".)").
  follows(".",0c~) => some(".~").
  follows(".",0c<) => some(".<").
  follows(".",0c^) => some(".^").
  follows(".",0c=) => some(".=").
  follows(".",0c>) => some(".>").
  follows(".",0c.) => some("..").
  follows(".",0c ) => some(". ").
  follows(".#",0c.) => some(".#.").
  follows(".&",0c.) => some(".&.").
  follows(".|",0c.) => some(".|.").
  follows(".~",0c.) => some(".~.").
  follows(".<",0c<) => some(".<<").
  follows(".<<",0c.) => some(".<<.").
  follows(".^",0c.) => some(".^.").
  follows(".>",0c>) => some(".>>").
  follows(".>>",0c.) => some(".>>.").
  follows(".>>",0c>) => some(".>>>").
  follows(".>>>",0c.) => some(".>>>.").
  follows("..",0c,) => some("..,").
  follows("/",0c/) => some("//").
  follows("//",0c/) => some("///").
  follows("{",0c.) => some("{.").
  follows("|",0c:) => some("|:").
  follows("|",0c|) => some("||").
  follows("~",0c~) => some("~~").
  follows("~",0c>) => some("~>").
  follows("\\",0c+) => some("\\+").
  follows("^",0c+) => some("^+").
  follows("^",0c^) => some("^^").
  follows("^",0c.) => some("^.").
  follows("^",0c/) => some("^/").
  follows("^",0c=) => some("^=").
  follows("^",0c|) => some("^|").
  follows("^/",0c/) => some("^//").
  follows(":",0c:) => some("::").
  follows(":",0c=) => some(":=").
  follows("::",0c=) => some("::=").
  follows("<",0c*) => some("<*").
  follows("<",0c~) => some("<~").
  follows("<",0c-) => some("<-").
  follows("<",0c=) => some("<=").
  follows("<*",0c>) => some("<*>").
  follows("<=",0c>) => some("<=>").
  follows("=",0c<) => some("=<").
  follows("=",0c.) => some("=.").
  follows("=",0c!) => some("=!").
  follows("=",0c=) => some("==").
  follows("=",0c>) => some("=>").
  follows("=!",0c=) => some("=!=").
  follows("==",0c>) => some("==>").
  follows(">",0c=) => some(">=").
  follows(">",0c>) => some(">>").
  follows(">>",0c=) => some(">>=").
  follows(_,_) default => none.

  public final:(string) => boolean.
  final("%") => true.  /* modulo */
  final("&&") => true.  /* conjunction */
  final("(") => true.  /* parentheses */
  final("(.") => true.  /* hidden parentheses */
  final(")") => true.  /* parentheses */
  final("*") => true.  /* zero or more repetitions */
  final("**") => true.  /* exponentiation */
  final("+") => true.  /* one or more repetitions */
  final("++") => true.  /* concatenate */
  final("+++") => true.  /* choice */
  final(",") => true.  /* tupling operator */
  final(",..") => true.  /* list cons */
  final("-") => true.  /* arithmetic negation */
  final("-->") => true.  /* grammar rule */
  final("->") => true.  /* map entry */
  final("->>") => true.  /* dependent type marker */
  final(".") => true.  /* object access */
  final(".#.") => true.  /* test nth bit */
  final(".&.") => true.  /* bitwise and */
  final(".|.") => true.  /* bitwise or */
  final(".}") => true.  /* non-recursive braces */
  final(".)") => true.  /* hidden parentheses */
  final(".~") => true.  /* grammar parse */
  final(".~.") => true.  /* bitwise 1's complement */
  final(".<<.") => true.  /* shift left */
  final(".^.") => true.  /* bitwise xor */
  final(".=") => true.  /* pattern match */
  final(".>>.") => true.  /* logical shift right */
  final(".>>>.") => true.  /* arithmetic shift right */
  final("..,") => true.  /* list cons */
  final(". ") => true.  /* statement terminator */
  final("/") => true.  /* division */
  final("//") => true.  /* map over */
  final("///") => true.  /* indexed map over */
  final("{") => true.  /* braces */
  final("{.") => true.  /* non-recursive braces */
  final("|") => true.  /* type union, conditional, and abstraction */
  final("|:") => true.  /* constrained type */
  final("||") => true.  /* disjunction */
  final("}") => true.  /* braces */
  final("~") => true.  /* grammar remainder */
  final("~~") => true.  /* quantifier */
  final("~>") => true.  /* type function */
  final("[") => true.  /* square brackets */
  final("\\+") => true.  /* logical negation */
  final("]") => true.  /* square brackets */
  final("^") => true.  /* Optional propagation */
  final("^+") => true.  /* look ahead parser operator */
  final("^^") => true.  /* Overall output from a parser rule */
  final("^.") => true.  /* optional object access */
  final("^/") => true.  /* filter */
  final("^//") => true.  /* filter map */
  final("^=") => true.  /* optional decomposition match */
  final("^|") => true.  /* option or-else operator */
  final(":") => true.  /* type annotation */
  final("::") => true.  /* type coercion */
  final("::=") => true.  /* algebraic type definition */
  final(":=") => true.  /* reassignable variable definition */
  final(";") => true.  /* sequencing operator */
  final("<") => true.  /* less than */
  final("<*>") => true.  /* applicative splat */
  final("<~") => true.  /* type interface rule */
  final("<-") => true.  /* variable bind */
  final("<=") => true.  /* pattern arrow */
  final("<=>") => true.  /* constructor arrow */
  final("=") => true.  /* definition */
  final("=<") => true.  /* less than or equal */
  final("=.") => true.  /* pattern match */
  final("=!=") => true.  /* not equals */
  final("==") => true.  /* equality predicate */
  final("==>") => true.  /* macro arrow */
  final("=>") => true.  /* function arrow */
  final(">") => true.  /* greater than */
  final(">=") => true.  /* greater than or equal */
  final(">>") => true.  /* monadic bind */
  final(">>=") => true.  /* monadic bind */
  final("?") => true.  /* conditional operator */
  final("@") => true.  /* meta annotation */
  final("!") => true.  /* pick up a value from a ref cell */
  final("•") => true.  /* function composition */
  final("#") => true.  /* Macro statement marker */
  final("$") => true.  /* Used for curried functions and types */
  final(_) default => false.
}
