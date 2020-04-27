/* Automatically generated, do not edit */

star.compiler.operators{
  import star.

  import star.compiler.ast.
  import star.compiler.location.

  public astGenerator ::= .noGen
             | genUnary((locn,ast)=>ast)
             | genBinary((locn,ast,ast)=>ast).

  operator ::= prefixOp(integer,integer,astGenerator)
             | infixOp(integer,integer,integer,astGenerator)
             | postfixOp(integer,integer,astGenerator).

  public bracket ::= bkt(string,string,string,integer).

  public isOperator:(string)=>boolean.
  isOperator(Nm) => size(oper(Nm))>0.

  public isInfixOp:(string) => option[(integer,integer,integer,astGenerator)].
  isInfixOp(Nm) => pickInfix(oper(Nm)).

  pickInfix:(cons[operator]) => option[(integer,integer,integer,astGenerator)].
  pickInfix([]) => .none.
  pickInfix([infixOp(Lf,Pr,Rg,Gen),.._]) => some((Lf,Pr,Rg,Gen)).
  pickInfix([_,..L]) => pickInfix(L).

  public isPrefixOp:(string) => option[(integer,integer,astGenerator)].
  isPrefixOp(Nm) => pickPrefix(oper(Nm)).

  pickPrefix:(cons[operator]) => option[(integer,integer,astGenerator)].
  pickPrefix([]) => .none.
  pickPrefix([prefixOp(Pr,Rg,Gen),.._]) => some((Pr,Rg,Gen)).
  pickPrefix([_,..L]) => pickPrefix(L).

  public isPostfixOp:(string) => option[(integer,integer,astGenerator)].
  isPostfixOp(Nm) => pickPostfix(oper(Nm)).

  pickPostfix:(cons[operator]) => option[(integer,integer,astGenerator)].
  pickPostfix([]) => .none.
  pickPostfix([postfixOp(Pr,Rg,Gen),.._]) => some((Pr,Rg,Gen)).
  pickPostfix([_,..L]) => pickPrefix(L).

  oper:(string)=>cons[operator].
  oper("all") => [prefixOp(1010,1009,.noGen)].
  oper("^=") => [infixOp(899,900,899,.noGen)].
  oper("&&") => [infixOp(910,910,909,.noGen)].
  oper("pure") => [prefixOp(300,299,.noGen)].
  oper("~>") => [infixOp(1230,1231,1230,.noGen)].
  oper("throw") => [prefixOp(930,929,.noGen)].
  oper(".|.") => [infixOp(720,720,719,.noGen)].
  oper("do") => [prefixOp(200,199,.noGen), infixOp(1199,1200,1199,.noGen)].
  oper("import") => [prefixOp(900,899,.noGen)].
  oper("catch") => [infixOp(1198,1199,1198,.noGen)].
  oper("of") => [infixOp(399,400,399,.noGen)].
  oper("valis") => [prefixOp(930,929,.noGen)].
  oper(",..") => [infixOp(999,1000,999,.noGen)].
  oper("for") => [prefixOp(1175,1174,.noGen)].
  oper("**") => [infixOp(600,600,599,.noGen)].
  oper("->") => [infixOp(889,890,889,.noGen)].
  oper(".+.") => [prefixOp(700,699,.noGen)].
  oper("<$") => [infixOp(719,720,720,.noGen)].
  oper("then") => [infixOp(1179,1180,1179,.noGen)].
  oper("!") => [prefixOp(905,904,.noGen)].
  oper("->>") => [infixOp(1199,1200,1199,.noGen)].
  oper("=!=") => [infixOp(899,900,899,.noGen)].
  oper("default") => [postfixOp(939,940,.noGen)].
  oper("#") => [prefixOp(1750,1749,.noGen), infixOp(759,760,759,.noGen)].
  oper("%") => [infixOp(700,700,699,.noGen)].
  oper("<-") => [infixOp(904,905,904,.noGen)].
  oper(".>>>.") => [infixOp(600,600,599,.noGen)].
  oper("<<-") => [infixOp(974,975,974,.noGen)].
  oper("*") => [postfixOp(699,700,.noGen), infixOp(700,700,699,.noGen)].
  oper("+") => [postfixOp(699,700,.noGen), infixOp(720,720,719,.noGen)].
  oper(".>>.") => [infixOp(600,600,599,.noGen)].
  oper("*>") => [infixOp(904,905,904,genBinary((Lc,X,Y)=>astImplies(Lc,X,Y)))].
  oper(",") => [infixOp(999,1000,1000,.noGen)].
  oper("contract") => [prefixOp(1260,1259,.noGen)].
  oper("\\/") => [infixOp(720,720,719,.noGen)].
  oper("-") => [prefixOp(300,299,.noGen), infixOp(720,720,719,.noGen)].
  oper(".") => [prefixOp(10,9,.noGen), infixOp(100,100,99,.noGen)].
  oper("/") => [infixOp(700,700,699,.noGen)].
  oper("<*>") => [infixOp(949,950,950,.noGen)].
  oper("val") => [prefixOp(900,899,.noGen)].
  oper("try") => [prefixOp(1200,1199,.noGen)].
  oper("exists") => [prefixOp(1010,1009,.noGen)].
  oper("if") => [prefixOp(1175,1174,.noGen)].
  oper("background") => [prefixOp(950,949,.noGen)].
  oper(":") => [infixOp(1249,1250,1249,.noGen)].
  oper(";") => [infixOp(1250,1251,1251,.noGen)].
  oper("<") => [infixOp(899,900,899,.noGen)].
  oper(".=") => [infixOp(899,900,899,.noGen)].
  oper("=") => [infixOp(974,975,974,.noGen)].
  oper("|:") => [infixOp(1234,1235,1234,.noGen)].
  oper("show") => [prefixOp(1240,1239,.noGen)].
  oper("++") => [infixOp(719,720,720,.noGen)].
  oper(">") => [infixOp(899,900,899,.noGen)].
  oper("return") => [prefixOp(930,929,.noGen)].
  oper("?") => [infixOp(919,920,920,.noGen)].
  oper("@") => [prefixOp(400,399,.noGen), infixOp(399,400,400,.noGen)].
  oper("in") => [infixOp(899,900,900,.noGen)].
  oper("^|") => [infixOp(919,920,920,.noGen)].
  oper("open") => [prefixOp(900,899,.noGen)].
  oper("~~") => [infixOp(1239,1240,1240,.noGen)].
  oper("assert") => [prefixOp(1240,1239,.noGen)].
  oper("!!") => [postfixOp(99,100,.noGen)].
  oper(".^.") => [infixOp(720,720,719,.noGen)].
  oper("//") => [infixOp(960,960,959,.noGen)].
  oper("public") => [prefixOp(1700,1699,.noGen)].
  oper("ref") => [prefixOp(899,898,.noGen)].
  oper(".~.") => [prefixOp(650,649,.noGen)].
  oper("where") => [infixOp(910,911,910,.noGen)].
  oper("=<") => [infixOp(899,900,899,.noGen)].
  oper("case") => [prefixOp(901,900,.noGen)].
  oper("==") => [infixOp(899,900,899,.noGen)].
  oper("\\") => [infixOp(700,700,699,.noGen)].
  oper("=>") => [infixOp(949,950,950,.noGen)].
  oper("^") => [prefixOp(100,99,.noGen), infixOp(99,100,99,.noGen)].
  oper("<=>") => [infixOp(949,950,949,.noGen)].
  oper("valof") => [prefixOp(300,299,.noGen)].
  oper("while") => [prefixOp(1175,1174,.noGen)].
  oper("private") => [prefixOp(1200,1199,.noGen)].
  oper("•") => [infixOp(450,450,449,.noGen)].
  oper(".&.") => [infixOp(700,700,699,.noGen)].
  oper("///") => [infixOp(960,960,959,.noGen)].
  oper("::") => [infixOp(399,400,399,.noGen)].
  oper("+++") => [infixOp(719,720,720,.noGen)].
  oper(":=") => [infixOp(974,975,974,.noGen)].
  oper(".<<.") => [infixOp(600,600,599,.noGen)].
  oper("^.") => [infixOp(450,450,449,.noGen)].
  oper(">>=") => [infixOp(949,950,950,.noGen)].
  oper("^/") => [infixOp(960,960,959,.noGen)].
  oper("<~") => [infixOp(1230,1231,1230,.noGen)].
  oper("type") => [prefixOp(1251,1250,.noGen)].
  oper("implementation") => [prefixOp(1260,1259,.noGen)].
  oper("|") => [infixOp(1248,1248,1247,.noGen)].
  oper(".#.") => [infixOp(600,600,599,.noGen)].
  oper("^//") => [infixOp(800,800,799,.noGen)].
  oper("||") => [infixOp(919,920,920,.noGen)].
  oper("else") => [infixOp(1199,1200,1200,.noGen)].
  oper("::=") => [infixOp(1249,1250,1249,.noGen)].
  oper("/\\") => [infixOp(700,700,699,.noGen)].
  oper(">=") => [infixOp(899,900,899,.noGen)].
  oper(">>") => [infixOp(949,950,950,.noGen)].
  oper(_) default => [].

  public isBracket:(string) => option[bracket].
  isBracket("{.") => some(bkt("{.","{..}",".}",2000)).
  isBracket(".}") => some(bkt("{.","{..}",".}",2000)).
  isBracket("{..}") => some(bkt("{.","{..}",".}",2000)).
  isBracket("[") => some(bkt("[","[]","]",2000)).
  isBracket("]") => some(bkt("[","[]","]",2000)).
  isBracket("[]") => some(bkt("[","[]","]",2000)).
  isBracket("(") => some(bkt("(","()",")",2000)).
  isBracket(")") => some(bkt("(","()",")",2000)).
  isBracket("()") => some(bkt("(","()",")",2000)).
  isBracket("{") => some(bkt("{","{}","}",2000)).
  isBracket("}") => some(bkt("{","{}","}",2000)).
  isBracket("{}") => some(bkt("{","{}","}",2000)).
  isBracket("(|") => some(bkt("(|","(||)","|)",2000)).
  isBracket("|)") => some(bkt("(|","(||)","|)",2000)).
  isBracket("(||)") => some(bkt("(|","(||)","|)",2000)).
  isBracket(_) default => .none.

  public isLeftBracket:(string) => boolean.
  isLeftBracket(S) => bkt(S,_,_,_) ^= isBracket(S).

  public isRightBracket:(string) => boolean.
  isRightBracket(S) => bkt(_,_,S,_) ^= isBracket(S).

  public first:(integer) => option[string].
  first(0c%) => some("%").
  first(0c&) => some("&").
  first(0c() => some("(").
  first(0c)) => some(")").
  first(0c*) => some("*").
  first(0c+) => some("+").
  first(0c,) => some(",").
  first(0c-) => some("-").
  first(0c.) => some(".").
  first(0c/) => some("/").
  first(0c{) => some("{").
  first(0c|) => some("|").
  first(0c}) => some("}").
  first(0c~) => some("~").
  first(0c[) => some("[").
  first(0c\\) => some("\\").
  first(0c]) => some("]").
  first(0c^) => some("^").
  first(0c:) => some(":").
  first(0c;) => some(";").
  first(0c<) => some("<").
  first(0c=) => some("=").
  first(0c>) => some(">").
  first(0c?) => some("?").
  first(0c@) => some("@").
  first(0c!) => some("!").
  first(0c•) => some("•").
  first(0c#) => some("#").
  first(_) default => .none.

  public follows:(string,integer) => option[string].
  follows("&",0c&) => some("&&").
  follows("(",0c|) => some("(|").
  follows("*",0c*) => some("**").
  follows("*",0c>) => some("*>").
  follows("+",0c+) => some("++").
  follows("++",0c+) => some("+++").
  follows(",",0c.) => some(",.").
  follows(",.",0c.) => some(",..").
  follows("-",0c>) => some("->").
  follows("->",0c>) => some("->>").
  follows(".",0c#) => some(".#").
  follows(".",0c&) => some(".&").
  follows(".",0c|) => some(".|").
  follows(".",0c}) => some(".}").
  follows(".",0c~) => some(".~").
  follows(".",0c<) => some(".<").
  follows(".",0c^) => some(".^").
  follows(".",0c+) => some(".+").
  follows(".",0c=) => some(".=").
  follows(".",0c>) => some(".>").
  follows(".",0c ) => some(". ").
  follows(".#",0c.) => some(".#.").
  follows(".&",0c.) => some(".&.").
  follows(".|",0c.) => some(".|.").
  follows(".~",0c.) => some(".~.").
  follows(".<",0c<) => some(".<<").
  follows(".<<",0c.) => some(".<<.").
  follows(".^",0c.) => some(".^.").
  follows(".+",0c.) => some(".+.").
  follows(".>",0c>) => some(".>>").
  follows(".>>",0c.) => some(".>>.").
  follows(".>>",0c>) => some(".>>>").
  follows(".>>>",0c.) => some(".>>>.").
  follows("/",0c\\) => some("/\\").
  follows("/",0c/) => some("//").
  follows("//",0c/) => some("///").
  follows("{",0c.) => some("{.").
  follows("|",0c:) => some("|:").
  follows("|",0c|) => some("||").
  follows("|",0c)) => some("|)").
  follows("~",0c~) => some("~~").
  follows("~",0c>) => some("~>").
  follows("\\",0c/) => some("\\/").
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
  follows("<",0c$) => some("<$").
  follows("<",0c-) => some("<-").
  follows("<",0c<) => some("<<").
  follows("<",0c=) => some("<=").
  follows("<*",0c>) => some("<*>").
  follows("<<",0c-) => some("<<-").
  follows("<=",0c>) => some("<=>").
  follows("=",0c<) => some("=<").
  follows("=",0c!) => some("=!").
  follows("=",0c=) => some("==").
  follows("=",0c>) => some("=>").
  follows("=!",0c=) => some("=!=").
  follows(">",0c=) => some(">=").
  follows(">",0c>) => some(">>").
  follows(">>",0c=) => some(">>=").
  follows("!",0c!) => some("!!").
  follows(_,_) default => .none.

  public final:(string) => boolean.
  final("%") => .true.  /* modulo */
  final("&&") => .true.  /* conjunction */
  final("(") => .true.  /* parentheses */
  final("(|") => .true.  /* banana brackets */
  final(")") => .true.  /* parentheses */
  final("*") => .true.  /* zero or more repetitions */
  final("**") => .true.  /* exponentiation */
  final("*>") => .true.  /* for all */
  final("+") => .true.  /* one or more repetitions */
  final("++") => .true.  /* concatenate */
  final("+++") => .true.  /* choice */
  final(",") => .true.  /* tupling operator */
  final(",..") => .true.  /* list cons */
  final("-") => .true.  /* arithmetic negation */
  final("->") => .true.  /* map entry */
  final("->>") => .true.  /* dependent type marker */
  final(".") => .true.  /* identify enumerator */
  final(".#.") => .true.  /* test nth bit */
  final(".&.") => .true.  /* bitwise and */
  final(".|.") => .true.  /* bitwise or */
  final(".}") => .true.  /* non-recursive braces */
  final(".~.") => .true.  /* bitwise 1's complement */
  final(".<<.") => .true.  /* shift left */
  final(".^.") => .true.  /* bitwise xor */
  final(".+.") => .true.  /* count of number of bits */
  final(".=") => .true.  /* pattern match */
  final(".>>.") => .true.  /* logical shift right */
  final(".>>>.") => .true.  /* arithmetic shift right */
  final(". ") => .true.  /* statement terminator */
  final("/") => .true.  /* division */
  final("/\\") => .true.  /* intersection */
  final("//") => .true.  /* map over */
  final("///") => .true.  /* indexed map over */
  final("{") => .true.  /* braces */
  final("{.") => .true.  /* non-recursive braces */
  final("|") => .true.  /* type union, conditional, and abstraction */
  final("|:") => .true.  /* constrained type */
  final("||") => .true.  /* disjunction */
  final("|)") => .true.  /* banana brackets */
  final("}") => .true.  /* braces */
  final("~~") => .true.  /* quantifier */
  final("~>") => .true.  /* type function */
  final("[") => .true.  /* square brackets */
  final("\\") => .true.  /* difference */
  final("\\/") => .true.  /* union */
  final("]") => .true.  /* square brackets */
  final("^") => .true.  /* Optional propagation */
  final("^.") => .true.  /* optional object access */
  final("^/") => .true.  /* filter */
  final("^//") => .true.  /* filter map */
  final("^=") => .true.  /* optional decomposition match */
  final("^|") => .true.  /* option or-else operator */
  final(":") => .true.  /* type annotation */
  final("::") => .true.  /* type coercion */
  final("::=") => .true.  /* algebraic type definition */
  final(":=") => .true.  /* reassignable variable definition */
  final(";") => .true.  /* sequencing operator */
  final("<") => .true.  /* less than */
  final("<*>") => .true.  /* applicative splat */
  final("<~") => .true.  /* type interface rule */
  final("<$") => .true.  /* constant replace */
  final("<-") => .true.  /* variable bind */
  final("<<-") => .true.  /* record replacement */
  final("<=>") => .true.  /* constructor arrow */
  final("=") => .true.  /* definition */
  final("=<") => .true.  /* less than or equal */
  final("=!=") => .true.  /* not equals */
  final("==") => .true.  /* equality predicate */
  final("=>") => .true.  /* function arrow */
  final(">") => .true.  /* greater than */
  final(">=") => .true.  /* greater than or equal */
  final(">>") => .true.  /* monadic bind */
  final(">>=") => .true.  /* monadic bind */
  final("?") => .true.  /* conditional operator */
  final("@") => .true.  /* meta annotation */
  final("!") => .true.  /* logical negation */
  final("!!") => .true.  /* pick up value from a ref cell */
  final("•") => .true.  /* function composition */
  final("#") => .true.  /* Macro statement marker */
  final(_) default => .false.

  public keyword:(string) => boolean.
  keyword("all") => .true.
  keyword("^=") => .true.
  keyword("&&") => .true.
  keyword("~>") => .true.
  keyword("throw") => .true.
  keyword("do") => .true.
  keyword("do") => .true.
  keyword("import") => .true.
  keyword("catch") => .true.
  keyword("of") => .true.
  keyword("valis") => .true.
  keyword(",..") => .true.
  keyword("for") => .true.
  keyword("then") => .true.
  keyword("!") => .true.
  keyword("->>") => .true.
  keyword("default") => .true.
  keyword("#") => .true.
  keyword("#") => .true.
  keyword("<-") => .true.
  keyword("<<-") => .true.
  keyword("*>") => .true.
  keyword(",") => .true.
  keyword("contract") => .true.
  keyword(".") => .true.
  keyword(".") => .true.
  keyword("val") => .true.
  keyword("try") => .true.
  keyword("exists") => .true.
  keyword("if") => .true.
  keyword(":") => .true.
  keyword(";") => .true.
  keyword(".=") => .true.
  keyword("=") => .true.
  keyword("|:") => .true.
  keyword("?") => .true.
  keyword("@") => .true.
  keyword("@") => .true.
  keyword("in") => .true.
  keyword("open") => .true.
  keyword("~~") => .true.
  keyword("public") => .true.
  keyword("ref") => .true.
  keyword("where") => .true.
  keyword("case") => .true.
  keyword("=>") => .true.
  keyword("^") => .true.
  keyword("^") => .true.
  keyword("<=>") => .true.
  keyword("valof") => .true.
  keyword("while") => .true.
  keyword("private") => .true.
  keyword("::") => .true.
  keyword("^.") => .true.
  keyword("<~") => .true.
  keyword("type") => .true.
  keyword("implementation") => .true.
  keyword("|") => .true.
  keyword("||") => .true.
  keyword("else") => .true.
  keyword("::=") => .true.
  keyword(_) default => .false.
}
