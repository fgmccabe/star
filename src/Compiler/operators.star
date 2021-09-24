/* Automatically generated, do not edit */

star.compiler.operators{
  import star.

  operator ::= prefixOp(integer,integer)
             | infixOp(integer,integer,integer)
             | postfixOp(integer,integer).

  public bracket ::= bkt(string,string,string,string,integer).

  public isOperator:(string)=>boolean.
  isOperator(Nm) => size(oper(Nm))>0.

  public isInfixOp:(string) => option[(integer,integer,integer)].
  isInfixOp(Nm) => pickInfix(oper(Nm)).

  pickInfix:(cons[operator]) => option[(integer,integer,integer)].
  pickInfix([]) => .none.
  pickInfix([infixOp(Lf,Pr,Rg),.._]) => some((Lf,Pr,Rg)).
  pickInfix([_,..L]) => pickInfix(L).

  public isPrefixOp:(string) => option[(integer,integer)].
  isPrefixOp(Nm) => pickPrefix(oper(Nm)).

  pickPrefix:(cons[operator]) => option[(integer,integer)].
  pickPrefix([]) => .none.
  pickPrefix([prefixOp(Pr,Rg),.._]) => some((Pr,Rg)).
  pickPrefix([_,..L]) => pickPrefix(L).

  public isPostfixOp:(string) => option[(integer,integer)].
  isPostfixOp(Nm) => pickPostfix(oper(Nm)).

  pickPostfix:(cons[operator]) => option[(integer,integer)].
  pickPostfix([]) => .none.
  pickPostfix([postfixOp(Pr,Rg),.._]) => some((Pr,Rg)).
  pickPostfix([_,..L]) => pickPrefix(L).

  oper:(string)=>cons[operator].
  oper("all") => [prefixOp(1010,1009)].
  oper(".<.") => [infixOp(699,700,699)].
  oper("^=") => [infixOp(899,900,899)].
  oper("&&") => [infixOp(910,910,909)].
  oper("pure") => [prefixOp(300,299)].
  oper("~=") => [infixOp(899,900,899)].
  oper("~>") => [infixOp(1230,1231,1230)].
  oper("throw") => [prefixOp(930,929)].
  oper(".|.") => [infixOp(720,720,719)].
  oper("do") => [prefixOp(200,199), infixOp(1199,1200,1199)].
  oper("import") => [prefixOp(900,899)].
  oper("catch") => [infixOp(1198,1199,1198)].
  oper("prompt") => [infixOp(299,300,299)].
  oper("valis") => [prefixOp(930,929)].
  oper(",..") => [infixOp(999,1000,999)].
  oper("for") => [prefixOp(1175,1174)].
  oper("**") => [infixOp(600,600,599)].
  oper("->") => [infixOp(889,890,889)].
  oper(".+.") => [prefixOp(700,699)].
  oper("ignore") => [prefixOp(930,929)].
  oper("<$") => [infixOp(719,720,720)].
  oper("then") => [infixOp(1179,1180,1179)].
  oper("!") => [postfixOp(99,100), infixOp(99,100,99)].
  oper("->>") => [infixOp(1199,1200,1199)].
  oper("default") => [postfixOp(939,940)].
  oper("#") => [prefixOp(1750,1749), infixOp(759,760,759)].
  oper("%") => [infixOp(700,700,699)].
  oper("<-") => [infixOp(904,905,904)].
  oper(".>>>.") => [infixOp(600,600,599)].
  oper("\\+") => [infixOp(700,700,699)].
  oper("<<-") => [infixOp(974,975,974)].
  oper("*") => [postfixOp(699,700), infixOp(700,700,699)].
  oper("\\-") => [infixOp(700,700,699)].
  oper("+") => [postfixOp(699,700), infixOp(720,720,719)].
  oper(".>>.") => [infixOp(600,600,599)].
  oper("*>") => [infixOp(904,905,904)].
  oper(",") => [infixOp(999,1000,1000)].
  oper("contract") => [prefixOp(1260,1259)].
  oper("\\/") => [infixOp(720,720,719)].
  oper("-") => [prefixOp(300,299), infixOp(720,720,719)].
  oper(".") => [prefixOp(10,9), infixOp(100,100,99)].
  oper("/") => [infixOp(700,700,699)].
  oper("<*>") => [infixOp(949,950,950)].
  oper("val") => [prefixOp(900,899)].
  oper("try") => [prefixOp(1200,1199)].
  oper("exists") => [prefixOp(1010,1009)].
  oper("if") => [prefixOp(1175,1174)].
  oper("$$") => [prefixOp(899,898)].
  oper("background") => [prefixOp(950,949)].
  oper(":") => [infixOp(1249,1250,1249)].
  oper(";") => [postfixOp(1250,1251), infixOp(1250,1251,1251)].
  oper("<") => [infixOp(899,900,899)].
  oper(".=") => [infixOp(899,900,899)].
  oper("=>>") => [infixOp(949,950,950)].
  oper("=") => [infixOp(974,975,974)].
  oper("|:") => [infixOp(1234,1235,1234)].
  oper("show") => [prefixOp(1240,1239)].
  oper("++") => [infixOp(719,720,720)].
  oper(">") => [infixOp(899,900,899)].
  oper("return") => [prefixOp(930,929)].
  oper("?") => [infixOp(919,920,920)].
  oper("@") => [prefixOp(400,399), infixOp(399,400,400)].
  oper("in") => [infixOp(899,900,900)].
  oper("^|") => [infixOp(919,920,920)].
  oper("cut") => [infixOp(949,950,949)].
  oper("open") => [prefixOp(900,899)].
  oper("~~") => [infixOp(1239,1240,1240)].
  oper("assert") => [prefixOp(1240,1239)].
  oper("!!") => [postfixOp(99,100)].
  oper("⊕") => [infixOp(720,720,719)].
  oper(".^.") => [infixOp(720,720,719)].
  oper("//") => [infixOp(960,960,959)].
  oper("public") => [prefixOp(1700,1699)].
  oper("ref") => [prefixOp(899,898)].
  oper(".~.") => [prefixOp(650,649)].
  oper("where") => [infixOp(910,911,910)].
  oper("=<") => [infixOp(899,900,899)].
  oper("case") => [prefixOp(901,900)].
  oper("==") => [infixOp(899,900,899)].
  oper("\\") => [infixOp(700,700,699)].
  oper("=>") => [infixOp(949,950,950)].
  oper("^") => [prefixOp(100,99), infixOp(99,100,99)].
  oper("<=>") => [infixOp(949,950,949)].
  oper("perform") => [prefixOp(300,299)].
  oper("valof") => [prefixOp(300,299)].
  oper("until") => [infixOp(1174,1175,1174)].
  oper("while") => [prefixOp(1175,1174)].
  oper("private") => [prefixOp(1200,1199)].
  oper("•") => [infixOp(450,450,449)].
  oper(".&.") => [infixOp(700,700,699)].
  oper("///") => [infixOp(960,960,959)].
  oper("::") => [infixOp(399,400,399)].
  oper("+++") => [infixOp(719,720,720)].
  oper(":=") => [infixOp(974,975,974)].
  oper(":?") => [infixOp(399,400,399)].
  oper(".<<.") => [infixOp(600,600,599)].
  oper("^.") => [infixOp(450,450,449)].
  oper(">>=") => [infixOp(949,950,950)].
  oper("^/") => [infixOp(960,960,959)].
  oper("<~") => [infixOp(1230,1231,1230)].
  oper("type") => [prefixOp(1251,1250)].
  oper("implementation") => [prefixOp(1260,1259)].
  oper("|") => [infixOp(1248,1248,1247)].
  oper(".#.") => [infixOp(600,600,599)].
  oper("~") => [prefixOp(905,904)].
  oper("^//") => [infixOp(800,800,799)].
  oper("||") => [infixOp(919,920,920)].
  oper("else") => [infixOp(1199,1200,1200)].
  oper("::=") => [infixOp(1249,1250,1249)].
  oper("/\\") => [infixOp(700,700,699)].
  oper(">=") => [infixOp(899,900,899)].
  oper(">>") => [infixOp(949,950,950)].
  oper(_) default => [].

  public isBracket:(string) => option[bracket].
  isBracket("[|") => some(bkt("[|","[||]","|]","",2000)).
  isBracket("|]") => some(bkt("[|","[||]","|]","",2000)).
  isBracket("[||]") => some(bkt("[|","[||]","|]","",2000)).
  isBracket("<|") => some(bkt("<|","<||>","|>","",2000)).
  isBracket("|>") => some(bkt("<|","<||>","|>","",2000)).
  isBracket("<||>") => some(bkt("<|","<||>","|>","",2000)).
  isBracket("{.") => some(bkt("{.","{..}",".}","",2000)).
  isBracket(".}") => some(bkt("{.","{..}",".}","",2000)).
  isBracket("{..}") => some(bkt("{.","{..}",".}","",2000)).
  isBracket("[") => some(bkt("[","[]","]",",",2000)).
  isBracket("]") => some(bkt("[","[]","]",",",2000)).
  isBracket("[]") => some(bkt("[","[]","]",",",2000)).
  isBracket("(") => some(bkt("(","()",")",",",2000)).
  isBracket(")") => some(bkt("(","()",")",",",2000)).
  isBracket("()") => some(bkt("(","()",")",",",2000)).
  isBracket("{") => some(bkt("{","{}","}",".\n",2000)).
  isBracket("}") => some(bkt("{","{}","}",".\n",2000)).
  isBracket("{}") => some(bkt("{","{}","}",".\n",2000)).
  isBracket("(|") => some(bkt("(|","(||)","|)","",2000)).
  isBracket("|)") => some(bkt("(|","(||)","|)","",2000)).
  isBracket("(||)") => some(bkt("(|","(||)","|)","",2000)).
  isBracket("{?") => some(bkt("{?","{??}","?}","",2000)).
  isBracket("?}") => some(bkt("{?","{??}","?}","",2000)).
  isBracket("{??}") => some(bkt("{?","{??}","?}","",2000)).
  isBracket("{!") => some(bkt("{!","{!!}","!}","",2000)).
  isBracket("!}") => some(bkt("{!","{!!}","!}","",2000)).
  isBracket("{!!}") => some(bkt("{!","{!!}","!}","",2000)).
  isBracket(_) default => .none.

  public isLeftBracket:(string) => boolean.
  isLeftBracket(S) => bkt(S,_,_,_,_) ^= isBracket(S).

  public isRightBracket:(string) => boolean.
  isRightBracket(S) => bkt(_,_,S,_,_) ^= isBracket(S).

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
  first(0c⊕) => some("⊕").
  first(0c•) => some("•").
  first(0c#) => some("#").
  first(0c$) => some("$").
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
  follows(".<",0c.) => some(".<.").
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
  follows("{",0c?) => some("{?").
  follows("{",0c.) => some("{.").
  follows("{",0c!) => some("{!").
  follows("|",0c]) => some("|]").
  follows("|",0c:) => some("|:").
  follows("|",0c|) => some("||").
  follows("|",0c>) => some("|>").
  follows("|",0c)) => some("|)").
  follows("~",0c~) => some("~~").
  follows("~",0c=) => some("~=").
  follows("~",0c>) => some("~>").
  follows("[",0c|) => some("[|").
  follows("\\",0c+) => some("\\+").
  follows("\\",0c-) => some("\\-").
  follows("\\",0c/) => some("\\/").
  follows("^",0c.) => some("^.").
  follows("^",0c/) => some("^/").
  follows("^",0c=) => some("^=").
  follows("^",0c|) => some("^|").
  follows("^/",0c/) => some("^//").
  follows(":",0c?) => some(":?").
  follows(":",0c:) => some("::").
  follows(":",0c=) => some(":=").
  follows("::",0c=) => some("::=").
  follows("<",0c*) => some("<*").
  follows("<",0c~) => some("<~").
  follows("<",0c$) => some("<$").
  follows("<",0c-) => some("<-").
  follows("<",0c<) => some("<<").
  follows("<",0c|) => some("<|").
  follows("<",0c=) => some("<=").
  follows("<*",0c>) => some("<*>").
  follows("<<",0c-) => some("<<-").
  follows("<=",0c>) => some("<=>").
  follows("=",0c<) => some("=<").
  follows("=",0c=) => some("==").
  follows("=",0c>) => some("=>").
  follows("=>",0c>) => some("=>>").
  follows(">",0c=) => some(">=").
  follows(">",0c>) => some(">>").
  follows(">>",0c=) => some(">>=").
  follows("?",0c}) => some("?}").
  follows("!",0c!) => some("!!").
  follows("!",0c}) => some("!}").
  follows("$",0c$) => some("$$").
  follows(_,_) default => .none.

  public final:(string) => boolean.
  final("%") => .true.  /* modulo */
  final("&&") => .true.  /* conjunction */
  final("(") => .true.  /* parentheses */
  final("(|") => .true.  /* banana brackets */
  final(")") => .true.  /* parentheses */
  final("*") => .true.  /* multicat */
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
  final(".<.") => .true.  /* set membership */
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
  final("{?") => .true.  /* test comprehension */
  final("{.") => .true.  /* non-recursive braces */
  final("{!") => .true.  /* iota comprehension */
  final("|") => .true.  /* type union and abstraction */
  final("|]") => .true.  /* measure brackets */
  final("|:") => .true.  /* constrained type */
  final("||") => .true.  /* disjunction */
  final("|>") => .true.  /* meta quote */
  final("|)") => .true.  /* banana brackets */
  final("}") => .true.  /* braces */
  final("~") => .true.  /* logical negation */
  final("~~") => .true.  /* quantifier */
  final("~=") => .true.  /* not equals */
  final("~>") => .true.  /* type function */
  final("[") => .true.  /* square brackets */
  final("[|") => .true.  /* measure brackets */
  final("\\") => .true.  /* difference */
  final("\\+") => .true.  /* add element to set */
  final("\\-") => .true.  /* remove element from set */
  final("\\/") => .true.  /* union */
  final("]") => .true.  /* square brackets */
  final("^") => .true.  /* Optional propagation */
  final("^.") => .true.  /* optional object access */
  final("^/") => .true.  /* filter */
  final("^//") => .true.  /* filter map */
  final("^=") => .true.  /* optional decomposition match */
  final("^|") => .true.  /* option or-else operator */
  final(":") => .true.  /* type annotation */
  final(":?") => .true.  /* fallable type coercion */
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
  final("<|") => .true.  /* meta quote */
  final("<=>") => .true.  /* constructor arrow */
  final("=") => .true.  /* definition */
  final("=<") => .true.  /* less than or equal */
  final("==") => .true.  /* equality predicate */
  final("=>") => .true.  /* function arrow */
  final("=>>") => .true.  /* continuation arrow */
  final(">") => .true.  /* greater than */
  final(">=") => .true.  /* greater than or equal */
  final(">>") => .true.  /* monadic bind */
  final(">>=") => .true.  /* monadic bind */
  final("?") => .true.  /* conditional operator */
  final("?}") => .true.  /* test comprehension */
  final("@") => .true.  /* meta annotation */
  final("!") => .true.  /* pick up value from a ref cell */
  final("!!") => .true.  /* pick up value from a memo */
  final("!}") => .true.  /* iota comprehension */
  final("⊕") => .true.  /* addition */
  final("•") => .true.  /* function composition */
  final("#") => .true.  /* Macro statement marker */
  final("$$") => .true.  /* wrap value in memo */
  final(_) default => .false.

  public keyword:(string) => boolean.
  keyword("all") => .true.
  keyword("^=") => .true.
  keyword("&&") => .true.
  keyword("~>") => .true.
  keyword("throw") => .true.
  keyword("do") => .true.
  keyword("import") => .true.
  keyword("catch") => .true.
  keyword("prompt") => .true.
  keyword("valis") => .true.
  keyword(",..") => .true.
  keyword("for") => .true.
  keyword("ignore") => .true.
  keyword("then") => .true.
  keyword("!") => .true.
  keyword("->>") => .true.
  keyword("default") => .true.
  keyword("#") => .true.
  keyword("<-") => .true.
  keyword("<<-") => .true.
  keyword("*>") => .true.
  keyword(",") => .true.
  keyword("contract") => .true.
  keyword(".") => .true.
  keyword("val") => .true.
  keyword("try") => .true.
  keyword("exists") => .true.
  keyword("if") => .true.
  keyword(":") => .true.
  keyword(";") => .true.
  keyword(".=") => .true.
  keyword("=>>") => .true.
  keyword("=") => .true.
  keyword("|:") => .true.
  keyword("?") => .true.
  keyword("@") => .true.
  keyword("in") => .true.
  keyword("cut") => .true.
  keyword("open") => .true.
  keyword("~~") => .true.
  keyword("public") => .true.
  keyword("ref") => .true.
  keyword("where") => .true.
  keyword("case") => .true.
  keyword("=>") => .true.
  keyword("^") => .true.
  keyword("<=>") => .true.
  keyword("perform") => .true.
  keyword("valof") => .true.
  keyword("until") => .true.
  keyword("while") => .true.
  keyword("private") => .true.
  keyword("::") => .true.
  keyword(":?") => .true.
  keyword("^.") => .true.
  keyword("<~") => .true.
  keyword("type") => .true.
  keyword("implementation") => .true.
  keyword("|") => .true.
  keyword("~") => .true.
  keyword("||") => .true.
  keyword("else") => .true.
  keyword("::=") => .true.
  keyword(_) default => .false.
}
