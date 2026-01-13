star.compiler.operators{
  /* Automatically generated, do not edit */

  import star.

  operator ::= .prefixOp(integer,integer)
             | .infixOp(integer,integer,integer)
             | .postfixOp(integer,integer).

  public bracket ::= .bkt(string,string,string,string,integer).

  public isOperator:(string)=>boolean.
  isOperator(Nm) => size(oper(Nm))>0.

  public isInfixOp:(string) => option[(integer,integer,integer)].
  isInfixOp(Nm) => pickInfix(oper(Nm)).

  pickInfix:(cons[operator]) => option[(integer,integer,integer)].
  pickInfix([]) => .none.
  pickInfix([.infixOp(Lf,Pr,Rg),.._]) => .some((Lf,Pr,Rg)).
  pickInfix([_,..L]) => pickInfix(L).

  public isPrefixOp:(string) => option[(integer,integer)].
  isPrefixOp(Nm) => pickPrefix(oper(Nm)).

  pickPrefix:(cons[operator]) => option[(integer,integer)].
  pickPrefix([]) => .none.
  pickPrefix([.prefixOp(Pr,Rg),.._]) => .some((Pr,Rg)).
  pickPrefix([_,..L]) => pickPrefix(L).

  public isPostfixOp:(string) => option[(integer,integer)].
  isPostfixOp(Nm) => pickPostfix(oper(Nm)).

  pickPostfix:(cons[operator]) => option[(integer,integer)].
  pickPostfix([]) => .none.
  pickPostfix([.postfixOp(Pr,Rg),.._]) => .some((Pr,Rg)).
  pickPostfix([_,..L]) => pickPrefix(L).

  oper:(string)=>cons[operator].
  oper(Op) => case Op in {
    | "!" => [.postfixOp(99,100), .infixOp(99,100,99)]
    | "!!" => [.postfixOp(99,100)]
    | "#" => [.infixOp(759,760,759)]
    | "$$" => [.prefixOp(305,304)]
    | "%" => [.infixOp(700,700,699)]
    | "&&" => [.infixOp(909,910,910)]
    | "*" => [.postfixOp(699,700), .infixOp(700,700,699)]
    | "**" => [.infixOp(600,600,599)]
    | "*>" => [.infixOp(904,905,904), .prefixOp(905,904)]
    | "+" => [.postfixOp(699,700), .infixOp(720,720,719)]
    | "++" => [.infixOp(719,720,720)]
    | "+++" => [.infixOp(719,720,720)]
    | "," => [.infixOp(999,1000,1000)]
    | ",.." => [.infixOp(999,1000,999)]
    | "-" => [.prefixOp(300,299), .infixOp(720,720,719)]
    | "-->" => [.infixOp(1248,1249,1248)]
    | "->" => [.infixOp(889,890,889)]
    | "->>" => [.infixOp(1199,1200,1199)]
    | "." => [.prefixOp(10,9), .infixOp(100,100,99)]
    | ".#." => [.infixOp(600,600,599)]
    | ".&." => [.infixOp(700,700,699)]
    | ".+." => [.prefixOp(700,699)]
    | "..<" => [.infixOp(749,750,749)]
    | "..>" => [.infixOp(749,750,749)]
    | ".<." => [.infixOp(699,700,699)]
    | ".<<." => [.infixOp(600,600,599)]
    | ".=" => [.infixOp(899,900,899)]
    | ".>>." => [.infixOp(600,600,599)]
    | ".>>>." => [.infixOp(600,600,599)]
    | ".^." => [.infixOp(720,720,719)]
    | ".|." => [.infixOp(720,720,719)]
    | ".~." => [.prefixOp(650,649)]
    | "/" => [.infixOp(700,700,699)]
    | "//" => [.infixOp(960,960,959)]
    | "///" => [.infixOp(960,960,959)]
    | "/\\" => [.infixOp(700,700,699)]
    | ":" => [.infixOp(1249,1250,1249)]
    | "::" => [.infixOp(399,400,399)]
    | "::=" => [.infixOp(1549,1550,1549)]
    | ":=" => [.infixOp(974,975,974)]
    | ":?" => [.infixOp(399,400,399)]
    | ";" => [.postfixOp(1250,1251), .infixOp(1250,1251,1251)]
    | "<" => [.infixOp(899,900,899)]
    | "<*" => [.infixOp(600,600,599)]
    | "<-" => [.infixOp(974,975,974)]
    | "<=>" => [.infixOp(949,950,949)]
    | "<~" => [.infixOp(998,999,998)]
    | "=" => [.infixOp(974,975,974)]
    | "=<" => [.infixOp(899,900,899)]
    | "==" => [.infixOp(899,900,899)]
    | "=>" => [.infixOp(949,950,950)]
    | "=>>" => [.infixOp(949,950,950)]
    | ">" => [.infixOp(899,900,899)]
    | ">=" => [.infixOp(899,900,899)]
    | ">>" => [.infixOp(949,950,950)]
    | ">>=" => [.infixOp(949,950,950)]
    | "?" => [.infixOp(299,300,299), .prefixOp(300,299)]
    | "?=" => [.infixOp(899,900,899)]
    | "??" => [.infixOp(919,920,920), .prefixOp(950,949)]
    | "?|" => [.infixOp(960,960,959)]
    | "@" => [.prefixOp(400,399), .infixOp(399,400,400)]
    | "\\" => [.infixOp(700,700,699)]
    | "\\+" => [.infixOp(700,700,699)]
    | "\\-" => [.infixOp(700,700,699)]
    | "\\/" => [.infixOp(720,720,719)]
    | "^/" => [.infixOp(960,960,959)]
    | "^//" => [.infixOp(800,800,799)]
    | "all" => [.prefixOp(1010,1009)]
    | "assert" => [.prefixOp(1240,1239)]
    | "async" => [.prefixOp(1234,1233)]
    | "break" => [.prefixOp(10,9)]
    | "case" => [.prefixOp(901,900)]
    | "catch" => [.infixOp(1198,1199,1198)]
    | "collect" => [.prefixOp(300,299)]
    | "contract" => [.prefixOp(1560,1559)]
    | "default" => [.postfixOp(939,940)]
    | "do" => [.prefixOp(200,199), .infixOp(1199,1200,1199)]
    | "elemis" => [.prefixOp(930,929)]
    | "else" => [.infixOp(1199,1200,1200)]
    | "exists" => [.prefixOp(1010,1009)]
    | "for" => [.prefixOp(1175,1174)]
    | "if" => [.prefixOp(1175,1174)]
    | "implementation" => [.prefixOp(1260,1259)]
    | "import" => [.prefixOp(900,899)]
    | "in" => [.infixOp(899,900,900)]
    | "let" => [.prefixOp(899,898)]
    | "private" => [.prefixOp(1700,1699)]
    | "public" => [.prefixOp(1700,1699)]
    | "ref" => [.prefixOp(899,898)]
    | "resume" => [.infixOp(898,899,898)]
    | "retire" => [.prefixOp(899,898), .infixOp(898,899,898)]
    | "return" => [.prefixOp(930,929)]
    | "show" => [.prefixOp(1240,1239)]
    | "suspend" => [.prefixOp(899,898), .infixOp(898,899,898)]
    | "then" => [.infixOp(1179,1180,1179)]
    | "throw" => [.prefixOp(230,229)]
    | "throws" => [.infixOp(949,950,949)]
    | "trace" => [.infixOp(139,140,139), .prefixOp(140,139)]
    | "try" => [.prefixOp(1200,1199)]
    | "valis" => [.prefixOp(930,929)]
    | "valof" => [.prefixOp(300,299)]
    | "where" => [.infixOp(910,911,910)]
    | "while" => [.prefixOp(1175,1174)]
    | "yield" => [.prefixOp(300,299)]
    | "|" => [.prefixOp(1548,1547), .infixOp(1548,1548,1547)]
    | "|=" => [.infixOp(1234,1235,1234)]
    | "||" => [.infixOp(919,920,920)]
    | "~" => [.prefixOp(905,904)]
    | "~=" => [.infixOp(899,900,899)]
    | "~>" => [.infixOp(1230,1231,1230)]
    | "~~" => [.infixOp(1239,1240,1240)]
    | "ζ" => [.prefixOp(1,0)]
    | "•" => [.infixOp(450,450,449)]
    | "••" => [.infixOp(450,450,449)]
    | "⊕" => [.infixOp(720,720,719)]
    | _ default => []
  }

  public isBracket:(string) => option[bracket].
  isBracket(Str) => case Str in {
    | "[|" => .some(.bkt("[|","[||]","|]","",2000))
    | "|]" => .some(.bkt("[|","[||]","|]","",2000))
    | "[||]" => .some(.bkt("[|","[||]","|]","",2000))
    | "<|" => .some(.bkt("<|","<||>","|>","",2000))
    | "|>" => .some(.bkt("<|","<||>","|>","",2000))
    | "<||>" => .some(.bkt("<|","<||>","|>","",2000))
    | "/." => .some(.bkt("/.","/../","./","",2000))
    | "./" => .some(.bkt("/.","/../","./","",2000))
    | "/../" => .some(.bkt("/.","/../","./","",2000))
    | "{." => .some(.bkt("{.","{..}",".}",".\n",2000))
    | ".}" => .some(.bkt("{.","{..}",".}",".\n",2000))
    | "{..}" => .some(.bkt("{.","{..}",".}",".\n",2000))
    | "[" => .some(.bkt("[","[]","]",",",2000))
    | "]" => .some(.bkt("[","[]","]",",",2000))
    | "[]" => .some(.bkt("[","[]","]",",",2000))
    | "(" => .some(.bkt("(","()",")",",",2000))
    | ")" => .some(.bkt("(","()",")",",",2000))
    | "()" => .some(.bkt("(","()",")",",",2000))
    | "{" => .some(.bkt("{","{}","}",".\n",2000))
    | "}" => .some(.bkt("{","{}","}",".\n",2000))
    | "{}" => .some(.bkt("{","{}","}",".\n",2000))
    | "{?" => .some(.bkt("{?","{??}","?}","",2000))
    | "?}" => .some(.bkt("{?","{??}","?}","",2000))
    | "{??}" => .some(.bkt("{?","{??}","?}","",2000))
    | "{!" => .some(.bkt("{!","{!!}","!}","",2000))
    | "!}" => .some(.bkt("{!","{!!}","!}","",2000))
    | "{!!}" => .some(.bkt("{!","{!!}","!}","",2000))
    | _ default => .none
  }

  public isLeftBracket:(string) => boolean.
  isLeftBracket(S) => .bkt(S,_,_,_,_) ?= isBracket(S).

  public isRightBracket:(string) => boolean.
  isRightBracket(S) => .bkt(_,_,S,_,_) ?= isBracket(S).

  public first:(char) => option[string].
  first(Op) => case Op in {
    | `⊕` => .some("⊕")
    | `ζ` => .some("ζ")
    | `!` => .some("!")
    | `•` => .some("•")
    | `#` => .some("#")
    | `$` => .some("$")
    | `%` => .some("%")
    | `&` => .some("&")
    | `(` => .some("(")
    | `)` => .some(")")
    | `*` => .some("*")
    | `+` => .some("+")
    | `,` => .some(",")
    | `-` => .some("-")
    | `.` => .some(".")
    | `/` => .some("/")
    | `:` => .some(":")
    | `;` => .some(";")
    | `<` => .some("<")
    | `=` => .some("=")
    | `>` => .some(">")
    | `?` => .some("?")
    | `@` => .some("@")
    | `[` => .some("[")
    | `\\` => .some("\\")
    | `]` => .some("]")
    | `^` => .some("^")
    | `{` => .some("{")
    | `|` => .some("|")
    | `}` => .some("}")
    | `~` => .some("~")
    | _ default => .none
  }

  public follows:(string,char) => option[string].
  follows("!",`!`) => .some("!!").
  follows("!",`}`) => .some("!}").
  follows("•",`•`) => .some("••").
  follows("$",`$`) => .some("$$").
  follows("&",`&`) => .some("&&").
  follows("*",`*`) => .some("**").
  follows("*",`>`) => .some("*>").
  follows("+",`+`) => .some("++").
  follows("++",`+`) => .some("+++").
  follows(",",`.`) => .some(",.").
  follows(",.",`.`) => .some(",..").
  follows("-",`-`) => .some("--").
  follows("-",`>`) => .some("->").
  follows("--",`>`) => .some("-->").
  follows("->",`>`) => .some("->>").
  follows(".",` `) => .some(". ").
  follows(".",`#`) => .some(".#").
  follows(".",`&`) => .some(".&").
  follows(".",`+`) => .some(".+").
  follows(".",`.`) => .some("..").
  follows(".",`/`) => .some("./").
  follows(".",`<`) => .some(".<").
  follows(".",`=`) => .some(".=").
  follows(".",`>`) => .some(".>").
  follows(".",`^`) => .some(".^").
  follows(".",`|`) => .some(".|").
  follows(".",`}`) => .some(".}").
  follows(".",`~`) => .some(".~").
  follows(".#",`.`) => .some(".#.").
  follows(".&",`.`) => .some(".&.").
  follows(".+",`.`) => .some(".+.").
  follows("..",`<`) => .some("..<").
  follows("..",`>`) => .some("..>").
  follows(".<",`.`) => .some(".<.").
  follows(".<",`<`) => .some(".<<").
  follows(".<<",`.`) => .some(".<<.").
  follows(".>",`>`) => .some(".>>").
  follows(".>>",`.`) => .some(".>>.").
  follows(".>>",`>`) => .some(".>>>").
  follows(".>>>",`.`) => .some(".>>>.").
  follows(".^",`.`) => .some(".^.").
  follows(".|",`.`) => .some(".|.").
  follows(".~",`.`) => .some(".~.").
  follows("/",`.`) => .some("/.").
  follows("/",`/`) => .some("//").
  follows("/",`\\`) => .some("/\\").
  follows("//",`/`) => .some("///").
  follows(":",`:`) => .some("::").
  follows(":",`=`) => .some(":=").
  follows(":",`?`) => .some(":?").
  follows("::",`=`) => .some("::=").
  follows("<",`*`) => .some("<*").
  follows("<",`-`) => .some("<-").
  follows("<",`=`) => .some("<=").
  follows("<",`|`) => .some("<|").
  follows("<",`~`) => .some("<~").
  follows("<=",`>`) => .some("<=>").
  follows("=",`<`) => .some("=<").
  follows("=",`=`) => .some("==").
  follows("=",`>`) => .some("=>").
  follows("=>",`>`) => .some("=>>").
  follows(">",`=`) => .some(">=").
  follows(">",`>`) => .some(">>").
  follows(">>",`=`) => .some(">>=").
  follows("?",`=`) => .some("?=").
  follows("?",`?`) => .some("??").
  follows("?",`|`) => .some("?|").
  follows("?",`}`) => .some("?}").
  follows("[",`|`) => .some("[|").
  follows("\\",`+`) => .some("\\+").
  follows("\\",`-`) => .some("\\-").
  follows("\\",`/`) => .some("\\/").
  follows("^",`/`) => .some("^/").
  follows("^/",`/`) => .some("^//").
  follows("{",`!`) => .some("{!").
  follows("{",`.`) => .some("{.").
  follows("{",`?`) => .some("{?").
  follows("|",`=`) => .some("|=").
  follows("|",`>`) => .some("|>").
  follows("|",`]`) => .some("|]").
  follows("|",`|`) => .some("||").
  follows("~",`=`) => .some("~=").
  follows("~",`>`) => .some("~>").
  follows("~",`~`) => .some("~~").
  follows(_,_) default => .none.

  public final:(string) => boolean.
  final(Op) => case Op in {
    | "⊕" => .true  /* addition */
    | "ζ" => .true  /* interpret a symbol without dereferencing constraints */
    | "!" => .true  /* pick up value from a ref cell */
    | "!!" => .true  /* pick up value from a thunk */
    | "!}" => .true  /* iota comprehension */
    | "•" => .true  /* function composition */
    | "••" => .true  /* binary function composition */
    | "#" => .true  /* package separator */
    | "$$" => .true  /* thunk expression */
    | "%" => .true  /* modulo */
    | "&&" => .true  /* conjunction */
    | "(" => .true  /* parentheses */
    | ")" => .true  /* parentheses */
    | "*" => .true  /* multicat */
    | "**" => .true  /* exponentiation */
    | "*>" => .true  /* for all */
    | "+" => .true  /* one or more repetitions */
    | "++" => .true  /* concatenate */
    | "+++" => .true  /* choice */
    | "," => .true  /* tupling operator */
    | ",.." => .true  /* list cons */
    | "-" => .true  /* arithmetic negation */
    | "-->" => .true  /* grammar rule arrow */
    | "->" => .true  /* map entry */
    | "->>" => .true  /* dependent type marker */
    | "." => .true  /* identify enumerator */
    | ". " => .true  /* statement terminator */
    | ".#." => .true  /* test nth bit */
    | ".&." => .true  /* bitwise and */
    | ".+." => .true  /* count of number of bits */
    | "..<" => .true  /* range increasing iterator expression */
    | "..>" => .true  /* range decreasing iterator expression */
    | "./" => .true  /* dfa expression */
    | ".<." => .true  /* set membership */
    | ".<<." => .true  /* left shift */
    | ".=" => .true  /* pattern match */
    | ".>>." => .true  /* logical shift right */
    | ".>>>." => .true  /* arithmetic shift right */
    | ".^." => .true  /* bitwise xor */
    | ".|." => .true  /* bitwise or */
    | ".}" => .true  /* recursive braces */
    | ".~." => .true  /* bitwise 1's complement */
    | "/" => .true  /* division */
    | "/." => .true  /* dfa expression */
    | "//" => .true  /* map over */
    | "///" => .true  /* indexed map over */
    | "/\\" => .true  /* intersection */
    | ":" => .true  /* type declaration/annotation */
    | "::" => .true  /* type coercion */
    | "::=" => .true  /* algebraic type definition */
    | ":=" => .true  /* assignment */
    | ":?" => .true  /* fallable type coercion */
    | ";" => .true  /* sequencing operator */
    | "<" => .true  /* less than */
    | "<*" => .true  /* left fold */
    | "<-" => .true  /* monadic valof */
    | "<=>" => .true  /* constructor arrow */
    | "<|" => .true  /* meta quote */
    | "<~" => .true  /* type interface rule */
    | "=" => .true  /* definition */
    | "=<" => .true  /* less than or equal */
    | "==" => .true  /* equality predicate */
    | "=>" => .true  /* function arrow */
    | "=>>" => .true  /* continuation arrow */
    | ">" => .true  /* greater than */
    | ">=" => .true  /* greater than or equal */
    | ">>" => .true  /* grammar produce value */
    | ">>=" => .true  /* monadic bind */
    | "?" => .true  /* option match */
    | "?=" => .true  /* optional decomposition match */
    | "??" => .true  /* conditional operator */
    | "?|" => .true  /* optional conditional */
    | "?}" => .true  /* test comprehension */
    | "@" => .true  /* meta annotation */
    | "[" => .true  /* square brackets */
    | "[|" => .true  /* measure brackets */
    | "\\" => .true  /* difference */
    | "\\+" => .true  /* add element to set */
    | "\\-" => .true  /* remove element from set */
    | "\\/" => .true  /* union */
    | "]" => .true  /* square brackets */
    | "^/" => .true  /* filter */
    | "^//" => .true  /* filter map */
    | "{" => .true  /* non-recursive braces */
    | "{!" => .true  /* iota comprehension */
    | "{." => .true  /* recursive braces */
    | "{?" => .true  /* test comprehension */
    | "|" => .true  /* type union, case union */
    | "|=" => .true  /* constrained type */
    | "|>" => .true  /* meta quote */
    | "|]" => .true  /* measure brackets */
    | "||" => .true  /* disjunction */
    | "}" => .true  /* non-recursive braces */
    | "~" => .true  /* logical negation */
    | "~=" => .true  /* not equals */
    | "~>" => .true  /* type function */
    | "~~" => .true  /* quantifier */
    | _ default => .false
  }

  public keyword:(string) => boolean.
  keyword(Op) => case Op in {
    | "!" => .true
    | "!!" => .true
    | "!}" => .true
    | "#" => .true
    | "$$" => .true
    | "&&" => .true
    | "(" => .true
    | ")" => .true
    | "*>" => .true
    | "," => .true
    | ",.." => .true
    | "-->" => .true
    | "->>" => .true
    | "." => .true
    | ". " => .true
    | "..<" => .true
    | "..>" => .true
    | "./" => .true
    | ".=" => .true
    | ".}" => .true
    | "/." => .true
    | ":" => .true
    | "::" => .true
    | "::=" => .true
    | ":?" => .true
    | ";" => .true
    | "<-" => .true
    | "<=>" => .true
    | "<|" => .true
    | "<~" => .true
    | "=" => .true
    | "=>" => .true
    | "=>>" => .true
    | ">>" => .true
    | "?=" => .true
    | "??" => .true
    | "?}" => .true
    | "@" => .true
    | "[" => .true
    | "[|" => .true
    | "]" => .true
    | "all" => .true
    | "async" => .true
    | "break" => .true
    | "case" => .true
    | "catch" => .true
    | "collect" => .true
    | "contract" => .true
    | "default" => .true
    | "do" => .true
    | "elemis" => .true
    | "else" => .true
    | "exists" => .true
    | "for" => .true
    | "generator" => .true
    | "if" => .true
    | "implementation" => .true
    | "import" => .true
    | "in" => .true
    | "let" => .true
    | "private" => .true
    | "public" => .true
    | "ref" => .true
    | "resume" => .true
    | "retire" => .true
    | "suspend" => .true
    | "then" => .true
    | "throw" => .true
    | "throws" => .true
    | "try" => .true
    | "unreachable" => .true
    | "valis" => .true
    | "valof" => .true
    | "void" => .true
    | "where" => .true
    | "while" => .true
    | "yield" => .true
    | "{" => .true
    | "{!" => .true
    | "{." => .true
    | "{?" => .true
    | "|" => .true
    | "|=" => .true
    | "|>" => .true
    | "|]" => .true
    | "||" => .true
    | "}" => .true
    | "~" => .true
    | "~>" => .true
    | "~~" => .true
    | "ζ" => .true
    | _ default => .false
  }
}
