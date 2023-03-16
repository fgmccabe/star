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
  pickInfix([.infixOp(Lf,Pr,Rg),.._]) => ?(Lf,Pr,Rg).
  pickInfix([_,..L]) => pickInfix(L).

  public isPrefixOp:(string) => option[(integer,integer)].
  isPrefixOp(Nm) => pickPrefix(oper(Nm)).

  pickPrefix:(cons[operator]) => option[(integer,integer)].
  pickPrefix([]) => .none.
  pickPrefix([.prefixOp(Pr,Rg),.._]) => ?(Pr,Rg).
  pickPrefix([_,..L]) => pickPrefix(L).

  public isPostfixOp:(string) => option[(integer,integer)].
  isPostfixOp(Nm) => pickPostfix(oper(Nm)).

  pickPostfix:(cons[operator]) => option[(integer,integer)].
  pickPostfix([]) => .none.
  pickPostfix([.postfixOp(Pr,Rg),.._]) => ?(Pr,Rg).
  pickPostfix([_,..L]) => pickPrefix(L).

  oper:(string)=>cons[operator].
  oper(Op) => case Op in {
    "all" => [.prefixOp(1010,1009)].
    ".<." => [.infixOp(699,700,699)].
    "^=" => [.infixOp(899,900,899)].
    "&&" => [.infixOp(909,910,910)].
    "^?" => [.infixOp(800,800,799)].
    "let" => [.prefixOp(899,898)].
    "~=" => [.infixOp(899,900,899)].
    "~>" => [.infixOp(1230,1231,1230)].
    ".|." => [.infixOp(720,720,719)].
    "do" => [.prefixOp(200,199), .infixOp(1199,1200,1199)].
    "import" => [.prefixOp(900,899)].
    "catch" => [.infixOp(1198,1199,1198)].
    "valis" => [.prefixOp(930,929)].
    ",.." => [.infixOp(999,1000,999)].
    "for" => [.prefixOp(1175,1174)].
    "••" => [.infixOp(450,450,449)].
    "**" => [.infixOp(600,600,599)].
    "->" => [.infixOp(889,890,889)].
    ".+." => [.prefixOp(700,699)].
    "<$" => [.infixOp(719,720,720)].
    "raise" => [.prefixOp(930,929)].
    "then" => [.infixOp(1179,1180,1179)].
    "!" => [.postfixOp(99,100), .infixOp(99,100,99)].
    "->>" => [.infixOp(1199,1200,1199)].
    "?=" => [.infixOp(899,900,899)].
    "default" => [.postfixOp(939,940)].
    "<*" => [.infixOp(600,600,599)].
    "#" => [.prefixOp(1750,1749), .infixOp(759,760,759)].
    "??" => [.infixOp(919,920,920)].
    "%" => [.infixOp(700,700,699)].
    "<-" => [.infixOp(904,905,904)].
    ".>>>." => [.infixOp(600,600,599)].
    "\\+" => [.infixOp(700,700,699)].
    "*" => [.postfixOp(699,700), .infixOp(700,700,699)].
    "\\-" => [.infixOp(700,700,699)].
    "+" => [.postfixOp(699,700), .infixOp(720,720,719)].
    ".>>." => [.infixOp(600,600,599)].
    "*>" => [.infixOp(904,905,904)].
    "," => [.infixOp(999,1000,1000)].
    "contract" => [.prefixOp(1560,1559)].
    "\\/" => [.infixOp(720,720,719)].
    "-" => [.prefixOp(300,299), .infixOp(720,720,719)].
    "raises" => [.infixOp(999,1000,999)].
    "." => [.prefixOp(10,9), .infixOp(100,100,99)].
    "/" => [.infixOp(700,700,699)].
    "<*>" => [.infixOp(949,950,950)].
    "try" => [.prefixOp(1200,1199)].
    "exists" => [.prefixOp(1010,1009)].
    "if" => [.prefixOp(1175,1174)].
    ":" => [.infixOp(1249,1250,1249)].
    ";" => [.postfixOp(1250,1251), .infixOp(1250,1251,1251)].
    "<" => [.infixOp(899,900,899)].
    ".=" => [.infixOp(899,900,899)].
    "=>>" => [.infixOp(949,950,950)].
    "=" => [.infixOp(974,975,974)].
    "|:" => [.infixOp(1234,1235,1234)].
    "show" => [.prefixOp(1240,1239)].
    "++" => [.infixOp(719,720,720)].
    ">" => [.infixOp(899,900,899)].
    "return" => [.prefixOp(930,929)].
    "?" => [.prefixOp(820,819)].
    "@" => [.prefixOp(400,399), .infixOp(399,400,400)].
    "|=" => [.infixOp(998,999,998)].
    "in" => [.infixOp(899,900,900)].
    "break" => [.prefixOp(10,9)].
    "open" => [.prefixOp(900,899)].
    "trace" => [.prefixOp(140,139)].
    "~~" => [.infixOp(1239,1240,1240)].
    "assert" => [.prefixOp(1240,1239)].
    "invoke" => [.prefixOp(930,929)].
    "⊕" => [.infixOp(720,720,719)].
    ".^." => [.infixOp(720,720,719)].
    "//" => [.infixOp(960,960,959)].
    "public" => [.prefixOp(1700,1699)].
    "ref" => [.prefixOp(899,898)].
    ".~." => [.prefixOp(650,649)].
    "where" => [.infixOp(910,911,910)].
    "=<" => [.infixOp(899,900,899)].
    "case" => [.prefixOp(901,900)].
    "==" => [.infixOp(899,900,899)].
    "\\" => [.infixOp(700,700,699)].
    "=>" => [.infixOp(949,950,950)].
    "^" => [.prefixOp(100,99), .infixOp(99,100,99)].
    "<=>" => [.infixOp(949,950,949)].
    "generator" => [.prefixOp(300,299)].
    "valof" => [.prefixOp(300,299)].
    "yield" => [.prefixOp(300,299)].
    "while" => [.prefixOp(1175,1174)].
    "private" => [.prefixOp(1510,1509)].
    "•" => [.infixOp(450,450,449)].
    ".&." => [.infixOp(700,700,699)].
    "///" => [.infixOp(960,960,959)].
    "::" => [.infixOp(399,400,399)].
    "+++" => [.infixOp(719,720,720)].
    ":=" => [.infixOp(974,975,974)].
    ":?" => [.infixOp(399,400,399)].
    ".<<." => [.infixOp(600,600,599)].
    "implementation" => [.prefixOp(1260,1259)].
    ">>=" => [.infixOp(949,950,950)].
    "^/" => [.infixOp(960,960,959)].
    "<~" => [.infixOp(998,999,998)].
    "type" => [.prefixOp(1251,1250)].
    "|" => [.infixOp(1548,1548,1547)].
    ".#." => [.infixOp(600,600,599)].
    "~" => [.prefixOp(905,904)].
    "^//" => [.infixOp(800,800,799)].
    "||" => [.infixOp(919,920,920)].
    "else" => [.infixOp(1199,1200,1200)].
    "::=" => [.infixOp(1549,1550,1549)].
    "/\\" => [.infixOp(700,700,699)].
    ">=" => [.infixOp(899,900,899)].
    ">>" => [.infixOp(949,950,950)].
    _ default => [].
  }

  public isBracket:(string) => option[bracket].
  isBracket(Str) => case Str in {
    "[|" => ?.bkt("[|","[||]","|]","",2000).
    "|]" => ?.bkt("[|","[||]","|]","",2000).
    "[||]" => ?.bkt("[|","[||]","|]","",2000).
    "<|" => ?.bkt("<|","<||>","|>","",2000).
    "|>" => ?.bkt("<|","<||>","|>","",2000).
    "<||>" => ?.bkt("<|","<||>","|>","",2000).
    "{." => ?.bkt("{.","{..}",".}",".\n",2000).
    ".}" => ?.bkt("{.","{..}",".}",".\n",2000).
    "{..}" => ?.bkt("{.","{..}",".}",".\n",2000).
    "[" => ?.bkt("[","[]","]",",",2000).
    "]" => ?.bkt("[","[]","]",",",2000).
    "[]" => ?.bkt("[","[]","]",",",2000).
    "(" => ?.bkt("(","()",")",",",2000).
    ")" => ?.bkt("(","()",")",",",2000).
    "()" => ?.bkt("(","()",")",",",2000).
    "{" => ?.bkt("{","{}","}",".\n",2000).
    "}" => ?.bkt("{","{}","}",".\n",2000).
    "{}" => ?.bkt("{","{}","}",".\n",2000).
    "{?" => ?.bkt("{?","{??}","?}","",2000).
    "?}" => ?.bkt("{?","{??}","?}","",2000).
    "{??}" => ?.bkt("{?","{??}","?}","",2000).
    "{!" => ?.bkt("{!","{!!}","!}","",2000).
    "!}" => ?.bkt("{!","{!!}","!}","",2000).
    "{!!}" => ?.bkt("{!","{!!}","!}","",2000).
    _ default => .none.
  }

  public isLeftBracket:(string) => boolean.
  isLeftBracket(S) => .bkt(S,_,_,_,_) ?= isBracket(S).

  public isRightBracket:(string) => boolean.
  isRightBracket(S) => .bkt(_,_,S,_,_) ?= isBracket(S).

  public first:(char) => option[string].
  first(Op) => case Op in {
    `%` => ?"%".
    `&` => ?"&".
    `(` => ?"(".
    `)` => ?")".
    `*` => ?"*".
    `+` => ?"+".
    `,` => ?",".
    `-` => ?"-".
    `.` => ?".".
    `/` => ?"/".
    `{` => ?"{".
    `|` => ?"|".
    `}` => ?"}".
    `~` => ?"~".
    `[` => ?"[".
    `\\` => ?"\\".
    `]` => ?"]".
    `^` => ?"^".
    `:` => ?":".
    `;` => ?";".
    `<` => ?"<".
    `=` => ?"=".
    `>` => ?">".
    `?` => ?"?".
    `@` => ?"@".
    `!` => ?"!".
    `⊕` => ?"⊕".
    `•` => ?"•".
    `#` => ?"#".
    _ default => .none.
  }

  public follows:(string,char) => option[string].
  follows(Op,Ch) => case (Op,Ch) in {
    ("&",`&`) => ?"&&".
    ("*",`*`) => ?"**".
    ("*",`>`) => ?"*>".
    ("+",`+`) => ?"++".
    ("++",`+`) => ?"+++".
    (",",`.`) => ?",.".
    (",.",`.`) => ?",..".
    ("-",`>`) => ?"->".
    ("->",`>`) => ?"->>".
    (".",`#`) => ?".#".
    (".",`&`) => ?".&".
    (".",`|`) => ?".|".
    (".",`}`) => ?".}".
    (".",`~`) => ?".~".
    (".",`<`) => ?".<".
    (".",`^`) => ?".^".
    (".",`+`) => ?".+".
    (".",`=`) => ?".=".
    (".",`>`) => ?".>".
    (".",`/`) => ?"./".
    (".",` `) => ?". ".
    (".#",`.`) => ?".#.".
    (".&",`.`) => ?".&.".
    (".|",`.`) => ?".|.".
    (".~",`.`) => ?".~.".
    (".<",`<`) => ?".<<".
    (".<",`.`) => ?".<.".
    (".<<",`.`) => ?".<<.".
    (".^",`.`) => ?".^.".
    (".+",`.`) => ?".+.".
    (".>",`>`) => ?".>>".
    (".>>",`.`) => ?".>>.".
    (".>>",`>`) => ?".>>>".
    (".>>>",`.`) => ?".>>>.".
    ("/",`\\`) => ?"/\\".
    ("/",`.`) => ?"/.".
    ("/",`/`) => ?"//".
    ("//",`/`) => ?"///".
    ("{",`?`) => ?"{?".
    ("{",`.`) => ?"{.".
    ("{",`!`) => ?"{!".
    ("|",`]`) => ?"|]".
    ("|",`:`) => ?"|:".
    ("|",`|`) => ?"||".
    ("|",`=`) => ?"|=".
    ("|",`>`) => ?"|>".
    ("~",`~`) => ?"~~".
    ("~",`=`) => ?"~=".
    ("~",`>`) => ?"~>".
    ("[",`|`) => ?"[|".
    ("\\",`+`) => ?"\\+".
    ("\\",`-`) => ?"\\-".
    ("\\",`/`) => ?"\\/".
    ("^",`?`) => ?"^?".
    ("^",`/`) => ?"^/".
    ("^",`=`) => ?"^=".
    ("^/",`/`) => ?"^//".
    (":",`?`) => ?":?".
    (":",`:`) => ?"::".
    (":",`=`) => ?":=".
    ("::",`=`) => ?"::=".
    ("<",`*`) => ?"<*".
    ("<",`~`) => ?"<~".
    ("<",`$`) => ?"<$".
    ("<",`-`) => ?"<-".
    ("<",`|`) => ?"<|".
    ("<",`=`) => ?"<=".
    ("<*",`>`) => ?"<*>".
    ("<=",`>`) => ?"<=>".
    ("=",`<`) => ?"=<".
    ("=",`=`) => ?"==".
    ("=",`>`) => ?"=>".
    ("=>",`>`) => ?"=>>".
    (">",`=`) => ?">=".
    (">",`>`) => ?">>".
    (">>",`=`) => ?">>=".
    ("?",`?`) => ?"??".
    ("?",`=`) => ?"?=".
    ("?",`}`) => ?"?}".
    ("!",`}`) => ?"!}".
    ("•",`•`) => ?"••".
    (_,_) default => .none.
  }

  public final:(string) => boolean.
  final(Op) => case Op in {
    "%" => .true.  /* modulo */
    "&&" => .true.  /* conjunction */
    "(" => .true.  /* parentheses */
    ")" => .true.  /* parentheses */
    "*" => .true.  /* multicat */
    "**" => .true.  /* exponentiation */
    "*>" => .true.  /* for all */
    "+" => .true.  /* one or more repetitions */
    "++" => .true.  /* concatenate */
    "+++" => .true.  /* choice */
    "," => .true.  /* tupling operator */
    ",.." => .true.  /* list cons */
    "-" => .true.  /* arithmetic negation */
    "->" => .true.  /* map entry */
    "->>" => .true.  /* dependent type marker */
    "." => .true.  /* identify enumerator */
    ".#." => .true.  /* test nth bit */
    ".&." => .true.  /* bitwise and */
    ".|." => .true.  /* bitwise or */
    ".}" => .true.  /* recursive braces */
    ".~." => .true.  /* bitwise 1's complement */
    ".<<." => .true.  /* left shift */
    ".<." => .true.  /* set membership */
    ".^." => .true.  /* bitwise xor */
    ".+." => .true.  /* count of number of bits */
    ".=" => .true.  /* pattern match */
    ".>>." => .true.  /* logical shift right */
    ".>>>." => .true.  /* arithmetic shift right */
    "./" => .true.  /* regexp right */
    ". " => .true.  /* statement terminator */
    "/" => .true.  /* division */
    "/\\" => .true.  /* intersection */
    "/." => .true.  /* regexp left */
    "//" => .true.  /* map over */
    "///" => .true.  /* indexed map over */
    "{" => .true.  /* non-recursive braces */
    "{?" => .true.  /* test comprehension */
    "{." => .true.  /* recursive braces */
    "{!" => .true.  /* iota comprehension */
    "|" => .true.  /* type union and abstraction */
    "|]" => .true.  /* measure brackets */
    "|:" => .true.  /* constrained type */
    "||" => .true.  /* disjunction */
    "|=" => .true.  /* implicit variable */
    "|>" => .true.  /* meta quote */
    "}" => .true.  /* non-recursive braces */
    "~" => .true.  /* logical negation */
    "~~" => .true.  /* quantifier */
    "~=" => .true.  /* not equals */
    "~>" => .true.  /* type function */
    "[" => .true.  /* square brackets */
    "[|" => .true.  /* measure brackets */
    "\\" => .true.  /* difference */
    "\\+" => .true.  /* add element to set */
    "\\-" => .true.  /* remove element from set */
    "\\/" => .true.  /* union */
    "]" => .true.  /* square brackets */
    "^" => .true.  /* Optional propagation */
    "^?" => .true.  /* option propagate */
    "^/" => .true.  /* filter */
    "^//" => .true.  /* filter map */
    "^=" => .true.  /* optional decomposition match */
    ":" => .true.  /* type annotation */
    ":?" => .true.  /* fallable type coercion */
    "::" => .true.  /* type coercion */
    "::=" => .true.  /* algebraic type definition */
    ":=" => .true.  /* assignment */
    ";" => .true.  /* sequencing operator */
    "<" => .true.  /* less than */
    "<*" => .true.  /* left fold */
    "<*>" => .true.  /* applicative splat */
    "<~" => .true.  /* type interface rule */
    "<$" => .true.  /* constant replace */
    "<-" => .true.  /* variable bind */
    "<|" => .true.  /* meta quote */
    "<=>" => .true.  /* constructor arrow */
    "=" => .true.  /* definition */
    "=<" => .true.  /* less than or equal */
    "==" => .true.  /* equality predicate */
    "=>" => .true.  /* function arrow */
    "=>>" => .true.  /* continuation arrow */
    ">" => .true.  /* greater than */
    ">=" => .true.  /* greater than or equal */
    ">>" => .true.  /* monadic bind */
    ">>=" => .true.  /* monadic bind */
    "?" => .true.  /* mark expression as optionally there */
    "??" => .true.  /* conditional operator */
    "?=" => .true.  /* optional decomposition match */
    "?}" => .true.  /* test comprehension */
    "@" => .true.  /* meta annotation */
    "!" => .true.  /* pick up value from a ref cell */
    "!}" => .true.  /* iota comprehension */
    "⊕" => .true.  /* addition */
    "•" => .true.  /* function composition */
    "••" => .true.  /* binary function composition */
    "#" => .true.  /* Macro statement marker */
    _ default => .false.
  }

  public keyword:(string) => boolean.
  keyword(Op) => case Op in {
    "all" => .true.
    "^=" => .true.
    "&&" => .true.
    "let" => .true.
    "~>" => .true.
    "{." => .true.
    "do" => .true.
    "import" => .true.
    "catch" => .true.
    "valis" => .true.
    ",.." => .true.
    "for" => .true.
    "{?" => .true.
    "raise" => .true.
    ". " => .true.
    "then" => .true.
    "!" => .true.
    "->>" => .true.
    "?=" => .true.
    "default" => .true.
    "#" => .true.
    "!}" => .true.
    "??" => .true.
    "<-" => .true.
    "(" => .true.
    ")" => .true.
    "*>" => .true.
    "," => .true.
    "contract" => .true.
    "./" => .true.
    "raises" => .true.
    "." => .true.
    "try" => .true.
    "exists" => .true.
    "if" => .true.
    ":" => .true.
    ";" => .true.
    ".=" => .true.
    "=>>" => .true.
    "=" => .true.
    "|:" => .true.
    "?" => .true.
    "@" => .true.
    "|=" => .true.
    "|>" => .true.
    "in" => .true.
    "break" => .true.
    "open" => .true.
    "~~" => .true.
    "invoke" => .true.
    "/." => .true.
    "public" => .true.
    "[|" => .true.
    "ref" => .true.
    "where" => .true.
    "case" => .true.
    "[" => .true.
    "=>" => .true.
    "]" => .true.
    "^" => .true.
    "<=>" => .true.
    "|]" => .true.
    "generator" => .true.
    "?}" => .true.
    "valof" => .true.
    "yield" => .true.
    "while" => .true.
    "private" => .true.
    "::" => .true.
    ":?" => .true.
    "implementation" => .true.
    "<|" => .true.
    "<~" => .true.
    "{" => .true.
    "type" => .true.
    ".}" => .true.
    "|" => .true.
    "}" => .true.
    "~" => .true.
    "||" => .true.
    "else" => .true.
    "::=" => .true.
    "{!" => .true.
    _ default => .false.
  }
}
