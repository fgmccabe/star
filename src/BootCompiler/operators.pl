/* Automatically generated, do not edit */

:-module(operators,[infixOp/4,prefixOp/3,postfixOp/3,isOperator/1,follows/3,final/2,bracket/5,
                    keyword/1,isKeyword/1]).

  isOperator(O) :-
    operator(O,_).

  infixOp(Op,L,P,R) :-
    operator(Op,Ops),
    is_in(infixOp(L,P,R),Ops),!.

  prefixOp(Op,P,R) :-
    operator(Op,Ops),
    is_in(prefixOp(P,R),Ops),!.

  postfixOp(Op,L,P) :-
    operator(Op,Ops),
    is_in(postfixOp(L,P),Ops),!.

  is_in(X,[X|_]).
  is_in(X,[_|Y]) :- is_in(X,Y).

  operator("retire", [prefixOp(899, 898), infixOp(898, 899, 898)]).
  operator("all", [prefixOp(1010, 1009)]).
  operator(".<.", [infixOp(699, 700, 699)]).
  operator("&&", [infixOp(909, 910, 910)]).
  operator("let", [prefixOp(899, 898)]).
  operator("~=", [infixOp(899, 900, 899)]).
  operator("~>", [infixOp(1230, 1231, 1230)]).
  operator(".|.", [infixOp(720, 720, 719)]).
  operator("do", [prefixOp(200, 199), infixOp(1199, 1200, 1199)]).
  operator("import", [prefixOp(900, 899)]).
  operator("catch", [infixOp(1198, 1199, 1198)]).
  operator("valis", [prefixOp(930, 929)]).
  operator(",..", [infixOp(999, 1000, 999)]).
  operator("for", [prefixOp(1175, 1174)]).
  operator("••", [infixOp(450, 450, 449)]).
  operator("..<", [infixOp(749, 750, 749)]).
  operator("**", [infixOp(600, 600, 599)]).
  operator("..>", [infixOp(749, 750, 749)]).
  operator("->", [infixOp(889, 890, 889)]).
  operator(".+.", [prefixOp(700, 699)]).
  operator("raise", [prefixOp(930, 929)]).
  operator("async", [prefixOp(1234, 1233)]).
  operator("then", [infixOp(1179, 1180, 1179)]).
  operator("ζ", [prefixOp(1, 0)]).
  operator("!", [postfixOp(99, 100), infixOp(99, 100, 99)]).
  operator("->>", [infixOp(1199, 1200, 1199)]).
  operator("?=", [infixOp(899, 900, 899)]).
  operator("default", [postfixOp(939, 940)]).
  operator("<*", [infixOp(600, 600, 599)]).
  operator("#", [prefixOp(1750, 1749), infixOp(759, 760, 759)]).
  operator("??", [infixOp(919, 920, 920)]).
  operator("%", [infixOp(700, 700, 699)]).
  operator(".>>>.", [infixOp(600, 600, 599)]).
  operator("\\+", [infixOp(700, 700, 699)]).
  operator("*", [postfixOp(699, 700), infixOp(700, 700, 699)]).
  operator("\\-", [infixOp(700, 700, 699)]).
  operator("+", [postfixOp(699, 700), infixOp(720, 720, 719)]).
  operator(".>>.", [infixOp(600, 600, 599)]).
  operator("*>", [infixOp(904, 905, 904), prefixOp(905, 904)]).
  operator("resume", [infixOp(898, 899, 898)]).
  operator(",", [infixOp(999, 1000, 1000)]).
  operator("contract", [prefixOp(1560, 1559)]).
  operator("\\/", [infixOp(720, 720, 719)]).
  operator("-", [prefixOp(300, 299), infixOp(720, 720, 719)]).
  operator(".", [prefixOp(10, 9), infixOp(100, 100, 99)]).
  operator("raises", [infixOp(950, 951, 951), prefixOp(999, 998)]).
  operator("/", [infixOp(700, 700, 699)]).
  operator("try", [prefixOp(1200, 1199)]).
  operator("exists", [prefixOp(1010, 1009)]).
  operator("if", [prefixOp(1175, 1174)]).
  operator("$$", [prefixOp(305, 304)]).
  operator(":", [infixOp(1249, 1250, 1249)]).
  operator(";", [postfixOp(1250, 1251), infixOp(1250, 1251, 1251)]).
  operator("-->", [infixOp(1248, 1249, 1248)]).
  operator("<", [infixOp(899, 900, 899)]).
  operator(".=", [infixOp(899, 900, 899)]).
  operator("=>>", [infixOp(949, 950, 950)]).
  operator("=", [infixOp(974, 975, 974)]).
  operator("|:", [infixOp(1234, 1235, 1234)]).
  operator("show", [prefixOp(1240, 1239)]).
  operator("++", [infixOp(719, 720, 720)]).
  operator(">", [infixOp(899, 900, 899)]).
  operator("return", [prefixOp(930, 929)]).
  operator("@", [prefixOp(400, 399), infixOp(399, 400, 400)]).
  operator("|=", [infixOp(998, 999, 998)]).
  operator("in", [infixOp(899, 900, 900)]).
  operator("break", [prefixOp(10, 9)]).
  operator("suspend", [prefixOp(899, 898), infixOp(898, 899, 898)]).
  operator("trace", [infixOp(139, 140, 139), prefixOp(140, 139)]).
  operator("~~", [infixOp(1239, 1240, 1240)]).
  operator("assert", [prefixOp(1240, 1239)]).
  operator("!!", [postfixOp(99, 100)]).
  operator("⊕", [infixOp(720, 720, 719)]).
  operator(".^.", [infixOp(720, 720, 719)]).
  operator("//", [infixOp(960, 960, 959)]).
  operator("public", [prefixOp(1700, 1699)]).
  operator("ref", [prefixOp(899, 898)]).
  operator(".~.", [prefixOp(650, 649)]).
  operator("where", [infixOp(910, 911, 910)]).
  operator("=<", [infixOp(899, 900, 899)]).
  operator("case", [prefixOp(901, 900)]).
  operator("==", [infixOp(899, 900, 899)]).
  operator("\\", [infixOp(700, 700, 699)]).
  operator("=>", [infixOp(949, 950, 950)]).
  operator("<=>", [infixOp(949, 950, 949)]).
  operator("valof", [prefixOp(300, 299)]).
  operator("yield", [prefixOp(300, 299)]).
  operator("while", [prefixOp(1175, 1174)]).
  operator("private", [prefixOp(1700, 1699)]).
  operator("•", [infixOp(450, 450, 449)]).
  operator(".&.", [infixOp(700, 700, 699)]).
  operator("///", [infixOp(960, 960, 959)]).
  operator("::", [infixOp(399, 400, 399)]).
  operator("+++", [infixOp(719, 720, 720)]).
  operator(":=", [infixOp(974, 975, 974)]).
  operator(":?", [infixOp(399, 400, 399)]).
  operator(".<<.", [infixOp(600, 600, 599)]).
  operator("implementation", [prefixOp(1260, 1259)]).
  operator(">>=", [infixOp(949, 950, 950)]).
  operator("^/", [infixOp(960, 960, 959)]).
  operator("<~", [infixOp(998, 999, 998)]).
  operator("type", [prefixOp(1251, 1250)]).
  operator("|", [prefixOp(1548, 1547), infixOp(1548, 1548, 1547)]).
  operator(".#.", [infixOp(600, 600, 599)]).
  operator("~", [prefixOp(905, 904)]).
  operator("^//", [infixOp(800, 800, 799)]).
  operator("||", [infixOp(919, 920, 920)]).
  operator("else", [infixOp(1199, 1200, 1200)]).
  operator("::=", [infixOp(1549, 1550, 1549)]).
  operator("/\\", [infixOp(700, 700, 699)]).
  operator(">=", [infixOp(899, 900, 899)]).
  operator(">>", [infixOp(949, 950, 950)]).

  bracket("[||]", "[|", "|]", "", 2000).
  bracket("<||>", "<|", "|>", "", 2000).
  bracket("/../", "/.", "./", "", 2000).
  bracket("{..}", "{.", ".}", ".\n", 2000).
  bracket("[]", "[", "]", ",", 2000).
  bracket("()", "(", ")", ",", 2000).
  bracket("{}", "{", "}", ".\n", 2000).
  bracket("{??}", "{?", "?}", "", 2000).
  bracket("{!!}", "{!", "!}", "", 2000).

  follows('','ζ','ζ').
  follows('','%','%').
  follows('','&','&').
  follows('','(','(').
  follows('',')',')').
  follows('','*','*').
  follows('','+','+').
  follows('',',',',').
  follows('','-','-').
  follows('','.','.').
  follows('','/','/').
  follows('','{','{').
  follows('','|','|').
  follows('','}','}').
  follows('','~','~').
  follows('','[','[').
  follows('','\\','\\').
  follows('',']',']').
  follows('','^','^').
  follows('',':',':').
  follows('',';',';').
  follows('','<','<').
  follows('','=','=').
  follows('','>','>').
  follows('','?','?').
  follows('','@','@').
  follows('','!','!').
  follows('','⊕','⊕').
  follows('','•','•').
  follows('','#','#').
  follows('','$','$').
  follows('&','&','&&').
  follows('*','*','**').
  follows('*','>','*>').
  follows('+','+','++').
  follows('++','+','+++').
  follows(',','.',',.').
  follows(',.','.',',..').
  follows('-','-','--').
  follows('-','>','->').
  follows('--','>','-->').
  follows('->','>','->>').
  follows('.','#','.#').
  follows('.','&','.&').
  follows('.','|','.|').
  follows('.','}','.}').
  follows('.','~','.~').
  follows('.','<','.<').
  follows('.','^','.^').
  follows('.','+','.+').
  follows('.','=','.=').
  follows('.','>','.>').
  follows('.','.','..').
  follows('.','/','./').
  follows('.',' ','. ').
  follows('.#','.','.#.').
  follows('.&','.','.&.').
  follows('.|','.','.|.').
  follows('.~','.','.~.').
  follows('.<','<','.<<').
  follows('.<','.','.<.').
  follows('.<<','.','.<<.').
  follows('.^','.','.^.').
  follows('.+','.','.+.').
  follows('.>','>','.>>').
  follows('.>>','.','.>>.').
  follows('.>>','>','.>>>').
  follows('.>>>','.','.>>>.').
  follows('..','<','..<').
  follows('..','>','..>').
  follows('/','\\','/\\').
  follows('/','.','/.').
  follows('/','/','//').
  follows('//','/','///').
  follows('{','?','{?').
  follows('{','.','{.').
  follows('{','!','{!').
  follows('|',']','|]').
  follows('|',':','|:').
  follows('|','|','||').
  follows('|','=','|=').
  follows('|','>','|>').
  follows('~','~','~~').
  follows('~','=','~=').
  follows('~','>','~>').
  follows('[','|','[|').
  follows('\\','+','\\+').
  follows('\\','-','\\-').
  follows('\\','/','\\/').
  follows('^','/','^/').
  follows('^/','/','^//').
  follows(':','?',':?').
  follows(':',':','::').
  follows(':','=',':=').
  follows('::','=','::=').
  follows('<','*','<*').
  follows('<','~','<~').
  follows('<','|','<|').
  follows('<','=','<=').
  follows('<=','>','<=>').
  follows('=','<','=<').
  follows('=','=','==').
  follows('=','>','=>').
  follows('=>','>','=>>').
  follows('>','=','>=').
  follows('>','>','>>').
  follows('>>','=','>>=').
  follows('?','?','??').
  follows('?','=','?=').
  follows('?','}','?}').
  follows('!','!','!!').
  follows('!','}','!}').
  follows('•','•','••').
  follows('$','$','$$').

  final('ζ',"ζ").	 /* interpret a symbol without dereferencing constraints */
  final('%',"%").	 /* modulo */
  final('&&',"&&").	 /* conjunction */
  final('(',"(").	 /* parentheses */
  final(')',")").	 /* parentheses */
  final('*',"*").	 /* multicat */
  final('**',"**").	 /* exponentiation */
  final('*>',"*>").	 /* for all */
  final('+',"+").	 /* one or more repetitions */
  final('++',"++").	 /* concatenate */
  final('+++',"+++").	 /* choice */
  final(',',",").	 /* tupling operator */
  final(',..',",..").	 /* list cons */
  final('-',"-").	 /* arithmetic negation */
  final('-->',"-->").	 /* grammar rule arrow */
  final('->',"->").	 /* map entry */
  final('->>',"->>").	 /* dependent type marker */
  final('.',".").	 /* identify enumerator */
  final('.#.',".#.").	 /* test nth bit */
  final('.&.',".&.").	 /* bitwise and */
  final('.|.',".|.").	 /* bitwise or */
  final('.}',".}").	 /* recursive braces */
  final('.~.',".~.").	 /* bitwise 1's complement */
  final('.<<.',".<<.").	 /* left shift */
  final('.<.',".<.").	 /* set membership */
  final('.^.',".^.").	 /* bitwise xor */
  final('.+.',".+.").	 /* count of number of bits */
  final('.=',".=").	 /* pattern match */
  final('.>>.',".>>.").	 /* logical shift right */
  final('.>>>.',".>>>.").	 /* arithmetic shift right */
  final('..<',"..<").	 /* range increasing iterator expression */
  final('..>',"..>").	 /* range decreasing iterator expression */
  final('./',"./").	 /* dfa expression */
  final('. ',". ").	 /* statement terminator */
  final('/',"/").	 /* division */
  final('/\\',"/\\").	 /* intersection */
  final('/.',"/.").	 /* dfa expression */
  final('//',"//").	 /* map over */
  final('///',"///").	 /* indexed map over */
  final('{',"{").	 /* non-recursive braces */
  final('{?',"{?").	 /* test comprehension */
  final('{.',"{.").	 /* recursive braces */
  final('{!',"{!").	 /* iota comprehension */
  final('|',"|").	 /* type union, case union */
  final('|]',"|]").	 /* measure brackets */
  final('|:',"|:").	 /* constrained type */
  final('||',"||").	 /* disjunction */
  final('|=',"|=").	 /* implicit variable */
  final('|>',"|>").	 /* meta quote */
  final('}',"}").	 /* non-recursive braces */
  final('~',"~").	 /* logical negation */
  final('~~',"~~").	 /* quantifier */
  final('~=',"~=").	 /* not equals */
  final('~>',"~>").	 /* type function */
  final('[',"[").	 /* square brackets */
  final('[|',"[|").	 /* measure brackets */
  final('\\',"\\").	 /* difference */
  final('\\+',"\\+").	 /* add element to set */
  final('\\-',"\\-").	 /* remove element from set */
  final('\\/',"\\/").	 /* union */
  final(']',"]").	 /* square brackets */
  final('^/',"^/").	 /* filter */
  final('^//',"^//").	 /* filter map */
  final(':',":").	 /* type annotation */
  final(':?',":?").	 /* fallable type coercion */
  final('::',"::").	 /* type coercion */
  final('::=',"::=").	 /* algebraic type definition */
  final(':=',":=").	 /* assignment */
  final(';',";").	 /* sequencing operator */
  final('<',"<").	 /* less than */
  final('<*',"<*").	 /* left fold */
  final('<~',"<~").	 /* type interface rule */
  final('<|',"<|").	 /* meta quote */
  final('<=>',"<=>").	 /* constructor arrow */
  final('=',"=").	 /* definition */
  final('=<',"=<").	 /* less than or equal */
  final('==',"==").	 /* equality predicate */
  final('=>',"=>").	 /* function arrow */
  final('=>>',"=>>").	 /* continuation arrow */
  final('>',">").	 /* greater than */
  final('>=',">=").	 /* greater than or equal */
  final('>>',">>").	 /* grammar produce value */
  final('>>=',">>=").	 /* monadic bind */
  final('??',"??").	 /* conditional operator */
  final('?=',"?=").	 /* optional decomposition match */
  final('?}',"?}").	 /* test comprehension */
  final('@',"@").	 /* meta annotation */
  final('!',"!").	 /* pick up value from a ref cell */
  final('!!',"!!").	 /* pick up value from a thunk */
  final('!}',"!}").	 /* iota comprehension */
  final('⊕',"⊕").	 /* addition */
  final('•',"•").	 /* function composition */
  final('••',"••").	 /* binary function composition */
  final('#',"#").	 /* Macro statement marker */
  final('$$',"$$").	 /* thunk expression */

  keyword("retire").
  keyword("all").
  keyword("&&").
  keyword("let").
  keyword("~>").
  keyword("{.").
  keyword("do").
  keyword("import").
  keyword("catch").
  keyword("valis").
  keyword(",..").
  keyword("for").
  keyword("..<").
  keyword("..>").
  keyword("{?").
  keyword("raise").
  keyword("async").
  keyword(". ").
  keyword("then").
  keyword("ζ").
  keyword("!").
  keyword("->>").
  keyword("?=").
  keyword("default").
  keyword("#").
  keyword("!}").
  keyword("??").
  keyword("(").
  keyword(")").
  keyword("*>").
  keyword("resume").
  keyword(",").
  keyword("contract").
  keyword("./").
  keyword(".").
  keyword("raises").
  keyword("try").
  keyword("exists").
  keyword("if").
  keyword("$$").
  keyword(":").
  keyword(";").
  keyword("-->").
  keyword(".=").
  keyword("=>>").
  keyword("=").
  keyword("|:").
  keyword("@").
  keyword("|=").
  keyword("|>").
  keyword("in").
  keyword("break").
  keyword("suspend").
  keyword("~~").
  keyword("!!").
  keyword("/.").
  keyword("public").
  keyword("[|").
  keyword("ref").
  keyword("where").
  keyword("case").
  keyword("[").
  keyword("=>").
  keyword("]").
  keyword("<=>").
  keyword("|]").
  keyword("generator").
  keyword("?}").
  keyword("valof").
  keyword("yield").
  keyword("while").
  keyword("private").
  keyword("::").
  keyword(":?").
  keyword("implementation").
  keyword("<|").
  keyword("<~").
  keyword("{").
  keyword("type").
  keyword(".}").
  keyword("|").
  keyword("}").
  keyword("~").
  keyword("||").
  keyword("else").
  keyword("::=").
  keyword(">>").
  keyword("{!").

  isKeyword(X):- keyword(X), !.

