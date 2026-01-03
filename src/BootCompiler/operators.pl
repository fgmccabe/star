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

  operator("!", [postfixOp(99, 100), infixOp(99, 100, 99)]).
  operator("!!", [postfixOp(99, 100)]).
  operator("#", [infixOp(759, 760, 759)]).
  operator("$$", [prefixOp(305, 304)]).
  operator("%", [infixOp(700, 700, 699)]).
  operator("&&", [infixOp(909, 910, 910)]).
  operator("*", [postfixOp(699, 700), infixOp(700, 700, 699)]).
  operator("**", [infixOp(600, 600, 599)]).
  operator("*>", [infixOp(904, 905, 904), prefixOp(905, 904)]).
  operator("+", [postfixOp(699, 700), infixOp(720, 720, 719)]).
  operator("++", [infixOp(719, 720, 720)]).
  operator("+++", [infixOp(719, 720, 720)]).
  operator(",", [infixOp(999, 1000, 1000)]).
  operator(",..", [infixOp(999, 1000, 999)]).
  operator("-", [prefixOp(300, 299), infixOp(720, 720, 719)]).
  operator("-->", [infixOp(1248, 1249, 1248)]).
  operator("->", [infixOp(889, 890, 889)]).
  operator("->>", [infixOp(1199, 1200, 1199)]).
  operator(".", [prefixOp(10, 9), infixOp(100, 100, 99)]).
  operator(".#.", [infixOp(600, 600, 599)]).
  operator(".&.", [infixOp(700, 700, 699)]).
  operator(".+.", [prefixOp(700, 699)]).
  operator("..<", [infixOp(749, 750, 749)]).
  operator("..>", [infixOp(749, 750, 749)]).
  operator(".<.", [infixOp(699, 700, 699)]).
  operator(".<<.", [infixOp(600, 600, 599)]).
  operator(".=", [infixOp(899, 900, 899)]).
  operator(".>>.", [infixOp(600, 600, 599)]).
  operator(".>>>.", [infixOp(600, 600, 599)]).
  operator(".^.", [infixOp(720, 720, 719)]).
  operator(".|.", [infixOp(720, 720, 719)]).
  operator(".~.", [prefixOp(650, 649)]).
  operator("/", [infixOp(700, 700, 699)]).
  operator("//", [infixOp(960, 960, 959)]).
  operator("///", [infixOp(960, 960, 959)]).
  operator("/\\", [infixOp(700, 700, 699)]).
  operator(":", [infixOp(1249, 1250, 1249)]).
  operator("::", [infixOp(399, 400, 399)]).
  operator("::=", [infixOp(1549, 1550, 1549)]).
  operator(":=", [infixOp(974, 975, 974)]).
  operator(":?", [infixOp(399, 400, 399)]).
  operator(";", [postfixOp(1250, 1251), infixOp(1250, 1251, 1251)]).
  operator("<", [infixOp(899, 900, 899)]).
  operator("<*", [infixOp(600, 600, 599)]).
  operator("<-", [infixOp(974, 975, 974)]).
  operator("<=>", [infixOp(949, 950, 949)]).
  operator("<~", [infixOp(998, 999, 998)]).
  operator("=", [infixOp(974, 975, 974)]).
  operator("=<", [infixOp(899, 900, 899)]).
  operator("==", [infixOp(899, 900, 899)]).
  operator("=>", [infixOp(949, 950, 950)]).
  operator("=>>", [infixOp(949, 950, 950)]).
  operator(">", [infixOp(899, 900, 899)]).
  operator(">=", [infixOp(899, 900, 899)]).
  operator(">>", [infixOp(949, 950, 950)]).
  operator(">>=", [infixOp(949, 950, 950)]).
  operator("?", [infixOp(299, 300, 299), prefixOp(300, 299)]).
  operator("?=", [infixOp(899, 900, 899)]).
  operator("??", [infixOp(919, 920, 920), prefixOp(950, 949)]).
  operator("?|", [infixOp(960, 960, 959)]).
  operator("@", [prefixOp(400, 399), infixOp(399, 400, 400)]).
  operator("\\", [infixOp(700, 700, 699)]).
  operator("\\+", [infixOp(700, 700, 699)]).
  operator("\\-", [infixOp(700, 700, 699)]).
  operator("\\/", [infixOp(720, 720, 719)]).
  operator("^/", [infixOp(960, 960, 959)]).
  operator("^//", [infixOp(800, 800, 799)]).
  operator("all", [prefixOp(1010, 1009)]).
  operator("assert", [prefixOp(1240, 1239)]).
  operator("async", [prefixOp(1234, 1233)]).
  operator("break", [prefixOp(10, 9)]).
  operator("case", [prefixOp(901, 900)]).
  operator("catch", [infixOp(1198, 1199, 1198)]).
  operator("collect", [prefixOp(300, 299)]).
  operator("contract", [prefixOp(1560, 1559)]).
  operator("default", [postfixOp(939, 940)]).
  operator("do", [prefixOp(200, 199), infixOp(1199, 1200, 1199)]).
  operator("elemis", [prefixOp(930, 929)]).
  operator("else", [infixOp(1199, 1200, 1200)]).
  operator("exists", [prefixOp(1010, 1009)]).
  operator("for", [prefixOp(1175, 1174)]).
  operator("if", [prefixOp(1175, 1174)]).
  operator("implementation", [prefixOp(1260, 1259)]).
  operator("import", [prefixOp(900, 899)]).
  operator("in", [infixOp(899, 900, 900)]).
  operator("let", [prefixOp(899, 898)]).
  operator("private", [prefixOp(1700, 1699)]).
  operator("public", [prefixOp(1700, 1699)]).
  operator("ref", [prefixOp(899, 898)]).
  operator("resume", [infixOp(898, 899, 898)]).
  operator("retire", [prefixOp(899, 898), infixOp(898, 899, 898)]).
  operator("return", [prefixOp(930, 929)]).
  operator("show", [prefixOp(1240, 1239)]).
  operator("suspend", [prefixOp(899, 898), infixOp(898, 899, 898)]).
  operator("then", [infixOp(1179, 1180, 1179)]).
  operator("throw", [prefixOp(230, 229)]).
  operator("throws", [infixOp(949, 950, 949)]).
  operator("trace", [infixOp(139, 140, 139), prefixOp(140, 139)]).
  operator("try", [prefixOp(1200, 1199)]).
  operator("valis", [prefixOp(930, 929)]).
  operator("valof", [prefixOp(300, 299)]).
  operator("where", [infixOp(910, 911, 910)]).
  operator("while", [prefixOp(1175, 1174)]).
  operator("yield", [prefixOp(300, 299)]).
  operator("|", [prefixOp(1548, 1547), infixOp(1548, 1548, 1547)]).
  operator("|=", [infixOp(1234, 1235, 1234)]).
  operator("||", [infixOp(919, 920, 920)]).
  operator("~", [prefixOp(905, 904)]).
  operator("~=", [infixOp(899, 900, 899)]).
  operator("~>", [infixOp(1230, 1231, 1230)]).
  operator("~~", [infixOp(1239, 1240, 1240)]).
  operator("ζ", [prefixOp(1, 0)]).
  operator("•", [infixOp(450, 450, 449)]).
  operator("••", [infixOp(450, 450, 449)]).
  operator("⊕", [infixOp(720, 720, 719)]).

  bracket("[||]", "[|", "|]", "", 2000).
  bracket("<||>", "<|", "|>", "", 2000).
  bracket("/../", "/.", "./", "", 2000).
  bracket("{..}", "{.", ".}", ".\n", 2000).
  bracket("[]", "[", "]", ",", 2000).
  bracket("()", "(", ")", ",", 2000).
  bracket("{}", "{", "}", ".\n", 2000).
  bracket("{??}", "{?", "?}", "", 2000).
  bracket("{!!}", "{!", "!}", "", 2000).

  follows('','⊕','⊕').
  follows('','ζ','ζ').
  follows('','!','!').
  follows('','•','•').
  follows('','#','#').
  follows('','$','$').
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
  follows('',':',':').
  follows('',';',';').
  follows('','<','<').
  follows('','=','=').
  follows('','>','>').
  follows('','?','?').
  follows('','@','@').
  follows('','[','[').
  follows('','\\','\\').
  follows('',']',']').
  follows('','^','^').
  follows('','{','{').
  follows('','|','|').
  follows('','}','}').
  follows('','~','~').
  follows('!','!','!!').
  follows('!','}','!}').
  follows('•','•','••').
  follows('$','$','$$').
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
  follows('.',' ','. ').
  follows('.','#','.#').
  follows('.','&','.&').
  follows('.','+','.+').
  follows('.','.','..').
  follows('.','/','./').
  follows('.','<','.<').
  follows('.','=','.=').
  follows('.','>','.>').
  follows('.','^','.^').
  follows('.','|','.|').
  follows('.','}','.}').
  follows('.','~','.~').
  follows('.#','.','.#.').
  follows('.&','.','.&.').
  follows('.+','.','.+.').
  follows('..','<','..<').
  follows('..','>','..>').
  follows('.<','.','.<.').
  follows('.<','<','.<<').
  follows('.<<','.','.<<.').
  follows('.>','>','.>>').
  follows('.>>','.','.>>.').
  follows('.>>','>','.>>>').
  follows('.>>>','.','.>>>.').
  follows('.^','.','.^.').
  follows('.|','.','.|.').
  follows('.~','.','.~.').
  follows('/','.','/.').
  follows('/','/','//').
  follows('/','\\','/\\').
  follows('//','/','///').
  follows(':',':','::').
  follows(':','=',':=').
  follows(':','?',':?').
  follows('::','=','::=').
  follows('<','*','<*').
  follows('<','-','<-').
  follows('<','=','<=').
  follows('<','|','<|').
  follows('<','~','<~').
  follows('<=','>','<=>').
  follows('=','<','=<').
  follows('=','=','==').
  follows('=','>','=>').
  follows('=>','>','=>>').
  follows('>','=','>=').
  follows('>','>','>>').
  follows('>>','=','>>=').
  follows('?','=','?=').
  follows('?','?','??').
  follows('?','|','?|').
  follows('?','}','?}').
  follows('[','|','[|').
  follows('\\','+','\\+').
  follows('\\','-','\\-').
  follows('\\','/','\\/').
  follows('^','/','^/').
  follows('^/','/','^//').
  follows('{','!','{!').
  follows('{','.','{.').
  follows('{','?','{?').
  follows('|','=','|=').
  follows('|','>','|>').
  follows('|',']','|]').
  follows('|','|','||').
  follows('~','=','~=').
  follows('~','>','~>').
  follows('~','~','~~').

  final('⊕',"⊕").	 /* addition */
  final('ζ',"ζ").	 /* interpret a symbol without dereferencing constraints */
  final('!',"!").	 /* pick up value from a ref cell */
  final('!!',"!!").	 /* pick up value from a thunk */
  final('!}',"!}").	 /* iota comprehension */
  final('•',"•").	 /* function composition */
  final('••',"••").	 /* binary function composition */
  final('#',"#").	 /* package separator */
  final('$$',"$$").	 /* thunk expression */
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
  final('. ',". ").	 /* statement terminator */
  final('.#.',".#.").	 /* test nth bit */
  final('.&.',".&.").	 /* bitwise and */
  final('.+.',".+.").	 /* count of number of bits */
  final('..<',"..<").	 /* range increasing iterator expression */
  final('..>',"..>").	 /* range decreasing iterator expression */
  final('./',"./").	 /* dfa expression */
  final('.<.',".<.").	 /* set membership */
  final('.<<.',".<<.").	 /* left shift */
  final('.=',".=").	 /* pattern match */
  final('.>>.',".>>.").	 /* logical shift right */
  final('.>>>.',".>>>.").	 /* arithmetic shift right */
  final('.^.',".^.").	 /* bitwise xor */
  final('.|.',".|.").	 /* bitwise or */
  final('.}',".}").	 /* recursive braces */
  final('.~.',".~.").	 /* bitwise 1's complement */
  final('/',"/").	 /* division */
  final('/.',"/.").	 /* dfa expression */
  final('//',"//").	 /* map over */
  final('///',"///").	 /* indexed map over */
  final('/\\',"/\\").	 /* intersection */
  final(':',":").	 /* type declaration/annotation */
  final('::',"::").	 /* type coercion */
  final('::=',"::=").	 /* algebraic type definition */
  final(':=',":=").	 /* assignment */
  final(':?',":?").	 /* fallable type coercion */
  final(';',";").	 /* sequencing operator */
  final('<',"<").	 /* less than */
  final('<*',"<*").	 /* left fold */
  final('<-',"<-").	 /* monadic valof */
  final('<=>',"<=>").	 /* constructor arrow */
  final('<|',"<|").	 /* meta quote */
  final('<~',"<~").	 /* type interface rule */
  final('=',"=").	 /* definition */
  final('=<',"=<").	 /* less than or equal */
  final('==',"==").	 /* equality predicate */
  final('=>',"=>").	 /* function arrow */
  final('=>>',"=>>").	 /* continuation arrow */
  final('>',">").	 /* greater than */
  final('>=',">=").	 /* greater than or equal */
  final('>>',">>").	 /* grammar produce value */
  final('>>=',">>=").	 /* monadic bind */
  final('?',"?").	 /* option match */
  final('?=',"?=").	 /* optional decomposition match */
  final('??',"??").	 /* conditional operator */
  final('?|',"?|").	 /* optional conditional */
  final('?}',"?}").	 /* test comprehension */
  final('@',"@").	 /* meta annotation */
  final('[',"[").	 /* square brackets */
  final('[|',"[|").	 /* measure brackets */
  final('\\',"\\").	 /* difference */
  final('\\+',"\\+").	 /* add element to set */
  final('\\-',"\\-").	 /* remove element from set */
  final('\\/',"\\/").	 /* union */
  final(']',"]").	 /* square brackets */
  final('^/',"^/").	 /* filter */
  final('^//',"^//").	 /* filter map */
  final('{',"{").	 /* non-recursive braces */
  final('{!',"{!").	 /* iota comprehension */
  final('{.',"{.").	 /* recursive braces */
  final('{?',"{?").	 /* test comprehension */
  final('|',"|").	 /* type union, case union */
  final('|=',"|=").	 /* constrained type */
  final('|>',"|>").	 /* meta quote */
  final('|]',"|]").	 /* measure brackets */
  final('||',"||").	 /* disjunction */
  final('}',"}").	 /* non-recursive braces */
  final('~',"~").	 /* logical negation */
  final('~=',"~=").	 /* not equals */
  final('~>',"~>").	 /* type function */
  final('~~',"~~").	 /* quantifier */

  keyword("!").
  keyword("!!").
  keyword("!}").
  keyword("#").
  keyword("$$").
  keyword("&&").
  keyword("(").
  keyword(")").
  keyword("*>").
  keyword(",").
  keyword(",..").
  keyword("-->").
  keyword("->>").
  keyword(".").
  keyword(". ").
  keyword("..<").
  keyword("..>").
  keyword("./").
  keyword(".=").
  keyword(".}").
  keyword("/.").
  keyword(":").
  keyword("::").
  keyword("::=").
  keyword(":?").
  keyword(";").
  keyword("<-").
  keyword("<=>").
  keyword("<|").
  keyword("<~").
  keyword("=").
  keyword("=>").
  keyword("=>>").
  keyword(">>").
  keyword("?=").
  keyword("??").
  keyword("?}").
  keyword("@").
  keyword("[").
  keyword("[|").
  keyword("]").
  keyword("all").
  keyword("async").
  keyword("break").
  keyword("case").
  keyword("catch").
  keyword("collect").
  keyword("contract").
  keyword("default").
  keyword("do").
  keyword("elemis").
  keyword("else").
  keyword("exists").
  keyword("for").
  keyword("generator").
  keyword("if").
  keyword("implementation").
  keyword("import").
  keyword("in").
  keyword("let").
  keyword("private").
  keyword("public").
  keyword("ref").
  keyword("resume").
  keyword("retire").
  keyword("suspend").
  keyword("then").
  keyword("throw").
  keyword("throws").
  keyword("try").
  keyword("unreachable").
  keyword("valis").
  keyword("valof").
  keyword("void").
  keyword("where").
  keyword("while").
  keyword("yield").
  keyword("{").
  keyword("{!").
  keyword("{.").
  keyword("{?").
  keyword("|").
  keyword("|=").
  keyword("|>").
  keyword("|]").
  keyword("||").
  keyword("}").
  keyword("~").
  keyword("~>").
  keyword("~~").
  keyword("ζ").

  isKeyword(X):- keyword(X), !.

