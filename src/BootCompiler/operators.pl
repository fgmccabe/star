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

  operator("retire", [prefixOp(901, 900), infixOp(900, 901, 900)]).
  operator("all", [prefixOp(1010, 1009)]).
  operator(".<.", [infixOp(699, 700, 699)]).
  operator("^=", [infixOp(899, 900, 899)]).
  operator("&&", [infixOp(910, 910, 909)]).
  operator("^?", [infixOp(800, 800, 799)]).
  operator("pure", [prefixOp(300, 299)]).
  operator("~=", [infixOp(899, 900, 899)]).
  operator("~>", [infixOp(1230, 1231, 1230)]).
  operator("throw", [prefixOp(930, 929)]).
  operator(".|.", [infixOp(720, 720, 719)]).
  operator("do", [prefixOp(200, 199), infixOp(1199, 1200, 1199)]).
  operator("import", [prefixOp(900, 899)]).
  operator("catch", [infixOp(1198, 1199, 1198)]).
  operator("valis", [prefixOp(930, 929)]).
  operator(",..", [infixOp(999, 1000, 999)]).
  operator("for", [prefixOp(1175, 1174)]).
  operator("**", [infixOp(600, 600, 599)]).
  operator("->", [infixOp(889, 890, 889)]).
  operator(".+.", [prefixOp(700, 699)]).
  operator("ignore", [prefixOp(930, 929)]).
  operator("<$", [infixOp(719, 720, 720)]).
  operator("raise", [prefixOp(930, 929)]).
  operator("then", [infixOp(1179, 1180, 1179)]).
  operator("!", [postfixOp(99, 100), infixOp(99, 100, 99)]).
  operator("->>", [infixOp(1199, 1200, 1199)]).
  operator("default", [postfixOp(939, 940)]).
  operator("#", [prefixOp(1750, 1749), infixOp(759, 760, 759)]).
  operator("%", [infixOp(700, 700, 699)]).
  operator("<-", [infixOp(904, 905, 904)]).
  operator(".>>>.", [infixOp(600, 600, 599)]).
  operator("\\+", [infixOp(700, 700, 699)]).
  operator("<<-", [infixOp(974, 975, 974)]).
  operator("*", [postfixOp(699, 700), infixOp(700, 700, 699)]).
  operator("\\-", [infixOp(700, 700, 699)]).
  operator("+", [postfixOp(699, 700), infixOp(720, 720, 719)]).
  operator(".>>.", [infixOp(600, 600, 599)]).
  operator("resume", [prefixOp(901, 900), infixOp(900, 901, 900)]).
  operator("*>", [infixOp(904, 905, 904)]).
  operator(",", [infixOp(999, 1000, 1000)]).
  operator("contract", [prefixOp(1260, 1259)]).
  operator("\\/", [infixOp(720, 720, 719)]).
  operator("-", [prefixOp(300, 299), infixOp(720, 720, 719)]).
  operator(".", [prefixOp(10, 9), infixOp(100, 100, 99)]).
  operator("/", [infixOp(700, 700, 699)]).
  operator("<*>", [infixOp(949, 950, 950)]).
  operator("try", [prefixOp(1200, 1199)]).
  operator("exists", [prefixOp(1010, 1009)]).
  operator("if", [prefixOp(1175, 1174)]).
  operator("$$", [prefixOp(899, 898)]).
  operator(":", [infixOp(1249, 1250, 1249)]).
  operator(";", [postfixOp(1250, 1251), infixOp(1250, 1251, 1251)]).
  operator("<", [infixOp(899, 900, 899)]).
  operator(".=", [infixOp(899, 900, 899)]).
  operator("=>>", [infixOp(949, 950, 950)]).
  operator("=", [infixOp(974, 975, 974)]).
  operator("|:", [infixOp(1234, 1235, 1234)]).
  operator("show", [prefixOp(1240, 1239)]).
  operator("++", [infixOp(719, 720, 720)]).
  operator(">", [infixOp(899, 900, 899)]).
  operator("return", [prefixOp(930, 929)]).
  operator("?", [infixOp(919, 920, 920)]).
  operator("@", [prefixOp(400, 399), infixOp(399, 400, 400)]).
  operator("in", [infixOp(899, 900, 900)]).
  operator("^|", [infixOp(919, 920, 920)]).
  operator("suspend", [prefixOp(901, 900), infixOp(900, 901, 900)]).
  operator("open", [prefixOp(900, 899)]).
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
  operator("^", [prefixOp(100, 99), infixOp(99, 100, 99)]).
  operator("<=>", [infixOp(949, 950, 949)]).
  operator("perform", [prefixOp(300, 299)]).
  operator("valof", [prefixOp(300, 299)]).
  operator("until", [infixOp(1174, 1175, 1174)]).
  operator("while", [prefixOp(1175, 1174)]).
  operator("private", [prefixOp(1200, 1199)]).
  operator("•", [infixOp(450, 450, 449)]).
  operator(".&.", [infixOp(700, 700, 699)]).
  operator("///", [infixOp(960, 960, 959)]).
  operator("::", [infixOp(399, 400, 399)]).
  operator("+++", [infixOp(719, 720, 720)]).
  operator(":=", [infixOp(974, 975, 974)]).
  operator(".access", [prefixOp(1260, 1259)]).
  operator(":?", [infixOp(399, 400, 399)]).
  operator(".<<.", [infixOp(600, 600, 599)]).
  operator("^.", [infixOp(450, 450, 449)]).
  operator(">>=", [infixOp(949, 950, 950)]).
  operator("^/", [infixOp(960, 960, 959)]).
  operator("<~", [infixOp(1230, 1231, 1230)]).
  operator("type", [prefixOp(1251, 1250)]).
  operator("implementation", [prefixOp(1260, 1259)]).
  operator("|", [infixOp(1248, 1248, 1247)]).
  operator(".#.", [infixOp(600, 600, 599)]).
  operator("handle", [infixOp(1198, 1199, 1198)]).
  operator("~", [prefixOp(905, 904)]).
  operator("^//", [infixOp(800, 800, 799)]).
  operator("||", [infixOp(919, 920, 920)]).
  operator("else", [infixOp(1199, 1200, 1200)]).
  operator("::=", [infixOp(1249, 1250, 1249)]).
  operator("/\\", [infixOp(700, 700, 699)]).
  operator(">=", [infixOp(899, 900, 899)]).
  operator(">>", [infixOp(949, 950, 950)]).

  bracket("[||]", "[|", "|]", "", 2000).
  bracket("<||>", "<|", "|>", "", 2000).
  bracket("{..}", "{.", ".}", ".\n", 2000).
  bracket("[]", "[", "]", ",", 2000).
  bracket("()", "(", ")", ",", 2000).
  bracket("{}", "{", "}", ".\n", 2000).
  bracket("(||)", "(|", "|)", "", 2000).
  bracket("{??}", "{?", "?}", "", 2000).
  bracket("{!!}", "{!", "!}", "", 2000).

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
  follows('(','|','(|').
  follows('*','*','**').
  follows('*','>','*>').
  follows('+','+','++').
  follows('++','+','+++').
  follows(',','.',',.').
  follows(',.','.',',..').
  follows('-','>','->').
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
  follows('.','a','.a').
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
  follows('.a','c','.ac').
  follows('.ac','c','.acc').
  follows('.acc','e','.acce').
  follows('.acce','s','.acces').
  follows('.acces','s','.access').
  follows('/','\\','/\\').
  follows('/','/','//').
  follows('//','/','///').
  follows('{','?','{?').
  follows('{','.','{.').
  follows('{','!','{!').
  follows('|',']','|]').
  follows('|',':','|:').
  follows('|','|','||').
  follows('|','>','|>').
  follows('|',')','|)').
  follows('~','~','~~').
  follows('~','=','~=').
  follows('~','>','~>').
  follows('[','|','[|').
  follows('\\','+','\\+').
  follows('\\','-','\\-').
  follows('\\','/','\\/').
  follows('^','?','^?').
  follows('^','.','^.').
  follows('^','/','^/').
  follows('^','=','^=').
  follows('^','|','^|').
  follows('^/','/','^//').
  follows(':','?',':?').
  follows(':',':','::').
  follows(':','=',':=').
  follows('::','=','::=').
  follows('<','*','<*').
  follows('<','~','<~').
  follows('<','$','<$').
  follows('<','-','<-').
  follows('<','<','<<').
  follows('<','|','<|').
  follows('<','=','<=').
  follows('<*','>','<*>').
  follows('<<','-','<<-').
  follows('<=','>','<=>').
  follows('=','<','=<').
  follows('=','=','==').
  follows('=','>','=>').
  follows('=>','>','=>>').
  follows('>','=','>=').
  follows('>','>','>>').
  follows('>>','=','>>=').
  follows('?','}','?}').
  follows('!','!','!!').
  follows('!','}','!}').
  follows('$','$','$$').

  final('%',"%").	 /* modulo */
  final('&&',"&&").	 /* conjunction */
  final('(',"(").	 /* parentheses */
  final('(|',"(|").	 /* banana brackets */
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
  final('->',"->").	 /* map entry */
  final('->>',"->>").	 /* dependent type marker */
  final('.',".").	 /* identify enumerator */
  final('.#.',".#.").	 /* test nth bit */
  final('.&.',".&.").	 /* bitwise and */
  final('.|.',".|.").	 /* bitwise or */
  final('.}',".}").	 /* recursive braces */
  final('.~.',".~.").	 /* bitwise 1's complement */
  final('.<<.',".<<.").	 /* shift left */
  final('.<.',".<.").	 /* set membership */
  final('.^.',".^.").	 /* bitwise xor */
  final('.+.',".+.").	 /* count of number of bits */
  final('.=',".=").	 /* pattern match */
  final('.>>.',".>>.").	 /* logical shift right */
  final('.>>>.',".>>>.").	 /* arithmetic shift right */
  final('.access',".access").	 /* field access implementation */
  final('. ',". ").	 /* statement terminator */
  final('/',"/").	 /* division */
  final('/\\',"/\\").	 /* intersection */
  final('//',"//").	 /* map over */
  final('///',"///").	 /* indexed map over */
  final('{',"{").	 /* non-recursive braces */
  final('{?',"{?").	 /* test comprehension */
  final('{.',"{.").	 /* recursive braces */
  final('{!',"{!").	 /* iota comprehension */
  final('|',"|").	 /* type union and abstraction */
  final('|]',"|]").	 /* measure brackets */
  final('|:',"|:").	 /* constrained type */
  final('||',"||").	 /* disjunction */
  final('|>',"|>").	 /* meta quote */
  final('|)',"|)").	 /* banana brackets */
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
  final('^',"^").	 /* Optional propagation */
  final('^?',"^?").	 /* option propagate */
  final('^.',"^.").	 /* optional object access */
  final('^/',"^/").	 /* filter */
  final('^//',"^//").	 /* filter map */
  final('^=',"^=").	 /* optional decomposition match */
  final('^|',"^|").	 /* option or-else operator */
  final(':',":").	 /* type annotation */
  final(':?',":?").	 /* fallable type coercion */
  final('::',"::").	 /* type coercion */
  final('::=',"::=").	 /* algebraic type definition */
  final(':=',":=").	 /* reassignable variable definition */
  final(';',";").	 /* sequencing operator */
  final('<',"<").	 /* less than */
  final('<*>',"<*>").	 /* applicative splat */
  final('<~',"<~").	 /* type interface rule */
  final('<$',"<$").	 /* constant replace */
  final('<-',"<-").	 /* variable bind */
  final('<<-',"<<-").	 /* record replacement */
  final('<|',"<|").	 /* meta quote */
  final('<=>',"<=>").	 /* constructor arrow */
  final('=',"=").	 /* definition */
  final('=<',"=<").	 /* less than or equal */
  final('==',"==").	 /* equality predicate */
  final('=>',"=>").	 /* function arrow */
  final('=>>',"=>>").	 /* continuation arrow */
  final('>',">").	 /* greater than */
  final('>=',">=").	 /* greater than or equal */
  final('>>',">>").	 /* monadic bind */
  final('>>=',">>=").	 /* monadic bind */
  final('?',"?").	 /* conditional operator */
  final('?}',"?}").	 /* test comprehension */
  final('@',"@").	 /* meta annotation */
  final('!',"!").	 /* pick up value from a ref cell */
  final('!!',"!!").	 /* pick up value from a memo */
  final('!}',"!}").	 /* iota comprehension */
  final('⊕',"⊕").	 /* addition */
  final('•',"•").	 /* function composition */
  final('#',"#").	 /* Macro statement marker */
  final('$$',"$$").	 /* wrap value in memo */

  keyword("retire").
  keyword("all").
  keyword("^=").
  keyword("&&").
  keyword("~>").
  keyword("throw").
  keyword("{.").
  keyword("do").
  keyword("import").
  keyword("catch").
  keyword("valis").
  keyword(",..").
  keyword("for").
  keyword("{?").
  keyword("ignore").
  keyword("raise").
  keyword(". ").
  keyword("(|").
  keyword("then").
  keyword("!").
  keyword("->>").
  keyword("default").
  keyword("#").
  keyword("!}").
  keyword("<-").
  keyword("(").
  keyword(")").
  keyword("<<-").
  keyword("|)").
  keyword("resume").
  keyword("*>").
  keyword(",").
  keyword("contract").
  keyword(".").
  keyword("try").
  keyword("exists").
  keyword("if").
  keyword(":").
  keyword(";").
  keyword(".=").
  keyword("=>>").
  keyword("=").
  keyword("|:").
  keyword("?").
  keyword("@").
  keyword("|>").
  keyword("in").
  keyword("suspend").
  keyword("open").
  keyword("~~").
  keyword("public").
  keyword("[|").
  keyword("ref").
  keyword("where").
  keyword("case").
  keyword("[").
  keyword("=>").
  keyword("]").
  keyword("^").
  keyword("<=>").
  keyword("|]").
  keyword("perform").
  keyword("?}").
  keyword("valof").
  keyword("until").
  keyword("while").
  keyword("private").
  keyword("::").
  keyword(":?").
  keyword("<|").
  keyword("^.").
  keyword("<~").
  keyword("{").
  keyword("type").
  keyword("implementation").
  keyword(".}").
  keyword("|").
  keyword("}").
  keyword("handle").
  keyword("~").
  keyword("||").
  keyword("else").
  keyword("::=").
  keyword("{!").

  isKeyword(X):- keyword(X), !.

