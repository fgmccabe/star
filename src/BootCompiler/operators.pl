/* Automatically generated, do not edit */

:-module(operators,[infixOp/4,prefixOp/3,postfixOp/3,isOperator/1,follows/3,final/2,bracket/4]).

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

  operator("all", [prefixOp(1010, 1009)]).
  operator("^=", [infixOp(899, 900, 899)]).
  operator("&&", [infixOp(910, 910, 909)]).
  operator("pure", [prefixOp(300, 299)]).
  operator("..,", [infixOp(999, 1000, 1000)]).
  operator("~>", [infixOp(1230, 1231, 1230)]).
  operator(".|.", [infixOp(720, 720, 719)]).
  operator("import", [prefixOp(900, 899)]).
  operator("of", [infixOp(399, 400, 399)]).
  operator(",..", [infixOp(999, 1000, 999)]).
  operator("==>", [infixOp(949, 950, 949)]).
  operator("**", [infixOp(600, 600, 599)]).
  operator("->", [infixOp(899, 900, 899)]).
  operator(".+.", [prefixOp(700, 699)]).
  operator("raise", [prefixOp(300, 299)]).
  operator("!", [postfixOp(99, 100)]).
  operator("->>", [infixOp(1199, 1200, 1199)]).
  operator("=!=", [infixOp(899, 900, 899)]).
  operator("default", [postfixOp(939, 940)]).
  operator("#", [prefixOp(1750, 1749), infixOp(759, 760, 759)]).
  operator("^^", [prefixOp(912, 911), infixOp(911, 912, 911)]).
  operator("%", [infixOp(700, 700, 699)]).
  operator("<-", [infixOp(904, 905, 904)]).
  operator(".>>>.", [infixOp(600, 600, 599)]).
  operator("\\+", [prefixOp(905, 904)]).
  operator("*", [postfixOp(699, 700), infixOp(700, 700, 699)]).
  operator("+", [postfixOp(699, 700), infixOp(720, 720, 719)]).
  operator(".>>.", [infixOp(600, 600, 599)]).
  operator("*>", [infixOp(904, 905, 904)]).
  operator(",", [infixOp(999, 1000, 1000)]).
  operator("contract", [prefixOp(1260, 1259)]).
  operator("-", [prefixOp(300, 299), infixOp(720, 720, 719)]).
  operator(".", [infixOp(100, 100, 99)]).
  operator("/", [infixOp(700, 700, 699)]).
  operator("<*>", [infixOp(949, 950, 950)]).
  operator("•", [infixOp(450, 450, 449)]).
  operator("exists", [prefixOp(1010, 1009)]).
  operator("<=", [infixOp(949, 950, 949)]).
  operator(":", [infixOp(1249, 1250, 1249)]).
  operator(";", [infixOp(914, 915, 915)]).
  operator("-->", [infixOp(1199, 1200, 1199)]).
  operator("<", [infixOp(899, 900, 899)]).
  operator(".=", [infixOp(899, 900, 899)]).
  operator("=", [infixOp(974, 975, 974)]).
  operator("|:", [infixOp(1234, 1235, 1234)]).
  operator("show", [prefixOp(1260, 1259)]).
  operator("++", [infixOp(719, 720, 720)]).
  operator(">", [infixOp(899, 900, 899)]).
  operator("return", [prefixOp(300, 299)]).
  operator("?", [infixOp(919, 920, 920)]).
  operator("@", [prefixOp(400, 399), infixOp(399, 400, 400)]).
  operator("in", [infixOp(899, 900, 899)]).
  operator("^|", [infixOp(1248, 1249, 1249)]).
  operator("open", [prefixOp(900, 899)]).
  operator("~~", [infixOp(1239, 1240, 1240)]).
  operator("assert", [prefixOp(1260, 1259)]).
  operator("=.", [infixOp(899, 900, 899)]).
  operator(".^.", [infixOp(720, 720, 719)]).
  operator("//", [infixOp(800, 800, 799)]).
  operator("public", [prefixOp(1700, 1699)]).
  operator("ref", [prefixOp(900, 899)]).
  operator(".~.", [prefixOp(650, 649)]).
  operator("where", [infixOp(910, 911, 910)]).
  operator("=<", [infixOp(899, 900, 899)]).
  operator("==", [infixOp(899, 900, 899)]).
  operator("=>", [infixOp(949, 950, 950)]).
  operator("^", [prefixOp(100, 99), infixOp(99, 100, 99)]).
  operator("<=>", [infixOp(949, 950, 949)]).
  operator("valof", [prefixOp(300, 299)]).
  operator("private", [prefixOp(1700, 1699)]).
  operator(".&.", [infixOp(700, 700, 699)]).
  operator("///", [infixOp(800, 800, 799)]).
  operator("::", [infixOp(399, 400, 399)]).
  operator("+++", [infixOp(719, 720, 720)]).
  operator(":=", [infixOp(974, 975, 974)]).
  operator(".<<.", [infixOp(600, 600, 599)]).
  operator("^+", [prefixOp(905, 904)]).
  operator("^.", [infixOp(450, 450, 449)]).
  operator(">>=", [infixOp(949, 950, 950)]).
  operator("^/", [infixOp(800, 800, 799)]).
  operator(">>>", [infixOp(919, 920, 919)]).
  operator("<~", [infixOp(1230, 1231, 1230)]).
  operator("type", [prefixOp(1251, 1250)]).
  operator("implementation", [prefixOp(1260, 1259)]).
  operator("|", [infixOp(1248, 1249, 1249)]).
  operator(".~", [infixOp(499, 500, 499)]).
  operator(".#.", [infixOp(600, 600, 599)]).
  operator("~", [infixOp(489, 499, 489)]).
  operator("^//", [infixOp(800, 800, 799)]).
  operator("||", [infixOp(919, 920, 920)]).
  operator("::=", [infixOp(1249, 1250, 1249)]).
  operator(">=", [infixOp(899, 900, 899)]).
  operator(">>", [infixOp(949, 950, 950)]).

  bracket("{..}", "{.", ".}", 2000).
  bracket("[]", "[", "]", 2000).
  bracket("(..)", "(.", ".)", 2000).
  bracket("()", "(", ")", 2000).
  bracket("{}", "{", "}", 2000).

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
  follows('','•','•').
  follows('','#','#').
  follows('','$','$').
  follows('&','&','&&').
  follows('(','.','(.').
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
  follows('.',')','.)').
  follows('.','~','.~').
  follows('.','<','.<').
  follows('.','^','.^').
  follows('.','+','.+').
  follows('.','=','.=').
  follows('.','>','.>').
  follows('.','.','..').
  follows('.',' ','. ').
  follows('.#','.','.#.').
  follows('.&','.','.&.').
  follows('.|','.','.|.').
  follows('.~','.','.~.').
  follows('.<','<','.<<').
  follows('.<<','.','.<<.').
  follows('.^','.','.^.').
  follows('.+','.','.+.').
  follows('.>','>','.>>').
  follows('.>>','.','.>>.').
  follows('.>>','>','.>>>').
  follows('.>>>','.','.>>>.').
  follows('..',',','..,').
  follows('/','/','//').
  follows('//','/','///').
  follows('{','.','{.').
  follows('|',':','|:').
  follows('|','|','||').
  follows('~','~','~~').
  follows('~','>','~>').
  follows('\\','+','\\+').
  follows('^','+','^+').
  follows('^','^','^^').
  follows('^','.','^.').
  follows('^','/','^/').
  follows('^','=','^=').
  follows('^','|','^|').
  follows('^/','/','^//').
  follows(':',':','::').
  follows(':','=',':=').
  follows('::','=','::=').
  follows('<','*','<*').
  follows('<','~','<~').
  follows('<','-','<-').
  follows('<','=','<=').
  follows('<*','>','<*>').
  follows('<=','>','<=>').
  follows('=','<','=<').
  follows('=','.','=.').
  follows('=','!','=!').
  follows('=','=','==').
  follows('=','>','=>').
  follows('=!','=','=!=').
  follows('==','>','==>').
  follows('>','=','>=').
  follows('>','>','>>').
  follows('>>','=','>>=').
  follows('>>','>','>>>').


  final('%',"%").	 /* modulo */
  final('&&',"&&").	 /* conjunction */
  final('(',"(").	 /* parentheses */
  final('(.',"(.").	 /* hidden parentheses */
  final(')',")").	 /* parentheses */
  final('*',"*").	 /* zero or more repetitions */
  final('**',"**").	 /* exponentiation */
  final('*>',"*>").	 /* for all */
  final('+',"+").	 /* one or more repetitions */
  final('++',"++").	 /* concatenate */
  final('+++',"+++").	 /* choice */
  final(',',",").	 /* tupling operator */
  final(',..',",..").	 /* list cons */
  final('-',"-").	 /* arithmetic negation */
  final('-->',"-->").	 /* grammar rule */
  final('->',"->").	 /* map entry */
  final('->>',"->>").	 /* dependent type marker */
  final('.',".").	 /* object access */
  final('.#.',".#.").	 /* test nth bit */
  final('.&.',".&.").	 /* bitwise and */
  final('.|.',".|.").	 /* bitwise or */
  final('.}',".}").	 /* non-recursive braces */
  final('.)',".)").	 /* hidden parentheses */
  final('.~',".~").	 /* grammar parse */
  final('.~.',".~.").	 /* bitwise 1's complement */
  final('.<<.',".<<.").	 /* shift left */
  final('.^.',".^.").	 /* bitwise xor */
  final('.+.',".+.").	 /* count of number of bits */
  final('.=',".=").	 /* pattern match */
  final('.>>.',".>>.").	 /* logical shift right */
  final('.>>>.',".>>>.").	 /* arithmetic shift right */
  final('..,',"..,").	 /* list cons */
  final('. ',". ").	 /* statement terminator */
  final('/',"/").	 /* division */
  final('//',"//").	 /* map over */
  final('///',"///").	 /* indexed map over */
  final('{',"{").	 /* braces */
  final('{.',"{.").	 /* non-recursive braces */
  final('|',"|").	 /* type union, conditional, and abstraction */
  final('|:',"|:").	 /* constrained type */
  final('||',"||").	 /* disjunction */
  final('}',"}").	 /* braces */
  final('~',"~").	 /* grammar remainder */
  final('~~',"~~").	 /* quantifier */
  final('~>',"~>").	 /* type function */
  final('[',"[").	 /* square brackets */
  final('\\+',"\\+").	 /* logical negation */
  final(']',"]").	 /* square brackets */
  final('^',"^").	 /* Optional propagation */
  final('^+',"^+").	 /* look ahead parser operator */
  final('^^',"^^").	 /* output from an action expression */
  final('^.',"^.").	 /* optional object access */
  final('^/',"^/").	 /* filter */
  final('^//',"^//").	 /* filter map */
  final('^=',"^=").	 /* optional decomposition match */
  final('^|',"^|").	 /* option or-else operator */
  final(':',":").	 /* type annotation */
  final('::',"::").	 /* type coercion */
  final('::=',"::=").	 /* algebraic type definition */
  final(':=',":=").	 /* reassignable variable definition */
  final(';',";").	 /* sequencing operator */
  final('<',"<").	 /* less than */
  final('<*>',"<*>").	 /* applicative splat */
  final('<~',"<~").	 /* type interface rule */
  final('<-',"<-").	 /* variable bind */
  final('<=',"<=").	 /* pattern arrow */
  final('<=>',"<=>").	 /* constructor arrow */
  final('=',"=").	 /* definition */
  final('=<',"=<").	 /* less than or equal */
  final('=.',"=.").	 /* pattern match */
  final('=!=',"=!=").	 /* not equals */
  final('==',"==").	 /* equality predicate */
  final('==>',"==>").	 /* macro arrow */
  final('=>',"=>").	 /* function arrow */
  final('>',">").	 /* greater than */
  final('>=',">=").	 /* greater than or equal */
  final('>>',">>").	 /* monadic bind */
  final('>>=',">>=").	 /* monadic bind */
  final('>>>',">>>").	 /* handle error */
  final('?',"?").	 /* conditional operator */
  final('@',"@").	 /* meta annotation */
  final('!',"!").	 /* pick up a value from a ref cell */
  final('•',"•").	 /* function composition */
  final('#',"#").	 /* Macro statement marker */
  final('$',"$").	 /* Used for curried functions and types */

