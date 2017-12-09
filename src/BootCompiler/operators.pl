/* Automatically generated, do not edit */

:-module(operators,[infixOp/4,prefixOp/3,postfixOp/3,isOperator/1,follows/3,final/2]).

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
  operator("&&", [infixOp(910, 910, 909)]).
  operator("~>", [infixOp(1230, 1231, 1230)]).
  operator(".|.", [infixOp(720, 720, 719)]).
  operator("import", [prefixOp(900, 899)]).
  operator("\\==", [infixOp(899, 900, 899)]).
  operator(",..", [infixOp(999, 1000, 1000)]).
  operator("**", [infixOp(600, 600, 599)]).
  operator("->", [infixOp(899, 900, 899)]).
  operator("raise", [prefixOp(899, 890)]).
  operator(". ", [postfixOp(1899, 1900), infixOp(1899, 1900, 1900)]).
  operator("!", [postfixOp(99, 100)]).
  operator("->>", [infixOp(1199, 1200, 1199)]).
  operator("default", [postfixOp(939, 940)]).
  operator("#", [prefixOp(1750, 1749), infixOp(759, 760, 759)]).
  operator("%", [infixOp(700, 700, 699)]).
  operator(".>>>.", [infixOp(600, 600, 599)]).
  operator("\\+", [prefixOp(905, 904)]).
  operator("*", [infixOp(700, 700, 699)]).
  operator("+", [postfixOp(759, 760), infixOp(720, 720, 719)]).
  operator(".>>.", [infixOp(600, 600, 599)]).
  operator(",", [infixOp(999, 1000, 1000)]).
  operator("contract", [prefixOp(1260, 1259)]).
  operator("-", [prefixOp(300, 299), infixOp(720, 720, 719)]).
  operator(".", [infixOp(450, 450, 449)]).
  operator("/", [infixOp(700, 700, 699)]).
  operator("exists", [prefixOp(1010, 1009)]).
  operator("•", [infixOp(450, 450, 449)]).
  operator("<<", [infixOp(499, 500, 499)]).
  operator("<=", [infixOp(949, 950, 949)]).
  operator(":", [infixOp(1249, 1250, 1249)]).
  operator("-->", [infixOp(1199, 1200, 1199)]).
  operator("<", [infixOp(899, 900, 899)]).
  operator("=", [infixOp(949, 950, 949)]).
  operator("|:", [infixOp(1234, 1235, 1234)]).
  operator("show", [prefixOp(1260, 1259)]).
  operator(">", [infixOp(899, 900, 899)]).
  operator("return", [prefixOp(899, 890)]).
  operator("?", [infixOp(1199, 1200, 1199)]).
  operator("@", [prefixOp(1255, 1255), infixOp(1254, 1255, 1255)]).
  operator("in", [infixOp(899, 900, 899)]).
  operator("open", [prefixOp(900, 899)]).
  operator("~~", [infixOp(1239, 1240, 1240)]).
  operator("assert", [prefixOp(1260, 1259)]).
  operator(".^.", [infixOp(720, 720, 719)]).
  operator("//", [infixOp(800, 800, 799)]).
  operator("public", [prefixOp(1700, 1699)]).
  operator("ref", [prefixOp(900, 899)]).
  operator(".~.", [prefixOp(650, 649)]).
  operator("where", [infixOp(929, 930, 929)]).
  operator("=<", [infixOp(899, 900, 899)]).
  operator("==", [infixOp(899, 900, 899)]).
  operator("=>", [infixOp(949, 950, 949)]).
  operator("<=>", [infixOp(949, 950, 949)]).
  operator("%%", [infixOp(499, 500, 499)]).
  operator("private", [prefixOp(1700, 1699)]).
  operator(".&.", [infixOp(700, 700, 699)]).
  operator("///", [infixOp(800, 800, 799)]).
  operator("::", [infixOp(399, 400, 399)]).
  operator(":=", [infixOp(949, 950, 949)]).
  operator(".<<.", [infixOp(600, 600, 599)]).
  operator(">>=", [infixOp(899, 900, 900)]).
  operator("^/", [infixOp(800, 800, 799)]).
  operator("<~", [infixOp(1230, 1231, 1230)]).
  operator("type", [prefixOp(1251, 1250)]).
  operator("implementation", [prefixOp(1260, 1259)]).
  operator("|", [infixOp(1248, 1249, 1249)]).
  operator(".#.", [infixOp(600, 600, 599)]).
  operator("~", [infixOp(489, 499, 489)]).
  operator("^//", [infixOp(800, 800, 799)]).
  operator("||", [infixOp(920, 920, 919)]).
  operator("::=", [infixOp(1249, 1250, 1249)]).
  operator(">=", [infixOp(899, 900, 899)]).


  follows('','%','%').
  follows('','&','&').
  follows('','*','*').
  follows('','+','+').
  follows('',',',',').
  follows('','-','-').
  follows('','.','.').
  follows('','/','/').
  follows('','|','|').
  follows('','~','~').
  follows('','\\','\\').
  follows('','^','^').
  follows('',':',':').
  follows('','<','<').
  follows('','=','=').
  follows('','>','>').
  follows('','?','?').
  follows('','@','@').
  follows('','!','!').
  follows('','•','•').
  follows('','#','#').
  follows('','$','$').
  follows('%','%','%%').
  follows('&','&','&&').
  follows('*','*','**').
  follows(',','.',',.').
  follows(',.','.',',..').
  follows('-','-','--').
  follows('-','>','->').
  follows('--','>','-->').
  follows('->','>','->>').
  follows('.','#','.#').
  follows('.','&','.&').
  follows('.','|','.|').
  follows('.','~','.~').
  follows('.','<','.<').
  follows('.','^','.^').
  follows('.','>','.>').
  follows('.',' ','. ').
  follows('.#','.','.#.').
  follows('.&','.','.&.').
  follows('.|','.','.|.').
  follows('.~','.','.~.').
  follows('.<','<','.<<').
  follows('.<<','.','.<<.').
  follows('.^','.','.^.').
  follows('.>','>','.>>').
  follows('.>>','.','.>>.').
  follows('.>>','>','.>>>').
  follows('.>>>','.','.>>>.').
  follows('/','/','//').
  follows('//','/','///').
  follows('|',':','|:').
  follows('|','|','||').
  follows('~','~','~~').
  follows('~','>','~>').
  follows('\\','+','\\+').
  follows('\\','=','\\=').
  follows('\\=','=','\\==').
  follows('^','/','^/').
  follows('^/','/','^//').
  follows(':',':','::').
  follows(':','=',':=').
  follows('::','=','::=').
  follows('<','~','<~').
  follows('<','<','<<').
  follows('<','=','<=').
  follows('<=','>','<=>').
  follows('=','<','=<').
  follows('=','=','==').
  follows('=','>','=>').
  follows('>','=','>=').
  follows('>','>','>>').
  follows('>>','=','>>=').


  final('%',"%").	 /* modulo */
  final('%%',"%%").	 /* grammar parse */
  final('&&',"&&").	 /* conjunction */
  final('*',"*").	 /* multiplication */
  final('**',"**").	 /* exponentiation */
  final('+',"+").	 /* lookahead in grammar rule */
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
  final('.~.',".~.").	 /* bitwise 1's complement */
  final('.<<.',".<<.").	 /* shift left */
  final('.^.',".^.").	 /* bitwise xor */
  final('.>>.',".>>.").	 /* logical shift right */
  final('.>>>.',".>>>.").	 /* arithmetic shift right */
  final('. ',". ").	 /* statement terminator */
  final('/',"/").	 /* division */
  final('//',"//").	 /* map over */
  final('///',"///").	 /* indexed map over */
  final('|',"|").	 /* type union and conditional */
  final('|:',"|:").	 /* constrained type */
  final('||',"||").	 /* disjunction */
  final('~',"~").	 /* grammar remainder */
  final('~~',"~~").	 /* quantifier */
  final('~>',"~>").	 /* type alias definition */
  final('\\+',"\\+").	 /* logical negation */
  final('\\==',"\\==").	 /* not equals */
  final('^/',"^/").	 /* filter */
  final('^//',"^//").	 /* filter map */
  final(':',":").	 /* type annotation */
  final('::',"::").	 /* type coercion */
  final('::=',"::=").	 /* algebraic type definition */
  final(':=',":=").	 /* reassignable variable definition */
  final('<',"<").	 /* less than */
  final('<~',"<~").	 /* type rule */
  final('<<',"<<").	 /* string formatting */
  final('<=',"<=").	 /* pattern arrow */
  final('<=>',"<=>").	 /* constructor arrow */
  final('=',"=").	 /* definition */
  final('=<',"=<").	 /* less than or equal */
  final('==',"==").	 /* equality predicate */
  final('=>',"=>").	 /* function arrow */
  final('>',">").	 /* greater than */
  final('>=',">=").	 /* greater than or equal */
  final('>>=',">>=").	 /* monadic bind */
  final('?',"?").	 /* conditional operator */
  final('@',"@").	 /* meta annotation */
  final('!',"!").	 /* pick up a value from a ref cell */
  final('•',"•").	 /* function composition */
  final('#',"#").	 /* Macro statement marker */
  final('$',"$").	 /* Used for curried functions and types */

