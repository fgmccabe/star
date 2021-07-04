:- module(parseUtils,[iden//1,
      alpha//1,digit//1,natural//1,hex//1,alphanum//1,
      spaces//0,at_end//0]).

:- use_module(misc).

iden([F|Rest]) --> alpha(F), alphaNumStar(Rest).

alphaNumStar([C|More]) --> alphanum(C), alphaNumStar(More).
alphaNumStar([]) --> [], \+ alphanum(_).

alpha(C) --> (lowAlpha(C) ; upAlpha(C)).

lowAlpha(C) --> [C], { isLowAlpha(C) }.

upAlpha(C) --> [C], { isUpAlpha(C) }.

natural([F|R]) --> digit(F), digitStar(R).

digitStar([C|More]) --> digit(C), digitStar(More).
digitStar([]) --> [], \+ digit(_).

digit(C) --> [C], { digit(C) }.

alphanum(C) --> alpha(C) ; digit(C).

hex(C) --> digit(C).
hex(C) --> [C], { hexDigit(C) }.

alphanum(C) :- isLowAlpha(C).
alphanum(C) :- isUpAlpha(C).
alphanum(C) :- digit(C).

spaces --> space, spaces.
spaces --> \+ space.

space --> ([' '] ; ['\t']; ['\n']),!.
space --> ['-', '-'], ([' '] ; ['\t']), eol. % universal comment syntax
space --> ['/','*'], block_comment.

eol --> ['\n'].
eol --> [C], { C \= '\n'}, eol.
eol --> at_end.

block_comment --> ['*','/'].
block_comment --> ['*'], \+ ['/'], block_comment.
block_comment --> [_], block_comment.

at_end --> \+ [_].

isLowAlpha('a').
isLowAlpha('b').
isLowAlpha('c').
isLowAlpha('d').
isLowAlpha('e').
isLowAlpha('f').
isLowAlpha('g').
isLowAlpha('h').
isLowAlpha('i').
isLowAlpha('j').
isLowAlpha('k').
isLowAlpha('l').
isLowAlpha('m').
isLowAlpha('n').
isLowAlpha('o').
isLowAlpha('p').
isLowAlpha('q').
isLowAlpha('r').
isLowAlpha('s').
isLowAlpha('t').
isLowAlpha('u').
isLowAlpha('v').
isLowAlpha('w').
isLowAlpha('x').
isLowAlpha('y').
isLowAlpha('z').

isUpAlpha('A').
isUpAlpha('B').
isUpAlpha('C').
isUpAlpha('D').
isUpAlpha('E').
isUpAlpha('F').
isUpAlpha('G').
isUpAlpha('H').
isUpAlpha('I').
isUpAlpha('J').
isUpAlpha('K').
isUpAlpha('L').
isUpAlpha('M').
isUpAlpha('N').
isUpAlpha('O').
isUpAlpha('P').
isUpAlpha('Q').
isUpAlpha('R').
isUpAlpha('S').
isUpAlpha('T').
isUpAlpha('U').
isUpAlpha('V').
isUpAlpha('W').
isUpAlpha('X').
isUpAlpha('Y').
isUpAlpha('Z').

digit('0').
digit('1').
digit('2').
digit('3').
digit('4').
digit('5').
digit('6').
digit('7').
digit('8').
digit('9').

hexDigit('a').
hexDigit('b').
hexDigit('c').
hexDigit('d').
hexDigit('e').
hexDigit('f').
hexDigit('A').
hexDigit('B').
hexDigit('C').
hexDigit('D').
hexDigit('E').
hexDigit('F').