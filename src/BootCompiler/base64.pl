:- module(base64, [encode64/3,decode64/3]).

:- use_module(misc).


encode64([H,M,L|R],O,Ox) :-
  Word is (H<<16)\/(M<<8)\/L,
  encodeWord(Word,O,O1),
  encode64(R,O1,Ox).
encode64([H,M],O,Ox) :-
  W is (H<<16)\/(M<<8),
  mapToSix(W,Up,Hi,Md,_),
  encLast(Up,Hi,Md,O,['='|Ox]).
encode64([H],O,Ox) :-
  W is (H<<16),
  mapToSix(W,Up,Hi,Md,_),
  encLast(Up,Hi,Md,O,['=','='|Ox]).
encode64([],O,O).

encLast(Up,0,0,[U|O],O) :-!,
  encByte(Up,U),!.
encLast(Up,Hi,0,[U,H|O],O) :-!,
  encByte(Up,U),
  encByte(Hi,H),!.
encLast(Up,Hi,Md,[U,H,M|O],O) :-!,
  encByte(Up,U),
  encByte(Hi,H),
  encByte(Md,M),!.

mapToSix(W,Up,Hi,Md,Lw) :-
  Up is (W>>18)/\63,
  Hi is (W>>12)/\63,
  Md is (W>>6)/\63,
  Lw is (W)/\63.

encodeWord(W,[U,H,M,L|O],O) :-
  mapToSix(W,Up,Hi,Md,Lw),
  encByte(Up,U),
  encByte(Hi,H),
  encByte(Md,M),
  encByte(Lw,L).

decode64([U,H,'=','='],[B1|O],O) :-
  encByte(Up,U),
  encByte(Hi,H),
  W is (Up<<18)\/(Hi<<12),
  B1 is (W>>16)/\255.
decode64([U,H,M,'='],[B1,B2|O],O) :-
  encByte(Up,U),
  encByte(Hi,H),
  encByte(Md,M),
  W is (Up<<18)\/(Hi<<12)\/(Md<<6),
  B1 is (W>>16)/\255,
  B2 is (W>>8)/\255.
decode64([U,H,M,L|Rest],[B1,B2,B3|O],Ox) :-
  encByte(Up,U),
  encByte(Hi,H),
  encByte(Md,M),
  encByte(Lw,L),!,
  W is (Up<<18)\/(Hi<<12)\/(Md<<6)\/(Lw),
  B1 is (W>>16)/\255,
  B2 is (W>>8)/\255,
  B3 is W/\255,
  decode64(Rest,O,Ox).
decode64([],O,O).

encByte(0,'A').
encByte(1,'B').
encByte(2,'C').
encByte(3,'D').
encByte(4,'E').
encByte(5,'F').
encByte(6,'G').
encByte(7,'H').
encByte(8,'I').
encByte(9,'J').
encByte(10,'K').
encByte(11,'L').
encByte(12,'M').
encByte(13,'N').
encByte(14,'O').
encByte(15,'P').
encByte(16,'Q').
encByte(17,'R').
encByte(18,'S').
encByte(19,'T').
encByte(20,'U').
encByte(21,'V').
encByte(22,'W').
encByte(23,'X').
encByte(24,'Y').
encByte(25,'Z').
encByte(26,'a').
encByte(27,'b').
encByte(28,'c').
encByte(29,'d').
encByte(30,'e').
encByte(31,'f').
encByte(32,'g').
encByte(33,'h').
encByte(34,'i').
encByte(35,'j').
encByte(36,'k').
encByte(37,'l').
encByte(38,'m').
encByte(39,'n').
encByte(40,'o').
encByte(41,'p').
encByte(42,'q').
encByte(43,'r').
encByte(44,'s').
encByte(45,'t').
encByte(46,'u').
encByte(47,'v').
encByte(48,'w').
encByte(49,'x').
encByte(50,'y').
encByte(51,'z').
encByte(52,'0').
encByte(53,'1').
encByte(54,'2').
encByte(55,'3').
encByte(56,'4').
encByte(57,'5').
encByte(58,'6').
encByte(59,'7').
encByte(60,'8').
encByte(61,'9').
encByte(62,'+').
encByte(63,'/').
