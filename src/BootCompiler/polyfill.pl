:- module(polyfill,[exit/1,'_int_plus'/3,'_int_minus'/3,'_int_times'/3,'_int_div'/3,'_int_mod'/3,
                    '_flt_plus'/3,'_flt_minus'/3,'_flt_times'/3,'_flt_div'/3,'_flt_mod'/3,
                    '_int_abs'/2,'_flt_abs'/2,
                    explode/2, implode/2,'_stringOf'/4,
                    '_str_find'/4, '_sub_str'/4, '_str_split'/4, '_str_concat'/3, '_str_multicat'/2,'_str_gen'/2,'_str_len'/2,'_str_start'/2,
                    '_int_format'/3,'_flt_format'/3,
                    '_unify'/2,
                    '_isCcChar'/1,'_isCfChar'/1,'_isCnChar'/1,'_isCoChar'/1,'_isCsChar'/1,
                    '_isLlChar'/1,'_isLmChar'/1,'_isLoChar'/1,'_isLtChar'/1,'_isLuChar'/1,
                    '_isMcChar'/1,'_isMeChar'/1,'_isMnChar'/1,'_isNdChar'/1,'_isNlChar'/1,
                    '_isNoChar'/1,'_isPcChar'/1,'_isPdChar'/1,'_isPeChar'/1,'_isPfChar'/1,
                    '_isPiChar'/1,'_isPoChar'/1,'_isPsChar'/1,'_isScChar'/1,'_isSkChar'/1,
                    '_isSmChar'/1,'_isSoChar'/1,'_isZlChar'/1,'_isZpChar'/1,'_isZsChar'/1,
                    '_isLetterChar'/1,
                    '_int2str'/5,
                    '_flt2str'/6,
                    '_int2flt'/2,'_flt2int'/2,
                    '_display'/2,
                    '_str_lt'/2, '_str_ge'/2,
                    '_int_lt'/2, '_int_ge'/2,
                    '_flt_lt'/2,'_flt_ge'/2,
                    '_str2flt'/2,'_str2int'/2,
                    listify/2,
                    '_get_file'/2,
                    '_put_file'/2,
                    '_cwd'/1,
                    '_logmsg'/1,
                    '_flt_hash'/2,
                    '_str_hash'/2,
                    '_band'/3,'_bor'/3,'_bxor'/3,
                    '_blsr'/3,'_basr'/3,'_blsl'/3,
                    '_nthb'/2,
                    '_file_present'/1,'_isdir'/1,'_file_size'/2,'_file_modified'/2,'_ls'/2,
                    '_ticks'/1
                    ]).

exit(X) :- halt(X).

'_unify'(X,Y) :- unify_with_occurs_check(X,Y).

'_int_plus'(X,Y,Z) :- plus(X,Y,Z).
'_int_minus'(X,Y,Z) :- plus(Y,Z,X).
'_int_times'(X,Y,Z) :- Z is (X*Y).
'_int_div'(X,Y,Z) :- Z is div(X,Y).
'_int_mod'(X,Y,Z) :- Z is (X mod Y).

'_flt_plus'(X,Y,Z) :- Z is X+Y.
'_flt_minus'(X,Y,Z) :- Z is X-Y.
'_flt_times'(X,Y,Z) :- Z is X*Y.
'_flt_div'(X,Y,Z) :- Z is X/Y.
'_flt_mod'(X,Y,Z) :- divmod(X,Y,_,Z).
'_int_abs'(X,Y) :- Y is abs(X).
'_flt_abs'(X,Y) :- Y is abs(X).

explode(S,C) :- string_codes(S,CC), listify(CC,C).

listify([],'star.core#[]').
listify([E|L],'star.core#,..'(E,LL)) :- listify(L,LL).

implode(C,S) :- listify(L,C),string_codes(S,L).

'_str2int'(S,I) :- number_string(I,S).
'_str2flt'(S,D) :- number_string(D,S).

/* Unicode character class */

'_isCcChar'(X) :- unicode_property(X,category('Cc')). % is Other, control char
'_isCfChar'(X) :- unicode_property(X,category('Cf')). % is Other, format char
'_isCnChar'(X) :- unicode_property(X,category('Cn')). % is Other, unassigned char
'_isCoChar'(X) :- unicode_property(X,category('Co')). % is Other, private char
'_isCsChar'(X) :- unicode_property(X,category('Cs')). % is Other, surrogate char
'_isLlChar'(X) :- unicode_property(X,category('Ll')). % is Letter, lowercase char
'_isLmChar'(X) :- unicode_property(X,category('Lm')). % is Letter, modifier char
'_isLoChar'(X) :- unicode_property(X,category('Lo')). % is Letter, other char
'_isLtChar'(X) :- unicode_property(X,category('Lt')). % is Letter, title char
'_isLuChar'(X) :- unicode_property(X,category('Lu')). % is Letter, uppercase char
'_isMcChar'(X) :- unicode_property(X,category('Mc')). % is Mark, spacing char
'_isMeChar'(X) :- unicode_property(X,category('Me')). % is Mark, enclosing char
'_isMnChar'(X) :- unicode_property(X,category('Mn')). % is Mark, nonspacing char
'_isNdChar'(X) :- unicode_property(X,category('Nd')). % is Number, decimal digit
'_isNlChar'(X) :- unicode_property(X,category('Nl')). % is Number, letter char
'_isNoChar'(X) :- unicode_property(X,category('No')). % is Number, other char
'_isPcChar'(X) :- unicode_property(X,category('Pc')). % is Punctuation, connector
'_isPdChar'(X) :- unicode_property(X,category('Pd')). % is Punctuation, dash char
'_isPeChar'(X) :- unicode_property(X,category('Pe')). % is Punctuation, close char
'_isPfChar'(X) :- unicode_property(X,category('Pf')). % is Punctuation, final quote
'_isPiChar'(X) :- unicode_property(X,category('Pi')). % is Punctuation, initial quote
'_isPoChar'(X) :- unicode_property(X,category('Po')). % is Punctuation, other char
'_isPsChar'(X) :- unicode_property(X,category('Ps')). % is Punctuation, open char
'_isScChar'(X) :- unicode_property(X,category('Sc')). % is Symbol, currency char
'_isSkChar'(X) :- unicode_property(X,category('Sk')). % is Symbol, modifier char
'_isSmChar'(X) :- unicode_property(X,category('Sm')). % is Symbol, math char
'_isSoChar'(X) :- unicode_property(X,category('So')). % is Symbol, other char
'_isZlChar'(X) :- unicode_property(X,category('Zl')). % is Separator, line char
'_isZpChar'(X) :- unicode_property(X,category('Zp')). % is Separator, para char
'_isZsChar'(X) :- unicode_property(X,category('Zs')). % is Separator, space char

'_isLetterChar'(X) :- unicode_property(X,category('L')). % is letter char

'_int2str'(Ix,_,_,_,Str) :- number_string(Ix,Str).
'_flt2str'(Dx,_,_,_,_,Str) :- number_string(Dx,Str).

'_int2flt'(X,X).
'_flt2int'(X,X).

'_int_lt'(X,Y) :- X<Y.
'_int_ge'(X,Y) :- X >= Y.

'_flt_lt'(X,Y) :- X<Y.
'_flt_ge'(X,Y) :- X>=Y.

'_int_format'(Ix,_,Str) :-
  number_string(Ix,Str).

'_flt_format'(Dx,_,Str) :-
  number_string(Dx,Str).

'_display'((Ln,Col,Sz),Term) :-
  writef("@%t:%t(%t) - %w\n",[Ln,Col,Sz,Term]).

'_logmsg'(Msg) :-
  writef("%w\n",[Msg]).

'_str_lt'(S1,S2) :-
     string_codes(S1,C1),
     string_codes(S2,C2),
     codeSmaller(C1,C2),!.

codeSmaller([],[_|_]).
codeSmaller([C|_],[D|_]) :-
     C<D.
codeSmaller([C|L],[C|M]) :-
     codeSmaller(L,M).

'_str_ge'(S1,S2) :-
     string_codes(S1,C1),
     string_codes(S2,C2),
     codeGe(C1,C2),!.

codeGe([_|_],[]).
codeGe([C|_],[D|_]) :-
     C>D.
codeGe([C|L],[C|M]) :-
     codeGe(L,M).

'_str_gen'(Prefix,Str) :-
  gensym(Prefix,A),
  atom_string(A,Str).

'_str_len'(S,L) :-
  string_length(S,L).

'_str_concat'(A,B,C) :-
  string_concat(A,B,C).

'_str_multicat'(A,B) :-
  multiStrCat(A,"",B),!.

multiStrCat('star.core#[]',S,S).
multiStrCat('star.core#,..'(S,L),F,O) :-
  string_concat(F,S,F1),
  multiStrCat(L,F1,O).

'_str_find'(Src,Tgt,From,Px) :-
  sub_string(Src,Px,_,_,Tgt), Px>=From,!.

'_sub_str'(Src,Frm,Len,Sub) :-
  sub_string(Src,Frm,Len,_,Sub).

'_str_split'(Src,Px,Front,Back) :-
  sub_string(Src,0,Px,_,Front),
  sub_string(Src,Px,_,0,Back).

'_str_start'(Key,Str) :-
  string_concat(Key,_,Str).

'_get_file'(Fl,Text) :-
  open(Fl,read,Stream),
  read_until_eof(Stream,Codes),
  string_codes(Text,Codes).

read_until_eof(Str,[]) :- at_end_of_stream(Str), close(Str).
read_until_eof(Str,[Ch|M]) :- get_code(Str,Ch), read_until_eof(Str,M).

'_put_file'(Fl,Text) :-
  string_codes(Text,Codes),
  open(Fl,write,Stream),
  writeCodes(Stream,Codes),
  close(Stream).

writeCodes(_,[]).
writeCodes(Str,[Code|More]) :-
     put_code(Str,Code),
     writeCodes(Str,More).

'_cwd'(U) :-
    working_directory(C,C),
    string_concat("file:",C,U).

'_str_hash'(S,HH) :-
    stringHash(0,S,H),
    HH is H mod 18446744073709551616.

stringHash(H,Str,Hx) :-
  string_codes(Str,Codes),
  hashCodes(Codes,H,Hx).

hashCodes([],H,H).
hashCodes([C|More],H0,Hx) :-
  H1 is 37*H0+C,
  hashCodes(More,H1,Hx).

'_flt_hash'(F,H) :- H is truncate(F).

'_band'(X,Y,Z) :- Z is X/\Y.
'_bor'(X,Y,Z) :- Z is X\/Y.
'_bxor'(X,Y,Z) :- Z is X xor Y.
'_blsl'(X,Y,Z) :- Z is X<<Y.
'_basr'(X,Y,Z) :- Z is X>>Y.
'_blsr'(X,Y,Z) :- Z is (X>>Y)/\(1<<(64-Y)-1).
'_nthb'(X,Y) :- 1 is getbit(X,Y).

% File access primitives

'_file_present'(S) :- exists_file(S).

'_isdir'(D) :- exists_directory(D).

'_file_size'(F,S) :- size_file(F,S).

'_file_modified'(F,S) :- time_file(F,S).

'_ls'(D,L) :- expand_file_name(D,LL), stringify(LL,L).

stringify([],'star.core#[]').
stringify([E|L],'star.core#,..'(S,LL)) :- atom_string(E,S), stringify(L,LL).

'_stringOf'(T,_,_,S) :-
  swritef(S,'%w',[T]).

'_ticks'(X) :- get_time(X).
