:- module(resource,[locateResource/2,locateResource/3,openResource/2,putResource/2,
      readFile/2,writeFile/2,readStdinput/1]).

:- use_module(uri).

locateResource(URI,Chars) :-
  openResource(URI,Stream),
  read_until_eof(Stream,Chars).

locateResource(Base,Rel,Chars) :-
  resolveURI(Base,Rel,Uri),
  locateResource(Uri,Chars).

readStdinput(Chars) :-
  set_stream(user_input,eof_action(eof_code)),
  set_stream(user_input,tty(false)),
  prompt(_,''),
  read_all(user_input,Chars),!.

read_all(Str,Chars) :-
  get_char(Str,Ch),
  read_more(Str,Ch,Chars).

read_more(_,end_of_file,[]) :- !.
read_more(Str,Ch,[Ch|Rest]) :-
  get_char(Str,NCh),
  read_more(Str,NCh,Rest).
read_mode(_,_,[]) :-
  writef("read from stdin failed\n").

read_until_eof(Str,[]) :- at_end_of_stream(Str), close(Str).
read_until_eof(Str,[Ch|M]) :- get_char(Str,Ch), read_until_eof(Str,M).

putResource(absUri("file",Path,noQuery),Text) :-
  makePath(Path,Fl),
  open(Fl,write,Str),
  write(Str,Text),
  close(Str).

openResource(absUri("file",Path,noQuery),Stream) :-
  makePath(Path,Fl),
  open(Fl,read,Stream).
openResource(absUri("internal",Path,_),Stream) :-
  makePath(Path,Fl),
  open(Fl,read,Stream).

readFile(Fl,Chars) :-
  open(Fl,read,Stream),
  read_until_eof(Stream,Chars).

writeFile(Fl,Chars) :-
  open(Fl,write,Stream),
  write(Stream,Chars),
  close(Stream).
