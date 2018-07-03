:- module(uri,[parseURI/2,uri//1,
              resolveURI/3,showUri/3,getUriPath/2,makePath/2,uriPath/2,uriHash/2,
              getCWDUri/1]).

:- use_module(misc).
:- use_module(encode).
:- use_module(parseUtils).

parseURI(S,U) :-
  string_chars(S,Chrs),
  phrase(uri(U),Chrs).

uri(absUri(Scheme,Path,Query)) --> absoluteURI(Scheme,Path,Query).
uri(relUri(Path,Query)) --> relativeURI(Path,Query).

isAbsoluteURI(absUri(_,_,_)).

absoluteURI(Scheme,Path,Query) -->
    scheme(Sc), [':'], {string_chars(Scheme,Sc)},
    hierPart(Path,Query).

relativeURI(Path,Query) -->
  netPath(Path), optQuery(Query).
relativeURI(Path,Query) -->
  absolutePath(Path), optQuery(Query).
relativeURI(Path,Query) --> relativePath(Path), optQuery(Query).

scheme([C|S]) --> alpha(C), alphaStar(S).

hierPart(Path,Query) --> ( netPath(Path) ; absolutePath(Path)) , optQuery(Query).

netPath(net(A,P)) --> ['/','/'], authority(A), optAbsolutePath(P).

absolutePath(abs(P)) --> ['/'], pathSegments(P).

relativePath(rel(S)) --> pathSegments(S).

authority(A) --> server(A).

optAbsolutePath(Segs) --> absolutePath(Segs).
optAbsolutePath(empty) --> [].

stringify(S,L) :- map(L,string_chars,S).

pathSegments([Seg|Ments]) --> pathSegment(Seg), (['/'], pathSegments(Ments) ; {Ments = []}).

pathSegment(Seg) --> pchars(S,Rest), parameters(Rest), { string_chars(Seg,S) }.

parameters(P) --> parameter(P,S), parameters(S).
parameters([]) --> [].

parameter([';'|P],M) --> [';'], pchars(P,M).

pchars([C|M],R) --> unreserved(C), pchars(M,R).
pchars(['%',U,L|M],R) --> escaped(U,L), pchars(M,R).
pchars([':'|M],R) --> [':'], pchars(M,R).
pchars(['@'|M],R) --> ['@'], pchars(M,R).
pchars(['&'|M],R) --> ['&'], pchars(M,R).
pchars(['='|M],R) --> ['='], pchars(M,R).
pchars(['+'|M],R) --> ['+'], pchars(M,R).
pchars(['$'|M],R) --> ['$'], pchars(M,R).
pchars([','|M],R) --> [','], pchars(M,R).
pchars(X,X) --> [].

optServer(S) --> server(S).
optServer(noHost) --> [].

server(userHost(U,H)) --> optUserInfo(U), hostPort(H).

optUserInfo(U) --> userInfo(U), ['@'].
optUserInfo(noOne) --> [].

userInfo(user(User)) --> userStar(U), { string_chars(User,U) }.

userStar([C|S]) --> unreserved(C), userStar(S).
userStar(['%',U,L|S]) --> escaped(U,L), userStar(S).
userStar(['$'|S]) --> ['$'], userStar(S).
userStar([','|S]) --> [','], userStar(S).
userStar([';'|S]) --> [';'], userStar(S).
userStar([':'|S]) --> [':'], userStar(S).
userStar(['&'|S]) --> ['&'], userStar(S).
userStar(['='|S]) --> ['='], userStar(S).
userStar(['+'|S]) --> ['+'], userStar(S).
userStar([]) --> [].

hostPort(host(Host,Port)) --> host(H), ([':'], port(P) ; { P=[] }), {string_chars(Host,H), string_chars(Port,P) }.

host(H) --> alphaDashStar(H).

alphaStar([C|S]) --> (alpha(C) ; digit(C) ; plus(C) ; minus(C) ; dot(C)), alphaStar(S).
alphaStar([]) --> [].

alphaDashStar([C|S]) --> (alpha(C) ; digit(C) ; minus(C) ; dot(C)) , alphaDashStar(S).
alphaDashStar([]) --> [].

port([D|M]) --> digit(D), port(M).
port([]) --> [].

optQuery(query(Q)) --> ['?'], query(S), { string_chars(Q,S)}.
optQuery(noQuery) --> [].

query(Q) --> uric(Q,M), query(M).
query([]) --> [].

minus('-') --> ['-'].
plus('+') --> ['+'].
dot('.') --> ['.'].

% special level designations

uric([C|M],M) --> reserved(C) ; unreserved(C).
uric(['%',U,L|M],M) --> escaped(U,L).

reserved(C) --> [C], { reserved(C) }.

unreserved(C) --> alphanum(C).
unreserved(C) --> [C], { mark(C) }.

mark(C) --> [C], { mark(C) }.

escaped(U,L) --> ['%'], hex(U), hex(L).

delim(C) --> [C], { delim(C) }.

reserved(';').
reserved('/').
reserved('?').
reserved(':').
reserved('@').
reserved('&').
reserved('=').
reserved('+').
reserved('$').
reserved(',').

mark('-').
mark('_').
mark('.').
mark('!').
mark('~').
mark('*').
mark('''').
mark('(').
mark(')').

delim('<').
delim('>').
delim('#').
delim('%').
delim('"').

resolveURI(_,Resolve,Resolve) :-
  isAbsoluteURI(Resolve),!.
resolveURI(absUri(Scheme,Base,_),relUri(Path,Query),absUri(Scheme,NP,Query)) :-
  resolvePath(Base,Path,NP),!.

resolvePath(_,net(A,P),net(A,P)).
resolvePath(net(A,_),abs(P),net(A,P)).
resolvePath(net(A,P),rel(Segs),net(A,NP)) :-
  reverse(P,[_|R]),
  edit(Segs,P,R,NP).
resolvePath(abs(_),abs(P),abs(P)).
resolvePath(abs(B),rel(P),abs(NewPath)) :-
  reverse(B,[_|R]),
  edit(P,NP,R,NR),
  revconcat(NR,NP,NewPath).

edit(["."|Segs],Sx,R,Rx) :- edit(Segs,Sx,R,Rx).
edit([".."|Segs],Sx,[_|R],Rx) :- edit(Segs,Sx,R,Rx).
edit(Segs,Segs,R,R).

showUri(absUri(Scheme,Path,Query),O,Ox) :-
  pScheme(Scheme,O,O1),
  pHierPath(Path,O1,O2),
  pQuery(Query,O2,Ox).
showUri(relUri(Path,Query),O,Ox) :-
  pHierPath(Path,O,O1),
  pQuery(Query,O1,Ox).

pScheme(S,O,Ox) :-
  string_chars(S,C),
  concat(C,[':'|Ox],O).

pHierPath(net(A,P),['/','/'|O],Ox) :-
  pAuthority(A,O,O1),
  pPath(P,O1,Ox).
pHierPath(abs(P),['/'|O],Ox) :-
  pSegs(P,O,Ox).
pHierPath(rel(P),O,Ox) :-
  pSegs(P,O,Ox).

pAuthority(userHost(U,H),O,Ox) :-
  pUser(U,O,O1),
  pHost(H,O1,Ox).

pUser(noOne,O,O).
pUser(user(U),O,Ox) :-
  string_chars(U,Ch),
  concat(Ch,['@'|Ox],O).

pHost(host(Host,Port),O,Ox) :-
  string_chars(Host,Chrs),
  concat(Chrs,O,O1),
  pPort(Port,O1,Ox).

pPort("",O,O).
pPort(Port,[':'|O],Ox) :-
  string_chars(Port,P),
  concat(P,Ox,O).

pPath(rel(Segs),O,Ox) :-
  pSegs(Segs,O,Ox).
pPath(abs(Segs),['/'|O],Ox) :-
  pSegs(Segs,O,Ox).

pSegs([],O,O).
pSegs([Seg],O,Ox) :- !,
  string_chars(Seg,Chrs),
  concat(Chrs,Ox,O).
pSegs([Seg|More],O,Ox) :-
  string_chars(Seg,Chrs),
  concat(Chrs,['/'|O1],O),
  pSegs(More,O1,Ox).

pQuery(noQuery,O,O).
pQuery(query(Q),['?'|O],Ox) :-
  string_chars(Q,Chrs),
  concat(Chrs,Ox,O).

makePath(P,Text) :-
  pHierPath(P,O,[]),
  string_chars(Text,O).

getUriPath(absUri(_,Pth,_),Path) :-
  makePath(Pth,Path).
getUriPath(relUri(Pth,_),Path) :-
  makePath(Pth,Path).

uriPath(absUri(_,Path,_),Path).
uriPath(relUri(Path,_),Path).

uriHash(absUri(Scheme,Path,Query),Hash) :-
  stringHash(0,Scheme,H1),
  pathHash(H1,Path,H2),
  queryHash(H2,Query,H3),
  hashSixtyFour(H3,Hash).
uriHash(relUri(Path,Query),Hash) :-
  pathHash(0,Path,H1),
  queryHash(H1,Query,H2),
  hashSixtyFour(H2,Hash).

authHash(H0,userHost(Usr,Hst),H) :-
  userHash(H0,Usr,H1),
  hostHash(H1,Hst,H).

hostHash(H0,host(S,P),H) :- stringHash(H0,S,H1), portHash(H1,P,H).

portHash(H0,"",H0).
portHash(H0,P,H) :- H1 is H0*47+58, stringHash(H1,P,H).

userHash(H,noOne,H).
userHash(H0,user(U),H) :- H1 is H0*47+58, stringHash(H1,U,H).

pathHash(H0,net(A,P),H) :- authHash(H0,A,H1), pathHash(H1,P,H).
pathHash(H0,abs(S),H) :- H1 is H0*47+47, segHash(H1,S,H). %% 47 = '/'
pathHash(H0,rel(S),H) :- segHash(H0,S,H).

queryHash(H,noQuery,H).
queryHash(H0,query(Q),H) :- H1 is H0*47+63, stringHash(H1,Q,H). %% 63 = '?'

segHash(H0,[],H0).
segHash(H0,[Seg|More],Hx) :-
  stringHash(H0,Seg,H1),
  segHash(H1,More,Hx).

getCWDUri(WD) :-
  working_directory(C,C),
  atom_string(C,D),
  string_concat("file:",D,DT),
  parseURI(DT,WD).
