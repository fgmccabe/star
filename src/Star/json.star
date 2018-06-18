star.json{
  import star.
  import star.parse.

  public json ::=
    jTrue | jFalse | jNull |
    jTxt(string) | jColl(map[string,json]) | jSeq(list[json]) |
    jNum(float).


  public implementation display[json] => {
    disp(j) => dispJson(j,0).
  }

  private dispJson:(json,integer) => ss.
  dispJson(jTrue,_) => ss("true").
  dispJson(jFalse,_) => ss("false").
  dispJson(jNull,_) => ss("null").
  dispJson(jTxt(T),_) => disp(T).
  dispJson(jNum(D),_) => disp(D).
  dispJson(jColl(M),Sp) => ssSeq([sc(0c{),ssSeq(dispColl(pairs(M),Sp+2,"")),sc(0c})]).
  dispJson(jSeq(L),Sp) => ssSeq([sc(0c[),ssSeq(dispSeq(L,Sp,"")),sc(0c])]).

  dispColl:(list[(string,json)],integer,string) => list[ss].
  dispColl([],_,_) => [].
  dispColl([(f,e),..l],Sp,s) => [ss(s),break(Sp),disp(f),ss(":"),dispJson(e,Sp),..dispColl(l,Sp,",")].

  dispSeq:(list[json],integer,string) => list[ss].
  dispSeq([],_,_) => [].
  dispSeq([e,..l],Sp,s) => [ss(s),dispJson(e,Sp),..dispSeq(l,Sp,",")].

  break:(integer) => ss.
  break(0) => ssSeq([]).
  break(X) => let{
    spaces:(integer) => list[ss].
    spaces(0) => [].
    spaces(X) => [ss(" "),..spaces(X-1)].
  } in ssSeq([ss("\n"),..spaces(X)]).

  public implementation equality[json] => {
    T1 == T2 => equalJson(T1,T2).
  }

  equalJson:(json,json)=>boolean.
  equalJson(jTrue,jTrue) => true.
  equalJson(jFalse,jFalse) => true.
  equalJson(jNull,jNull) => true.
  equalJson(jTxt(S1),jTxt(S2)) => S1==S2.
  equalJson(jNum(D1),jNum(D2)) => D1==D2.
  equalJson(jColl(C1),jColl(C2)) => C1==C2.
  equalJson(jSeq(L1),jSeq(L2)) => L1==L2.
  equalJson(_,_) => false.

  public pJson:parser[integer,json].
  pJson --> spaces, jP.

  jP:parser[integer,json].
  jP --> "true" ^^ jTrue ||
         "false" ^^ jFalse ||
         "null" ^^ jNull ||
         (F<-real ^^ jNum(F)) ||
         (S<-string ^^ jTxt(S)) ||
         pSeq ||
         pColl.

  string:parser[integer,string].
  string --> [0c"], T<-strchr*, [0c"] ^^ T::string.

  strchr:parser[integer,integer].
  strchr --> [0c\\], _item || _item.

  pSeq:parser[integer,json].
  pSeq --> [0c[], S<- sepby(pJson,skip(_str(","))), [0c]] ^^ jSeq(S).

  pEntry:parser[integer,(string,json)].
  pEntry --> spaces, K<-string, spaces, ":", spaces, V<-jP ^^ (K,V).

  pColl:parser[integer,json].
  pColl --> "{", C<-sepby(pEntry,skip(_str(","))), "}" ^^jColl(C::map[string,json]).

}
