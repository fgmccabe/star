star.manifest{
  import star.
  import star.parse.
  import star.json.
  import star.repo.
  import star.uri.

  public manifest ::= man(uri,map[string,pEntry]).

  pEntry ::= pEntry(string,list[(version,mInfo)]).

  mInfo ::= mInfo(version,map[string,string]).

  fromJson:(uri,json) => manifest.
  fromJson(base,jColl(M)) => man(base,M///jsonEntry).

  jsonEntry:(string,json) => pEntry.
  jsonEntry(P,jColl(Vs)) => pEntry(P,foldMap(pickEntry,[],Vs)).

  pickEntry:(string,json,list[(version,mInfo)]) => list[(version,mInfo)].
  pickEntry(V,E,M) => [(V::version,jsonInfo(V,E)),..M].

  jsonInfo:(string,json) => mInfo.
  jsonInfo(V,jColl(I)) => mInfo(V::version,foldMap(pickInfo,[],I)).

  pickInfo:(string,json,map[string,string]) => map[string,string].
  pickInfo(K,jTxt(V),M) => M[K->V].

  public implementation repo[manifest] => {
    hasResource(man(_,M),pkg(P,V),K) where
      present(M,P) =. some(pEntry(_,Vs)) =>
        hasCompatibleVersion(Vs,V,K).
    hasResource(_,_,_) => none.

    hasCompatibleVersion([],_,_) => none.
    hasCompatibleVersion([(Vr,mInfo(_,I)),.._],Vq,K) where
      compatibleVersion(Vr,Vq) => present(I,K).
    hasCompatibleVersion([_,..Vs],Vq,K) => hasCompatibleVersion(Vs,Vq,K).

    baseUri(man(U,_)) => U.
  }



}
