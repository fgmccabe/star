star.repo.manifest{
  import star.
  import star.parse.
  import star.json.
  import star.repo.

  public manifest ::= man(map[string,pEntry]).

  pEntry ::= pEntry(string,list[(version,mInfo)]).

  mInfo ::= mInfo(version,map[string,string]).

  public implementation coercion[json,manifest] => {
    _coerce(J) => fromJson(J).
  }

  fromJson:(json) => manifest.
  fromJson(jColl(M)) => man(M///jsonEntry).

  jsonEntry:(string,json) => pEntry.
  jsonEntry(P,jColl(Vs)) => pEntry(P,foldMap(pickEntry,[],Vs)).

  pickEntry:(string,json,list[(version,mInfo)]) => list[(version,mInfo)].
  pickEntry(V,E,M) => [(V::version,jsonInfo(V,E)),..M].

  jsonInfo:(string,json) => mInfo.
  jsonInfo(V,jColl(I)) => mInfo(V::version,foldMap(pickInfo,[],I)).

  pickInfo:(string,json,map[string,string]) => map[string,string].
  pickInfo(K,jTxt(V),M) => M[K->V].

  public implementation coercion[manifest,json] => {
    _coerce(M) => toJson(M).
  }

  toJson:(manifest)=>json.
  toJson(man(Ps)) => jColl(foldMap((K,pEntry(P,Vs),M) => M[K->jColl(mkVersions(Vs))],[],Ps)).

  mkVersions:(list[(version,mInfo)]) => map[string,json].
  mkVersions(Vs) => foldLeft(((V,I),M)=>M[V::string->mkEntry(I)],[],Vs).

  mkEntry:(map[string,string]) => json.
  mkEntry(M) => foldMap((K,T,MM)=>MM[K->jTxt(T)],[],M).

  public locateInManifest:(manifest,pkg,string) => option[string].
  locateInManifest(man(M),pkg(P,V),K) where
      present(M,P) =. some(pEntry(_,Vs)) =>
        hasCompatibleVersion(Vs,V,K).
  locateInManifest(_,_,_) => none.

  hasCompatibleVersion([],_,_) => none.
  hasCompatibleVersion([(Vr,mInfo(_,I)),.._],Vq,K) where
    compatibleVersion(Vr,Vq) => present(I,K).
  hasCompatibleVersion([_,..Vs],Vq,K) => hasCompatibleVersion(Vs,Vq,K).
}
