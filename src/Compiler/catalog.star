star.compiler.catalog{
  import star.
  import star.json.
  import star.uri.
  import star.pkg.
  import star.resources.

  public catalog ::= catalog{
    base : uri.
    parent:option[catalog].
    vers:option[version].
    entries:map[string,string].
  }

  public parseCat:(json,uri) => option[catalog].
  parseCat(jColl(M),U) => some(catalog{
      parent=M["default"]>>=(jTxt(Ut))=>parseUri(Ut)>>=(PU)=>loadCatalog(^resolveUri(U,PU)).
      vers=M["version"] >>= (jTxt(V)) => some(V::version).
      base=U.
      entries=deflt(M["content"]>>=(jColl(C))=>some(C///((_,jTxt(E))=>E)),()=>[]).
  }).
  parseCat(_,_) => .none.

  parseSubCats:(uri,cons[json],cons[catalog]) => option[cons[catalog]].
  parseSubCats(_,[],So) => some(So).
  parseSubCats(U,[jTxt(CU),..Cs],So) => valof action{
    SC ^= (parseUri(CU) >>= (PU)=>loadCatalog(^resolveUri(U,PU)));
    valis parseSubCats(U,Cs,[SC,..So])
  }

  public loadCatalog:(uri)=>option[catalog].
  loadCatalog(U) => getResource(U) >>=(Txt)=>parseJson(Txt)>>=(J)=>parseCat(J,U).

  public resolveInCatalog:(catalog,string) => option[(uri,pkg)].
  resolveInCatalog(Cat,Pkg) where
    E ^= Cat.entries[Pkg] &&
      CU ^= parseUri(E) &&
      PU ^= resolveUri(Cat.base,CU) =>
    some((PU,pkg(Pkg,deflt(Cat.vers,()=>.defltVersion)))).
  resolveInCatalog(Cat,Pkg) where
    P ^= Cat.parent => resolveInCatalog(P,Pkg).
  resolveInCatalog(_,_) => .none.

  public implementation display[catalog] => let{
    dispCat:(catalog)=>ss.
    dispCat(Cat) => ssSeq([
	ss("catalog:\n"),
	ss("content: "),
	disp(Cat.entries),
	ss("\nversion: "),
	disp(Cat.vers),
	ss("\nparent: "),
	disp(Cat.parent),
	ss("\nbase: "),
	disp(Cat.base)
    ])
   } in {
    disp(Cat) => dispCat(Cat)
  }
}
