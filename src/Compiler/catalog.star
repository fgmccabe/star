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
  parseCat(jColl(M),U) => let{
    content = M["content"]>>=(jColl(C))=>some(C///((_,jTxt(E))=>E)).
    vers = M["version"]>>=(jTxt(V))=>some(V::version).
    parent = M["default"]>>=(jTxt(Ut))=>parseUri(Ut)>>=(PU)=>loadCatalog(resolveUri(U,PU)).
  } in some(catalog{.
    parent=parent.
    vers=vers.
    base=U.
    entries=deflt(content,()=>[]).
  .}).
  parseCat(_,_) => none.

  public loadCatalog:(uri)=>option[catalog].
  loadCatalog(U) => getResource(U) >>=(Txt)=>parseJson(Txt)>>=(J)=>parseCat(J,U).

  public resolveInCatalog:(catalog,string) => option[(uri,pkg)].
  resolveInCatalog(Cat,Pkg) where
    E ^= Cat.entries[Pkg] &&
    CU ^= parseUri(E) => some((resolveUri(Cat.base,CU),pkg(Pkg,deflt(Cat.vers,()=>defltVersion)))).
  resolveInCatalog(Cat,Pkg) where
    P ^= Cat.parent => resolveInCatalog(P,Pkg).
  resolveInCatalog(_,_) => none.

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
