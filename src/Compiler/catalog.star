star.compiler.catalog{
  import star.
  import star.json.
  import star.uri.
  import star.pkg.
  import star.resources.

  import star.compiler.misc.

  public catalog ::= catalog{
    base : uri.
    parent:option[catalog].
    vers:option[version].
    entries:map[string,string].
    subcats:cons[catalog].
  }

  public parseCat:(json,uri) => option[catalog].
  parseCat(jColl(M),U) => some(catalog{
      parent=M["default"]>>=(jTxt(Ut))=>parseUri(Ut)>>=(PU)=>loadCatalog(^resolveUri(U,PU)).
      vers=M["version"] >>= (jTxt(V)) => some(V::version).
      base=U.
      entries=deflt(M["content"]>>=(jColl(C))=>some(C///((_,jTxt(E))=>E)),()=>[]).
      subcats = deflt(M["subcatalogs"] >>=
	  (jSeq(L)) => parseSubCats(U,L,[]),()=>[]).
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
  resolveInCatalog(Cat,Pkg) where Entry ^= findInCat(Cat,Pkg) => some(Entry).
  resolveInCatalog(Cat,Pkg) where
    P ^= Cat.parent => resolveInCatalog(P,Pkg).
  resolveInCatalog(_,_) => .none.

  findInCat:(catalog,string) => option[(uri,pkg)].
  findInCat(Cat,Pkg) where
    E ^= Cat.entries[Pkg] &&
      CU ^= parseUri(E) &&
      PU ^= resolveUri(Cat.base,CU) =>
    some((PU,pkg(Pkg,deflt(Cat.vers,()=>.defltVersion)))).
  findInCat(Cat,Pkg) => findInSubs(Cat.subcats,Pkg).

  findInSubs([],_) => .none.
  findInSubs([Cat,..Cats],Pkg) where R^=findInCat(Cat,Pkg) => some(R).
  findInSubs([_,..Cats],Pkg) => findInSubs(trace("look for $(Pkg) in ",Cats),Pkg).

  public implementation display[catalog] => let{
    dispCat:(catalog)=>ss.
    dispCat(Cat) => ssSeq([
	ss("catalog: at "),
	disp(Cat.base),
	ss("\ncontent: "),
	disp(Cat.entries),
	ss("\nversion: "),
	disp(Cat.vers),
	(Parent^=Cat.parent ? ssSeq([ss("\nparent: "),dispCat(Parent)]) || ss("")),
	ss("\nsubcats: "),ssSeq(interleave(Cat.subcats//dispCat,ss(";")))
    ])
   } in {
    disp(Cat) => dispCat(Cat)
  }
}
