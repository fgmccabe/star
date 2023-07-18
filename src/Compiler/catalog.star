star.compiler.catalog{
  import star.
  import star.file.
  import star.json.
  import star.uri.
  import star.pkg.
  import star.resources.

  import star.compiler.misc.

  public catalog ::= catalog{
    base : uri.
    parent:option[catalog].
    vers:option[version].
    content:map[string,string].
    subcats:cons[catalog].
  }

  public parseCat:(json,uri) => option[catalog].
  parseCat(.jColl(M),U) => .some(catalog{
      parent=M["default"]>>=(.jTxt(Ut))=>parseUri(Ut)>>=(PU)=>
	(RU?=resolveUri(U,PU) ?? loadCatalog(RU) || .none).
      vers=M["version"] >>= (.jTxt(V)) => .some(V::version).
      base=U.
      content=deflt(M["content"]>>=(.jColl(C))=>.some(C///((_,.jTxt(E))=>E)),()=>[]).
      subcats = deflt(M["subcatalogs"] >>=
	  (.jSeq(L)) => parseSubCats(U,L,[]),()=>[]).
  }).
  parseCat(_,_) => .none.

  parseSubCats:(uri,cons[json],cons[catalog]) => option[cons[catalog]].
  parseSubCats(_,[],So) => .some(So).
  parseSubCats(U,[.jTxt(CU),..Cs],So) => valof{
    SC = _optval((parseUri(CU) >>= (PU)=>
	  (RU?=resolveUri(U,PU) ?? loadCatalog(RU) || .none)));
    valis parseSubCats(U,Cs,[SC,..So])
  }

  public loadCatalog:(uri)=>option[catalog].
  loadCatalog(U) => getResource(U) >>=(Txt)=>parseJson(Txt)>>=(J)=>parseCat(J,U).

  public resolveInCatalog:(catalog,string) => option[(uri,pkg)].
  resolveInCatalog(Cat,Pkg) where Entry ?= findInCat(Cat,Pkg) => .some(Entry).
  resolveInCatalog(Cat,Pkg) where
    P ?= Cat.parent => resolveInCatalog(P,Pkg).
  resolveInCatalog(_,_) => .none.

  findInCat:(catalog,string) => option[(uri,pkg)].
  findInCat(Cat,Pkg) where
    E ?= Cat.content[Pkg] &&
      CU ?= parseUri(E) &&
      PU ?= resolveUri(Cat.base,CU) =>
    .some((PU,.pkg(Pkg,deflt(Cat.vers,()=>.defltVersion)))).
  findInCat(Cat,Pkg) => findInSubs(Cat.subcats,Pkg).

  findInSubs:(cons[catalog],string) => option[(uri,pkg)].
  findInSubs([],_) => .none.
  findInSubs([Cat,..Cats],Pkg) where R?=findInCat(Cat,Pkg) => .some(R).
  findInSubs([_,..Cats],Pkg) => findInSubs(Cats,Pkg).

  public implementation display[catalog] => let{.
    dispCat:(catalog)=>string.
    dispCat(Cat) => "catalog: at $(Cat.base)\ncontent: $(Cat.content)\nversion: $(Cat.vers) #(Parent?=Cat.parent ?? "\nparent: #(dispCat(Parent))" || "")\nsubcats: #(interleave(Cat.subcats//dispCat,";")*)"
  .} in {
    disp(Cat) => dispCat(Cat)
  }
}
