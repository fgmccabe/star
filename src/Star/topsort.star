star.topsort{
  import star.

  topDef[o,t] ::= topDef{ orig:o. definitions: list[t]. references:list[t]. }

  topological:all o,t ~~ display[t],display[o] |:
    (list[topDef[o,t]]) => list[list[topDef[o,t]]].
  topological(Defs) => let{
    defEntry::= dE{ df:topDef[o,t]. stackRef : ref option[integer]. }

    stack : ref list[dE].
    stack := [].

    defs = Defs//((D)=>dE{. df=D. stackRef := none .}).

    groups : ref list[list[topDef[o,t]]].
    groups := [].

    index = buildIndex(Defs).

    buildIndex(Defs) => foldLeft((Inx,D) =>
      foldLeft((Index,d)=>(links^=Index[d] ? Index[d->[D,..links]] | Index[d->[D]]),Inx,D.df.definitions),
      [],Defs).

    minPoint:(integer,integer) => integer.
    minPoint(x,y) where x=<y => x.
    minPoint(_,y) default => y.
  } in sort().
}
