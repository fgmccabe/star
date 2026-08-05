rdf.sparql.solution.test{
  import star.
  import star.assert.
  import rdf.triple.
  import rdf.sparql.solution.

  /* Pure algebra unit tests for rdf.sparql.solution -- no graph or parser involved, just
     hand-built mappings via [] + [K->V] insertion (the proven-safe map idiom; see
     solution.star's header comment for why {X|Y in L} comprehensions and cons[(k,v)]::map
     coercion are avoided here). */

  main:(){}.
  main(){
    Failures = ref 0;

    S = .uri("http://example.org/s").
    S2 = .uri("http://example.org/s2").
    O1 = .int(1).
    O2 = .int(2).

    Ms = emptyMapping["s"->S].
    Mso = emptyMapping["s"->S]["o"->O1].
    MsoSame = emptyMapping["s"->S]["o"->O1].
    MsoDiff = emptyMapping["s"->S]["o"->O2].
    Mo = emptyMapping["o"->O1].

    checkBool(Failures,compatible(Ms,Mo),.true,"disjoint mappings are compatible");
    checkBool(Failures,compatible(Mso,MsoSame),.true,"mappings agreeing on shared vars are compatible");
    checkBool(Failures,compatible(Mso,MsoDiff),.false,"mappings disagreeing on a shared var are not compatible");

    checkMapping(Failures,merge(Ms,Mo),Mso,"merge combines disjoint mappings");
    checkMapping(Failures,merge(Mso,MsoSame),Mso,"merge of compatible overlapping mappings keeps the shared binding");

    Om1 = [Ms,emptyMapping["s"->S2]].
    Om2 = [Mo].
    checkSolutions(Failures,join(Om1,Om2),[Mso,emptyMapping["s"->S2]["o"->O1]],
      "join cross-products then filters by compatibility");

    Om2Incompatible = [emptyMapping["s"->S2]["o"->O2]].
    checkSolutions(Failures,join(Om1,Om2Incompatible),[emptyMapping["s"->S2]["o"->O2]],
      "join drops pairs that aren't compatible");

    checkSolutions(Failures,filterSol((M)=>(V ?= M["o"] ?? V==O1 || .false),[Mso,Mo,Ms]),[Mso,Mo],
      "filterSol keeps only mappings satisfying the predicate");

    -- leftJoin: Om2Lj only compatibly matches the "s"->S row, not "s"->S2 -- that row
    -- must survive unmerged in the result.
    Om1Lj = [Ms,emptyMapping["s"->S2]].
    Om2Lj = [Mso].
    checkSolutions(Failures,leftJoin(Om1Lj,Om2Lj,(_)=>.true),[Mso,emptyMapping["s"->S2]],
      "leftJoin merges rows with a compatible match, keeps others unmerged");

    checkSolutions(Failures,unionSol([Ms],[Ms]),[Ms,Ms],"unionSol keeps duplicates (bag union)");

    checkMapping(Failures,extend(Ms,"o",O1),Mso,"extend adds a new binding");

    -- minus: sharing no variables at all is a no-op, per the spec's domain-overlap gate --
    -- this is the behaviour that makes MINUS differ from NOT EXISTS (see solution.star).
    checkSolutions(Failures,minus([Ms],[Mo]),[Ms],
      "minus is a no-op when the two sides share no variables");
    checkSolutions(Failures,minus([Mso],[Mo]),[],
      "minus removes rows with a domain-overlapping compatible match");
    checkSolutions(Failures,minus([Mso],[MsoDiff]),[Mso],
      "minus keeps rows whose overlapping vars disagree (not compatible)");

    if Failures! == 0 then{
      logMsg(.info,"all solution algebra tests passed")
    } else{
      logMsg(.severe,"$(Failures!) solution algebra test(s) failed")
    };
    assert(Failures! == 0)
  }

  checkBool(Failures,Got,Expect,Descr) => valof{
    if Got==Expect then{
      logMsg(.info,"PASS: $(Descr)");
      valis ()
    } else{
      Failures := Failures! + 1;
      logMsg(.severe,"FAIL ($(Descr)): expected $(Expect), got $(Got)");
      valis ()
    }
  }

  checkMapping(Failures,Got,Expect,Descr) => checkBool(Failures,Got==Expect,.true,Descr).

  checkSolutions(Failures,Got,Expect,Descr) => checkBool(Failures,sameBag(Got,Expect),.true,Descr).

  -- solutions are a multiset -- compare as bags (same elements, same multiplicities),
  -- not in list order, since join/leftJoin/union don't guarantee a particular order.
  sameBag:(solutions,solutions) => boolean.
  sameBag(Om1,Om2) => size(Om1)==size(Om2) && containsAll(Om1,Om2).

  containsAll:(solutions,solutions) => boolean.
  containsAll([],_) => .true.
  containsAll([M,..Rest],Om2) where Om2r ?= removeOne(M,Om2) => containsAll(Rest,Om2r).
  containsAll(_,_) default => .false.

  removeOne:(mapping,solutions) => option[solutions].
  removeOne(_,[]) => .none.
  removeOne(M,[M2,..Rest]) where M==M2 => .some(Rest).
  removeOne(M,[M2,..Rest]) where Rest2 ?= removeOne(M,Rest) => .some([M2,..Rest2]).
  removeOne(_,_) default => .none.
}
