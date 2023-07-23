test.lt4{
  import star.
  import star.script.

  defn ::= .defn(string,string).

  canonDef ~> (string,integer).

  decl ::= .decl(string,string).

  implementation display[decl] => {
    disp(.decl(V,T)) => "#(V)\:#(T)"
  }

  implementation equality[decl] => {
    .decl(V1,T1)==.decl(V2,T2) => V1==V2 && T1==T2.
  }

  dict ~> cons[cons[decl]].

  checkGroup:(cons[defn],dict) => cons[decl].
  checkGroup(Specs,Env) => let{.
    checkDefs([],Dcs,Ev) => Dcs.
    checkDefs([D,..Ds],Decls,Ev) => valof{
      (Dcs) = checkDefn(D,Ev);
      
      valis checkDefs(Ds,Decls++Dcs,declareDecls(Dcs,Ev))
    }.

    redoDoWe = valof{
      valis [|Specs|]>1
    }
  .} in valof{
    if redoDoWe then{
      Dcs = checkDefs(Specs,[],Env);
      valis checkDefs(Specs,[],declareDecls(Dcs,Env))
    } else{
      valis checkDefs(Specs,[],Env)
    }
  }

  declareDecls:(cons[decl],dict)=>dict.
  declareDecls(Dcs,Ev) => [Dcs,..Ev].

  checkDefn(.defn(Nm,Ss),Ev) => [.decl(Nm,Ss)].

  isTFS("yes") => .some("yes").
  isTFS(_) default => .none.
  
  main:()=>().
  main() => valof{
    Group = [.defn("A","AA"),.defn("B","BB")];
    show checkGroup(Group,[]);

    assert {? D in ([.decl("B","BB"),.decl("A","AA")]:cons[decl]) *>D in checkGroup(Group,[]) ?};
    valis ()
  }
}
