test.rc2{
  import star.
  import star.script.

  import star.location.

  tipe ::= .voidType.

  typeRule ::= .tpRule(tipe,tipe).

  canon ::= .void.

  dict ~> map[string,locn].

  public tpDef ::= .tpDefn(option[locn],string,tipe,typeRule,map[string,tipe]).

  public vrEntry ::= .vrEntry(option[locn],(option[locn],dict)=>canon,tipe,option[tipe]).

  public accEntry ::= .accEntry(option[locn],string,tipe).

  public scope ::= scope{
    types:map[string,tpDef].
    vars:map[string,vrEntry].
    contracts:map[string,typeRule].
    accessors:map[string,map[string,accEntry]].
    updaters:map[string,map[string,accEntry]].
    labels:map[string,(option[locn],string)]
  }.

  main:()=>().
  main()=>valof{
    valis ()
  }
}
