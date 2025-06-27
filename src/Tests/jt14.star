test.jt14{
  import star.
  import star.assert.

  all a ~~ tree[a] ::= .empty | .node(tree[a],a,tree[a]).

  dispTree:all e~~(tree[e],(e)=>string)=>string.
  dispTree(Tr,Pr) => let{.
    dTree(.empty) => "e".
    dTree(.node(L,Lb,R)) => "<#(dTree(L))#(Pr(Lb))#(dTree(R))>".
  .} in dTree(Tr).

  splits:(tree[(integer,string)]) => (tree[integer],cons[string]).
  splits(T) => let{.
    spl(.empty) => valof{ try{_gc(64)} catch {_ => logM("gc raised error")}; valis (.empty,.nil)}.
    spl(.node(L,(X,S),R)) => valof{
      (LL,LX) = spl(L);
      (RR,RX) = spl(R);
      valis (.node(LL,X,RR),concat(LX,.cons(S,RX)))
    }
  .} in spl(T).

  conc:(string,string)=>string.
  conc(A,B) => _str_concat(A,B).

  concat:all e ~~ (cons[e],cons[e])=>cons[e].
  concat(.nil,Y) => Y.
  concat(.cons(E,X),Y) => .cons(E,concat(X,Y)).

  dInt:(integer)=>string.
  dInt(X) => _int2str(X).

  logM:(string)=>().
  logM(M) => valof{
    try{
      _logmsg(M)
    } catch {_ => {}};
    valis ()
  }

  main:()=>().
  main()=> valof{
    (Itree,Stree) = splits(.node(.node(.empty,(1,"a"),.empty),(2,"b"),.empty));
    show dispTree(Itree,dInt);

    try{
      _jit_compile("#(__pkg__)@dispTree",2);
      _jit_compile("#(__pkg__)@splits",1);
      _jit_compile("#(__pkg__)@Γ_1@spl",2);
      _jit_compile("#(__pkg__)@dInt",1);
      _jit_compile("#(__pkg__)@dInt^",2);
      _jit_compile("#(__pkg__)@Γ_0@dTree",2);
      _jit_compile("#(__pkg__)@concat",2);
      _jit_compile("#(__pkg__)@conc",2);
    } catch {
      X => showMsg("$(X)")
    };
    logM(dispTree(Itree,dInt));

    logM(dispTree(splits(.node(.node(.empty,(1,"a"),.empty),(2,"b"),.empty)).0,dInt));

    valis ()
  }
}
