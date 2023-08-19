test.h{
  import star.
  import star.script.
  import star.heap.

  iota:(integer) => heap[integer].
  iota(0) => [].
  iota(N) => [N,..iota(N-1)].

  leq:(integer,integer)=>boolean.
  leq(X,Y)=>X=<Y.

  main:()=>().
  main() => valof{
    HH = ([10,20,5]:heap[integer]);

    logMsg(showHeap(HH));

    show iota(20);

    ZZ := iota(50);

    mm := 0;

    while [M,..R] .= ZZ! do{
      assert M>mm!;
      mm := M;
      ZZ := R;
    };

    for M in iota(12) do{
      show M
    };
    
    valis ()
  }
}
