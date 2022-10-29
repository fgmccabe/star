test.c0a{
  import test.c0.

  fact:(integer)=>integer.
  fact(X) => valof{
    F := 1;
    I := 0;
    while I! < X do{
      F := F!*I!;
      I := I!+1;
    };
    valis F!
  }

  rcf:(integer)=>integer.
  rcf(1) => 1.
  rcf(X) => X*rcf(X-1).
}  
