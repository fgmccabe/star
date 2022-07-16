test.ac9{
  import star.
  import star.script.

  large:(cons[integer])=>integer.
  large(idxes) => valof {
    count = ref 0;
    for i in idxes do {
      count := count!+i
    };

    for ix in idxes do {
      count := count!-ix
    };

    valis count!
  }

  main:()=>().
  main()=>valof{
    assert large([1,2,3,4])==0;
    valis ()
  }
}  
