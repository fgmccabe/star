test.rbiota{
  import star.
  import star.redblack.
  import star.vector.
  import star.assert.

  rbiota:(integer,integer)=>rbtree[integer,integer].
  rbiota(Mx,Mx) => [].
  rbiota(Ix,Mx) where Ix<Mx => [Ix->Ix,..rbiota(Ix+1,Mx)].

  main:(integer)=>().
  main(Count)=>valof{
    rb_list = ref rbiota(0,Count);
    showMsg("red/black tree: $(rb_list!)");
--    showMsg("red/black tree: #(_stringOf(rb_list!,1000))");

    assert validRb(rb_list!);

    for i->_ in rb_list! do {
      showMsg("rb element: $(i)")
    };

    del_list := rb_list!;
    for ix in 0..<Count do{
      showMsg("delete $(2*ix)");
      del_list := del_list![~ix*2];
    };
    showMsg("half deleted tree: $(del_list!)");
    for ix in 0..<Count do{
      showMsg("delete $(2*ix+1)");
      del_list := del_list![~ix*2+1];
    };
    
    showMsg("deleted tree: #(_stringOf(del_list!,1000))");

    assert isEmpty(del_list!);
    assert validRb(del_list!);
    valis ()
  }

  public _main:(cons[string])=>().
  _main([]) => main(10).
  _main([Count]) => main(Count::integer).
  
}
    
  
