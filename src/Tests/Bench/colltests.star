test.bench.collections{
  import star.
  import star.redblack.
  import star.skew.
  import star.vector.
  import star.assert.
  import test.lib.timer.
  
  idealIota:(integer,integer)=>map[integer,integer].
  idealIota(Mx,Mx) => [].
  idealIota(Ix,Mx) where Ix<Mx => idealIota(Ix+1,Mx)[Ix->Ix].

  rbiota:(integer,integer)=>rbtree[integer,integer].
  rbiota(Mx,Mx) => [].
  rbiota(Ix,Mx) where Ix<Mx => [Ix->Ix,..rbiota(Ix+1,Mx)].

  empty:all e ~~ (e)=>().
  empty(_) => ().

  testCons:(integer)=>float.
  testCons(Count)=>timeOf((){
      idxes : cons[integer];
      idxes = iota(0, Count);

      cn_list : ref cons[integer];
      cn_list = ref iota(0,Count);

      for i in idxes do {
	El = cn_list![i]
      };
      for ix in idxes do {
	cn_list[ix] := ix + 4
      }
    }).

  testIdeal:(integer)=>float.
  testIdeal(Count)=>timeOf((){
      idxes : cons[integer];
      idxes = iota(0, Count);
      
      id_list = ref idealIota(0,Count);

      for (i->_) in (id_list!) do {
	empty(.some(i))
      };

      for i in idxes do {
	El = id_list![i];
	empty(.some(i))
      };

      for ix in idxes do {
        id_list[ix] := ix + 4
      }
    }).

  testSkew:(integer)=>float.
  testSkew(Count)=>timeOf((){
      sk_list : ref sk[integer];
      sk_list = ref iota(0,Count);

      idxes : cons[integer];
      idxes = iota(0, Count);
      
      for i in (sk_list!) do {
	empty(.some(i))
      };

      for i in idxes do {
	El = sk_list![i]
      };

      for ix in idxes do {
        sk_list[ix] := ix + 4
      }
    }).

  testRedBlack(Count)=>timeOf((){
      rb_list = ref rbiota(0,Count);

      idxes : cons[integer];
      idxes = iota(0, Count);
      
      for i->_ in (rb_list!) do {
	empty(.some(i))
      };

      for i in idxes do {
	El = rb_list![i]
      };

      for ix in idxes do {
        rb_list[ix] := ix + 4
      }
    }).

  testVector(Count)=>timeOf((){
      idxes : cons[integer];
      idxes = iota(0, Count);
      
      v = ref idxes::vect[integer];

      for i in v! do {
	empty(.some(i));
      };

      for i in idxes do {
	El = v![i]
      };

      for ix in idxes do {
        v[ix] := ix + 4
      }
    }).

  public collectionBenchTest:()=>cons[float].
  collectionBenchTest()=>
    [testCons(10000),
    testIdeal(10000),
    testSkew(10000),
    testRedBlack(10000),
    testVector(10000)].

  main:(integer){}.
  main(Count){
    showMsg("cons list time:$(testCons(Count))");
    showMsg("ideal map time:$(testIdeal(Count))");
    showMsg("skew list time:$(testSkew(Count))");
    showMsg("red/black list time:$(testRedBlack(Count))");
    showMsg("vector time:$(testVector(Count))");
  }

  public _main:(cons[string])=> integer.
  _main([]) => valof{ main(10); valis 0}.
  _main([Count]) => valof{
    try {
      main(Count::integer);
      valis 0
    } catch { _ do {
	_show("Cannot parse [#(Count)] as an integer");
	valis 1}}
  }
}
