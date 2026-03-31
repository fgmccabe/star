test.flt{
  import star.
  import star.assert.

  fltr:all e ~~ (cons[e],(e)=>boolean)=>cons[e].
  fltr(L,F) => case L in {
    | [] => []
    | [E,..Es] where F(E) => [E,..fltr(Es,F)]
    | [_,..Es] => fltr(Es,F)
  }
  
  main:(){}.
  main(){
    show fltr([1,2,3,4,5,6,7,8],(X)=>X<5);

    assert fltr([1,2,3,4,5,6,7,8],(X)=>X<5) == [1,2,3,4];
  }
}
