test.vis{
  import star.
  import star.assert.

  do1: (integer){}.
  do1(Ix){
    show "do 1 with $(Ix)"
  }

  implementation all x ~~ visitor[cons[x]->>x] => let{.
    visitCons:(cons[x],(x){}){}.
    visitCons(.nil,_) do {}.
    visitCons(.cons(E,C),P) do {
      P(E);
      visitCons(C,P)
    }
  .} in {
    visit(L,P) do visitCons(L,P)
  }

  main:(){}.
  main(){
    visit([1,2,10,-10]|:cons[integer],do1);
    visit([3,5,7]|:cons[integer],(X){ show "$(X)" });
    visit([3,5,7]|:cons[integer],(X){ assert X > 0 })
  }
}

  
