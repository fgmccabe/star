test.w{
  import star.
  import star.sort.

  result ::= result{
    index:integer.
  }.


  compIndex:all x,y ~~ x<~{index:integer}, y<~{index:integer} |= (x,y)=>boolean.
  compIndex(X,Y) => X.index<Y.index.

  sortResults:(cons[result])=>cons[result].
  sortResults(L) => sort(L,compIndex).

  main:(){}.
  main(){
    L = [result{ index=0}, result{ index=10}, result{index=-10}].
    LL = sortResults(L)
  }
}
    
