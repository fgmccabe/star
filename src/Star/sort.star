star.sort{
  import star.core.
  import star.lists.

  public sort:all e ~~ (list[e],(e,e)=>boolean) => list[e].
  sort(L,P) => mergeSort(L,P).

  mergeSort:all e ~~ (list[e],(e,e)=>boolean) => list[e].
  mergeSort([],_) => [].
  mergeSort([e],_) => [e].
  mergeSort(L,P) where (L1,L2).=split(L) => merge(mergeSort(L1,P),mergeSort(L2,P),P).

  split:all e ~~ (list[e])=>(list[e],list[e]).
  split([]) => ([],[]).
  split([e]) => ([],[e]).
  split([e1,e2,..L]) where (L1,L2) .= split(L) => ([e1,..L1],[e2,..L2]).

  merge:all e ~~ (list[e],list[e],(e,e)=>boolean) => list[e].
  merge([],L,_) => L.
  merge(L,[],_) => L.
  merge([d,..L1],[e1,..L2],P) where P(d,e1) => [d,..merge(L1,[e1,..L2],P)].
  merge(L12,[e2,..L22],P1) => [e2,..merge(L12,L22,P1)].
}
