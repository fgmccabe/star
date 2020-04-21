test.c1{
  import star.

/*  checkGroups:(cons[cons[integer]],integer) => either[(),integer].
  checkGroups([],Ix) => either(Ix).
  checkGroups([G,..Gs],Ix) => do{
    E <- checkGroup(G,0);
    checkGroups(Gs,Ix+E)
  }

  checkGroup:(cons[integer],integer) => either[(),integer].
  checkGroup([],Ix) => either(Ix).
  checkGroup([T,..G],Ix) => do{
    Ev <- checkDefn(T);
    checkGroup(G,Ev+Ix)
  }

  checkDefn:(integer)=>either[(),integer].
  checkDefn(Ix) => do{
    valis Ix
  }
*/

  fooBar() => let{
    A = .nil.
    B = cons(A,.nil).
  } in B.

  
}


















