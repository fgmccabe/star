diff is package{  
  contract hasDiff of %t is {
    diff has type (%t,%t) => ((%t)=>%t);
  }
  
  id(X) is X;
  
  K(X) is (function(_) is X);
  
  implementation hasDiff of list of %t requiring hasDiff is{
    diff=listDiff
  } using {
    listDiff(X,X) is id; 
    listDiff(list{X;..L1},list{X;..L2}) is let{
      D is listDiff(L1,L2);
      differ(list{XX;..LL}) is list{XX;..D(LL)};
    } in differ;
    listDiff(list{X1;..L1},list{X2;..L2}) where X1!=X2 is 
      let{
        H is diff(X1,X2);
        D is listDiff(L1,L2);
        differ(list{A;..B}) is list{H(A);..D(B)};
      } in differ;
  }
  
 implementation hasDiff of integer is {
    diff = intDiff
  } using {
    intDiff(X,X) is id;
    intDiff(X1,X2) default is K(X2)
  };
  
  main()
  {
    D1 is diff(list{1;3;3},list{1;2;3});
    
    logMsg(info,"d is $D1");
    logMsg(info,"apply to list{1;3;3} is $(D1(list{1;3;3}))");
    assert D1(list{1;3;3}) = list{1;2;3};
  }
}