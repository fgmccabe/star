worksheet{

  plan(From,Goal,Next) is let{
    var Fringe := list of [(list of [],From)];
    var soFar := [];

    

    while Fringe matches [Nd,..nF] do{
      if Goal(Nd) then
        valis some(pathTo(Nd))
      else{
        soFar := [Nd,..soFar];
        Fringe := expand(Fringe,soFar,Nd,Next)
      }
    };
    valis none;
  }

}