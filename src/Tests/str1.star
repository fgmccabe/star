test.str1{
  import star.
  import star.file.
  import star.assert.

  pkgFileName:(string) => option[(string,string)].
  pkgFileName(Fl) => valof{
    if Ix?= strFind(Fl,".star",0) then{
      logMsg("Ix=$(Ix)");
      Pkg = subString(Fl,0,Ix);
      logMsg("Pk=$(Pkg)");
      Tl = subString(Fl,Ix+5,[|Fl|]);
      logMsg("Tl=$(Tl), $(Tl=="")");
      
      if Tl=="" then
	valis .some((Pkg,""))
      else if strPrefix(":",Tl) then
	valis .some((Pkg,subString(Tl,1,[|Tl|])))
      else
      valis .none
    }
    else
    valis .none
  }

  main:()=>().
  main()=>valof{
    show subString("foo.bar.star",4,5);

    show ({ Pr| Fl in ls(cwd()) && Pr ?= pkgFileName(Fl) } :cons[(string,string)]);

    valis ()
  }
}
