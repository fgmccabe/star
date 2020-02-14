test.cstream{
  import star.
  import star.script.

  aa = ([0]:list[_]).

  bb = disp(aa).

  cc = disp(aa)::string.

  main:()=>action[(),()].
  main()=>do{
    show cc::string;

    show disp([0]:list[_])::string
  }
}
