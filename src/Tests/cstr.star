test.cstream{
  import star.
  import star.script.

  aa = ([0]:cons[_]).

  bb = disp(aa).

  cc = disp(aa)::string.

  main:()=>action[(),()].
  main()=>do{
    show cc::string;

    show disp([0]:cons[_])::string
  }
}
