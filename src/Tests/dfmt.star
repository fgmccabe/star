test.dfmt{
  import star.
  import star.script.
  import star.date.

  main:()=>().
  main()=>valof{
    show today();
    show now();

    show "$(now()):wwwwwwww yyyyy-mmm-dddd HH:MMAA;";

    show "Now = $(now()):yyyy-mm-dd HH:MM:SS;";
    show "Now = $(now()):yyyy-mm-dd HH:MM:SSZZZ;";
    valis ()
  }
}
