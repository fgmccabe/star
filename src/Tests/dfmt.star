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

    show "$(parseTime("$(today()):yyyy-mm-dd;","yyyy-mm-dd"))";

    assert parseTime("$(today()):yyyy-mm-dd;","yyyy-mm-dd") == ?today();
    valis ()
  }
}
