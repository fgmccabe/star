test.ar{
  import star.
  import star.script.

  main:()=>action[(),()].
  main()=>do{
    show "smallest integer = $(smallest:integer)";
    show "largest integer = $(largest:integer)";

    show "smallest float = $(smallest:float)";
    show "largest float = $(largest:float)"
  }
}
