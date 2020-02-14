test.cmd{
  import star.
  import star.script.

  main:()=>action[(),()].
  main()=>do{
    show disp(_command_line())::string
  }
}
