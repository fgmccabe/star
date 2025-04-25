test.ex0{
  except ::= .except(string).

  main:()=>().
  main()=>valof{
    try{
      throw .except("bong");
    } catch {
      .except(Msg) => { _logmsg("out with a #(Msg)"); valis () }
    };
  }
}
