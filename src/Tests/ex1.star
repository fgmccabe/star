test.ex1{
  except ::= .except(string).

  throwOne:(integer) => boolean throws except.
  throwOne(X) =>  _int_ge(X,0) ?? .true || throw .except("bing").

  main:()=>().
  main()=>valof{
    try{
      _logmsg(throwOne(-1)??"no throw"||"either");
    } catch {
      .except(Msg) => { _logmsg("out with a #(Msg)"); valis () }
    };
  }
}
