test.ex0{
  except ::= .except(string).

  main:(){}.
  main(){
    try{
      throw .except("bong");
    } catch {
      .except(Msg) do { _logmsg("out with a #(Msg)") }
    };
  }
}
