test.ce{
  import star.
  import star.assert.

  contract all x,y,e ~~ corc[x,y->>e] ::= {
    coer:(x)=>y throws e
  }

  implementation corc[string,integer->>exception] => {
    coer(Txt) => (try _str2int(Txt) catch {_ => throw .exception("Cannot parse #(Txt) as integer")}).
  }

  implementation corc[string,float->>exception] => {
    coer(Txt) => (try _str2flt(Txt) catch {_ => throw .exception("Cannot parse #(Txt) as float")}).
  }

  implementation corc[char,integer->>void] => {
    coer(Ch) => _codePoint(Ch)
  }

  main:(){}
  main(){
    show (coer(`$`):integer);
    
    try{
      assert (coer("23"):integer)==23;
      show (coer("2.3"):float);
      show (coer("alpha"):integer);
    } catch {
      M do { show "exception $(M)" }
    };

    try{
      show (coer("2.3x"):float);
    } catch {
      M do { show "exception $(M)" }
    }
  }
}
