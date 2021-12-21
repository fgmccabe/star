test.al{
  import star.
  import star.script.

  person[a] ::= someOne{
    friend:a.
    name:string.
    age:(integer)=>integer
  } |
    private student{
      name:string.
    } |
    employee{
      name:string.
      friend:a
    }

  Pete  = someOne{
    name = "peter".
    friend = 23.
    age(X) => X+30
    }

  contract all t,e ~~ '$name'[t->>e] ::= {
    '$name':(t)=>e
  }

  implementation all a ~~ '$name'[person[a]->>string] => {
    '$name':(person[a])=>string.
    '$name'(P) => P.name.
  }
  
  main:()=>action[(),()].
  main()=>action{
    assert Pete.name=="peter";
    assert '$name'(Pete)=="peter";
    assert Pete.age(3)==33
  }
}
