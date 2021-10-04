test.hndl{
  import star.
  import star.script.

  main:()=>action[(),()].
/*  main()=>action{
    try{
      X .= throw test(4);
      assert X==10
    } handle {
      test(K).(A) => K.(A+6)
    }
  }
*/
  testHdl[x,y] ::= hdlXX{
    test:((x)=>>y,integer)=>y
  }

  main()=>action{
    Tg .= tag();
    HH .= hdlXX{.
      test(K,A) => K.(A+6)
    .};
    Tg prompt {
      X .= (Tg cut K in HH.test(K,4));
      assert X==10
    }
  }


  main()=>action{
    Tg .= tag();
    HH .= hdlXX{.
      test(K,A) => K.(A+6)
    .};
    Tg prompt {
      X .= (Tg cut K in HH.test(K,4));
      assert X==10
    }
  }
  
}
      
