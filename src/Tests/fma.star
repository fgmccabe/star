test.fma{
  import star.
  import star.assert.

  -- Illustrating potential for fma operation

  main:(){}.
  main(){
    dx = 0.1;
    showMsg("0.1 maps to $(dx):0.999999999999999999999999;");
    showMsg("0.1*10 is $(dx*10.0):0.999999999999999999999999;");
    
    dy = 0.1 * 10.0 - 1.0;
    showMsg("0.1 * 10 - 1 = $(dy):0.999999999999999999999999;");
  }
}
