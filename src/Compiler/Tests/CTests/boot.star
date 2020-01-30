star.boot{
  -- special boot for testing in this directory

  import test.fl.

  public __boot:()=>().
  __boot() where 
    _ .= _logmsg("Run fl:") &&
      _ .= ff(10) => ().
}
