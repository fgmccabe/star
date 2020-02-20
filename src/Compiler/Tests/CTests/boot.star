star.boot{
  -- special boot for testing in this directory

  import test.fl.
  import test.ls.

  public __boot:()=>().
  __boot() where 
    _ .= _logmsg("Run fl:") &&
      _ .= ff(10) => ().
}
