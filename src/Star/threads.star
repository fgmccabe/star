star.thread{
  import star.core.

  public threadState ::= quiescent |
    runnable | wait_io | wait_timer |
    wait_term | wait_lock | wait_rendezvous |
    in_exclusion | dead.

  public thread <~ {}.

  

}
