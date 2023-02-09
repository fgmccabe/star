star.actor{
  import star.
  import star.mbox.

  public contract all a,i ~~ sa[a->>i] ::= {
    _query:all x ~~ (a,(i)=>x) => x.
    _tell:(a,(i)=>()) => ().
  }

  actorHead(S) => (this,chnl,body)=>valof{
    while .true do{
      case collect(this,chnl) in {
	.query(Q) => {
	  
	
	
