star.lex.nfa{
  import star.
  import star.location.

  import star.lex.re.

  import star.compiler.errors.
  import star.compiler.token.


  -- Labeled version of regexp tree

  ltree[f] ::= ltr{t:f. e:boolean. p:cons[integer]. l:cons[integer]}.

  lR[f] ::= .ordR(char,integer)
  | .emptyR
  | .orR(ltree[lR[f]],ltree[lR[f]])
  | .catR(ltree[lR[f]],ltree[lR[f]])
  | .starR(ltree[lR[f]])
  | .chrsR(cons[char],integer)
  | .nchrsR(cons[char],integer)
  | .finalR(ltree[lR[f]],string,integer).

  -- Construct a labelled tree from a regular expression
  mkLbld:(re[char],integer,set[char],set[char]) =>
    (ltree[lR[char]],integer,cons[(cons[char],integer)]).
  mkLbld(Re,P,Sigma,Pos) => case Re in {
    | .one(Ch) => (ltr{t=.ordR(Ch,P). e=.false. p=[P]. l=[P]},P+1,[([Ch],P),..Pos])
    | .epsilon => (ltr{t=.emptyR. e=.true. p=[]. l=[]},P+1,Pos)
    | .class(Cs) => (ltr{t=.chrsR(Cs,P). e=.false. p=[P]. l=[P]},P+1,[(Cs,P),..Pos])
    | .nclass(Cs) => (ltr{t=.nchrsR(Cs,P). e=.false. p=[P]. l=[P]},P+1,[(Cs,P),..Pos])
    | .seq(L,R) => valof{
      (Lt,P1,Lp) = mkLbld(L,P,Sigma,Pos);
      (Rt,P2,Rp) = mkLbld(R,P1,Sigma,Lp);
      valis (ltr{
	  t=.catR(Lt.t,Rt.t).
	  e=L.e&&R.e.
	  p=(L.e??L.p\/R.p||L.p).
	  l=(R.e??L.l\/R.l||R.l).
	}, P2, Rp)
    }



  mtree(Tr,P,S,pos) => { case Tr in {
  | periodRE => (tree(charsR(S\ [''\n,''\+ffff],P),false,[P],[P]),P+1,
		  [(S\ [''\n,''\+ffff],P),..pos])






  | catRE(l,r) => valof{
      (L::tree(Lval,Ln,Lpre,Llast).=L,P1,pos1) = mtree(l,P,S,pos);
      (R::tree(Rval,Rn,Rpre,Rlast).=R,P2,pos2) = mtree(r,P1,S,pos1);
      FF = { if Ln then Lpre\/Rpre else Lpre};
      LL = { if Rn then Llast\/Rlast else Rlast};
      valis (tree(catR(L,R),Ln&&Rn,FF,LL),P2,pos2)
    }
  | orRE(l,r) => valof{
      (L::tree(Lval,Ln,Lpre,Llast).=L,P1,pos1) = mtree(l,P,S,pos);
      (R::tree(Rval,Rn,Rpre,Rlast).=R,P2,pos2) = mtree(r,P1,S,pos1);
      valis (tree(orR(L,R),Ln||Rn,Lpre\/Rpre,Llast\/Rlast),P2,pos2)
    }
  | starRE(l) => valof{
      (L::tree(Lval,Ln,Lpre,Llast).=L,P1,pos1) = mtree(l,P,S,pos);
      valis (tree(starR(L),true,Lpre,Llast),P1,pos1)
    }
  | plusRE(l) => mtree(catRE(l,starRE(l)),P,S,pos)

  | optRE(l) => mtree(orRE(l,emptyRE),P,S,pos)

  | finalRE(e,v) => valof{
      (L::tree(Lval,Ln,Lpre,Llast).=L,P1,pos1) = mtree(catRE(e,ordRE(''\+feff)),P,S,pos);
      EP = { if ([''\+feff],XX) in pos1 then XX else exception failed};
      valis (tree(finalR(L,v,EP),Ln,Lpre,Llast),P1,pos1)
    }
  }};    




  /* Function to pick a set */
  L ** R => { if L==[] then R else L };


  follow(tree(E,N,F,L),V)=>
  {
    case E in {
      catR(l::tree(Lval,Ln,Lpre,Llast).=l,r::tree(Rval,Rn,Rpre,Rlast).=r) => valof {
        V1 : follow(r,follow(l,V));

        for i in Llast do{
	  if (i,VV) in V1 then
	    V1 := (V1^\ (i,_))\/[(i,VV\/Rpre)]
          else
	    V1 := [(i,Rpre),..V1]
	};
        valis V1;
      }
    | starR(l::tree(Lval,Ln,Lpre,Llast).=l) => valof{
        V1 : follow(l,V);

        for i in Llast do{
	  if (i,VV) in V1 then
	    V1 := (V1^\ (i,_))\/[(i,VV\/Lpre)]
          else
	    V1 := [(i,Lpre),..V1];
	};
        valis V1;
      }
    | orR(l,r) => follow(r,follow(l,V))
    | finalR(e,_,_) => follow(e,V)
    | _ => V
    }
  };
    
  displayTree(tree(E,N,F,L),Off,O,Root)
  {
    W = 0-(Off+16);
    case E in {
      ordR(C,i) -> {
	F^0~W++" \""++string%%C++"\":"++i^0++" "++L^0++"\n">>O
      }
    | catR(l,r) -> {
        displayTree(l,Off+2,O,false);
        if Root then
         "Root : ">>O;
        F^0~W++" . "++L^0++"\n">>O;
        displayTree(r,Off+2,O,false);
      }
    | orR(l,r) -> {
        displayTree(l,Off+2,O,false);
        if Root then
         "Root : ">>O;
        F^0~W++" | "++L^0++"\n">>O;
        displayTree(r,Off+2,O,false);
      }
    | starR(l) -> {
        displayTree(l,Off+2,O,false);
        if Root then
         "Root : ">>O;
        F^0~W++" * "++L^0++"\n">>O;
      }
    | finalR(e,f,P) -> {
        displayTree(e,Off+2,O,false);
        if Root then
         "Root : ">>O;
        F^0~W++" => ("++P^0++")"++f^0++L^0++"\n">>O;
      }
    | charsR(l,P) -> {
        if Root then
         "Root : ">>O;
	F^0~W++" ["++string%%reverse(l)++"]:"++P^0++" "++L^0++"\n">>O
      }
    | emptyR -> {}
    | OO -> {
        if Root then
         "Root : ">>O;
        "Funny tree "++OO^0++"\n">>O
      }
    };
  };










    

  public nfa[c] ::= .arrow(integer,c,integer)
  | .eps(integer,integer).

  public genNfa:all c ~~ hashable[c] |= (re[c],set[c]) => map[c,set[nfa[c]]].
  genNfa(Re,Alpha) => gen(Re,Alpha,0,0,{}).


  gen:all c ~~ hashable[c] |= (re[c],set[c],integer,integer,map[c,set[nfa[c]]]) => map[c,set[nfa[c]]].
  


}
