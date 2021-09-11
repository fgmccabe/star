star.tuples{
  import star.core.
  import star.arith.
  import star.cons.
  import star.coerce.

  -- Some basic stuff for tuples
  -- Zero-tuples
  public implementation display[()] => {
    disp(_) => "()".
  }

  public implementation equality[()] => {
    () == () => .true.
  }

  public implementation hash[()] => {
    hash(()) => 0.
  }

  -- 2-tuples
  public implementation all x,y ~~ display[x], display[y] |: display[(x,y)] => {.
    disp((a,b)) => "($(a),$(b))".
  .}

  public implementation all x,y ~~ equality[x], equality[y] |: equality[(x,y)] => {.
    (A1,A2)==(B1,B2) => A1==B1 && A2==B2.
  .}

  public implementation all x,y ~~ hash[x], hash[y] |: hash[(x,y)] => {.
    hash((A,B)) => hash(A)*37+hash(B).
  .}

  public fst:all x,y ~~ ((x,y)) => x.
  fst((X,_)) => X.

  public snd:all x,y ~~ ((x,y)) => y.
  snd((_,Y)) => Y.

  -- 3-tuples
  public implementation all x,y,z ~~ display[x], display[y], display[z] |: display[(x,y,z)] => {.
    disp((a,b,c)) => "($(a),$(b),$(c))".
  .}

  public implementation all x,y,z ~~ equality[x], equality[y],equality[z] |: equality[(x,y,z)] => {.
    (A1,A2,A3)==(B1,B2,B3) => A1==B1 && A2==B2 && A3==B3.
  .}

  public implementation all x,y,z ~~ hash[x], hash[y],hash[z] |: hash[(x,y,z)] => {.
    hash((A,B,C)) => (hash(A)*37+hash(B))*37+hash(C).
  .}

  -- 4-tuples
  public implementation all x,y,z,w ~~ display[w], display[x], display[y], display[z] |: display[(w,x,y,z)] => {.
    disp((a,b,c,d)) => "($(a),$(b),$(c),$(d))".
  .}

  public implementation all w,x,y,z ~~ equality[w], equality[x], equality[y],equality[z] |: equality[(w,x,y,z)] => {.
    (A1,A2,A3,A4)==(B1,B2,B3,B4) => A1==B1 && A2==B2 && A3==B3 && A4==B4.
  .}

  public implementation all w,x,y,z ~~ hash[w], hash[x], hash[y],hash[z] |: hash[(w,x,y,z)] => {.
    hash((A,B,C,D)) => ((hash(A)*37+hash(B))*37+hash(C))*37+hash(D).
  .}

  -- 5-tuples
  public implementation all u,x,y,z,w ~~ display[u],display[w], display[x], display[y], display[z] |: display[(u,w,x,y,z)] => {.
    disp((a,b,c,d,e)) => "($(a),$(b),$(c),$(d),$(e))".
  .}

  public implementation all u,w,x,y,z ~~ equality[u], equality[w], equality[x], equality[y],equality[z] |: equality[(u,w,x,y,z)] => {.
    (A1,A2,A3,A4,A5)==(B1,B2,B3,B4,B5) =>
      A1==B1 && A2==B2 && A3==B3 && A4==B4 && A5==B5.
  .}

  public implementation all u,w,x,y,z ~~ hash[u], hash[w], hash[x], hash[y],hash[z] |: hash[(u,w,x,y,z)] => {.
    hash((A,B,C,D,E)) => (((hash(A)*37+hash(B))*37+hash(C))*37+hash(D))*37+hash(E).
  .}

  public all K,V ~~ keyval[K,V] ::= (->)(K,V).
  
  public implementation all k,v ~~ display[k],display[v] |:
    display[keyval[k,v]] => {.
      disp(K->V) => "$(K) -> $(V)"
    .}.

  public implementation all k,v ~~ equality[k],equality[v] |:
    equality[keyval[k,v]] => {.
      K1->V1 == K2->V2 => K1==K2 && V1==V2
    .}.
}
