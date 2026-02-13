test.bench.nbody{
  import star.
  import star.assert.

  import test.lib.timer.

  PI = 3.141592653589793.
  SOLAR_MASS = 4.0 * (PI * PI).
  DAYS_PER_YEAR = 365.24.

  body ::= body{
    x : ref float.
    y : ref float.
    z : ref float.
    vx : ref float.
    vy : ref float.
    vz : ref float.
    mass : float.
  }

  create(x,y,z,vx,vy,vz,m) => body{
    x := x.
    y := y.
    z := z.
    vx := vx*DAYS_PER_YEAR.
    vy := vy*DAYS_PER_YEAR.
    vz := vz*DAYS_PER_YEAR.
    mass = m * SOLAR_MASS
  }

  offsetMomentum:(body,float,float,float){}.
  offsetMomentum(b, px, py, pz){
    try{
      b.vx := (px/SOLAR_MASS);
      b.vy := (py/SOLAR_MASS);
      b.vz := (pz/SOLAR_MASS);
    } catch {
      _ do {}
    }
  }

  jupiter = create(
    4.84143144246472090e00,
    -1.16032004402742839e00,
    -1.03622044471123109e-01,
    1.66007664274403694e-03,
    7.69901118419740425e-03,
    -6.90460016972063023e-05,
    9.54791938424326609e-04).

  saturn = create(8.34336671824457987e00,
    4.12479856412430479e00,
    -4.03523417114321381e-01,
    -2.76742510726862411e-03,
    4.99852801234917238e-03,
    2.30417297573763929e-05,
    2.85885980666130812e-04).

  uranus = create(
    1.28943695621391310e01,
    -1.51111514016986312e01,
    -2.23307578892655734e-01,
    2.96460137564761618e-03,
    2.37847173959480950e-03,
    -2.96589568540237556e-05,
    4.36624404335156298e-05).

  neptune = create(
    1.53796971148509165e01,
    -2.59193146099879641e01,
    1.79258772950371181e-01,
    2.68067772490389322e-03,
    1.62824170038242295e-03,
    -9.51592254519715870e-05,
    5.15138902046611451e-05).

  sun = create(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0).

  sqr:all e ~~ arith[e] |= (e)=>e.
  sqr(X) => X*X.

  distance:(body,body)=>float throws exception.
  distance(b,c) => sqrt(sqr(b.x!-c.x!)+sqr(b.y!-c.y!)+sqr(b.z!-c.z!)).

  setup:()=>cons[body].
  setup() => valof{
    bodies = [sun, jupiter, saturn, uranus, neptune];
    px := 0.0;
    py := 0.0;
    pz := 0.0;

    for b in bodies do{
      px += b.vx! * b.mass;
      py += b.vy! * b.mass;
      pz += b.vz! * b.mass;
    };

    offsetMomentum(sun,px!,py!,pz!);
    valis bodies
  }

  advance(system,dt){
    try{
      for ix in 0..<size(system) do{
	iB = ?system[ix];
	for jx in ix+1..<size(system) do{
	  jB = ?system[jx];
	  
	  dx = iB.x! - jB.x!;
	  dy = iB.y! - jB.y!;
	  dz = iB.z! - jB.z!;
	  
	  dSqrd = sqr(dx) + sqr(dy) + sqr(dz);
	  dist = sqrt(dSqrd);
	  mag = dt/(dSqrd*dist);

	  iB.vx -= dx*jB.mass*mag;
	  iB.vy -= dy*jB.mass*mag;
	  iB.vz -= dz*jB.mass*mag;

	  jB.vx += dx*iB.mass*mag;
	  jB.vy += dy*iB.mass*mag;
	  jB.vz += dz*iB.mass*mag;
	}
      };
      
      for b in system do{
	b.x += dt*b.vx!;
	b.y += dt*b.vy!;
	b.z += dt*b.vz!;
      }
    } catch { _ do { logMsg(.severe,"Math error") }}
  }

  energy:(cons[body])=>float.
  energy(system) => valof{
    try{
      e := 0.0;
      for ix in 0..<size(system) do{
	b = ?system[ix];
	e += 0.5*(b.mass*(sqr(b.vx!)+sqr(b.vy!)+sqr(b.vz!)));
	
	for jx in ix+1..<size(system) do{
	  jb = ?system[jx];
	  
	  e -= (b.mass*jb.mass)/distance(b,jb)
	};
      };
      valis e!;
    } catch{ _ do {
	logMsg(.severe,"Math error");
	valis 0.0
    }
    }
  }

  innerLoop(iterations) => valof{
    system = setup();
    for ix in 0..<iterations do{
      advance(system,0.01)
    };

    valis verify(energy(system),iterations)
  }

  /* Looks like we lose some precision because we don't have support for FMA
  in floating point */
  verify:(float,integer)=>boolean.
  verify(Result,Count) => case Count in {
    | 250000 => Result==-0.1690353223172673402 -- -0.1690859889909308
    | 1 => Result==-0.1690743916428782522 -- -0.16907495402506745
  }

  public nbodyBenchTest() => timeOf((){ innerLoop(250000) }).

  main:(integer){}.
  main(Cnt){
    timer = timer_start(Cnt,"N-Body benchmark");

    assert innerLoop(Cnt);

    timer_finish(timer);
  }
}
