
Random is package {
/*

Derived from ghc's System.Random library:

-- Module      :  System.Random
 -- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This library deals with the common task of pseudo-random number
-- generation. The library makes it possible to generate repeatable
-- results, by starting with a specified initial random number generator,
-- or to get different results on each run by using the system-initialised
-- generator or by supplying a seed from some other source.
--
-- The library is split into two layers:
--
-- * A core /random number generator/ provides a supply of bits.
--   The class 'RandomGen' provides a common interface to such generators.
--   The library provides one instance of 'RandomGen', the abstract
--   data type 'StdGen'.  Programmers may, of course, supply their own
--   instances of 'RandomGen'.
--
-- * The class 'Random' provides a way to extract values of a particular
--   type from a random number generator.  For example, the 'Float'
--   instance of 'Random' allows one to generate random values of type
--   'Float'.
--
-- This implementation uses the Portable Combined Generator of L'Ecuyer
-- ["System.Random\#LEcuyer"] for 32-bit computers, transliterated by
-- Lennart Augustsson.  It has a period of roughly 2.30584e18.
--

-- | The class 'RandomGen' provides a common interface to random number
-- generators.
--
-- Minimal complete definition: 'next' and 'split'.
*/

contract RandomGen over %g is {

   /**
    * The 'next' operation returns an 'Int' that is uniformly distributed
    * in the range returned by 'genRange' (including both end points),
    * and a new generator.
    */
   next has type (%g) => (integer, %g);

   /**
    * The 'split' operation allows one to obtain two distinct random number
    * generators. This is very useful in functional programs (for example, when
    * passing a random number generator down to recursive calls), but very
    * little work has been done on statistically robust implementations of
    * 'split' (["System.Random\#Burton", "System.Random\#Hellekalek"]
    * are the only examples we know of).
	*/
   split has type (%g) => (%g, %g);

   /**
    * The 'genRange' operation yields the range of values returned by
    * the generator.
    *
    * It is required that:
    *
    * * If @(a,b) = 'genRange' g@, then @a < b@.
	*
    * * 'genRange' always returns a pair of defined 'Int's.
    *
    * The second condition ensures that 'genRange' cannot examine its
    * argument, and hence the value it returns can be determined only by the
    * instance of 'RandomGen'.  That in turn allows an implementation to make
    * a single call to 'genRange' to establish a generator's range, without
    * being concerned that the generator returned by (say) 'next' might have
    * a different range to the generator passed to 'next'.
    *
    * The default definition spans the full range of 'Int'.
    */
   genRange has type (%g) => (integer, integer);

   /** default method #### */
   /* genRange(_) is (-1000, 1000); */
}

/**
 * The 'StdGen' instance of 'RandomGen' has a 'genRange' of at least 30 bits.
 *
 * The result of repeatedly using 'next' should be at least as statistically
 * robust as the /Minimal Standard Random Number Generator/ described by
 * ["System.Random\#Park", "System.Random\#Carta"].
 * Until more is known about implementations of 'split', all we require is
 * that 'split' deliver generators that are (a) not identical and
 * (b) independently robust in the sense just given.
 */

type StdGen is StdGen (integer, integer);

/**
 * The function 'mkStdGen' provides an alternative way of producing an initial
 * generator, by mapping an 'Int' into a generator. Again, distinct arguments
 * should be likely to produce distinct generators.
 */

mkStdGen has type (integer) => StdGen;
mkStdGen(s) where s < 0 is mkStdGen (-s);
mkStdGen(s) default is
  valof {
    q is s / 2147483562;
    s1 is s % 2147483562;
	s2 is q % 2147483398;
    valis StdGen (s1+1, s2+1);
  };

/**
 * With a source of random number supply in hand, the 'Random' class allows the
 * programmer to extract random values of a variety of types.

 * Minimal complete definition: 'randomR' and 'random'.
 */

contract Random over %a is {
  /**
   *  Takes a range /(lo,hi)/ and a random number generator
   * /g/, and returns a random value uniformly distributed in the closed
   * interval /[lo,hi]/, together with a new generator. It is unspecified
   * what happens if /lo>hi/. For continuous types there is no requirement
   * that the values /lo/ and /hi/ are ever produced, but they may be,
   * depending on the implementation and the interval.
   */
  randomR has type (%a, %a, %g) => (%a, %g) where RandomGen over %g;

  /**
   * The same as 'randomR', but using a default range determined by the type:
   *
   * * For bounded types (instances of 'Bounded', such as 'Char'),
   *   the range is normally the whole type.
   *
   * * For fractional types, the range is normally the semi-closed interval
   * @[0,1)@.
   *
   * * For 'Integer', the range is (arbitrarily) the range of 'Int'.
   */
  random has type (%g) => (%a, %g) where RandomGen over %g;
};


mkStdRNG has type (integer) => StdGen;
mkStdRNG(o) is valof {
	ct is nanos() cast integer;
	msecs is now() cast integer;
	valis (mkStdGen (msecs * 12345 + ct));
  }

randomIvalInteger has type (integer, integer, %g) => (integer, %g) where RandomGen over %g;
randomIvalInteger(l, h, rng) where l > h is randomIvalInteger (h, l, rng)
randomIvalInteger(l, h, rng) default is
  valof {
	k is h - l + 1;
	b is 2147483561;
	n is iLogBase(b, k);

    (v, rng1) is
	 let {
	   f(0, acc, g) is (acc, g)
	   f(n1, acc, g) is
		   let {
			   (x, g1) is next(g)
		   } in
			   f(n1 - 1, x + acc * b, g1);
	 } in
	  f(n, 1, rng);
	valis (l + v % k, rng1);
  };

iLogBase has type (integer, integer) => integer
iLogBase(b, i) is
	 (i < b) ? 1 || 1 + iLogBase(b, i / b)

min32Bound is -2**32;
max32Bound is 2**32-1;
int32Range is max32Bound - min32Bound

implementation Random over integer is {
	randomR(lo, hi, g) is randomIvalInteger(lo, hi, g);
	random(g) is randomIvalInteger(min32Bound, max32Bound, g); -- had to pick something
};

randomIvalFloat has type (float, float, %g) => (float, %g) where RandomGen over %g;
randomIvalFloat(l, h, rng) where l > h is randomIvalFloat(h, l, rng)
randomIvalFloat(l, h, rng) default is
	let {
		(x, rng1) is randomIvalInteger(min32Bound, max32Bound, rng);
		scaled_x is (l+h)/2.0 + ((h-l) / (int32Range as float)) * (x as float);
	} in (scaled_x, rng1)

implementation Random over float is {
	randomR(lo, hi, g) is randomIvalFloat(lo, hi, g);
	random(g) is randomIvalFloat(0.0, 1.0, g);
};

implementation Random over boolean is {
  randomR(a, b, g) is
  	let {
  		bool2Int(false) is 0;
  		bool2Int(true) is 1;
  		int2Bool(0) is false;
  		int2Bool(_) default is true;
  		(x, g1) is randomIvalInteger(bool2Int(a), bool2Int(b), g)
  	} in (int2Bool(x), g1)
  random(g) is randomR(false, true, g)
};

implementation RandomGen over StdGen is {
	next = stdNext;
	split = stdSplit;
	genRange = stdRange;
} using {
  stdRange has type (StdGen) => (integer, integer);
  stdRange(_) is (0, 2147483562);

  /** eturns values in the range stdRange */
  stdNext has type (StdGen) => (integer, StdGen);
  stdNext (StdGen(s1, s2))  is
	valof {
	  k    is s1 / 53668;
	  s1_1  is 40014 * (s1 - k * 53668) - k * 12211;
	  s1_2 is (s1_1 < 0 ? s1_1 + 2147483563 || s1_1);

	  k1   is s2 / 52774;
	  s2_1  is 40692 * (s2 - k1 * 52774) - k1 * 3791;
	  s2_2 is (s2_1 < 0 ? s2_1 + 2147483399 || s2_1);

	  z  is s1_2 - s2_2;
	  z1 is (z < 1 ? z + 2147483562 || z);

	  valis (z1, StdGen(s1_2, s2_2));
	};

  stdSplit has type (StdGen) => (StdGen, StdGen);
  stdSplit(std) where std matches StdGen(s1, s2) is
	valof {
	  /* no statistical foundation for this! */
	  new_s1 is (s1 = 2147483562 ? 1 || s1 + 1);
	  new_s2 is (s2 = 1 ? 2147483398 || s2 - 1);
	  (_, StdGen(t1, t2)) is stdNext(std);
	  left  is StdGen(new_s1, t2);
	  right is StdGen(t1, new_s2);
	  valis (left, right);
	};
};

/**
 *
 * 1. FW #Burton# Burton and RL Page, /Distributed random number generation/,
 * Journal of Functional Programming, 2(2):203-212, April 1992.
 *
 * 2. SK #Park# Park, and KW Miller, /Random number generators -
 * good ones are hard to find/, Comm ACM 31(10), Oct 1988, pp1192-1201.
 *
 * 3. DG #Carta# Carta, /Two fast implementations of the minimal standard
 * random number generator/, Comm ACM, 33(1), Jan 1990, pp87-88.
 *
 * 4. P #Hellekalek# Hellekalek, /Don\'t trust parallel Monte Carlo/,
 * Department of Mathematics, University of Salzburg,
 * <http://random.mat.sbg.ac.at/~peter/pads98.ps>, 1998.
 *
 * 5. Pierre #LEcuyer# L'Ecuyer, /Efficient and portable combined random
 * number generators/, Comm ACM, 31(6), Jun 1988, pp742-749.
 *
 * The Web site <http://random.mat.sbg.ac.at/> is a great source of information.
*/

}
