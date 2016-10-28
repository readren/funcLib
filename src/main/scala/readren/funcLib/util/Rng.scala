package readren.funcLib.util

trait Rng {
	def nextInt: (Int, Rng)

	//Ejercicio 1
	def positiveInt: (Int, Rng) = {
		val (ni, nextRng) = nextInt
		if (ni == Int.MinValue) (0, nextRng)
		else (math.abs(ni), nextRng)
	}

	//Ejercicio 2: es una variable aleatoria con distribución uniforme en el intervalo [0,1)
	def nextDouble: (Double, Rng) = {
		val (ni, nextRng) = nextInt
		val nl = ni.toLong - Int.MinValue.toLong
		val rl = 1 + Int.MaxValue.toLong - Int.MinValue.toLong
		(nl.toDouble / rl.toDouble, nextRng)
	};

	// En el libro la llama `choose`: es una variable aleatoria discreta con distribución constante en el intervalo [begin, end). La P(s==i) == 1/(end-begin)
	def uniform(begin: Int, end: Int): (Int, Rng) = {
		val (a, rng2) = this.nextInt
		((((a.toLong - Integer.MIN_VALUE.toLong) * (end.toLong - begin.toLong)) / (Integer.MAX_VALUE.toLong - Integer.MIN_VALUE.toLong + 1)).toInt + begin, rng2)
	};

	def oneOf[A](as: A*): (A, Rng) = {
		val (i, rng2) = this.uniform(0, as.size)
		(as(i), rng2)
	};

	// Agregada por mi: es una variable aleatoria con distribución uniforme en el intervalo [begin,end). La fdp es: fdp(x) = 1/(end-begin)
	def uniform(begin: Double, end: Double): (Double, Rng) = {
		val (ni, rng) = nextInt
		(begin + (((ni.toLong - Int.MinValue.toLong) * (end - begin)) / (Int.MaxValue.toLong - Int.MinValue.toLong + 1)), rng)
	};

	/**
	 * Agregada por mi: es una variable aleatoria con distribución exponencial en el intervalo [0,infinito) con el parámetro alfa igual al recibido. La fdp es: fdp(x) = alfa * Exp(-alfa * x)  para todo x > 0
	 * Fundamento matemático: Estos son los pasos que hice en "Mathematica 5" para averiguar la función `H` que transforma una distribución uniforme en el intervalo (0,1] a una distribución exponencial en el intervalo [0, +infinito):
	 * La fdp de la distribución uniforme en intervalo (0,1] es f[x_] = 1    (Notar que se eligió el intervalo (0,1] en lugar de [0,1) para eludir la singularidad en cero que tiene la H que hallaremos luego)
	 * La fdp de la distribución exponencial es: g[y_] = alfa * Exp[-alfa * y]
	 * Según teorema 5.1 del libro de Meyer y suponiendo que y=H[x] es estrictamente decreciente:
	 * 	G[y_] == P[Y <= y] == P[H[X] <= y] == P[X >= invH[y]] == 1 - P[X < invH[y]] == 1 - F[invH[y]]  donde invH es la inversa de H, y F y G las fdas correspondientes a f y g
	 * Derivando ambos lados de la ecuación respecto de y:
	 * 	g[y_] == 0 - f[invH[y]] * invH'[y] == -invH'[y]  para todo y tal que 0 < invH[y] <= 1
	 * Integrando toda la ecuación respecto a `y` tenemos:
	 * 	Integral[g[y], y] == - Integral[invH'[y], y]  <=> Integral[alfa * Exp[-alfa * y], y] == - invH[y] <=> -Exp[-alfa*y] == -invH[y]
	 * Aplicando `H` sobre toda la ecuación tenemos:
	 *  H[Exp[-alfa*y]] == y
	 * Sustituyendo `Exp[-alfa*y]` con `x`    
	 *  H[x] == y[x] == -Log[x]/alfa
	 * Pero el generador que tenemos da numeros en el intervalo [0,1) y el que pide este desarrollo debe generar numeros en el intervalo (0,1]. Afortunadamente el generador X requerido se puede fabricar con 1-U donde U tiene distribucion uniforme en [0,1).
	 */
	def exponential(alfa: Double): (Double, Rng) = {
		assert(alfa > 0)
		val (u, rng2) = nextDouble;
		val e = -math.log(1d - u) / alfa;
		(if (java.lang.Double.isFinite(e)) e else java.lang.Double.MAX_VALUE, rng2)
	}

	//Agregado por mi
	def weighted[A](weights: (Double, A)*): (A, Rng) = {
		val weightsIterator: Iterator[(Double, A)] = weights.iterator;
		def loop(acumulator: Double): (Either[Double, A], Rng) = {
			if (weightsIterator.hasNext) {
				val (weight, a) = weightsIterator.next();
				val newAcum = weight + acumulator;
				val lr = loop(newAcum)
				lr._1 match {
					case Left(rn) =>
						if (rn >= acumulator)
							(Right(a), lr._2)
						else
							lr
					case _ => lr
				}
			} else {
				val (r, rng2) = uniform(0d, acumulator)
				(Left(r), rng2)
			}
		};
		val (e, rng2) = loop(0)
		e match {
			case Left(_) => throw new AssertionError
			case Right(a) => (a, rng2)
		}
	}

	//Ejercicio 4
	def ints(count: Int): (List[Int], Rng) = {
		if (count <= 0) (Nil, this)
		else {
			val (h, nRng) = nextInt
			val (t, fRng) = nRng.ints(count - 1)
			(h :: t, fRng)
		}
	}

	//Ejercicio 8-B
	def ints2(count: Int): (List[Int], Rng) = {
		val e0: Rng.Rand[Int] = x => x.nextInt
		val l0 = List.fill(count)(e0)
		Rng.sequence(l0)(this)
	}

	//Ejercicio 9-B
	def positiveInt2: (Int, Rng) = {
		val pepe = Rng.flatMap(_.nextInt)(x => if (x == Int.MinValue) (r => (0, r)) else (r => (x, r)))
		pepe(this)
	}
}

object Rng {
	def simple(seed: Long): Rng = new Rng {
		def nextInt = {
			val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
				((1L << 48) - 1)
			((seed2 >>> 16).asInstanceOf[Int],
				simple(seed2))
		}
		override def toString: String = "Simple(" + seed.toString + ")"
	}

	type Rand[+A] = Rng => (A, Rng)

	def unit[A](a: A): Rand[A] = rng => (a, rng)

	def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
		rng => {
			val (a, rng2) = s(rng)
			(f(a), rng2)
		};

	def int: Rand[Int] = _.nextInt
	def double: Rand[Double] = _.nextDouble;

	//Ejercicio 5
	def positiveMax(n: Int): Rand[Int] =
		map(_.positiveInt)(x => ((x.toLong * (n + 1)) / (Int.MaxValue.toLong + 1)).toInt)

	//Ejercicio 6: Use map to re-implement double in a more elegant way. See exercise 6.2.
	def double_2: Rand[Double] = {
		def f(x: Int) = (x.toLong - Int.MinValue.toLong).toDouble / (1 + Int.MaxValue.toLong - Int.MinValue.toLong).toDouble
		map[Int, Double](_.nextInt)(f)
	}

	//Ejercicio 7-A
	def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
		rng0 => {
			val (a, rng1) = ra(rng0)
			val (b, rng2) = rb(rng1)
			(f(a, b), rng2)
		}

	//Ejercicio 8-A
	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
		def loop(rem: List[Rand[A]], rng: Rng): (List[A], Rng) = {
			if (rem.isEmpty) (Nil, rng)
			else {
				val (h, nRng) = rem.head(rng)
				val (t, fRng) = loop(rem.tail, nRng)
				(h :: t, fRng)
			}
		}
		rng0 => loop(fs, rng0)
	}
	def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = rng0 => {
		fs.foldRight[(List[A], Rng)]((Nil, rng0))((ra, acc) => {
			val (a, nRng) = ra(acc._2)
			(a :: acc._1, nRng)
		})
	}

	//Ejercicio 9-A
	def flatMap[A, B](h: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
		val (a, rng2) = h(rng)
		g(a)(rng2)
	}

	// (x,y) => f(x,y) ; x => (y => f(x,y))

	//Ejercicio 10
	// Version larga (resulto metódicamente)
	//(1) map:	 rng => (f(s(rng)._1), s(rng)._2)  
	//(2) flatMap: rng => g(h(rng)._1)(h(rng)._2)
	// Entonces, para que (1)==(2)
	// h(rng) == ((s(rng)._1, s(rng)._2), s(rng)._2)							
	// g(x) = rng => (f(x._1), x._2)	
	def map_[A, B](s: Rand[A])(f: A => B): Rand[B] = {
		def h: Rand[(A, Rng)] = rng => {
			val (a, r) = s(rng)
			((a, r), r)
		}
		def g(x: (A, Rng)): Rand[B] = rng => (f(x._1), x._2)
		flatMap[(A, Rng), B](h)(g)
	}
	// Version corta (resuelto conceptualmente)
	// (1) s0.t = (a, s1) donde a == s0.t._1 y s1 == s0.t._2
	// (2) map(t)(f) = s0 => (f(a),s1)
	// (3) flatMap(t)(g) = s0 => g(a)(s1)
	// entonces, para que (2) == (3) g debe ser: g(a) = r => (f(a),r)
	def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
		flatMap(s)(a => r => (f(a), r))

	// (1a) s0.ta = (a, s1) donde a == s0.ta._1 y s1 == s0.ta._2
	// (1b) s1.tb = (b, s2) donde b == s1.tb._1 y s2 == s1.tb._2
	// (2) map2(ta, tb)(f) = s0 => (f(a,b),s2)
	// (3) flatMap(t)(g) = s0 => g(a)(s1)
	// entonces, para que (2) == (3) ...
	def map2_[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
		def h: Rand[(A, B)] = rng1 => {
			val (a, rng2) = ra(rng1)
			val (b, rng3) = rb(rng2)
			((a, b), rng3)
		}
		def g(x: (A, B)): Rand[C] = rng => (f(x._1, x._2), rng)
		flatMap[(A, B), C](h)(g)
	}

	def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
		flatMap(ra)(a => map(rb)(b => f(a, b)))

}