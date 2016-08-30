package readren.funcLib.dataTypes

import readren.funcLib.util.Rng

object Gen {
	// traído desde ejer3a5
	def unit[A](a: A): Gen[A] = Gen(Transition.unit(a));
	def lazyUnit[A](a: => A): Gen[A] = Gen(Transition.lazyUnit(a));

	// traído desde ejer3a5
	def choose(start: Int, stop: Int): Gen[Int] = Gen(sample = Transition { (rng1: Rng) =>
		val (a, rng2) = rng1.nextInt
		((((a.toLong - Integer.MIN_VALUE.toLong) * (stop.toLong - start.toLong)) / (Integer.MAX_VALUE.toLong - Integer.MIN_VALUE.toLong + 1)).toInt + start, rng2)
	})

	// creada para usar en ejercicio 8. Distribución uniforme
	def uniform(start: Double, stop: Double): Gen[Double] =
		Gen(Transition { _.uniform(start, stop) });
	
	def exponential(alfa:Double):Gen[Double] =
		Gen(Transition { _.exponential(alfa) })

	// traído desde ejer3a5
	def boolean: Gen[Boolean] = Gen(choose(0, 2).sample.map {
		case 0 => false
		case 1 => true
	})

	def lowercaseChar: Gen[Int] =
		choose('a', 'z')
	def uppercaseChar: Gen[Int] =
		choose('A', 'Z')
	def letterChar: Gen[Int] =
		union(lowercaseChar, uppercaseChar)
	def digitChar: Gen[Int] =
		choose('0', '9')
	def nonSpaceChar: Gen[Int] =
		weighted((letterChar, 7), (digitChar, 2))
	def spaceChar: Gen[Int] =
		weighted((weighted((unit('\n'), 1), (unit('\t'), 2)), 1), (unit(' '), 9))
	def char: Gen[Int] =
		weighted((nonSpaceChar, 6), (spaceChar, 1));
	
	def sizedWord(size:Int):Gen[String] =
		letterChar.list(size).map( word => String.valueOf(word.toArray));
	def word(gSize:Gen[Int]):Gen[String] =
		letterChar.listOfN(gSize).map(word => String.valueOf(word.toArray));

	//Hecho para el ejercicio 12
	def sequence[A](lga: List[Gen[A]]): Gen[List[A]] =
		Gen { Transition.sequence(lga.map(_.sample)) }

	// Ejercicio 7: combines two generators of the same type into one, by pulling values from each generator with equal likelihood
	def union_[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
		Gen.boolean.flatMap { b => if (b) g1 else g2 };
	def union[A](alternatives: Gen[A]*): Gen[A] = {
		choose(0, alternatives.size).flatMap(x => alternatives(x))
	}

	// Ejercicio 8: a version of union that accepts a weight for each Gen and generates values from each Gen with probability proportional to its weight
	def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
		Gen.uniform(-g1._2, g2._2).flatMap { w =>
			if (w < 0) g1._1 else g2._1
		};
		
	// agregado por mi
	def weightedN[A](alternatives: (Double, Gen[A])*): Gen[A] = Gen {
		Transition { rng1 =>
			val (a, rng2) = rng1.weighted(alternatives);
			a.sample.run(rng2)
		}
	};

	// Hechos porque son útiles
	def oneOf[A](alternatives: A*): Gen[A] =
		choose(0, alternatives.size).map(x => alternatives(x))
	def option[A](ga: Gen[A]): Gen[Option[A]] =
		weighted((Gen.unit(None), 0.1), (ga.map(Some(_)), 0.9))
}

case class Gen[+A](sample: Transition[Rng, A]) {

	// Ejercicio 6a
	def flatMap[B](f: A => Gen[B]): Gen[B] = Gen {
		sample.flatMap { f andThen (_.sample) }
	}

	//Hecho por gusto
	def map[B](f: A => B): Gen[B] = Gen {
		sample.map(f)
	}

	def list(size: Int): Gen[List[A]] =
		Gen { Transition.sequence(List.fill(size)(sample)) }

	// Ejercicio 6b
	def listOfN(gSize: Gen[Int]): Gen[List[A]] =
		gSize.flatMap { size =>
			Gen { Transition.sequence(List.fill(size)(sample)) }
		}

	def **[B](gb: Gen[B]): Gen[(A, B)] =
		this flatMap { a => gb map { b => (a, b) } }

	//Ejercicio 10: Implement helper functions for converting Gen to SGen. You can add this as a method on Gen
	def unsized: SGen[A] = SGen { _ => this }

}
