package readren.funcLib.dataTypes

import scala.language.higherKinds
import scala.language.implicitConversions

import readren.funcLib.util.Rng
import readren.funcLib.common.Monad

abstract class GenAlgebra[Gen[+_], S] extends Monad[Gen] { self =>

	//////////// primitive combinators /////////////

	def rngOp[A](op: Rng => (A, Rng)): Gen[A];
	def run[A](gen: Gen[A])(s: S): (A, S);

	//////////// derived combinators ////////////

	def uniform(start: Int, stop: Int): Gen[Int] =
		rngOp(rng => rng.uniform(start, stop));

	def uniform(begin: Double, end: Double): Gen[Double] =
		rngOp(rng => rng.uniform(begin, end));

	def exponential(alfa: Double): Gen[Double] =
		rngOp(rng => rng.exponential(alfa));

	// traído desde ejer3a5
	def boolean: Gen[Boolean] = uniform(0, 2).map {
		case 0 => false
		case 1 => true
	}

	def lowercaseChar: Gen[Int] =
		uniform('a', 'z')
	def uppercaseChar: Gen[Int] =
		uniform('A', 'Z')
	def letterChar: Gen[Int] =
		union(lowercaseChar, uppercaseChar)
	def digitChar: Gen[Int] =
		uniform('0', '9')
	def nonSpaceChar: Gen[Int] =
		weighted2((letterChar, 7), (digitChar, 2))
	def spaceChar: Gen[Int] =
		weightedN((1, unit('\n')), (2, unit('\t')), (17, unit(' ')))
	def char: Gen[Int] =
		weighted2((nonSpaceChar, 6), (spaceChar, 1));

	def sizedWord(size: Int): Gen[String] =
		letterChar.list(size).map(word => String.valueOf(word.toArray));
	def word(gSize: Gen[Int]): Gen[String] =
		letterChar.listOfN(gSize).map(word => String.valueOf(word.toArray));

	// Ejercicio 7: combines two generators of the same type into one, by pulling values from each generator with equal likelihood
	def union_[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
		boolean.flatMap { b => if (b) g1 else g2 };
	def union[A](alternatives: Gen[A]*): Gen[A] = {
		uniform(0, alternatives.size).flatMap(x => alternatives(x))
	}

	// Ejercicio 8: a version of union that accepts a weight for each Gen and generates values from each Gen with probability proportional to its weight
	def weighted2[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
		uniform(-g1._2, g2._2).flatMap { w =>
			if (w < 0) g1._1 else g2._1
		};

	// agregado por mi
	def weightedN[A](alternatives: (Double, Gen[A])*): Gen[A] = {
		rngOp { _.weighted(alternatives: _*) } flatMap identity
		/* version anterior que usa `run` en lugar de `flatMap`
		 rngOp { rng1 =>
			val (ga, rng2) = rng1.weighted(alternatives)
			run(ga)(rng2)
		};
		*/
	};

	// Hechos porque son útiles
	def oneOf[A](alternatives: A*): Gen[A] = {
		rngOp(rng => rng.oneOf(alternatives: _*))
	}

	def option[A](ga: Gen[A]): Gen[Option[A]] =
		weighted2((unit(None), 0.1), (ga.map(Some(_)), 0.9));

	implicit class GenOps[A](ga: Gen[A]) extends MonadOps(ga) {
		def list(size: Int): Gen[List[A]] = self.sequence(List.fill(size)(ga))

		// Ejercicio 6b
		def listOfN(gSize: Gen[Int]): Gen[List[A]] =
			gSize.flatMap { size => list(size) }

		def **[B](gb: Gen[B]): Gen[(A, B)] =
			this flatMap { a => gb map { b => (a, b) } }
	};
}

////////////////// Partial Implementation ///////////////////////

/**A partial implementation of `GenAlgebra` using `StateAlgebra[S]#Transition` as the `Gen` representation. */
abstract class GenAlgebraImplWithTransition[S] extends GenAlgebra[StateAlgebra[S]#Transition, S] {
	val stateAlgebra: StateAlgebra[S] = new StateAlgebra;
	import stateAlgebra._

	def run[A](ta: Transition[A])(s: S): (A, S) =
		ta(s)

	def unit[A](a: A): Transition[A] =
		Transition.unit(a);

	def lazyUnit[A](a: => A): Transition[A] =
		Transition.lazyUnit(a);

	def flatMap[A, B](ta: Transition[A])(f: A => Transition[B]): Transition[B] =
		Transition.flatMap(ta)(f)

}

////////////////// Implementation ///////////////////////

/** Note that `Gen` is an alias of `StateAlgebra[Rng]#Transition[A]` defined in the `dataTypes` package object  */
object Gen extends GenAlgebraImplWithTransition[Rng] {
	def rngOp[A](op: Rng => (A, Rng)): Gen[A] =
		rng => op(rng);

	implicit class OpsExt[A](gen: Gen[A]) {
		//Ejercicio 10: Implement helper functions for converting Gen to SGen. You can add this as a method on Gen
		def unsized: SGen[A] = _ => gen
	}
}

object Borrame {
	val x: Gen[Int] = Gen.unit(5)
	val y: Gen[List[Int]] = Gen.GenOps[Int](x).list(2)
	import Gen.GenOps
	val z: Gen[List[Int]] = x.list(2)
}