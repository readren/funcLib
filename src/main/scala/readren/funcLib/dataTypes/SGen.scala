package readren.funcLib.dataTypes

import scala.language.higherKinds;
import readren.funcLib.common.Monad

abstract class SGenAlgebra[SGen[+_]] extends Monad[SGen] { self =>

	///////// primitive //////////

	def genOp[A](op: Int => Gen[A]): SGen[A];

	def listOf[A](g: Gen[A]): SGen[List[A]];
	def listOf1[A](g: Gen[A]): SGen[List[A]];
	def listOfN[A](sa:SGen[A])(gSize: Gen[Int]): SGen[List[A]]

	///////// derived implemented here /////////////

	//Ejercicio 11b. Nota: el `Gen` que da esta operación ignora el size recibido por `Gen.forSize`.
	def choose(start: Int, stop: Int): SGen[Int] =
		genOp(_ => Gen.uniform(start, stop));

	//Ejercicio 11c. Nota: el `Gen` que da esta operación ignora el size recibido por `Gen.forSize`.
	def choose(start: Double, stop: Double): SGen[Double] =
		genOp(_ => Gen.uniform(start, stop))

	//Ejercicio 11d. Nota: el `Gen` que da esta operación ignora el size recibido por `Gen.forSize`.
	def boolean: SGen[Boolean] =
		genOp(_ => Gen.boolean)

	//Ejercicio 11e. Nota: el `Gen` que da esta operación ignora el size recibido por `Gen.forSize`.
	def union[A](alternatives: SGen[A]*): SGen[A] = {
		flatMap(choose(0, alternatives.size))(x => alternatives(x))
	}

	def word: SGen[String] =
		genOp(size => Gen.sizedWord(size))

	def string: SGen[String] = {
		val genChar = Gen.map(Gen.char)(_.toChar)
		map(listOf(genChar)) { lc => String.valueOf(lc.toArray) }
	}
	
	
	implicit class SGenOps[A](sa:SGen[A]) extends MonadOps(sa) {
		def listOfN(gSize: Gen[Int]): SGen[List[A]]=
			self.listOfN(sa)(gSize);
		
		def **[B](gb: SGen[B]): SGen[(A, B)] =
			this flatMap { a => gb map { b => (a, b) } }
	}
}

object SGen extends SGenAlgebra[SGen] {
	
	def genOp[A](op: Int => Gen[A]): SGen[A] =
		size => op(size) 
	
	/** Ejercicio 11a. Nota: el `Gen` que da esta operación ignora el size recibido por `Gen.forSize`. */
	def unit[A](a: A): SGen[A] =
		_ => Gen.unit(a);
		
	def lazyUnit[A](a: => A): SGen[A] =
		_ => Gen.lazyUnit(a)

	//Ejercicio 11e
	def flatMap[A, B](sa: SGen[A])(f: A => SGen[B]): SGen[B] =
		size => Gen.flatMap(sa(size))(f.andThen(_(size)))
		
	//Hecho por gusto y eficiencia
	override def map[A, B](sa:SGen[A])(f: A => B): SGen[B] =
		size => Gen.map(sa(size))(f)
		
	//Ejercicio 11f
	def listOfN[A](sa:SGen[A])(gSize: Gen[Int]): SGen[List[A]] =
		size => Gen.GenOps(sa(size)).listOfN(gSize)
		
	//Ejercicio 12: Implement a listOf combinator that doesn’t accept an explicit size. It should return an SGen instead of a Gen. The implementation should generate lists of the requested size.
	def listOf[A](g: Gen[A]): SGen[List[A]] =
		size => Gen.sequence(List.fill(size)(g))

	//Ejercicio 13: Define listOf1 for generating nonempty lists, and then update your specification of max to use this generator.
	def listOf1[A](g: Gen[A]): SGen[List[A]] =
		size =>
			if (size == 0) Gen.map(g)(List(_))
			else Gen.sequence(List.fill(size)(g))
}
