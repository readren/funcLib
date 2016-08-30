package readren.funcLib.dataTypes

object SGen {
	/** Ejercicio 11a. Nota: el `Gen` que da esta operación ignora el size recibido por `Gen.forSize`. */
	def unit[A](a: A): SGen[A] =
		SGen { _ => Gen.unit(a) };
	def lazyUnit[A](a: => A): SGen[A] =
		SGen { _ => Gen.lazyUnit(a) };

	//Ejercicio 11b. Nota: el `Gen` que da esta operación ignora el size recibido por `Gen.forSize`.
	def choose(start: Int, stop: Int): SGen[Int] =
		SGen { _ => Gen.choose(start, stop) }

	//Ejercicio 11c. Nota: el `Gen` que da esta operación ignora el size recibido por `Gen.forSize`.
	def choose(start: Double, stop: Double): SGen[Double] =
		SGen { _ => Gen.uniform(start, stop) }

	//Ejercicio 11d. Nota: el `Gen` que da esta operación ignora el size recibido por `Gen.forSize`.
	def boolean: SGen[Boolean] =
		SGen { _ => Gen.boolean }

	//Ejercicio 11e. Nota: el `Gen` que da esta operación ignora el size recibido por `Gen.forSize`.
	def union[A](alternatives: SGen[A]*): SGen[A] = {
		choose(0, alternatives.size).flatMap(x => alternatives(x))
	}

	//Ejercicio 12: Implement a listOf combinator that doesn’t accept an explicit size. It should return an SGen instead of a Gen. The implementation should generate lists of the requested size.
	def listOf[A](g: Gen[A]): SGen[List[A]] =
		SGen { size => Gen.sequence(List.fill(size)(g)) }

	//Ejercicio 13: Define listOf1 for generating nonempty lists, and then update your specification of max to use this generator.
	def listOf1[A](g: Gen[A]): SGen[List[A]] =
		SGen { size =>
			if (size == 0) g.map(List(_))
			else Gen.sequence(List.fill(size)(g))
		}

	def word: SGen[String] =
		SGen { size => Gen.sizedWord(size) };
	

	def string: SGen[String] =
		listOf(Gen.char.map(_.toChar)) map { lc => String.valueOf(lc.toArray) }

}

case class SGen[+A](forSize: Int => Gen[A]) {

	//Ejercicio 11e
	def flatMap[B](f: A => SGen[B]): SGen[B] =
		SGen { size => forSize(size).flatMap(f andThen (_.forSize(size))) }

	//Hecho por gusto
	def map[B](f: A => B): SGen[B] =
		SGen { size => forSize(size).map(f) }

	//Ejercicio 11f
	def listOfN(gSize: Gen[Int]): SGen[List[A]] =
		SGen { size => forSize(size).listOfN(gSize) }

	def **[B](gb: SGen[B]): SGen[(A, B)] =
		this flatMap { a => gb map { b => (a, b) } }

}
