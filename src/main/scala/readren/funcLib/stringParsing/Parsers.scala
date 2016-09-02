package readren.funcLib.stringParsing

import scala.language.higherKinds;
import scala.language.implicitConversions;
import scala.language.postfixOps;
import scala.Right
import scala.util.matching.Regex

/**
 * @author Gustavo
 */
trait Parsers[Parser[+_]] { self =>
	def run[A](p: Parser[A])(input: String, offset: Int = 0): Either[ParseError, A]

	/**Always succeeds with the value `a` */
	def succeed[A](a: A): Parser[A]

	/**Recognizes and returns a single `String` */
	implicit def string(s: String): Parser[String]

	/**Promotes the received regular expression to a `Parser[String]` */
	implicit def regex(regex: Regex): Parser[String]

	/**Runs a parser, and then uses its result to select a second parser to run in sequence */
	def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B]

	/**Chooses between two parsers, first attempting p1, and then p2 if p1 fails in an uncommitted state on the input */
	def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

	/**Returns the portion of input inspected by p if successful */
	def slice[A](pa: Parser[A]): Parser[String]

	/**In the event of failure, replaces the assigned message with the received text */
	def label[A](text: String)(p: Parser[A]): Parser[A]
	/**In the event of failure, adds the received text to the error stack returned by p */
	def scope[A](text: String)(p: Parser[A]): Parser[A]
	/**Delays committing to pa until after it succeeds */
	def attempt[A](pa: Parser[A]): Parser[A]

	implicit def char(c: Char): Parser[Char] =
		string(c.toString) map { (x: String) => x.charAt(0) }

	def countZeroOrMoreRepetitionsOf(c: Char): Parser[Int] =
		slice(many(char(c))) map (_.size)

	def countOneOrMoreRepetitionsOf(c: Char): Parser[Int] =
		slice(product(char(c), many(char(c)))) map (_.size)

	def twoParalelCountOfZeroOrMoreRepetitions(a: Char, b: Char): Parser[(Int, Int)] =
		char(a).many.slice.map(_.size) ** char(b).many1.slice.map(_.size)

	// Ejercicio 1a: Using product, implement the now-familiar combinator map2 ...
	/**La implementación no debe evaluar el segundo parámetro hasta después de saber si el primero es exitoso  */
	def map2_[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
		product(pa, pb) map { case (a, b) => f(a, b) }
	// Ejercicio 1b: ... and then use this to implement many1 in terms of many.
	def many1[A](pa: Parser[A]): Parser[List[A]] =
		map2(pa, many(pa)) { (a, la) => a :: la }

	// Ejercicio 3: Hard: Before continuing, see if you can define many in terms of or, map2, and succeed. No logré resolverlo :( Fue necesario hacer que el segundo parámetro de product y map2 no sea estricto para poder implementar esta operación así.
	def many[A](pa: Parser[A]): Parser[List[A]] =
		many1(pa) or succeed(Nil)

	// Ejercicio 4: Hard: Using map2 and succeed, implement the listOfN combinator from earlier.
	def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
		sequence(List.fill(n)(p))
	// Hecho para el ejercicio 4
	def sequence[A](spa: List[Parser[A]]): Parser[List[A]] =
		spa.foldLeft[Parser[List[A]]](succeed(List())) { (pAcc, pa) => map2(pa, pAcc) { (a, acc) => a :: acc } }

	// Ejercicio 6: Using flatMap and any other combinators, write the context-sensitive parser we couldn’t express earlier. To parse the digits, you can make use of a new primitive, regex, which promotes a regular expression to a Parser. In Scala, a string s can be promoted to a Regex object (which has methods for matching) using s.r, for instance, "[a-zA-Z_][a-zA-Z0-9_]*".r
	def listOf[A](p: Parser[A]): Parser[List[A]] =
		listOf(integer, p)
	// Hecho para el ejercicio 6
	def listOf[A](sizeParser: Parser[Int], p: Parser[A]): Parser[List[A]] =
		sizeParser flatMap { howMany => listOfN(howMany, p) }
	// Hecho para el ejercicio 6
	def integer: Parser[Int] =
		regex("[0-9]{1,9}"r) map { s => s.toInt }

	// Ejercicio 7: Implement product and map2 in terms of flatMap
	/**La implementación no debe evaluar el segundo parámetro hasta después de saber si el primero es exitoso  */
	def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
		pa flatMap { a => map(pb) { b => (a, b) } } // here map_ is used but it is implemented in terms of flatMap and succeed, which in turn are also implemented in terms of flatMap 
	def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
		pa flatMap { a => map(pb) { b => f(a, b) } } // here map_ is used but it is implemented in terms of flatMap and succeed, which in turn are also implemented in terms of flatMap
	// Ejercicio 8:
	def map[A, B](pa: Parser[A])(f: A => B): Parser[B] =
		pa flatMap { a => succeed(f(a)) }

	implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
	implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

	case class ParserOps[A](p1: Parser[A]) {
		@inline def |[B >: A](p2: Parser[B]): Parser[B] =
			self.or(p1, p2)
		@inline def or[B >: A](p2: => Parser[B]): Parser[B] =
			self.or(p1, p2)
		@inline def map[B](f: A => B): Parser[B] =
			self.map(p1)(f)
		@inline def flatMap[B](f: A => Parser[B]): Parser[B] =
			self.flatMap(p1)(f)
		@inline def **[B](pb: => Parser[B]): Parser[(A, B)] =
			product(p1, pb)
		@inline def map2[B, C](pb: Parser[B])(f: (A, B) => C): Parser[C] =
			self.map2(p1, pb)(f)
		@inline def many: Parser[List[A]] =
			self.many(p1)
		@inline def many1: Parser[List[A]] =
			self.many1(p1)
		@inline def slice: Parser[String] =
			self.slice(p1)
	}

	object Laws {
		import readren.funcLib.propCheck.Prop
		import readren.funcLib.dataTypes.Gen
		import readren.funcLib.dataTypes.SGen;
		import Gen.GenOps;
		import SGen.SGenOps;

		private def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
			Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

		def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
			equal(p, p.map(a => a))(in)

		def succeedLaw[A](ga: Gen[A])(gIn: Gen[String]): Prop =
			Prop.forAll(ga ** gIn) { case (a, in) => run(succeed(a))(in) == Right(a) }

		// Ejercicio 2: Hard: Try coming up with laws to specify the behavior of product  
		def productLaw[A, B, C](pa: Parser[A], pb: Parser[B], pc: Parser[C])(in: Gen[String]): Prop =
			equal(
				product(pa, product(pb, pc)).map { case (a, (b, c)) => (a, b, c) },
				product(product(pa, pb), pc).map { case ((a, b), c) => (a, b, c) })(in)

		def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
			Prop.forAllProgresively(inputs ** SGen.string) {
				case (input, msg) =>
					run(label(msg)(p))(input) match {
						case Left(e) => e.top.message == msg
						case _ => true
					}
			}

		def orLaw[A](p1: Parser[A], p2: Parser[A])(input: Gen[String]): Prop =
			Prop.forAll(input) {
				case in =>
					val oneOrTwo = run(p1 or p2)(in)
					val one = run(p1)(in)
					val two = run(p2)(in)
					oneOrTwo.isRight == (one.isRight || two.isRight)
			}

	}
}