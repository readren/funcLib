package readren.funcLib.dataTypes

import readren.funcLib.common.Applicative

// Ejercicio 6: Write an Applicative instance for Validation that accumulates errors in Failure. Note that in the case of Failure there’s always at least one error, stored in head. The rest of the errors accumulate in the tail.
sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
	def validationApplicative[E] = new Applicative[({ type f[x] = Validation[E, x] })#f] {
		def unit[A](a: => A): Validation[E, A] = Success(a)

		def map2[A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
			va match {
				case Success(a) => vb match {
					case Success(b) => Success(f(a, b))
					case _ => vb.asInstanceOf[Failure[E]]
				}
				case Failure(h1, t1) => vb match {
					case Failure(h2, t2) => Failure(h1, h2 +: (t1 ++ t2))
					case _ => va.asInstanceOf[Failure[E]]
				}
			}
	}
}