package readren.funcLib.dataTypes
import readren.funcLib.common.Monad

/**
 * @author Gustavo
 */
object monadic {

	case class Id[A](value: A) {
		def flatMap[B](f: A => Id[B]): Id[B] = f(value)
	}
	object Id {
		def unit[A](a: A): Id[A] = Id(a)
		def lazyUnit[A](a: => A): Id[A] = Id(a)
	}

	// Ejercicio 17 : Implement map and flatMap as methods on this class, and give an implementation for Monad[Id].
	val idMonad: Monad[Id] = new Monad[Id] {
		def unit[A](a: A): Id[A] = Id.unit(a)
		def lazyUnit[A](a: => A): Id[A] = Id.lazyUnit(a)
		def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)

	}

	// Ejercicio 20: Hard: To cement your understanding of monads, give a monad instance for the following type, and explain what it means. What are its primitive operations? What is the action of flatMap? What meaning does it give to monadic functions like sequence, join, and replicateM? What meaning does it give to the monad laws?
	case class Reader[R, A](run: R => A)
	//What it means? Supongo que sirve para armar mediante combinadores un interprete de una fuente de información mutable (R) que es consumida cada vez que la función 'run' es llamada.
	def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
		override def unit[A](a: A): Reader[R, A] =
			Reader(r => a)
		override def lazyUnit[A](a: => A): Reader[R, A] =
			Reader(r => a)
		override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
			Reader[R, B] { (r: R) => f(st.run(r)).run(r) }
	}

	def eitherRightProjectionMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
		def unit[A](a: A): Either[E, A] = Right(a)
		def lazyUnit[A](a: => A): Either[E, A] = Right(a)
		def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma.right flatMap f
	}

	def eitherLeftProjectionMonad[E] = new Monad[({ type f[x] = Either[x, E] })#f] {
		def unit[A](a: A): Either[A, E] = Left(a)
		def lazyUnit[A](a: => A): Either[A, E] = Left(a)
		def flatMap[A, B](ma: Either[A, E])(f: A => Either[B, E]): Either[B, E] = ma.left flatMap f
	}
}