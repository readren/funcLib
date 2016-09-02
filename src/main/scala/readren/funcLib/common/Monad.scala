package readren.funcLib.common

import scala.language.higherKinds
import scala.language.implicitConversions

import readren.funcLib;

/**@param M a monadic type constructor */
trait Monad[M[_]] extends Applicative[M] { self =>
	def unit[A](a: A): M[A]
	def lazyUnit[A](a: => A): M[A]
	def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

	override def map[A, B](ma: M[A])(f: A => B): M[B] =
		flatMap(ma)(a => unit(f(a)))
	def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
		flatMap(ma)(a => map(mb)(b => f(a, b)))

	// Ejercicio 3: The sequence and traverse combinators should be pretty familiar to you by now, and your implementations of them from various prior chapters are probably all very similar. Implement them once and for all on Monad[F].
	def sequence_transplantadoAApplicative[A](lma: List[M[A]]): M[List[A]] =
		lma.foldRight(unit(List[A]())) { (me, macc) =>
			map2(me, macc) { _ :: _ }
		}
	def traverse_transplantadoAApplicative[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
		sequence(la.map(f))

	// Ejercicio 4: Implement replicateM.
	def replicateM_transplantadoAApplicative[A](n: Int, ma: M[A]): M[List[A]] =
		sequence(List.fill(n) { ma })

	// Desarrollado en el libro
	def product_transplantadoAApplicative[A, B](ma: M[A], mb: M[B]): M[(A, B)] =
		map2(ma, mb)((_, _))

	// Ejercicio 6: Hard: Here’s an example of a function we haven’t seen before. Implement the function filterM. It’s a bit like filter, except that instead of a function from A => Boolean, we have an A => F[Boolean].
	def filterM[A](la: List[A])(f: A => M[Boolean]): M[List[A]] = {
		// primer versión que se me ocurrió. Luego intenté evitar las colecciones intermedias y salió la segunda version de abajo
		// val x = traverse(ms)(a => product(f(a), unit(a)))
		// map(x) { lp => lp.collect { case (true,y) => y } }

		// segunda version que es mas eficiente porque no usa colecciones intermedias.
		la.foldRight(unit(List[A]())) { (a, macc) =>
			flatMap(f(a)) {
				case false => macc
				case true => map(macc) { a :: _ }
			}
		}
	}

	// Ejercicio 7: Implement the Kleisli composition function compose.
	def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
		(a: A) => flatMap[B, C](f(a))(g)

	// Ejercicio 8: Hard: Implement flatMap in terms of compose. It seems that we’ve found another minimal set of monad combinators: compose and unit.
	def flatMap_inTermsOfCompose[A, B](ma: M[A])(h: A => M[B]): M[B] = {
		object calculoAuxiliar {
			type X = Unit
			type Y = A
			type Z = B
			def t(r: X => M[Z]): M[B] = r(())
			def f(x: X): M[Y] = ma
			def g(y: Y): M[Z] = h(y)
			val eq1: M[B] = t(compose[X, Y, Z](f, g))
			val eq2: M[B] = t { (x: X) => flatMap(f(x))(g) }
		}

		compose[Unit, A, B]((_: Unit) => ma, h)(())
	}

	// Ejercicio 9: Show that the two formulations of the associative law, the one in terms of flatMap and the one in terms of compose, are equivalent.
	object ejer9 {

		def eq1[A, B, C](x: M[A], f: A => M[B], g: B => M[C]) = flatMap[B, C] { flatMap[A, B](x)(f) }(g) == flatMap[A, C](x) { a => flatMap[B, C](f(a))(g) }
		def eq2[A, B, C, D](f: A => M[B], g: B => M[C], h: C => M[D]) = compose[A, C, D](compose[A, B, C](f, g), h) == compose[A, B, D](f, compose[B, C, D](g, h))

		// sustituyendo en eq2 compose por su implementación en términos de flat map:
		def eq3[A, B, C, D](a: A, f: A => M[B], g: B => M[C], h: C => M[D]) = flatMap[C, D] { compose(f, g)(a) }(h) == flatMap[B, D](f(a))(compose(g, h))
		def eq4[A, B, C, D](a: A, f: A => M[B], g: B => M[C], h: C => M[D]) =
			flatMap[C, D] { ((x: A) => flatMap[B, C] { f(x) }(g))(a) }(h) == flatMap[B, D](f(a)) { (y: B) => flatMap[C, D] { g(y) }(h) }
		// simplificando el lado izquierdo        
		def eq5[A, B, C, D](a: A, f: A => M[B], g: B => M[C], h: C => M[D]) =
			flatMap[C, D] { flatMap[B, C] { f(a) }(g) }(h) == flatMap[B, D](f(a)) { (y: B) => flatMap[C, D] { g(y) }(h) }
		// reemplazando f(a) por mb
		def eq6[B, C, D](mb: M[B], g: B => M[C], h: C => M[D]) =
			flatMap[C, D] { flatMap[B, C] { mb }(g) }(h) == flatMap[B, D](mb) { (y: B) => flatMap[C, D] { g(y) }(h) }
	}

	// Ejercicio 10:
	object ejer10 {
		def eq1[A, B](f: A => M[B]) = compose[A, B, B](f, (y: B) => unit(y)) == f
		def eq2[A, B](f: A => M[B]) = compose[A, A, B]((y: A) => unit(y), f) == f

		def eq3[A](x: M[A]) = flatMap[A, A](x)((y: A) => unit(y)) == x
		def eq4[A, B](a: A, f: A => M[B]) = flatMap[A, B](unit(a))(f) == f(a)

		// en eq1 y eq2 reemplazo compose por su implementación en términos de flatMap
		def eq5[A, B](f: A => M[B]) = (a: A) => flatMap[B, B](f(a))((y: B) => unit(y)) == f
		def eq6[A, B](f: A => M[B]) = (a: A) => flatMap[A, B](unit(a))(f) == f

		// haciendo f(a) == mb en eq5, y aplicando ambos lados de eq5 y eq6 sobre la variable arbitraria 'a'
		def eq5[A, B](mb: M[B]) = flatMap[B, B](mb)((y: B) => unit(y)) == mb
		def eq6[A, B](a: A, f: A => M[B]) = flatMap[A, B](unit(a))(f) == f(a)
	}

	// Ejercicio 11: Prove that the identity laws hold for a monad of your choice.
	// no lo hice

	// Ejercicio 12: There’s a third minimal set of monadic combinators: map, unit, and join. Implement join in terms of flatMap.
	def join[A](mma: M[M[A]]): M[A] =
		flatMap[M[A], A](mma) { ma => ma }

	// Ejercicio 13, 14, 15, y 16: no los hice

	// Ejercicio 12.11 (Nota: es del capitulo siguiente: 12- Applicative Functors): Try to write compose on Monad. It’s not possible, but it is instructive to attempt it and understand why this is the case.
	def compose_notPossible[Q[_]](q: Monad[Q]) = new Monad[({ type R[x] = M[Q[x]] })#R] {
		type R[x] = M[Q[x]]
		def unit[A](a: A): R[A] =
			self.unit(q.unit(a))
		def lazyUnit[A](a: => A): R[A] =
			self.lazyUnit(q.lazyUnit(a))
		def flatMap[A, B](ra: R[A])(f: A => R[B]): R[B] = {
			def h(a: A): Q[B] = ???
			def g(qa: Q[A]): M[Q[B]] = self.unit(q.flatMap(qa)(h))
			self.flatMap(ra)(g)
		}

		override def map2[A, B, C](ra: R[A], rb: R[B])(f: (A, B) => C): R[C] = {
			def g(qa: Q[A], qb: Q[B]): Q[C] = q.map2(qa, qb)(f)
			self.map2(ra, rb) { g }
		}
	}

	// Ejercicio 12:20 (Nota: es del capitulo siguiente: 12- Traversable functors): Hard: Implement the composition of two monads where one of them is traversable.
	def composeM[N[_]](mn: Monad[N], tn: Traverzable[N]): Monad[({ type R[x] = M[N[x]] })#R] = { // no me salió. Tuve que mirar el resultado mas de una vez :(  Pero si me salió cuando ambos monads son traversables (en lugar de uno).
		type R[X] = M[N[X]]
		new Monad[R] {
			def unit[A](a: A): R[A] =
				self.unit(mn.unit(a));
			def lazyUnit[A](a: => A): R[A] =
				self.lazyUnit(mn.lazyUnit(a))
			def flatMap[A, B](ra: M[N[A]])(f: A => M[N[B]]): R[B] =
				self.flatMap(ra)(na => self.map(tn.traverse(na)(f)(self))(mn.join))
		}
	}

	// version de composeM que requiere que ambos monads sean traversables (en lugar de solo uno). Es lo que logré hacer por mi cuenta cuando intenté hacer el ejercicio 12:20
	def composeM_masEsquisita[N[_]](mn: Monad[N], tn: Traverzable[N], tm: Traverzable[M]): Monad[({ type R[x] = M[N[x]] })#R] = {
		type R[X] = M[N[X]]
		new Monad[R] {
			def unit[A](a: A): R[A] = {
				self.unit(mn.unit(a))
			}
			def lazyUnit[A](a: => A): R[A] = {
				self.lazyUnit(mn.lazyUnit(a))
			}
			def flatMap[A, B](ra: M[N[A]])(f: A => M[N[B]]): R[B] = {
				self.flatMap[N[A], N[B]](ra) { (na: N[A]) => tn.sequence[M, B](mn.flatMap[A, M[B]](na) { (a: A) => tm.sequence[N, B](f(a))(mn) })(self) }
			}
		}
	}

	// Agregado por mi para poder usar for-comprehension sobre instancias de M[A] para las cuales existe un Monad[M]
	implicit def operators[A](ma: M[A]): MonadOps[A] = MonadOps(ma)
	case class MonadOps[A](ma: M[A]) {
		def map[B](f: A => B): M[B] = self.map(ma)(f)
		def flatMap[B](f: A => M[B]): M[B] = self.flatMap[A, B](ma)(f)
	}
}

object Monad {

	import funcLib.dataTypes.Gen
	val genMonad:Monad[Gen] = Gen

	// Ejercicio 1: Write monad instances for Par, Parser, Option, Stream, and List
	import funcLib.dataTypes.Par.Par;
	val parMonad: Monad[Par] = new Monad[Par] {
		import funcLib.dataTypes.Par;
		override def unit[A](a: A): Par[A] = Par.unit(a)
		override def lazyUnit[A](a: => A): Par[A] = Par.async(a)
		override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
	};

	import readren.funcLib.stringParsing.myImpl.MyParser;
	val parserMonad: Monad[MyParser] = new Monad[MyParser] {
		import funcLib.stringParsing.myImpl.MyParsers
		override def unit[A](a: A): MyParser[A] = MyParsers.succeed(a)
		override def lazyUnit[A](a: => A): MyParser[A] = MyParsers.succeed(a)
		override def flatMap[A, B](pa: MyParser[A])(f: A => MyParser[B]): MyParser[B] = MyParsers.flatMap(pa)(f)
	}

	val optionMonad: Monad[Option] = new Monad[Option] {
		override def unit[A](a: A): Option[A] = Some(a);
		override def lazyUnit[A](a: => A): Option[A] = Some(a);
		override def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] = oa.flatMap(f)
	}
	val streamMonad: Monad[Stream] = new Monad[Stream] {
		override def unit[A](a: A): Stream[A] = Stream(a)
		override def lazyUnit[A](a: => A): Stream[A] = Stream(a)
		override def flatMap[A, B](sa: Stream[A])(f: A => Stream[B]): Stream[B] = sa.flatMap(f)
	};
	
	val listMonad: Monad[List] = new Monad[List] {
		override def unit[A](a: A): List[A] = List(a)
		override def lazyUnit[A](a: => A): List[A] = List(a)
		override def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] = la.flatMap(f)
	}

	// Ejercicio 2: Hard: State looks like it would be a monad too, but it takes two type arguments and you need a type constructor of one argument to implement Monad. Try to implement a State monad
	import funcLib.dataTypes.StateAlgebra;
	def transitionMonad[S]: Monad[StateAlgebra[S]#Transition] = 
		(new StateAlgebra[S]).Transition
	

	// Agregado por mí para poder usar for-comprehension sobre instancias de M[A] para las cuales existe un Monad[M]
	implicit def toMonadOps[A, M[_]](ma: M[A])(implicit monad: Monad[M]) = monad.MonadOps(ma)
}