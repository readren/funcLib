package readren.funcLib.common

trait Applicative[P[_]] extends Functor[P] { self =>
	// primitive combinators
	def map2[A, B, C](fa: P[A], fb: P[B])(f: (A, B) => C): P[C]
	def unit[A](a: => A): P[A]

	// derived combinators
	def map[A, B](fa: P[A])(f: A => B): P[B] =
		map2(fa, unit(()))((a, _) => f(a))

	def traverse[A, B](as: List[A])(f: A => P[B]): P[List[B]] =
		as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

	// Ejercicio 1: Transplant the implementations of as many combinators as you can from Monad to Applicative, using only map2 and unit, or methods implemented in terms of them.
	def sequence[A](lpa: List[P[A]]): P[List[A]] =
		traverse(lpa)(x => x)

	def replicateM[A](n: Int, pa: P[A]): P[List[A]] =
		sequence(List.fill(n) { pa })

	def product[A, B](pa: P[A], pb: P[B]): P[(A, B)] =
		map2(pa, pb)((_, _))

	// Ejercicio 2: Hard: The name applicative comes from the fact that we can formulate the Applicative interface using an alternate set of primitives, unit and the function apply, rather than unit and map2. Show that this formulation is equivalent in expressiveness by defining map2 and map in terms of unit and apply. Also establish that apply can be implemented in terms of map2 and unit.
	def apply_inTermsOfMap2AndUnit[A, B](pfab: P[A => B])(pa: P[A]): P[B] =
		map2(pfab, pa) { (fab, a) => fab(a) }

	def map_inTermsOfApplyAndUnit[A, B](pa: P[A])(f: A => B): P[B] =
		apply_inTermsOfMap2AndUnit { unit(f) }(pa)

	def map2_inTermsOfApplyAndUnit[A, B, C](pa: P[A], pb: P[B])(f: (A, B) => C): P[C] = {
		apply_inTermsOfMap2AndUnit {
			map_inTermsOfApplyAndUnit(pa) { f.curried } // primero lo hice así: { a => (b: B) => f(a, b) }, pero el ejercicio 3 me hizo dar cuenta que podía usar curried.
		}(pb)
	}

	// Ejercicio 3: The apply method is useful for implementing map3, map4, and so on, and the pattern is straightforward. Implement map3 and map4 using only unit, apply, and the curried method available on functions.1
	def map3[A, B, C, D](pa: P[A], pb: P[B], pc: P[C])(f: (A, B, C) => D): P[D] =
		apply_inTermsOfMap2AndUnit {
			apply_inTermsOfMap2AndUnit {
				map_inTermsOfApplyAndUnit(pa) { f.curried }
			}(pb)
		}(pc)

	def map4[A, B, C, D, E](pa: P[A], pb: P[B], pc: P[C], pd: P[D])(f: (A, B, C, D) => E): P[E] =
		apply_inTermsOfMap2AndUnit {
			apply_inTermsOfMap2AndUnit {
				apply_inTermsOfMap2AndUnit {
					map_inTermsOfApplyAndUnit(pa) { f.curried }
				}(pb)
			}(pc)
		}(pd)

	// Ejercicio 8
	def product[Q[_]](q: Applicative[Q]): Applicative[({ type R[x] = (P[x], Q[x]) })#R] = {
		type R[x] = (P[x], Q[x])
		new Applicative[R] {
			override def unit[A](a: => A): R[A] = (self.unit(a), q.unit(a))
			override def map2[A, B, C](ra: R[A], rb: R[B])(f: (A, B) => C): R[C] = {
				val x: P[C] = self.map2(ra._1, rb._1)(f)
				val y: Q[C] = q.map2(ra._2, rb._2)(f)
				(x, y)
			}
		}
	}

	// Ejercicio 9: Hard: Applicative functors also compose another way! If F[_] and G[_] are applicative functors, then so is F[G[_]]. Implement this function:
	def compose[Q[_]](q: Applicative[Q]) = new Applicative[({ type R[x] = P[Q[x]] })#R] {
		type R[x] = P[Q[x]]
		override def unit[A](a: => A): R[A] =
			self.unit(q.unit(a))
		override def map2[A, B, C](ra: R[A], rb: R[B])(f: (A, B) => C): R[C] = {
			def g(qa: Q[A], qb: Q[B]): Q[C] = q.map2(qa, qb)(f)
			self.map2(ra, rb) { g }
		}
	}

	// Ejercicio 10: Hard: Prove that this composite applicative functor meets the applicative laws. This is an extremely challenging exercise.
	// No lo hice

	// Ejercicio 11: Try to write compose on Monad. It’s not possible, but it is instructive to attempt it and understand why this is the case.
	// Esta resuelto en ch11.FunctorAndMonad.Monad 

	// Ejercicio 12: implement sequence over a Map rather than a List
	def sequenceMap[K, V](ofa: Map[K, P[V]]): P[Map[K, V]] =
		ofa.foldLeft(unit(Map[K, V]())) { (acc, e) =>
			map2(acc, e._2) { (m, v) => m + (e._1 -> v) }
		}
}

object Applicative {

	val streamApplicative = new Applicative[Stream] {
		def unit[A](a: => A): Stream[A] =
			Stream.continually(a)
		def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
			a zip b map f.tupled

		// Ejercicio 4: Hard: What is the meaning of streamApplicative.sequence? Specializing the signature of sequence to Stream, we have this:
		// Da un stream de listas, donde la primer lista contiene el primer elemento de los streams recibidos, la segunda lista contiene el segundo elemento de los stream recibidos, y así sucesivamente.
		override def sequence[A](lsa: List[Stream[A]]): Stream[List[A]] =
			lsa.foldRight(unit(List[A]()))((sa, sla) => map2(sa, sla)(_ :: _))
	}

}
