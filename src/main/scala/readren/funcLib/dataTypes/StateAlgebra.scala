package readren.funcLib.dataTypes

import scala.language.higherKinds
import scala.language.implicitConversions
import readren.funcLib.common.Monad

class StateAlgebra[S] {

	abstract class TransitionAlgebra[Transition[+_]] extends Monad[Transition] { self =>

		//////// primitive ///////

		def getState: Transition[S];
		def setState(s: S): Transition[Unit];

		/////// derived ///////

		def modify(f: S => S): Transition[Unit] =
			for {
				s <- getState
				_ <- setState(f(s))
			} yield ()

	};

	///// Algebra implementation /////

	type Transition[+A] = S => (A, S);
	object Transition extends TransitionAlgebra[Transition] {

		def unit[A](a: A): Transition[A] = s => (a, s)
		def lazyUnit[A](a: => A): Transition[A] = s => (a, s);
		def flatMap[A, B](ta: Transition[A])(f: A => Transition[B]): Transition[B] = s1 => {
			val (a, s2) = ta(s1)
			f(a)(s2)
		}
		def getState: Transition[S] = s => (s, s)
		def setState(s: S): Transition[Unit] = _ => ((), s)
	};

};

object pruebas {
	val saInt: StateAlgebra[Int] = new StateAlgebra
	import saInt._
	val ta:saInt.Transition[String] = saInt.Transition.unit("hola")
	val tb:Transition[Int] = Transition.MonadOps(ta).map( _.length)
//	val tc:Transition[Int] = ta.map(_.length)
	
}

/*
case class Transition[S, +A](run: S => (A, S)) {

	def map[B](f: A => B): Transition[S, B] =
		Transition(s1 => {
			val (a, s2) = run(s1)
			(f(a), s2)
		})

	def flatMap[B](g: A => Transition[S, B]): Transition[S, B] =
		Transition(s1 => {
			val (a, s2) = run(s1)
			g(a).run(s2)
		})

	def map2[B, C](s: Transition[S, B])(f: (A, B) => C): Transition[S, C] = {
		flatMap[C](a => s.map(b => f(a, b)))
	}

};

object Transition {

	// Ejercicio 11
	def unit[S, A](a: A): Transition[S, A] = Transition(s => (a, s))
	def lazyUnit[S, A](a: => A): Transition[S, A] = Transition(s => (a, s));

	def getState[S]: Transition[S, S] =
		Transition(s => (s, s))

	def setState[S](s: S): Transition[S, Unit] =
		Transition[S, Unit](_ => ((), s))

	def modify[S](f: S => S): Transition[S, Unit] =
		for {
			s <- getState
			_ <- setState(f(s))
		} yield ()

	def sequence[S, A](l: List[Transition[S, A]]): Transition[S, List[A]] =
		l.foldRight(Transition[S, List[A]](s => (Nil, s)))((sa, sas) => sa.map2(sas)(_ :: _))

}
*/