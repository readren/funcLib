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

