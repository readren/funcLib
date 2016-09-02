package readren.funcLib.common

import scala.language.higherKinds;
import scala.language.implicitConversions


/**Luego vi la del libro (esta de aquí) y, en un principio, me pareció que estaba mal porque traverse y sequence se llaman mutuamente provocando recursión infinita. Lo que no me di cuenta, y no dice en el libro, es la idea de implementar todas las funciones del algebra en término de las otras (cuando es posible) cosa de dar libertad a quien implementa el algebra de elegir el grupo de funciones primitivas que mas le convenga. En este caso particular, hay solo dos funciones y ambas pueden ser implementadas en términos de la otra, así que cualquiera de las dos pude ser elegida como la primitiva (dejando a la otra como derivada). O sea, la implementación deberá pisar al menos una de las dos, la que sea elegida como primitiva. */
trait Traverzable[T[_]] extends Functor[T] with Foldable[T] { self =>
	def traverse[P[_]: Applicative, A, B](ta: T[A])(f: A => P[B]): P[T[B]] =
		sequence(map(ta)(f))
	def sequence[P[_]: Applicative, A](tpa: T[P[A]]): P[T[A]] =
		traverse(tpa)(x => x)

	// Ejercicio 14:
	def map[A, B](ta: T[A])(f: A => B): T[B] = {
		implicit val p = Monad.optionMonad
		traverse(ta) { (x: A) => p.unit(f(x)) }.get
	}

	// Copiado del libro
	override def foldMap[A, B](ta: T[A])(f: A => B)(mb: Monoid[B]): B =
		traverse[({ type f[x] = Traverzable.Const[B, x] })#f, A, Nothing](ta)(f)(Traverzable.monoidApplicative[B](mb))

	// Ejercicio 15: Answer, to your own satisfaction, the question of why it’s not possible for Foldable to extend Functor. Can you think of a Foldable that isn’t a functor?
	// Seguramente porque no se puede implementar map en términos de las funciones primitivas de Foldable cumpliendo las Functor laws. Y seguramente debe haber algún Foldable que no es un Functor (lo vi sin querer en el las respuestas mientras buscaba otra cosa: Iterator)

	// Copiado del libro. Es una versión de 'traverse' apropiada para los casos en que se quiere que la función que se aplica sobre los elementos atravesados dependa de: un estado inicial, los elementos que ya fueron atravesados, y su ubicación u orden.
	import readren.funcLib.dataTypes.StateAlgebra
	def traverseS[S, A, B](ta: T[A])(f: A => StateAlgebra[S]#Transition[B])(implicit sas:StateAlgebra[S]): sas.Transition[T[B]] = {
		traverse[sas.Transition, A, B](ta)(f)(sas.Transition)
	}

	// Copiado del libro: listing 12.11.
	def mapAccum[S, A, B](ta: T[A], s: S)(f: (A, S) => (B, S)): (T[B], S) = {
		implicit val sas = new StateAlgebra[S]
		val ttb = traverseS(ta) { (a: A) =>
			sas.Transition.flatMap(sas.Transition.getState) { s1 =>
				val (b, s2) = f(a, s1)
				sas.Transition.map(sas.Transition.setState(s2))(_ => b)
			}
		}
		ttb(s)
	}

	override def toList[A](fa: T[A]): List[A] = {
		mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse
	}

	def zipWithIndex[A](fa: T[A]): T[(A, Int)] = {
		mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1;
	}

	// Ejercicio 16: There’s an interesting consequence of being able to turn any traversable functor into a reversed list — we can write, once and for all, a function to reverse any traversable functor! Write this function, and think about what it means for List, Tree, and other traversable functors.
	def reverse[A](ta: T[A]): T[A] = {
		mapAccum[List[A], A, A](ta, toList[A](ta).reverse) { // tuve que mirar las respuestas. No se me ocurrió como hacerlo sin conocer un builder de T[A] :-(
			(a: A, la: List[A]) => (la.head, la.tail)
		}._1
	}
	// It should obey the following law, for all x and y of the appropriate types:
	object testReverse {
		def eq[A](x: T[A], y: T[A])(implicit f: Foldable[T]) = Foldable.toList(reverse(x)) ++ Foldable.toList(reverse(y)) == (Foldable.toList(y) ++ Foldable.toList(x)).reverse
	}

	// Ejercicio 17: Use mapAccum to give a default implementation of foldLeft for the Traverse trait.
	override def foldLeft[A, B](ta: T[A])(z: B)(f: (B, A) => B): B =
		mapAccum[B, A, B](ta, z) { (a, s) =>
			val b = f(s, a)
			(b, b)
		}._2

	// Ejercicio 18: Use applicative functor products to write the fusion of two traversals. This function will, given two functions f and g, traverse ta a single time, collecting the results of both functions at once.
	def fuse[G[_], H[_], A, B](ta: T[A])(f: A => G[B], g: A => H[B])(pg: Applicative[G], ph: Applicative[H]): (G[T[B]], H[T[B]]) = {
		type R[X] = (G[X], H[X])
		traverse[R, A, B](ta) {
			a => (f(a), g(a))
		}(pg.product(ph))
	}

	// Ejercicio 19: Implement the composition of two Traverse instances.
	def compose[U[_]](implicit tu: Traverzable[U]): Traverzable[({ type R[x] = T[U[x]] })#R] = {
		type R[X] = T[U[X]]
		new Traverzable[R] {
			override def traverse[P[_]: Applicative, A, B](ra: R[A])(f: A => P[B]): P[R[B]] = {
				val g: U[A] => P[U[B]] = (ua: U[A]) => tu.traverse[P, A, B](ua)(f)
				self.traverse[P, U[A], U[B]](ra)(g)
			}
		}
	}

	// Ejercicio 20: Hard: Implement the composition of two monads where one of them is traversable.
	// Esta hecho en ch11.FunctorAndMond.Monad

}

object Traverzable {

	// Ejercicio 13a usando el Traversable del libro
	val listTraversable = new Traverzable[List] {
		override def traverse[P[_], A, B](ta: List[A])(f: A => P[B])(implicit p: Applicative[P]): P[List[B]] =
			ta.foldRight(p.unit(List[B]())) { (a, acc) =>
				p.map2(f(a), acc) { _ :: _ }
			}
	}

	// Ejercicio 13b usando el Traversable del libro
	val optionTraversable = new Traverzable[Option] {
		override def traverse[P[_], A, B](ta: Option[A])(f: A => P[B])(implicit p: Applicative[P]): P[Option[B]] =
			ta match {
				case None => p.unit(None)
				case Some(a) => p.map(f(a)) { Some(_) }
			}
	}

	// Ejercicio 13c usando el Traversable del libro
	import readren.funcLib.dataTypes.{ Tree, Branch, Leaf };
	val treeTraversable = new Traverzable[Tree] {
		override def traverse[P[_], A, B](ta: Tree[A])(f: A => P[B])(implicit p: Applicative[P]): P[Tree[B]] =
			ta match {
				case Leaf(a) => p.map(f(a)) { Leaf(_) }
				case Branch(l, r) => p.map2(traverse(l)(f), traverse(r)(f)) { Branch(_, _) }
			}
	}

	// Del libro
	type Const[Y, B] = Y
	implicit def monoidApplicative[Y](my: Monoid[Y]): Applicative[({ type f[x] = Const[Y, x] })#f] =
		new Applicative[({ type f[x] = Const[Y, x] })#f] {
			def unit[A](a: A): Y = my.zero
			def lazyUnit[A](a: => A): Y = my.zero
			def map2[A, B, C](y1: Y, y2: Y)(f: (A, B) => C): Y = my.op(y1, y2)
		}

}
