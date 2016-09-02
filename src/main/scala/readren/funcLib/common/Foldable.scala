package readren.funcLib.common

import scala.language.higherKinds

import readren.funcLib.dataTypes.{ Branch, Leaf, Tree }

/**
 * @author Gustavo
 */

// Ejercicio 12: Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].
trait Foldable[F[_]] {
	def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
		foldMap(as)(f.curried)(Monoid.endoMonoid[B])(z)

	def foldLeft[A, B](la: F[A])(z: B)(f: (B, A) => B): B = {
		val m: Monoid[B => B] = new Monoid[B => B] {
			def op(y1: B => B, y2: B => B): B => B = y1 andThen y2
			def zero: B => B = y => y
		}
		foldMap(la)((a: A) => (b: B) => f(b, a))(m)(z)
	}

	def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
		foldLeft(as)(mb.zero) { (b, a) => mb.op(b, f(a)) }

	def concatenate[A](as: F[A])(m: Monoid[A]): A =
		foldLeft(as)(m.zero)(m.op)

	def toList[A](as: F[A]): List[A] =
		foldRight(as)(List[A]())(_ :: _)
}

object Foldable {

	private trait HeadAndTailExtractor[C[_]] {
		def isEmpty(c: C[_]): Boolean
		def head[X](c: C[X]): X
		def tail[X](c: C[X]): C[X]
	}
	private def foldableBuilder[F[_], G[X] >: F[X]](hte: HeadAndTailExtractor[G]): Foldable[F] = new Foldable[F] {
		import scala.annotation.tailrec

		override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
			@tailrec
			def loop(remaining: G[A], acc: B): B = {
				if (hte.isEmpty(remaining)) acc
				else loop(hte.tail(remaining), f(acc, hte.head(remaining)))
			}
			loop(as, z)
		}

		override def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
			def loop(remaining: G[A]): B = {
				if (hte.isEmpty(remaining)) z
				else f(hte.head(remaining), loop(hte.tail(remaining)))
			}
			loop(as)
		}
	}

	private val seqHeadAntTailExtractor = new HeadAndTailExtractor[Traversable] {
		def isEmpty(c: Traversable[_]): Boolean = c.isEmpty
		def head[X](c: Traversable[X]): X = c.head
		def tail[X](c: Traversable[X]): Traversable[X] = c.tail
	}

	def foldableList: Foldable[List] = foldableBuilder[List, Traversable](seqHeadAntTailExtractor)
	def foldableIndexedSeq: Foldable[IndexedSeq] = foldableBuilder[IndexedSeq, Traversable](seqHeadAntTailExtractor)
	def foldableStream: Foldable[Stream] = foldableBuilder[Stream, Traversable](seqHeadAntTailExtractor)

	//Ejercicio 13: Recall the binary Tree data type from chapter 3. Implement a Foldable instance for it.
	def foldableTree: Foldable[Tree] = new Foldable[Tree] {
		override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
			as match {
				case Leaf(v) => f(v, z)
				case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
			}
		override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
			as match {
				case Leaf(v) => f(z, v)
				case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
			}
		override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
			as match {
				case Leaf(v) => f(v)
				case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
			}
	}

	//Ejercicio 14: Write a Foldable[Option] instance.
	def foldableOption: Foldable[Option] = new Foldable[Option] {
		override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
			as match {
				case None => z
				case Some(v) => f(v, z)
			}
		override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
			foldRight(as)(z) { (a, b) => f(b, a) }
		override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
			as match {
				case None => mb.zero
				case Some(v) => f(v)
			}
	}

	// Ejercicio 15: Any Foldable structure can be turned into a List. Write this conversion in a generic way:
	def toList[A, C[A]](c: C[A])(implicit f: Foldable[C]): List[A] =
		f.foldRight[A, List[A]](c)(List()) { (e, la) => e :: la }
}
