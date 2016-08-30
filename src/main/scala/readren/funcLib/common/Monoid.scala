package readren.funcLib.common

import readren.funcLib.dataTypes.SGen
import readren.funcLib.propCheck.Prop

trait Monoid[A] {
  def op(p1: A, p2: A): A
  def zero: A
}

object Monoid {

  def listMonoid[A] = new Monoid[List[A]] {
    def op(p2: List[A], p3: List[A]) = p2 ++ p3
    val zero = Nil
  }

  // Ejercicio 1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(p2: Int, p3: Int): Int = p2 + p3
    def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(p2: Int, p3: Int): Int = p2 * p3
    def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(p2: Boolean, p3: Boolean): Boolean = p2 || p3
    def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(p2: Boolean, p3: Boolean): Boolean = p2 && p3
    def zero: Boolean = true
  }

  class IntAddition extends Monoid[Int] {
    def op(p2: Int, p3: Int): Int = p2 + p3
    def zero: Int = 0
  }

  // Ejercicio 2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(p2: Option[A], p3: Option[A]): Option[A] = p2.orElse(p3)
    def zero: Option[A] = None
  }
  def map2OptionMonoid[A](m: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(oa1: Option[A], oa2: Option[A]): Option[A] =
      for {
        p2 <- oa1
        p3 <- oa2
      } yield m.op(p2, p3)

    def zero: Option[A] = Some(m.zero)
  }

  //Ejercicio 3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(p2: A => A, p3: A => A): A => A = p2 compose p3
    def zero: A => A = x => x

  }

  //Ejercicio 4
  def monoidLaws[A](m: Monoid[A], gen: SGen[A]): Prop = {
    Prop.forAll(gen ** gen ** gen) {
      case ((g1, g2), g3) =>
        m.op(m.op(g1, g2), g3) == m.op(g1, m.op(g2, g3)) &&
          m.op(g1, m.zero) == m.op(m.zero, g1)
    }
  }

  // Del libro, necesario para el ejercicio 18
  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  // Ejercicio 15: Prove that, if types A and B are monoids, then the tuple type (A, B) is also a monoid (called their product).
  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(p2: (A, B), p3: (A, B)): (A, B) = (ma.op(p2._1, p3._1), mb.op(p2._2, p3._2))
    def zero: (A, B) = (ma.zero, mb.zero)
    def proof(p1: (A, B), p2: (A, B), p3: (A, B)) = {
      val e1 = op(p1, op(p2, p3)) == op(op(p1, p2), p3)
      // falta terminar, no lo hice porque es muy engorrosa la forma que iba a hacerlo (sustitución y comparación) y no aporta nada.
    }
  }

  // Ejercicio 17: Write a monoid instance for functions whose results are monoids.
  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(p1: A => B, p2: A => B): A => B =
      a => mb.op(p1(a), p2(a))
    def zero: A => B = a => mb.zero
  }

}

