package readren.funcLib.common

import scala.language.higherKinds;

import readren.funcLib;
import funcLib.dataTypes.Par;
import Par.Par;

sealed trait Free[F[_], A] {
  // Ejercicio 13.1.a: Free is a monad for any choice of F. Implement map and flatMap methods on the Free trait, ... 
  def map[B](f: A => B): Free[F, B] = FlatMap(this, (a: A) => Return(f(a)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
}
case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](r: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]
  type IO[A] = Free[Par, A]

 // Ejercicio 13.1.b: ... and give the Monad instance for Free[F,_]
  def unit[F[_], A](a: A): Free[F, A] = Return(a)
  def freeMonad[F[_]]: Monad[({ type f[x] = Free[F, x] })#f] = {
    type M[A] = ({ type f[X] = Free[F, X] })#f[A]
    new Monad[M] {
      def unit[A](a: A): M[A] = Free.unit(a)
      def lazyUnit[A](a: => A): M[A] = Free.unit(a)
      def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = ma.flatMap(f)
    }
  }

  /**Crea un `Suspend[Par,A] { Par.delay(a) }`.
   * Notar que `Free.unit(a:A)` da un `Return[Par, A]` y `IO(a:A)` da un `Suspend[Par, A]` */
  def IO[A](a: => A): IO[A] = Suspend { Par.delay(a) }

  // Ejercicio 13.2: Implement a specialized tail-recursive interpreter, runTrampoline, for running a Free[Function0,A].
  @annotation.tailrec
  def runTrampoline[A](ea: TailRec[A]): A =
    ea match {
      case Return(a1)  => a1
      case Suspend(r1) => r1()
      case FlatMap(io1, f1) => //run(f1(run(io1))) no es tail-recursive, por eso hubo que desglosar con 'match' 
        io1 match {
          case Return(a2)  => runTrampoline(f1(a2))
          case Suspend(r2) => runTrampoline(f1(r2()))
          case FlatMap(io2, f2) =>
            runTrampoline(FlatMap(io2, (a: Any) => FlatMap(f2(a), f1))) // ya que "FlatMap(FlatMap(io2, f2), f1)" equivale a "FlatMap(io2, a=>FlatMap(f2(a),f1))" por una ley asociativa de los monads, y la segunda tiene la ventaja de reducir un nivel el grado de anidamiento. 
        }
    }

  // Ejercicio 13.3: Hard: Implement a generic interpreter for Free[F,A], given a Monad[F]. You can pattern your implementation after the Async interpreter given previously, including use of a tail-recursive step function.
  def run[M[_], A](ea: Free[M, A])(implicit me: Monad[M]): M[A] = {

    @annotation.tailrec
    def step(ea: Free[M, A]): Free[M, A] = ea match {
      case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
      case FlatMap(Return(x), f)     => step(f(x))
      case _                         => ea
    }
    step(ea) match {
      case Return(a)  => me.unit(a)
      case Suspend(r) => r // me.flatMap(r)(a => run[M,A](Free.unit(a)))
      case FlatMap(x, f) => x match {
        case Suspend(r) => me.flatMap(r)(a => run(f(a)))
        case _          => sys.error("Impossible; `step` eliminates these cases")
      }
    }
  }

  // Copiado del libro
  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
  type ~>[F[_], G[_]] = Translate[F, G]

  def runFree[J[_], K[_], A](free: Free[J, A])(t: J ~> K)(implicit mk: Monad[K]): K[A] = {
    @annotation.tailrec
    def step(ega: Free[J, A]): Free[J, A] = ega match {
      case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
      case FlatMap(Return(x), f)     => step(f(x))
      case _                         => ega
    }

    step(free) match {
      case Return(a)              => mk.unit(a)
      case Suspend(r)             => t(r)
      case FlatMap(Suspend(r), f) => mk.flatMap(t(r))(a => runFree(f(a))(t))
      case _                      => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  def unsafePerformIO[A](io: IO[A])(implicit E: java.util.concurrent.ExecutorService): A =
    Par.run(E) { run(io)(Monad.parMonad) }.get
}
