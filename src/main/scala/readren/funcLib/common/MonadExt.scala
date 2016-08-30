package readren.funcLib.common

object MonadExt {
  
  implicit def toMonadExt[M[_]](monad: Monad[M]): MonadExt[M] = new MonadExt[M] {
    def unit[A](a: => A): M[A] = monad.unit(a)
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = monad.flatMap(ma)(f)
  }

}

/**Se pudo haber agregado estos combinators de Monad en la definición de Monad, pero dado que son usados en casos muy particulares, aquí para poder agregarlos a Monad de forma opcional */
trait MonadExt[M[_]] extends Monad[M] { self =>
  def doWhile[A](a: M[A])(cond: A => M[Boolean]): M[Unit] = {
    import Monad.toMonadOps
    for {
      a1 <- MonadOps(a)
      ok <- MonadOps(cond(a1))
      _ <- MonadOps(if (ok) doWhile(a)(cond) else unit(()))
    } yield ()
  }
  def forever[A, B](a: M[A]): M[B] = {
    lazy val t: M[B] = forever(a) // la única razón de usar 'lazy' acá es para evitar que 'forever(a)' se ejecute mas de una vez si la implementación de 'flatMap' esta mal hecha y llama mas de una vez a la función que recibe. A mi me parece una medida costosa para soportar mal código.
    a flatMap (_ => t)  
  }

  // Corresponden a un side-note del capitulo 13.2 - A simple IO
  def foldM[A, B](l: Stream[A])(z: B)(f: (B, A) => M[B]): M[B] =
    l match {
      case h #:: t => f(z, h) flatMap (z2 => foldM(t)(z2)(f))
      case _       => unit(z)
    }

  /* comentado porque no compila porque 'skip' no esta definido
  // Corresponden a un side-note del capitulo 13.2 - A simple IO
  def foldM_[A, B](l: Stream[A])(z: B)(f: (B, A) => M[B]): M[Unit] =
    skip { foldM(l)(z)(f) }

  // Corresponden a un side-note del capitulo 13.2 - A simple IO
  def foreachM[A](l: Stream[A])(f: A => M[Unit]): M[Unit] =
    foldM_(l)(())((u, a) => skip(f(a)))
	*/
}
