package readren.funcLib.dataTypes

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