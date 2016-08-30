package readren.funcLib.dataTypes

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons == None

  //Ejercicio 1
  def toList: List[A] = {
    val x = for (i <- uncons) yield (i._1 :: i._2.toList)
    x.getOrElse(Nil)
  }

  //Ejercicio 2
  def take(n: Int): Stream[A] =
    if (n == 0) Stream.empty
    else {
      val x = for (i <- uncons) yield (Stream.cons(i._1, i._2.take(n - 1)))
      x.getOrElse(Stream.empty)
    }

  //Ejercicio 3
  def takeWhile(p: A => Boolean): Stream[A] = {
    val x = for (i <- uncons; if p(i._1)) yield (Stream.cons(i._1, i._2.takeWhile(p)))
    x.getOrElse(Stream.empty)
  }

  //Ejercicio 4
  def foldRight[B](z: B)(f: (A, => B) => B): B = {
    val y = for (i <- uncons) yield (f(i._1, i._2.foldRight(z)(f)))
    y.getOrElse(z)
  }
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  //Ejercicio 5
  def takeWhile_2(p: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](Stream.empty)((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)
  }

  //Ejercicio 6
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Stream.empty)((a, b) => Stream.cons(f(a), b))
  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Stream.empty)((a, b) => if (f(a)) Stream.cons(a, b) else b)
  def append[T >: A](t: Stream[T]): Stream[T] =
    foldRight[Stream[T]](t)((a, b) => Stream.cons(a, b))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Stream.empty)((a, b) => f(a).append(b))

  //Ejercicio 12
  def map_uf[B](f: A => B): Stream[B] =
    Stream.unfold[B, Stream[A]](this)(s => s.uncons match {
      case None => None
      case Some((h, t)) => Some((f(h), t))
    })
  def map_uf2[B](f: A => B): Stream[B] =
    Stream.unfold[B, Stream[A]](this)(s =>
      for (i <- s.uncons) yield (f(i._1), i._2))

  def take_uf(n: Int): Stream[A] = {
    assert(n >= 0)
    Stream.unfold[A, (Stream[A], Int)]((this, n))(s =>
      if (s._2 == 0) None
      else s._1.uncons match {
        case None => None
        case Some((h, t)) => Some((h, (t, s._2 - 1)))
      })
  }
  def take_uf2(n: Int): Stream[A] = {
    Stream.unfold[A, (Stream[A], Int)]((this, n))(s =>
      for (i <- s._1.uncons; if s._2 > 0) yield ((i._1, (i._2, s._2 - 1))))
  }

  def takeWhile_uf(p: A => Boolean): Stream[A] =
    Stream.unfold[A, Stream[A]](this)(s =>
      s.uncons match {
        case None => None
        case i @ Some((h, _)) =>
          if (p(h)) i
          else None
      })
  def takeWhile_uf2(p: A => Boolean): Stream[A] =
    Stream.unfold[A, Stream[A]]((this))(s =>
      for (i <- s.uncons; if p(i._1)) yield i)

  def zip[B](that: Stream[B]): Stream[(A, B)] =
    Stream.unfold((this, that))(s =>
      for (i <- s._1.uncons; j <- s._2.uncons) yield ((i._1, j._1), (i._2, j._2)))

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold(this, that)(s =>
      (s._1.uncons, s._2.uncons) match {
        case (Some((xh, xt)), None) => Some((Some(xh), None), (xt, Stream.empty))
        case (None, Some((yh, yt))) => Some((None, Some(yh)), (Stream.empty, yt))
        case (Some((xh2, xt2)), Some((yh2, yt2))) => Some((Some(xh2), Some(yh2)), (xt2, yt2))
        case (None, None) => None
      })

  //Ejercicio 13
  def startsWith[B >: A](that: Stream[B]): Boolean = {
    val matches = Stream.unfold((this, that))(s =>
      for (i <- s._2.uncons) yield (s._1.uncons match {
        case None => (false, (Stream.empty, i._2))
        case Some((xh, xt)) => (xh == i._1, (xt, i._2))
      }))
    matches.forAll(_ == true)
  }

  def startsWith_2[B >: A](that: Stream[B]): Boolean = {
    (this.uncons, that.uncons) match {
      case (Some((hThis, tThis)), Some((hThat, tThat))) => if (hThis == hThat) tThis.startsWith_2(tThat) else false
      case (None, Some(_)) => false
      case (_, None) => true
    }
  }

  //Ejercicio 14
  def tails: Stream[Stream[A]] =
    Stream.unfold[Stream[A], Stream[A]](this)(s =>
      for (i <- s.uncons) yield (s, i._2)) append Stream.empty

  //Ejercicio 15
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight[Stream[B]](Stream(z))((a, sb) => sb.uncons.get match {
      case (xh, _) => Stream.cons[B](f(a, xh), sb)
    })

}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // Ejercicio 7
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // Ejercicio 8
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  //Ejercicio 9
  def fibs: Stream[Int] = {
    def g(i1: Int, i2: Int): Stream[Int] = Stream.cons(i1, g(i2, i1 + i2))
    g(0, 1)
  }

  //Ejercicio 10
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Stream.empty
      case Some((a, n)) => Stream.cons(a, unfold(n)(f))
    }

  //Ejercicio 11
  def fibs_2: Stream[Int] =
    unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))
  def from_2(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  // Para pensar antes del ejercicio 13
  def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = {
    if (sub.length > list.length) false
    else {
      if (list.take(sub.length) == sub) true
      else hasSubsequence(list.tail, sub)
    }
  }
}
