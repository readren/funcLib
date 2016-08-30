package readren.funcLib.dataTypes

sealed trait Tree[+A] {
  
  // Ejercicio 26
  def size: Int
  
  // Ejercicio 28
  def depth: Int
  
  // Ejercico 29
  def map[B](f: A => B): Tree[B]
  
  // Ejercicio 30
  def fold[B](z: A => B)(f: (B, B) => B): B
  def size2 = fold[Int](a => 1)((x, y) => 1 + x + y)
  def depth2 = fold[Int](a => 1)((x, y) => 1 + math.max(x, y))
  def map2[B](f:A=>B):Tree[B] = fold[Tree[B]](a => new Leaf[B](f(a)))((x,y)=>new Branch[B](x,y))
}

case class Leaf[A](value: A) extends Tree[A] {
  override def size = 1
  override def depth = 1
  override def map[B](f: A => B) = new Leaf[B](f(value))
  override def fold[B](z: A => B)(f: (B, B) => B): B = z(value)
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size = 1 + left.size + right.size
  override def depth = 1 + math.max(left.depth, right.depth)
  override def map[B](f: A => B) = new Branch(left.map(f), right.map(f))
  override def fold[B](z: A => B)(f: (B, B) => B): B = f(left.fold(z)(f), right.fold(z)(f))
}

object Tree {
  //Ejercicio 27
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left, right) => math.max(maximum(left), maximum(right))
  }
}