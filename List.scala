sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail : List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
 
  // 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // 3.3
  def setHead[A](l: List[A], newH: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(newH, t) 
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(cur: Int, as: List[A]): List[A] = {
      if (cur == n) as
      else go(cur + 1, tail(as))
    }
    go(0, l)
  }
}
