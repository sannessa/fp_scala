object FPScala {
  // 2.1
  def fibonnaci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, curr: Int, prev: Int, prevPrev: Int): Int = {
      if (curr == n) prev + prevPrev
      else go(n, curr + 1, prev + prevPrev, prev)
    }

    if (n == 0) 0
    else if (n == 1) 1
    else go(n, 2, 1, 0)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= (as.length - 1)) true
      else if (ordered(as(n), as(n + 1))) go(n + 1) 
      else false
    }

    go(0)
  }
 
  def aLessThanB(a: Int, b: Int): Boolean = {
    a < b
  }

  // 2.3
  def currying[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }

  // 2.4
  def uncurrying[A,B,C](f: A => B => C): (A,B) = {
    (a,b) => f(a)(b)
  }
}
