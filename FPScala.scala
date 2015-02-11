object FPScala {
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
}
