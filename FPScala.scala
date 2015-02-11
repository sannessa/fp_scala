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

  // 2.5 
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  // 3.1
  /* no code here but logical analysis
   
     List(1,2,3,4,5)

     so, the first case:
       case Cons(x, Cons(2, Cons(4, _))) => x
       - no match, x is allowed to be anything but the tail is List(2,4) which
         this list is not - it's (1,2,3,4,5)

       case Nil => 42
       - no match, the list is not Nil

       case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
       - there should be a match here, x and y are allowed to be anything
         and y is in a link with (3,4); the _ at the end is a "don't care" 
	 - the result would be 1 + 2 = 3

       case Cons(h, t) => h + sum(t)
       - this should also be a match, it's saying sum the entire list together
         which would make the resulting x be 1 + 2 + 3 + 4 + 5 = 15...but what
	 about hitting the end of the list? I think the sum function takes care
	 of that case

       case _ => 101
       - match, it's a don't care and it'll guarantee to match and return 101

    so, there are 3 matching cases, I believe it's going to pick the very first
    case that it matches which would be:
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  */
}
