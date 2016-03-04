package ithaka.core

/**
 * Various utility methods.
 */
object Utils {

  /**
   * Calculate the longest common subsequence via dynamic programming.
   *
   * From: http://rosettacode.org/wiki/Longest_common_subsequence#Scala
   *
   * @param a a first String
   * @param b a second String
   * @return
   */
  def lcs(a: String, b: String): String = {
    if (a.size==0 || b.size==0) ""
    else if (a==b) a
    else {
      val lengths = Array.ofDim[Int](a.size+1,b.size+1)
      for (i <- 0 until a.size)
        for (j <- 0 until b.size)
          if (a(i) == b(j))
            lengths(i+1)(j+1) = lengths(i)(j) + 1
          else
            lengths(i+1)(j+1) = scala.math.max(lengths(i+1)(j),lengths(i)(j+1))

      val sb = new StringBuilder()
      var x = a.size
      var y = b.size
      do {
        if (lengths(x)(y) == lengths(x-1)(y))
          x -= 1
        else if (lengths(x)(y) == lengths(x)(y-1))
          y -= 1
        else {
          assert(a(x-1) == b(y-1))
          sb += a(x-1)
          x -= 1
          y -= 1
        }
      } while (x!=0 && y!=0)
      sb.toString().reverse
    }
  }

  /**
   * Split a list xs by pattern p.
   * @param xs the list
   * @param p a boolean pattern
   * @tparam T type of the list
   * @return
   */
  def splitByPattern[T](xs: List[T])(p: T => Boolean): List[List[T]] = xs match {
    case List() => List()
    case x :: xs1 =>
      val (ys, zs) = xs1 span (!p(_))
      if (zs.isEmpty)
        (x :: ys) :: List()
      else
        (x :: (ys :+ zs.head)) :: splitByPattern(zs.tail)(p)
  }

  /**
   * Zip with three lists.
   *
   * @param l1 the first list
   * @param l2 the second list
   * @param l3 the third list
   * @return
   */
  def zip3(l1 : List[_], l2 : List[_],l3 : List[_]) : List[Tuple3[_, _, _]] =
  {
    def zip3$ (l1$ : List[_], l2$ : List[_], l3$ : List[_], acc : List[Tuple3[_, _, _]]) : List[Tuple3[_, _, _]] = l1$ match
    {
      case Nil                => acc reverse
      case l1$head :: l1$tail => zip3$(l1$tail, l2$.tail, l3$.tail, Tuple3(l1$head, l2$.head, l3$.head) :: acc)
    }

    zip3$(l1, l2, l3, List[Tuple3[_,_,_]]())
  }

}
