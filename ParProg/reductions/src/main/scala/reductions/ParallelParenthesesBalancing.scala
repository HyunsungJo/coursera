package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var acc = 0
    for (char <- chars) {
      char match {
        case c: Char if (c == '(') => acc = acc + 1
        case c: Char if (c == ')') => acc = acc - 1
        case _ => // do nothing
      }
      if (acc < 0) return false
    }
    acc == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var i = idx
      var (total, least) = (0, 0)
      while (i < until) {
        chars(i) match {
          case c: Char if (c == '(') => total = total + 1
          case c: Char if (c == ')') => total = total - 1
          case _ => // do nothing
        }
        least = math.min(least, total)
        i = i + 1
      }
      (total, least)
    }

    def reduce(from: Int, until: Int): (Int, Int)  = {
      println("="*20)
      val rng = until - from
      val mid = from + math.ceil((until.toDouble - from.toDouble) / 2).toInt
      val (a, b) = rng match {
        case i: Int if (i <= threshold) =>
          val (a, b) = traverse(from, until, 0, 0)
          (a, b)
        case i: Int if (i == chars.length) =>
          val (pair1, pair2) = parallel(reduce(from, mid), reduce(mid, until))
          val total = pair1._1 + pair2._1
          val verify = pair1._1 + pair2._2
          (total, verify)
        case _ =>
          val (pair1, pair2) = parallel(reduce(from, mid), reduce(mid, until))
          val total = pair1._1 + pair2._1
          val min = math.min(math.min(pair1._2, pair2._2), total)
          (total, min)
      }
      (a, b)
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
