package jgogstad

import breeze.linalg.DenseMatrix
import breeze.math.Semiring
import breeze.storage.Zero
import cats.kernel.Group
import cats.syntax.all._
import spire.ClassTag
import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec
import scala.util.Try

object utils {
  def clamp(min: Int, max: Int)(i: Int): Int = Math.min(Math.max(i, min), max)

  object char {
    def unapply(s: String): Option[Char] = {
      s.toCharArray.toList match {
        case h :: Nil => Some(h)
        case Nil      => None
      }
    }
  }
  object chars    { def unapplySeq(s: String): Option[Seq[Char]] = if (s.nonEmpty) Some(s.toCharArray.toSeq) else None }
  object long     { def unapply(s: String): Option[Long] = s.toLongOption                                              }
  object int      { def unapply(s: String): Option[Int] = s.toIntOption                                                }
  object safeLong { def unapply(s: String): Option[SafeLong] = s.toLongOption.map(_.toSafeLong)                        }
  object comma {
    def unapply(s: String): Option[(Int, Int)] = s.split(",").toList match {
      case h :: t :: Nil => (Try(h.toInt).toOption, Try(t.toInt).toOption).tupled
      case _             => None
    }
  }

  object Masks {

    /**
     * Example 3x3
     *
     * 0 1 0
     * 1 0 1
     * 0 1 0
     */
    def emptyCross[A: ClassTag: Zero: Semiring](rows: Int, cols: Int): DenseMatrix[A] = {
      val m      = cross[A](rows, cols)
      val middle = (rows / 2, cols / 2)
      m.update(middle, implicitly[Zero[A]].zero)
      m
    }

    /**
     * Example 3x3
     *
     * 0 1 0
     * 1 1 1
     * 0 1 0
     */
    def cross[A: ClassTag: Zero: Semiring](rows: Int, cols: Int): DenseMatrix[A] = {
      if (rows % 2 == 0 || cols % 2 == 0) throw new Exception("Need odd numbered rows and columns for cross mask")
      val rowMiddle = rows / 2
      val colMiddle = cols / 2

      val m   = DenseMatrix.zeros[A](rows, cols)
      val one = implicitly[Semiring[A]].one

      for (i <- 0 until cols) m.update(rowMiddle, i, one)
      for (i <- 0 until rows) m.update(i, colMiddle, one)
      m
    }
  }

  /**
   * lhs % rhs to the range [1,rhs]
   */
  def mod1(lhs: Int, rhs: Int): Int = (lhs - 1) % rhs + 1
}
