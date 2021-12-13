package jgogstad

import breeze.linalg.DenseMatrix
import cats.syntax.all._
import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec
import scala.util.Try

object utils {
  def clamp(min: Int, max: Int)(i: Int): Int = Math.min(Math.max(i, min), max)

  object char {
    def unapply(s: String): Option[Char] = s.toCharArray.toList match {
      case h :: Nil => Some(h)
      case Nil      => None
    }
  }
  object long     { def unapply(s: String): Option[Long] = s.toLongOption                       }
  object int     { def unapply(s: String): Option[Int] = s.toIntOption                       }
  object safeLong { def unapply(s: String): Option[SafeLong] = s.toLongOption.map(_.toSafeLong) }
  object comma {
    def unapply(s: String): Option[(Int, Int)] = s.split(",").toList match {
      case h :: t :: Nil => (Try(h.toInt).toOption, Try(t.toInt).toOption).tupled
      case _             => None
    }
  }

  object CellularAutomata {
    /**
     * Run cellular automation on a matrix
     *
     * The function `f` is applied to all elements in the neighbourhood with an accumulated value,
     * coordinates and value of the element. Output next accumulated value and new element value
     *
     * @param matrix data matrix
     * @param indexes indexes to run
     * @param mask mask used to expand a neighbourhood around each element in the matrix
     * @param z Accumulation zero value
     * @param f Take accumulation, coordinates, and value at coordinate to new State and value
     *          output new accumulation and new element value. The new accumulation is input to the
     *          next function application.
     */
    def step[A, S](matrix: DenseMatrix[A], mask: DenseMatrix[Boolean])(z: S)(
      f: (S, (Int, Int), A) => (S, A)
    ): (S, DenseMatrix[A]) = stepAccumulate(matrix, matrix.activeKeysIterator.toList, mask)(z)(f)

    /**
     * Run a single cellular automation step on a subset of indexes in a matrix
     *
     * The function `f` is applied to all elements in the neighbourhood with an accumulated value,
     * coordinates and value of the element. Output next accumulated value and new element value
     *
     * @param matrix data matrix
     * @param indexes indexes to run
     * @param mask mask used to expand a neighbourhood around each element in the matrix
     * @param z Accumulation zero value
     * @param f Take accumulation, coordinates, and value at coordinate to new State and value
     *          output new accumulation and new element value. The new accumulation is input to the
     *          next function application.
     */
    def stepAccumulate[A, S](matrix: DenseMatrix[A], indexes: List[(Int, Int)], mask: DenseMatrix[Boolean])(z: S)(
      f: (S, (Int, Int), A) => (S, A)
    ): (S, DenseMatrix[A]) = {
      if (mask.rows % 2 == 0 || mask.cols % 2 == 0) throw new Exception("Need mask with odd number of rows and columns")
      val shiftLeft = mask.cols / 2
      val shiftUp   = mask.rows / 2

      val centeredMask = mask.activeIterator.toList
        .mapFilter { case (c, el) => Option(c).filter(_ => el) }
        .map { case (i, j) => (i - shiftUp, j - shiftLeft) }

      @tailrec
      def go(stack: List[(Int, Int)], s: S, acc: DenseMatrix[A]): (S, DenseMatrix[A]) =
        stack match {
          case Nil => s -> acc
          case (i, j) :: t =>
            val realMask = centeredMask
              .map { case (r, c) => clamp(0, matrix.rows - 1)(i + r) -> clamp(0, matrix.cols - 1)(c + j) }
              .distinct
              .toList
            val s1 = realMask.foldLeft(s) { case (s, (i, j)) =>
              val (s1, a) = f(s, i -> j, acc(i, j))
              acc.update(i, j, a)
              s1
            }
            go(t, s1, acc)
        }

      go(indexes, z, matrix.copy)
    }
  }
}
