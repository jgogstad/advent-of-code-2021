import cats.Show
import cats.syntax.all._
import breeze.linalg.DenseMatrix
import breeze.storage.Zero
import jgogstad.utils.clamp
import org.slf4j.LoggerFactory
import spire.math.SafeLong

import scala.annotation.tailrec

package object jgogstad {
  implicit def showTuple[A: Show, B: Show](t2: Tuple2[A, B]): Show[Tuple2[A, B]] = { case (a, b) => show"($a -> $b)" }
  implicit def showIterable[F[_], A: Show](implicit ev: F[A] <:< IterableOnce[A]): Show[F[A]] = fa => ev(fa).map(_.show).iterator.mkString(",")
  implicit def showMatrix[A: Show]: Show[DenseMatrix[A]] = _.map(_.show).toString
  implicit def showSafeLong: Show[SafeLong] = Show.fromToString[SafeLong]

  val log = LoggerFactory.getLogger("jgogstad")

  implicit class DenseMatrixOps[A](matrix: DenseMatrix[A]) {
    def convolve[S](maskRows: Int, maskCols: Int)(f: ((Int, Int), A, DenseMatrix[A]) => A): DenseMatrix[A] =
      convolveAcc(maskRows, maskCols, ())((_, c, a, m) => () -> f(c, a, m))._2

    def convolveMap[S, B: Zero](mask: DenseMatrix[B])(f: ((Int, Int), A, List[((Int, Int), A)]) => A): DenseMatrix[A] =
      convolveAcc(mask, ())((_, c, a, m) => () -> f(c, a, m))._2

    def convolveTap[S, B: Zero](mask: DenseMatrix[B])(f: ((Int, Int), A, List[((Int, Int), A)]) => A): DenseMatrix[A] = {
      convolveAcc(mask, ())((_, c, a, m) => () -> f(c, a, m))._2
      matrix
    }

    def convolveAcc[S](maskRows: Int, maskCols: Int, z: S)(f: (S, (Int, Int), A, DenseMatrix[A]) => (S, A)): (S, DenseMatrix[A]) = {
      val matrixCopy = matrix.copy

      val s = matrix.activeKeysIterator.foldLeft(z) { case (acc, ij@(i, j)) =>
        val shiftRows = Math.max(0, i - maskRows / 2)
        val shiftCols = Math.max(0, j - maskCols / 2)

        val window = matrix(shiftRows to Math.min(matrix.rows - 1, i + maskRows / 2), shiftCols to Math.min(matrix.cols - 1, j + maskCols / 2))
        val mn = (i - shiftRows, j - shiftCols)
        val (s, a) = f(acc, mn, matrix(ij), window)
        matrixCopy.update(ij, a)
        s
      }
      s -> matrixCopy
    }

    def convolveAcc[S, B: Zero](mask: DenseMatrix[B], z: S)(f: (S, (Int, Int), A, List[((Int, Int), A)]) => (S, A)): (S, DenseMatrix[A]) = {
      val matrixCopy = matrix.copy
      val zero = implicitly[Zero[B]].zero

      val s = matrix.activeKeysIterator.foldLeft(z) { case (acc, ij@(i, j)) =>
        val clampRows = Math.max(0, i - mask.rows / 2)
        val clampCols = Math.max(0, j - mask.cols / 2)

        val window: DenseMatrix[A] = matrix(clampRows to Math.min(matrix.rows - 1, i + mask.rows / 2), clampCols to Math.min(matrix.cols - 1, j + mask.cols / 2))

          val maskUp = Math.abs(Math.min(0, i - mask.rows / 2))
          val maskDown = Math.abs(Math.max(0, i + mask.rows / 2 - (matrix.rows - 1)))
          val maskLeft = Math.abs(Math.min(0, j - mask.cols / 2))
          val maskRight = Math.abs(Math.max(0, j + mask.cols / 2 - (matrix.cols - 1)))

          val shiftedMask = mask(maskUp until (mask.rows - maskDown), maskLeft until (mask.cols - maskRight))

        val list = window.activeIterator.zip(shiftedMask.activeIterator).toList
        val realCoordinates = list.mapFilter {
          case (c, (_, include)) => (include != zero).guard[Option].as(c).map {
            case ((p, q), a) => {
              val vv = ((p - mask.rows / 2 + maskUp + i), (q - mask.cols / 2 + maskLeft + j)) -> a
              vv
            }
          }
        }

        val (s, a) = f(acc, ij, matrix(ij), realCoordinates)
        matrixCopy.update(ij, a)
        s
      }
      s -> matrixCopy
    }

  }
}
