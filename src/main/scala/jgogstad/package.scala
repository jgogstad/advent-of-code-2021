import cats.Show
import cats.syntax.all._
import breeze.linalg.DenseMatrix
import breeze.math.Field
import breeze.storage.Zero
import jgogstad.utils.clamp
import org.slf4j.LoggerFactory
import spire.ClassTag
import spire.math.SafeLong

import scala.annotation.tailrec

package object jgogstad {
  implicit def showTuple[A: Show, B: Show](t2: Tuple2[A, B]): Show[Tuple2[A, B]] = { case (a, b) => show"($a -> $b)" }
  implicit def showIterable[F[_], A: Show](implicit ev: F[A] <:< IterableOnce[A]): Show[F[A]] = fa =>
    ev(fa).map(_.show).iterator.mkString(",")
  implicit def showMatrix[A: Show]: Show[DenseMatrix[A]] = _.map(_.show).toString
  implicit def showSafeLong: Show[SafeLong]              = Show.fromToString[SafeLong]

  val log = LoggerFactory.getLogger("jgogstad")

  implicit class DenseMatrixOps[A: ClassTag: Field](matrix: DenseMatrix[A]) {
    def convolve[S](maskRows: Int, maskCols: Int)(f: ((Int, Int), A, DenseMatrix[A]) => A): DenseMatrix[A] =
      convolveAcc(maskRows, maskCols, ())((_, c, a, m) => () -> f(c, a, m))._2

    def convolveMap[S, B: Zero](mask: DenseMatrix[B], default: Option[A] = None)(f: ((Int, Int), A, List[((Int, Int), A)]) => A): DenseMatrix[A] =
      convolveAcc(mask, default, ())((_, c, a, m) => () -> f(c, a, m))._2

    def convolveAcc[S](maskRows: Int, maskCols: Int, z: S)(
      f: (S, (Int, Int), A, DenseMatrix[A]) => (S, A)
    ): (S, DenseMatrix[A]) = {
      val matrixCopy = matrix.copy

      val s = matrix.activeKeysIterator.foldLeft(z) { case (acc, ij @ (i, j)) =>
        val shiftRows = Math.max(0, i - maskRows / 2)
        val shiftCols = Math.max(0, j - maskCols / 2)

        val window = matrix(
          shiftRows to Math.min(matrix.rows - 1, i + maskRows / 2),
          shiftCols to Math.min(matrix.cols - 1, j + maskCols / 2)
        )
        val mn     = (i - shiftRows, j - shiftCols)
        val (s, a) = f(acc, mn, matrix(ij), window)
        matrixCopy.update(ij, a)
        s
      }
      s -> matrixCopy
    }

    def convolveAcc[S, B: Zero](mask: DenseMatrix[B], default: Option[A], z: S)(
      f: (S, (Int, Int), A, List[((Int, Int), A)]) => (S, A)
    ): (S, DenseMatrix[A]) = {
      val matrixCopy = matrix.copy
      val zero       = implicitly[Zero[B]].zero

      val s = matrix.activeKeysIterator.foldLeft(z) { case (acc, ij @ (i, j)) =>
        val maskRowRadius = mask.rows / 2
        val maskColRadius = mask.cols / 2

        val clampRows = Math.max(0, i - maskRowRadius)
        val clampCols = Math.max(0, j - maskColRadius)

        val window: DenseMatrix[A] = matrix(
          clampRows to Math.min(matrix.rows - 1, i + maskRowRadius),
          clampCols to Math.min(matrix.cols - 1, j + maskColRadius)
        )

        val maskUp    = Math.abs(Math.min(0, i - maskRowRadius))
        val maskDown  = Math.abs(Math.max(0, i + maskRowRadius - (matrix.rows - 1)))
        val maskLeft  = Math.abs(Math.min(0, j - maskColRadius))
        val maskRight = Math.abs(Math.max(0, j + maskColRadius - (matrix.cols - 1)))

        val alignedMask = mask(maskUp until (mask.rows - maskDown), maskLeft until (mask.cols - maskRight))
        val windowMaskPairs = window.activeIterator.zip(alignedMask.activeIterator).toList

        val outUp = mask(0 until maskUp, ::)
        val outLeft = mask(::, 0 until maskLeft)
        val outDown = mask((mask.rows - maskDown) until mask.rows, ::)
        val outRight = mask(::, (mask.cols - maskRight) until mask.cols)

        val overlappingCoordinates = windowMaskPairs.mapFilter { case (c, (_, include)) =>
          (include != zero).guard[Option].as(c).map {
            case ((p, q), a) => ((p - maskRowRadius + maskUp + i), (q - maskColRadius + maskLeft + j)) -> a
          }
        }

        val overflowCoordinates = default.fold(List.empty[((Int, Int), A)]) { d =>
          val upLeft = (outUp.activeIterator ++ outLeft.activeIterator).map {
            case ((r, c), v) => (r - maskRowRadius + i, c - maskColRadius + j) -> d
          }.toList
          val downRight = (outDown.activeIterator ++ outRight.activeIterator).map {
            case ((r, c), v) => (r - maskRowRadius + maskDown + i, c - maskColRadius + maskRight + j) -> d
          }.toList
          (upLeft ++ downRight).toList
        }.toList.distinct

        val (s, a) = f(acc, ij, matrix(ij), overlappingCoordinates ++ overflowCoordinates)
        matrixCopy.update(ij, a)
        s
      }
      s -> matrixCopy
    }

    /**
     * Pad the matrix with el
     *
     * Pads left, right, up, down
     */
    def pad(n: Int, el: A): DenseMatrix[A] = {
      val rows = DenseMatrix.create(n, matrix.cols, Array.fill(matrix.cols * n)(el), 0, matrix.cols, true)
      val m1 = DenseMatrix.vertcat(rows, matrix, rows)
      val cols = DenseMatrix.create(m1.rows, n, Array.fill(m1.rows * n)(el))
      DenseMatrix.horzcat(cols, m1, cols)
    }
  }
}
