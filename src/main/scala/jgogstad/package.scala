import cats.Show
import cats.syntax.all._
import breeze.linalg.DenseMatrix
import jgogstad.utils.clamp
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

package object jgogstad {
  implicit def showIterable[F[_], A: Show](implicit ev: F[A] <:< IterableOnce[A]): Show[F[A]] = fa => ev(fa).map(_.show).iterator.mkString(",")
  implicit def showMatrix[A: Show]: Show[DenseMatrix[A]] = _.map(_.show).toString

  val log = LoggerFactory.getLogger("jgogstad")

  implicit class DenseMatrixOps[A](matrix: DenseMatrix[A]) {
    def convolve[S](maskRows: Int, maskCols: Int)(f: ((Int, Int), A, DenseMatrix[A]) => A): DenseMatrix[A] =
      convolveAcc(maskRows, maskCols, ())((_, c, a, m) => () -> f(c, a, m))._2

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
  }
}
