import cats.Show

import cats.syntax.all._
import breeze.linalg.DenseMatrix
import jgogstad.utils.clamp

import scala.annotation.tailrec

package object jgogstad {
  implicit def showIterable[F[_], A: Show](implicit ev: F[A] <:< IterableOnce[A]): Show[F[A]] = fa => ev(fa).map(_.show).iterator.mkString(",")
  implicit def showMatrix[A: Show]: Show[DenseMatrix[A]] = _.map(_.show).toString

//  extension [A](matrix: DenseMatrix[A]) {
//    def convolveAcc[S](z: S)(f: (S, DenseMatrix[A], (Int, Int)) => (S, A)): (S, DenseMatrix[A]) = {
//      val kernel = DenseMatrix
//      val shiftLeft = kernel.cols / 2
//      val shiftUp = kernel.rows / 2
//
//      val centeredMask: List[(Int ,Int)] = kernel.activeIterator.toList
//        .map { case (i, j) => (i - shiftUp, j - shiftLeft) }
//
//      matrix.mapPairs { case ((i, j), a) =>
//        val realMask = centeredMask
//          .map { case (r, c) => clamp(0, matrix.rows - 1)(i + r) -> clamp(0, matrix.cols - 1)(c + j) }
//          .distinct
//          .toList
//        DenseMatrix.
//      }
//
//      @tailrec
//      def go(stack: List[(Int, Int)], s: S, acc: DenseMatrix[A]): (S, DenseMatrix[A]) =
//        stack match {
//          case Nil => s -> acc
//          case (i, j) :: t =>
//            val realMask = centeredMask
//              .map { case (r, c) => clamp(0, matrix.rows - 1)(i + r) -> clamp(0, matrix.cols - 1)(c + j) }
//              .distinct
//              .toList
//            val s1 = realMask.foldLeft(s) { case (s, (i, j)) =>
//              val (s1, a) = f(s, i -> j, acc(i, j))
//              acc.update(i, j, a)
//              s1
//            }
//            go(t, s1, acc)
//        }
//
//      go(indexes, z, matrix.copy)
//
//    }
//  }
}
