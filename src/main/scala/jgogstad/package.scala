import cats.Show

import cats.syntax.all._
import breeze.linalg.DenseMatrix

package object jgogstad {
  implicit def showIterable[F[_], A: Show](implicit ev: F[A] <:< IterableOnce[A]): Show[F[A]] = fa => ev(fa).map(_.show).iterator.mkString(",")
  implicit def showMatrix[A: Show]: Show[DenseMatrix[A]] = _.map(_.show).toString
}
