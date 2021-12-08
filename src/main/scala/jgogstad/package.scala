import cats.Show

package object jgogstad {
  implicit def show[F[_], A](implicit ev: F[A] <:< IterableOnce[A]): Show[F[A]] = fa => ev(fa).mkString(",")
}
