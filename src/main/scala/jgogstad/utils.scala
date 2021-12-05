package jgogstad

import cats.syntax.all._
import spire.math.SafeLong
import spire.implicits._

import scala.util.Try

object utils {
  object long { def unapply(s: String): Option[Long] = s.toLongOption }
  object safeLong { def unapply(s: String): Option[SafeLong] = s.toLongOption.map(_.toSafeLong) }
  object comma {
    def unapply(s: String): Option[(Int, Int)] = s.split(",").toList match {
      case h :: t :: Nil => (Try(h.toInt).toOption, Try(t.toInt).toOption).tupled
      case _             => None
    }
  }
}
