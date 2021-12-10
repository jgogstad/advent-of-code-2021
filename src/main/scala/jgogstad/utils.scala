package jgogstad

import cats.syntax.all.*
import spire.math.SafeLong
import spire.implicits.*

import scala.util.Try

object utils {
  object char {
    def unapply(s: String): Option[Char] = s.toCharArray.toList match {
      case h :: _ => Some(h)
      case Nil      => None
    }
  }
  object long     { def unapply(s: String): Option[Long] = s.toLongOption                       }
  object safeLong { def unapply(s: String): Option[SafeLong] = s.toLongOption.map(_.toSafeLong) }
  object comma {
    def unapply(s: String): Option[(Int, Int)] = s.split(",").toList match {
      case h :: t :: Nil => (Try(h.toInt).toOption, Try(t.toInt).toOption).tupled
      case _             => None
    }
  }
}
