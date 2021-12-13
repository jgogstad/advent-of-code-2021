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
}
