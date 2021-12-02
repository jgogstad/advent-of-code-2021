package jgogstad

import spire.math.SafeLong
import spire.implicits._

object utils {
  object long { def unapply(s: String): Option[Long] = s.toLongOption }
  object safeLong { def unapply(s: String): Option[SafeLong] = s.toLongOption.map(_.toSafeLong) }
}
