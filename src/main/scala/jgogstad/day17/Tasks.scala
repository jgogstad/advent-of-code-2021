package jgogstad.day17

import cats.syntax.all._
import jgogstad._
import jgogstad.utils._

object Tasks extends App {

  val input = "target area: x=94..151, y=-156..-103"

  val (landingX, landingY) = input match {
    case s"target area: x=${int(x1)}..${int(x2)}, y=${int(y1)}..${int(y2)}" => (x1 to x2) -> (y1 to y2)
  }

  def shoot(dx: Int, dy: Int): Stream[((Int, Int), Int, Boolean)] = {
    Stream.unfold((0, 0, dx, dy, 0)) { case (x, y, dx, dy, maxY) =>
      val newMax = if (y > maxY) y else maxY
      val next   = (x + dx, y + dy, Math.max(0, dx - 1), dy - 1, newMax)

      if (x > landingX.max || y < landingY.min) None
      else Some((((x, y), newMax, (landingX.contains(x) && landingY.contains(y))), next))
    }
  }

  val attempts = for {
    dx <- 1 to landingX.end
    dy <- -500 to 500 // manually tuned 👍
  } yield shoot(dx, dy).dropWhile(t3 => !t3._3).headOption.map(t => (dx, dy) -> t)

  val landings = attempts.toList.flattenOption

  val task1 = landings.map(_._2._2).max
  val task2 = landings.map(_._1).distinct.size

  println(s"task 1: $task1, task 2: $task2")
}
