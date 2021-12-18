package jgogstad.day18

import fastparse._, NoWhitespace._
import jgogstad._
import jgogstad.utils._
import cats.syntax.all._

import scala.annotation.tailrec
import scala.io.Source

private object Tasks extends App {
  var input = Source.fromResource("day18/input.txt").getLines().toList

  def plus(s1: String, s2: String): String = s"[$s1,$s2]"

  def explode(s: String, pStart: Int, pEnd: Int, a: Int, b: Int): String = {
    val explodeLeft  = "(\\d+)(?=[^\\d])".r
    val explodeRight = "(?<=[^\\d])(\\d+)".r

    val (leftExplode, inserted) = explodeLeft.findAllMatchIn(s.substring(0, pStart)).toList.lastOption match {
      case Some(m) =>
        val next = (m.group(1).toInt + a).toString
        s.patch(m.start, next, m.end - m.start) -> (next.length - (m.end - m.start))
      case None => s -> 0
    }

    val result = explodeRight.findFirstMatchIn(leftExplode.substring(inserted + pEnd)) match {
      case Some(m) => leftExplode.patch(inserted + pEnd + m.start, (m.group(1).toInt + b).toString, m.end - m.start)
      case None    => leftExplode
    }
    result.patch(inserted + pStart, "0", pEnd - pStart + 1)
  }

  def split(s: String, at: Int, value: Int): String =
    s.patch(at, s"[${value / 2},${Math.ceil(value / 2.0).toInt}]", value.toString.length)

  @tailrec
  def reduce(s: String): String = {
    @tailrec
    def doExplosions(depth: Int, i: Int): Option[String] =
      if (i == s.length - 1) None
      else
        s.substring(i) match {
          case e @ s"[${int(a)},${int(b)}]$r" if depth == 4 =>
            Some(explode(s, i, i + (e.length - r.length) - 1, a, b))
          case s"[$_" => doExplosions(depth + 1, i + 1)
          case s"]$_" => doExplosions(depth - 1, i + 1)
          case _      => doExplosions(depth, i + 1)
        }

    @tailrec
    def doSplit(depth: Int, i: Int): Option[String] =
      if (i == s.length - 1) None
      else
        s.substring(i) match {
          case e @ s",${int(a)}]$_" if a > 9 => Some(split(s, i + 1, a))
          case e @ s"[${int(a)},$_" if a > 9 => Some(split(s, i + 1, a))
          case s"[$_"                        => doSplit(depth + 1, i + 1)
          case s"]$_"                        => doSplit(depth - 1, i + 1)
          case _                             => doSplit(depth, i + 1)
        }

    doExplosions(0, 0).orElse(doSplit(0, 0)) match {
      case Some(v) => reduce(v)
      case None    => s
    }
  }

  def magnitude(s: String): Int = {
    def number[_: P]: P[Int] = P(CharIn("0-9").rep(1).!).map(_.toInt)
    def pair[_: P]: P[Int]   = P("[" ~ number ~ "," ~ number ~ "]").map { case (a, b) => a.toInt * 3 + b.toInt * 2 }

    def pairs[_: P]: P[Int] = P("[" ~ (pair | number | pairs) ~ "," ~ (pair | number | pairs) ~ "]").map {
      case (a, b) => 3 * a.toInt + 2 * b.toInt
    }

    parse(s, pairs(_)).get.value
  }

  val task1 = {
    val folded = input.tail.foldLeft(input.head) { case (acc, el) => reduce(plus(acc, el))}
    magnitude(folded)
  }
  val task2 = input.combinations(2).map { case a :: b :: Nil => magnitude(reduce(plus(a, b))) }.max

  println(show"task 1: $task1, task2: $task2")
}
