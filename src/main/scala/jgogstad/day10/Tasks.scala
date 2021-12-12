package jgogstad.day10

import spire.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.io.file.{Files, Path}
import io.odin.{consoleLogger, Logger}
import spire.math.SafeLong

import scala.annotation.tailrec

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day10/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)

  val parens   = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  val points   = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val t2Points = Map('(' -> 1, '[' -> 2, '{' -> 3, '<' -> 4)

  def parse(s: String): (List[Char], Option[Char]) = {
    @tailrec
    def go(stack: List[Char], remaining: List[Char]): (List[Char], Option[Char]) =
      remaining match {
        case Nil => stack -> None
        case h :: t =>
          if (parens.keySet.contains(h)) go(h :: stack, t)
          else
            stack match {
              case Nil      => stack -> Option(h)
              case sh :: st => if (parens(sh) == h) go(st, t) else stack -> Option(h)
            }
      }
    go(Nil, s.toCharArray.toList)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    def score(list: List[Char]): SafeLong = {
      def go(list: List[Char], acc: SafeLong): SafeLong =
        list match {
          case Nil    => acc
          case h :: t => go(t, acc * 5 + t2Points(h))
        }
      go(list, 0.toSafeLong)
    }

    val task1 = input.map(parse).map(_._2).unNone.map(points).foldMonoid.compile.lastOrError
    val task2 = input
      .map(parse)
      .mapFilter { case (s, c) => c.fold(Option(s))(_ => None) }
      .filter(_.nonEmpty)
      .map(score)
      .compile
      .toList
      .map(l => l.sorted.drop(l.size / 2).head)

    (task1, task2).parTupled.flatTap(i => log.info(s"Task 1, 2: $i")).as(ExitCode.Success)
  }

}
