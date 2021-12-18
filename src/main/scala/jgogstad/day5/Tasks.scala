package jgogstad.day5

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.odin.{consoleLogger, Logger}
import jgogstad.utils._

import scala.annotation.tailrec
import scala.collection.immutable.MultiSet

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day5/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .evalMap {
      case s"${comma(x1, y1)} -> ${comma(x2, y2)}" => ((x1, y1), (x2, y2)).pure[IO]
      case s                                       => IO.raiseError(new Exception(s"Unknown input: ${s}"))
    }

  def expand(p1: (Int, Int), p2: (Int, Int)): Stream[IO, (Int, Int)] = {
    @tailrec
    def go(acc: List[(Int, Int)], p1: (Int, Int), p2: (Int, Int)): List[(Int, Int)] = {
      if (p1 === p2) acc
      else {
        val diffX = p2._1 - p1._1
        val diffY = p2._2 - p1._2
        val stepX = diffX / Math.max(1, Math.abs(diffX))
        val stepY = diffY / Math.max(1, Math.abs(diffY))

        val next = (p1._1 + stepX) -> (p1._2 + stepY)
        go(next :: acc, next, p2)
      }
    }
    Stream.emits(go(p1 :: Nil, p1, p2))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val task1 = input
      .filter { case ((a, b), (c, d)) =>
        a == c || b == d
      }
      .flatMap((expand _).tupled)

    val task2 = input.flatMap((expand _).tupled)

    (solve(task1), solve(task2)).parTupled.flatMap(t2 => log.info(t2.show)).as(ExitCode.Success)
  }

  def solve(input: Stream[IO, (Int, Int)]): IO[Int] =
    input
      .fold(MultiSet.empty[(Int, Int)]: MultiSet[(Int, Int)]) { case (acc, el) => acc + el }
      .map(_.occurrences.filter(_._2 >= 2).size)
      .compile
      .lastOrError
}
