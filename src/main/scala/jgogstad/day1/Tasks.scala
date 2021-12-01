package jgogstad.day1

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import io.odin.{consoleLogger, Logger}
import jgogstad.utils.FileIo

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input = FileIo.contentsOf[IO]("day1/input.txt").evalMap(i => IO(i.toInt))

  override def run(args: List[String]): IO[ExitCode] = {
    val countIncreasing = (s: Stream[IO, Int]) =>
      s.zipWithNext.mapFilter { case (i, next) =>
        next.map(_ - i).filter(_ > 0)
      }

    val task1 = input.through(countIncreasing).compile.count
    val task2 = input.sliding(3).map(_.toList.sum).through(countIncreasing).compile.count

    (task1, task2).parTupled.flatTap(log.info(_)).as(ExitCode.Success)
  }
}
