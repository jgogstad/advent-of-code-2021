package jgogstad.day1

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.odin.{Logger, consoleLogger}

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input: Stream[IO, Int] = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day1/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .evalMap(i => IO(i.toInt))

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
