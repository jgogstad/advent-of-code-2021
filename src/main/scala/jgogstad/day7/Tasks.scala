package jgogstad.day7

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.io.file.{Files, Path}
import io.odin.{consoleLogger, Logger}

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input: IO[List[Int]] = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day7/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .map(_.split(",").map(_.toInt).toList)
    .compile
    .lastOrError

  override def run(args: List[String]): IO[ExitCode] = {
    def fuelCost(crabs: List[Int])(cost: (Int, Int) => Int): Int =
      (crabs.min to crabs.max).map(i => crabs.map(cost.curried(i)).sum).min

    input.map(fuelCost).flatMap { cost =>
      val task1 = cost((i, el) => Math.abs(i - el))
      val task2 = cost { (i, el) =>
        val d = Math.abs(i - el)
        (d * (d + 1)) / 2
      }

      log.info(show"Task1: ${task1} Task 2: ${task2}").as(ExitCode.Success)
    }

  }

}
