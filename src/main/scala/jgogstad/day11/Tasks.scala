package jgogstad.day11

import breeze.linalg._
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.odin.{consoleLogger, Logger}
import jgogstad._
import spire.implicits._

import scala.annotation.tailrec

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input: IO[DenseMatrix[Int]] = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day11/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .map(_.split("").map(_.toInt))
    .foldMonoid
    .map(data => DenseMatrix.create(10, 10, data, 0, 10, true))
    .compile
    .lastOrError

  def evolve(matrix: DenseMatrix[Int]): Stream[IO, (Int, DenseMatrix[Int])] = {
    // run one full iteration of flashes
    @tailrec
    def flashIteration(acc: Int, data: DenseMatrix[Int]): (Int, DenseMatrix[Int]) = {
      val flashed = data.findAll(_ > 9)

      val next = data.convolve(3, 3) { case (ij, el, neighbours) =>
        val flashes = neighbours.activeIterator.filter(_._1 != ij).filter(_._2 > 9)
        if (el > 9 || el == 0) 0 else el + flashes.size
      }

      if (next.findAll(_ > 9).nonEmpty) flashIteration(acc + flashed.size, next) else (acc + flashed.size) -> next
    }

    Stream.unfoldLoop(matrix) { data =>
      val copy            = data.copy
      val plusOne         = copy.map(_ + 1)
      val out @ (_, next) = flashIteration(0, plusOne)
      out -> Some(next)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    input
      .flatMap { l =>
        val task1 = evolve(l).map(_._1).take(100).foldMonoid.compile.lastOrError
        val task2 = evolve(l).zipWithIndex
          .dropWhile { case ((flashes, matrix), _) => flashes != matrix.size }
          .map(_._2 + 1)
          .take(1)
          .compile
          .lastOrError

        (task1, task2).parTupled.flatMap(t2 => log.info(show"Task 1 and 2: $t2"))
      }
      .as(ExitCode.Success)
  }

}
