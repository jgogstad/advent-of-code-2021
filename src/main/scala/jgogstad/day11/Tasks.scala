package jgogstad.day11

import fs2.Stream
import spire.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.io.file.{Files, Path}
import io.odin.{consoleLogger, Logger}
import spire.math.SafeLong
import jgogstad.utils.{clamp, CellularAutomata}
import breeze.linalg._
import jgogstad._

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
    val mask = DenseMatrix.ones[Boolean](3, 3)
    mask.update(1, 1, false)

    // run one full iteration of flashes
    @tailrec
    def flashIteration(acc: Int, data: DenseMatrix[Int], toFlash: List[(Int, Int)]): (Int, DenseMatrix[Int]) = {
      toFlash.foreach { case (i, j) => data.update(i, j, 0) }

      val (moreFlashes, next) = CellularAutomata.stepAccumulate(data, toFlash, mask)(List.empty[(Int, Int)]) {
        case (flashes, (i, j), el) =>
          if (el >= 9) (((i -> j) :: flashes).distinct) -> (el + 1)
          else if (el == 0) flashes -> 0
          else (flashes, (el + 1))
      }

      moreFlashes match {
        case Nil => (acc + toFlash.size) -> next
        case l => {
          flashIteration(acc + toFlash.size, next, moreFlashes)
        }
      }
    }

    Stream.unfoldLoop(matrix) { data =>
      val copy = data.copy
      val plusOne = copy.map(_ + 1)
      val toFlash = plusOne.findAll(_ > 9)
      val out@(_, next) = flashIteration(0, plusOne, toFlash.toList)
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
