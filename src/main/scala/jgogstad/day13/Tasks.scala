package jgogstad.day13

import breeze.linalg.{DenseMatrix, _}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.io.file.{Files, Path}
import io.odin.{consoleLogger, Logger}
import jgogstad.utils._

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  type Instruction = (String, Int)

  val input = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day13/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .collect {
      case s"${int(a)},${int(b)}"        => Left(a -> b)
      case s"fold along $axis=${int(n)}" => Right(axis -> n)
    }

  def foldCol(m: DenseMatrix[Int], col: Int): DenseMatrix[Int] = {
    val left     = m(::, 0 until col)
    val right    = m(::, (col + 1) until m.cols)
    val flipLeft = fliplr(left)

    val alignRight = DenseMatrix.horzcat(right, DenseMatrix.zeros[Int](m.rows, Math.max(0, left.cols - right.cols)))
    val alignLeft  = DenseMatrix.horzcat(flipLeft, DenseMatrix.zeros[Int](m.rows, Math.max(0, right.cols - left.cols)))

    alignLeft + alignRight
  }

  def foldRow(m: DenseMatrix[Int], row: Int): DenseMatrix[Int] = {
    val upper = m(0 until row, ::)
    val lower = m((row + 1) until m.rows, ::)

    flipud(lower) + upper
  }

  def applyInstructions(matrix: DenseMatrix[Int], instructions: List[Instruction]): DenseMatrix[Int] =
    instructions.foldLeft(matrix) {
      case (matrix, ("x", axis)) => foldCol(matrix, axis)
      case (matrix, ("y", axis)) => foldRow(matrix, axis)
    }

  override def run(args: List[String]): IO[ExitCode] =
    input.compile.toList
      .flatMap { input =>
        val (locations, instructions) = input.separate

        val (cols, rows) = locations.sortBy(_._1).last._1 -> locations.sortBy(_._2).last._2
        val matrix       = DenseMatrix.zeros[Int](rows + 1, cols + 1)
        locations.foreach { case r => matrix.update(r.swap, 1) }

        val task1 = applyInstructions(matrix, instructions.take(1)).findAll(_ > 0).size
        val task2 = applyInstructions(matrix, instructions).map(i => i / Math.max(i, 1))

        log.info(show"task1: $task1") *> log.info(show"task2: \n${task2.toString(10, 200)}")
      }
      .as(ExitCode.Success)

}
