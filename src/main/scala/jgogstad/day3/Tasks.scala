package jgogstad.day3

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.odin.{Logger, consoleLogger}
import scodec.bits._

import scala.annotation.tailrec

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input: Stream[IO, String] = Files[IO]
    .readAll(Path("src/main/resources/day3/input.txt"))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)

  val inputVectors: Stream[IO, BitVector] = input.evalMap(s => IO(BitVector.fromValidBin(s)))

  override def run(args: List[String]): IO[ExitCode] =
    (task1, task2).parTupled.flatMap { case (t1, t2) => log.info(s"$t1 $t2") }.as(ExitCode.Success)

  val task1: IO[Long] = {
    val count        = input.compile.count
    val countBits = input
      .map(_.split("").map(_.toInt).toList)
      .reduce { (a, b) => a.zip(b).map { case (a, b) => a + b } }
      .compile
      .lastOrError

    (count, countBits).parTupled.map { case (count, bitCounts) =>
      val gamma   = BitVector.bits(bitCounts.map(i => i > (count - i)))
      val epsilon = BitVector.bits(bitCounts.map(i => i < (count - i)))

      gamma.toLong(signed = false) * epsilon.toLong(signed = false)
    }
  }

  val task2 =  {
    val vectorLength = input.take(1).compile.lastOrError.map(_.length)

    (vectorLength, inputVectors.compile.toList).tupled.map { case (vectorLength, inputs) =>
        @tailrec
        def reduce(index: Int, remaining: List[BitVector], op: (Int, Int) => Boolean): Option[BitVector] = {
          val ones  = remaining.map(_.apply(index)).map(b => if (b) 1 else 0).sum
          val zeros = remaining.length - ones

          remaining.filter(_.apply(index) == op(ones, zeros)) match {
            case Nil                           => None
            case h :: Nil                      => Some(h)
            case l if index + 1 < vectorLength => reduce(index + 1, l, op)
            case _                             => None
          }
        }

        val ogr      = reduce(0, inputs, _ >= _)
        val scrubber = reduce(0, inputs, _ < _)

        (ogr, scrubber).tupled.map { case (ogr, scrubber) => ogr.toLong(false) * scrubber.toLong(false) }
    }
  }
}
