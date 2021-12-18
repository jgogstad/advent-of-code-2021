package jgogstad.day14

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.odin.{consoleLogger, Logger}
import jgogstad.utils._
import jgogstad._
import spire.implicits._
import spire.math.SafeLong

import scala.collection.MapView

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  type Instruction = (String, Int)

  val input = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day14/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .collect {
      case s"$l -> $r"     => Right(l -> r)
      case s if s.nonEmpty => Left(s)
    }

  override def run(args: List[String]): IO[ExitCode] =
    input.compile.toList
      .flatMap { input =>
        val (template: String, instructions: MapView[String, Char]) =
          input.separate.bimap({ case h :: Nil => h }, _.toMap.view.mapValues { case s"${char(c)}" => c })

        val pairs     = template.sliding(2).map(_ -> 1.toSafeLong).toMap
        val frequency = template.toCharArray.groupBy(identity).map { case (c, cs) => c -> cs.length.toSafeLong }.toMap

        val stream: Stream[IO, Map[Char, SafeLong]] = Stream
          .unfoldLoop(pairs -> frequency) { case (pairs, frequency) =>
            val next = pairs.toList.foldLeft(Map.empty[String, SafeLong] -> frequency) {
              case ((acc, frequency), (ab @ s"${chars(a, b)}", v)) =>
                val middle = instructions(ab)
                val left   = show"$a$middle"
                val right  = show"$middle$b"

                val leftOcc  = acc.get(left).getOrElse(0.toSafeLong)
                val rightOcc = acc.get(right).getOrElse(0.toSafeLong)

                val newFrequency = frequency.updatedWith(middle)(_.orElse(0.toSafeLong.some).map(_ + v))

                if (left == right) acc.updated(left, leftOcc + v * 2)            -> newFrequency
                else acc.updated(left, leftOcc + v).updated(right, rightOcc + v) -> newFrequency
            }

            next._2 -> Some(next)
          }
          .covary[IO]

        (stream.take(10), stream.take(40)).tupled.compile.lastOrError.flatMap { case (t1, t2) =>
          def result(m: Map[Char, SafeLong]): SafeLong = m.toList.sortBy(_._2).map(_._2) match {
            case h :: t => t.last - h
          }

          log.info(show"Task 1: ${result(t1)}, task2: ${result(t2)}")
        }
      }
      .as(ExitCode.Success)

}
