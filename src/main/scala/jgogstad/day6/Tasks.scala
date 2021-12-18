package jgogstad.day6

import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.{Files, Path}
import io.odin.{consoleLogger, Logger}
import spire.implicits._
import spire.math.SafeLong

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input: IO[List[(Int, SafeLong)]] = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day6/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .map(_.split(",").map(_.toInt -> 1.toSafeLong).toList)
    .compile
    .lastOrError

  val tick: Map[Int, SafeLong] => List[(Int, SafeLong)] = _.toList.flatMap {
    case (k, v) if k == 0 => List(6 -> v, 8 -> v)
    case (k, v)           => List(k - 1 -> v)
  }

  val group: List[(Int, SafeLong)] => Map[Int, SafeLong] =
    _.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2).reduce(_ + _) }.toMap

  val evolve = group compose tick

  def solve(gen: Int, fish: Map[Int, SafeLong]): Map[Int, SafeLong] =
    if (gen == 0) fish else solve(gen - 1, evolve(fish))

  override def run(args: List[String]): IO[ExitCode] =
    input
      .map(fish => solve(256, group(fish)).values.reduce(_ + _).toString())
      .flatMap(log.info(_))
      .as(ExitCode.Success)

}
