package jgogstad.day2

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.odin.{Logger, consoleLogger}
import jgogstad.utils.*
import spire.implicits.*
import spire.math.{Complex, SafeLong}

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input: Stream[IO, String] = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day2/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)

  override def run(args: List[String]): IO[ExitCode] = {
    val toComplex = input.evalMap {
      case s"forward ${safeLong(n)}" => Complex(n, 0.toSafeLong).pure[IO]
      case s"down ${safeLong(n)}"    => Complex(0.toSafeLong, n).pure[IO]
      case s"up ${safeLong(n)}"      => Complex(0.toSafeLong, n * -1).pure[IO]
      case s                         => IO.raiseError[Complex[SafeLong]](new Exception(s"Unknown input $s"))
    }

    val task1 = toComplex.reduce(_ + _)

    val task2 = toComplex
      .fold(0.toSafeLong -> Complex(0.toSafeLong, 0.toSafeLong)) { case ((aim, position), Complex(real, imag)) =>
        (aim + imag) -> (position + Complex(real, aim * real))
      }
      .map(_._2)

    val run: (Stream[IO, Complex[SafeLong]]) => IO[String] =
      _.compile.lastOrError.map(c => c.real * c.imag).map(_.toString).flatTap(log.info(_))

    (run(task1), run(task2)).parTupled.as(ExitCode.Success)
  }
}
