package jgogstad.day8

import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.odin.{Logger, consoleLogger}
import jgogstad.utils._
import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.immutable.MultiSet

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input: Stream[IO, (List[String], List[String])] = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day8/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .map(_.split('|').map(_.trim))
    .flatMap {
      case Array(patterns, segments) =>
        Stream.emit(patterns.split(' ').map(_.trim).toList -> segments.split(' ').map(_.trim).toList)
    }

  val segmentsToInt = Map(
    "abcefg"  -> '0',
    "cf"      -> '1',
    "acdeg"   -> '2',
    "acdfg"   -> '3',
    "bcdf"    -> '4',
    "abdfg"   -> '5',
    "abdefg"  -> '6',
    "acf"     -> '7',
    "abcdefg" -> '8',
    "abcdfg"  -> '9'
  )

  override def run(args: List[String]): IO[ExitCode] = {
    val task1 = input
      .flatMap(t2 => Stream.emits(t2._2))
      .fold(MultiSet.empty[String]: MultiSet[String])(_ + _)
      .map { ms =>
        val subset    = ms.filter(s => Set(2, 3, 4, 7).contains(s.length))
        val histogram = subset.occurrences.toList.groupBy(_._1.length).view.mapValues(_.map(_._2).sum)
        histogram.toList.map(_._2).sum
      }
      .compile
      .lastOrError
      .flatTap(i => log.info(show"Task 1: $i"))

    def solve(patterns: List[String], digits: List[String]): Either[String, Int] = {
      val multi  = decodeToMultiple(patterns) _
      val single = decodeToSingle(patterns) _
      val expectSingle: Set[Char] => Either[String, Char] = _.toList match {
        case h :: Nil  => Right(h)
        case l => Left(show"Expected single element, but got $l")
      }

      val translate = for {
        cf <- multi(2, Set.empty)
        a  <- single(3, cf)                   // 7
        bd <- multi(4, cf)                    // 4
        g  <- single(6, cf ++ bd + a)         // 9
        e  <- single(7, cf ++ bd + a + g)     // 8
        b  <- single(6, cf + a + g + e)       // 0
        f  <- single(5, (bd - b) + a + b + g) // 5
      } yield {
        val cm = expectSingle(cf - f)
        val dm = expectSingle(bd - b)

        (cm, dm).tupled.map { case (c, d) =>
          Map(a -> 'a', b -> 'b', c -> 'c', d -> 'd', e -> 'e', f -> 'f', g -> 'g')
        }
      }

      translate.flatten.map { translate =>
        digits.map(_.map(translate).mkString.sorted).map(segmentsToInt).mkString.toInt
      }
    }

    val task2 = input
      .evalMap { case (pattern, digits) => solve(pattern, digits).leftMap(new Exception(_)).liftTo[IO] }
      .foldMonoid
      .compile
      .lastOrError
      .flatTap(i => log.info(show"Task 2: $i"))

    (task1, task2).parTupled.as(ExitCode.Success)
  }

  def decodeToMultiple(data: List[String])(lookup: Int, required: Set[Char]): Either[String, Set[Char]] =
    data.filter(_.length == lookup).filter(s => s.toSet.intersect(required).size == required.size) match {
      case Nil => Left(show"No pattern found with length $lookup, required $required")
      case h :: Nil =>
        NonEmptySet
          .fromSet(SortedSet.from(h.toSet diff required))
          .toRight(show"Empty set after diff, lookup $lookup, required $required")
          .map(_.toSortedSet)
      case l => Left(show"Multiple patterns found, length $lookup, required $required")
    }

  def decodeToSingle(data: List[String])(lookup: Int, required: Set[Char]): Either[String, Char] =
    decodeToMultiple(data)(lookup, required)
      .map(_.toList)
      .flatMap {
        case h :: Nil => Right(h)
        case Nil      => Left(show"No elements left after diff, lookup $lookup, required $required")
        case l        => Left(show"Multiple elements left after diff, lookup $lookup, required $required")
      }
}
