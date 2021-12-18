package jgogstad.day4

import breeze.linalg._
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.odin.{consoleLogger, Logger}

import scala.annotation.tailrec

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val rows = 5

  type Board = (DenseMatrix[Int], Int) // board and board index

  // set of locations in a board along with markings
  case class ValueLocation(coords: List[(Int, Int)], board: Board, markings: DenseMatrix[Int])

  val input: Stream[IO, String] = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day4/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)

  val numbers: IO[List[Int]] = input.take(1).flatMap(s => Stream.emits(s.split(",").toList)).map(_.toInt).compile.toList
  val boards: IO[List[Board]] = input
    .drop(2)
    .fold((List.empty[DenseMatrix[Int]], List.empty[Int])) {
      case ((ms, acc), "") =>
        (DenseMatrix.create(acc.length / rows, rows, acc.toArray, 0, rows, true) :: ms) -> List.empty
      case ((ms, acc), el) => ms -> (acc ++ el.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toList)
    }
    .flatMap { case (ms, acc) =>
      Stream.emits(DenseMatrix.create(acc.length / rows, rows, acc.toArray, 0, rows, true) :: ms)
    }
    .compile
    .toList
    .map(_.reverse.zipWithIndex)

  override def run(args: List[String]): IO[ExitCode] = {
    (numbers, boards).parTupled.map { case (numbers, boards) =>
      val observations: Map[Int, List[ValueLocation]] = boards
        .map(b => b -> DenseMatrix.ones[Int](b._1.rows, b._1.cols))
        .map { case (b @ (m, _), s) =>
          m.iterator.toList.groupBy(_._2).view.mapValues(v => ValueLocation(v.map(_._1), b, s))
        }
        .foldLeft(Map.empty[Int, List[ValueLocation]]) { (acc, el) =>
          el.iterator.foldLeft(acc) { case (acc, (key, value)) =>
            acc.updatedWith(key) {
              case Some(v) => Some(value :: v)
              case None    => Some(List(value))
            }
          }
        }

      val task1: Option[(Int, ValueLocation)] = runBingo(numbers, observations).flatMap { case (ns, winners) =>
        (ns.lastOption, winners.sortBy(_.board._2).headOption).tupled
      }

      val task2: Option[(Int, ValueLocation)] = lastWinner(numbers, observations, None)

      def score(number: Int, value: ValueLocation): Int = {
        val sum = (value.board._1 *:* value.markings).data.sum
        sum * number
      }

      task1.map((score _).tupled).foreach(a => println(s"Task 1: $a"))
      task2.map((score _).tupled).foreach(a => println(s"Task 2: $a"))

      ExitCode.Success
    }

  }

  private def runBingo(
    numbers: List[Int],
    observedAt: Map[Int, List[ValueLocation]]
  ): Option[(List[Int], List[ValueLocation])] = // Option[ [numbers taken] -> [winners] ]
    numbers.zipWithIndex.collectFirstSome { case (n, index) =>
      val winners = observedAt.get(n).toList.flatMap { locs =>
        locs.map { case v @ ValueLocation(coords, _, markings) =>
          coords.collectFirstSome { case (i, j) =>
            markings.update(i, j, 0)
            val bingoRow    = markings(i, ::).t.toArray.sum
            val bingoColumn = markings(::, j).toArray.sum

            (bingoRow == 0 || bingoColumn == 0).guard[Option].as(v)
          }
        }.flattenOption
      }
      winners match {
        case Nil => None
        case ws  => Some(numbers.take(index + 1) -> ws)
      }

    }

  @tailrec
  private def lastWinner(
    numbers: List[Int],
    observedAt: Map[Int, List[ValueLocation]],
    previous: Option[(Int, ValueLocation)]
  ): Option[(Int, ValueLocation)] =
    runBingo(numbers, observedAt) match {
      case None => previous
      case o @ Some((ns, winners)) =>
        val winnerIds = winners.map(_.board._2).toSet
        (ns.lastOption, winners.sortBy(_.board._2)(Ordering[Int].reverse).headOption).tupled match {
          case Some((n, vl)) =>
            val removeBoard = observedAt.map { case (k, obs) =>
              k -> obs.filter(o => !winnerIds.contains(o.board._2))
            }
            val result = n -> vl
            lastWinner(numbers.drop(ns.length), removeBoard, Some(result))
        }
    }
}
