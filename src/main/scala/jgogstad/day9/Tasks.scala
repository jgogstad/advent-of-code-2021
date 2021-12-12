package jgogstad.day9

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.io.file.{Files, Path}
import io.odin.{Logger, consoleLogger}
import jgogstad.utils.clamp

import scala.annotation.tailrec

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day9/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .map(_.toCharArray.map(_.toString.toInt))

    override def run(args: List[String]): IO[ExitCode] = {
    input.compile.toList.flatMap { list =>
      val mask = List((0, 1), (0, -1), (1, 0), (-1, 0))
      val cols = list.head.length

      def applyMask(i: Int, j: Int): List[((Int, Int), Int)] =
        mask
          .map { case (x, y) => (i + x) -> (j + y) }
          .map { case (x, y) => clamp(0, list.length - 1)(x) -> clamp(0, cols - 1)(y) }
          .filter { case (x, y) => x != i || y != j }
          .map { case (x, y) => (x, y) -> list(x)(y) }

      @tailrec
      def basin(i: Int, j: Int, stack: List[(Int, Int)], visited: Set[(Int, Int)], basins: List[Array[(Set[(Int, Int)])]]): Set[(Int, Int)] = {
        val el       = list(i)(j)
        val ns = applyMask(i, j).filter(_._2 > el).filter { case (t2, _) => !visited.contains(t2)}
        val (l, r) = ns.map { case l @ ((x, y), _) => Option(basins(x)(y)).filter(_.nonEmpty).toRight(l) }.separate

        val newVisisted = if (el == 9) visited else visited ++ (r.toSet.flatten) + (i -> j)
        val newStack = l.map(_._1) ++ stack
        newStack match {
          case Nil         => newVisisted
          case (x, y) :: t => basin(x, y, t, newVisisted, basins)
        }
      }

      val task1 = list.zipWithIndex
        .map { case (row, i) =>
          row.zipWithIndex.map { case (el, j) =>
            applyMask(i, j).map(_._2).forall(_ > el).guard[Option].as(el).map(_ + 1).getOrElse(0)
          }
        }
        .map(_.sum)
        .sum

      val task2 = {
        val memo = List.fill(list.length)(Array.fill(cols)(Set.empty[(Int, Int)]))
        val basins = list.zipWithIndex.map { case (row, i) =>
          row.zipWithIndex.map { case (el, j) =>
            val members = basin(i, j, Nil, Set.empty, memo)
            memo(i).update(j, members)
            members.size
          }
        }
        basins.flatMap(_.toList).sorted.takeRight(3).reduce(_ * _)
      }

      log.info(show"Task1: $task1, task2: $task2").as(ExitCode.Success)
    }
  }

}
