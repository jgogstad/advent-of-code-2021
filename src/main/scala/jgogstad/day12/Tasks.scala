package jgogstad.day12

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.odin.{consoleLogger, Logger}
import jgogstad.utils.{clamp, CellularAutomata}
import jgogstad._
import org.jgrapht.graph.{DefaultUndirectedWeightedGraph, DefaultWeightedEdge, SimpleDirectedGraph, SimpleGraph}
import spire.implicits._
import spire.math.SafeLong

import scala.annotation.tailrec
import scala.collection.immutable.MultiSet
import scala.collection.immutable.MultiDict

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day12/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .map { case s"$from-$to" => from -> to }

  def mod_dfs(
    acc: List[List[String]],
    stack: List[(String, List[String], MultiSet[String])],
    data: MultiDict[String, String]
  ): List[List[String]] = {
    stack match {
      case Nil => acc
      case (h, path, visitedSmall) :: t =>
        if (h == "end") mod_dfs(("end" :: path).reverse :: acc, t, data)
        else {
          val newVisited = if (h.matches("[a-z]+")) visitedSmall + h else visitedSmall
          val twice = newVisited.occurrences.toList.filter(_._2 > 1)

//          task 1
//          val expand = data.get(h).filterNot(s => visitedSmall.contains(s))
          val expand = data.get(h).filter(s => !visitedSmall.contains(s) || twice.isEmpty || twice.exists(t2 => t2._1 == s && t2._2 < 2))

          if (expand.isEmpty) mod_dfs(acc, t, data)
          else mod_dfs(acc, expand.toList.map(el => (el, (h :: path), newVisited)) ++ t, data)
        }
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    input.compile.toList.flatMap { (input: List[(String, String)]) =>
      val edges = input
        .flatMap { case (a, b) => List(a -> b, b -> a) }
        .filterNot { case (a, b) => b == "start" || a == "end" }

      val graph = MultiDict.from(edges)

      val task1 = mod_dfs(Nil, ("start", Nil, MultiSet.empty[String]) :: Nil, graph)
      val task2 = mod_dfs(Nil, ("start", Nil, MultiSet.empty[String]) :: Nil, graph)

      log.info(show"task2: ${task2.size}")
    }.as(ExitCode.Success)

}
