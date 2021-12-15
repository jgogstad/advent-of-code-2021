package jgogstad.day15

import breeze.linalg._
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.{Chunk, Pipe, Stream}
import fs2.io.file.{Files, Path}
import io.odin.{consoleLogger, Logger}
import jgogstad.utils._
import jgogstad._
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.{DefaultDirectedWeightedGraph, DefaultEdge, DefaultWeightedEdge, SimpleWeightedGraph}
import spire.implicits._
import spire.math.SafeLong

import scala.jdk.CollectionConverters._
import scala.collection.MapView
import scala.util.{Left, Right}

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  val input: Stream[IO, DenseMatrix[Int]] = Files[IO]
    .readAll(Path(getClass.getClassLoader.getResource("day15/input.txt").getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .map(List.apply(_))
    .foldMonoid
    .map { case rows @ h :: t =>
      new DenseMatrix(rows.size, h.size, rows.toArray.flatMap(_.split("").map(_.toInt)), 0, h.size, true)
    }

  def shortestPath(m: DenseMatrix[Int]): Double = {
    val g    = new DefaultDirectedWeightedGraph[(Int, Int), DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    val mask = Masks.emptyCross[Boolean](3, 3)

    m.convolveMap(mask) { case (ij, a, neighbours) =>
      g.addVertex(ij)
      neighbours.foreach { case (mn, v) =>
        g.addVertex(mn)
        g.addEdge(ij, mn, new DefaultWeightedEdge())
        g.setEdgeWeight(g.getEdge(ij, mn), v)
      }
      a
    }

    val dijkstra = new DijkstraShortestPath(g)
    dijkstra.getPath(0 -> 0, (m.rows - 1, m.cols - 1)).getEdgeList.asScala.map(e => g.getEdgeWeight(e)).sum
  }

  def horizontalConcat(cols: Int): Pipe[IO, DenseMatrix[Int], DenseMatrix[Int]] = _.zipWithIndex
    .groupAdjacentBy(_._2 / cols)
    .map { case (rowNumber, chunk) =>
      val matrices = chunk.toList.map { case (matrix, index) =>
        matrix.map { el =>
          val v = el + (index.toInt % cols) + rowNumber.toInt
          if (v > 9) v % 9 else v
        }
      }
      matrices.reduce(DenseMatrix.horzcat(_, _))
    }

  override def run(args: List[String]): IO[ExitCode] = {
    input.compile.lastOrError
      .flatMap { case m =>
        val task1 = IO(shortestPath(m))
        val big   = Stream.repeatEval(IO(m.copy)).through(horizontalConcat(5)).take(5).reduce(DenseMatrix.vertcat(_, _))
        val task2 = big.compile.lastOrError.map(shortestPath)

        (task1, task2).parTupled.flatMap(v => log.info(show"Task 1 and 2: $v"))
      }
      .as(ExitCode.Success)
  }
}
