package jgogstad.day19

import breeze.linalg._
import cats.implicits._
import jgogstad.utils._

import java.lang.Math.abs
import scala.annotation.tailrec
import scala.io.Source

private object Tasks extends App {
  type Triplet = (Int, Int, Int)

  val input: List[(Int, List[Triplet])] = Source
    .fromResource("day19/input.txt")
    .getLines()
    .foldLeft(List.empty[(Int, List[Triplet])]) {
      case (acc, s"--- scanner ${int(a)} ---")               => (a -> List.empty[Triplet]) :: acc
      case (acc, s) if s.trim.isEmpty                        => acc
      case (((n, l) :: t), s"${int(x)},${int(y)},${int(z)}") => (n -> ((x, y, z) :: l)) :: t
    }
    .map { case (a, ls) => a -> ls }
    .reverse

  // 3d rotation group, inefficiently generated
  val so3: List[DenseMatrix[Int]] = {
    val x90 = DenseMatrix(Array(1, 0, 0), Array(0, 0, -1), Array(0, 1, 0))
    val y90 = DenseMatrix(Array(0, 0, 1), Array(0, 1, 0), Array(-1, 0, 0))
    val z90 = DenseMatrix(Array(0, -1, 0), Array(1, 0, 0), Array(0, 0, 1))
    val e   = DenseMatrix.eye[Int](3)

    val m = Map(1 -> x90, 2 -> y90, 3 -> z90, 4 -> e)

    (for {
      i <- 1 to 4
      j <- 1 to 4
      k <- 1 to 4
      l <- 1 to 4
    } yield m(i) * m(j) * m(k) * m(l)).toList.distinct
  }

  def fit(focus: List[Triplet], candidate: List[Triplet]): Option[Triplet] = {
    val result = (focus, candidate).tupled
      .map { case (z, o) => z |-| o }
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .filter(_._2 >= 12)
      .map(_._1)

    result.toList match {
      case Nil      => None
      case h :: Nil => Some(h)
      // shouldn't have more than one match
    }
  }

  def orientations(beacons: List[Triplet]): List[List[Triplet]] =
    so3.map { permutation =>
      beacons.map { case (x, y, z) =>
        val r = permutation * DenseVector(x, y, z)
        (r.data(0), r.data(1), r.data(2))
      }
    }

  @tailrec
  def solve(
    acc: List[(Triplet, Int, List[Triplet])],
    queue: List[(Int, List[Triplet])]
  ): List[(Triplet, Int, List[Triplet])] = queue match {
    case Nil => acc
    case (el @ (n, h)) :: t =>
      val sensorCoordinates = orientations(h).collectFirstSome { case o =>
        acc.collectFirstSome { case (xyz, id, bs) =>
          fit(bs, o).map(xyz |+| _).map(xyz1 => (xyz1, n, o))
        }
      }

      sensorCoordinates match {
        case None    => solve(acc, t :+ el)
        case Some(c) => solve(c :: acc, t)
      }
  }

  def manhatten(a: Triplet, b: Triplet): Int =
    abs(a._1 - b._1) + abs(a._2 - b._2) + abs(a._3 - b._3)

  val (id, bs) :: tail = input
  val result           = solve(List(((0, 0, 0), id, bs)), tail)

  val task1 = result.flatMap { case (xyz, _, bs) => bs.map(_ |+| xyz) }.distinct.size
  val task2 = result.map(_._1).combinations(2).map { case a :: b :: Nil => manhatten(a, b) }.max

  println(show"Task 1: $task1, task 2: $task2")
}
