package jgogstad.day20

import breeze.linalg.DenseMatrix
import jgogstad._
import scodec.bits.BitVector

import scala.annotation.tailrec
import scala.io.Source

private object Tasks extends App {

  val toInt: Char => Int = {
    case '#' => 1
    case '.' => 0
  }

  val (enhancement, input: DenseMatrix[Int]) = {
    val lines = Source.fromResource("day20/input.txt").getLines().toList
    (toInt compose lines.head) -> DenseMatrix(lines.drop(2).toArray.map(_.toCharArray.map(toInt)): _*)
  }

  val mask = DenseMatrix.ones[Boolean](3, 3)

  @tailrec
  def loop(acc: DenseMatrix[Int], i: Int, default: Int): DenseMatrix[Int] =
    if (i <= 0) acc
    else {
      val next = acc.pad(1, default).convolveMap(mask, Some(default)) { case (xy, value, neighbours) =>
        val sorted = neighbours.sorted
        val index  = BitVector.fromValidBin(sorted.map(_._2).mkString).toInt(false)
        enhancement(index)
      }

      val evenDefault = enhancement(BitVector.fromValidBin(List.fill(9)(0).mkString).toInt(false))
      val oddDefault  = enhancement(BitVector.fromValidBin(List.fill(9)(1).mkString).toInt(false))
      loop(next, i - 1, if (i % 2 == 1) oddDefault else evenDefault)
    }

  val result = loop(input, 2, 0)

  val task1 = loop(input, 2, 0).activeIterator.filter(_._2 > 0).size
  val task2 = loop(input, 50, 0).activeIterator.filter(_._2 > 0).size
  println(task1)
  println(task2)
}
