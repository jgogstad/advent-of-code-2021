package jgogstad.day21

import cats.data.Ior
import spire.syntax.IntegralSyntax

import scala.annotation.tailrec
//import cats.syntax.all._
import cats.implicits._
import breeze.linalg.DenseMatrix
import cats.effect.{ExitCode, IO, IOApp}
import cats.kernel.Group
import fs2.Stream
import io.odin.{consoleLogger, Logger}
import jgogstad._
import jgogstad.utils._
//import spire.implicits._
import spire.math.SafeLong

object Tasks extends IOApp with IntegralSyntax {

  val log: Logger[IO] = consoleLogger()

  val test  = (4, 8)
  val input = (10, 7)

  val throws: Stream[IO, (Int, Either[Int, Int])] = Stream.unfold((0, true)) { case (throwCount, p1turn) =>
    val threeThrows   = mod1(throwCount + 1, 100) + mod1(throwCount + 2, 100) + mod1(throwCount + 3, 100)
    val newThrowCount = throwCount + 3
    val assignThrows  = if (p1turn) Left(threeThrows) else Right(threeThrows)
    if (newThrowCount > 1000) None else Some((newThrowCount -> assignThrows, newThrowCount -> !p1turn))
  }

  def game(start: (Int, Int)): Stream[IO, (Int, (Int, Int))] =
    throws
      .scan((0, start, (0, 0))) {
        case ((_, (pos1, pos2), (points1, points2)), (throwCount, Left(threeThrows))) =>
          val nextPos = mod1(pos1 + threeThrows, 10)
          (throwCount, (nextPos, pos2), (points1 + nextPos, points2))
        case ((_, (pos1, pos2), (points1, points2)), (throwCount, Right(threeThrows))) =>
          val nextPos = mod1(pos2 + threeThrows, 10)
          (throwCount, (pos1, nextPos), (points1, points2 + nextPos))
      }
      .map { case (throws, _, points) => throws -> points }

  def rollPlayers(score: Int, pos: Int): List[Ior[(Int, Int), (Int, Int)]] = {
    val rolls    = ((1 to 3).toList, (1 to 3).toList, (1 to 3).toList).tupled
    val allRolls = rolls.flatMap(r => rolls.map(r -> _))

    def newPos(roll: (Int, Int, Int)) = mod1(pos + List(roll._1, roll._2, roll._3).sum, 10)

    allRolls.map { case (p1roll, p2roll) =>
      val newPos1 = newPos(p1roll)
      val newPos2 = newPos(p2roll)

      val newScore1 = Math.min(21, score + newPos1)
      val newScore2 = Math.min(21, score + newPos2)

      if (newScore1 >= 21) Ior.Left(newScore1 -> newPos1)
      else if (newScore2 >= 21) Ior.Right(newScore2 -> newPos2)
      else Ior.Both(newScore1                       -> newPos1, newScore2 -> newPos2)
    }
  }

  @tailrec
  def quantumRolls(acc: DenseMatrix[(SafeLong, SafeLong)], score: Int): DenseMatrix[(SafeLong, SafeLong)] = {
    if (score == 21) acc
    else {
      acc(score, ::).t.activeIterator.toList.collect {
        case (pos, (count1, count2)) if count1 > 0 || count2 > 0 => {
          rollPlayers(score, pos).foreach {
            case Ior.Left(c)  => acc.update(c, acc(c) |+| (count1, 0))
            case Ior.Right(c) => acc.update(c, acc(c) |+| (0, count2))
            case Ior.Both(c1, c2) =>
              acc.update(c1, acc(c1) |+| (count1, 0))
              acc.update(c2, acc(c2) |+| (0, count2))
          }
        }
      }
      quantumRolls(acc, score + 1)
    }
  }

  val task2 = {
    val m = DenseMatrix.zeros[(SafeLong, SafeLong)](22, 11)
    m.update(0, input._1, 1.toSafeLong -> 0.toSafeLong)
    m.update(0, input._2, 0.toSafeLong -> 1.toSafeLong)
    val result = quantumRolls(m, 0)
    val all21  = result(21, ::).t.activeIterator.map { case (_, (p1, _)) => p1 }.toList.combineAll
    val all212 = result(21, ::).t.activeIterator.map { case (_, (_, p2)) => p2 }.toList.combineAll
    println(all21)
    println(all212)
//    println(result)
    println(m.toString(1000, 1000))
  }

  def task1 = game(input).collectFirst {
    case (throws, (points1, points2)) if points1 >= 1000 => throws * points2
    case (throws, (points1, points2)) if points2 >= 1000 => throws * points1
  }

  override def run(args: List[String]): IO[ExitCode] = {
    task1.compile.lastOrError.map(println(_))
    println(task2)
    IO.pure(()).as(ExitCode.Success)
  }

}
//          444 356 092 776 315
// high 541 449 990 693 784 377
