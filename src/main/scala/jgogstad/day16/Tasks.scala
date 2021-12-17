package jgogstad.day16

import cats.effect.{ExitCode, IO}
import fastparse.SingleLineWhitespace._
import fastparse.{Parsed, _}
import jgogstad._
import scodec.bits.BitVector
import spire.implicits._
import spire.math.SafeLong

import java.math.BigInteger
import scala.io.Source

object Tasks extends App {
  var input = Source.fromResource("day16/input.txt").getLines().map(BitVector.fromValidHex(_).toBin).toList.head

  def bit[_: P]             = P("0" | "1")
  def version[_: P]: P[Int] = P(bit.rep(exactly = 3)).!.map(Integer.parseInt(_, 2))
  def typeId[_: P]: P[Int]  = P(bit.rep(exactly = 3)).!.map(Integer.parseInt(_, 2))

  def literalValue[_: P]: P[SafeLong] = {
    def startsOne  = P("1" ~/ bit.rep(exactly = 4).!)
    def startsZero = P("0" ~/ bit.rep(exactly = 4).!)

    P(startsOne.rep ~ startsZero).flatMap { case (ones, zero) =>
      val number  = (ones :+ zero).mkString("")
      val mod     = number.length % 4
      val padding = if (mod > 0) 4 - mod else 0
      P(bit.rep(exactly = padding)).map(_ => new BigInt(new BigInteger(number, 2)).toSafeLong)
    }
  }

  def operator[_: P]: P[Seq[SafeLong]] = {
    def lengthType0 = P(bit.rep(exactly = 15).!).map(Integer.parseInt(_, 2)).flatMap { case packetsLength =>
      P(bit.rep(exactly = packetsLength).!).map { data =>
        val Parsed.Success(result, _) = parse(data, packets(_))
        result
      }
    }

    def lengthType1 = P(bit.rep(exactly = 11).!).map(Integer.parseInt(_, 2)).flatMap { case subpackets =>
      packet.rep(exactly = subpackets)
    }

    P(bit).!.flatMap {
      case "0" => lengthType0
      case "1" => lengthType1
    }
  }

  def packet[_: P]: P[SafeLong] = P(version ~ typeId).flatMap {
    case (v, 0) => operator.map(_.reduce(_ + _))
    case (v, 1) => operator.map(_.reduce(_ * _))
    case (v, 2) => operator.map(_.min)
    case (v, 3) => operator.map(_.max)
    case (v, 4) => literalValue
    case (v, 5) => operator.map(_.toList).map { case a :: b :: Nil => if (a > b) 1 else 0 }
    case (v, 6) => operator.map(_.toList).map { case a :: b :: Nil => if (a < b) 1 else 0 }
    case (v, 7) => operator.map(_.toList).map { case a :: b :: Nil => if (a == b) 1 else 0 }
  }

  def packets[_: P]: P[Seq[SafeLong]] = P(packet.rep(1))

  val Parsed.Success(result, i) = parse(input, packet(_))
  log.info(s"Result ${result}")
}
