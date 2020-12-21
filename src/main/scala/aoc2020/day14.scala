package aoc2020

import utils.FileScanner

object day14 extends App {
  val maskRegex = "mask = ([X01]{36})".r
  val memRegex = raw"mem\[(\d+)\] = (\d+)".r

  implicit class BinParser(x: String) {
    import java.lang.Long.parseLong
    def toLongBinned(y: Int) = parseLong(x, y)
  }

  trait Operation
  case class Mask(value: String) extends Operation
  case class Write(slot: Int, value: String) extends Operation {
    def applyMask(mask: Mask): Write = Write(
      slot: Int,
      value = value
        .zip(mask.value)
        .map({ case (ori, ma) => if (ma == 'X') ori else ma })
        .mkString
    )
  }

  def parseOperation(input: String): Operation = input match {
    case maskRegex(value) => Mask(value)
    case memRegex(address, value) =>
      Write(
        address.toInt,
        value.toInt.toBinaryString.reverse.padTo(36, '0').reverse
      )
  }

  val operations = new FileScanner("src/main/resources/day-14.tst.txt")
    .lines()
    .map(parseOperation)

  lazy val initialize =
    operations.foldLeft((Mask("x" * 36), Map.empty[Int, String])) {
      case ((mask, mem), _: Mask) => (mask, mem)
      case ((mask, mem), w: Write) => {
        val modded = w.applyMask(mask)
        (mask, mem + (modded.slot -> modded.value))
      }
    }

//  println(initialize._2.values.map(_.toLongBinned(2)).sum)

  val x = "000000000000000000000000000000000XXX"
    .zipWithIndex
    .filter(_._1 == 'X')
    .map(36 - 1 - _._2)
    .toSet
    .subsets()
    .map(_.foldLeft(0L)({ case (acc, i) => acc | (1L << i) }))
    .toSeq
  println("**")
  println(x.foreach(println))
}
