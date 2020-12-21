package aoc2020

import utils.FileScanner

object Day05 extends App{
  case class BoardingPass(row: Int, column: Int) {
    val seatId: Int = row * 8 + column
  }

  def parse(a: Char, b: Char)(value: String): Int =
    Integer.parseInt(value.replace(a, '0').replace(b, '1'), 2)

  val parseRow: String => Int = value => parse('F', 'B')(value)
  val parseSeat: String => Int = value => parse('L', 'R')(value)

  val file = new FileScanner("src/main/resources/day-05.input")

  val seatIds = file.lines
    .map(_.splitAt(7))
    .map(x => BoardingPass(parseRow(x._1), parseSeat(x._2)).seatId)
    .toVector
    .sorted

  println(s"max: ${seatIds.max}")
  println(s"missing: ${seatIds.zip(seatIds.tail).find(x => x._2 - x._1 != 1).map(x => x._1 + 1).get}")



}
