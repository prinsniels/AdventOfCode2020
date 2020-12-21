package aoc2020

import utils.FileScanner

object Day07 extends App {
  val base = raw"^(\D+) bags contain (?:(no other bags)|(.+)).".r
  val inhoud = raw"(\d+) (\D+) bags?".r

  val file = new FileScanner("src/main/resources/day-07.tst.input")

  def parseInput(input: String): (String, List[(Int, String)]) = input match {
    case base(x, "no other bags", _) => (x -> List())
    case base(x, _, z) => (x -> inhoud.findAllMatchIn(z).map(x => x.group(1).toInt -> x.group(2)).toList)
  }

  val graph = file.lines().map(parseInput).toMap




}
