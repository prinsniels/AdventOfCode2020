package aoc2020

import utils.FileScanner

object Day07 extends App {
  val base = raw"^(\D+) bags contain (?:(no other bags)|(.+)).".r
  val containing = raw"(\d+) (\D+) bags?".r

  val file = FileScanner("src/main/resources/day-07.input")

  def parseInput(input: String): (String, List[(Int, String)]) = input match {
    case base(x, "no other bags", _) => (x -> List())
    case base(x, _, z) => (x -> containing.findAllMatchIn(z).map(x => x.group(1).toInt -> x.group(2)).toList)
  }

  val graph: Map[String, List[(Int, String)]] = file.lines().map(parseInput).toMap

  // be aware, this is not stack safe
  def containsShinyGold(bagName: String): Boolean = bagName == "shiny gold" || graph(bagName).exists(b => containsShinyGold(b._2))

  println(graph.keys.filter(_ != "shiny gold").count(containsShinyGold))

  // be aware, this is not stack safe
  def amountWithin(bagName: String): Int = graph(bagName).map(res => res._1 + res._1 * amountWithin(res._2)).sum

  println(amountWithin("shiny gold"))

}
