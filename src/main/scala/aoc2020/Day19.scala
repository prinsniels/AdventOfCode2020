package aoc2020

import utils.FileScanner

object Day19 extends App {

  def lines = new FileScanner("src/main/resources/day-19.input").asString()
    .split("\n\n")
    .map(_.split("\n")
      .toList)
    .toList

  val baseRegex = raw"(\d+): (.+)".r
  val charRegex = """"([a-z])"""".r

  trait Thingy
  case class Val(value: String) extends Thingy
  case class Pointing(value: List[List[Int]]) extends Thingy

  def parse(value: String): (Int, Thingy) = value match {
    case baseRegex(id, rest) =>
      rest.strip() match {
        case charRegex(v) => id.toInt -> Val(v)
        case _ => id.toInt -> Pointing(rest.split('|').map(x => x.strip().split(' ').map(_.toInt).toList).toList)
      }
  }

  val rules = lines.head.map(parse).toMap
  val data = lines.tail.head


  // quick fix to stop calculations
  val max = 200
  // note this is not tail recursive
  def build(rules: Map[Int, Thingy], id: Int = 0, length: Int = 0): String = {
    if (length > max) "" else rules(id) match {
      case Val(v) => v
      case Pointing(rule) => s"(${rule.map(r => r.map(s => build(rules, s, length + r.size)).mkString).mkString("|")})"
    }
  }

  println(s"Solution 1: ${data.count(build(rules).r.matches)}")

  val replace = "8: 42 | 42 8" :: "11: 42 31 | 42 11 31" :: Nil
  val repaired = rules ++ replace.map(parse).toMap
  println(s"Solution 1: ${data.count(build(repaired).r.matches)}")

}
