package  aoc2020

import  utils.FileScanner
import java.util.concurrent.Future

object day02 extends App {
  val file: FileScanner = FileScanner("src/main/resources/day-02.input")

  case class Rule(mi: Int, ma: Int, ch: Char, pw: String) {
    val test_1: Boolean = {
      val i = pw.count(_ == ch)
      mi <= i && i <= ma
    }

    val test_2: Boolean = {
      val l = pw(mi - 1)
      val r = pw(ma - 1)
      (ch == l || ch == r) && (l != r)
    }
  }

  def parseRule(a: String): Rule = {
    val pattern = raw"(\d+)-(\d+) (\D): (\D+)".r
    a match {
      case pattern(mi, ma, ch, pw) =>
        Rule(mi.toInt, ma.toInt, ch.head, pw)
    }
  }

  val assignOne: Int = file
    .lines()
    .map(parseRule)
    .count(_.test_1)

  println(assignOne)

  val assignTwo: Int = file
    .lines()
    .map(parseRule)
    .count(_.test_2)

    println(assignTwo)
}
