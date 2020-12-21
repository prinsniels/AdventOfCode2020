package aoc2020

import utils.FileScanner

import scala.annotation.tailrec

object day04 {
  val file = new FileScanner("src/main/resources/day-04.input")

  def main(args: Array[String]): Unit = {
    val keyValuePattern = "(.+):(.+)".r

    val minKeys = file
      .lines()
      .mkString(" ")
      .split("  ")
      .map(_.split(" ").map({ case keyValuePattern(a, b) => (a, b) }).toMap)
      .count(x =>
        Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").subsetOf(x.keySet)
      )

    println(s"minimum required keys met ${minKeys}")

    val byr: Map[String, String] => Boolean = pp =>
      (for {
        v <- pp.get("byr")
        n <- v.toIntOption
      } yield (1920 <= n && n <= 2002)).getOrElse(false)

    val iyr: Map[String, String] => Boolean = pp =>
      (for {
        v <- pp.get("iyr")
        n <- v.toIntOption
      } yield (2010 <= n && n <= 2020)).getOrElse(false)

    val eyr: Map[String, String] => Boolean = pp =>
      (for {
        v <- pp.get("eyr")
        n <- v.toIntOption
      } yield (2020 <= n && n <= 2030)).getOrElse(false)

    val hgt: Map[String, String] => Boolean = pp => {
      val cm = raw"(\d+)cm".r
      val inc = raw"(\d)in".r
      pp.get("hgt") match {
        case Some(cm(v))  => 150 <= v.toInt && v.toInt <= 193
        case Some(inc(v)) => 59 <= v.toInt && v.toInt <= 76
        case _            => false
      }
    }

    val hcl: Map[String, String] => Boolean = pp =>
      pp.get("hcl").map(x => ("^#[0-9|a-f]{6}$".r).matches(x)).getOrElse(false)

    val ecl: Map[String, String] => Boolean = pp =>
      pp.get("ecl")
        .map(x =>
          Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")(x)
        )
        .getOrElse(false)

    val pid: Map[String, String] => Boolean = pp =>
      pp.get("pid")
        .map(x => x.length == 9 && x.toIntOption.getOrElse(-1) > 0)
        .getOrElse(false)

    val rules = Seq(byr, iyr, eyr, hgt, hcl, ecl, pid)

    val r = file
      .lines()
      .mkString(" ")
      .split("  ")
      .map(_.split(" ").map({ case keyValuePattern(a, b) => (a, b) }).toMap)
      .count(x => rules.forall(r => r(x)))

    val pp = Map(
      "eyr" -> "2030",
      "hcl" -> "#18171d",
      "ecl" -> "grn",
      "hgt" -> "60in",
      "pid" -> "087499704",
      "iyr" -> "2012",
      "byr" -> "1980"
    )
    println(rules)
    println(rules.map(rule => rule(pp)))
    println(
      eyr(pp), 
      hcl(pp), 
      ecl(pp),
      hgt(pp),
      pid(pp),
      iyr(pp),
      byr(pp) )

    println(r)
  }
}
