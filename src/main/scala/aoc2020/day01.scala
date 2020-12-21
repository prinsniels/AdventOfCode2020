package aoc2020

import utils.FileScanner

object day01 extends App {
  val file = new FileScanner("src/main/resources/day-01.input")

  lazy val input_data: List[Int] = file.lines().map(_.toInt).toList

  val pairs = for {
    a <- input_data
    b <- input_data
    if a + b == 2020
  } yield (a, b)

  val triples = for {
    a <- input_data
    b <- input_data
    c <- input_data
    if a + b + c == 2020
  } yield (a, b, c)

  println("pairs:", pairs.headOption.map(x => x._1 * x._2))
  println("triples:", triples.headOption.map(x => x._1 * x._2 * x._3))


}
