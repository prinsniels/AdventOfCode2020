package aoc2020

import utils.FileScanner

object Day06 extends App {
  val groups: List[List[String]] = FileScanner("src/main/resources/day-06.input").asString().split("\n\n").map(_.split('\n').toList).toList

  println(s"Solution 1: ${groups.map(x => x.tail.foldLeft(x.head.toSet)( _ ++ _.toSet).size).sum}")
  println(s"Solution 2: ${groups.map(x => x.tail.foldLeft(x.head.toSet)( _ & _.toSet).size).sum}")

}
