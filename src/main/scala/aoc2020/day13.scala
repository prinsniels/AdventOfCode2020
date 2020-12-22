package aoc2020

import utils.FileScanner

import scala.annotation.tailrec

object day13 extends App {
  val file = FileScanner("src/main/resources/day-13.input").lines().toList
  val earliestTime = file.head.toInt

  val earliestBus  = file.tail.head.split(',').filter(_ != "x").map(_.toInt).map(x => x -> (x - (earliestTime % x))).minBy(_._2)
  println(s"Solution 1: ${earliestBus._1 * earliestBus._2}")

}
