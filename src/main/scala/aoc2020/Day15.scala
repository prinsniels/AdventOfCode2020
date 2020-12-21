package aoc2020

import scala.annotation.tailrec

object Day15 extends App{
    val numbers: Vector[Int] = (13 :: 0 :: 10 :: 12 :: 1 :: 5 :: 8 :: Nil).toVector

    @tailrec
    def solve(turns: Int, currentTurn: Int, lastCalled: Int, called: Map[Int, Int]): Int =
      if (currentTurn == turns - 1) lastCalled
      else solve(turns, currentTurn + 1, currentTurn - called.getOrElse(lastCalled, currentTurn), called + (lastCalled -> currentTurn))

    println(s"Solution 1: ${solve(2020, numbers.length - 1, numbers.last, numbers.init.zipWithIndex.toMap)}")
    println(s"Solution 2: ${solve(30000000, numbers.length - 1, numbers.last, numbers.init.zipWithIndex.toMap)}")
}
