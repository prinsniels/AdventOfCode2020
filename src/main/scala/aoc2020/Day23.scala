package aoc2020

import scala.annotation.tailrec

object Day23 extends App {

  val input: Vector[Int] = Vector(3,8,9,1,2,5,4,6,7)
//  val input: Vector[Int] = Vector(9,4,2,3,8,7,6,1,5)

  @tailrec
  def nextCup(h: Int, remainder: Set[Int], min: Int = 1, max: Int = 9): Int = {
    val n: Int = if (h - 1 < min) max else h - 1
    if (remainder.contains(n)) n
    else nextCup(n, remainder)
  }

  @tailrec
  def place(remaining: Vector[Int], starting: Vector[Int], stack: Vector[Int], v: Int): Vector[Int] =
    if (remaining.head != v) place(remaining.tail, remaining.head +: starting, stack, v)
    else starting.reverse ++ (remaining.head +: (stack ++ remaining.tail))

  @tailrec
  def play(input: Vector[Int], remainingTurns: Int, min:Int=0, max:Int=9): Vector[Int] = if (remainingTurns <= 0) input
  else {
    val h = input.head
    val (stack, remainder) = input.tail.splitAt(3)
    val newSequence = place(h +: remainder, Vector(), stack, nextCup(h, remainder.toSet, min, max))
    play(newSequence.tail :+ newSequence.head, remainingTurns - 1)
  }

  @tailrec
  def getNextToOne(remaining: Vector[Int]): Vector[Int] =
    if (remaining.head == 1) remaining.tail.take(2)
    else getNextToOne(remaining.tail)


  println(play(input, 10))

  println(nextCup(3, Set(2,  5,  4,  6,  7)))
  println(nextCup(2, Set(5,  4,  6,  7)))
  println(nextCup(5, Set(3, 2,  8,  9,  1)))
  println(nextCup(1, Set(9,  2,  5,  8,  4)))
  println(place(Vector(8, 4, 6, 7, 2, 5), Vector(), Vector(9,1,3), 7))

  println(play(input, 100).mkString.split('1').toList.foldLeft("")((a, b) => b + a))

}

