package aoc2020

import utils.FileScanner

import scala.annotation.tailrec

object Day22 extends App {
  val decks = FileScanner("src/main/resources/day-22.input").asString().split("\n\n").toVector.map(_.split('\n').toVector)

  val player1 = decks.head.tail.map(_.toInt)
  val player2 = decks.tail.head.tail.map(_.toInt)

  def standardRules(deck1: Vector[Int], deck2: Vector[Int]): Int = if (deck1.head < deck2.head) 2 else 1

  def recursiveRules(deck1: Vector[Int], deck2: Vector[Int]): Int = {
    if ((deck1.head <= deck1.tail.size) && (deck2.head <= deck2.tail.size))
      play(deck1.tail.take(deck1.head), deck2.tail.take(deck2.head), Set(), Set(), recursiveRules)._1
    else standardRules(deck1, deck2)
  }

  @tailrec
  def play(deck1: Vector[Int], deck2: Vector[Int], p1Hands: Set[Vector[Int]], p2Hands: Set[Vector[Int]], rules: (Vector[Int], Vector[Int]) => Int): (Int, Vector[Int]) = {
    if (p1Hands.contains(deck1) || p2Hands.contains(deck2)) (1, Vector())
    else if (deck1.isEmpty) (2, deck2)
    else if (deck2.isEmpty) (1, deck1)
    else {
      val winner = rules(deck1, deck2)
      if (winner == 1) play(deck1.tail :+ deck1.head :+ deck2.head, deck2.tail, p1Hands + deck1, p2Hands + deck2, rules)
      else play(deck1.tail, deck2.tail :+ deck2.head :+ deck1.head, p1Hands + deck1, p2Hands + deck2, rules)
    }
  }

  println(s"solution 1: ${play(player1, player2, Set(), Set(), standardRules)._2.reverse.zipWithIndex.map(x => x._1 * (x._2 + 1)).sum}")
  println(s"solution 2: ${play(player1, player2, Set(), Set(), recursiveRules)._2.reverse.zipWithIndex.map(x => x._1 * (x._2 + 1)).sum}")

}
