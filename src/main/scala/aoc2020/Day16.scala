package aoc2020

import utils.FileScanner

import scala.annotation.tailrec

object Day16 extends App {

  val file = new FileScanner("src/main/resources/day-16.input")

  val ruleRegex = raw"^(\D+): +(\d+)-(\d+) or (\d+)-(\d+)".r

  case class Rule(name: String, lowerRange: Set[Int], upperRange: Set[Int]) {
    def matches(value: Int): Boolean = lowerRange.contains(value) || upperRange.contains(value)
  }

  val rules: Vector[Rule] = file.lines().take(20).filter(m => m != "").map({
    case ruleRegex(name, l1, h1, l2, h2) => Rule(name, (l1.toInt to h1.toInt).toSet, (l2.toInt to h2.toInt).toSet)
  }).toVector

  val myTicket: Vector[Int] = file.lines().slice(22, 23).collectFirst(_.split(',').map(_.toInt).toVector).get

  val otherTickets: Vector[Vector[Int]] = file.lines().splitAt(25)._2.toVector.map(_.split(',').map(_.toInt).toVector)

  println(s"Solution a: ${otherTickets.flatMap(_.filterNot(x => rules.exists(_.matches(x)))).sum}")

  // all valid tickets, each line contains all proof for each position
  val proof: Vector[Vector[Int]] = (myTicket +: otherTickets.filter(_.forall(v => rules.exists(_.matches(v))))).transpose


  val placeablePositions: Map[Int, Set[Int]] = rules.zipWithIndex.map(r => r._2 -> proof.zipWithIndex.filter(p => p._1.forall(v => r._1.matches(v))).map(_._2).toSet).toMap

  def sequence(rules: Vector[Rule], placeablePositions: Map[Int, Set[Int]]): Option[Seq[Int]] = {

    @tailrec
    def helper(stack: List[(Seq[Int], Seq[Int], Set[Int])]): Option[Seq[Int]] = {
      stack match {
        case Nil => None
        case (remaining, path, takenPositions) :: tail => if (remaining.isEmpty) Some(path.reverse)
        else {

          // the next rule to sequence
          val n = remaining.head
          val allOptions = placeablePositions.getOrElse(n, Set.empty[Int])
          val allUntakenOptions = allOptions.filter(o => !takenPositions(o))

          // append all remaining options to the tail
          val extendedStack = allUntakenOptions.foldLeft(tail){
            case (acc, opt) => (remaining.tail, opt +: path, takenPositions + opt) :: acc
          }
          helper(extendedStack)
        }
      }
    }
    helper((rules.indices.toList, List.empty[Int], Set.empty[Int]) :: Nil)
  }


//
  val res = sequence(rules, placeablePositions)
  println(res.map(r => r.toVector
    .zipWithIndex
    .map(idx => (rules(idx._2).name, idx._1))
    .filter(x => x._1.startsWith("departure"))
    .map(x => myTicket(x._2))
  ).get.map(_.toLong).product)
  println(myTicket)

}
