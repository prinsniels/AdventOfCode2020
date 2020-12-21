package aoc2020

import utils.FileScanner
import scala.annotation.tailrec

object day10 extends App {

  val adaptersInBag: List[Int] = FileScanner("src/main/resources/day-10.input")
    .intLines()
    .toList

  @tailrec
  def maxAndDiff(
      adaptors: List[Int],
      cur: Int,
      accDiff: List[Int]
  ): (Int, List[Int]) =
    adaptors match {
      case Nil                       => (cur + 3, 3 +: accDiff)
      case x :: xs if (x - cur <= 3) => maxAndDiff(xs, x, (x - cur) +: accDiff)
    }

  def spawn(
      curPath: List[Int],
      adaptors: List[Int]
  ): List[(List[Int], List[Int])] =
    if (adaptors.isEmpty) Nil
    else if ((adaptors.head - curPath.head) > 3) Nil
    else
      (adaptors.head +: curPath, adaptors.tail) +: spawn(curPath, adaptors.tail)

  @tailrec
  def getAllPaths(
      options: List[(List[Int], List[Int])],
      finished: List[List[Int]]
  ): List[List[Int]] = {
    if (options.isEmpty) finished
    else {
      val (curPath, adaptors) = options.head
      if (adaptors.isEmpty) {
        getAllPaths(options.tail, curPath :: finished)
      } else {
        getAllPaths(spawn(curPath, adaptors) ::: options.tail, finished)
      }

    }
  }

  val (highest, diffs) = maxAndDiff(adaptersInBag.sorted, 0, List.empty[Int])
  println(diffs.count(_ == 1) * diffs.count(_ == 3))
  // println(getAllPaths(List((List(0), adaptersInBag.sorted)), List.empty).length)

  val sorted: List[Int] = 0 +: adaptersInBag.sorted :+ (adaptersInBag.max + 3)

  val differences: List[Int] = sorted.tail.zip(sorted).map(x => x._1 - x._2 )

  lazy val sequence: LazyList[Long] =
    0L #:: 1L #:: 1L #:: sequence.sliding(3).to(LazyList).map(_.sum)

  val solutionB: Long = differences
    .foldLeft(Seq(1))((acc, e) =>
      e match {
        case 1 => (acc.head + 1) +: acc.tail
        case 3 => 1 +: acc
      }
    )
    .map(sequence)
    .product

  println(solutionB)
}
