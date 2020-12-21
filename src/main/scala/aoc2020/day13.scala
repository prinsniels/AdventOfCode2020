package aoc2020

import utils.FileScanner

import scala.annotation.tailrec

object day13 {

  def main(args: Array[String]): Unit = {
    val (waitingTime, minutes) = getWaitingTimesAt("src/main/resources/day-13.input").min
//    println(waitingTime, minutes, waitingTime * minutes)

    val requiredSequence = getRequiredSequence("src/main/resources/day-13.tst.input")
    val (highestBus, offset) = requiredSequence.minBy(x => -x._1)
    val offsetSequence = requiredSequence.map(x => (x._1, x._2 - offset))

    println(requiredSequence.map(_._1).product)
//    println(find(highestBus, 0, offsetSequence))
//    println(busStartsAt(8, 7))
  }

  @tailrec
  def find(stepSize: Int, cur: Long, pattern: Vector[(Int, Int)]): Long = {
    if (pattern.forall({ case (busId, tsOffset) => busStartsAt(busId, cur + tsOffset)})) cur
    else find (stepSize, cur + stepSize, pattern)
  }


  val minutesRemainingAr: (Int, Long) => Int = (bId, ts) => (bId - ts % bId).toInt

  val busStartsAt: (Int, Long) => Boolean = (bus, ts) => minutesRemainingAr(bus, ts) == bus

  val getTimeStamp: String => Int = x => new FileScanner(x).lines().toVector.head.toInt

  val getWaitingTimesAt: String => Vector[(Int, Int)] = x => new FileScanner(x)
    .lines().toVector.tail.head
    .split(',').toVector.filter(_!="x")
    .map(_.toInt)
    .map(i => (minutesRemainingAr( i, getTimeStamp(x)), i))

  val getRequiredSequence: String => Vector[(Int, Int)] = x => new FileScanner(x)
    .lines().toVector.tail.head.split(',')
    .zipWithIndex.filter(_._1 != "x" )
    .map(x => (x._1.toInt, x._2))
    .toVector


}
