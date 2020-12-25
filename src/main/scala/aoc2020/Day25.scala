package aoc2020

import scala.annotation.tailrec

object Day25 extends App {

  def calcKey(subject: Long, loopSize: Int): Long =
    (1 to loopSize).foldLeft(1L)({ case (acc, _) => (acc * subject) % 20201227 })

  @tailrec
  def findLoopSize(target: Long, acc: Int = 1, loop: Int = 1): Int = {
    val s = (acc * 7) % 20201227
    if (s == target) loop
    else findLoopSize(target, s, loop + 1)
  }

  assert(findLoopSize(5764801) == 8)
  assert(findLoopSize(17807724) == 11)
  assert(calcKey(17807724, 8) == 14897079)
  assert(calcKey(17807724, 8) == calcKey(5764801, 11))

  val cardPublicKey: Long = 6270530
  val doorPublicKey: Long = 14540258

  assert( calcKey(cardPublicKey, findLoopSize(doorPublicKey)) == calcKey(doorPublicKey, findLoopSize(cardPublicKey)))
  println(calcKey(cardPublicKey, findLoopSize(doorPublicKey)))
}
