package aoc2020

import utils.FileScanner
import scala.annotation.tailrec

object Day09 extends App  {

    val sequence: Seq[Long] = FileScanner("src/main/resources/day-09.input").longLines().toSeq

    def combinationExists(sequence: Seq[Long], target: Long): Boolean = sequence.combinations(2).exists(_.sum == target)
    
    @tailrec
    def search(pre: Seq[Long], target: Long, rest: Seq[Long]): Long = {
        if (!combinationExists(pre, target)) target
        else search(pre.tail :+ target, rest.head, rest.tail)
    }

    @tailrec
    def findSumOption(sequence: Seq[Long], amount: Int, target: Long): Seq[Long] = {
        val summed  = sequence.take(amount).sum
        if (summed  == target ) sequence.take(amount)
        else if (summed > target) findSumOption(sequence.tail, 1, target)
        else findSumOption(sequence, amount + 1, target)
    }

    val (firstX, rest) = sequence.splitAt(25)
    val firstNotCorrectValue = search(firstX, rest.head, rest.tail)

    println(s"first incorrect ${search(firstX, rest.head, rest.tail)}")

    val option = findSumOption(sequence, 1, firstNotCorrectValue)
    println(option.min + option.max)

}
