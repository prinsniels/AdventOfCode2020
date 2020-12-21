package aoc2020

import utils.FileScanner

import scala.annotation.tailrec

object Day18 extends App {

  val splitRegex = raw"(\([\d+*]+\))".r
  val operatorsRegex = raw"[*,+]".r
  val numbersRegex = raw"\d+".r

  def getOperators(input: String): List[String] = operatorsRegex.findAllMatchIn(input).map(x => x.toString()).toList

  def getNumbers(input: String): List[Long] = numbersRegex.findAllMatchIn(input).map(_.toString.toLong).toList

  def preform(l: Long, r: Long, opp: String): Long = opp match {
      case "*" => l * r
      case "+" => l + r
    }

  def eval(eval: String)(opPrio: String => Int): Long = {
    @tailrec
    def evalTailRec(
         remainingNumbers: List[Long],
         remainingOperators: List[String],
         numbersStack: List[Long],
         operatorStack: List[String]
   ): Long = {
      if (remainingNumbers.isEmpty) {
        if (operatorStack.isEmpty) numbersStack.head  // the answer
        else {
          val n2 = numbersStack.head
          val n1 = numbersStack.tail.head
          val o = operatorStack.head
          val res = preform(n1, n2, o)
          evalTailRec(remainingNumbers, remainingOperators, res :: numbersStack.drop(2), operatorStack.tail)
        }
      }
      else if (remainingNumbers.size > remainingOperators.size) evalTailRec(remainingNumbers.tail, remainingOperators, remainingNumbers.head :: numbersStack, operatorStack)
      else if (operatorStack.isEmpty || opPrio(operatorStack.head) < opPrio(remainingOperators.head)) evalTailRec(remainingNumbers, remainingOperators.tail, numbersStack, remainingOperators.head :: operatorStack)
      else {
        val n2 = numbersStack.head
        val n1 = numbersStack.tail.head
        val o = operatorStack.head
        val res = preform(n1, n2, o)
        evalTailRec(remainingNumbers, remainingOperators, res :: numbersStack.drop(2), operatorStack.tail)
      }
    }
    evalTailRec(getNumbers(eval), getOperators(eval), List(), List())
  }

  def priority1(opp: String): Int = opp match {
    case "+" => 1
    case "*" => 1
  }

  def priority2(opp: String): Int = opp match {
    case "+" => 2
    case "*" => 1
  }

  @tailrec
  def evaluateFullExpression(input: String)(opPrio: String => Int): Long =  splitRegex.findFirstMatchIn(input) match {
    case None => eval(input)(opPrio)
    case Some(value) =>
      val modded  = input.replace(value.toString, eval(value.toString)(opPrio).toString)
      evaluateFullExpression(modded)(opPrio)
  }

  val input = "2 * 3 + (4 * 5)".replace(" ", "")
  println(evaluateFullExpression(input)(priority2))

  val lines = new FileScanner("src/main/resources/day-18.input").lines().map(_.replace(" ","")).toList

  println(s"Solution 1: ${lines.map(x => evaluateFullExpression(x)(priority1)).sum}")
  println(s"Solution 2: ${lines.map(x => evaluateFullExpression(x)(priority2)).sum}")

}
