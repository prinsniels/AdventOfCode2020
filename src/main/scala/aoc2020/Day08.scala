package aoc2020

import utils.FileScanner
import scala.util.Try
import scala.util.Failure
import scala.annotation.tailrec
import cats.instances.set
import scala.util.Success

object Day08 {

  val file = new FileScanner("src/main/resources/day-08.input")

  type Routine = Vector[(String, Int)]

  val program: Routine = file
    .lines()
    .map(x => {
      val split = x.split(" ")
      (split(0), split(1).toInt)
    })
    .toVector

  def alterProgram(program: Routine)(idx: Int): Routine = {
    program(idx) match {
      case ("jmp", a) => program.updated(idx, ("nop", a))
      case ("nop", a) => program.updated(idx, ("jmp", a))
      case _          => program
    }
  }

  @tailrec
  def step(
      program: Routine,
      idx: Int,
      acc: Int,
      visited: Set[Int]
  ): Try[Int] = {
    if (idx == program.length) Success(acc)
    else if (idx < 0 || idx >= program.length)
      Failure(new IndexOutOfBoundsException(idx))
    else if (visited contains idx)
      Failure(new NotImplementedError(s"stuck in loop at: ${acc}"))
    else {
      program(idx) match {
        case ("nop", _)  => step(program, idx + 1, acc, visited + idx)
        case ("acc", am) => step(program, idx + 1, acc + am, visited + idx)
        case ("jmp", am) => step(program, idx + am, acc, visited + idx)
        case _           => Failure(new NotImplementedError(s"unknown ${program(idx)}"))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"look where it's stuck ${step(program, 0, 0, Set.empty[Int])}")

    val modifier = alterProgram(program) _
    val search = for {
      i <- 0 until program.length
    } yield step(modifier(i), 0, 0, Set.empty[Int])
    println(s" result fixed program ${search.find(x => x.isSuccess)}")

  }
}
