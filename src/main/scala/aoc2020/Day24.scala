package aoc2020

import utils.FileScanner

object Day24 extends App {

  val lines = FileScanner("src/main/resources/day-24.input").lines().toVector

  case class Vec(x: Double, y: Double) {
    def +(other: Vec): Vec = Vec(x + other.x, y + other.y)
    def *(scaler: Int): Vec = Vec(x * scaler, y * scaler)
  }

  implicit val moves: List[(String, Vec)] = List(
    "se" -> Vec(.5, -.5),
    "sw" -> Vec(-.5, -.5),
    "nw" -> Vec(-.5, .5),
    "ne" -> Vec(.5, .5),
    "e" -> Vec(1, 0),
    "w" -> Vec(-1, 0),
  )

  def transform(inp: String)(implicit moves: List[(String, Vec)]): Vec = {
    moves.foldLeft((inp, Vec(0,0))) {
      case ((rmStr, c), (m, v)) => (m.r.replaceAllIn(rmStr, ""), c + (v * m.r.findAllMatchIn(rmStr).size))
    }._2
  }

  println(s"solution 1: ${lines.map(transform).foldLeft(Map.empty[Vec, Boolean]){case (state, move) => state + (move -> !state.getOrElse(move, true))}.count(_._2 == false)}")

}
