package aoc2020

import utils.FileScanner

import scala.annotation.tailrec

object day03 extends App {
  val file: FileScanner = FileScanner("src/main/resources/day-03.input")
  case class Point(x: Int, y: Int)
  case class Slope(x: Int, y: Int)

  type Forest = Array[Array[Char]]
  lazy val forest: Forest = file.charArray()

  def nextPoint(point: Point, slope: Slope, mw: Int): Point = Point(
    x = (point.x + slope.x) % mw,
    y = point.y + slope.y
  )

  def countTreesOnRoute(forest: Forest, start: Point, slope: Slope): Int = {
    val mh = forest.length - 1
    val mw = forest(0).length

    @tailrec
    def helper(cur: Point, acc: Int): Int = {
      val np = nextPoint(cur, slope, mw)
      if (np.y > mh) acc
      else if (forest(np.y)(np.x) == '#') helper(np, acc + 1)
      else helper(np, acc)
    }
    helper(start, 0)
  }

  println(countTreesOnRoute(forest, Point(0, 0), Slope(3, 1)))
  println(
    List(Slope(1, 1), Slope(3, 1), Slope(5, 1), Slope(7, 1), Slope(1, 2))
      .map(s => countTreesOnRoute(forest, Point(0, 0), s))
      .product
  )
}
