package aoc2020

import utils.FileScanner

import scala.annotation.tailrec

object day11 extends App {

  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)
  }

  @tailrec
  def seatVisible(start: Point, v: Point, area: Map[Point, Char]): Boolean =
    area.get(start + v) match {
      case Some('#') => true
      case Some('.') => seatVisible(start + v, v, area)
      case _         => false
    }

  val directions = for {
    x <- -1 to 1
    y <- -1 to 1
    if !(x == 0 && y == 0)
  } yield Point(x, y)

  def lookAround(p: Point, area: Map[Point, Char]): Int =
    directions.map(dir => seatVisible(p, dir, area)).count(x => x)

  def surrounding(p: Point, area: Map[Point, Char]): Int =
    directions.map(_ + p).map(x => area.getOrElse(x, '_') != '#').count(x => !x)

  def modifyNew(area: Map[Point, Char]): Map[Point, Char] =
    area.foldLeft(Map.empty[Point, Char]) {
      case (acc, (point, '.')) => acc + (point -> '.')
      case (acc, (point, value)) =>
        val seatsVisible = lookAround(point, area)
        if (value == 'L' && seatsVisible == 0) acc + (point -> '#')
        else if (value == '#' && seatsVisible >= 5) acc + (point -> 'L')
        else acc + (point -> value)
    }

  def modify(area: Map[Point, Char]): Map[Point, Char] =
    area.foldLeft(Map.empty[Point, Char]) {
      case (acc, (point, '.')) => acc + (point -> '.')
      case (acc, (point, value)) =>
        val seatsVisible = surrounding(point, area)
        if (value == 'L' && seatsVisible == 0) acc + (point -> '#')
        else if (value == '#' && seatsVisible >= 4) acc + (point -> 'L')
        else acc + (point -> value)
    }

  @tailrec
  def find(area: Map[Point, Char], f: Map[Point, Char] => Map[Point, Char]): Map[Point, Char] = {
    val nwArea = f(area)
    if (nwArea == area) nwArea
    else find(nwArea, f)
  }

  def show(a: Map[Point, Char]): Unit =
    Range(0, 10).foreach(y => println(Range(0, 10).map(x => a(Point(x, y)))))
  
  val floorPlan = FileScanner("src/main/resources/day-11.input")
  .lines()
  .zipWithIndex
  .flatMap(y => y._1.zipWithIndex.map(x => (Point(x._2, y._2), x._1)))
  .toMap

  println(find(floorPlan, modify).count(_._2 == '#'))
  println(find(floorPlan, modifyNew).count(_._2 == '#'))
}
