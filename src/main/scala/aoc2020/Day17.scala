package aoc2020

import utils.FileScanner

import scala.annotation.tailrec

object Day17 extends App {
  type Space = Map[Point, Char]

  case class Point(x: Int, y: Int, z: Int, w: Int) {
    def + (p: Point): Point = Point(x + p.x, y + p.y, z + p.z, w + p.w)
  }

  val modifications = for {
    x <- -1 to 1
    y <- -1 to 1
    z <- -1 to 1
    w <- -1 to 1
    if !(x == 0 &  y == 0 & z == 0 & w == 0)
  } yield Point(x, y, z, w)

  def countSurroundings(space: Space, point: Point): Int =
    modifications.map(m => point + m).foldLeft(0) {
      case (acc, p) => if (space.getOrElse(p, '.') == '#') acc + 1 else acc
    }

  def extendSpace(space: Space): Space = space.foldLeft(Map.empty[Point, Char]) {
    case (nwSpace, (point, '#')) => modifications.map(p => point + p).foldLeft(nwSpace) ({
      case (acc, p) => acc + (p -> space.getOrElse(p, '.'))
    }) + (point -> '#')
  }

  /**
  1. If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
  2. If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
  */
  def modify(space: Space): Space =
    space.foldLeft(Map.empty[Point, Char]) {
      case (nwSpace, (point, '#')) =>
        val active = countSurroundings(space, point)
        if (active == 2 || active == 3) nwSpace + (point -> '#')
        else nwSpace + (point -> '.')

      case (nwSpace, (point, '.')) =>
        val active = countSurroundings(space, point)
        if (active == 3) nwSpace + (point -> '#')
        else nwSpace + (point -> '.')
    }

  @tailrec
  def cycle(space: Space, turns: Int): Space = {
    if (turns <= 0) space
    else {
      val cleanedSpace = space.filter(_._2 == '#')
      val extendedSpace = extendSpace(cleanedSpace)
      cycle(modify(extendedSpace), turns - 1)
    }
  }

  val startSpace = new FileScanner("src/main/resources/day-17.input").lines()
    .zipWithIndex
    .flatMap(y => y._1
      .zipWithIndex.map(x => Point(x._2, y._2, 0, 0) -> x._1 ))
    .toMap

  println(startSpace.count(_._2 == '#'))

  println(cycle(startSpace, 6).count(_._2 == '#'))

}
