package aoc2020

import utils.FileScanner

object day12 extends App {

  case class Vec(x: Int, y: Int) {
    def +(h: Vec): Vec = Vec(x + h.x, y + h.y)
    def *(i: Int): Vec = Vec(x * i, y * i)
    def rotate(i: Int): Vec = {
      // rotation is around the ship and only 90, 180 or 270 or its negatives
      // the ship is always origen in relation to the vector
      if (i == 90 || i == -270) Vec(y, -x)
      else if (i == 180 || i == -180) Vec(-x, -y)
      else if (i == 270 || i == -90) Vec(-y, x)
      else this
    }
    def manhattan: Int = Math.abs(x) + Math.abs(y)
  }

  val north = Vec(0, 1)
  val east = Vec(1, 0)
  val south = Vec(0, -1)
  val west = Vec(-1, 0)

  val headMap = Map(0 -> north, 90 -> east, 180 -> south, 270 -> west)

  case class Ship(heading: Int, pos: Vec) {
    def newHeading(degrees: Int): Int = (heading + degrees + 360) % 360

    def move(c: Char, i: Int): Ship = c match {
      case 'N' => copy(pos = pos + (north * i))
      case 'E' => copy(pos = pos + (east * i))
      case 'S' => copy(pos = pos + (south * i))
      case 'W' => copy(pos = pos + (west * i))
      case 'F' => copy(pos = pos + (headMap(heading) * i))
      case 'L' => copy(heading = newHeading(-i))
      case 'R' => copy(heading = newHeading(i))
    }
  }

  val movements = (x: String) => FileScanner(x).lines().map(x => (x.head, x.tail.toInt))

  lazy val solution1 = movements("src/main/resources/day-12.input")
    .foldLeft(Ship(90, Vec(0, 0))) {
      case (ship, (action, amount)) => ship.move(action, amount)
  }
  println(solution1.pos.manhattan)

  def solution2: (Vec, Vec) = movements("src/main/resources/day-12.input")
    .foldLeft((Vec(0, 0), Vec(10, 1))) {
      case ((position, waypoint), (action, amount)) =>
        action match {
          case 'N' => (position, waypoint + (north * amount))
          case 'E' => (position, waypoint + (east * amount))
          case 'S' => (position, waypoint + (south * amount))
          case 'W' => (position, waypoint + (west * amount))
          case 'F' => (position + waypoint * amount, waypoint)
          case 'L' => (position, waypoint.rotate(-amount))
          case 'R' => (position, waypoint.rotate(amount))
        }
  }

  println(solution2._1.manhattan)
}
