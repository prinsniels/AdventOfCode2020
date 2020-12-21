package aoc2020

import utils.FileScanner

object Day20  extends  App {

  val data = new FileScanner("src/main/resources/day-20.input").asString().split("\n\n").toList.map(_.split('\n').toList)

  case class Tile(id: Int, grid: List[List[Char]]) {
    // top, bottom, left, right
    val edges: List[String] = List(
      grid.head.mkString,
      grid.last.mkString,
      grid.transpose.head.mkString,
      grid.transpose.last.mkString
    )

    def linesUp(other: Tile ): Boolean = {
      edges.exists(x => other.edges.exists(y => x == y || x == y.reverse))
    }
  }

  def parse(data: List[String]): Tile = Tile(
      raw"\d+".r.findFirstIn(data.head).head.toInt,
      data.tail.map(_.toList)
    )

  val allTiles = data.map(parse)


  def surroundingOptions(t: Tile, options: List[Tile]): List[Tile] = options.filter(o => o.id != t.id && t.linesUp(o))

  // find all tiles that line up with precisely two other tiles
  println(allTiles.map(t => (t.id, surroundingOptions(t, allTiles).size)).filter(x => x._2 == 2).map(_._1.toLong).product)

}
