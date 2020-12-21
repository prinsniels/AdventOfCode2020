package utils

import java.io.File

import scala.io.Source

class FileScanner(path: String) {

  def lines(): Iterator[String] = {
    val source = Source.fromFile(new File(path))
    source.getLines
  }

  def charArray(): Array[Array[Char]] =
    lines()
      .map(x => x.toCharArray)
      .toArray

  def asString(): String = {
    val source  = Source.fromFile(new File(path))
    try source.mkString finally source.close
  }

  def intLines(): Iterator[Int] = lines().map(_.toInt)

  def longLines(): Iterator[Long] = lines().map(_.toLong)

}
