package aoc2020

import utils.FileScanner

import scala.annotation.tailrec

object Day21 extends App {

  val wordRegex = raw"([a-z]+)".r
  case class Product(ingredients: Set[String], allergens: Set[String])


  val products = new FileScanner("src/main/resources/day-21.input").lines()
    .map(x => {
      val row = x.split("contains").toList
      Product(wordRegex.findAllIn(row.head).toSet, wordRegex.findAllIn(row.tail.head).toSet)
    }
  ).toVector

  // get all possible allergens
  val allergens: Set[String] = products.foldLeft(Set.empty[String])(_ | _.allergens)
  val ingredients = products.flatMap(p => p.ingredients.toList).toList
  val ingredientsCount: Map[String, Int] = ingredients.groupMapReduce((x: String) => x)(_ => 1)(_ + _)
  val allergenPossibleIngredients: Map[String, Set[String]] = allergens.map(a => {
    val containing = products.filter(_.allergens.contains(a))
    a -> containing.foldLeft(containing.head.ingredients)(_ & _.ingredients)
  }).toMap

  println(s"solution 1: ${(ingredients.toSet -- allergenPossibleIngredients.foldLeft(Set.empty[String])(_ | _._2)).foldLeft(0)(_ + ingredientsCount.getOrElse(_, 0))}")


  // WARNING THIS DOES NOT GUARANTY TERMINATION
  @tailrec
  def filterDown(input: Map[String, Set[String]], acc: Map[String, String]): Map[String, String] = {
    if (input.isEmpty) acc
    else {
      val known = input.filter(_._2.size == 1).map(a => a._2.head -> a._1)
      filterDown(
        input.map(x => x._1 -> x._2.diff(known.keys.toSet)).filter(_._2.nonEmpty),
        acc ++ known
      )
    }
  }

  println(s"solution 2: ${filterDown(allergenPossibleIngredients, Map()).toVector.sortBy(_._2).map(_._1).mkString(",")}")

}
