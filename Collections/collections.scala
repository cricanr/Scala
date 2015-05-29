package collections

import java.util

import scala.collection.mutable
import scala.collection.parallel.immutable

object Exercises {
  def main(args: Array[String]) {

    // exercise 1 mutable
    println("Mississippi")
    val map = indices("Mississippi")

    map.foreach(p => println("Char: " + p._1 + " appears in the word on the following indexes: " + p._2.toString))

    println()

    // exercise 2 immutable
    val map2 = indices("Mississippi")

    map.foreach(p => println("Char: " + p._1 + " appears in the word on the following indexes: " + p._2.toString))

    // exercise 3 - remove zeros
    val intList = List(1,23,0,23,12,3,1,230,0,0,-1)
    println("Initial list is: " + intList)

    println(s"List after removal of zeros: ${removeZeros(intList)}\n")

    // exercise 4
    println(s"Common indexes for commonIndices(List('Motan', 'Garfield', 'TopCat'), Map(('Motan', 1), ('Tom', 4), ('Garfield', 5)) are: ${commonIndices(List("Motan", "Garfield", "TopCat"), Map(("Motan", 1), ("Tom", 4), ("Garfield", 5)))}\n")

    // exercise 5
    val concated = makeString(Array("Tom", "TopCat", "Garfield"))
    val mkStr = (Array("Tom", "TopCat", "Garfield")).mkString
    assert(concated == mkStr, "makeString yields the same result as mkString")
    println(s"makeString(Array('Tom', 'TopCat', 'Garfield')) yields: ${makeString(Array("Tom", "TopCat", "Garfield"))}\n")

    // exercise 7
    val prices : List[Double] = List(10.2, 231.2, 2, 5)
    val quantities = List(2, 4, 1, 5)
    val result = zipPriceQuant(prices, quantities)
    println(s"prices: $List(10.2, 231.2, 2, 5) zipped with $List(2, 4, 1, 5) as tupled after calling zipPriceQuant yield: $result\n")

    // exercise 8
    val array = Array(12.2, 23.1, 4, 64, 24, 5.8)
    val groupedArray = groupArray(array, 3)

    groupedArray.foreach(i => {  i.foreach(j => print(j.toString + ", ")) } )
  }

  def indices(word : String) : mutable.HashMap[Char, mutable.LinkedHashSet[Int]] = {
    var mapIndices = new mutable.HashMap[Char, mutable.LinkedHashSet[Int]]

    for (ch <- word.distinct) {
      var hashSet = new mutable.LinkedHashSet[Int]
      for (i <- 0 until word.length if word(i) == ch ) {
        hashSet+=i
      }
      mapIndices(ch) = hashSet
    }

    mapIndices
  }

  def indicesImmutable(word: String) : scala.collection.immutable.HashMap[Char, List[Int]] = {
    var mapIndices = new scala.collection.immutable.HashMap[Char, List[Int]]

    for (ch <- word.distinct) {
      val indices = (for (i <- 0 until word.length if word(i) == ch) yield i).toList
      mapIndices = mapIndices + (ch -> indices)
    }

    mapIndices
  }

  def removeZeros(numbers: List[Int]) : List[Int] = {
    numbers.filter(n => n != 0)
  }

  def commonIndices(words: List[String], map : Map[String, Int]) : List[Int] = {
    words.flatMap(w => map.get(w))
  }

  def makeString(array: Array[String]) : String = {
    array.reduceLeft(_ + _)
  }

  def zipPriceQuant(prices: List[Double], quantities: List[Int]) = {
    (prices zip quantities) map Function.tupled(_ * _)
  }

  def groupArray(array: Array[Double], noColumns : Int) = {
    array.grouped(noColumns).toArray.map(_.toArray)
  }
}