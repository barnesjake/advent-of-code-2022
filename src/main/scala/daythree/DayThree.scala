package daythree

import util.ReadPuzzleInput

import scala.annotation.tailrec

/**
 * https://adventofcode.com/2022/day/3
 */

object DayThree extends App {

  val puzzleInputAsList: List[String] = ReadPuzzleInput.puzzleInputToList(s"src/main/scala/daythree/puzzleinput.txt")

  println("\n\n----- Part 1 -----\n\n")

  val aToZ = ('a'.to('z') ++ 'A'.to('Z'))
    .zipWithIndex
    .map(character => updateIndexToBePriority(character)(1))
    .toMap

  private def updateIndexToBePriority(zipped: (Char, Int))(numberToAdd: Int): (Char, Int) = (zipped._1, zipped._2 + numberToAdd)

  @tailrec
  def iterateThroughFile1(f: String => List[Char])(list: List[String], listContainingDuplicates: List[Char]): List[Char] = {
    list match {
      case h :: t => iterateThroughFile1(f)(t, listContainingDuplicates ++ f(h))
      case h :: Nil => iterateThroughFile1(f)(Nil, listContainingDuplicates ++ f(h))
      case Nil => listContainingDuplicates
    }
  }

  private def findDuplicatesBetweenCompartments(input: String): List[Char] = {
    val (a, b) = input.splitAt(input.length / 2)
    a.toList.intersect(b.toList).distinct
  }

  println("Sum of priorities: " + iterateThroughFile1(findDuplicatesBetweenCompartments)(puzzleInputAsList, Nil).map(aToZ).sum)

  println("\n\n----- Part 2 -----\n\n")

  val groups: List[List[String]] = puzzleInputAsList.grouped(3).toList

  @tailrec
  def iterate(list: List[List[Char]], acc:List[Char]): List[Char] = {
    list match {
      case h::t if acc.isEmpty  => iterate(t, h.intersect(t.head))
      case h::t if acc.nonEmpty => iterate(t, acc.intersect(h))
      case h::Nil               => iterate(Nil, acc.intersect(h))
      case Nil                  => acc.distinct
    }
  }

  val itemTypesSummed: Int =
    puzzleInputAsList.grouped(3)
      .map((thing: List[String]) => iterate(thing.map(_.toList), Nil))
      .toList
      .flatten.map(z => aToZ(z)).sum

  println("Sum of the priorities of item types: " + itemTypesSummed)

}
