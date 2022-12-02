package dayone

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

/**
 * https://adventofcode.com/2022/day/1
 */

object DayOne extends App {

  val puzzleInput: BufferedSource = Source.fromFile("src/main/scala/dayone/puzzleinput.txt")
  val puzzleInputAsList: List[String] = puzzleInput.getLines().toList

  println("\n\n----------\n\n")

  val sumTheCaloriesOfEachElf: Array[Int] =
    puzzleInputAsList                               // get all lines in a list
    .mkString("+")                                  // just add a character so we can easily detect the spaces and then split
    .split("\\+\\+")                         // create the groupings of "elves"
    .map(_.split("\\+").map(_.toInt).sum)    // sum each elves' calories

  //-- part 1 --
  val highestCalories: Int = sumTheCaloriesOfEachElf.max // just get the highest value

  println(s"[Part 1]: Total calories of the Elf with the most calories: $highestCalories")

  //-- part 2 --
  val topThreeCalories: List[Int] = sumTheCaloriesOfEachElf.toList.sorted.takeRight(3)
  val topThreeSummed: Int = topThreeCalories.sum
  println(s"[Part 2]: Three elves with highest calories: $topThreeCalories")
  println(s"[Part 2]: Total calories of top three elves: $topThreeSummed")

  println("\n\n----------\n\n")

  /**
   * functional approach
   */

  @tailrec
  def elvesTotals(list: List[String], currentBunch: Int, acc: List[Int]): List[Int] = {
    list match {
      case Nil => acc
      case h :: t => h match {
        case s if s.isEmpty  => elvesTotals(t, 0, acc :+ currentBunch)
        case s if s.nonEmpty => elvesTotals(t, currentBunch + s.toInt, acc)
      }
      case h :: Nil => h match {
        case s if s.isEmpty  => elvesTotals(Nil, 0, acc :+ currentBunch)
        case s if s.nonEmpty => elvesTotals(Nil, currentBunch + s.toInt, acc)
      }
    }
  }

  val eachElf: List[Int] = elvesTotals(puzzleInputAsList, 0, Nil)
  val mostCalories: Int = eachElf.max
  val topThree: List[Int] = eachElf.sorted.takeRight(3)
  val topThreeAdded: Int = topThree.sum
  println(s"[Part 1]: Total calories of the Elf with the most calories: $mostCalories")
  println(s"[Part 2]: Three elves with highest calories: $topThree")
  println(s"[Part 2]: Total calories of top three elves: $topThreeAdded")

}
