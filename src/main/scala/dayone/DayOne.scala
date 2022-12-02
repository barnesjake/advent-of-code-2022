package dayone

import scala.io.Source

/**
 * https://adventofcode.com/2022/day/1
 */

object DayOne extends App {
  val puzzleInput = Source.fromFile("src/main/scala/dayone/puzzleinput.txt")

  val sumTheCaloriesOfEachElf: Array[Int] =
    puzzleInput.getLines().toList                   // get all lines in a list
    .mkString("+")                                  // just add a character so we can easily detect the spaces and then split
    .split("\\+\\+")                         // create the groupings of "elves"
    .map(_.split("\\+").map(_.toInt).sum)    // sum each elves' calories

  //-- part 1 --
  val highestCalories = sumTheCaloriesOfEachElf.max // just get the highest value

  println(s"Total calories of the Elf with the most calories: $highestCalories")

  //-- part 2 --
  val topThreeCalories: List[Int] = sumTheCaloriesOfEachElf.toList.sorted.takeRight(3)
  val topThreeSummed: Int = topThreeCalories.sum
  println(s"Total calories of top three elves: $topThreeSummed")

}
