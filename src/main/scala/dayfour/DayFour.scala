package dayfour

import util.ReadPuzzleInput

/**
 * https://adventofcode.com/2022/day/4
 */

object DayFour extends App {

  val puzzleInputAsList: List[String] = ReadPuzzleInput.puzzleInputToList(s"src/main/scala/dayfour/puzzleinput.txt")

  println("\n\n----- Part 1 -----\n\n")

  private def stringToRanges(string: String): (Range.Inclusive, Range.Inclusive) = {
    val ranges = string.split(",")
      .map(_.split("-"))
      .map(range => range.head.toInt to range.last.toInt)
    ranges.head -> ranges.last
  }

  private def isSubRange(ranges: (Range, Range)): Boolean = {
    val (range1, range2) = ranges._1 -> ranges._2
      range1.contains(range2.start) && range1.contains(range2.end) ||
        range2.contains(range1.start) && range2.contains(range1.end)
  }


  val countSubRanges: Int = puzzleInputAsList.map((line: String) => isSubRange(stringToRanges(line))).count(_ == true)

  println("Assignment pairs where one range fully contains the other: " + countSubRanges)

  println("\n\n----- Part 2 -----\n\n")

  private def isOverlap(ranges: (Range, Range)): Boolean = {
    val (range1, range2) = ranges._1 -> ranges._2
      range1.contains(range2.start) || range1.contains(range2.end) ||
        range2.contains(range1.start) || range2.contains(range1.end)
  }

  val countOverlaps: Int = puzzleInputAsList.map((line: String) => isOverlap(stringToRanges(line))).count(_ == true)
  println("Assignment pairs with overlaps: " + countOverlaps)


}
