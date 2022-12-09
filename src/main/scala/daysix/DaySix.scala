package daysix

import util.ReadPuzzleInput

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

/**
 * https://adventofcode.com/2022/day/6
 */

object DaySix extends App {

  val puzzleInputFromFile: String = ReadPuzzleInput.puzzleInputToList(s"src/main/scala/daysix/puzzleinput.txt").mkString

  @tailrec
  def doTheThing(currentIndex: Int, previousCharacters: List[Char])(uniqueCharacters: Int)(puzzleInput: String): Int = {
    val currentCheck: Char = puzzleInput(currentIndex)
    println(puzzleInput.length)
    val isUnique = !previousCharacters.contains(currentCheck) && previousCharacters.distinct.size == uniqueCharacters
    if (isUnique) currentIndex
    else {
      val characterBuffer: List[Char] = if (previousCharacters.length == uniqueCharacters) previousCharacters.drop(1) else previousCharacters
     doTheThing(currentIndex + 1, characterBuffer ++ List(currentCheck))(uniqueCharacters)(puzzleInput)
    }
  }

  println("\n\n----- Part 1 -----\n\n")
  println("First marker after character: " + doTheThing(0, Nil)(4)(puzzleInputFromFile))

  println("\n\n----- Part 2 -----\n\n")
  println("First marker after character: " + doTheThing(0, Nil)(14)(puzzleInputFromFile))

}
