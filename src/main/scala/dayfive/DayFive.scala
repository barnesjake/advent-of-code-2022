package dayfive

import util.ReadPuzzleInput

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.seqOrdering

/**
 * https://adventofcode.com/2022/day/5
 */

object DayFive extends App {

  val puzzleInputAsList: List[String] = ReadPuzzleInput.puzzleInputToList(s"src/main/scala/dayfive/puzzleinput.txt")

  case class Move(numberOfCrates: Int, fromColumn: Int, toColumn: Int)

  def computeMove(line: String): Move = {
    Move(
      numberOfCrates = line.split("from")(0).filter(_.isDigit).toInt,
      fromColumn = line.split("from")(1).split("to")(0).filter(_.isDigit).toInt - 1,
      toColumn = line.split("to")(1).filter(_.isDigit).toInt - 1
    )
  }

  def splitMovesFromCrates(list: List[String]): (List[String], List[String]) =
    list.filter(_.startsWith("move")) -> list.filterNot(_.startsWith("move")).dropRight(2) //todo filter our by regex -> numeric or something

  val (moves, crates) = splitMovesFromCrates(puzzleInputAsList)

  val movesParsed: List[Move] = moves.map(computeMove)
  crates.foreach(println)
  println("---")
  val test: List[String] = crates.map(_.toList.padTo(35, " ")).map(_.mkString).map(_.replaceAll("\\s", "!"))
  test.foreach(println)
  println("---")

  def sanitiseProperly(string: String, updatedString: String): String = {
    if (string.isEmpty) updatedString
    else sanitiseProperly(string.drop(4), string.take(3))
  }

  test.map(sanitiseProperly(_, "")).foreach(println)


  val sanitisedCrates: List[List[Char]] = crates.map(
    _.replaceAll(" ", "-")
      .replace("---", "!")
      .replace("-[", "").replace("[", "").replace("]", "")
  ).map(_.toList.padTo(9, '-'))

  sanitisedCrates.foreach(println)
  val grouped = sanitisedCrates.flatMap(_.zipWithIndex).groupBy(_._2).toSeq.sorted
  println("\n---\n")
  println("grouped")
  grouped.foreach(println)
  println("\n---\n")
  val columns: List[List[Char]] = grouped.map(_._2.map(_._1)).toList.map(_.reverse).map(_.filterNot(_ == '-'))
  println("\n---\n")
  columns.foreach(println)
  println("\n---\n")


  def doTheMoving(columns: List[List[Char]], move: Move): List[List[Char]] = {
    if (columns(move.fromColumn).isEmpty) columns
    else {
      @tailrec
      def moveOneAtTime(c: List[List[Char]], movesLeft: Int): List[List[Char]] = {
        if (movesLeft == 0) c
        else if (c(move.fromColumn).isEmpty) c
        else {
          moveOneAtTime(columns
            .updated(move.toColumn, c(move.toColumn) ++ c(move.fromColumn).takeRight(1))
            .updated(move.fromColumn, c(move.fromColumn).dropRight(1)), movesLeft - 1)
        }
      }

      moveOneAtTime(columns, move.numberOfCrates)
    }
  }

  @tailrec
  def iterate(moves: List[Move], columns: List[List[Char]]): List[List[Char]] = {
    moves match {
      case ::(head, next) =>
        iterate(next, doTheMoving(columns, head))
      case ::(head, Nil) =>
        iterate(Nil, doTheMoving(columns, head))
      case Nil => columns
    }
  }

  val allMoves: List[List[Char]] = iterate(movesParsed, columns)

  println("Crates on top: " + allMoves.flatMap(_.lastOption))

}
