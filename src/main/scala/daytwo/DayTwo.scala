package daytwo

import daytwo.RockPaperScissors.{Move, Paper, Rock, Scissors, determineWhatToPlay, symbolToMove}

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

/**
 * https://adventofcode.com/2022/day/2
 */

object DayTwo extends App {

  val puzzleInput: BufferedSource = Source.fromFile(s"src/main/scala/daytwo/puzzleinput.txt")
  val puzzleInputAsList: List[String] = puzzleInput.getLines().toList

  println("\n\n----- Part 1 -----\n\n")

  @tailrec
  def iterateThroughFile(s: String => (Move, Move))(remainingLines: List[String], accScore: Int): Int = {
    remainingLines match {
      case h :: t   => iterateThroughFile(s)(t, calculateScore(s(h)) + accScore)
      case h :: Nil => iterateThroughFile(s)(Nil, calculateScore(s(h)) + accScore)
      case Nil      => accScore
    }
  }

  private def parseLine(line: String): (Move, Move) = {
    line.split("\\s").toList.map(symbolToMove) match {
      case List(p1, p2) => p1 -> p2
    }
  }

  private val winningMoveValue: Int = 6
  private val drawingMoveValue: Int = 3
  private val losingMoveValue: Int = 0

  private def calculateScore(moves: (Move, Move)): Int = {
    val winLoseDrawPoints: Int = (moves._1, moves._2) match {
      case (Rock, Paper)       => winningMoveValue
      case (Paper, Scissors)   => winningMoveValue
      case (Scissors, Rock)    => winningMoveValue
      case (Rock, Scissors)    => losingMoveValue
      case (Paper, Rock)       => losingMoveValue
      case (Scissors, Paper)   => losingMoveValue
      case (p1,p2) if p1 == p2 => drawingMoveValue
    }
    winLoseDrawPoints + moves._2.moveValue
  }

  val score = iterateThroughFile(parseLine)(puzzleInputAsList, 0)
  println(s"Score: [ $score ]")


  println("\n\n----- Part 2 -----\n\n")

  private def parseLine2(line: String): (Move, Move) = {
    val stringsFromFile = line.split("\\s")
    val (opponentChoice, myChoice: String) = stringsFromFile(0) -> stringsFromFile(1)
    (symbolToMove(opponentChoice), determineWhatToPlay(myChoice, symbolToMove(opponentChoice)))
  }

  val score2 = iterateThroughFile(parseLine2)(puzzleInputAsList, 0)
  println(s"Score: [ $score2 ]")

}

object RockPaperScissors {
  sealed trait Move {
    def moveValue: Int
  }
  case object Rock extends Move {
    val moveValue: Int = 1
  }
  case object Paper extends Move {
    val moveValue: Int = 2
  }
  case object Scissors extends Move {
    val moveValue: Int = 3
  }

  def symbolToMove(input: String): Move = input match {
    case "A" | "X" => Rock
    case "B" | "Y" => Paper
    case "C" | "Z" => Scissors
    case error => throw new MatchError(s"Input not valid... [ $error ]")
  }

  def determineWhatToPlay(input: String, move: Move): Move = input match {
    case "Y" => move
    case "X" if move == Rock => Scissors
    case "X" if move == Paper => Rock
    case "X" if move == Scissors => Paper
    case "Z" if move == Rock => Paper
    case "Z" if move == Paper => Scissors
    case "Z" if move == Scissors => Rock
    case error => throw new MatchError(s"Input not valid... [ $error ]")
  }
}
