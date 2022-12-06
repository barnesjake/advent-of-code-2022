package util

import scala.io.{BufferedSource, Source}

object ReadPuzzleInput {
  def puzzleInputToList(puzzleInputLocation: String): List[String] = {
    val file: BufferedSource = Source.fromFile(puzzleInputLocation)
    val lines: List[String] = file.getLines().toList
    file.close()
    lines
  }
}
