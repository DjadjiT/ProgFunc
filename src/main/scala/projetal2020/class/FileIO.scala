package projetal2020.`class`

import java.io.IOException

import play.api.libs.json.JsObject
import projetal2020.exception.IncorrectDataException

import scala.io.Source
import scala.util.{Failure, Success, Try}

object FileIO {

  def read(filename: String): Try[List[String]] = {
    try {
      val bufferedSource = Source.fromFile(filename)
      val lines = bufferedSource.getLines().toList
      bufferedSource.close
      Success(lines)
    } catch {
      case _: IOException =>
        Failure(new IOException("Couldn't find the file to read. \n"))
    }
  }

  def readFileForInstructions(filename: String): Try[JsObject] = {
    try {
      read(filename) match {
        case Success(lines) =>
          if (lines.size % 2 != 1) {
            Failure(
              new IncorrectDataException(
                "The file is not written correctly, the number of line doesn't match !"
              )
            )
          } else {
            val firstLine: String = lines(0)
            val lineSplit: Array[String] = firstLine.split(" ")
            val grid: Grid =
              new Grid(lineSplit(0).toInt, lineSplit(1).toInt)

            val instructionList: List[List[String]] =
              lines.slice(1, lines.size).grouped(2).toList
            parseLines(instructionList, grid)
          }
        case Failure(exception) => Failure(exception)
      }
    } catch {
      case _: NumberFormatException =>
        Failure(
          new IncorrectDataException(
            "Expected a number but didn't find one! "
          )
        )
    }
  }

  def parseLines(lines: List[List[String]], grid: Grid): Try[JsObject] = {
    for (line: List[String] <- lines: List[List[String]]) {
      grid.createLawnMowerAndGiveInstructions(line(0), line(1))
    }
    Success(grid.getJson())
  }

}
