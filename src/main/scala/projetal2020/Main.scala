package projetal2020

import play.api.libs.json.Json
import projetal2020.`class`.FileIO

import scala.util.{Failure, Success}

object Main extends App {
  val filename: String = "src/instructions2.txt"

  FileIO.readFileForInstructions(filename) match {
    case Success(value)     => println(Json.prettyPrint(value))
    case Failure(exception) => println(exception)
  }
}
