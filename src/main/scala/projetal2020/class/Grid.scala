package projetal2020.`class`

import play.api.libs.json.{JsObject, Json}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

class Grid(x: Int, y: Int) {
  val width: Int = x
  val height: Int = y
  val lawnMowerJsonOutPut: ArrayBuffer[JsObject] = ArrayBuffer()

  def createLawnMowerAndGiveInstructions(
      line: String,
      instructions: String
  ): Unit = {
    val lineSplit = line.split(" ")
    val lawnMower: LawnMower = new LawnMower(
      lineSplit(0).toInt,
      lineSplit(1).toInt,
      lineSplit(2)(0),
      (width, height)
    )
    lawnMower.moveInstruction(instructions.toCharArray) match {
      case Success(value) =>
        lawnMowerJsonOutPut.insert(lawnMowerJsonOutPut.size, value)
      case Failure(exception) => println(exception)
    }
  }

  def getJson(): JsObject = {
    Json.obj(
      "limite"    -> Json.obj("x" -> width, "y" -> height),
      "tondeuses" -> lawnMowerJsonOutPut
    )
  }
}
