package projetal2020.`class`

import scala.collection.mutable
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}
import projetal2020.exception.{IncorrectDataException, OutOfBoundException}

import scala.collection.mutable.ArrayBuffer

class LawnMower(x: Int, y: Int, direction: Char, gridSize: (Int, Int)) {

  val positionMap: mutable.HashMap[Char, Int] =
    new mutable.HashMap[Char, Int]()
  val directionMap: mutable.HashMap[String, Char] =
    new mutable.HashMap[String, Char]()

  val stringOutput: ArrayBuffer[String] =
    new ArrayBuffer[String]()

  positionMap('x') = x
  positionMap('y') = y
  directionMap("direction") = direction

  def move(letter: Char): Try[String] = letter match {
    case 'D' | 'G' =>
      changeDirection(letter) match {
        case Success(char) =>
          directionMap("direction") = char
          Success("")
        case Failure(exception) => Failure(exception)
      }
    case 'A' =>
      movingForward() match {
        case Success((pos, value)) =>
          pos match {
            case 'x' =>
              positionMap('x') = positionMap('x').toString.toInt + value
              Success("")
            case 'y' =>
              positionMap('y') = positionMap('y').toString.toInt + value
              Success("")
          }
        case Failure(exception) => Failure(exception)
      }
    case _ =>
      Failure(
        new IncorrectDataException(
          "Instructions not recognized, please use the letter : A, G or D !"
        )
      )
  }

  def changeDirection(dir: Char): Try[Char] = {
    directionMap("direction") match {
      case 'N' =>
        if (dir.equals('G')) {
          Success('W')
        } else if (dir.equals('D')) {
          Success('E')
        } else {
          Failure(
            new IncorrectDataException(
              "The instructions given are incorrect, please modify the instructions file."
            )
          )
        }
      case 'E' =>
        if (dir.equals('G')) {
          Success('N')
        } else if (dir.equals('D')) {
          Success('S')
        } else {
          Failure(
            new IncorrectDataException(
              "The instructions given are incorrect, please modify the instructions file."
            )
          )
        }
      case 'W' =>
        if (dir.equals('G')) {
          Success('S')
        } else if (dir.equals('D')) {
          Success('N')
        } else {
          Failure(
            new IncorrectDataException(
              "The instructions given are incorrect, please modify the instructions file."
            )
          )
        }
      case 'S' =>
        if (dir.equals('G')) {
          Success('E')
        } else if (dir.equals('D')) {
          Success('W')
        } else {
          Failure(
            new IncorrectDataException(
              "The instructions given are incorrect, please modify the instructions file."
            )
          )
        }
      case _ =>
        Failure(
          new IncorrectDataException(
            "The instructions given are incorrect, please modify the instructions file."
          )
        )
    }
  }

  def movingForward(): Try[(Char, Int)] = {
    val y = positionMap('y')
    val x = positionMap('x')
    directionMap("direction") match {
      case 'N' =>
        if (gridSize._2 > y)
          Success(('y', 1))
        else
          Failure(
            new OutOfBoundException(
              "The lawnmower can't get outside of the boundary"
            )
          )
      case 'E' =>
        if (gridSize._1 > x)
          Success(('x', 1))
        else
          Failure(
            new OutOfBoundException(
              "The lawnmower can't get outside of the boundary"
            )
          )
      case 'W' =>
        if (x > 0)
          Success(('x', -1))
        else
          Failure(
            new OutOfBoundException(
              "The lawnmower can't get outside of the boundary"
            )
          )
      case 'S' =>
        if (y > 0)
          Success(('y', -1))
        else
          Failure(
            new OutOfBoundException(
              "The lawnmower can't get outside of the boundary"
            )
          )
      case _ =>
        Failure(
          new IncorrectDataException("Error in the lawn mower direction!")
        )
    }
  }

  def toJson(
      beginning: JsObject,
      ending: JsObject,
      instructions: Array[Char]
  ): JsObject = {
    Json.obj(
      "debut"        -> beginning,
      "instructions" -> String.valueOf(instructions),
      "fin"          -> ending
    )
  }

  def infoJson(): JsObject = {
    Json.obj(
      "point" -> Json
        .obj("x" -> positionMap('x'), "y" -> positionMap('y')),
      "direction" -> directionMap("direction").toString
    )
  }

  def moveInstruction(instructions: Array[Char]): Try[JsObject] = {
    val debut: JsObject = infoJson()
    for (instruction: Char <- instructions: Array[Char]) {
      try {
        move(instruction)
      } catch {
        case _: IncorrectDataException =>
          Failure(
            new IncorrectDataException(
              "Instructions not recognized, please use the letter : A, G or D !"
            )
          )
      }
    }
    val fin: JsObject = infoJson()
    Success(toJson(debut, fin, instructions))
  }

}
