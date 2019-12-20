package com.even.financial.test

import java.util.IllegalFormatException

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object BoundingBox {
  def main(args: Array[String]): Unit = {
    val (maxLines, maxChars: Int) = lineAndCharLimit
    val graph: List[String] = getGraph(maxLines, maxChars)
    val coordinates = getCoordinates(graph, maxLines, maxChars)

    println("Printing out coordinates for all minimum bounding box with the giving inputs")
    coordinates.foreach(println)
  }

  def lineAndCharLimit: (Int, Int) = {
    println("Please enter the maximum number of lines for the box")
    val maxLines = retry ({ scala.io.StdIn.readInt() }, { _ => println("Please only enter a number") })

    println("Please enter the maximum number of characters per line for the box")
    val maxChar = retry ({ scala.io.StdIn.readInt() }, { _ => println("Please only enter a number") })
    (maxLines, maxChar)
  }

  def getGraph(maxLines: Int, maxChars: Int): List[String] = {
    println("Please enter the requirements for the bounding box. Make sure to only enter these characters [\"-\", \"*\"]")
    var boxBuffer = ListBuffer[String]()

    for(currentLine <- 1 to maxLines) {
      println(s"Line $currentLine")
      boxBuffer += retry({
        val line = scala.io.StdIn.readLine()
        if(line.length != maxChars || !line.matches("[*-]+"))
          throw new Throwable
        line
      }, { _ => println(s"Only enter asterisks and/or - hyphens. Please enter $maxChars characters") })

    }

    boxBuffer.toList
  }

  def getCoordinates(graph: List[String], maxLines: Int, maxChars: Int): List[((Int, Int), (Int, Int))] = {
    var coordinates = ListBuffer[((Int, Int), (Int, Int))]()

    for(row <- 0 until maxLines) {
      for(column <- 0 until maxChars){
        if(graph(row).charAt(column).equals('*')){
          if(toProcess(coordinates, graph, row, column)) {
            val topLeft = (row + 1, column + 1)
            val bottomRight = getBottomRightCoordinate(graph, row, column)
            if(bottomRight.isDefined)
              coordinates.+=((topLeft, bottomRight.get))
          }
        }
      }
    }

    coordinates.toList
  }

  def getBottomRightCoordinate(graph: List[String], row: Int, column: Int): Option[(Int, Int)] = {

    val furthestColumn = goRight(graph, row, column)
    if(furthestColumn < 0) return None

    val furthestRow = goDown(graph, row, furthestColumn)
    if(furthestRow < 0) return None

    if(row == furthestRow || column == furthestColumn) return None

    Some(furthestRow + 1, furthestColumn + 1)
  }

  /**
   * Keep searching for asterisk on right. If there is an asterisk on top, then fail because it is overlapping
   * @param graph
   * @param row
   * @param column
   * @return
   */
  def goRight(graph: List[String], row: Int, column: Int): Int = {
    if(row != 0 && graph(row - 1).charAt(column) == '*') -1
    else if(column == graph(row).length -1 || graph(row).charAt(column + 1) == '-') column
    else goRight(graph, row, column + 1)
  }

  /**
   * Keep searching for asterisk on bottom. If there is an asterisk on right, then fail because it is overlapping
   * @param graph
   * @param row
   * @param column
   * @return
   */
  def goDown(graph: List[String], row: Int, column: Int): Int = {
    if(column != graph(row).length -1 && graph(row).charAt(column + 1) == '*') -1
    else if(row == graph.length - 1 || graph(row + 1).charAt(column) == '-') row
    else goDown(graph, row + 1, column)
  }

  /**
   * If there is an asterisk on the left return false, the current row and column are between an existing bounding box, we skip
   * @param coordinates
   * @param graph
   * @param row
   * @param column
   * @return
   */
  def toProcess(coordinates: ListBuffer[((Int, Int), (Int, Int))], graph: List[String], row: Int, column: Int): Boolean = {
    if(column != 0 && graph(row).charAt(column - 1) =='*')
      return false

    coordinates.foreach(coordinate => {
      val topLeft: (Int, Int) = coordinate._1
      val bottomRight: (Int, Int) = coordinate._2

      if((topLeft._1 <= row + 1 && row + 1 <= bottomRight._1 ) && (topLeft._2 <= column + 1 && column + 1 <= bottomRight._2 ))
          return false
    })

    true
  }

  def retry[T](op: => T, onWrong: Throwable => Any = _ => ()): T =
    Iterator.continually(Try(op)).flatMap {
      case Success(t) => Some(t)
      case Failure(f) => onWrong(f); None
    }.toSeq.head
}
