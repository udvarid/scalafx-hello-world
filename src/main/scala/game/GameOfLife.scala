package game

import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

import scala.annotation.tailrec
import scala.util.Random

class GameOfLife(initCells: Int, canvas: Canvas, size: Int) {

  private val cells: Set[Cell] = fillUpCells

  def fillUpCells: Set[Cell] = {
    @tailrec
    def fillCells(cellsInProgress: Set[Cell]): Set[Cell] = {
      if (cellsInProgress.size == initCells) cellsInProgress
      else {
        val coordX = Random.nextInt(size / 25)
        val coordY = Random.nextInt(size / 25)
        fillCells(cellsInProgress + Cell(coordX, coordY))
      }
    }

    fillCells(Set[Cell]())
  }

  private def drawCells(cellsToDraw: Set[Cell]): Unit = {
    val gc = canvas.graphicsContext2D
    cellsToDraw.filter(c => c.newBorn || !c.live)
      .foreach(c => {
        gc.fill = if (c.live) Color.Green else Color.White
        val fold = if (c.live) 10 else 11
        gc.fillOval(c.x * 25 + 7, c.y * 25 + 7, fold, fold)
        if (c.newBorn) c.newBorn = false
      })
  }


  implicit class PimpMyInteger(number: Int) {
    def isDying: Boolean = number < 2 || number > 3
    def canBorn: Boolean = number == 3
    def isInside: Boolean = number >= 0 && number < size / 25
  }

  def passAways(workingCells: Set[Cell]) = {
    workingCells.foreach(c => {
      if (workingCells.count(c2 => c2 ~ c).isDying) c.diePlease
    })
  }

  def newBurnAdded(workingCells: Set[Cell]): Set[Cell] = {

    val newBurns = workingCells.flatMap(c => (!c).filter(cf => cf.x.isInside && cf.y.isInside))

    @tailrec
    def selectNewBurns(newBurnCells: Set[Cell], livingCells: Set[Cell]): Set[Cell] = {
      if (newBurnCells.isEmpty) return livingCells
      else {
        if (livingCells.count(c2 => c2 ~ newBurnCells.head).canBorn)
          selectNewBurns(newBurnCells.toList.tail.toSet, livingCells + newBurnCells.head)
        else
          selectNewBurns(newBurnCells.toList.tail.toSet, livingCells)
      }
    }

    selectNewBurns(newBurns.diff(workingCells), workingCells)

  }

  def startGame: Unit = {
    drawCells(cells)

    @tailrec
    def nextRound(workingCells: Set[Cell]): Unit = {
      Thread.sleep(500)
      if (workingCells.isEmpty) return
      else {
        passAways(workingCells)
        val extendedCells = newBurnAdded(workingCells)
        drawCells(extendedCells)
        nextRound(extendedCells.filter(c => c.live))
      }
    }

    nextRound(cells)
  }

}
