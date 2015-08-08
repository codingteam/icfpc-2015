package ru.org.codingteam.icfpc.visual

import java.awt.Color
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.Timer

import ru.org.codingteam.icfpc.Emulator

class Visualizator (emulator: Emulator, board: Board) extends ActionListener{
  val timer = new Timer(1000, this)

  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    println("tick!")
    clearBoard()
    renderFilled()

    board.repaint()
  }

  def start(): Unit = {
    timer.start()
  }

  private def clearBoard(): Unit = {
    for (row <- 0 until board.rowsCount) {
      for (col <- 0 until board.colsCount) {
        board.putCell(row, col, Color.WHITE)
      }
    }
  }

  private def renderFilled(): Unit = {
    for (cel <- emulator.fieldDef.filled) {
      board.putCell(cel.y, cel.x, Color.YELLOW)
    }
  }
}
