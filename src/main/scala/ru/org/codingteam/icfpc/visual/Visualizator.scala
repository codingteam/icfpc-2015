package ru.org.codingteam.icfpc.visual

import java.awt.Color
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.Timer

import ru.org.codingteam.icfpc.{Command, Emulator}

class Visualizator (emulator: Emulator, board: Board) {


  def visualizeState: Unit = {
    clearBoard()
    renderFilled()
    renderCurrentUnit()
    board.repaint()
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

  private def renderCurrentUnit(): Unit = {

    if (emulator.currentUnit != null) {
      for (cel <- emulator.currentUnit.members) {
        board.putCell(cel.y, cel.x, Color.BLUE)
      }
    }
  }
}
