package ru.org.codingteam.icfpc.visual

import java.awt.Color

import ru.org.codingteam.icfpc.{CellState, Emulator}

class Visualizator (board: Board) {

  def visualizeState(emulator: Emulator): Unit = {
    board.putSize(emulator.field.height, emulator.field.width)
    renderFilled(emulator)
    renderCurrentUnit(emulator)
    renderScore(emulator)
    board.repaint()
  }

  private def renderFilled(emulator: Emulator): Unit = {
    for (row <- 0 until emulator.field.height) {
      for (col <- 0 until emulator.field.width) {
        val color = if (emulator.field(col, row) == CellState.Full) {
          Color.YELLOW
        } else {
          Color.WHITE
        }
        board.putCell(row, col, color)
      }
    }
  }

  private def renderCurrentUnit(emulator: Emulator): Unit = {
    if (emulator.currentUnit != null) {
      for (cel <- emulator.currentUnit.members) {
        board.putCell(cel.y, cel.x, Color.BLUE)
      }
    }
  }

  private def renderScore(emulator: Emulator): Unit = {
    board.putScore(emulator.score)
  }

}
