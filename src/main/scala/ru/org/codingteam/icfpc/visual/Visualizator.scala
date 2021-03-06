package ru.org.codingteam.icfpc.visual

import java.awt.Color

import ru.org.codingteam.icfpc.{CellState, Emulator}

class Visualizator (board: Board) {

  def visualizeState(emulator: Emulator): Unit = {
    board.putSize(emulator.field.height, emulator.field.width)
    board.setGameOver(emulator.gameOver)

    renderFull(emulator)
    renderCurrentUnit(emulator)
    renderStats(emulator)
    board.repaint()
  }

  private def renderFull(emulator: Emulator): Unit = {
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

  private def renderStats(emulator: Emulator): Unit = {
    board.putScore(emulator.score)
    board.putUnits(emulator.units)
  }
}
