package ru.org.codingteam.icfpc.visual

import java.awt.{Font, Color, Graphics2D, Graphics}
import javax.swing.JPanel


class Board extends JPanel {
  private var cells = Array(Array(Color.WHITE))
  private var currentScore = 0
  private var currentUnits = 0
  private var rowsCount = 1
  private var colsCount = 1
  private var isGameOver = false

  override def paintComponent(g: Graphics): Unit = {
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setColor(Color.GRAY)
    g2.fillRect(0, 0, getWidth, getHeight)

    renderCells(g2)
    renderStats(g2)
    renderGameOver(g2)
  }

  private def renderCells(g2: Graphics2D): Unit = {
    val cellWidth = getWidth / (colsCount + 1).toFloat
    val cellHeight = getHeight / rowsCount.toFloat

    for (row <- 0 until rowsCount) {
      for (col <- 0 until colsCount) {
        val shift = if (row % 2 == 0) {
          0.0
        } else {
          cellWidth / 2.0
        }

        g2.setColor(cells(row)(col))
        g2.fillRect((col * cellWidth + shift).toInt, (row * cellHeight).toInt,
          cellWidth.toInt, cellHeight.toInt)

        g2.setColor(Color.BLACK)
        g2.drawRect((col * cellWidth + shift).toInt, (row * cellHeight).toInt,
          cellWidth.toInt, cellHeight.toInt)

      }
    }
  }

  private def renderStats(g2: Graphics2D): Unit = {
    g2.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 16))

    val metrics = g2.getFontMetrics
    val height = metrics.getHeight

    g2.setColor(Color.ORANGE)
    g2.drawString(s"Score: $currentScore", 10, height)
    g2.drawString(s"Units: $currentUnits", 10, height * 2 + 5)
  }

  private def renderGameOver(g2: Graphics2D): Unit = {
    if (isGameOver) {
      g2.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 16))

      val text = "Game over"
      val metrics = g2.getFontMetrics
      val height = metrics.getHeight

      g2.setColor(Color.RED)
      g2.drawString(text, 10, 2 * height)
    }
  }

  def putCell(row: Int, col: Int, color: Color): Unit = {
    cells(row)(col) = color
  }

  def putScore(score: Int): Unit = {
    currentScore = score
  }

  def putUnits(units: Int): Unit = {
    currentUnits = units
  }

  def setGameOver(gameover : Boolean) = {
    isGameOver = gameover
  }

  def putSize(rowsCount: Int, colsCount: Int): Unit = {
    this.rowsCount = rowsCount
    this.colsCount = colsCount
    cells = Array.fill(rowsCount, colsCount)(Color.WHITE)
  }
}
