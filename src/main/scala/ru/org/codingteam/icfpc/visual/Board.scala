package ru.org.codingteam.icfpc.visual

import java.awt.{Font, Color, Graphics2D, Graphics}
import javax.swing.JPanel


class Board(val rowsCount: Int, val colsCount: Int) extends JPanel {
  private val cells = Array.fill(rowsCount, colsCount)(Color.WHITE)
  var score = 0

  override def paintComponent(g: Graphics): Unit = {
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setColor(Color.GRAY)
    g2.fillRect(0, 0, getWidth, getHeight)

    renderCells(g2)
    renderScore(g2)
  }


  private def renderGrid(g2: Graphics2D): Unit = {
    val cellWidth = getWidth / colsCount.toFloat
    val cellHeight = getHeight / rowsCount.toFloat

    g2.setColor(Color.BLACK)

    for (i <- 1 until colsCount) {
      g2.drawLine((i * cellWidth).toInt, 0, (i * cellWidth).toInt, getHeight)
    }

    for (i <- 1 until rowsCount) {
      g2.drawLine(0, (i * cellHeight).toInt, getWidth, (i * cellHeight).toInt)
    }
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

  private def renderScore(g2: Graphics2D): Unit = {
    g2.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 16))

    val text = s"Score: $score"
    val metrics = g2.getFontMetrics
    val height = metrics.getHeight

    g2.setColor(Color.ORANGE)
    g2.drawString(text, 10, height)
  }

  def putCell(row: Int, col: Int, color: Color): Unit = {
    cells(row)(col) = color
  }
}
