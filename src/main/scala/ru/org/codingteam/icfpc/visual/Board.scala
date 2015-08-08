package ru.org.codingteam.icfpc.visual

import java.awt.{Color, Graphics2D, Graphics}
import javax.swing.JPanel


class Board(width: Int, height: Int) extends JPanel {
  override def paintComponent(g: Graphics): Unit = {
    val g2 = g.asInstanceOf[Graphics2D]
    renderGrid(g2)
  }


  protected def renderGrid(g2: Graphics2D): Unit = {
    val cellWidth = getWidth / (width + 0.0)
    val cellHeight = getHeight / (height + 0.0)

    for (i <- 1 to width - 1) {
      g2.drawLine((i * cellWidth).toInt, 0, (i * cellWidth).toInt, getHeight)
    }

    for (i <- 1 to height - 1) {
      g2.drawLine(0, (i * cellHeight).toInt, getWidth, (i * cellHeight).toInt)
    }
  }

  def putCell(x: Int, y: Int, color: Color): Unit = {

  }
}
