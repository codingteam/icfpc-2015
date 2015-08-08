package ru.org.codingteam.icfpc.visual

import java.awt.{Color, Dimension}
import javax.swing.{WindowConstants, JFrame}

object VisualizatorApplication {
  def main(args: Array[String]): Unit = {
    val f = new JFrame("Codingteam ICFPC 2015")
    val board = new Board(10, 10)

    board.putCell(0, 0, Color.RED)
    board.putCell(1, 1, Color.GREEN)

    f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    f.setContentPane(board)
    f.pack()
    f.setSize(new Dimension(800, 600))
    f.setVisible(true)
  }
}
