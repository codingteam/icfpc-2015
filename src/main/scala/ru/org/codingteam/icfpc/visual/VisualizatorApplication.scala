package ru.org.codingteam.icfpc.visual

import java.awt.Dimension
import javax.swing.{WindowConstants, JFrame}

object VisualizatorApplication {
  def main(args: Array[String]): Unit = {
    val f = new JFrame("Codingteam ICFPC 2015")
    f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    f.setContentPane(new Board(10, 10))
    f.pack()
    f.setSize(new Dimension(800, 600))
    f.setVisible(true)
  }
}
