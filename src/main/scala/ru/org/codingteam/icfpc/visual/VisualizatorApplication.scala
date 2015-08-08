package ru.org.codingteam.icfpc.visual

import java.awt.{Color, Dimension}
import javax.swing.{WindowConstants, JFrame}

import ru.org.codingteam.icfpc.Emulator

object VisualizatorApplication {
  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Codingteam ICFPC 2015")
    val emulator = Emulator("problem_1.json")
    val board = new Board(emulator.fieldDef.height, emulator.fieldDef.width)
    val visualizator = new Visualizator(emulator, board)

    visualizator.start()
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setContentPane(board)
    frame.pack()
    frame.setSize(new Dimension(600, 800))
    frame.setVisible(true)
  }
}
