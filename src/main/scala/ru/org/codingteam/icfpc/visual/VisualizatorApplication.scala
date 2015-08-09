package ru.org.codingteam.icfpc.visual

import java.awt.Dimension
import javax.swing.{JFrame, WindowConstants}

import ru.org.codingteam.icfpc.visual.controller.ManualController

object VisualizatorApplication {
  def main(args: Array[String]): Unit = {
    println(args.toList)
    val filePath = if (args.length == 1) args(0) else "problem_0.json"
    val board = new Board
    val frame = new JFrame
    val controller = new ManualController(new Visualizator(board), filePath)

    frame.setContentPane(board)
    frame.setTitle(controller.title)
    frame.addKeyListener(controller.keyListener)

    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.pack()
    frame.setSize(new Dimension(600, 800))
    frame.setVisible(true)
  }
}
